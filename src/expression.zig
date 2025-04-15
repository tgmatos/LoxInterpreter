const std = @import("std");
const To = @import("token.zig");
const Token = To.Token;
const ValueLiteral = To.Literal;

const activeTag = std.meta.activeTag;
const print = std.debug.print;

const OutOfMemory = std.mem.Allocator.Error.OutOfMemory;
const RuntimeError = error{ OpNaN, OutOfMemory };

pub const Expr = union(enum) {
    const Self = @This();
    literal: *Literal,
    unary: *Unary,
    binary: *Binary,
    grouping: *Grouping,

    pub fn evaluate(expr: *Self, allocator: std.mem.Allocator) RuntimeError!*Expr {
        switch (expr.*) {
            .literal => return expr,
            .grouping => return try Grouping.evaluate(expr.grouping.*, allocator),
            .unary => return try Unary.evaluate(expr.unary.*, allocator),
            .binary => return try Binary.evaluate(expr.binary.*, allocator),
        }
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .literal => |v| {
                allocator.destroy(v);
            },
            .grouping => |v| {
                v.expr.deinit(allocator);
                allocator.destroy(v);
            },
            .unary => |v| {
                v.right.deinit(allocator);
                allocator.destroy(v);
            },
            .binary => |v| {
                v.exprLeft.deinit(allocator);
                v.exprRight.deinit(allocator);
                allocator.destroy(v);
            },
        }

        allocator.destroy(self);
    }
};

pub const Literal = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    nil: void,

    pub fn create(allocator: std.mem.Allocator, value: Literal) !*Expr {
        const literal = try allocator.create(Literal);
        const expr = try allocator.create(Expr);
        switch (value) {
            .number => literal.* = value,
            .string => literal.* = value,
            .boolean => literal.* = value,
            .nil => literal.* = value,
        }
        expr.* = .{ .literal = literal };

        return expr;
    }
};

pub const Grouping = struct {
    expr: *Expr,

    pub fn create(allocator: std.mem.Allocator, expression: *Expr) !*Expr {
        var grouping = try allocator.create(Grouping);
        const expr = try allocator.create(Expr);
        grouping.expr = expression;
        expr.* = .{ .grouping = grouping };
        return expr;
    }

    pub fn evaluate(grouping: Grouping, allocator: std.mem.Allocator) RuntimeError!*Expr {
        return try grouping.expr.evaluate(allocator);
    }
};

pub const Unary = struct {
    operator: Token,
    right: *Expr,

    pub fn create(allocator: std.mem.Allocator, operator: Token, right: *Expr) !*Expr {
        var unary = try allocator.create(Unary);
        const expr = try allocator.create(Expr);
        unary.operator = operator;
        unary.right = right;
        expr.* = .{ .unary = unary };
        return expr;
    }

    pub fn evaluate(unary: Unary, allocator: std.mem.Allocator) RuntimeError!*Expr {
        const right = try unary.right.evaluate(allocator);
        // errdefer right.deinit(allocator);

        const expr = try allocator.create(Expr);
        errdefer allocator.destroy(expr);

        const literal = try allocator.create(Literal);
        errdefer allocator.destroy(literal);

        return switch (unary.operator.kind) {
            .MINUS => {
                if ((activeTag(right.*) != Expr.literal) or (activeTag(right.literal.*) != Literal.number)) {
                    return RuntimeError.OpNaN;
                }

                literal.* = .{ .number = -right.literal.number };
                expr.* = .{ .literal = literal };
                return expr;
            },

            .BANG => {
                // isTruthy
                switch (right.literal.*) {
                    .boolean => {
                        literal.* = .{ .boolean = !right.literal.boolean };
                        expr.* = .{ .literal = literal };
                        return expr;
                    },
                    .nil => {
                        literal.* = .{ .boolean = false };
                        expr.* = .{ .literal = literal };
                        return expr;
                    },
                    else => {
                        literal.* = Literal{ .nil = void{} };
                        expr.* = .{ .literal = literal };
                        return expr;
                    },
                }
            },

            // TODO
            else => {
                literal.* = Literal{ .nil = void{} };
                expr.* = .{ .literal = literal };
                return expr;
            },
        };
    }
};

pub const Binary = struct {
    exprLeft: *Expr,
    operator: Token,
    exprRight: *Expr,

    pub fn create(allocator: std.mem.Allocator, left: *Expr, operator: Token, right: *Expr) !*Expr {
        var binary = try allocator.create(Binary);
        const expr = try allocator.create(Expr);
        binary.exprLeft = left;
        binary.operator = operator;
        binary.exprRight = right;
        expr.* = .{ .binary = binary };

        return expr;
    }

    pub fn evaluate(binary: Binary, allocator: std.mem.Allocator) RuntimeError!*Expr {
        const left = try binary.exprLeft.evaluate(allocator);
        const right = try binary.exprRight.evaluate(allocator);

        const expr = try allocator.create(Expr);
        errdefer allocator.destroy(expr);

        const literal = try allocator.create(Literal);
        errdefer allocator.destroy(literal);

        return switch (binary.operator.kind) {
            .MINUS => {
                literal.* = Literal{ .number = left.literal.number - right.literal.number };
                expr.* = .{ .literal = literal };
                return expr;
            },
            .SLASH => {
                literal.* = Literal{ .number = left.literal.number / right.literal.number };
                expr.* = .{ .literal = literal };
                return expr;
            },
            .STAR => {
                literal.* = Literal{ .number = left.literal.number * right.literal.number };
                expr.* = .{ .literal = literal };
                return expr;
            },
            .GREATER => {
                literal.* = Literal{ .boolean = left.literal.number > right.literal.number };
                expr.* = .{ .literal = literal };
                return expr;
            },
            .GREATER_EQUAL => {
                literal.* = Literal{ .boolean = left.literal.number >= right.literal.number };
                expr.* = .{ .literal = literal };
                return expr;
            },
            .LESS => {
                literal.* = Literal{ .boolean = left.literal.number < right.literal.number };
                expr.* = .{ .literal = literal };
                return expr;
            },
            .LESS_EQUAL => {
                literal.* = Literal{ .boolean = left.literal.number <= right.literal.number };
                expr.* = .{ .literal = literal };
                return expr;
            },

            .BANG_EQUAL => {
                literal.* = Literal{ .boolean = !isEqual(left, right) };
                expr.* = .{ .literal = literal };
                return expr;
            },
            .EQUAL_EQUAL => {
                literal.* = Literal{ .boolean = isEqual(left, right) };
                expr.* = .{ .literal = literal };
                return expr;
            },

            // TODO
            .PLUS => {
                const leftTag = activeTag(left.literal.*);
                const rightTag = activeTag(right.literal.*);
                if (leftTag == Literal.number and rightTag == Literal.number) {
                    literal.* = Literal{ .number = left.literal.number + right.literal.number };
                    expr.* = .{ .literal = literal };
                    return expr;
                }

                if (leftTag == Literal.string and rightTag == Literal.string) {
                    const str = std.mem.concat(allocator, u8, &[_][]const u8{ left.literal.string, right.literal.string }) catch |err| {
                        std.log.err("Error on concatenation string. {any}", .{err});
                        @panic("Aborting");
                    };
                    literal.* = Literal{ .string = str };
                    expr.* = .{ .literal = literal };
                    return expr;
                }

                // TODO: Return an error
                literal.* = Literal{ .nil = void{} };
                expr.* = .{ .literal = literal };
                return expr;
            },

            else => {
                literal.* = Literal{ .nil = void{} };
                expr.* = .{ .literal = literal };
                return expr;
            },
        };
    }

    fn isEqual(left: *Expr, right: *Expr) bool {
        const leftTag = activeTag(left.literal.*);
        const rightTag = activeTag(right.literal.*);

        if (leftTag == Literal.nil and rightTag == Literal.nil) {
            return true;
        }

        if (leftTag == Literal.nil) {
            return false;
        }

        switch (leftTag) {
            Literal.string => {
                if (rightTag == Literal.string) {
                    return std.mem.eql(u8, left.literal.string, right.literal.string);
                }
            },
            Literal.number => {
                if (rightTag == Literal.number) {
                    return left.literal.number == right.literal.number;
                }
            },
            Literal.boolean => {
                if (rightTag == Literal.boolean) {
                    return left.literal.boolean == right.literal.boolean;
                }
            },
            else => return false,
        }

        return false;
    }
};

// expression     → literal
//                | unary
//                | binary
//                | grouping ;

// literal        → NUMBER | STRING | "true" | "false" | "nil" ;
// grouping       → "(" expression ")" ;
// unary          → ( "-" | "!" ) expression ;
// binary         → expression operator expression ;
// operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
//                | "+"  | "-"  | "*" | "/" | "," ;
