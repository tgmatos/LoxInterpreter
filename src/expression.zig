const std = @import("std");

const Util = @import("util.zig");
const To = @import("token.zig");
const Token = To.Token;
const ValueLiteral = To.Literal;

const activeTag = std.meta.activeTag;
const print = std.debug.print;

const OutOfMemory = std.mem.Allocator.Error.OutOfMemory;
const RuntimeError = error{ InvalidOperand, InvalidExpr, OutOfMemory };

pub const Expr = union(enum) {
    const Self = @This();
    literal: *Literal,
    unary: *Unary,
    binary: *Binary,
    grouping: *Grouping,
    variable: *Variable,

    pub fn evaluate(expr: *Self, allocator: std.mem.Allocator) RuntimeError!*Expr {
        switch (expr.*) {
            .literal => {
                const literal: *Literal = try allocator.create(Literal);

                if (activeTag(expr.literal.*) == Literal.string) {
                    const str = try allocator.alloc(u8, expr.literal.string.len);
                    @memcpy(str, expr.literal.string);
                    literal.* = expr.literal.*;
                    literal.*.string = str;
                } else {
                    literal.* = expr.literal.*;
                }

                const cpyExpr: *Expr = try allocator.create(Expr);
                cpyExpr.* = .{ .literal = literal };
                return cpyExpr;
            },
            .grouping => return try Grouping.evaluate(expr.grouping.*, allocator),
            .unary => return try Unary.evaluate(expr.unary.*, allocator),
            .binary => return try Binary.evaluate(expr.binary.*, allocator),
            .variable => {
                return expr;
            },
        }
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .literal => |v| {
                if (activeTag(v.*) == Literal.string) {
                    allocator.free(v.string);
                }
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
            .variable => |v| {
                allocator.destroy(v);
            },
        }
        allocator.destroy(self);
    }
};

pub const Literal = union(enum) {
    number: f64,
    string: []u8,
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
        var grouping: *Grouping = try allocator.create(Grouping);
        const expr: *Expr = try allocator.create(Expr);
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
        var unary: *Unary = try allocator.create(Unary);
        const expr: *Expr = try allocator.create(Expr);
        unary.operator = operator;
        unary.right = right;
        expr.* = .{ .unary = unary };
        return expr;
    }

    pub fn evaluate(unary: Unary, allocator: std.mem.Allocator) RuntimeError!*Expr {
        const right: *Expr = try unary.right.evaluate(allocator);
        defer right.deinit(allocator);

        const expr: *Expr = try allocator.create(Expr);
        errdefer allocator.destroy(expr);

        const literal: *Literal = try allocator.create(Literal);
        errdefer allocator.destroy(literal);

        return switch (unary.operator.kind) {
            .MINUS => {
                if (!checkOperand(right.*, Literal.number)) {
                    return RuntimeError.InvalidOperand;
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
                        return RuntimeError.InvalidOperand;
                    },
                }
            },

            // TODO
            else => {
                return RuntimeError.InvalidExpr;
            },
        };
    }

    fn checkOperand(expr: Expr, t: anytype) bool {
        if ((activeTag(expr) != Expr.literal) or (activeTag(expr.literal.*) != t)) {
            return false;
        }

        return true;
    }
};

pub const Binary = struct {
    exprLeft: *Expr,
    operator: Token,
    exprRight: *Expr,

    pub fn create(allocator: std.mem.Allocator, left: *Expr, operator: Token, right: *Expr) !*Expr {
        var binary: *Binary = try allocator.create(Binary);
        const expr: *Expr = try allocator.create(Expr);
        binary.exprLeft = left;
        binary.operator = operator;
        binary.exprRight = right;
        expr.* = .{ .binary = binary };
        return expr;
    }

    pub fn evaluate(binary: Binary, allocator: std.mem.Allocator) RuntimeError!*Expr {
        const left: *Expr = try binary.exprLeft.evaluate(allocator);
        defer left.deinit(allocator);

        const right: *Expr = try binary.exprRight.evaluate(allocator);
        defer right.deinit(allocator);

        const expr: *Expr = try allocator.create(Expr);
        errdefer allocator.destroy(expr);

        const literal: *Literal = try allocator.create(Literal);
        errdefer allocator.destroy(literal);

        return switch (binary.operator.kind) {
            .MINUS => {
                if (!checkOperand(left.*, right.*)) {
                    return RuntimeError.InvalidOperand;
                }

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

                if (leftTag == Literal.string) {
                    switch (rightTag) {
                        .string => {
                            const str = std.fmt.allocPrint(allocator, "{s}{s}", .{ left.literal.string, right.literal.string }) catch |err| {
                                std.log.err("Error on concatenation string. {any}", .{err});
                                @panic("Aborting");
                            };
                            literal.* = Literal{ .string = str };
                            expr.* = .{ .literal = literal };
                            return expr;
                        },

                        .number => {
                            var buf: [128]u8 = undefined;
                            const result = std.fmt.formatFloat(&buf, right.literal.number, .{ .mode = .decimal }) catch |err| {
                                std.log.err("Error on concatenation string. {any}", .{err});
                                @panic("Aborting");
                            };

                            const str = std.mem.concat(allocator, u8, &[_][]const u8{ left.literal.string, result }) catch |err| {
                                std.log.err("Error on concatenation string. {any}", .{err});
                                @panic("Aborting");
                            };
                            literal.* = Literal{ .string = str };
                            expr.* = .{ .literal = literal };
                            return expr;
                        },
                        .boolean => {
                            switch (right.literal.boolean) {
                                true => {
                                    const str = try std.mem.concat(allocator, u8, &[_][]const u8{ left.literal.string, "true" });
                                    literal.* = Literal{ .string = str };
                                    expr.* = .{ .literal = literal };
                                    return expr;
                                },
                                false => {
                                    const str = try std.mem.concat(allocator, u8, &[_][]const u8{ left.literal.string, "false" });
                                    literal.* = Literal{ .string = str };
                                    expr.* = .{ .literal = literal };
                                    return expr;
                                },
                            }
                        },
                        .nil => {
                            const str = try std.mem.concat(allocator, u8, &[_][]const u8{ left.literal.string, "nil" });
                            literal.* = Literal{ .string = str };
                            expr.* = .{ .literal = literal };
                            return expr;
                        },
                    }
                }

                if (rightTag == Literal.string) {
                    switch (leftTag) {
                        .string => {
                            const str = std.fmt.allocPrint(allocator, "{s}{s}", .{ left.literal.string, right.literal.string }) catch |err| {
                                std.log.err("Error on concatenation string. {any}", .{err});
                                @panic("Aborting");
                            };
                            literal.* = Literal{ .string = str };
                            expr.* = .{ .literal = literal };
                            return expr;
                        },
                        .number => {
                            var buf: [128]u8 = undefined;
                            const result = std.fmt.formatFloat(&buf, left.literal.number, .{ .mode = .decimal }) catch |err| {
                                std.log.err("Error on concatenation string. {any}", .{err});
                                @panic("Aborting");
                            };
                            const str = std.mem.concat(allocator, u8, &[_][]const u8{ result, right.literal.string }) catch |err| {
                                std.log.err("Error on concatenation string. {any}", .{err});
                                @panic("Aborting");
                            };
                            literal.* = Literal{ .string = str };
                            expr.* = .{ .literal = literal };
                            return expr;
                        },
                        .boolean => {
                            switch (left.literal.boolean) {
                                true => {
                                    const str = std.mem.concat(allocator, u8, &[_][]const u8{ "true", right.literal.string }) catch |err| {
                                        std.log.err("Error on concatenation string. {any}", .{err});
                                        @panic("Aborting");
                                    };
                                    literal.* = Literal{ .string = str };
                                    expr.* = .{ .literal = literal };
                                    return expr;
                                },
                                false => {
                                    const str = std.mem.concat(allocator, u8, &[_][]const u8{ "false", right.literal.string }) catch |err| {
                                        std.log.err("Error on concatenation string. {any}", .{err});
                                        @panic("Aborting");
                                    };
                                    literal.* = Literal{ .string = str };
                                    expr.* = .{ .literal = literal };
                                    return expr;
                                },
                            }
                        },
                        .nil => {
                            const str = std.mem.concat(allocator, u8, &[_][]const u8{ "nil", right.literal.string }) catch |err| {
                                std.log.err("Error on concatenation string. {any}", .{err});
                                @panic("Aborting");
                            };
                            literal.* = Literal{ .string = str };
                            expr.* = .{ .literal = literal };
                            return expr;
                        },
                    }
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

    fn checkOperand(left: Expr, right: Expr) bool {
        if (activeTag(left) != Expr.literal) return false;
        if (activeTag(right) != Expr.literal) return false;
        if (activeTag(left.literal.*) != Literal.number) return false;
        if (activeTag(right.literal.*) != Literal.number) return false;
        return true;
    }
};

pub const Variable = struct {
    name: Token,

    pub fn create(allocator: std.mem.Allocator, name: Token) !*Expr {
        var variable: *Variable = try allocator.create(Variable);
        variable.name = name;

        const expr: *Expr = try allocator.create(Expr);
        expr.* = .{ .variable = variable };
        return expr;
    }
};

// program        → statement* EOF ;
// statement      → exprStmt
//                | printStmt
//                | varDecl;

// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
// exprStmt       → expression ";" ;
// printStmt      → "print" expression ";" ;
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

// expression     → comma
// comma          → equality ( (",") equality)*
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")"
//                | IDENTIFIER ;
