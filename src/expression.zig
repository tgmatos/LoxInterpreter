const std = @import("std");
const Allocator = std.mem.Allocator;

const Util = @import("util.zig");

const To = @import("token.zig");
const Token = To.Token;
const ValueLiteral = To.Literal;

const Env = @import("environment.zig");
const Environment = Env.Environment;
const UndefinedVariable = Env.EnvironmentError.UndefinedVariable;

const activeTag = std.meta.activeTag;
const print = std.debug.print;

const OutOfMemory = std.mem.Allocator.Error.OutOfMemory;
const RuntimeError = error{
    InvalidOperand,
    InvalidExpr,
    OutOfMemory,
    VariableDontExist,
    UndefinedVariable,
};

pub const Expr = union(enum) {
    const Self = @This();
    literal: *Literal,
    unary: *Unary,
    binary: *Binary,
    grouping: *Grouping,
    variable: *Variable,
    assign: *Assign,

    pub fn evaluate(expr: *Self, allocator: Allocator, env: *Environment) RuntimeError!*Expr {
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
            .grouping => return try Grouping.evaluate(expr.grouping.*, allocator, env),
            .unary => return try Unary.evaluate(expr.unary.*, allocator, env),
            .binary => return try Binary.evaluate(expr.binary.*, allocator, env),
            .variable => {
                // Todo: return a copy(?)
                return try Variable.evaluate(expr.variable.*, allocator, env);
            },
            .assign => {
                return try Assign.evaluate(expr.assign.*, allocator, env);
            },
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
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
                v.operator.deinit(allocator);
                v.right.deinit(allocator);
                allocator.destroy(v);
            },
            .binary => |v| {
                v.operator.deinit(allocator);
                v.exprLeft.deinit(allocator);
                v.exprRight.deinit(allocator);
                allocator.destroy(v);
            },

            .variable => |v| {
                v.name.deinit(allocator);
                allocator.destroy(v);
            },
            .assign => |v| {
                v.expr.deinit(allocator);
                v.name.deinit(allocator);
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

    pub fn create(allocator: Allocator, value: Literal) !*Expr {
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

    pub fn create(allocator: Allocator, expression: *Expr) !*Expr {
        var grouping: *Grouping = try allocator.create(Grouping);
        const expr: *Expr = try allocator.create(Expr);
        grouping.expr = expression;
        expr.* = .{ .grouping = grouping };
        return expr;
    }

    pub fn evaluate(grouping: Grouping, allocator: Allocator, env: *Environment) RuntimeError!*Expr {
        return try grouping.expr.evaluate(allocator, env);
    }
};

pub const Unary = struct {
    operator: *Token,
    right: *Expr,

    pub fn create(allocator: Allocator, operator: *Token, right: *Expr) !*Expr {
        const token: *Token = try copyToken(allocator, operator);
        var unary: *Unary = try allocator.create(Unary);
        const expr: *Expr = try allocator.create(Expr);
        unary.operator = token;
        unary.right = right;
        expr.* = .{ .unary = unary };
        return expr;
    }

    pub fn evaluate(unary: Unary, allocator: Allocator, env: *Environment) RuntimeError!*Expr {
        const right: *Expr = try unary.right.evaluate(allocator, env);
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
    operator: *Token,
    exprRight: *Expr,

    pub fn create(allocator: Allocator, left: *Expr, operator: *Token, right: *Expr) !*Expr {
        const token: *Token = try copyToken(allocator, operator);
        var binary: *Binary = try allocator.create(Binary);
        const expr: *Expr = try allocator.create(Expr);
        binary.exprLeft = left;
        binary.operator = token;
        binary.exprRight = right;
        expr.* = .{ .binary = binary };
        return expr;
    }

    pub fn evaluate(binary: Binary, allocator: Allocator, env: *Environment) RuntimeError!*Expr {
        const left: *Expr = try binary.exprLeft.evaluate(allocator, env);
        defer left.deinit(allocator);

        const right: *Expr = try binary.exprRight.evaluate(allocator, env);
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
                            const result = std.fmt.float.render(&buf, right.literal.number, .{ .mode = .decimal }) catch |err| {
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
                            const result = std.fmt.float.render(&buf, left.literal.number, .{ .mode = .decimal }) catch |err| {
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
    name: *Token,

    pub fn create(allocator: Allocator, name: *Token) !*Expr {
        // TODO: Clean Variable to not leak
        const token: *Token = try copyToken(allocator, name);

        var variable: *Variable = try allocator.create(Variable);
        variable.name = token;

        const expr: *Expr = try allocator.create(Expr);
        expr.* = .{ .variable = variable };
        return expr;
    }

    pub fn evaluate(variable: Variable, allocator: Allocator, env: *Environment) !*Expr {
        const expr = try env.get(variable.name.lexeme);
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
            else => return RuntimeError.VariableDontExist,
        }

        return RuntimeError.VariableDontExist;
    }
};

pub const Assign = struct {
    name: *Token,
    expr: *Expr,

    pub fn create(allocator: Allocator, token: *Token, expr: *Expr, env: *Environment) !*Expr {
        const assing: *Assign = allocator.create(Assign);
        const newExpr: *Expr = allocator.create(Expr);
        const cpyToken: *Token = copyToken(allocator, token);

        const evaluated_expr: *Expr = expr.evaluate(allocator, env);

        assing.* = .{ .expr = evaluated_expr, .name = cpyToken };
        newExpr.* = .{ .assign = assing };
        return assing;
    }

    pub fn evaluate(assign: Assign, allocator: Allocator, env: *Environment) !*Expr {
        const str = try allocator.alloc(u8, assign.name.lexeme.len);
        errdefer allocator.free(str);
        @memcpy(str, assign.name.lexeme);

        if (activeTag(assign.expr.*) == Expr.literal) {
            const cpyExpr: *Expr = try copyExpr(allocator, assign.expr);
            try env.define(allocator, str, cpyExpr);

            const expr: *Expr = try env.get(assign.name.lexeme);
            const result: *Expr = try copyExpr(allocator, expr);
            return result;
        }

        const evalResult: *Expr = try assign.expr.evaluate(allocator, env);
        try env.define(allocator, str, evalResult);
        const expr: *Expr = try env.get(assign.name.lexeme);
        const result: *Expr = try copyExpr(allocator, expr);
        return result;
    }
};

fn copyToken(allocator: Allocator, source: *Token) !*Token {
    const dst: *Token = try allocator.create(Token);

    const lexeme = try allocator.alloc(u8, source.lexeme.len);
    @memcpy(lexeme, source.lexeme);

    var literalNullable: ?*ValueLiteral = null;

    if (source.literal) |l| {
        literalNullable = try allocator.create(ValueLiteral);

        switch (l.*) {
            .string => {
                const strLiteral = try allocator.alloc(u8, l.string.len);
                @memcpy(strLiteral, l.string);
                literalNullable.?.* = .{ .string = strLiteral };
            },
            .number => {
                literalNullable.?.* = .{ .number = l.number };
            },
        }
    }

    dst.* = .{ .kind = source.kind, .lexeme = lexeme, .line = source.line, .literal = literalNullable };
    return dst;
}

fn copyExpr(allocator: Allocator, expr: *Expr) !*Expr {
    switch (expr.*) {
        .literal => {
            const literal: *Literal = try allocator.create(Literal);
            const cpyExpr: *Expr = try allocator.create(Expr);

            if (activeTag(expr.literal.*) == Literal.string) {
                const str = try allocator.alloc(u8, expr.literal.string.len);
                @memcpy(str, expr.literal.string);
                literal.* = expr.literal.*;
                literal.*.string = str;
            } else {
                literal.* = expr.literal.*;
            }

            cpyExpr.* = .{ .literal = literal };
            return cpyExpr;
        },

        else => {
            return RuntimeError.InvalidExpr;
        },
    }
}

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
