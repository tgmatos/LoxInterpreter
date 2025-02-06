const std = @import("std");
const To = @import("token.zig");
const Token = To.Token;
const ValueLiteral = To.Literal;

pub const Expr = union(enum) {
    literal: *Literal,
    unary: *Unary,
    binary: *Binary,
    grouping: *Grouping,
};

// pub const LiteralType = enum {
//     number,
//     string,
//     boolean,
//     nil,
// };

pub const Literal = union(enum) {
    number: ValueLiteral,
    string: ValueLiteral,
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
