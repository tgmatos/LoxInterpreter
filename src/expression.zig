const std = @import("std");
const T = @import("token.zig");
const Token = T.Token;
const ValueLiteral = T.Literal;

pub const Expr = union(enum) {
    literal: Literal,
    unary: Unary,
    binary: Binary,
    grouping: Grouping,
};

pub const Literal = union(enum) {
    number: ValueLiteral,
    string: ValueLiteral,
    boolean: bool,
    nil: void,
};

pub const Grouping = struct {
    expr: *Expr,
};

pub const Unary = struct {
    operator: Token,
    right: *Expr,
};

pub const Binary = struct {
    exprLeft: *Expr,
    operator: Token,
    exprRight: *Expr,
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
