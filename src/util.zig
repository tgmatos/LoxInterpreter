const std = @import("std");
const S = @import("scanner.zig");
const T = @import("token.zig");
const P = @import("parser.zig");
const E = @import("expression.zig");

const Scanner = S.Scanner;
const TokenType = T.TokenType;

const Parser = P.Parser;
const ParserError = P.ParserError;

const Expr = E.Expr;
const Literal = E.Literal;
const Unary = E.Unary;
const Binary = E.Binary;
const Grouping = E.Grouping;

const allocator = std.testing.allocator;

// pub fn printAST(expr: *Expr) void {
//     switch (expr.*) {
//         .binary => |value| {
//             printAST(value.exprLeft);
//             std.debug.print("{any}\n", .{value.operator});
//             printAST(value.exprRight);
//         },
//         .literal => |value| {
//             switch (value.*) {
//                 .number => |literal| std.debug.print("{any}\n", .{literal}),
//                 .string => |literal| std.debug.print("{any}\n", .{literal}),
//                 .boolean => |literal| std.debug.print("{any}\n", .{literal}),
//                 .nil => std.debug.print("null\n", .{}),
//             }
//             std.debug.print("{any}\n", .{value});
//         },
//         .unary => |value| {
//             std.debug.print("{any}\n", .{value.operator});
//             printAST(value.right);
//         },
//         .grouping => |value| {
//             //std.debug.print("{any}\n", .{expr.grouping});
//             printAST(value.expr);
//         },
//         //else => std.debug.print("Error\n"),
//     }
// }

pub fn printAST(expr: *Expr) void {
    switch (expr.*) {
        .literal => |value| printLiteral(value),
        .unary => |value| printUnary(value),
        .binary => |value| printBinary(value),
        .grouping => |value| printGrouping(value),
    }
}

fn printLiteral(literal: *Literal) void {
    std.debug.print("{any}\n", .{literal});
}

fn printUnary(unary: *Unary) void {
    std.debug.print("{any}\n", .{unary.operator});
    printAST(unary.right);
}

fn printBinary(binary: *Binary) void {
    printAST(binary.exprLeft);
    std.debug.print("{any}\n", .{binary.operator});
    printAST(binary.exprRight);
}

fn printGrouping(grouping: *Grouping) void {
    //std.debug.print("==>{any}\n", .{grouping});
    printAST(grouping.expr);
    // std.debug.print("==>{any}\n", .{grouping.expr});
}
