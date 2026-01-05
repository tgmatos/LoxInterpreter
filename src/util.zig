const std = @import("std");

const ParserError = @import("parser.zig").ParserError;

const E = @import("expression.zig");
const Expr = E.Expr;
const Literal = E.Literal;
const Unary = E.Unary;
const Binary = E.Binary;
const Grouping = E.Grouping;
const Variable = E.Variable;
const Assign = E.Assign;

const S = @import("statement.zig");
const Statement = S.Statement;

pub fn printStatement(stmt: *Statement) void {
    switch (stmt.*) {
        .expression => printExpr(stmt.expression),
        .print => printExpr(stmt.print.expression),
        .varDeclaration => printExpr(stmt.varDeclaration.initializer),
    }
}

pub fn printExpr(expr: *Expr) void {
    switch (expr.*) {
        .binary => std.debug.print("\x1b[32m{any}\x1b[0m\n", .{expr.binary.*}),
        .grouping => std.debug.print("\x1b[32m{any}\x1b[0m\n", .{expr.grouping.*}),
        .unary => std.debug.print("\x1b[32m{any}\x1b[0m\n", .{expr.unary.*}),
        .literal => {
            switch (expr.literal.*) {
                .number => std.debug.print("\x1b[32m{d}\x1b[0m\n", .{expr.literal.number}),
                .string => std.debug.print("\x1b[32m\"{s}\"\x1b[0m\n", .{expr.literal.string}),
                .boolean => std.debug.print("\x1b[32m{any}\x1b[0m\n", .{expr.literal.boolean}),
                .nil => std.debug.print("\x1b[32mnil\x1b[0m\n", .{}),
            }
        },
        .variable => std.debug.print("\x1b[32m{any}\x1b[0m\n", .{expr.variable.name}),
        .assign => std.debug.print("\x1b[32m{any}\x1b[0m\n", .{expr.assign.name}),
    }
}

pub fn printError(err: ParserError) void {
    std.log.err("\x1b[31m{any}\x1b[0m", .{err});
}

pub fn printAST(expr: *Expr) void {
    switch (expr.*) {
        .literal => |value| printLiteral(value),
        .unary => |value| printUnary(value),
        .binary => |value| printBinary(value),
        .grouping => |value| printGrouping(value),
        .variable => |value| printVariable(value),
    }
}

fn printLiteral(literal: *Literal) void {
    std.debug.print("{any}\n", .{literal.*});
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
    std.debug.print("\n\nGrouping: {any}\n\n", .{grouping.expr.*});
    printAST(grouping.expr);
}

fn printVariable(variable: *Variable) void {
    std.debug.print("{any}\n", .{variable.name.*});
}

fn printAssign(assign: *Assign) void {
    std.debug.print("{any}\n", .{assign.name.*});
    printAST(assign.expr.*);
}
