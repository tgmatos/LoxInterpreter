const std = @import("std");
const Allocator = std.mem.Allocator;
const activeTag = std.meta.activeTag;

const E = @import("expression.zig");
const Expr = E.Expr;
const Literal = E.Literal;

const Env = @import("environment.zig");
const Environment = Env.Environment;

const T = @import("token.zig");
const Token = T.Token;

const Util = @import("util.zig");
const red = "\x1b[31m";
const green = "\x1b[32m";
const reset = "\x1b[0m";

pub const Statement = union(enum) {
    const Self = @This();
    expression: *Expr,
    print: *Print,
    varDeclaration: *VarDeclaration,

    pub fn evaluate(self: *Self, allocator: Allocator, env: *Environment) !?*Expr {
        switch (self.*) {
            .expression => {
                return try self.expression.evaluate(allocator, env);
            },
            .print => {
                try self.print.evaluate(allocator, env);
                return null;
            },
            .varDeclaration => {
                const initializer: *Expr = self.varDeclaration.initializer;
                if (initializer.* == .binary) {
                    std.log.debug("{s}Binary:{s}{s} {any}{s}", .{ red, reset, green, initializer.binary.*, reset });
                }

                const value: *Expr = try initializer.evaluate(allocator, env);
                const strCopied: []u8 = try allocator.alloc(u8, self.varDeclaration.name.lexeme.len);
                @memcpy(strCopied, self.varDeclaration.name.lexeme);

                _ = try env.define(allocator, strCopied, value);
                return null;
            },
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        switch (self.*) {
            .expression => {
                self.expression.deinit(allocator);
                allocator.destroy(self);
            },
            .print => {
                self.print.expression.deinit(allocator);
                allocator.destroy(self.print);
                allocator.destroy(self);
            },
            .varDeclaration => {
                self.varDeclaration.initializer.deinit(allocator);
                allocator.destroy(self.varDeclaration);
                allocator.destroy(self);
            },
        }
    }
};

pub const Print = struct {
    const Self = @This();
    expression: *Expr,

    pub fn create(allocator: Allocator, expr: *Expr) !*Statement {
        var printStmt: *Print = try allocator.create(Print);
        const stmt: *Statement = try allocator.create(Statement);
        printStmt.expression = expr;
        stmt.* = .{ .print = printStmt };
        return stmt;
    }

    pub fn evaluate(self: *Self, allocator: Allocator, env: *Environment) !void {
        const expr: *Expr = try self.expression.evaluate(allocator, env);
        defer expr.deinit(allocator);
        Util.printExpr(expr);
    }
};

pub const VarDeclaration = struct {
    const Self = @This();
    name: *Token,
    initializer: *Expr,

    pub fn create(allocator: Allocator, name: *Token, initializer: *Expr) !*Statement {
        const varDecl: *VarDeclaration = try allocator.create(VarDeclaration);
        // Attention: I don't remember when name:*Token will be freed
        varDecl.* = .{ .initializer = initializer, .name = name };

        const statement: *Statement = try allocator.create(Statement);
        statement.* = .{ .varDeclaration = varDecl };
        return statement;
    }

    pub fn deinit(self: *Self, allocator: Allocator) !void {
        self.initializer.deinit(allocator);
        allocator.destroy(self);
    }
};
