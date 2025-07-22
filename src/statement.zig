const std = @import("std");

const E = @import("expression.zig");
const Expr = E.Expr;

const T = @import("token.zig");
const Token = T.Token;

pub const Statement = union(enum) {
    const Self = @This();
    expression: *Expr,
    print: *Print,
    varDeclaration: *VarDeclaration,

    pub fn evaluate(self: *Self, allocator: std.mem.Allocator) !?*Expr {
        switch (self.*) {
            .expression => {
                return try self.expression.evaluate(allocator);
            },
            .print => {
                try self.print.evaluate(allocator);
                return null;
            },
            .varDeclaration => {
                return null;
                // return self.varDeclaration;
            },
        }
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
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
                allocator.destroy(self);
            },
        }
    }
};

pub const Print = struct {
    const Self = @This();
    expression: *Expr,

    pub fn create(allocator: std.mem.Allocator, expr: *Expr) !*Statement {
        var printStmt = try allocator.create(Print);
        const stmt = try allocator.create(Statement);
        printStmt.expression = expr;
        stmt.* = .{ .print = printStmt };
        return stmt;
    }

    pub fn evaluate(self: *Self, allocator: std.mem.Allocator) !void {
        const expr: *Expr = try self.expression.evaluate(allocator);
        defer expr.deinit(allocator);
        switch (expr.*) {
            .binary => std.debug.print("{any}\n", .{expr.binary.*}),
            .grouping => std.debug.print("{any}\n", .{expr.grouping.*}),
            .unary => std.debug.print("{any}\n", .{expr.unary.*}),
            .literal => {
                switch (expr.literal.*) {
                    .number => std.debug.print("{d}\n", .{expr.literal.number}),
                    .string => std.debug.print("\"{s}\"\n", .{expr.literal.string}),
                    .boolean => std.debug.print("{any}\n", .{expr.literal.boolean}),
                    .nil => std.debug.print("nil\n", .{}),
                }
            },
            .variable => {
                std.debug.print("{any}\n", .{expr.variable.name});
            },
        }
    }
};

pub const VarDeclaration = struct {
    const Self = @This();
    name: Token,
    initializer: *Expr,

    pub fn create(allocator: std.mem.Allocator, name: Token, initializer: *Expr) !Statement {
        const varDecl = try allocator.create(VarDeclaration);
        varDecl.* = .{ .initializer = initializer, .name = name };

        const statement = try allocator.create(Statement);
        statement.* = .{ .varDeclaration = varDecl };
        return statement;
    }
};
