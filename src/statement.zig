const std = @import("std");

const E = @import("expression.zig");
const Expr = E.Expr;

const T = @import("token.zig");
const Token = T.Token;

const Util = @import("util.zig");

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
                allocator.destroy(self.varDeclaration);
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
        Util.printExpr(expr);
    }
};

pub const VarDeclaration = struct {
    const Self = @This();
    name: Token,
    initializer: *Expr,

    pub fn create(allocator: std.mem.Allocator, name: Token, initializer: *Expr) !*Statement {
        const varDecl = try allocator.create(VarDeclaration);
        varDecl.* = .{ .initializer = initializer, .name = name };

        const statement = try allocator.create(Statement);
        statement.* = .{ .varDeclaration = varDecl };
        return statement;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) !void {
        std.debug.print("Deiniting varDcle\n", .{});
        self.initializer.deinit(allocator);
        allocator.destroy(self);
    }
};
