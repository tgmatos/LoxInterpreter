const std = @import("std");
const E = @import("expression.zig");
const Expression = E.Expr;

pub const Statement = union(enum) {
    const Self = @This();
    expression: *Expression,
    print: *Print,

    pub fn evaluate(self: *Self, allocator: std.mem.Allocator) !void {
        switch (self.*) {
            .expression => {
                const lastExpr = self.expression;
                defer lastExpr.deinit(allocator);

                const expr = try self.expression.evaluate(allocator);
                self.expression = expr;
                return;
            },
            .print => {
                try self.print.evaluate(allocator);
                return;
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
        }
    }
};

pub const Print = struct {
    const Self = @This();
    expression: *Expression,

    pub fn create(allocator: std.mem.Allocator, expr: *Expression) !*Statement {
        var printStmt = try allocator.create(Print);
        const stmt = try allocator.create(Statement);
        printStmt.expression = expr;
        stmt.* = .{ .print = printStmt };
        return stmt;
    }

    pub fn evaluate(self: *Self, allocator: std.mem.Allocator) !void {
        const expr: *Expression = try self.expression.evaluate(allocator);
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
        }
    }
};
