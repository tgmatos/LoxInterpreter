const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;

const E = @import("expression.zig");
const Expr = E.Expr;

const S = @import("statement.zig");
const Statement = S.Statement;

pub const EnvironmentError = error{
    UndefinedVariable,
};

pub const Environment = struct {
    const Self = @This();
    map: StringHashMap(*Expr),

    pub fn init(allocator: Allocator) !*Environment {
        const map = StringHashMap(*Expr).init(allocator);
        const env: *Environment = try allocator.create(Environment);
        env.* = .{ .map = map };
        return env;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        var iterator = self.map.iterator();
        while (iterator.next()) |obj| {
            const key = obj.key_ptr.*;
            const value = obj.value_ptr.*;

            allocator.free(key);
            value.deinit(allocator);
        }

        self.map.deinit();
        allocator.destroy(self);
    }

    pub fn define(self: *Self, allocator: Allocator, name: []u8, value: *Expr) !void {
        if (self.map.contains(name)) {
            const key = self.map.getKey(name).?;
            const previousValue: *Expr = self.map.get(name).?;

            allocator.free(name);
            previousValue.deinit(allocator);

            try self.map.put(key, value);
        } else {
            try self.map.put(name, value);
        }
    }

    pub fn get(self: *Self, name: []u8) !*Expr {
        if (self.map.contains(name)) {
            return self.map.get(name).?;
        }

        return EnvironmentError.UndefinedVariable;
    }
};
