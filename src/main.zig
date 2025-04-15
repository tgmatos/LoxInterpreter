const std = @import("std");
const token = @import("token.zig");
const Scanner = @import("scanner.zig").Scanner;
const Parser = @import("parser.zig").Parser;
const Util = @import("util.zig");

pub fn main() !void {
    try runPrompt();
}

fn runFile(path: []u8) !void {
    _ = try std.fs.openFileAbsolute(path, .{ .mode = .read_only });
}

fn run(source: []u8) void {
    _ = source;
}

fn runPrompt() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn();
    var buf_reader = std.io.bufferedReader(stdin.reader());
    var input = buf_reader.reader();
    var line: [2048]u8 = undefined;
    std.debug.print("> ", .{});

    while (try input.readUntilDelimiterOrEof(&line, '\n')) |x| {
        var scanner = try Scanner.init(allocator);
        defer scanner.deinit();

        const ts = try scanner.scanTokens(x);

        var parser: Parser = Parser.init(allocator, &ts);
        const expr = try parser.parser();
        defer expr.deinit(allocator);

        // Util.printAST(expr);

        const a = expr.evaluate(allocator) catch |err| {
            std.log.err("{any}", .{err});
            std.debug.print("> ", .{});
            continue;
        };
        defer a.deinit(allocator);

        switch (a.*) {
            .binary => std.debug.print("{any}\n", .{a.binary.*}),
            .grouping => std.debug.print("{any}\n", .{a.grouping.*}),
            .unary => std.debug.print("{any}\n", .{a.unary.*}),
            .literal => {
                switch (a.literal.*) {
                    .number => std.debug.print("{d}\n", .{a.literal.number}),
                    .string => std.debug.print("{s}\n", .{a.literal.string}),
                    .boolean => std.debug.print("{any}\n", .{a.literal.boolean}),
                    .nil => std.debug.print("nil\n", .{}),
                }
            },
        }
        std.debug.print("> ", .{});
    }
}

fn lexError(line: u32, message: []const u8) !void {
    try report(line, "", message);
}

fn report(line: u32, where: []u8, message: []const u8) !void {
    std.debug.print("[line {d}] Error {s}: {s}", .{ line, where, message });
}
