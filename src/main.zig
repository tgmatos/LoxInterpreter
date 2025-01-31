const std = @import("std");
const token = @import("token.zig");
const Scanner = @import("scanner.zig").Scanner;
const Parser = @import("parser.zig").Parser;

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // defer _ = gpa.deinit();

    // const allocator = gpa.allocator();

    // const source = "()";
    // var scanner = try Scanner.init(allocator);
    // defer scanner.deinit();

    // const ts = try scanner.scanTokens(allocator, source);
    // defer ts.deinit();
    // for (ts.items) |item| {
    //     std.debug.print("{any}\n", .{item});
    // }

    try runPrompt();
    // if (std.os.argv.len > 1) {
    //     std.debug.print("Usage: lox [script]\n", .{});
    //     std.os.linux.exit(64);
    // } else if (std.os.argv.len == 1) {
    //     runFile(std.os.argv[0]);
    // }
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
        std.debug.print("\n> ", .{});
        var scanner = try Scanner.init(allocator);
        defer scanner.deinit();

        const ts = try scanner.scanTokens(allocator, x);
        defer ts.deinit();

        for (ts.items) |item| {
            std.debug.print("{any}\n", .{item});
        }
    }
}

fn lexError(line: u32, message: []const u8) !void {
    try report(line, "", message);
}

fn report(line: u32, where: []u8, message: []const u8) !void {
    std.debug.print("[line {d}] Error {s}: {s}", .{ line, where, message });
}
