const std = @import("std");

const token = @import("token.zig");
const Scanner = @import("scanner.zig").Scanner;
const Parser = @import("parser.zig").Parser;
const Util = @import("util.zig");

const E = @import("expression.zig");
const Expr = E.Expr;
const Binary = E.Binary;
const Unary = E.Unary;
const Grouping = E.Grouping;
const Literal = E.Literal;

const Env = @import("environment.zig");
const Environment = Env.Environment;

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
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator: std.mem.Allocator = gpa.allocator();

    var stdin: std.fs.File = std.fs.File.stdin();
    var stdin_buffer: [1024 * 1024]u8 = undefined;
    var reader = stdin.readerStreaming(&stdin_buffer);
    var input: *std.io.Reader = &reader.interface;
    std.debug.print("> ", .{});

    // Attention here, maybe the env should not be "global"
    const env: *Environment = try Environment.init(allocator);
    defer env.deinit(allocator);

    while (input.takeDelimiterInclusive('\n')) |x| {
        var scanner = try Scanner.init(allocator);
        const ts = try scanner.scanTokens(x);
        defer scanner.deinit();

        var parser: Parser = Parser.init(allocator, &ts);
        var statementList = parser.parser() catch |err| {
            Util.printError(err);
            std.debug.print("> ", .{});
            continue;
        };
        defer statementList.deinit(allocator);

        for (statementList.items) |stmt| {
            defer stmt.deinit(allocator);

            switch (stmt.*) {
                .print => {
                    _ = stmt.evaluate(allocator, env) catch |err| {
                        std.log.err("\x1b[31m{any}\x1b[0m", .{err});
                        std.debug.print("> ", .{});
                    };
                    continue;
                },
                .expression => {
                    const b = stmt.evaluate(allocator, env) catch |err| {
                        std.log.err("\x1b[31m{any}\x1b[0m", .{err});
                        std.debug.print("> ", .{});
                        continue;
                    };
                    defer b.?.deinit(allocator);
                    Util.printExpr(b.?);
                },
                .varDeclaration => {
                    _ = stmt.evaluate(allocator, env) catch |err| {
                        std.log.err("\x1b[31m{any}\x1b[0m", .{err});
                        std.debug.print("> ", .{});
                    };
                    continue;
                },
            }
        }
        std.debug.print("> ", .{});
    } else |errs| switch (errs) {
        error.StreamTooLong,
        error.EndOfStream,
        error.ReadFailed,
        => |e| return e,
    }
}

fn lexError(line: u32, message: []const u8) !void {
    try report(line, "", message);
}

fn report(line: u32, where: []u8, message: []const u8) !void {
    std.debug.print("[line {d}] Error {s}: {s}", .{ line, where, message });
}
