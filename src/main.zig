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
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn();
    var buf_reader = std.io.bufferedReader(stdin.reader());
    var input = buf_reader.reader();
    var line: [1024 * 1024]u8 = undefined;
    std.debug.print("> ", .{});

    while (try input.readUntilDelimiterOrEof(&line, '\n')) |x| {
        var scanner = try Scanner.init(allocator);
        const ts = try scanner.scanTokens(x);
        defer scanner.deinit();

        var parser: Parser = Parser.init(allocator, &ts);
        const statementList = parser.parser() catch |err| {
            Util.printError(err);
            std.debug.print("> ", .{});
            continue;
        };
        defer statementList.deinit();

        for (statementList.items) |stmt| {
            defer stmt.deinit(allocator);

            switch (stmt.*) {
                .print => {
                    _ = stmt.evaluate(allocator) catch |err| {
                        Util.printError(err);
                    };
                    continue;
                },
                .expression => {
                    const b = stmt.evaluate(allocator) catch |err| {
                        Util.printError(err);
                        std.debug.print("> ", .{});
                        continue;
                    };

                    const a = b.?;
                    defer a.deinit(allocator);
                    Util.printExpr(a);
                },
                .varDeclaration => {
                    Util.printExpr(stmt.varDeclaration.initializer);
                },
            }
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
