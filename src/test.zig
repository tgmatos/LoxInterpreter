const std = @import("std");
const Token = @import("token.zig");
const Scanner = @import("scanner.zig").Scanner;

test "Test Parse Error" {
    std.debug.print("\nTest error handling on single char parser\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const source = "(+)^";
    var scanner = try Scanner.init(allocator, source);
    defer scanner.deinit();

    const ts = try scanner.scanTokens();
    _ = ts;
}

test "Test All Possibilities" {
    std.debug.print("\nTest all possibilities\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const source =
        \\// this is a comment
        \\(( )){} // grouping stuff
        \\!*+-/=<> <= == // operators
    ;

    var scanner = try Scanner.init(allocator, source);
    defer scanner.deinit();

    const ts = try scanner.scanTokens();
    for (ts.items) |item| {
        std.debug.print("{any}\n", .{item});
    }
}

test "Test Strings" {
    std.debug.print("\nTest Strings\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const source = "\"testando\"";

    var scanner = try Scanner.init(allocator, source);
    defer scanner.deinit();

    const ts = try scanner.scanTokens();
    for (ts.items) |item| {
        std.debug.print("{any}\n", .{item});
    }
}

test "Test String Error Parsing" {
    std.debug.print("\nTest Parser Error of Strings\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const source = "\"testando";

    var scanner = try Scanner.init(allocator, source);
    defer scanner.deinit();

    const ts = try scanner.scanTokens();
    for (ts.items) |item| {
        std.debug.print("{any}\n", .{item});
    }
}

test "Test Numbers" {
    std.debug.print("\nTest Number Parsing\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const source = "1.337";

    var scanner = try Scanner.init(allocator, source);
    defer scanner.deinit();

    const ts = try scanner.scanTokens();
    for (ts.items) |item| {
        std.debug.print("{any}\n", .{item});
    }
}

test "Test Keywords" {
    std.debug.print("\nTest Keywords Parsing\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const source = "1 - (2 * 3) < 4 == false";

    var scanner = try Scanner.init(allocator, source);
    defer scanner.deinit();

    const ts = try scanner.scanTokens();
    for (ts.items) |item| {
        std.debug.print("{any}\n", .{item});
    }
}
