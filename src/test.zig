const std = @import("std");
const Token = @import("token.zig");
const S = @import("scanner.zig");
const Scanner = S.Scanner;
const TokenType = @import("token.zig").TokenType;
const P = @import("parser.zig");
const Parser = P.Parser;
const ParserError = P.ParserError;
const Expresssion = @import("expression.zig");
const allocator = std.testing.allocator;

test "Test Parse Error" {
    const source = "(+)^";
    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(allocator, source);
    defer ts.deinit();
}

test "Test All Possibilities" {
    const source =
        \\// this is a comment
        \\(( )){} // grouping stuff
        \\!*+-/=<> <= == // operators
        \\"testando" // string
        \\1.337 // number
        \\false // keyword
    ;

    const expectedTokenTypes = [_]TokenType{ .LEFT_PAREN, .LEFT_PAREN, .RIGHT_PAREN, .RIGHT_PAREN, .LEFT_BRACE, .RIGHT_BRACE, .BANG, .STAR, .PLUS, .MINUS, .SLASH, .EQUAL, .LESS, .GREATER, .LESS_EQUAL, .EQUAL_EQUAL, .STRING, .NUMBER, .FALSE, .EOF };

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(allocator, source);
    defer ts.deinit();

    for (ts.items, expectedTokenTypes) |item, token| {
        try std.testing.expectEqual(token, item.kind);
    }
}

test "Test Strings" {
    const source = "\"testando\"";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(allocator, source);
    defer ts.deinit();

    try std.testing.expectEqual(@as(usize, 2), ts.items.len);
    try std.testing.expectEqual(TokenType.STRING, ts.items[0].kind);
    try std.testing.expectEqualStrings("testando", ts.items[0].literal.?.string);
}

test "Test String Error Parsing" {
    const source = "\"testando";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(allocator, source);
    defer ts.deinit();

    const expectedTokenTypes = [_]TokenType{ .ERROR, .EOF };
    for (ts.items, expectedTokenTypes) |item, token| {
        try std.testing.expectEqual(token, item.kind);
    }
}

test "Test Numbers" {
    const source = "1.337";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(allocator, source);
    defer ts.deinit();

    try std.testing.expectEqual(@as(usize, 2), ts.items.len);
    try std.testing.expectEqual(TokenType.NUMBER, ts.items[0].kind);
    try std.testing.expectEqual(1.337, ts.items[0].literal.?.number);
}

test "Test Keywords" {
    const source = "1 - (2 * 3) < 4 == false";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(allocator, source);
    defer ts.deinit();

    const expectedTokenTypes = [_]TokenType{ .NUMBER, .MINUS, .LEFT_PAREN, .NUMBER, .STAR, .NUMBER, .RIGHT_PAREN, .LESS, .NUMBER, .EQUAL_EQUAL, .FALSE, .EOF };

    try std.testing.expectEqual(expectedTokenTypes.len, ts.items.len);
    for (ts.items, expectedTokenTypes) |item, token| {
        try std.testing.expectEqual(token, item.kind);
    }

    // Check the values of the tokens
    try std.testing.expectEqual(@as(f64, 1.0), ts.items[0].literal.?.number);
    try std.testing.expectEqual(@as(f64, 2.0), ts.items[3].literal.?.number);
    try std.testing.expectEqual(@as(f64, 3.0), ts.items[5].literal.?.number);
    try std.testing.expectEqual(@as(f64, 4.0), ts.items[8].literal.?.number);
}

// test "Test Parser" {
//     std.debug.print("\nTest Parser\n", .{});

//     const source = "1 - (2 + 3) < 4 == false";
//     //const source = "== false";

//     var scanner = try Scanner.init(allocator);
//     defer scanner.deinit();

//     const ts = try scanner.scanTokens(allocator, source);
//     defer ts.deinit();

//     var parser: Parser = Parser.init(&ts);
//     const p = parser.parser();
//     std.debug.print("{any}\n", .{p});
// }

// test "Test Right Parenthesis Not Present" {
//     std.debug.print("\nTest Right Parenthesis Not Present\n", .{});
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer _ = gpa.deinit();

//     const allocator = gpa.allocator();

//     const source = "1 - (2 + 3 < 4 == false";
//     //const source = "== false";

//     var scanner = try Scanner.init(allocator);
//     defer scanner.deinit();

//     const ts = try scanner.scanTokens(allocator, source);
//     defer ts.deinit();

//     var parser: Parser = Parser.init(&ts);
//     const p = parser.parser();

//     std.debug.print("{any}\n", .{p});
// }

test "Test Missing Left Hand Operator in Parser" {
    const source = "== false";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(allocator, source);
    defer ts.deinit();

    var parser: Parser = Parser.init(&ts);
    try std.testing.expectError(ParserError.MissingLeftOperandError, parser.parser());
}
