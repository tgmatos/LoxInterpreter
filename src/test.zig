const std = @import("std");
const Token = @import("token.zig");
const S = @import("scanner.zig");
const Scanner = S.Scanner;
const TokenType = @import("token.zig").TokenType;
const P = @import("parser.zig");
const Parser = P.Parser;
const ParserError = P.ParserError;
const E = @import("expression.zig");
const Expr = E.Expr;
const Literal = E.Literal;
// const allocator = std.testing.allocator;
const allocator = std.heap.page_allocator;
const Env = @import("environment.zig");
const Environment = Env.Environment;

const Util = @import("util.zig");
const pretty = @import("pretty.zig");

test "Test Parse Error" {
    const source = "(+)^";
    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    _ = try scanner.scanTokens(source);
    //defer ts.deinit();
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

    const ts = try scanner.scanTokens(source);
    //defer ts.deinit();

    for (ts.items, expectedTokenTypes) |item, token| {
        try std.testing.expectEqual(token, item.kind);
    }
}

test "Test Strings" {
    const source = "\"testando\"";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(source);
    //defer ts.deinit();

    try std.testing.expectEqual(@as(usize, 2), ts.items.len);
    try std.testing.expectEqual(TokenType.STRING, ts.items[0].kind);
    try std.testing.expectEqualStrings("testando", ts.items[0].literal.?.string);
}

test "Test String Error Parsing" {
    const source = "\"testando;";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(source);
    //defer ts.deinit();

    const expectedTokenTypes = [_]TokenType{ .ERROR, .EOF };
    for (ts.items, expectedTokenTypes) |item, token| {
        try std.testing.expectEqual(token, item.kind);
    }
}

test "Test Numbers" {
    const source = "1.337;";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(source);

    try std.testing.expectEqual(@as(usize, 3), ts.items.len);
    try std.testing.expectEqual(TokenType.NUMBER, ts.items[0].kind);
    try std.testing.expectEqual(1.337, ts.items[0].literal.?.number);
}

test "Test Keywords" {
    const source = "5 - (2 * 3) < 4 == false;";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(source);

    const expectedTokenTypes = [_]TokenType{ .NUMBER, .MINUS, .LEFT_PAREN, .NUMBER, .STAR, .NUMBER, .RIGHT_PAREN, .LESS, .NUMBER, .EQUAL_EQUAL, .FALSE, .SEMICOLON, .EOF };

    try std.testing.expectEqual(expectedTokenTypes.len, ts.items.len);
    for (ts.items, expectedTokenTypes) |item, token| {
        try std.testing.expectEqual(token, item.kind);
    }

    // Check the values of the tokens
    try std.testing.expectEqual(@as(f64, 5.0), ts.items[0].literal.?.number);
    try std.testing.expectEqual(@as(f64, 2.0), ts.items[3].literal.?.number);
    try std.testing.expectEqual(@as(f64, 3.0), ts.items[5].literal.?.number);
    try std.testing.expectEqual(@as(f64, 4.0), ts.items[8].literal.?.number);
}

test "Check Binary Evaluation" {
    const source = "((2 + 2) - (1 + 2)) * (3 * 2);";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(source);

    var parser: Parser = Parser.init(allocator, &ts);
    const expr = try parser.parser();

    const env: *Environment = try Env.Environment.init(allocator);

    for (expr.items) |e| {
        const a = try e.evaluate(allocator, env);
        std.debug.assert(a.?.literal.number == 6);
    }
}

test "Check Concatenation Evaluation" {
    const source = "\"testando \" + \"isso\";";

    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(source);

    var parser: Parser = Parser.init(allocator, &ts);
    const expr = try parser.parser();

    const env: *Environment = try Env.Environment.init(allocator);

    for (expr.items) |e| {
        const a = try e.evaluate(allocator, env);
        std.debug.assert(std.mem.eql(u8, a.?.literal.string, "testando isso"));
    }
}

test "Check truthy" {
    const result = try evaluateSource("(2 >= 2) == (3 < 1);");
    std.debug.assert(result.?.literal.boolean == false);
}

test "Check Unary Negation" {
    const result = try evaluateSource("-5;");
    std.debug.assert(result.?.literal.number == -5);
}

test "Check Grouping Evaluation" {
    const result = try evaluateSource("(3 + 2) * 2;");
    std.debug.assert(result.?.literal.number == 10);
}

test "Check Binary Subtraction" {
    const result = try evaluateSource("10 - 4;");
    std.debug.assert(result.?.literal.number == 6);
}

test "Check Binary Multiplication" {
    const result = try evaluateSource("3 * 4;");
    std.debug.assert(result.?.literal.number == 12);
}

test "Check true == true" {
    const result = try evaluateSource("(true == true);");
    std.debug.assert(result.?.literal.boolean == true);
}

test "Check Complex Precedence" {
    const result = try evaluateSource("5 + 2 * 10 / (2 + 3);");
    std.debug.assert(result.?.literal.number == 9);
}

test "Check String Equality" {
    const result = try evaluateSource("\"hello\" == \"hello\";");
    std.debug.assert(result.?.literal.boolean == true);
}

test "Variable Declaration" {
    const source = "var a = 10; a;";
    const result = try evaluateSource(source);

    std.debug.assert(result.?.literal.number == 10);
}

test "Multiple Variables" {
    const source =
        \\var width = 10;
        \\var height = 20;
        \\width * height;
    ;
    const result = try evaluateSource(source);

    std.debug.assert(result.?.literal.number == 200);
}

test "Uninitialized Variable" {
    const source = "var a; a;";
    const result = try evaluateSource(source);

    // This assumes your Value type has a 'nil' or 'none' literal type
    // Adjust based on your specific implementation

    std.debug.assert(result.?.literal.* == .nil);
}

// test "Variable Shadowing" {
//     const source =
//         \\var global = 1;
//         \\{
//         \\  var global = 2;
//         \\}
//         \\global;
//     ;
//     const result = try evaluateSource(source);

//     // The inner 'global' should disappear, leaving the original '1'
//     std.debug.assert(result.?.literal.number == 1);
// }

test "Variable Reassignment" {
    const source = "var x = 5; x = x + 10;";
    const result = try evaluateSource(source);

    std.debug.assert(result.?.literal.number == 15);
}

fn evaluateSource(source: []const u8) !?*Expr {
    var scanner = try Scanner.init(allocator);
    defer scanner.deinit();

    const ts = try scanner.scanTokens(source);

    var parser: Parser = Parser.init(allocator, &ts);
    const expr = try parser.parser();

    const env = try Env.Environment.init(allocator);
    // If your environment needs to be freed, uncomment the next line:
    defer env.deinit(allocator);

    var last_result: ?*Expr = null;
    for (expr.items) |e| {
        last_result = try e.evaluate(allocator, env);
    }
    return last_result;
}

// The *specifier* has several options for types:
// - `x` and `X`: output numeric value in hexadecimal notation
// - `s`:
//   - for pointer-to-many and C pointers of u8, print as a C-string using zero-termination
//   - for slices of u8, print the entire slice as a string without zero-termination
// - `e`: output floating point value in scientific notation
// - `d`: output numeric value in decimal notation
// - `b`: output integer value in binary notation
// - `o`: output integer value in octal notation
// - `c`: output integer as an ASCII character. Integer type must have 8 bits at max.
// - `u`: output integer as an UTF-8 sequence. Integer type must have 21 bits at max.
// - `?`: output optional value as either the unwrapped value, or `null`; may be followed by a format specifier for the underlying value.
// - `!`: output error union value as either the unwrapped value, or the formatted error value; may be followed by a format specifier for the underlying value.
// - `*`: output the address of the value instead of the value itself.
// - `any`: output a value of any type using its default format.
