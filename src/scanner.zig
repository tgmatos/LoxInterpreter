const std = @import("std");
const T = @import("token.zig");
const Token = T.Token;
const TokenType = T.TokenType;
const Literal = T.Literal;

pub const Scanner = struct {
    const Self = @This();
    source: []const u8,
    tokens: std.ArrayList(Token),
    keywords: std.StringHashMap(TokenType),
    start: u32,
    current: u32,
    line: u32,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Scanner {
        var hashmap = std.StringHashMap(TokenType).init(allocator);
        try hashmap.put("and", TokenType.AND);
        try hashmap.put("class", TokenType.CLASS);
        try hashmap.put("else", TokenType.ELSE);
        try hashmap.put("false", TokenType.FALSE);
        try hashmap.put("for", TokenType.FOR);
        try hashmap.put("fun", TokenType.FUN);
        try hashmap.put("if", TokenType.IF);
        try hashmap.put("nil", TokenType.NIL);
        try hashmap.put("or", TokenType.OR);
        try hashmap.put("print", TokenType.PRINT);
        try hashmap.put("return", TokenType.RETURN);
        try hashmap.put("super", TokenType.SUPER);
        try hashmap.put("this", TokenType.THIS);
        try hashmap.put("true", TokenType.TRUE);
        try hashmap.put("var", TokenType.VAR);
        try hashmap.put("while", TokenType.WHILE);

        return .{ .source = source, .start = 0, .current = 0, .line = 1, .tokens = std.ArrayList(Token).init(allocator), .keywords = hashmap };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.keywords.deinit();
    }

    pub fn scanTokens(self: *Self) !std.ArrayList(Token) {
        while (self.current < self.source.len) {
            self.start = self.current;
            try scanToken(self);
            self.current += 1;
        }

        const eof = Token.init(T.TokenType.EOF, "", null, self.line);
        try self.tokens.append(eof);
        return self.tokens;
    }

    fn scanToken(self: *Self) !void {
        const c: u8 = self.source[self.current];
        switch (c) {
            // One char tokens
            '(' => try self.addTokenSingle(TokenType.LEFT_PAREN, "("),
            ')' => try self.addTokenSingle(TokenType.RIGHT_PAREN, ")"),
            '{' => try self.addTokenSingle(TokenType.LEFT_BRACE, "{"),
            '}' => try self.addTokenSingle(TokenType.RIGHT_BRACE, "}"),
            ',' => try self.addTokenSingle(TokenType.COMMA, ","),
            '.' => try self.addTokenSingle(TokenType.DOT, "."),
            '-' => try self.addTokenSingle(TokenType.MINUS, "-"),
            '+' => try self.addTokenSingle(TokenType.PLUS, "+"),
            ';' => try self.addTokenSingle(TokenType.SEMICOLON, ";"),
            '*' => try self.addTokenSingle(TokenType.STAR, "*"),

            // Two char tokens
            '!' => {
                const token = if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG;
                try self.addTokenSingle(token, "!=");
            },
            '=' => {
                const token = if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG;
                try self.addTokenSingle(token, "==");
            },
            '<' => {
                const token = if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG;
                try self.addTokenSingle(token, "<=");
            },
            '>' => {
                const token = if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG;
                try self.addTokenSingle(token, ">=");
            },

            // Slash and comments
            '/' => {
                if (self.match('/')) {
                    while ((self.current < self.source.len) and (self.source[self.current] != '\n')) {
                        self.current += 1;
                    }
                } else {
                    try self.addTokenSingle(TokenType.SLASH, "/");
                }
            },

            // whitespaces
            ' ', '\r', '\t' => undefined,
            '\n' => self.line += 1,

            // Strings
            '"' => try self.parseString(),
            else => {
                if (isDigit(c)) {
                    try self.parseNumber();
                } else if (isAlpha(c)) {
                    try self.identifier();
                } else {
                    report(self.line, "", "Unexpected character.");
                }
            },
        }
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn isAlpha(c: u8) bool {
        return ((c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_');
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn addTokenSingle(self: *Self, kind: T.TokenType, character: []const u8) !void {
        const token = Token.init(kind, character, null, self.line);
        try self.tokens.append(token);
    }

    fn addToken(self: *Self, kind: T.TokenType, value: Literal) !void {
        const token = Token.init(kind, "", value, self.line);
        try self.tokens.append(token);
    }

    fn addKeywordToken(self: *Self, kind: T.TokenType) !void {
        const token = Token.init(kind, "", null, self.line);
        try self.tokens.append(token);
    }

    fn match(self: *Self, expected: u8) bool {
        if ((self.current >= self.source.len) or self.source[self.current + 1] != expected) {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn report(line: u32, where: []const u8, message: []const u8) void {
        std.debug.print("[line {d}] Error {s}: {s}\n", .{ line, where, message });
    }

    fn parseString(self: *Self) !void {
        while ((self.current + 1 < self.source.len) and (self.source[self.current + 1] != '"')) {
            if (self.source[self.current + 1] == '\n') {
                self.line += 1;
            }
            self.current += 1;
        }

        if (self.current + 1 >= self.source.len) {
            report(self.line, "", "Unterminated string");
            return;
        }

        self.current += 1;
        const str = self.source[self.start + 1 .. self.current - 1];

        const value = Literal{ .stringLiteral = str };
        try self.addToken(TokenType.STRING, value);
    }

    fn parseNumber(self: *Self) !void {
        while (!self.isAtEnd() and isDigit(self.source[self.current])) {
            self.current += 1;
        }

        if (!self.isAtEnd() and (self.source[self.current] == '.') and isDigit(self.source[self.current + 1])) {
            self.current += 1;

            while (!self.isAtEnd() and isDigit(self.source[self.current])) {
                self.current += 1;
            }
        }

        const number_str = self.source[self.start..self.current];
        const number = try std.fmt.parseFloat(f64, number_str);
        const literal = Literal{ .floatLiteral = number };
        try self.addToken(TokenType.NUMBER, literal);
        self.current -= 1;
    }

    fn identifier(self: *Self) !void {
        while (!self.isAtEnd() and isAlphaNumeric(self.source[self.current])) {
            self.current += 1;
        }

        const str = self.source[self.start..self.current];
        const ttype: TokenType = self.keywords.get(str).?; // handle null
        try self.addKeywordToken(ttype);
    }
};
