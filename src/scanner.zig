const std = @import("std");
const T = @import("token.zig");
const Token = T.Token;
const TokenType = T.TokenType;
const Literal = T.Literal;

pub const Scanner = struct {
    const Self = @This();
    tokens: std.ArrayList(Token),
    keywords: std.StringHashMap(TokenType),
    start: u32,
    current: u32,
    line: u32,

    // Todo: Change the hashmap to a comptime hash map.
    pub fn init(allocator: std.mem.Allocator) !Scanner {
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

        return .{ .start = 0, .current = 0, .line = 1, .tokens = std.ArrayList(Token).init(allocator), .keywords = hashmap };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.keywords.deinit();
    }

    pub fn scanTokens(self: *Self, source: []const u8) !std.ArrayList(Token) {
        while (self.current < source.len) {
            const token = scanToken(self, source);
            if (token) |t| {
                try self.tokens.append(t);
            }
            self.current += 1;
        }

        const eof = Token{ .kind = .EOF, .lexeme = "", .line = self.line, .literal = null };
        try self.tokens.append(eof);
        return self.tokens;
    }

    fn scanToken(self: *Self, source: []const u8) ?Token {
        var b = true;
        while (!self.isAtEnd(source) and b) {
            b = self.parseWhitespace(source, source[self.current]);
        }

        if (self.isAtEnd(source)) {
            return null;
        }

        self.start = self.current;
        const c: u8 = source[self.current];
        return switch (c) {
            // One char tokens
            '(' => self.makeTokenSingle(.LEFT_PAREN, source),
            ')' => self.makeTokenSingle(.RIGHT_PAREN, source),
            '{' => self.makeTokenSingle(.LEFT_BRACE, source),
            '}' => self.makeTokenSingle(.RIGHT_BRACE, source),
            ',' => self.makeTokenSingle(.COMMA, source),
            '.' => self.makeTokenSingle(.DOT, source),
            '-' => self.makeTokenSingle(.MINUS, source),
            '+' => self.makeTokenSingle(.PLUS, source),
            ';' => self.makeTokenSingle(.SEMICOLON, source),
            '*' => self.makeTokenSingle(.STAR, source),

            // Two char tokens
            '!' => {
                if (self.match(source, '=')) {
                    self.current += 1;
                    return self.makeTokenSingle(TokenType.BANG_EQUAL, source);
                } else {
                    return self.makeTokenSingle(TokenType.BANG, source);
                }
            },

            //
            '=' => {
                if (self.match(source, '=')) {
                    self.current += 1;
                    return self.makeTokenSingle(TokenType.EQUAL_EQUAL, source);
                } else {
                    return self.makeTokenSingle(TokenType.EQUAL, source);
                }
            },

            //
            '<' => {
                if (self.match(source, '=')) {
                    self.current += 1;
                    return self.makeTokenSingle(TokenType.LESS_EQUAL, source);
                } else {
                    return self.makeTokenSingle(TokenType.LESS, source);
                }
            },

            //
            '>' => {
                if (self.match(source, '=')) {
                    self.current += 1;
                    return self.makeTokenSingle(TokenType.GREATER_EQUAL, source);
                } else {
                    return self.makeTokenSingle(TokenType.GREATER, source);
                }
            },

            // Slash and comments
            '/' => return self.makeTokenSingle(.SLASH, source),

            // Strings
            '"' => self.parseString(source),
            else => {
                if (isDigit(c)) {
                    return self.parseNumber(source);
                } else if (isAlpha(c)) {
                    return self.identifier(source);
                } else {
                    report(self.line, "", "Unexpected character.");
                    return Token{ .kind = TokenType.ERROR, .lexeme = "", .line = self.line, .literal = null };
                }
            },
        };
    }

    fn parseWhitespace(self: *Self, source: []const u8, c: u8) bool {
        if (c == '/' and source[self.current + 1] == '/') {
            while (!self.isAtEnd(source) and
                source[self.current] != '\n')
            {
                self.current += 1;
            }
            self.line += 1;
            return true;
        } else if (c == ' ' or c == '\r' or c == '\t') {
            self.current += 1;
            return true;
        } else if (c == '\n') {
            self.current += 1;
            self.line += 1;
            return true;
        }

        return false;
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn isAtEnd(self: *Self, source: []const u8) bool {
        return self.current >= source.len;
    }

    fn isAlpha(c: u8) bool {
        return ((c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_');
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn makeTokenSingle(self: *Self, kind: T.TokenType, source: []const u8) Token {
        return Token{ .kind = kind, .lexeme = source[self.start..self.current], .line = self.line, .literal = null };
    }

    fn makeToken(self: *Self, kind: T.TokenType, value: Literal) Token {
        return Token{ .kind = kind, .literal = value, .lexeme = "", .line = self.line };
    }

    fn makeKeywordToken(self: *Self, kind: T.TokenType) Token {
        return Token{ .kind = kind, .lexeme = "", .line = self.line, .literal = null };
    }

    fn match(self: *Self, source: []const u8, expected: u8) bool {
        if ((self.current >= source.len) or source[self.current + 1] != expected) {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn report(line: u32, where: []const u8, message: []const u8) void {
        std.log.err("[line {d}] Error {s}: {s}\n", .{ line, where, message });
    }

    fn parseString(self: *Self, source: []const u8) Token {
        while ((self.current + 1 < source.len) and (source[self.current + 1] != '"')) {
            if (source[self.current + 1] == '\n') {
                self.line += 1;
            }
            self.current += 1;
        }

        if (self.current + 1 >= source.len) {
            report(self.line, "", "Unterminated string");
            return Token{ .kind = .ERROR, .line = self.line, .lexeme = "", .literal = null };
        }

        self.current += 1;
        const str = source[self.start + 1 .. self.current];
        const value = Literal{ .string = str };
        return self.makeToken(TokenType.STRING, value);
    }

    fn parseNumber(self: *Self, source: []const u8) Token {
        while (!self.isAtEnd(source) and isDigit(source[self.current])) {
            self.current += 1;
        }

        if (!self.isAtEnd(source) and (source[self.current] == '.') and isDigit(source[self.current + 1])) {
            self.current += 1;

            while (!self.isAtEnd(source) and isDigit(source[self.current])) {
                self.current += 1;
            }
        }

        const number_str = source[self.start..self.current];
        const number = std.fmt.parseFloat(f64, number_str) catch {
            report(self.line, "", "Number invalid");
            return Token{ .kind = .ERROR, .line = self.line, .lexeme = "", .literal = null };
        };
        const literal = Literal{ .number = number };
        self.current -= 1;
        return self.makeToken(TokenType.NUMBER, literal);
    }

    fn identifier(self: *Self, source: []const u8) Token {
        while (!self.isAtEnd(source) and isAlphaNumeric(source[self.current])) {
            self.current += 1;
        }

        const str = source[self.start..self.current];
        const ttype = self.keywords.get(str); // handle null
        if (ttype) |tokentype| {
            return self.makeKeywordToken(tokentype);
        } else {
            report(self.line, "", "Invalid Identifier");
            return Token{ .kind = .ERROR, .line = self.line, .lexeme = "", .literal = null };
        }
    }
};
