const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TokenType = enum {
    // Single-char tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two char tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    EOF,
    ERROR,
};

pub const Literal = union(enum) { string: []u8, number: f64 };

// Consider using an Struct of Arrays with MultiArrayList
pub const Token = struct {
    const Self = @This();
    kind: TokenType,
    lexeme: []u8,
    literal: ?*Literal,
    line: u32,

    pub fn init(allocator: Allocator, kind: TokenType, lexeme: []const u8, literal: ?Literal, line: u32) *Token {
        var literalNullable: ?*Literal = null;
        if (literal) |l| {
            literalNullable = allocator.create(Literal) catch |err| {
                std.log.err("{any}", .{err});
                @panic("Panic allocating literal");
            };

            switch (l) {
                .string => {
                    const strLiteral = allocator.alloc(u8, l.string.len) catch |err| {
                        std.log.err("{any}", .{err});
                        @panic("Panic allocating string");
                    };

                    @memcpy(strLiteral, l.string);
                    literalNullable.?.* = .{ .string = strLiteral };
                },
                .number => {
                    literalNullable.?.* = .{ .number = l.number };
                },
            }
        }
        const str = allocator.alloc(u8, lexeme.len) catch |err| {
            std.log.err("{any}", .{err});
            @panic("Panic allocating string");
        };
        @memcpy(str, lexeme);

        const token = allocator.create(Token) catch |err| {
            std.log.err("{any}", .{err});
            @panic("Panic allocating token");
        };
        token.* = .{ .kind = kind, .lexeme = str, .literal = literalNullable, .line = line };
        return token;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.lexeme);
        if (self.literal) |l| {
            switch (l.*) {
                .string => {
                    allocator.free(l.string);
                },
                .number => {},
            }

            allocator.destroy(l);
        }
        allocator.destroy(self);
    }
};
