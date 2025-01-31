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

pub const Literal = union(enum) { string: []const u8, number: f64 };

// Consider using an Struct of Arrays with MultiArrayList
pub const Token = struct {
    kind: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
    line: u32,

    pub fn init(kind: TokenType, lexeme: []const u8, literal: ?Literal, line: u32) Token {
        return Token{ .kind = kind, .lexeme = lexeme, .literal = literal, .line = line };
    }
};
