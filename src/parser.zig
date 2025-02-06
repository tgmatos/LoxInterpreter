const std = @import("std");
const T = @import("token.zig");
const E = @import("expression.zig");
const OutOfMemory = std.mem.Allocator.Error.OutOfMemory;

const Token = T.Token;
const TokenType = T.TokenType;
const Expr = E.Expr;
const Binary = E.Binary;
const Unary = E.Unary;
const Literal = E.Literal;
const LiteralType = E.LiteralType;
const Grouping = E.Grouping;

pub const ParserError = error{ RightParenNotPresent, TokenInvalid, MissingLeftOperandError, OutOfMemory };

pub const Parser = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    current: usize,
    tokens: *const std.ArrayList(Token),

    pub fn init(allocator: std.mem.Allocator, tokens: *const std.ArrayList(Token)) Parser {
        return .{ .allocator = allocator, .current = 0, .tokens = tokens };
    }

    pub fn parser(self: *Self) ParserError!*Expr {
        return try self.expression();
    }

    fn expression(self: *Self) ParserError!*Expr {
        const result = try self.comma();
        return result;
    }

    fn comma(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }
        var expr: *Expr = try self.equality();
        const matches = [_]TokenType{.COMMA};

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.equality();
            expr = try Binary.create(self.allocator, expr, operator, right);
        }

        return expr;
    }

    fn equality(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: *Expr = try self.comparison();
        const matches = [_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right = try self.comparison();
            expr = try Binary.create(self.allocator, expr, operator, right);
        }
        return expr;
    }

    fn comparison(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: *Expr = try self.term();
        const matches = [_]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.term();
            expr = try Binary.create(self.allocator, expr, operator, right);
        }
        return expr;
    }

    fn term(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: *Expr = try self.factor();
        const matches = [_]TokenType{ .PLUS, .MINUS };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.factor();
            expr = try Binary.create(self.allocator, expr, operator, right);
        }
        return expr;
    }

    fn factor(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: *Expr = try self.unary();
        const matches = [_]TokenType{ .SLASH, .STAR };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.unary();
            expr = try Binary.create(self.allocator, expr, operator, right); //Unary.create(self.allocator, operator, right);
        }
        return expr;
    }

    fn unary(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        const matches = [_]TokenType{ .BANG, .MINUS };

        if (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.unary();

            const expr = try Unary.create(self.allocator, operator, right);
            return expr;
        }

        return try self.primary();
    }

    fn primary(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        switch (self.peek().kind) {
            .FALSE => return try Literal.create(self.allocator, Literal{ .boolean = false }),
            .TRUE => return try Literal.create(self.allocator, Literal{ .boolean = true }),
            .NIL => return try Literal.create(self.allocator, Literal{ .nil = void{} }),
            .NUMBER => {
                _ = self.advance();
                return try Literal.create(self.allocator, Literal{ .number = self.previous().literal.? });
            },
            .STRING => {
                _ = self.advance();
                return try Literal.create(self.allocator, Literal{ .string = self.previous().literal.? });
            },

            // Handle the parens
            .LEFT_PAREN => {
                _ = self.advance();
                const expr: *Expr = try self.expression();

                // Check if the right paren is present
                switch (self.check(.RIGHT_PAREN)) {
                    true => _ = self.advance(),
                    false => std.debug.panic("[Line {d}] Error at {any}: expect ')' after expression.", .{ self.peek().line, self.peek().lexeme }),
                    //std.log.err("[Line {d}] Error at {any}: expect ')' after expression.", .{ self.peek().line, self.peek().lexeme });
                    // return ParserError.RightParenNotPresent;

                }

                return try Grouping.create(self.allocator, expr);
            },
            else => std.debug.panic("Error: {any}\n", .{ParserError.TokenInvalid}),
        }
    }

    fn synchronize(self: *Self) void {
        self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().kind == .SEMICOLON) {
                return;
            }

            switch (self.peek().kind) {
                .CLASS => return,
                .FUN => return,
                .VAR => return,
                .FOR => return,
                .IF => return,
                .WHILE => return,
                .PRINT => return,
                .RETURN => return,
            }

            self.advance();
        }
    }

    fn validate_left_operand(self: *Self) bool {
        const matches = [_]TokenType{ .EQUAL_EQUAL, .BANG_EQUAL, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL, .PLUS, .MINUS, .STAR, .COMMA };

        return self.match(&matches);
    }

    fn match_one(self: *Self, tokenType: TokenType) bool {
        if (self.check(tokenType)) {
            _ = self.advance();
            return true;
        }

        return false;
    }

    fn match(self: *Self, tokenTypeList: []const TokenType) bool {
        for (tokenTypeList) |tokenType| {
            if (self.check(tokenType)) {
                _ = self.advance();
                return true;
            }
        }

        return false;
    }

    fn check(self: *Self, tokenType: TokenType) bool {
        if (self.isAtEnd()) {
            return false;
        }

        return self.peek().kind == tokenType;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }

        return self.previous();
    }

    fn isAtEnd(self: Self) bool {
        return self.peek().kind == TokenType.EOF;
    }

    fn peek(self: Self) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: Self) Token {
        return self.tokens.items[self.current - 1];
    }
};

// expression     → comma
// comma          → equality ( (",") equality)*
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

// Add error productions to handle each binary operator appearing without a left-hand operand.
// In other words, detect a binary operator appearing at the beginning of an expression.
// Report that as an error, but also parse and discard a right-hand operand with the appropriate precedence.
