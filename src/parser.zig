const std = @import("std");
const T = @import("token.zig");
const Token = T.Token;
const TokenType = T.TokenType;
const exp = @import("expression.zig");
const Expr = exp.Expr;
const Binary = exp.Binary;
const Unary = exp.Unary;
const Literal = exp.Literal;
const Grouping = exp.Grouping;

pub const ParserError = error{ RightParenNotPresent, TokenInvalid, MissingLeftOperandError };

pub const Parser = struct {
    const Self = @This();
    current: usize,
    tokens: *const std.ArrayList(Token),

    pub fn init(tokens: *const std.ArrayList(Token)) Parser {
        return .{ .current = 0, .tokens = tokens };
    }

    pub fn parser(self: *Self) ParserError!Expr {
        return try self.expression();
    }

    fn expression(self: *Self) ParserError!Expr {
        const result = try self.equality();
        return result;
    }

    fn comma(self: *Self) !Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: Expr = try self.equality();
        const matches = [_]TokenType{.COMMA};

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            var right = try self.equality();
            expr = Expr{ .binary = .{ .exprLeft = &expr, .operator = operator, .exprRight = &right } };
        }

        return expr;
    }

    fn equality(self: *Self) !Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: Expr = try self.comparison();
        const matches = [_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            var right = try self.comparison();
            expr = Expr{ .binary = .{ .exprLeft = &expr, .operator = operator, .exprRight = &right } };
        }

        return expr;
    }

    fn comparison(self: *Self) !Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: Expr = try self.term();
        const matches = [_]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            var right: Expr = try self.term();
            expr = Expr{ .binary = .{ .exprLeft = &expr, .operator = operator, .exprRight = &right } };
        }

        return expr;
    }

    fn term(self: *Self) !Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: Expr = try self.factor();
        const matches = [_]TokenType{ .PLUS, .MINUS };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            var right: Expr = try self.factor();
            expr = Expr{ .binary = .{ .exprLeft = &expr, .operator = operator, .exprRight = &right } };
        }

        return expr;
    }

    fn factor(self: *Self) !Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: Expr = try self.unary();
        const matches = [_]TokenType{ .SLASH, .STAR };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            var right: Expr = try self.unary();
            expr = Expr{ .binary = .{ .exprLeft = &expr, .operator = operator, .exprRight = &right } };
        }

        return expr;
    }

    fn unary(self: *Self) !Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        const matches = [_]TokenType{ .BANG, .MINUS };

        if (self.match(&matches)) {
            const operator: Token = self.previous();
            var right: Expr = try self.unary();
            return Expr{ .unary = .{ .operator = operator, .right = &right } };
        }

        return self.primary();
    }

    fn primary(self: *Self) !Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        switch (self.peek().kind) {
            .FALSE => return Expr{ .literal = .{ .boolean = false } },
            .TRUE => return Expr{ .literal = .{ .boolean = true } },
            .NIL => return Expr{ .literal = .{ .nil = void{} } },
            .NUMBER => {
                _ = self.advance();
                return Expr{ .literal = .{ .number = self.previous().literal.? } };
            },
            .STRING => {
                _ = self.advance();
                return Expr{ .literal = .{ .string = self.previous().literal.? } };
            },

            // Handle the parens
            .LEFT_PAREN => {
                _ = self.advance();
                var expr: Expr = try self.expression();

                // Check if the right paren is present
                switch (self.check(.RIGHT_PAREN)) {
                    true => _ = self.advance(),
                    false => std.debug.panic("[Line {d}] Error at {any}: expect ')' after expression.", .{ self.peek().line, self.peek().lexeme }),
                    //std.log.err("[Line {d}] Error at {any}: expect ')' after expression.", .{ self.peek().line, self.peek().lexeme });
                    // return ParserError.RightParenNotPresent;

                }

                return Expr{ .grouping = .{ .expr = &expr } };
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
