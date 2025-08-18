#ifndef SWIFTC_PARSER_PARSER_H
#define SWIFTC_PARSER_PARSER_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Lexer/Token.h"
#include "swiftc/AST/ASTNode.h"
#include "swiftc/AST/Expr.h"
#include "swiftc/AST/Stmt.h"
#include "swiftc/AST/Decl.h"
#include "swiftc/AST/Type.h"
#include <memory>
#include <vector>

namespace swiftc {

class SourceManager;

/// Swift parser that builds an AST from tokens.
class Parser {
  Lexer& Lex;
  DiagnosticEngine& Diags;
  Token CurrentToken;

public:
  Parser(Lexer& lexer, DiagnosticEngine& diags);

  /// Parse a complete source file.
  std::vector<std::unique_ptr<Decl>> parseSourceFile();

  /// Parse a top-level declaration.
  std::unique_ptr<Decl> parseTopLevelDecl();

private:
  /// Consume the current token and advance to the next one.
  void consumeToken();

  /// Consume a token of the expected kind, or diagnose an error.
  bool consumeToken(TokenKind expectedKind);

  /// Check if the current token is of the given kind.
  bool is(TokenKind kind) const { return CurrentToken.is(kind); }

  /// Check if the current token is not of the given kind.
  bool isNot(TokenKind kind) const { return CurrentToken.isNot(kind); }

  /// Get the current token.
  const Token& getCurrentToken() const { return CurrentToken; }

  /// Diagnose an unexpected token.
  void diagnoseUnexpectedToken(StringRef expected = "");

  // Declaration parsing
  std::unique_ptr<Decl> parseVarDecl();
  std::unique_ptr<Decl> parseFuncDecl();
  std::unique_ptr<Decl> parseClassDecl();
  std::unique_ptr<Decl> parseStructDecl();
  std::unique_ptr<Decl> parseEnumDecl();
  std::unique_ptr<Decl> parseProtocolDecl();
  std::unique_ptr<Decl> parseExtensionDecl();
  std::unique_ptr<Decl> parseImportDecl();
  std::unique_ptr<Decl> parseOperatorDecl();
  std::unique_ptr<Decl> parsePrecedenceGroupDecl();

  // Statement parsing
  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<Stmt> parseExprStmt();
  std::unique_ptr<Stmt> parseReturnStmt();
  std::unique_ptr<Stmt> parseBreakStmt();
  std::unique_ptr<Stmt> parseContinueStmt();
  std::unique_ptr<Stmt> parseIfStmt();
  std::unique_ptr<Stmt> parseWhileStmt();
  std::unique_ptr<Stmt> parseForStmt();
  std::unique_ptr<Stmt> parseSwitchStmt();
  std::unique_ptr<Stmt> parseGuardStmt();
  std::unique_ptr<Stmt> parseDoStmt();
  std::unique_ptr<Stmt> parseCompoundStmt();

  // Expression parsing
  std::unique_ptr<Expr> parseExpr();
  std::unique_ptr<Expr> parsePrimaryExpr();
  std::unique_ptr<Expr> parsePostfixExpr();
  std::unique_ptr<Expr> parseBinaryExpr(int minPrec = 0);
  std::unique_ptr<Expr> parseUnaryExpr();
  std::unique_ptr<Expr> parseCallExpr(std::unique_ptr<Expr> base);
  std::unique_ptr<Expr> parseMemberRefExpr(std::unique_ptr<Expr> base);
  std::unique_ptr<Expr> parseSubscriptExpr(std::unique_ptr<Expr> base);
  std::unique_ptr<Expr> parseParenExpr();
  std::unique_ptr<Expr> parseArrayExpr();
  std::unique_ptr<Expr> parseDictionaryExpr();

  // Type parsing
  std::unique_ptr<Type> parseType();
  std::unique_ptr<Type> parseSimpleType();
  std::unique_ptr<Type> parseTupleType();
  std::unique_ptr<Type> parseFunctionType();
  std::unique_ptr<Type> parseArrayType();
  std::unique_ptr<Type> parseDictionaryType();
  std::unique_ptr<Type> parseOptionalType(std::unique_ptr<Type> base);

  // Utility functions
  std::vector<std::unique_ptr<ParamDecl>> parseParameterList();
  std::unique_ptr<ParamDecl> parseParameter();
  std::vector<std::unique_ptr<Expr>> parseArgumentList();
  
  /// Get operator precedence.
  int getOperatorPrecedence(TokenKind kind);

  /// Check if a token can start an expression.
  bool canStartExpr(TokenKind kind);

  /// Check if a token can start a statement.
  bool canStartStmt(TokenKind kind);

  /// Check if a token can start a declaration.
  bool canStartDecl(TokenKind kind);
};

} // namespace swiftc

#endif // SWIFTC_PARSER_PARSER_H