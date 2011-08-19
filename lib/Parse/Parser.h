//===--- Parser.h - Swift Language Parser -----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Parser interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSER_H
#define SWIFT_PARSER_H

#include "Scope.h"
#include "Token.h"
#include "swift/AST/Type.h"

namespace llvm {
  class SourceMgr;
  template <typename PT1, typename PT2, typename PT3> class PointerUnion3;
}

namespace swift {
  class Lexer;
  class Expr;
  class FuncExpr;
  class Stmt;
  class BraceStmt;
  class Type;
  class Decl;
  class DeclAttributes;
  class TranslationUnitDecl;
  class TypeAliasDecl;
  class FuncDecl;
  class MethDecl;
  class VarDecl;
  class OneOfType;
  class ASTContext;
  class DeclVarName;
  class TupleTypeElt;
  class ScopeInfo;
  template<typename T> class ParseResult;
  
  struct OneOfElementInfo;

class Parser {
  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;
public:
  llvm::SourceMgr &SourceMgr;
  Lexer &L;
  ASTContext &Context;
  ScopeInfo ScopeInfo;
  
  /// Tok - This is the current token being considered by the parser.
  Token Tok;
  
  typedef llvm::PointerUnion3<Expr*, Stmt*, Decl*> ExprStmtOrDecl;


  Parser(unsigned BufferID, ASTContext &Ctx);
  ~Parser();
  
  //===--------------------------------------------------------------------===//
  // Utilities
  
  /// peekToken - Return the next token that will be installed by consumeToken.
  const Token &peekToken();
  
  // Utilities.
  SMLoc consumeToken();
  SMLoc consumeToken(tok K) {
    assert(Tok.is(K) && "Consuming wrong token kind");
    return consumeToken();
  }
  
  /// consumeIf - If the current token is the specified kind, consume it and
  /// return true.  Otherwise, return false without consuming it.
  bool consumeIf(tok K) {
    if (Tok.isNot(K)) return false;
    consumeToken(K);
    return true;
  }
  
  /// skipUntil - Read tokens until we get to the specified token, then return
  /// without consuming it.  Because we cannot guarantee that the token will
  /// ever occur, this skips to some likely good stopping point.
  ///
  void skipUntil(tok T);
  
  void note(SMLoc Loc, const Twine &Message);
  void warning(SMLoc Loc, const Twine &Message);
  void error(SMLoc Loc, const Twine &Message);
  
  //===--------------------------------------------------------------------===//
  // Primitive Parsing
  
  bool parseIdentifier(Identifier &Result, const Twine &Message);

  /// parseToken - The parser expects that 'K' is next in the input.  If so, it
  /// is consumed and false is returned.
  ///
  /// If the input is malformed, this emits the specified error diagnostic.
  /// Next, if SkipToTok is specified, it calls skipUntil(SkipToTok).  Finally,
  /// true is returned.
  bool parseToken(tok K, const char *Message, tok SkipToTok = tok::unknown);
  
  bool parseValueSpecifier(Type &Ty, NullablePtr<Expr> &Init, bool Single);

  bool parseBraceItemList(SmallVectorImpl<ExprStmtOrDecl> &Decls,
                          bool IsTopLevel);

  //===--------------------------------------------------------------------===//
  // Decl Parsing
  
  TranslationUnitDecl *parseTranslationUnit();
  TypeAliasDecl *parseDeclTypeAlias();
  void parseAttributeList(DeclAttributes &Attributes);
  bool parseAttribute(DeclAttributes &Attributes);
  bool parseVarName(DeclVarName &Name);
  
  Decl *parseDeclImport();
  Decl *parseDeclOneOf();
  bool parseDeclStruct(SmallVectorImpl<ExprStmtOrDecl> &Decls);
  bool parseDeclVar(SmallVectorImpl<ExprStmtOrDecl> &Decls);
  FuncDecl *parseDeclFunc();

  void actOnVarDeclName(const DeclVarName *Name,
                        SmallVectorImpl<unsigned> &AccessPath,
                        VarDecl *VD,
                        SmallVectorImpl<Parser::ExprStmtOrDecl> &Decls);
                        
  //===--------------------------------------------------------------------===//
  // Type Parsing
  
  bool parseType(Type &Result);
  bool parseType(Type &Result, const Twine &Message);
  bool parseTypeTupleBody(SMLoc LPLoc, Type &Result);
  
  bool parseTypeOneOfBody(SMLoc OneOfLoc, const DeclAttributes &Attrs,
                          Type &Result, TypeAliasDecl *TypeName = 0);
  bool parseTypeArray(SMLoc LSquareLoc, Type &Result);
  
  struct OneOfElementInfo {
    SMLoc NameLoc;
    StringRef Name;
    Type EltType;
  };
  
  OneOfType *actOnOneOfType(SMLoc OneOfLoc, const DeclAttributes &Attrs,
                            ArrayRef<OneOfElementInfo> Elts,
                            TypeAliasDecl *PrettyTypeName);

  //===--------------------------------------------------------------------===//
  // Expression Parsing
  
  static bool isStartOfExpr(const Token &Tok, const Token &Next);
  ParseResult<Expr> parseSingleExpr(const char *Message = 0);
  ParseResult<Expr> parseExpr(const char *Message = 0);
  ParseResult<Expr> parseExprPostfix(const char *Message = 0);
  ParseResult<Expr> parseExprUnary(const char *Message = 0);
  ParseResult<Expr> parseExprIdentifier();
  Expr *parseExprOperator();
  ParseResult<Expr> parseExprNumericConstant();
  ParseResult<Expr> parseExprDollarIdentifier();
  ParseResult<Expr> parseExprParen();
  ParseResult<Expr> parseExprFunc();
  
  Expr *actOnIdentifierExpr(Identifier Text, SMLoc Loc);
  FuncExpr *actOnFuncExprStart(SMLoc FuncLoc, Type FuncTy);

  // Statement Parsing
  ParseResult<Stmt> parseStmtOtherThanAssignment();
  ParseResult<BraceStmt> parseStmtBrace(const char *Message = 0);
  ParseResult<Stmt> parseStmtReturn();
  ParseResult<Stmt> parseStmtIf();
  ParseResult<Stmt> parseStmtWhile();

  /// actOnCondition - Handle a condition to an if/while statement, inserting
  /// the call that will convert to a 1-bit type.
  Expr *actOnCondition(Expr *Cond);

};
  
} // end namespace swift

#endif
