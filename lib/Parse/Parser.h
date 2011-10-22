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

#include "ParseResult.h"
#include "Scope.h"
#include "Token.h"
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"

namespace llvm {
  class SourceMgr;
  template <typename PT1, typename PT2, typename PT3> class PointerUnion3;
}

namespace swift {
  class Lexer;
  class ScopeInfo;
  class DiagnosticEngine;
  
  struct OneOfElementInfo;

class Parser {
  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;
public:
  llvm::SourceMgr &SourceMgr;
  DiagnosticEngine &Diags;
  Lexer &L;
  DeclContext *CurDeclContext;
  ASTContext &Context;
  ScopeInfo ScopeInfo;
  
  /// Tok - This is the current token being considered by the parser.
  Token Tok;
  
  typedef llvm::PointerUnion3<Expr*, Stmt*, Decl*> ExprStmtOrDecl;

  /// A RAII object for temporarily changing CurDeclContext.
  class ContextChange {
    Parser &P;
    DeclContext *OldContext;
  public:
    ContextChange(Parser &P, DeclContext *DC)
      : P(P), OldContext(P.CurDeclContext) {
      assert(DC && "pushing null context?");
      P.CurDeclContext = DC;
    }

    /// Prematurely pop the DeclContext installed by the constructor.
    /// Makes the destructor a no-op.
    void pop() {
      assert(OldContext && "already popped context!");
      P.CurDeclContext = OldContext;
      OldContext = nullptr;
    }

    ~ContextChange() {
      if (OldContext) P.CurDeclContext = OldContext;
    }
  };


  Parser(unsigned BufferID, ASTContext &Ctx);
  ~Parser();
  
  //===--------------------------------------------------------------------===//
  // Utilities
  
  /// peekToken - Return the next token that will be installed by consumeToken.
  const Token &peekToken();
  
  // Utilities.
  SourceLoc consumeToken();
  SourceLoc consumeToken(tok K) {
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
  
  /// skipUntil - Read tokens until we get to one of the specified tokens, then
  /// return without consuming it.  Because we cannot guarantee that the token 
  /// will ever occur, this skips to some likely good stopping point.
  ///
  void skipUntil(tok T1, tok T2 = tok::unknown);
  
  template<typename ...ArgTypes>
  void diagnose(SourceLoc Loc, Diag<ArgTypes...> ID,
                typename detail::PassArgument<ArgTypes>::type... Args) {
    Diags.diagnose(Loc, ID, Args...);
  }

  /// \brief Emit a diagnostic at the given source location.
  template<typename ...ArgTypes>
  void diagnose(Token Tok, Diag<ArgTypes...> ID,
                typename detail::PassArgument<ArgTypes>::type... Args) {
    Diags.diagnose(Tok.getLoc(), ID, Args...);
  }

  //===--------------------------------------------------------------------===//
  // Primitive Parsing
  
  bool parseIdentifier(Identifier &Result, DiagID ID,
                       ArrayRef<DiagnosticArgument> Args);

  bool parseIdentifier(Identifier &Result, Diag<> ID) {
    return parseIdentifier(Result, ID.ID, ArrayRef<DiagnosticArgument>());
  }
  
  template<typename ...ArgTypes>
  bool parseIdentifier(Identifier &Result, Diag<ArgTypes...> ID,
                       typename detail::PassArgument<ArgTypes>::type... Args) {
    DiagnosticArgument DiagArgs[] = { Args... };
    return parseIdentifier(Result, ID.ID, DiagArgs);
  }

  /// parseToken - The parser expects that 'K' is next in the input.  If so, it
  /// is consumed and false is returned.
  ///
  /// If the input is malformed, this emits the specified error diagnostic.
  /// Next, if SkipToTok is specified, it calls skipUntil(SkipToTok).  Finally,
  /// true is returned.
  bool parseToken(tok K, Diag<>, tok SkipToTok = tok::unknown);
  
  bool parseValueSpecifier(Type &Ty, NullablePtr<Expr> &Init, bool Single);

  bool parseBraceItemList(SmallVectorImpl<ExprStmtOrDecl> &Decls,
                          bool IsTopLevel);

  //===--------------------------------------------------------------------===//
  // Decl Parsing
  
  TranslationUnit *parseTranslationUnit();
  TypeAliasDecl *parseDeclTypeAlias();
  void parseAttributeList(DeclAttributes &Attributes) {
    if (Tok.is(tok::l_square))
      parseAttributeListPresent(Attributes);
  }
  void parseAttributeListPresent(DeclAttributes &Attributes);
  bool parseAttribute(DeclAttributes &Attributes);
  bool parseVarName(DeclVarName &Name);
  
  Decl *parseDeclImport();
  Decl *parseDeclOneOf();
  bool parseDeclStruct(SmallVectorImpl<ExprStmtOrDecl> &Decls);
  bool parseDeclVar(SmallVectorImpl<ExprStmtOrDecl> &Decls);
  VarDecl *parseDeclVarSimple();
  FuncDecl *parseDeclFunc(bool AllowScoped);
  Decl *parseDeclProtocol();

  void actOnVarDeclName(const DeclVarName *Name,
                        SmallVectorImpl<unsigned> &AccessPath,
                        VarDecl *VD,
                        SmallVectorImpl<Parser::ExprStmtOrDecl> &Decls);
  
  //===--------------------------------------------------------------------===//
  // Type Parsing
  
  bool parseType(Type &Result);
  bool parseType(Type &Result, Diag<> ID);
  bool parseTypeTupleBody(SourceLoc LPLoc, Type &Result);
  
  bool parseTypeOneOf(Type &Result);
  bool parseTypeOneOfBody(SourceLoc OneOfLoc, const DeclAttributes &Attrs,
                          Type &Result, TypeAliasDecl *TypeName = 0);
  bool parseTypeArray(SourceLoc LSquareLoc, Type &Result);
  bool parseTypeProtocol(Type &Result);
  bool parseTypeProtocolBody(SourceLoc ProtocolLoc, const DeclAttributes &Attrs,
                             Type &Result, TypeAliasDecl *TypeName = 0);
  
  struct OneOfElementInfo {
    SourceLoc NameLoc;
    StringRef Name;
    Type EltType;
  };
  
  OneOfType *actOnOneOfType(SourceLoc OneOfLoc, const DeclAttributes &Attrs,
                            ArrayRef<OneOfElementInfo> Elts,
                            TypeAliasDecl *PrettyTypeName);

  //===--------------------------------------------------------------------===//
  // Expression Parsing
  
  static bool isStartOfExpr(const Token &Tok, const Token &Next);
  ParseResult<Expr> parseSingleExpr(Diag<> ID);
  ParseResult<Expr> parseExpr(Diag<> ID);
  ParseResult<Expr> parseExprPostfix(Diag<> ID);
  ParseResult<Expr> parseExprUnary(Diag<> ID);
  ParseResult<Expr> parseExprIdentifier();
  Expr *parseExprOperator();
  ParseResult<Expr> parseExprNumericConstant();
  ParseResult<Expr> parseExprDollarIdentifier();
  ParseResult<Expr> parseExprParen();
  ParseResult<Expr> parseExprFunc();
  
  Expr *actOnIdentifierExpr(Identifier Text, SourceLoc Loc);
  FuncExpr *actOnFuncExprStart(SourceLoc FuncLoc, Type FuncTy);

  // Statement Parsing
  ParseResult<Stmt> parseStmtOtherThanAssignment();
  ParseResult<BraceStmt> parseStmtBrace(Diag<> ID);
  ParseResult<Stmt> parseStmtReturn();
  ParseResult<Stmt> parseStmtIf();
  ParseResult<Stmt> parseStmtWhile();

  /// actOnCondition - Handle a condition to an if/while statement, inserting
  /// the call that will convert to a 1-bit type.
  Expr *actOnCondition(Expr *Cond);

};
  
} // end namespace swift

#endif
