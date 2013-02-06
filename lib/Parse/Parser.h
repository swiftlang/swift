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
#include "swift/Parse/Token.h"
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/ADT/SetVector.h"

namespace llvm {
  class Component;
  class MemoryBuffer;
  class SourceMgr;
  template <typename PT1, typename PT2, typename PT3> class PointerUnion3;
}

namespace swift {
  class DiagnosticEngine;
  class Lexer;
  class ScopeInfo;
  struct TypeLoc;
  class TupleType;
  
  struct OneOfElementInfo;

class Parser {
  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;

public:
  llvm::SourceMgr &SourceMgr;
  DiagnosticEngine &Diags;
  const llvm::MemoryBuffer *Buffer;
  Lexer *L;
  DeclContext *CurDeclContext;
  swift::Component *Component;
  ASTContext &Context;
  ScopeInfo ScopeInfo;
  std::vector<TranslationUnit::IdentTypeAndContext> UnresolvedIdentifierTypes;
  std::vector<std::vector<VarDecl*>> AnonClosureVars;
  std::vector<TranslationUnit::TupleTypeAndContext> TypesWithDefaultValues;
  bool IsMainModule;
  bool FoundSideEffects;

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


  Parser(unsigned BufferID, swift::Component *Component, ASTContext &Ctx,
         unsigned Offset, unsigned EndOffset, bool IsMainModule);
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
  void skipUntilAnyOperator();
  
  /// skipUntilDeclStmtRBrace - Skip to the next decl or '}'.
  void skipUntilDeclRBrace();

  /// skipUntilDeclStmtRBrace - Skip to the next decl, statement or '}'.
  void skipUntilDeclStmtRBrace();

private:
  /// Skip a single token, but match parentheses, braces, and square brackets.
  ///
  /// Note: this does \em not match angle brackets ("<" and ">")! These are
  /// matched in the source when they refer to a generic type, 
  /// but not when used as comparison operators.
  void skipSingle();

public:
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(SourceLoc Loc, ArgTypes... Args) {
    return Diags.diagnose(Loc, Diagnostic(Args...));
  }
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(Token Tok, ArgTypes... Args) {
    return Diags.diagnose(Tok.getLoc(), Diagnostic(Args...));
  }
  
  void diagnoseRedefinition(ValueDecl *Prev, ValueDecl *New);
     
  /// \brief Check whether the current token starts with '<'.
  bool startsWithLess(Token Tok) {
    return Tok.isAnyOperator() && Tok.getText()[0] == '<';
  }

  /// \brief Check whether the current token starts with '>'.
  bool startsWithGreater(Token Tok) {
    return Tok.isAnyOperator() && Tok.getText()[0] == '>';
  }

  /// \brief Consume the starting '<' of the current token, which may either
  /// be a complete '<' token or some kind of operator token starting with '<',
  /// e.g., '<>'.
  SourceLoc consumeStartingLess();

  /// \brief Consume the starting '>' of the current token, which may either
  /// be a complete '>' token or some kind of operator token starting with '>',
  /// e.g., '>>'.
  SourceLoc consumeStartingGreater();

  //===--------------------------------------------------------------------===//
  // Primitive Parsing

  /// parseIdentifier - Consume an identifier (but not an operator) if
  /// present and return its name in Result.  Otherwise, emit an error and
  /// return true.
  bool parseIdentifier(Identifier &Result, const Diagnostic &D);
  
  template<typename ...ArgTypes>
  bool parseIdentifier(Identifier &Result,  ArgTypes... Args) {
    return parseIdentifier(Result, Diagnostic(Args...));
  }
  
  /// parseAnyIdentifier - Consume an identifier or operator if present and
  /// return its name in Result.  Otherwise, emit an error and return true.
  bool parseAnyIdentifier(Identifier &Result, const Diagnostic &D);

  template<typename ...ArgTypes>
  bool parseAnyIdentifier(Identifier &Result,  ArgTypes... Args) {
    return parseAnyIdentifier(Result, Diagnostic(Args...));
  }

  /// parseToken - The parser expects that 'K' is next in the input.  If so, it
  /// is consumed and false is returned.
  ///
  /// If the input is malformed, this emits the specified error diagnostic.
  /// Next, if SkipToTok is specified, it calls skipUntil(SkipToTok).  Finally,
  /// true is returned.
  bool parseToken(tok K, Diag<> D, tok SkipToTok = tok::unknown) {
    SourceLoc L;
    return parseToken(K, L, D, SkipToTok);
  }
  bool parseToken(tok K, SourceLoc &TokLoc, Diag<> D,
                  tok SkipToTok = tok::unknown);
  
  /// parseMatchingToken - Parse the specified expected token and return its
  /// location on success.  On failure, emit the specified error diagnostic, and
  /// a note at the specified note location.
  bool parseMatchingToken(tok K, SourceLoc &TokLoc, Diag<> ErrorDiag,
                          SourceLoc OtherLoc, tok SkipToTok = tok::unknown);
  
  bool parseValueSpecifier(TypeLoc &Loc, NullablePtr<Expr> &Init);

  void parseBraceItemList(SmallVectorImpl<ExprStmtOrDecl> &Decls,
                          bool IsTopLevel, bool IsGetSet = false);

  //===--------------------------------------------------------------------===//
  // Decl Parsing
  static bool isStartOfDecl(const Token &Tok, const Token &Tok2);

  void parseTranslationUnit(TranslationUnit *TU);
  bool parseDecl(SmallVectorImpl<Decl*> &Entries, unsigned Flags);
  enum {
    PD_Default              = 0,
    PD_AllowTopLevel        = 1 << 1,
    PD_DisallowVar          = 1 << 2,
    PD_HasContainerType     = 1 << 3,
    PD_DisallowProperty     = 1 << 4,
    PD_DisallowNominalTypes = 1 << 5,
    PD_DisallowFuncDef      = 1 << 6,
    PD_DisallowInit         = 1 << 7,
    PD_DisallowTypeAliasDef = 1 << 8,
    PD_AllowDestructor      = 1 << 9
  };
  
  TypeAliasDecl *parseDeclTypeAlias(bool WantDefinition);
  /// addVarsToScope - Add the variables in the given pattern to the current
  /// scope, collecting the variables in the vector \c Decls and applying
  /// \c Attributes to each one.
  void addVarsToScope(Pattern *Pat, SmallVectorImpl<Decl*> &Decls,
                      DeclAttributes &Attributes);
  bool parseAttributeList(DeclAttributes &Attributes) {
    if (Tok.is(tok::l_square_starting))
      return parseAttributeListPresent(Attributes);
    return false;
  }
  bool parseAttributeListPresent(DeclAttributes &Attributes);
  bool parseAttribute(DeclAttributes &Attributes);
  
  Decl *parseDeclImport(unsigned Flags);
  bool parseInheritance(SmallVectorImpl<TypeLoc> &Inherited);
  Decl *parseDeclExtension(unsigned Flags);
  bool parseDeclOneOf(unsigned Flags, SmallVectorImpl<Decl*> &Decls);

  bool parseDeclStruct(unsigned Flags, SmallVectorImpl<Decl*> &Decls);
  bool parseDeclClass(unsigned Flags, SmallVectorImpl<Decl*> &Decls);
  bool parseDeclVar(unsigned Flags, SmallVectorImpl<Decl*> &Decls);
  bool parseGetSet(bool HasContainerType, Pattern *Indices, Type ElementTy, 
                   FuncDecl *&Get, FuncDecl *&Set, SourceLoc &LastValidLoc);
  void parseDeclVarGetSet(Pattern &pattern, bool hasContainerType);
  
  Pattern *buildImplicitThisParameter();
  FuncDecl *parseDeclFunc(unsigned Flags);
  Decl *parseDeclProtocol(unsigned Flags);
  
  bool parseDeclSubscript(bool HasContainerType,
                          bool NeedDefinition,
                          SmallVectorImpl<Decl *> &Decls);

  ConstructorDecl *parseDeclConstructor(bool HasContainerType);
  DestructorDecl *parseDeclDestructor(unsigned Flags);

  //===--------------------------------------------------------------------===//
  // Type Parsing
  
  bool parseType(TypeLoc &ResultLoc);
  bool parseType(TypeLoc &ResultLoc, Diag<> ID);
  bool parseTypeAnnotation(TypeLoc &ResultLoc);
  bool parseTypeAnnotation(TypeLoc &ResultLoc, Diag<> ID);
  bool parseGenericArguments(ArrayRef<TypeLoc> &Args);
  bool parseTypeIdentifier(TypeLoc &ResultLoc);
  bool parseTypeComposition(TypeLoc &ResultLoc);
  bool parseTypeTupleBody(SourceLoc LPLoc, TypeLoc &ResultLoc);
  bool parseTypeArray(TypeLoc &ResultLoc);

  //===--------------------------------------------------------------------===//
  // Pattern Parsing

  bool parseFunctionArguments(SmallVectorImpl<Pattern*> &argPatterns,
                              SmallVectorImpl<Pattern*> &bodyPatterns);
  bool parseFunctionSignature(SmallVectorImpl<Pattern*> &argPatterns,
                              SmallVectorImpl<Pattern*> &bodyPatterns,
                              TypeLoc &retLoc);
  NullablePtr<Pattern> parsePattern(bool &CForLoopHack);
  NullablePtr<Pattern> parsePatternTuple(bool &CForLoopHack,bool AllowInitExpr);
  NullablePtr<Pattern> parsePatternAtom(bool &CForLoopHack);
  NullablePtr<Pattern> parsePatternIdentifier();

  //===--------------------------------------------------------------------===//
  // Expression Parsing
  
  // Each of these parsing methods returns null (in a NullablePtr) on a parse
  // error, or an ErrorExpr on a semantic error.  If the method cannot fail, it
  // returns a raw Expr*.
  NullablePtr<Expr> parseExpr(Diag<> ID);
  NullablePtr<Expr> parseExprPostfix(Diag<> ID);
  NullablePtr<Expr> parseExprUnary(Diag<> ID);
  NullablePtr<Expr> parseExprNew();
  NullablePtr<Expr> parseExprSuper();
  Expr *parseExprStringLiteral();
  Expr *parseExprIdentifier();
  NullablePtr<Expr> parseExprExplicitClosure();
  Expr *parseExprAnonClosureArg();
  NullablePtr<Expr> parseExprList(tok LeftTok, tok RightTok);
  NullablePtr<Expr> parseExprArray();
  NullablePtr<Expr> parseExprFunc();
  
  Expr *parseExprOperator();
  Expr *actOnIdentifierExpr(Identifier Text, SourceLoc Loc);
  FuncExpr *actOnFuncExprStart(SourceLoc FuncLoc, TypeLoc FuncRetTy,
                               ArrayRef<Pattern*> ArgPatterns,
                               ArrayRef<Pattern*> BodyPatterns);

  //===--------------------------------------------------------------------===//
  // Statement Parsing
  // Each of these returns null (in a NullablePtr) on a parse error, or an
  // ErrorStmt on a semantic error.
  static bool isStartOfStmtOtherThanAssignment(const Token &Tok);
  NullablePtr<Stmt> parseStmtOtherThanAssignment();
  bool parseExprOrStmtAssign(ExprStmtOrDecl &Results);
  bool parseExprOrStmt(ExprStmtOrDecl &Results);
  NullablePtr<BraceStmt> parseStmtBrace(Diag<> ID);
  NullablePtr<Stmt> parseStmtReturn();
  NullablePtr<Stmt> parseStmtIf();
  NullablePtr<Stmt> parseStmtWhile();
  NullablePtr<Stmt> parseStmtDoWhile();
  NullablePtr<Stmt> parseStmtFor();
  NullablePtr<Stmt> parseStmtForCStyle(SourceLoc ForLoc, SourceLoc LPLoc,
                                       bool CForLoopHack);
  NullablePtr<Stmt> parseStmtForEach(SourceLoc ForLoc);

  //===--------------------------------------------------------------------===//
  // Generics Parsing
  GenericParamList *parseGenericParameters();
  GenericParamList *maybeParseGenericParams();
  bool parseRequiresClause(SourceLoc &RequiresLoc,
                           SmallVectorImpl<Requirement> &Requirements);
};

} // end namespace swift

#endif
