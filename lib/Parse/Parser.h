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
#include "swift/Parse/Lexer.h"
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/ADT/SetVector.h"

namespace llvm {
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
  class SILModule;
  
  struct OneOfElementInfo;
  
  /// Different contexts in which BraceItemList are parsed.
  enum class BraceItemListKind {
    /// A statement list terminated by a closing brace. The default.
    Brace,
    /// A statement list in a property getter or setter. The list
    /// is terminated by a closing brace or a 'get:' or 'set:' label.
    Property,
    /// A statement list in a case block. The list is terminated
    /// by a closing brace or a 'case' or 'default' label.
    Case,
    /// The top-level of a file, when not in parse-as-library mode (i.e. the
    /// repl or a script).
    TopLevelCode
  };

class Parser {
  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;

public:
  typedef llvm::PointerUnion3<Expr*, Stmt*, Decl*> ExprStmtOrDecl;

  llvm::SourceMgr &SourceMgr;
  DiagnosticEngine &Diags;
  TranslationUnit *TU;
  const llvm::MemoryBuffer *Buffer;
  Lexer *L;
  SILModule *SIL;    // Non-null when parsing a .sil file.
  DeclContext *CurDeclContext;
  swift::Component *Component;
  ASTContext &Context;
  ScopeInfo ScopeInfo;
  std::vector<std::vector<VarDecl*>> AnonClosureVars;
  std::vector<TranslationUnit::TupleTypeAndContext> TypesWithDefaultValues;
  bool IsMainModule;

  /// \brief Whether the '|' character is currently a delimiter character,
  /// as in a closure.
  bool PipeIsDelimiter = false;

  /// Tok - This is the current token being considered by the parser.
  Token Tok;

  /// \brief The location of the previous token.
  SourceLoc PreviousLoc;
  
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


  Parser(unsigned BufferID, TranslationUnit *TU,
         unsigned Offset, unsigned EndOffset, bool IsMainModule,
         SILModule *SIL);
  Parser(TranslationUnit *TU, llvm::StringRef fragment, DiagnosticEngine &Diags,
         SILModule *SIL);
  ~Parser();

  bool isInSILMode() const { return SIL != nullptr; }

  //===--------------------------------------------------------------------===//
  // Routines to save and restore parser state.

  class ParserState {
    ParserState(Lexer::State LS, SourceLoc PreviousLoc):
        LS(LS), PreviousLoc(PreviousLoc)
    {}
    Lexer::State LS;
    SourceLoc PreviousLoc;
    friend class Parser;
  };

  ParserState getParserState() {
    return ParserState(L->getStateForBeginningOfToken(Tok),
                       PreviousLoc);
  }

  void restoreParserState(ParserState PS) {
    L->restoreState(PS.LS);
    PreviousLoc = PS.PreviousLoc;
    consumeToken();
  }

  void backtrackToState(ParserState PS) {
    L->backtrackToState(PS.LS);
    PreviousLoc = PS.PreviousLoc;
    consumeToken();
  }

  /// RAII object that, when it is destructed, restores the parser and lexer to
  /// their positions at the time the object was constructed.
  struct BacktrackingScope {
  private:
    Parser &P;
    ParserState PS;

  public:
    BacktrackingScope(Parser &P) : P(P), PS(P.getParserState()) {}

    ~BacktrackingScope() {
      P.backtrackToState(PS);
    }
  };

  //===--------------------------------------------------------------------===//
  // Utilities

  /// \brief Return the next token that will be installed by \c consumeToken.
  const Token &peekToken();

  SourceLoc consumeToken();
  SourceLoc consumeToken(tok K) {
    assert(Tok.is(K) && "Consuming wrong token kind");
    return consumeToken();
  }

  /// \brief Retrieve the location just past the end of the previous
  /// source location.
  SourceLoc getEndOfPreviousLoc();

  /// consumeIf - If the current token is the specified kind, consume it and
  /// return true.  Otherwise, return false without consuming it.
  bool consumeIf(tok K) {
    if (Tok.isNot(K)) return false;
    consumeToken(K);
    return true;
  }
  
  bool consumeIfNotAtStartOfLine(tok K) {
    if (Tok.isAtStartOfLine()) return false;
    return consumeIf(K);
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
  bool parseIdentifier(Identifier &Result, SourceLoc &Loc, const Diagnostic &D);
  
  template<typename ...DiagArgTypes, typename ...ArgTypes>
  bool parseIdentifier(Identifier &Result, Diag<DiagArgTypes...> ID,
                       ArgTypes... Args) {
    SourceLoc L;
    return parseIdentifier(Result, L, Diagnostic(ID, Args...));
  }

  template<typename ...DiagArgTypes, typename ...ArgTypes>
  bool parseIdentifier(Identifier &Result, SourceLoc &L,
                       Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    return parseIdentifier(Result, L, Diagnostic(ID, Args...));
  }
  

  /// parseAnyIdentifier - Consume an identifier or operator if present and
  /// return its name in Result.  Otherwise, emit an error and return true.
  bool parseAnyIdentifier(Identifier &Result, SourceLoc &Loc,
                          const Diagnostic &D);

  template<typename ...DiagArgTypes, typename ...ArgTypes>
  bool parseAnyIdentifier(Identifier &Result, Diag<DiagArgTypes...> ID,
                          ArgTypes... Args) {
    SourceLoc L;
    return parseAnyIdentifier(Result, L, Diagnostic(ID, Args...));
  }

  template<typename ...DiagArgTypes, typename ...ArgTypes>
  bool parseAnyIdentifier(Identifier &Result, SourceLoc &L,
                          Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    return parseAnyIdentifier(Result, L, Diagnostic(ID, Args...));
  }

  /// parseToken - The parser expects that 'K' is next in the input.  If so, it
  /// is consumed and false is returned.
  ///
  /// If the input is malformed, this emits the specified error diagnostic.
  bool parseToken(tok K, SourceLoc &TokLoc, const Diagnostic &D);
  
  template<typename ...DiagArgTypes, typename ...ArgTypes>
  bool parseToken(tok K, Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    SourceLoc L;
    return parseToken(K, L, Diagnostic(ID, Args...));
  }
  template<typename ...DiagArgTypes, typename ...ArgTypes>
  bool parseToken(tok K, SourceLoc &L,
                  Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    return parseToken(K, L, Diagnostic(ID, Args...));
  }

  
  /// parseMatchingToken - Parse the specified expected token and return its
  /// location on success.  On failure, emit the specified error diagnostic, and
  /// a note at the specified note location.
  bool parseMatchingToken(tok K, SourceLoc &TokLoc, Diag<> ErrorDiag,
                          SourceLoc OtherLoc);

  /// parseList - Parse the list of statements, expressions, or declarations.
  bool parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                 tok SeparatorK, bool OptionalSep, Diag<> ErrorDiag,
                 std::function<bool()> callback);
  
  bool parseValueSpecifier(TypeLoc &Loc, NullablePtr<Expr> &Init);

  void parseBraceItems(SmallVectorImpl<ExprStmtOrDecl> &Decls,
                       bool IsTopLevel,
                       BraceItemListKind Kind = BraceItemListKind::Brace);
  NullablePtr<BraceStmt> parseBraceItemList(Diag<> ID);

  //===--------------------------------------------------------------------===//
  // Decl Parsing
  static bool isStartOfDecl(const Token &Tok, const Token &Tok2);
  static bool isStartOfOperatorDecl(const Token &Tok, const Token &Tok2);

  bool parseTranslationUnit(TranslationUnit *TU);
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
    if (Tok.is(tok::l_square))
      return parseAttributeListPresent(Attributes);
    return false;
  }
  bool parseAttributeListPresent(DeclAttributes &Attributes);
  bool parseAttribute(DeclAttributes &Attributes);
  
  Decl *parseDeclImport(unsigned Flags);
  bool parseInheritance(SmallVectorImpl<TypeLoc> &Inherited);
  Decl *parseDeclExtension(unsigned Flags);
  bool parseDeclOneOf(unsigned Flags, SmallVectorImpl<Decl*> &Decls);
  bool parseNominalDeclMembers(SmallVectorImpl<Decl *> &memberDecls,
                               SourceLoc LBLoc, SourceLoc &RBLoc,
                               Diag<> ErrorDiag, unsigned flags);
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
  
  OperatorDecl *parseDeclOperator(bool AllowTopLevel);
  OperatorDecl *parseDeclPrefixOperator(SourceLoc OperatorLoc,
                                        SourceLoc PrefixLoc,
                                        Identifier Name,
                                        SourceLoc NameLoc);
  OperatorDecl *parseDeclPostfixOperator(SourceLoc OperatorLoc,
                                         SourceLoc PostfixLoc,
                                         Identifier Name,
                                         SourceLoc NameLoc);
  OperatorDecl *parseDeclInfixOperator(SourceLoc OperatorLoc,
                                       SourceLoc InfixLoc,
                                       Identifier Name,
                                       SourceLoc NameLoc);

  //===--------------------------------------------------------------------===//
  // SIL Parsing.

  bool parseDeclSIL();


  //===--------------------------------------------------------------------===//
  // Type Parsing
  
  bool parseType(TypeLoc &ResultLoc);
  bool parseType(TypeLoc &ResultLoc, Diag<> ID);
  bool parseTypeAnnotation(TypeLoc &ResultLoc);
  bool parseTypeAnnotation(TypeLoc &ResultLoc, Diag<> ID);
  bool parseGenericArguments(MutableArrayRef<TypeLoc> &Args,
                             SourceLoc &LAngleLoc,
                             SourceLoc &RAngleLoc);
  bool parseTypeIdentifier(TypeLoc &ResultLoc);
  bool parseTypeComposition(TypeLoc &ResultLoc);
  bool parseTypeTupleBody(TypeLoc &ResultLoc);
  bool parseTypeArray(TypeLoc &ResultLoc);

  //===--------------------------------------------------------------------===//
  // Pattern Parsing

  bool parseFunctionArguments(SmallVectorImpl<Pattern*> &argPatterns,
                              SmallVectorImpl<Pattern*> &bodyPatterns);
  bool parseFunctionSignature(SmallVectorImpl<Pattern*> &argPatterns,
                              SmallVectorImpl<Pattern*> &bodyPatterns,
                              TypeLoc &retLoc);

  NullablePtr<Pattern> parsePattern();

  /// \brief Determine whether this token can start a pattern.
  bool isStartOfPattern(Token tok);
  
  /// \brief Determine whether this token can start a binding name, whether an
  /// identifier or the special discard-value binding '_'.
  bool isStartOfBindingName(Token tok);

  /// Parse a tuple pattern element.
  ///
  ///   pattern-tuple-element:
  ///     pattern ('=' expr)?
  ///
  /// \param allowInitExpr Whether to allow initializers.
  ///
  /// \returns The tuple pattern element, if successful.
  Optional<TuplePatternElt> parsePatternTupleElement(bool allowInitExpr);
  NullablePtr<Pattern> parsePatternTuple(bool AllowInitExpr);
  NullablePtr<Pattern> parsePatternAtom();
  NullablePtr<Pattern> parsePatternIdentifier();

  //===--------------------------------------------------------------------===//
  // Speculative type list parsing
  //===--------------------------------------------------------------------===//
  
  /// Returns true if we can parse a generic argument list at the current
  /// location in expression context. This parses types without generating
  /// AST nodes from the '<' at the current location up to a matching '>'. If
  /// the type list parse succeeds, and the closing '>' is followed by one
  /// of the following tokens:
  ///   lparen_following rparen lsquare_following rsquare lbrace rbrace
  ///   period_following comma semicolon
  /// then this function returns true, and the expression will parse as a
  /// generic parameter list. If the parse fails, or the closing '>' is not
  /// followed by one of the above tokens, then this function returns false,
  /// and the expression will parse with the '<' as an operator.
  bool canParseAsGenericArgumentList();
  
  bool canParseType();
  bool canParseTypeIdentifier();
  bool canParseTypeComposition();
  bool canParseTypeTupleBody();
  bool canParseTypeArray();
  bool canParseGenericArguments();
  
  //===--------------------------------------------------------------------===//
  // Expression Parsing
  
  // Each of these parsing methods returns null (in a NullablePtr) on a parse
  // error, or an ErrorExpr on a semantic error.  If the method cannot fail, it
  // returns a raw Expr*.
  NullablePtr<Expr> parseExpr(Diag<> ID, bool isExprBasic = false);
  NullablePtr<Expr> parseExprBasic(Diag<> ID) {
    return parseExpr(ID, /*isExprBasic=*/true);
  }
  NullablePtr<Expr> parseExprIs();
  NullablePtr<Expr> parseExprAs();
  NullablePtr<Expr> parseExprSequence(Diag<> ID);
  NullablePtr<Expr> parseExprPostfix(Diag<> ID);
  NullablePtr<Expr> parseExprUnary(Diag<> ID);
  NullablePtr<Expr> parseExprNew();
  NullablePtr<Expr> parseExprSuper();
  Expr *parseExprStringLiteral();
  
  Expr *parseExprIdentifier();

  /// Parse a closure expression after the opening brace.
  ///
  ///   expr-closure:
  ///     '{' closure-signature? brace-item-list* '}'
  ///
  ///   closure-signature:
  ///     '|' closure-signature-arguments? '|' closure-signature-result?
  ///
  ///   closure-signature-arguments:
  ///     pattern-tuple-element (',' pattern-tuple-element)*
  ///
  ///   closure-signature-result:
  ///     '->' type
  Expr *parseExprClosure();

  Expr *parseExprAnonClosureArg();
  NullablePtr<Expr> parseExprList(tok LeftTok, tok RightTok);
  NullablePtr<Expr> parseExprCollection();
  NullablePtr<Expr> parseExprArray(SourceLoc LSquareLoc,
                                   Expr *FirstExpr);
  NullablePtr<Expr> parseExprDictionary(SourceLoc LSquareLoc,
                                        Expr *FirstKey);

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
  bool parseExprOrStmt(ExprStmtOrDecl &Results);
  NullablePtr<Stmt> parseStmtReturn();
  NullablePtr<Stmt> parseStmtIf();
  NullablePtr<Stmt> parseStmtWhile();
  NullablePtr<Stmt> parseStmtDoWhile();
  NullablePtr<Stmt> parseStmtFor();
  NullablePtr<Stmt> parseStmtForCStyle(SourceLoc ForLoc);
  NullablePtr<Stmt> parseStmtForEach(SourceLoc ForLoc);
  NullablePtr<Stmt> parseStmtSwitch();
  NullablePtr<CaseStmt> parseStmtCase();

  //===--------------------------------------------------------------------===//
  // Generics Parsing
  GenericParamList *parseGenericParameters();
  GenericParamList *parseGenericParameters(SourceLoc LAngleLoc);
  GenericParamList *maybeParseGenericParams();
  bool parseRequiresClause(SourceLoc &RequiresLoc,
                           SmallVectorImpl<Requirement> &Requirements);
};

} // end namespace swift

#endif
