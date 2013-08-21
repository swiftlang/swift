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

#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Basic/Optional.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Parse/Token.h"
#include "swift/Parse/ParserResult.h"
#include "llvm/ADT/SetVector.h"

namespace llvm {
  template <typename PT1, typename PT2, typename PT3> class PointerUnion3;
}

namespace swift {
  class DiagnosticEngine;
  class Lexer;
  class ScopeInfo;
  struct TypeLoc;
  class TupleType;
  class SILParserState;
  class SourceManager;
  class PersistentParserState;
  class CodeCompletionCallbacks;
  class DelayedParsingCallbacks;
  
  struct UnionElementInfo;
  
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

  SourceManager &SourceMgr;
  const unsigned BufferID;
  DiagnosticEngine &Diags;
  TranslationUnit *TU;
  Lexer *L;
  SILParserState *SIL;    // Non-null when parsing a .sil file.
  PersistentParserState *State;
  std::unique_ptr<PersistentParserState> OwnedState;
  DeclContext *CurDeclContext;
  swift::Component *Component;
  ASTContext &Context;
  CodeCompletionCallbacks *CodeCompletion = nullptr;
  std::vector<std::vector<VarDecl*>> AnonClosureVars;
  std::pair<const DeclContext *, ArrayRef<VarDecl *>> CurVars;
  unsigned VarPatternDepth = 0;

  DelayedParsingCallbacks *DelayedParseCB = nullptr;

  bool isDelayedParsingEnabled() const { return DelayedParseCB != nullptr; }

  void setDelayedParsingCallbacks(DelayedParsingCallbacks *DelayedParseCB) {
    this->DelayedParseCB = DelayedParseCB;
  }

  void setCodeCompletionCallbacks(CodeCompletionCallbacks *Callbacks) {
    CodeCompletion = Callbacks;
  }

  bool isCodeCompletionFirstPass() {
    return L->isCodeCompletion() && !CodeCompletion;
  }

  bool allowTopLevelCode() const {
    return TU->Kind == TranslationUnit::Main ||
           TU->Kind == TranslationUnit::REPL;
  }

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

public:
  Parser(unsigned BufferID, TranslationUnit *TU, SILParserState *SIL,
         PersistentParserState *PersistentState = nullptr);
  ~Parser();

  bool isInSILMode() const { return SIL != nullptr; }

  //===--------------------------------------------------------------------===//
  // Routines to save and restore parser state.

  class ParserPosition {
  public:
    ParserPosition() = default;
    ParserPosition &operator=(const ParserPosition &) = default;

  private:
    ParserPosition(Lexer::State LS, SourceLoc PreviousLoc):
        LS(LS), PreviousLoc(PreviousLoc)
    {}
    Lexer::State LS;
    SourceLoc PreviousLoc;
    friend class Parser;
  };

  ParserPosition getParserPosition() {
    return ParserPosition(L->getStateForBeginningOfToken(Tok),
                          PreviousLoc);
  }

  ParserPosition getParserPosition(const PersistentParserState::ParserPos &Pos){
    return ParserPosition(L->getStateForBeginningOfTokenLoc(Pos.Loc),
                          Pos.PrevLoc);
  }

  void restoreParserPosition(ParserPosition PP) {
    L->restoreState(PP.LS);
    PreviousLoc = PP.PreviousLoc;

    // We might be at tok::eof now, so ensure that consumeToken() does not
    // assert about lexing past eof.
    Tok.setKind(tok::unknown);
    consumeToken();
  }

  void backtrackToPosition(ParserPosition PP) {
    L->backtrackToState(PP.LS);
    PreviousLoc = PP.PreviousLoc;

    // We might be at tok::eof now, so ensure that consumeToken() does not
    // assert about lexing past eof.
    Tok.setKind(tok::unknown);
    consumeToken();
  }

  /// RAII object that, when it is destructed, restores the parser and lexer to
  /// their positions at the time the object was constructed.  Will not jump
  /// forward in the token stream.
  class BacktrackingScope {
    Parser &P;
    ParserPosition PP;

  public:
    BacktrackingScope(Parser &P) : P(P), PP(P.getParserPosition()) {}

    ~BacktrackingScope() {
      P.backtrackToPosition(PP);
    }
  };
  
  /// RAII object for managing 'var' patterns. Inside a 'var' pattern, bare
  /// identifiers are parsed as new VarDecls instead of references to existing
  /// ones.
  class VarPatternScope {
    Parser &P;
  public:
    VarPatternScope(Parser &P) : P(P) { ++P.VarPatternDepth; }
    ~VarPatternScope() { --P.VarPatternDepth; }
  };

  /// RAII object that, when it is destructed, restores the parser and lexer to
  /// their positions at the time the object was constructed.
  struct ParserPositionRAII {
  private:
    Parser &P;
    ParserPosition PP;

  public:
    ParserPositionRAII(Parser &P) : P(P), PP(P.getParserPosition()) {}

    ~ParserPositionRAII() {
      P.restoreParserPosition(PP);
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
  void skipUntil(tok T1, bool StopAtCodeComplete = true) {
    skipUntil(T1, tok::unknown, StopAtCodeComplete);
  }
  void skipUntil(tok T1, tok T2, bool StopAtCodeComplete = true);
  void skipUntilAnyOperator();
  
  /// skipUntilDeclStmtRBrace - Skip to the next decl or '}'.
  void skipUntilDeclRBrace();

  /// skipUntilDeclStmtRBrace - Skip to the next decl, statement or '}'.
  void skipUntilDeclStmtRBrace(bool StopAtCodeComplete = true);

  void skipUntilDeclRBrace(tok T1, tok T2 = tok::unknown,
                           bool StopAtCodeComplete = true);

private:
  /// Skip a single token, but match parentheses, braces, and square brackets.
  ///
  /// Note: this does \em not match angle brackets ("<" and ">")! These are
  /// matched in the source when they refer to a generic type, 
  /// but not when used as comparison operators.
  void skipSingle(bool StopAtCodeComplete = true);

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

  swift::ScopeInfo &getScopeInfo() { return State->getScopeInfo(); }

  /// \brief Add the given Decl to the current scope.
  void addToScope(ValueDecl *D) {
    getScopeInfo().addToScope(D, *this);
  }

  ValueDecl *lookupInScope(Identifier Name) {
    return getScopeInfo().lookupValueName(Name);
  }

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

  /// \brief Parse the list of statements, expressions, or declarations.
  ParserStatus parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                         tok SeparatorK, bool OptionalSep, Diag<> ErrorDiag,
                         std::function<ParserStatus()> callback);

  /// \brief Parse the list of statements, expressions, or declarations.
  ///
  /// \returns false on success, true on error.
  bool parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                 tok SeparatorK, bool OptionalSep, Diag<> ErrorDiag,
                 std::function<bool()> callback);

  void consumeTopLevelDecl(ParserPosition BeginParserPosition);

  void parseBraceItems(SmallVectorImpl<ExprStmtOrDecl> &Decls,
                       bool IsTopLevel,
                       BraceItemListKind Kind = BraceItemListKind::Brace);
  NullablePtr<BraceStmt> parseBraceItemList(Diag<> ID);

  void parseTopLevelCodeDeclDelayed();

  //===--------------------------------------------------------------------===//
  // Decl Parsing
  static bool isStartOfDecl(const Token &Tok, const Token &Tok2);
  static bool isStartOfOperatorDecl(const Token &Tok, const Token &Tok2);

  bool parseTranslationUnit(TranslationUnit *TU);
  void consumeDecl(ParserPosition BeginParserPosition, unsigned Flags);
  bool parseDecl(SmallVectorImpl<Decl*> &Entries, unsigned Flags);
  void parseDeclDelayed();
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
    PD_AllowDestructor      = 1 << 9,
    PD_AllowUnionElement    = 1 << 10,
    PD_InProtocol           = 1 << 11,
  };
  
  ParserResult<TypeDecl> parseDeclTypeAlias(bool WantDefinition,
                                            bool isAssociatedType);

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
  
  ParserResult<ImportDecl> parseDeclImport(unsigned Flags);
  bool parseInheritance(SmallVectorImpl<TypeLoc> &Inherited);
  ParserResult<ExtensionDecl> parseDeclExtension(unsigned Flags);
  ParserResult<UnionDecl> parseDeclUnion(unsigned Flags);
  ParserResult<UnionElementDecl> parseDeclUnionElement(unsigned Flags);
  bool parseNominalDeclMembers(SmallVectorImpl<Decl *> &memberDecls,
                               SourceLoc LBLoc, SourceLoc &RBLoc,
                               Diag<> ErrorDiag, unsigned flags);
  ParserResult<StructDecl> parseDeclStruct(unsigned Flags);
  ParserResult<ClassDecl> parseDeclClass(unsigned Flags);
  ParserStatus parseDeclVar(unsigned Flags, SmallVectorImpl<Decl *> &Decls);
  bool parseGetSet(bool HasContainerType, Pattern *Indices, TypeLoc ElementTy,
                   FuncDecl *&Get, FuncDecl *&Set, SourceLoc &LastValidLoc);
  void parseDeclVarGetSet(Pattern &pattern, bool hasContainerType);
  
  Pattern *buildImplicitThisParameter();
  void consumeFunctionBody(FuncExpr *FE);
  ParserResult<FuncDecl> parseDeclFunc(SourceLoc StaticLoc, unsigned Flags);
  bool parseDeclFuncBodyDelayed(FuncDecl *FD);
  ParserResult<ProtocolDecl> parseDeclProtocol(unsigned Flags);
  
  ParserStatus parseDeclSubscript(bool HasContainerType,
                                  bool NeedDefinition,
                                  SmallVectorImpl<Decl *> &Decls);

  ParserResult<ConstructorDecl> parseDeclConstructor(bool HasContainerType);
  ParserResult<DestructorDecl> parseDeclDestructor(unsigned Flags);
  
  ParserResult<OperatorDecl> parseDeclOperator(bool AllowTopLevel);
  ParserResult<OperatorDecl> parseDeclPrefixOperator(SourceLoc OperatorLoc,
                                                     SourceLoc PrefixLoc,
                                                     Identifier Name,
                                                     SourceLoc NameLoc);
  ParserResult<OperatorDecl> parseDeclPostfixOperator(SourceLoc OperatorLoc,
                                                      SourceLoc PostfixLoc,
                                                      Identifier Name,
                                                      SourceLoc NameLoc);
  ParserResult<OperatorDecl> parseDeclInfixOperator(SourceLoc OperatorLoc,
                                                    SourceLoc InfixLoc,
                                                    Identifier Name,
                                                    SourceLoc NameLoc);

  //===--------------------------------------------------------------------===//
  // SIL Parsing.

  bool parseDeclSIL();
  bool parseDeclSILStage();

  //===--------------------------------------------------------------------===//
  // Type Parsing
  
  ParserResult<TypeRepr> parseType();
  ParserResult<TypeRepr> parseType(Diag<> ID);
  ParserResult<TypeRepr> parseTypeAnnotation();
  ParserResult<TypeRepr> parseTypeAnnotation(Diag<> ID);
  ParserResult<TypeRepr> parseTypeSimple();
  ParserResult<TypeRepr> parseTypeSimple(Diag<> ID);
  bool parseGenericArguments(SmallVectorImpl<TypeRepr*> &Args,
                             SourceLoc &LAngleLoc,
                             SourceLoc &RAngleLoc);
  ParserResult<IdentTypeRepr> parseTypeIdentifier();
  ParserResult<ProtocolCompositionTypeRepr> parseTypeComposition();
  ParserResult<TupleTypeRepr> parseTypeTupleBody();
  ParserResult<ArrayTypeRepr> parseTypeArray(TypeRepr *Base);
  ParserResult<OptionalTypeRepr> parseTypeOptional(TypeRepr *Base);

  TypeRepr *applyAttributeToType(TypeRepr *Ty, DeclAttributes &Attr);

  //===--------------------------------------------------------------------===//
  // Pattern Parsing

  ParserStatus parseFunctionArguments(SmallVectorImpl<Pattern*> &ArgPatterns,
                                      SmallVectorImpl<Pattern*> &BodyPatterns);
  ParserStatus parseFunctionSignature(SmallVectorImpl<Pattern *> &argPatterns,
                                      SmallVectorImpl<Pattern *> &bodyPatterns,
                                      TypeRepr *&retLoc);

  ParserResult<Pattern> parsePattern();

  /// \brief Determine whether this token can start a pattern.
  bool isStartOfPattern(Token tok);
  
  /// \brief Determine whether this token can start a binding name, whether an
  /// identifier or the special discard-value binding '_'.
  bool isStartOfBindingName(Token tok);

  /// \brief Parse a tuple pattern element.
  ///
  /// \code
  ///   pattern-tuple-element:
  ///     pattern ('=' expr)?
  /// \endcode
  ///
  /// \param allowInitExpr Whether to allow initializers.
  ///
  /// \returns The tuple pattern element, if successful.
  std::pair<ParserStatus, Optional<TuplePatternElt>>
  parsePatternTupleElement(bool allowInitExpr);
  ParserResult<Pattern> parsePatternTuple(bool AllowInitExpr);
  ParserResult<Pattern> parsePatternAtom();
  ParserResult<Pattern> parsePatternIdentifier();
  
  Pattern *createBindingFromPattern(SourceLoc loc, Identifier name);
  
  //===--------------------------------------------------------------------===//
  // Pattern Parsing

  // TODO: Depending on how robust our distributive-var design works out, we
  // may be able to integrate exhaustive pattern parsing in var/func decls
  // with matching pattern parsing when it matures.
  
  ParserResult<Pattern> parseMatchingPattern();
  ParserResult<Pattern> parseMatchingPatternVar();
  ParserResult<Pattern> parseMatchingPatternIsa();
  
  /// \brief Determine whether this token can only start a matching pattern
  /// production and not an expression.
  bool isOnlyStartOfMatchingPattern();

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

  ParserResult<Expr> parseExpr(Diag<> ID, bool isExprBasic = false);
  ParserResult<Expr> parseExprBasic(Diag<> ID) {
    return parseExpr(ID, /*isExprBasic=*/true);
  }
  ParserResult<Expr> parseExprIs();
  ParserResult<Expr> parseExprAs();
  NullablePtr<Expr> parseExprSequence(Diag<> ID);
  NullablePtr<Expr> parseExprPostfix(Diag<> ID);
  NullablePtr<Expr> parseExprUnary(Diag<> ID);
  ParserResult<Expr> parseExprNew();
  ParserResult<Expr> parseExprSuper();
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

  /// Parse the closure signature, if present.
  ///
  ///   closure-signature:
  ///     pattern-tuple func-signature-result? 'in'
  ///     identifier (',' identifier)* func-signature-result? 'in'
  ///
  /// \param params The parsed parameter list, or null if none was provided.
  /// \param arrowLoc The location of the arrow, if present.
  /// \param explicitResultType The explicit result type, if specified.
  /// \param inLoc The location of the 'in' keyword, if present.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool parseClosureSignatureIfPresent(Pattern *&params,
                                      SourceLoc &arrowLoc,
                                      TypeRepr *&explicitResultType,
                                      SourceLoc &inLoc);

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
  static bool isStartOfStmt(const Token &Tok);
  NullablePtr<Stmt> parseStmt();
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
  bool parseStmtCaseLabels(SmallVectorImpl<CaseLabel*> &labels,
                           SmallVectorImpl<Decl *> &boundDecls);

  //===--------------------------------------------------------------------===//
  // Generics Parsing
  GenericParamList *parseGenericParameters();
  GenericParamList *parseGenericParameters(SourceLoc LAngleLoc);
  GenericParamList *maybeParseGenericParams();
  bool parseGenericWhereClause(SourceLoc &WhereLoc,
                               SmallVectorImpl<Requirement> &Requirements);
};

} // end namespace swift

#endif
