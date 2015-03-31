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
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/LocalContext.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Parse/Token.h"
#include "swift/Parse/ParserResult.h"
#include "swift/Basic/OptionSet.h"
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
  
  struct EnumElementInfo;
  
  /// Different contexts in which BraceItemList are parsed.
  enum class BraceItemListKind {
    /// A statement list terminated by a closing brace. The default.
    Brace,
    /// A statement list in a case block. The list is terminated
    /// by a closing brace or a 'case' or 'default' label.
    Case,
    /// The top-level of a file, when not in parse-as-library mode (i.e. the
    /// repl or a script).
    TopLevelCode,
    /// The top-level of a file, when in parse-as-library mode.
    TopLevelLibrary,
    /// The body of the inactive clause of an #if/#else/#endif block
    InactiveConfigBlock,
    /// The body of the active clause of an #if/#else/#endif block
    ActiveConfigBlock
  };

  
class Parser {
  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;

  bool IsInputIncomplete = false;

public:
  SourceManager &SourceMgr;
  DiagnosticEngine &Diags;
  SourceFile &SF;
  Lexer *L;
  SILParserState *SIL;    // Non-null when parsing a .sil file.
  PersistentParserState *State;
  std::unique_ptr<PersistentParserState> OwnedState;
  DeclContext *CurDeclContext;
  ASTContext &Context;
  CodeCompletionCallbacks *CodeCompletion = nullptr;
  std::vector<std::vector<VarDecl*>> AnonClosureVars;
  std::pair<const DeclContext *, ArrayRef<VarDecl *>> CurVars;
  llvm::SmallPtrSet<Decl *, 2> AlreadyHandledDecls;
  enum {
    /// InVarOrLetPattern has this value when not parsing a pattern.
    IVOLP_NotInVarOrLet,
    
    /// InVarOrLetPattern has this value when we're in a matching pattern, but
    /// not within a var/let pattern.  In this phase, identifiers are references
    /// to the enclosing scopes, not a variable binding.
    IVOLP_InMatchingPattern,
    
    /// InVarOrLetPattern has this value when parsing a pattern in which bound
    /// variables are implicitly immutable, but allowed to be marked mutable by
    /// using a 'var' pattern.  This happens in for-each loop patterns.
    IVOLP_ImplicitlyImmutable,
    
    /// When InVarOrLetPattern has this value, bound variables are mutable, and
    /// nested let/var patterns are not permitted. This happens when parsing a
    /// 'var' decl or when parsing inside a 'var' pattern.
    IVOLP_InVar,

    /// When InVarOrLetPattern has this value, bound variables are immutable,and
    /// nested let/var patterns are not permitted. This happens when parsing a
    /// 'let' decl or when parsing inside a 'let' pattern.
    IVOLP_InLet
  } InVarOrLetPattern = IVOLP_NotInVarOrLet;

  bool GreaterThanIsOperator = true;

  /// FIXME: Temporary hack to keep the selector-style declaration
  /// syntax working.
  bool ArgumentIsParameter = false;

  bool InPoundLineEnvironment = false;

  LocalContext *CurLocalContext = nullptr;

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
    return SF.isScriptMode();
  }

  /// Returns true if the parser reached EOF with incomplete source input, due
  /// for example, a missing right brace.
  bool isInputIncomplete() const { return IsInputIncomplete; }

  void checkForInputIncomplete() {
    IsInputIncomplete = IsInputIncomplete || Tok.is(tok::eof);
  }

  /// \brief This is the current token being considered by the parser.
  Token Tok;

  /// \brief The location of the previous token.
  SourceLoc PreviousLoc;
  
  /// A RAII object for temporarily changing CurDeclContext.
  class ContextChange {
  protected:
    Parser &P;
    DeclContext *OldContext; // null signals that this has been popped
    LocalContext *OldLocal;

    ContextChange(const ContextChange &) = delete;
    ContextChange &operator=(const ContextChange &) = delete;

  public:
    ContextChange(Parser &P, DeclContext *DC,
                  LocalContext *newLocal = nullptr)
      : P(P), OldContext(P.CurDeclContext), OldLocal(P.CurLocalContext) {
      assert(DC && "pushing null context?");
      P.CurDeclContext = DC;
      P.CurLocalContext = newLocal;
    }

    /// Prematurely pop the DeclContext installed by the constructor.
    /// Makes the destructor a no-op.
    void pop() {
      assert(OldContext && "already popped context!");
      popImpl();
      OldContext = nullptr;
    }

    ~ContextChange() {
      if (OldContext) popImpl();
    }

  private:
    void popImpl() {
      P.CurDeclContext = OldContext;
      P.CurLocalContext = OldLocal;
    }
  };

  /// A RAII object for parsing a new local context.
  class ParseFunctionBody : public LocalContext {
  private:
    ContextChange CC;
  public:
    ParseFunctionBody(Parser &P, DeclContext *DC) : CC(P, DC, this) {
      assert(!isa<TopLevelCodeDecl>(DC) &&
             "top-level code should be parsed using TopLevelCodeContext!");
    }

    void pop() {
      CC.pop();
    }
  };

  /// A RAII object for temporarily changing whether an operator starting with
  /// '>' is an operator.
  class GreaterThanIsOperatorRAII {
    Parser &P;
    bool OldValue;

  public:
    GreaterThanIsOperatorRAII(Parser &p, bool newValue)
      : P(p), OldValue(p.GreaterThanIsOperator)
    {
      P.GreaterThanIsOperator = newValue;
    }

    ~GreaterThanIsOperatorRAII() {
      P.GreaterThanIsOperator = OldValue;
    }
  };

  /// Describes the kind of a lexical structure marker, indicating
  /// what kind of structural element we started parsing at a
  /// particular location.
  enum class StructureMarkerKind : unsigned char {
    /// The start of a declaration.
    Declaration,
    /// The start of a statement.
    Statement,
    /// An open parentheses.
    OpenParen,
    /// An open brace.
    OpenBrace,
    /// An open square bracket.
    OpenSquare,
    /// An #if conditional clause.
    IfConfig,
  };

  /// A structure marker, which identifies the location at which the
  /// parser saw an entity it is parsing.
  struct StructureMarker {
    /// The location at which the marker occurred.
    SourceLoc Loc;

    /// The kind of marker.
    StructureMarkerKind Kind;

    /// The leading whitespace for this marker, if it has already been
    /// computed.
    Optional<StringRef> LeadingWhitespace;
  };

  /// An RAII object that notes when we have seen a structure marker.
  class StructureMarkerRAII {
    Parser &P;

  public:
    StructureMarkerRAII(Parser &parser, SourceLoc loc,
                               StructureMarkerKind kind)
      : P(parser) {
      P.StructureMarkers.push_back({loc, kind, None});
    }

    StructureMarkerRAII(Parser &parser, const Token &tok);

    ~StructureMarkerRAII() {
      P.StructureMarkers.pop_back();
    }
  };
  friend class StructureMarkerRAII;

  /// The stack of structure markers indicating the locations of
  /// structural elements actively being parsed, including the start
  /// of declarations, statements, and opening operators of various
  /// kinds.
  ///
  /// This vector is managed by \c StructureMarkerRAII objects.
  llvm::SmallVector<StructureMarker, 16> StructureMarkers;

public:
  Parser(unsigned BufferID, SourceFile &SF, SILParserState *SIL,
         PersistentParserState *PersistentState = nullptr);
  Parser(std::unique_ptr<Lexer> Lex, SourceFile &SF,
         SILParserState *SIL = nullptr,
         PersistentParserState *PersistentState = nullptr);
  ~Parser();

  bool isInSILMode() const { return SIL != nullptr; }

  //===--------------------------------------------------------------------===//
  // Routines to save and restore parser state.

  class ParserPosition {
  public:
    ParserPosition() = default;
    ParserPosition &operator=(const ParserPosition &) = default;

    bool isValid() const {
      return LS.isValid();
    }

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

  /// \brief Return parser position after the first character of token T
  ParserPosition getParserPositionAfterFirstCharacter(Token T);

  void restoreParserPosition(ParserPosition PP) {
    L->restoreState(PP.LS);

    // We might be at tok::eof now, so ensure that consumeToken() does not
    // assert about lexing past eof.
    Tok.setKind(tok::unknown);
    consumeToken();

    PreviousLoc = PP.PreviousLoc;
  }

  void backtrackToPosition(ParserPosition PP) {
    assert(PP.isValid());

    L->backtrackToState(PP.LS);

    // We might be at tok::eof now, so ensure that consumeToken() does not
    // assert about lexing past eof.
    Tok.setKind(tok::unknown);
    consumeToken();

    PreviousLoc = PP.PreviousLoc;
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

  SourceLoc consumeIdentifier(Identifier *Result = nullptr) {
    assert(Tok.is(tok::identifier) || Tok.is(tok::kw_self) ||
           Tok.is(tok::kw_Self) || Tok.is(tok::kw_throws));
    if (Result)
      *Result = Context.getIdentifier(Tok.getText());
    return consumeToken();
  }

  /// \brief Retrieve the location just past the end of the previous
  /// source location.
  SourceLoc getEndOfPreviousLoc();

  /// \brief If the current token is the specified kind, consume it and
  /// return true.  Otherwise, return false without consuming it.
  bool consumeIf(tok K) {
    if (Tok.isNot(K)) return false;
    consumeToken(K);
    return true;
  }

  /// \brief If the current token is the specified kind, consume it and
  /// return true.  Otherwise, return false without consuming it.
  bool consumeIf(tok K, SourceLoc &consumedLoc) {
    if (Tok.isNot(K)) return false;
    consumedLoc = consumeToken(K);
    return true;
  }

  bool consumeIfNotAtStartOfLine(tok K) {
    if (Tok.isAtStartOfLine()) return false;
    return consumeIf(K);
  }
  
  /// \brief Read tokens until we get to one of the specified tokens, then
  /// return without consuming it.  Because we cannot guarantee that the token
  /// will ever occur, this skips to some likely good stopping point.
  void skipUntil(tok T1) {
    skipUntil(T1, tok::unknown);
  }
  void skipUntil(tok T1, tok T2);
  void skipUntilAnyOperator();

  /// \brief Skip until a token that starts with '>'.  Applies heuristics that
  /// are suitable when trying to find the end of a list of generic parameters,
  /// generic arguments, or list of types in a protocol composition.
  void skipUntilGreaterInTypeList();

  /// skipUntilDeclStmtRBrace - Skip to the next decl or '}'.
  void skipUntilDeclRBrace();

  void skipUntilDeclStmtRBrace(tok T1);

  /// \brief Skip to the next decl, statement or '}'.
  void skipUntilDeclStmtRBrace() {
    skipUntilDeclStmtRBrace(tok::unknown);
  }

  void skipUntilDeclRBrace(tok T1, tok T2 = tok::unknown);
  
  /// \brief Skip until the next '#else', '#endif' or until eof.
  void skipUntilConfigBlockClose();

  /// Skip a single token, but match parentheses, braces, and square brackets.
  ///
  /// Note: this does \em not match angle brackets ("<" and ">")! These are
  /// matched in the source when they refer to a generic type,
  /// but not when used as comparison operators.
  void skipSingle();

public:
  InFlightDiagnostic diagnose(SourceLoc Loc, Diagnostic Diag) {
    if (Diags.isDiagnosticPointsToFirstBadToken(Diag.getID()) &&
        Loc == Tok.getLoc() && Tok.isAtStartOfLine())
      Loc = getEndOfPreviousLoc();
    return Diags.diagnose(Loc, Diag);
  }

  InFlightDiagnostic diagnose(Token Tok, Diagnostic Diag) {
    return diagnose(Tok.getLoc(), Diag);
  }

  template<typename ...DiagArgTypes, typename ...ArgTypes>
  InFlightDiagnostic diagnose(SourceLoc Loc, Diag<DiagArgTypes...> DiagID,
                              ArgTypes &&...Args) {
    return diagnose(Loc, Diagnostic(DiagID, std::forward<ArgTypes>(Args)...));
  }

  template<typename ...DiagArgTypes, typename ...ArgTypes>
  InFlightDiagnostic diagnose(Token Tok, Diag<DiagArgTypes...> DiagID,
                              ArgTypes &&...Args) {
    return diagnose(Tok.getLoc(),
                    Diagnostic(DiagID, std::forward<ArgTypes>(Args)...));
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

  /// \brief Consume the starting character of the current token, and split the
  /// remainder of the token into a new token (or tokens).
  SourceLoc consumeStartingCharacterOfCurrentToken();

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

  /// \brief Consume an identifier (but not an operator) if present and return
  /// its name in \p Result.  Otherwise, emit an error.
  ///
  /// \returns false on success, true on error.
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
  

  /// \brief Consume an identifier or operator if present and return its name
  /// in \p Result.  Otherwise, emit an error and return true.
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

  /// \brief The parser expects that \p K is next token in the input.  If so,
  /// it is consumed and false is returned.
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
  
  /// \brief Parse the specified expected token and return its location
  /// on success.  On failure, emit the specified error diagnostic, and
  /// a note at the specified note location.
  bool parseMatchingToken(tok K, SourceLoc &TokLoc, Diag<> ErrorDiag,
                          SourceLoc OtherLoc);

  /// \brief Parse the list of statements, expressions, or declarations.
  ParserStatus parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                         tok SeparatorK, bool OptionalSep,
                         bool AllowSepAfterLast, Diag<> ErrorDiag,
                         std::function<ParserStatus()> callback);

  void consumeTopLevelDecl(ParserPosition BeginParserPosition,
                           TopLevelCodeDecl *TLCD);

  ParserStatus parseBraceItems(SmallVectorImpl<ASTNode> &Decls,
                               BraceItemListKind Kind =
                                   BraceItemListKind::Brace,
                               BraceItemListKind ConfigKind =
                                   BraceItemListKind::Brace);
  ParserResult<BraceStmt> parseBraceItemList(Diag<> ID);
  
  void parseIfConfigClauseElements(bool isActive,
                                   BraceItemListKind Kind,
                                   SmallVectorImpl<ASTNode> &Elements);
  
  void parseTopLevelCodeDeclDelayed();

  //===--------------------------------------------------------------------===//
  // Decl Parsing

  /// Return true if parser is at the start of a decl or decl-import.
  bool isStartOfDecl();

  bool parseTopLevel();

  /// Flags that control the parsing of declarations.
  enum ParseDeclFlags {
    PD_Default              = 0,
    PD_AllowTopLevel        = 1 << 1,
    PD_HasContainerType     = 1 << 2,
    PD_DisallowNominalTypes = 1 << 3,
    PD_DisallowInit         = 1 << 4,
    PD_DisallowTypeAliasDef = 1 << 5,
    PD_AllowDestructor      = 1 << 6,
    PD_AllowEnumElement     = 1 << 7,
    PD_InProtocol           = 1 << 8,
    PD_InClass              = 1 << 9,
    PD_InExtension          = 1 << 10,
    PD_InStruct             = 1 << 11,
    PD_InEnum               = 1 << 12,
    PD_InLoop               = 1 << 13
  };

  /// Options that control the parsing of declarations.
  typedef OptionSet<ParseDeclFlags> ParseDeclOptions;

  /// Skips the current token if it is '}', and emits a diagnostic.
  ///
  /// \returns true if any tokens were skipped.
  bool skipExtraTopLevelRBraces();

  void delayParseFromBeginningToHere(ParserPosition BeginParserPosition,
                                     ParseDeclOptions Flags);
  void consumeDecl(ParserPosition BeginParserPosition, ParseDeclOptions Flags,
                   bool IsTopLevel);

  // When compiling for the Debugger, some Decl's need to be moved from the
  // current scope.  In which case although the Decl will be returned in the
  // ParserResult, it should not be inserted into the Decl list for the current
  // context.  markWasHandled asserts that the Decl is already where it
  // belongs, and declWasHandledAlready is used to check this assertion.
  // To keep the handled decl array small, we remove the Decl when it is
  // checked, so you can only call declWasAlreadyHandled once for a given
  // decl.

  void markWasHandled(Decl *D) {
    AlreadyHandledDecls.insert(D);
  }

  bool declWasHandledAlready(Decl *D) {
    return AlreadyHandledDecls.erase(D);
  }

  ParserStatus parseDecl(SmallVectorImpl<Decl*> &Entries, ParseDeclOptions Flags);
  void parseDeclDelayed();

  ParserResult<TypeDecl> parseDeclTypeAlias(bool WantDefinition,
                                            bool isAssociatedType,
                                            DeclAttributes &Attributes);
  
  ParserResult<IfConfigDecl> parseDeclIfConfig(ParseDeclOptions Flags);
  /// Parse a #line directive.
  ParserStatus parseLineDirective();

  void setLocalDiscriminator(ValueDecl *D);

  /// Parse the optional attributes before a declaration.
  bool parseDeclAttributeList(DeclAttributes &Attributes,
                              bool &FoundCodeCompletionToken,
                              bool StopAtTypeAttributes = false,
                              bool InParam = false);

  /// Parse a specific attribute.
  bool parseDeclAttribute(DeclAttributes &Attributes, SourceLoc AtLoc);

  bool parseNewDeclAttribute(DeclAttributes &Attributes, SourceLoc AtLoc,
                             DeclAttrKind DK);
  
  /// Parse a version tuple of the form x[.y[.z]]. Returns true if there was
  /// an error parsing.
  bool parseVersionTuple(clang::VersionTuple &Version, SourceRange &Range,
                         const Diagnostic &D);

  bool parseTypeAttributeList(TypeAttributes &Attributes) {
    if (Tok.is(tok::at_sign))
      return parseTypeAttributeListPresent(Attributes);
    return false;
  }
  bool parseTypeAttributeListPresent(TypeAttributes &Attributes);
  bool parseTypeAttribute(TypeAttributes &Attributes,
                          bool justChecking = false);
  
  
  ParserResult<ImportDecl> parseDeclImport(ParseDeclOptions Flags,
                                           DeclAttributes &Attributes);
  ParserStatus parseInheritance(SmallVectorImpl<TypeLoc> &Inherited,
                                SourceLoc *classRequirementLoc);
  ParserResult<ExtensionDecl> parseDeclExtension(ParseDeclOptions Flags,
                                                 DeclAttributes &Attributes);
  ParserResult<EnumDecl> parseDeclEnum(ParseDeclOptions Flags,
                                       DeclAttributes &Attributes);
  ParserStatus parseDeclEnumCase(ParseDeclOptions Flags, DeclAttributes &Attributes,
                                 SmallVectorImpl<Decl *> &decls);
  bool parseNominalDeclMembers(SmallVectorImpl<Decl *> &memberDecls,
                               SourceLoc LBLoc, SourceLoc &RBLoc,
                               Diag<> ErrorDiag, ParseDeclOptions flags);
  ParserResult<StructDecl>
  parseDeclStruct(ParseDeclOptions Flags, DeclAttributes &Attributes);
  ParserResult<ClassDecl>
  parseDeclClass(SourceLoc ClassLoc,
                 ParseDeclOptions Flags, DeclAttributes &Attributes);
  ParserStatus parseDeclVar(ParseDeclOptions Flags, DeclAttributes &Attributes,
                            SmallVectorImpl<Decl *> &Decls,
                            SourceLoc StaticLoc,
                            StaticSpellingKind StaticSpelling);

  void consumeGetSetBody(AbstractFunctionDecl *AFD, SourceLoc LBLoc);

  struct ParsedAccessors {
    SourceLoc LBLoc, RBLoc;
    FuncDecl *Get = nullptr;
    FuncDecl *Set = nullptr;
    FuncDecl *Addressor = nullptr;
    FuncDecl *MutableAddressor = nullptr;
    FuncDecl *WillSet = nullptr;
    FuncDecl *DidSet = nullptr;

    void record(Parser &P, AbstractStorageDecl *storage, bool invalid,
                ParseDeclOptions flags, SourceLoc staticLoc,
                const DeclAttributes &attrs,
                TypeLoc elementTy, Pattern *indices,
                SmallVectorImpl<Decl *> &decls);
  };

  bool parseGetSetImpl(ParseDeclOptions Flags,
                       Pattern *Indices, TypeLoc ElementTy,
                       ParsedAccessors &accessors,
                       SourceLoc &LastValidLoc,
                       SourceLoc StaticLoc, SourceLoc VarLBLoc,
                       SmallVectorImpl<Decl *> &Decls);
  bool parseGetSet(ParseDeclOptions Flags,
                   Pattern *Indices, TypeLoc ElementTy,
                   ParsedAccessors &accessors,
                   SourceLoc StaticLoc, SmallVectorImpl<Decl *> &Decls);
  void recordAccessors(AbstractStorageDecl *storage, ParseDeclOptions flags,
                       TypeLoc elementTy, const DeclAttributes &attrs,
                       SourceLoc staticLoc, ParsedAccessors &accessors);
  void parseAccessorBodyDelayed(AbstractFunctionDecl *AFD);
  VarDecl *parseDeclVarGetSet(Pattern *pattern, ParseDeclOptions Flags,
                              SourceLoc StaticLoc, bool hasInitializer,
                              const DeclAttributes &Attributes,
                              SmallVectorImpl<Decl *> &Decls);
  
  void consumeAbstractFunctionBody(AbstractFunctionDecl *AFD,
                                   const DeclAttributes &Attrs);
  ParserResult<FuncDecl> parseDeclFunc(SourceLoc StaticLoc,
                                       StaticSpellingKind StaticSpelling,
                                       ParseDeclOptions Flags,
                                       DeclAttributes &Attributes);
  bool parseAbstractFunctionBodyDelayed(AbstractFunctionDecl *AFD);
  ParserResult<ProtocolDecl> parseDeclProtocol(ParseDeclOptions Flags,
                                               DeclAttributes &Attributes);

  ParserStatus parseDeclSubscript(ParseDeclOptions Flags,
                                  DeclAttributes &Attributes,
                                  SmallVectorImpl<Decl *> &Decls);

  ParserResult<ConstructorDecl>
  parseDeclInit(ParseDeclOptions Flags, DeclAttributes &Attributes);
  ParserResult<DestructorDecl>
  parseDeclDeinit(ParseDeclOptions Flags, DeclAttributes &Attributes);

  void addPatternVariablesToScope(ArrayRef<Pattern *> Patterns);

  ParserStatus parseDeclOperator(ParseDeclOptions Flags,
                                 DeclAttributes &Attributes,
                                 SmallVectorImpl<Decl *> &Decls);
  ParserResult<OperatorDecl> parseDeclPrefixOperator(SourceLoc OperatorLoc,
                                                     Identifier Name,
                                                     SourceLoc NameLoc,
                                                     DeclAttributes &Attrs);
  ParserResult<OperatorDecl> parseDeclPostfixOperator(SourceLoc OperatorLoc,
                                                      Identifier Name,
                                                      SourceLoc NameLoc,
                                                      DeclAttributes &Attrs);
  ParserStatus parseDeclInfixOperator(SourceLoc OperatorLoc,
                                      Identifier Name,
                                      SourceLoc NameLoc,
                                      DeclAttributes &Attributes,
                                      SmallVectorImpl<Decl *> &Decls);

  //===--------------------------------------------------------------------===//
  // SIL Parsing.

  bool parseDeclSIL();
  bool parseDeclSILStage();
  bool parseSILVTable();
  bool parseSILGlobal();
  bool parseSILWitnessTable();
  bool parseSILCoverageMap();

  //===--------------------------------------------------------------------===//
  // Type Parsing
  
  ParserResult<TypeRepr> parseType();
  ParserResult<TypeRepr> parseType(Diag<> MessageID);

  /// \brief Parse any type, but diagnose all types except type-identifier.
  ///
  /// In some places the grammar allows type-identifier, but when it is not
  /// ambiguous, we want to parse any type for recovery purposes.
  ///
  /// \param MessageID a generic diagnostic for a syntax error in the type
  /// \param NonIdentifierTypeMessageID a diagnostic for a non-identifier type
  ///
  /// \returns null, IdentTypeRepr or ErrorTypeRepr.
  ParserResult<TypeRepr>
  parseTypeIdentifierWithRecovery(Diag<> MessageID,
                                  Diag<TypeLoc> NonIdentifierTypeMessageID);

  ParserResult<TypeRepr> parseTypeSimple();
  ParserResult<TypeRepr> parseTypeSimple(Diag<> MessageID);
  bool parseGenericArguments(SmallVectorImpl<TypeRepr*> &Args,
                             SourceLoc &LAngleLoc,
                             SourceLoc &RAngleLoc);
  ParserResult<IdentTypeRepr> parseTypeIdentifier();

  ParserResult<ProtocolCompositionTypeRepr> parseTypeComposition();
  ParserResult<TupleTypeRepr> parseTypeTupleBody();
  ParserResult<ArrayTypeRepr> parseTypeArray(TypeRepr *Base);

  /// Parse a collection type.
  ///   type-simple:
  ///     '[' type ']'
  ///     '[' type ':' type ']'
  ParserResult<TypeRepr> parseTypeCollection();
  ParserResult<OptionalTypeRepr> parseTypeOptional(TypeRepr *Base);

  ParserResult<ImplicitlyUnwrappedOptionalTypeRepr>
    parseTypeImplicitlyUnwrappedOptional(TypeRepr *Base);

  bool isOptionalToken(const Token &T) const;
  SourceLoc consumeOptionalToken();
  
  bool isImplicitlyUnwrappedOptionalToken(const Token &T) const;
  SourceLoc consumeImplicitlyUnwrappedOptionalToken();

  TypeRepr *applyAttributeToType(TypeRepr *Ty, const TypeAttributes &Attr);

  //===--------------------------------------------------------------------===//
  // Pattern Parsing

  /// A structure for collecting information about the default
  /// arguments of a context.
  struct DefaultArgumentInfo {
    llvm::SmallVector<DefaultArgumentInitializer *, 4> ParsedContexts;
    unsigned NextIndex : 31;
    
    /// Track whether or not one of the parameters in a signature's argument
    /// list accepts a default argument.
    unsigned HasDefaultArgument : 1;

    /// Claim the next argument index.  It's important to do this for
    /// all the arguments, not just those that have default arguments.
    unsigned claimNextIndex() { return NextIndex++; }

    /// Set the parsed context for all the initializers to the given
    /// function.
    void setFunctionContext(DeclContext *DC);
    
    DefaultArgumentInfo() {
      NextIndex = 0;
      HasDefaultArgument = false;
    }
  };

  /// Describes a parsed parameter.
  struct ParsedParameter {
    /// Any declaration attributes attached to the parameter.
    DeclAttributes Attrs;

    /// The location of the 'let', 'var', or 'inout' keyword, if present.
    ///
    SourceLoc LetVarInOutLoc;

    enum SpecifierKindTy {
      Let,
      Var,
      InOut
    };
    SpecifierKindTy SpecifierKind = Let; // Defaults to let.

    
    /// The location of the back-tick preceding the first name, if any.
    SourceLoc PoundLoc;

    /// The location of the first name.
    ///
    /// \c FirstName is the name.
    SourceLoc FirstNameLoc;

    /// The location of the second name, if present.
    ///
    /// \p SecondName is the name.
    SourceLoc SecondNameLoc;

    /// The location of the ':', if present, indicating that a type is
    /// provided.
    SourceLoc ColonLoc;

    /// The location of the '...', if present.
    SourceLoc EllipsisLoc;

    /// The location of the '=', if present, indicating that a default argument
    /// is provided.
    SourceLoc EqualLoc;

    /// The first name.
    Identifier FirstName;

    /// The second name, the presence of which is indicated by \c SecondNameLoc.
    Identifier SecondName;

    /// The type following the ':'.
    TypeRepr *Type = nullptr;

    /// The default argument for this parameter.
    ExprHandle *DefaultArg = nullptr;
  };

  /// Describes the context in which the given parameter is being parsed.
  enum class ParameterContextKind {
    /// An operator.
    Operator,
    /// A function that is not a method.
    Function,
    /// A method.
    Method,
    /// An initializer.
    Initializer,
    /// A closure.
    Closure,
    /// A subscript.
    Subscript
  };

  /// Parse a parameter-clause.
  ///
  /// \verbatim
  ///   parameter-clause:
  ///     '(' ')'
  ///     '(' parameter (',' parameter)* '...'? )'
  ///
  ///   parameter:
  ///     'inout'? ('let' | 'var')? '`'? identifier-or-none identifier-or-none?
  ///         (':' type)? ('...' | '=' expr)?
  ///
  ///   identifier-or-none:
  ///     identifier
  ///     '_'
  /// \endverbatim
  ParserStatus parseParameterClause(SourceLoc &leftParenLoc,
                                    SmallVectorImpl<ParsedParameter> &params,
                                    SourceLoc &rightParenLoc,
                                    DefaultArgumentInfo *defaultArgs,
                                    ParameterContextKind paramContext);

  ParserResult<Pattern> parseSingleParameterClause(
                          ParameterContextKind paramContext,
                          SmallVectorImpl<Identifier> *namePieces = nullptr);

  ParserStatus parseFunctionArguments(SmallVectorImpl<Identifier> &NamePieces,
                                      SmallVectorImpl<Pattern*> &BodyPatterns,
                                      ParameterContextKind paramContext,
                                      DefaultArgumentInfo &defaultArgs);
  ParserStatus parseFunctionSignature(Identifier functionName,
                                      DeclName &fullName,
                                      SmallVectorImpl<Pattern *> &bodyPatterns,
                                      DefaultArgumentInfo &defaultArgs,
                                      bool &throws,
                                      TypeRepr *&retType);
  ParserStatus parseConstructorArguments(DeclName &FullName,
                                         Pattern *&BodyPattern,
                                         DefaultArgumentInfo &defaultArgs);

  //===--------------------------------------------------------------------===//
  // Pattern Parsing

  ParserResult<Pattern> parseTypedPattern();
  ParserResult<Pattern> parsePattern();
  
  /// \brief Parse a tuple pattern element.
  ///
  /// \code
  ///   pattern-tuple-element:
  ///     pattern ('=' expr)?
  /// \endcode
  ///
  /// \returns The tuple pattern element, if successful.
  std::pair<ParserStatus, Optional<TuplePatternElt>>
  parsePatternTupleElement();
  ParserResult<Pattern> parsePatternTuple();
  ParserResult<Pattern> parsePatternTupleAfterLP(SourceLoc LPLoc);
  
  ParserResult<Pattern> parseTypedMatchingPattern();
  ParserResult<Pattern> parseMatchingPattern(bool isExprBasic);
  ParserResult<Pattern> parseMatchingPatternAsLetOrVar(bool isLet,
                                                       SourceLoc VarLoc,
                                                       bool isExprBasic);
  ParserResult<Pattern> parseSwift1IfLetPattern(bool isLet, SourceLoc VarLoc);
  

  Pattern *createBindingFromPattern(SourceLoc loc, Identifier name, bool isLet);
  

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
  bool canParseAttributes();

  bool canParseType();
  bool canParseTypeIdentifier();
  bool canParseTypeComposition();
  bool canParseTypeTupleBody();
  bool canParseTypeAttribute();
  bool canParseGenericArguments();

  bool canParseTypedPattern();

  //===--------------------------------------------------------------------===//
  // Expression Parsing
  ParserResult<Expr> parseExpr(Diag<> ID) {
    return parseExprImpl(ID, /*isExprBasic=*/false);
  }
  ParserResult<Expr> parseExprBasic(Diag<> ID) {
    return parseExprImpl(ID, /*isExprBasic=*/true);
  }
  ParserResult<Expr> parseExprImpl(Diag<> ID, bool isExprBasic = false);
  ParserResult<Expr> parseExprIs();
  ParserResult<Expr> parseExprAs();
  ParserResult<Expr> parseExprSequence(Diag<> ID,
                                       bool isExprBasic,
                                       bool isConfigCondition = false);
  ParserResult<Expr> parseExprSequenceElement(Diag<> ID,
                                              bool isExprBasic);
  ParserResult<Expr> parseExprPostfix(Diag<> ID, bool isExprBasic);
  ParserResult<Expr> parseExprUnary(Diag<> ID, bool isExprBasic);
  ParserResult<Expr> parseExprSuper();
  ParserResult<Expr> parseExprConfiguration();
  Expr *parseExprStringLiteral();
  
  Expr *parseExprIdentifier();
  Expr *parseExprEditorPlaceholder(Token PlaceholderTok,
                                   Identifier PlaceholderId);

  /// \brief Parse a closure expression after the opening brace.
  ///
  /// \verbatim
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
  /// \endverbatim
  ParserResult<Expr> parseExprClosure();

  /// \brief Parse the closure signature, if present.
  ///
  /// \verbatim
  ///   closure-signature:
  ///     parameter-clause func-signature-result? 'in'
  ///     identifier (',' identifier)* func-signature-result? 'in'
  /// \endverbatim
  ///
  /// \param captureList The entries in the capture list.
  /// \param params The parsed parameter list, or null if none was provided.
  /// \param arrowLoc The location of the arrow, if present.
  /// \param explicitResultType The explicit result type, if specified.
  /// \param inLoc The location of the 'in' keyword, if present.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool parseClosureSignatureIfPresent(
                                SmallVectorImpl<CaptureListEntry> &captureList,
                                      Pattern *&params,
                                      SourceLoc &arrowLoc,
                                      TypeRepr *&explicitResultType,
                                      SourceLoc &inLoc);

  Expr *parseExprAnonClosureArg();
  ParserResult<Expr> parseExprList(tok LeftTok, tok RightTok);
  ParserResult<Expr> parseExprCallSuffix(ParserResult<Expr> fn,
                                         Identifier firstSelectorPiece
                                           = Identifier(),
                                         SourceLoc firstSelectorPieceLoc
                                           = SourceLoc());
  ParserResult<Expr> parseExprCollection();
  ParserResult<Expr> parseExprArray(SourceLoc LSquareLoc, Expr *FirstExpr);
  ParserResult<Expr> parseExprDictionary(SourceLoc LSquareLoc, Expr *FirstKey);

  UnresolvedDeclRefExpr *parseExprOperator();

  //===--------------------------------------------------------------------===//
  // Statement Parsing

  bool isStartOfStmt();
  ParserResult<Stmt> parseStmt();
  ParserStatus parseExprOrStmt(ASTNode &Result);
  ParserResult<Stmt> parseStmtBreak();
  ParserResult<Stmt> parseStmtContinue();
  ParserResult<Stmt> parseStmtReturn();
  ParserStatus parseStmtCondition(StmtCondition &Result, Diag<> ID);
  ParserResult<Stmt> parseStmtIf(LabeledStmtInfo LabelInfo);
  ParserResult<Stmt> parseStmtIfConfig(BraceItemListKind Kind
                                        = BraceItemListKind::Brace);
  ParserResult<Stmt> parseStmtWhile(LabeledStmtInfo LabelInfo);
  ParserResult<Stmt> parseStmtDo(LabeledStmtInfo LabelInfo);
  ParserResult<CatchStmt> parseStmtCatch();
  ParserResult<Stmt> parseStmtFor(LabeledStmtInfo LabelInfo);
  ParserResult<Stmt> parseStmtForCStyle(SourceLoc ForLoc,
                                        LabeledStmtInfo LabelInfo);
  ParserResult<Stmt> parseStmtForEach(SourceLoc ForLoc,
                                      LabeledStmtInfo LabelInfo);
  ParserResult<Stmt> parseStmtSwitch(LabeledStmtInfo LabelInfo);
  ParserResult<CaseStmt> parseStmtCase();

  /// Evaluate the conditional configuration expression of an #if statement
  bool evaluateConfigConditionExpr(Expr *configExpr);
  
  /// Decide whether the current token pointing to an #if block should be parsed
  /// as a statement or as a declaration
  bool isStartOfIfConfigDecl();

  //===--------------------------------------------------------------------===//
  // Generics Parsing

  GenericParamList *parseGenericParameters();
  GenericParamList *parseGenericParameters(SourceLoc LAngleLoc);
  GenericParamList *maybeParseGenericParams();
  bool parseGenericWhereClause(SourceLoc &WhereLoc,
                               SmallVectorImpl<RequirementRepr> &Requirements);

  //===--------------------------------------------------------------------===//
  // Availability Specification Parsing
  
  ParserResult<VersionConstraintAvailabilitySpec> parseVersionConstraintSpec();
  
  /// Parse a version comparison operator. Returns true on error.
  bool parseVersionComparison(VersionComparison &Comparison, SourceLoc &OpLoc);
};

} // end namespace swift

#endif
