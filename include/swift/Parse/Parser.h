//===--- Parser.h - Swift Language Parser -----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Parser interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSER_H
#define SWIFT_PARSER_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/Expr.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/LayoutConstraint.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/LocalContext.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Parse/Token.h"
#include "swift/Parse/ParserPosition.h"
#include "swift/Parse/ParserResult.h"
#include "swift/Parse/SyntaxParserResult.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "swift/Config.h"
#include "llvm/ADT/SetVector.h"

namespace llvm {
  template <typename PT1, typename PT2, typename PT3> class PointerUnion3;
}

namespace swift {
  class CodeCompletionCallbacks;
  class DefaultArgumentInitializer;
  class DelayedParsingCallbacks;
  class DiagnosticEngine;
  class Expr;
  class Lexer;
  class PersistentParserState;
  class SILParserTUStateBase;
  class ScopeInfo;
  class SourceManager;
  class TupleType;
  struct TypeLoc;
  
  struct EnumElementInfo;
  
  namespace syntax {
    class AbsolutePosition;
    class RawSyntax;
    enum class SyntaxKind;
    class TypeSyntax;
  }// end of syntax namespace

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
    InactiveConditionalBlock,
    /// The body of the active clause of an #if/#else/#endif block
    ActiveConditionalBlock,
  };

/// The receiver will be fed with consumed tokens while parsing. The main purpose
/// is to generate a corrected token stream for tooling support like syntax
/// coloring.
class ConsumeTokenReceiver {
public:
  /// This is called when a token is consumed.
  virtual void receive(Token Tok) {}

  /// This is called to update the kind of a token whose start location is Loc.
  virtual void registerTokenKindChange(SourceLoc Loc, tok NewKind) {};

  /// This is called when a source file is fully parsed.
  virtual void finalize() {};
  virtual ~ConsumeTokenReceiver() = default;
};

/// The main class used for parsing a source file (.swift or .sil).
///
/// Rather than instantiating a Parser yourself, use one of the parsing APIs
/// provided in Subsystems.h.
class Parser {
  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;

  bool IsInputIncomplete = false;
  std::vector<Token> SplitTokens;

public:
  SourceManager &SourceMgr;
  DiagnosticEngine &Diags;
  SourceFile &SF;
  Lexer *L;
  SILParserTUStateBase *SIL; // Non-null when parsing a .sil file.
  PersistentParserState *State;
  std::unique_ptr<PersistentParserState> OwnedState;
  DeclContext *CurDeclContext;
  ASTContext &Context;
  CodeCompletionCallbacks *CodeCompletion = nullptr;
  std::vector<std::pair<SourceLoc, std::vector<ParamDecl*>>> AnonClosureVars;

  bool IsParsingInterfaceTokens = false;
  
  /// DisabledVars is a list of variables for whom local name lookup is
  /// disabled.  This is used when parsing a PatternBindingDecl to reject self
  /// uses and to disable uses of the bound variables in a let/else block.  The
  /// diagnostic to emit is stored in DisabledVarReason.
  ArrayRef<VarDecl *> DisabledVars;
  Diag<> DisabledVarReason;
  
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

  bool InPoundLineEnvironment = false;
  bool InPoundIfEnvironment = false;
  bool InSwiftKeyPath = false;

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

  bool allowTopLevelCode() const;

  const std::vector<Token> &getSplitTokens() { return SplitTokens; }

  void markSplitToken(tok Kind, StringRef Txt);

  /// Returns true if the parser reached EOF with incomplete source input, due
  /// for example, a missing right brace.
  bool isInputIncomplete() const { return IsInputIncomplete; }

  void checkForInputIncomplete() {
    IsInputIncomplete = IsInputIncomplete ||
      // Check whether parser reached EOF but the real EOF, not the end of a
      // string interpolation segment.
      (Tok.is(tok::eof) && Tok.getText() != ")");
  }

  /// \brief This is the current token being considered by the parser.
  Token Tok;

  /// \brief leading trivias for \c Tok.
  /// Always empty if !SF.shouldBuildSyntaxTree().
  syntax::Trivia LeadingTrivia;

  /// \brief trailing trivias for \c Tok.
  /// Always empty if !SF.shouldBuildSyntaxTree().
  syntax::Trivia TrailingTrivia;

  /// \brief The receiver to collect all consumed tokens.
  ConsumeTokenReceiver *TokReceiver;

  /// \brief The location of the previous token.
  SourceLoc PreviousLoc;

  /// Stop parsing immediately.
  void cutOffParsing() {
    // Cut off parsing by acting as if we reached the end-of-file.
    Tok.setKind(tok::eof);
  }
  
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

  /// Describes the kind of a lexical structure marker, indicating
  /// what kind of structural element we started parsing at a
  /// particular location.
  enum class StructureMarkerKind : uint8_t {
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
    Parser *const P;

    /// Max nesting level
    // TODO: customizable.
    enum { MaxDepth = 256 };

    StructureMarkerRAII(Parser *parser) : P(parser) {}

    /// Have the parser start the new Structure or fail if already too deep.
    bool pushStructureMarker(Parser &parser, SourceLoc loc,
                             StructureMarkerKind kind);

  public:
    StructureMarkerRAII(Parser &parser, SourceLoc loc, StructureMarkerKind kind)
        : StructureMarkerRAII(
              pushStructureMarker(parser, loc, kind) ? &parser : nullptr) {}

    StructureMarkerRAII(Parser &parser, const Token &tok);

    /// Did we fail to push the new structure?
    bool isFailed() {
      return P == nullptr;
    }

    ~StructureMarkerRAII() {
      if (P != nullptr) {
        P->StructureMarkers.pop_back();
      }
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

  /// Current syntax parsing context where call backs should be directed to.
  SyntaxParsingContext *SyntaxContext;

public:
  Parser(unsigned BufferID, SourceFile &SF, DiagnosticEngine* LexerDiags,
         SILParserTUStateBase *SIL,
         PersistentParserState *PersistentState);
  Parser(unsigned BufferID, SourceFile &SF, SILParserTUStateBase *SIL,
         PersistentParserState *PersistentState = nullptr);
  Parser(std::unique_ptr<Lexer> Lex, SourceFile &SF,
         SILParserTUStateBase *SIL = nullptr,
         PersistentParserState *PersistentState = nullptr);
  ~Parser();

  bool isInSILMode() const { return SIL != nullptr; }

  /// Calling this function to finalize libSyntax tree creation without destroying
  /// the parser instance.
  void finalizeSyntaxTree() {
    assert(Tok.is(tok::eof) && "not done parsing yet");
    SyntaxContext->finalizeRoot();
  }

  //===--------------------------------------------------------------------===//
  // Routines to save and restore parser state.

  ParserPosition getParserPosition() {
    return ParserPosition(L->getStateForBeginningOfToken(Tok, LeadingTrivia),
                          PreviousLoc);
  }

  ParserPosition getParserPosition(const PersistentParserState::ParserPos &Pos){
    return ParserPosition(L->getStateForBeginningOfTokenLoc(Pos.Loc),
                          Pos.PrevLoc);
  }

  void restoreParserPosition(ParserPosition PP, bool enableDiagnostics = false) {
    L->restoreState(PP.LS, enableDiagnostics);
    L->lex(Tok, LeadingTrivia, TrailingTrivia);
    PreviousLoc = PP.PreviousLoc;
  }

  void backtrackToPosition(ParserPosition PP) {
    assert(PP.isValid());
    L->backtrackToState(PP.LS);
    L->lex(Tok, LeadingTrivia, TrailingTrivia);
    PreviousLoc = PP.PreviousLoc;
  }

  /// RAII object that, when it is destructed, restores the parser and lexer to
  /// their positions at the time the object was constructed.  Will not jump
  /// forward in the token stream.
  class BacktrackingScope {
    Parser &P;
    ParserPosition PP;
    DiagnosticTransaction DT;
    /// This context immediately deconstructed with transparent accumulation
    /// on cancelBacktrack().
    llvm::Optional<SyntaxParsingContext> SynContext;
    bool Backtrack = true;

  public:
    BacktrackingScope(Parser &P)
        : P(P), PP(P.getParserPosition()), DT(P.Diags) {
      SynContext.emplace(P.SyntaxContext);
      SynContext->setBackTracking();
    }

    ~BacktrackingScope();
    bool willBacktrack() const { return Backtrack; }

    void cancelBacktrack() {
      Backtrack = false;
      SynContext->setTransparent();
      SynContext.reset();
      DT.commit();
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

  /// Consume a token that we created on the fly to correct the original token
  /// stream from lexer.
  void consumeExtraToken(Token K);
  SourceLoc consumeTokenWithoutFeedingReceiver();
  SourceLoc consumeToken();
  SourceLoc consumeToken(tok K) {
    assert(Tok.is(K) && "Consuming wrong token kind");
    return consumeToken();
  }

  SourceLoc consumeIdentifier(Identifier *Result = nullptr) {
    assert(Tok.isAny(tok::identifier, tok::kw_self, tok::kw_Self));
    if (Result)
      *Result = Context.getIdentifier(Tok.getText());
    return consumeToken();
  }

  SourceLoc consumeArgumentLabel(Identifier &Result) {
    assert(Tok.canBeArgumentLabel());
    assert(Result.empty());
    if (!Tok.is(tok::kw__)) {
      Tok.setKind(tok::identifier);
      Result = Context.getIdentifier(Tok.getText());
    }
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

  bool isContextualYieldKeyword() {
    return (Tok.isContextualKeyword("yield") &&
            isa<AccessorDecl>(CurDeclContext) &&
            cast<AccessorDecl>(CurDeclContext)->isCoroutine());
  }
  
  /// \brief Read tokens until we get to one of the specified tokens, then
  /// return without consuming it.  Because we cannot guarantee that the token
  /// will ever occur, this skips to some likely good stopping point.
  void skipUntil(tok T1, tok T2 = tok::NUM_TOKENS);
  void skipUntilAnyOperator();

  /// \brief Skip until a token that starts with '>', and consume it if found.
  /// Applies heuristics that are suitable when trying to find the end of a list
  /// of generic parameters, generic arguments, or list of types in a protocol
  /// composition.
  SourceLoc skipUntilGreaterInTypeList(bool protocolComposition = false);

  /// skipUntilDeclStmtRBrace - Skip to the next decl or '}'.
  void skipUntilDeclRBrace();

  void skipUntilDeclStmtRBrace(tok T1);
  void skipUntilDeclStmtRBrace(tok T1, tok T2);

  void skipUntilDeclRBrace(tok T1, tok T2);
  
  /// Skip a single token, but match parentheses, braces, and square brackets.
  ///
  /// Note: this does \em not match angle brackets ("<" and ">")! These are
  /// matched in the source when they refer to a generic type,
  /// but not when used as comparison operators.
  void skipSingle();

  /// \brief Skip until the next '#else', '#endif' or until eof.
  void skipUntilConditionalBlockClose();

  /// \brief Skip until either finding \c T1 or reaching the end of the line.
  ///
  /// This uses \c skipSingle and so matches parens etc. After calling, one or
  /// more of the following will be true: Tok.is(T1), Tok.isStartOfLine(),
  /// Tok.is(tok::eof). The "or more" case is the first two: if the next line
  /// starts with T1.
  ///
  /// \returns true if there is an instance of \c T1 on the current line (this
  /// avoids the foot-gun of not considering T1 starting the next line for a
  /// plain Tok.is(T1) check).
  bool skipUntilTokenOrEndOfLine(tok T1);

  /// If the parser is generating only a syntax tree, try loading the current
  /// node from a previously generated syntax tree.
  /// Returns \c true if the node has been loaded and inserted into the current
  /// syntax tree. In this case the parser should behave as if the node has
  /// successfully been created.
  bool loadCurrentSyntaxNodeFromCache();

  /// Parse an #endif.
  bool parseEndIfDirective(SourceLoc &Loc);

  /// Given that the current token is a string literal,
  /// - if it is not interpolated, returns the contents;
  /// - otherwise, diagnoses and returns None.
  ///
  /// \param Loc where to diagnose.
  /// \param DiagText name for the string literal in the diagnostic.
  Optional<StringRef>
  getStringLiteralIfNotInterpolated(SourceLoc Loc, StringRef DiagText);

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
  
  /// Add a fix-it to remove the space in consecutive identifiers.
  /// Add a camel-cased option if it is different than the first option.
  void diagnoseConsecutiveIDs(StringRef First, SourceLoc FirstLoc,
                              StringRef DeclKindName);

  bool startsWithSymbol(Token Tok, char symbol) {
    return (Tok.isAnyOperator() || Tok.isPunctuation()) &&
           Tok.getText()[0] == symbol;
  }
  /// \brief Check whether the current token starts with '<'.
  bool startsWithLess(Token Tok) { return startsWithSymbol(Tok, '<'); }

  /// \brief Check whether the current token starts with '>'.
  bool startsWithGreater(Token Tok) { return startsWithSymbol(Tok, '>'); }

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
  SourceLoc
  consumeStartingCharacterOfCurrentToken(tok Kind = tok::oper_binary_unspaced,
                                         size_t Len = 1);

  swift::ScopeInfo &getScopeInfo() { return State->getScopeInfo(); }

  /// \brief Add the given Decl to the current scope.
  void addToScope(ValueDecl *D) {
    getScopeInfo().addToScope(D, *this);
  }

  ValueDecl *lookupInScope(DeclName Name) {
    if (Context.LangOpts.EnableASTScopeLookup)
      return nullptr;

    return getScopeInfo().lookupValueName(Name);
  }

  //===--------------------------------------------------------------------===//
  // Primitive Parsing

  /// \brief Consume an identifier (but not an operator) if present and return
  /// its name in \p Result.  Otherwise, emit an error.
  ///
  /// \returns false on success, true on error.
  bool parseIdentifier(Identifier &Result, SourceLoc &Loc, const Diagnostic &D);
  
  /// Consume an identifier with a specific expected name.  This is useful for
  /// contextually sensitive keywords that must always be present.
  bool parseSpecificIdentifier(StringRef expected, SourceLoc &Loc,
                               const Diagnostic &D);

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
  
  template<typename ...DiagArgTypes, typename ...ArgTypes>
  bool parseSpecificIdentifier(StringRef expected,
                               Diag<DiagArgTypes...> ID, ArgTypes... Args) {
    SourceLoc L;
    return parseSpecificIdentifier(expected, L, Diagnostic(ID, Args...));
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

  /// \brief Parse a comma separated list of some elements.
  ParserStatus parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                         bool AllowSepAfterLast, Diag<> ErrorDiag,
                         syntax::SyntaxKind Kind,
                         llvm::function_ref<ParserStatus()> callback);

  void consumeTopLevelDecl(ParserPosition BeginParserPosition,
                           TopLevelCodeDecl *TLCD);

  ParserStatus parseBraceItems(SmallVectorImpl<ASTNode> &Decls,
                               BraceItemListKind Kind =
                                   BraceItemListKind::Brace,
                               BraceItemListKind ConditionalBlockKind =
                                   BraceItemListKind::Brace);
  ParserResult<BraceStmt> parseBraceItemList(Diag<> ID);
  
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
    PD_DisallowInit         = 1 << 3,
    PD_AllowDestructor      = 1 << 4,
    PD_AllowEnumElement     = 1 << 5,
    PD_InProtocol           = 1 << 6,
    PD_InClass              = 1 << 7,
    PD_InExtension          = 1 << 8,
    PD_InStruct             = 1 << 9,
    PD_InEnum               = 1 << 10,
  };

  /// Options that control the parsing of declarations.
  using ParseDeclOptions = OptionSet<ParseDeclFlags>;

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

  ParserResult<Decl> parseDecl(ParseDeclOptions Flags,
                               llvm::function_ref<void(Decl*)> Handler);

  void parseDeclDelayed();

  void parseDeclListDelayed(IterableDeclContext *IDC);

  ParserResult<TypeDecl> parseDeclTypeAlias(ParseDeclOptions Flags,
                                            DeclAttributes &Attributes);

  ParserResult<TypeDecl> parseDeclAssociatedType(ParseDeclOptions Flags,
                                                 DeclAttributes &Attributes);
  
  /// Parse a #if ... #endif directive.
  /// Delegate callback function to parse elements in the blocks.
  ParserResult<IfConfigDecl> parseIfConfig(
    llvm::function_ref<void(SmallVectorImpl<ASTNode> &, bool)> parseElements);

  /// Parse a #error or #warning diagnostic.
  ParserResult<PoundDiagnosticDecl> parseDeclPoundDiagnostic();

  /// Parse a #line/#sourceLocation directive.
  /// 'isLine = true' indicates parsing #line instead of #sourcelocation
  ParserStatus parseLineDirective(bool isLine = false);

  void setLocalDiscriminator(ValueDecl *D);
  void setLocalDiscriminatorToParamList(ParameterList *PL);

  /// Parse the optional attributes before a declaration.
  bool parseDeclAttributeList(DeclAttributes &Attributes,
                              bool &FoundCodeCompletionToken);

  /// Parse the optional modifiers before a declaration.
  bool parseDeclModifierList(DeclAttributes &Attributes, SourceLoc &StaticLoc,
                             StaticSpellingKind &StaticSpelling);

  /// Parse an availability attribute of the form
  /// @available(*, introduced: 1.0, deprecated: 3.1).
  /// \return \p nullptr if the platform name is invalid
  ParserResult<AvailableAttr>
  parseExtendedAvailabilitySpecList(SourceLoc AtLoc, SourceLoc AttrLoc,
                                    StringRef AttrName);

  /// Parse the Objective-C selector inside @objc
  void parseObjCSelector(SmallVector<Identifier, 4> &Names,
                         SmallVector<SourceLoc, 4> &NameLocs,
                         bool &IsNullarySelector);

  /// Parse the @_specialize attribute.
  /// \p closingBrace is the expected closing brace, which can be either ) or ]
  /// \p Attr is where to store the parsed attribute
  bool parseSpecializeAttribute(swift::tok ClosingBrace, SourceLoc AtLoc,
                                SourceLoc Loc, SpecializeAttr *&Attr);

  /// Parse the arguments inside the @_specialize attribute
  bool parseSpecializeAttributeArguments(
      swift::tok ClosingBrace, bool &DiscardAttribute, Optional<bool> &Exported,
      Optional<SpecializeAttr::SpecializationKind> &Kind,
      TrailingWhereClause *&TrailingWhereClause);

  /// Parse the @_implements attribute.
  /// \p Attr is where to store the parsed attribute
  ParserResult<ImplementsAttr> parseImplementsAttribute(SourceLoc AtLoc,
                                                        SourceLoc Loc);

  /// Parse a specific attribute.
  bool parseDeclAttribute(DeclAttributes &Attributes, SourceLoc AtLoc);

  bool parseNewDeclAttribute(DeclAttributes &Attributes, SourceLoc AtLoc,
                             DeclAttrKind DK);

  /// Parse a version tuple of the form x[.y[.z]]. Returns true if there was
  /// an error parsing.
  bool parseVersionTuple(llvm::VersionTuple &Version, SourceRange &Range,
                         const Diagnostic &D);

  bool parseTypeAttributeList(VarDecl::Specifier &Specifier,
                              SourceLoc &SpecifierLoc,
                              TypeAttributes &Attributes) {
    if (Tok.isAny(tok::at_sign, tok::kw_inout) ||
        (Tok.is(tok::identifier) &&
         (Tok.getRawText().equals("__shared") ||
          Tok.getRawText().equals("__owned"))))
      return parseTypeAttributeListPresent(Specifier, SpecifierLoc, Attributes);
    return false;
  }
  bool parseTypeAttributeListPresent(VarDecl::Specifier &Specifier,
                                     SourceLoc &SpecifierLoc,
                                     TypeAttributes &Attributes);
  bool parseTypeAttribute(TypeAttributes &Attributes,
                          bool justChecking = false);
  
  
  ParserResult<ImportDecl> parseDeclImport(ParseDeclOptions Flags,
                                           DeclAttributes &Attributes);
  ParserStatus parseInheritance(SmallVectorImpl<TypeLoc> &Inherited,
                                bool allowClassRequirement,
                                bool allowAnyObject);
  ParserStatus parseDeclItem(bool &PreviousHadSemi,
                             Parser::ParseDeclOptions Options,
                             llvm::function_ref<void(Decl*)> handler);
  bool parseDeclList(SourceLoc LBLoc, SourceLoc &RBLoc,
                     Diag<> ErrorDiag, ParseDeclOptions Options,
                     llvm::function_ref<void(Decl*)> handler);
  ParserResult<ExtensionDecl> parseDeclExtension(ParseDeclOptions Flags,
                                                 DeclAttributes &Attributes);
  ParserResult<EnumDecl> parseDeclEnum(ParseDeclOptions Flags,
                                       DeclAttributes &Attributes);
  ParserResult<EnumCaseDecl>
  parseDeclEnumCase(ParseDeclOptions Flags, DeclAttributes &Attributes,
                    SmallVectorImpl<Decl *> &decls);
  ParserResult<StructDecl>
  parseDeclStruct(ParseDeclOptions Flags, DeclAttributes &Attributes);
  ParserResult<ClassDecl>
  parseDeclClass(ParseDeclOptions Flags, DeclAttributes &Attributes);
  ParserResult<PatternBindingDecl>
  parseDeclVar(ParseDeclOptions Flags, DeclAttributes &Attributes,
               SmallVectorImpl<Decl *> &Decls,
               SourceLoc StaticLoc,
               StaticSpellingKind StaticSpelling,
               SourceLoc TryLoc);

  void consumeGetSetBody(AbstractFunctionDecl *AFD, SourceLoc LBLoc);

  struct ParsedAccessors;
  ParserStatus parseGetSet(ParseDeclOptions Flags,
                           GenericParamList *GenericParams,
                           ParameterList *Indices,
                           TypeLoc ElementTy,
                           ParsedAccessors &accessors,
                           AbstractStorageDecl *storage,
                           SourceLoc StaticLoc);
  void recordAccessors(AbstractStorageDecl *storage, ParseDeclOptions flags,
                       TypeLoc elementTy, const DeclAttributes &attrs,
                       SourceLoc staticLoc, ParsedAccessors &accessors);
  ParserResult<VarDecl> parseDeclVarGetSet(Pattern *pattern,
                                           ParseDeclOptions Flags,
                                           SourceLoc StaticLoc,
                                           SourceLoc VarLoc,
                                           bool hasInitializer,
                                           const DeclAttributes &Attributes,
                                           SmallVectorImpl<Decl *> &Decls);
  
  void consumeAbstractFunctionBody(AbstractFunctionDecl *AFD,
                                   const DeclAttributes &Attrs);
  ParserResult<FuncDecl> parseDeclFunc(SourceLoc StaticLoc,
                                       StaticSpellingKind StaticSpelling,
                                       ParseDeclOptions Flags,
                                       DeclAttributes &Attributes);
  void parseAbstractFunctionBody(AbstractFunctionDecl *AFD);
  bool parseAbstractFunctionBodyDelayed(AbstractFunctionDecl *AFD);
  ParserResult<ProtocolDecl> parseDeclProtocol(ParseDeclOptions Flags,
                                               DeclAttributes &Attributes);

  ParserResult<SubscriptDecl>
  parseDeclSubscript(ParseDeclOptions Flags, DeclAttributes &Attributes,
                     SmallVectorImpl<Decl *> &Decls);

  ParserResult<ConstructorDecl>
  parseDeclInit(ParseDeclOptions Flags, DeclAttributes &Attributes);
  ParserResult<DestructorDecl>
  parseDeclDeinit(ParseDeclOptions Flags, DeclAttributes &Attributes);

  void addPatternVariablesToScope(ArrayRef<Pattern *> Patterns);
  void addParametersToScope(ParameterList *PL);

  ParserResult<OperatorDecl> parseDeclOperator(ParseDeclOptions Flags,
                                               DeclAttributes &Attributes);
  ParserResult<OperatorDecl> parseDeclOperatorImpl(SourceLoc OperatorLoc,
                                                   Identifier Name,
                                                   SourceLoc NameLoc,
                                                   DeclAttributes &Attrs);

  ParserResult<PrecedenceGroupDecl>
  parseDeclPrecedenceGroup(ParseDeclOptions flags, DeclAttributes &attributes);

  //===--------------------------------------------------------------------===//
  // Type Parsing
  
  ParserResult<TypeRepr> parseType();
  ParserResult<TypeRepr> parseType(Diag<> MessageID,
                                   bool HandleCodeCompletion = true,
                                   bool IsSILFuncDecl = false);

  ParserResult<TypeRepr>
    parseTypeSimpleOrComposition(Diag<> MessageID,
                                 bool HandleCodeCompletion = true);

  ParserResult<TypeRepr> parseTypeSimple(Diag<> MessageID,
                                         bool HandleCodeCompletion = true);

  /// \brief Parse layout constraint.
  LayoutConstraint parseLayoutConstraint(Identifier LayoutConstraintID);

  bool parseGenericArguments(SmallVectorImpl<TypeRepr*> &Args,
                             SourceLoc &LAngleLoc,
                             SourceLoc &RAngleLoc);

  ParserResult<TypeRepr> parseTypeIdentifier();
  ParserResult<TypeRepr> parseOldStyleProtocolComposition();
  ParserResult<CompositionTypeRepr> parseAnyType();
  ParserResult<TypeRepr> parseSILBoxType(GenericParamList *generics,
                                         const TypeAttributes &attrs,
                                         Optional<Scope> &GenericsScope);
  
  ParserResult<TupleTypeRepr> parseTypeTupleBody();
  ParserResult<TypeRepr> parseTypeArray(TypeRepr *Base);

  /// Parse a collection type.
  ///   type-simple:
  ///     '[' type ']'
  ///     '[' type ':' type ']'
  SyntaxParserResult<syntax::TypeSyntax, TypeRepr> parseTypeCollection();

  SyntaxParserResult<syntax::TypeSyntax, OptionalTypeRepr>
  parseTypeOptional(TypeRepr *Base);

  SyntaxParserResult<syntax::TypeSyntax, ImplicitlyUnwrappedOptionalTypeRepr>
  parseTypeImplicitlyUnwrappedOptional(TypeRepr *Base);

  bool isOptionalToken(const Token &T) const;
  SourceLoc consumeOptionalToken();
  
  bool isImplicitlyUnwrappedOptionalToken(const Token &T) const;
  SourceLoc consumeImplicitlyUnwrappedOptionalToken();

  TypeRepr *applyAttributeToType(TypeRepr *Ty, const TypeAttributes &Attr,
                                 VarDecl::Specifier Specifier,
                                 SourceLoc SpecifierLoc);

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
    void setFunctionContext(DeclContext *DC, ParameterList *paramList);
    
    DefaultArgumentInfo() {
      NextIndex = 0;
      HasDefaultArgument = false;
    }
  };

  /// Describes a parsed parameter.
  struct ParsedParameter {
    /// Any declaration attributes attached to the parameter.
    DeclAttributes Attrs;

    /// The location of the 'inout' keyword, if present.
    SourceLoc SpecifierLoc;
    
    /// The parsed specifier kind, if present.
    VarDecl::Specifier SpecifierKind = VarDecl::Specifier::Default;

    /// The location of the first name.
    ///
    /// \c FirstName is the name.
    SourceLoc FirstNameLoc;

    /// The location of the second name, if present.
    ///
    /// \p SecondName is the name.
    SourceLoc SecondNameLoc;

    /// The location of the '...', if present.
    SourceLoc EllipsisLoc;

    /// The first name.
    Identifier FirstName;

    /// The second name, the presence of which is indicated by \c SecondNameLoc.
    Identifier SecondName;

    /// The type following the ':'.
    TypeRepr *Type = nullptr;

    /// The default argument for this parameter.
    Expr *DefaultArg = nullptr;
    
    /// True if we emitted a parse error about this parameter.
    bool isInvalid = false;
  };

  /// Describes the context in which the given parameter is being parsed.
  enum class ParameterContextKind {
    /// An operator.
    Operator,
    /// A function.
    Function,
    /// An initializer.
    Initializer,
    /// A closure.
    Closure,
    /// A subscript.
    Subscript,
    /// A curried argument clause.
    Curried,
    /// An enum element.
    EnumElement,
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

  ParserResult<ParameterList> parseSingleParameterClause(
                          ParameterContextKind paramContext,
                          SmallVectorImpl<Identifier> *namePieces = nullptr,
                          DefaultArgumentInfo *defaultArgs = nullptr);

  ParserStatus parseFunctionArguments(SmallVectorImpl<Identifier> &NamePieces,
                                      ParameterList *&BodyParams,
                                      ParameterContextKind paramContext,
                                      DefaultArgumentInfo &defaultArgs);
  ParserStatus parseFunctionSignature(Identifier functionName,
                                      DeclName &fullName,
                                      ParameterList *&bodyParams,
                                      DefaultArgumentInfo &defaultArgs,
                                      SourceLoc &throws,
                                      bool &rethrows,
                                      TypeRepr *&retType);

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
  
  ParserResult<Pattern>
  parseOptionalPatternTypeAnnotation(ParserResult<Pattern> P,
                                     bool isOptional);
  ParserResult<Pattern> parseMatchingPattern(bool isExprBasic);
  ParserResult<Pattern> parseMatchingPatternAsLetOrVar(bool isLet,
                                                       SourceLoc VarLoc,
                                                       bool isExprBasic);
  

  Pattern *createBindingFromPattern(SourceLoc loc, Identifier name,
                                    VarDecl::Specifier specifier);
  

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
  bool canParseTypeIdentifierOrTypeComposition();
  bool canParseOldStyleProtocolComposition();
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
  ParserResult<Expr> parseExprImpl(Diag<> ID, bool isExprBasic);
  ParserResult<Expr> parseExprIs();
  ParserResult<Expr> parseExprAs();
  ParserResult<Expr> parseExprArrow();
  ParserResult<Expr> parseExprSequence(Diag<> ID,
                                       bool isExprBasic,
                                       bool isForConditionalDirective = false);
  ParserResult<Expr> parseExprSequenceElement(Diag<> ID,
                                              bool isExprBasic);
  ParserResult<Expr> parseExprPostfixSuffix(ParserResult<Expr> inner,
                                            bool isExprBasic,
                                            bool periodHasKeyPathBehavior,
                                            bool &hasBindOptional);
  ParserResult<Expr> parseExprPostfix(Diag<> ID, bool isExprBasic);
  ParserResult<Expr> parseExprPrimary(Diag<> ID, bool isExprBasic);
  ParserResult<Expr> parseExprUnary(Diag<> ID, bool isExprBasic);
  ParserResult<Expr> parseExprKeyPathObjC();
  ParserResult<Expr> parseExprKeyPath();
  ParserResult<Expr> parseExprSelector();
  ParserResult<Expr> parseExprSuper(bool isExprBasic);
  ParserResult<Expr> parseExprConfiguration();
  ParserResult<Expr> parseExprStringLiteral();
  ParserResult<Expr> parseExprTypeOf();

  ParserStatus parseStringSegments(SmallVectorImpl<Lexer::StringSegment> &Segments,
                                   SmallVectorImpl<Expr*> &Exprs,
                                   Token EntireTok);

  /// Parse an argument label `identifier ':'`, if it exists.
  ///
  /// \param name The parsed name of the label (empty if it doesn't exist, or is
  /// _)
  /// \param loc The location of the label (empty if it doesn't exist)
  void parseOptionalArgumentLabel(Identifier &name, SourceLoc &loc);

  /// Parse an unqualified-decl-name.
  ///
  ///   unqualified-decl-name:
  ///     identifier
  ///     identifier '(' ((identifier | '_') ':') + ')'
  ///
  /// \param afterDot Whether this identifier is coming after a period, which
  /// enables '.init' and '.default' like expressions.
  /// \param loc Will be populated with the location of the name.
  /// \param diag The diagnostic to emit if this is not a name.
  /// \param allowOperators Whether to allow operator basenames too.
  /// \param allowZeroArgCompoundNames Whether to allow empty argument lists.
  DeclName parseUnqualifiedDeclName(bool afterDot, DeclNameLoc &loc,
                                    const Diagnostic &diag,
                                    bool allowOperators=false,
                                    bool allowZeroArgCompoundNames=false);

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
                                      ParameterList *&params,
                                      SourceLoc &throwsLoc,
                                      SourceLoc &arrowLoc,
                                      TypeRepr *&explicitResultType,
                                      SourceLoc &inLoc);

  Expr *parseExprAnonClosureArg();
  ParserResult<Expr> parseExprList(tok LeftTok, tok RightTok,
                                   syntax::SyntaxKind Kind);

  /// Parse an expression list, keeping all of the pieces separated.
  ParserStatus parseExprList(tok leftTok, tok rightTok,
                             bool isPostfix,
                             bool isExprBasic,
                             SourceLoc &leftLoc,
                             SmallVectorImpl<Expr *> &exprs,
                             SmallVectorImpl<Identifier> &exprLabels,
                             SmallVectorImpl<SourceLoc> &exprLabelLocs,
                             SourceLoc &rightLoc,
                             Expr *&trailingClosure,
                             syntax::SyntaxKind Kind);

  ParserResult<Expr> parseTrailingClosure(SourceRange calleeRange);

  // NOTE: used only for legacy support for old object literal syntax.
  // Will be removed in the future.
  bool isCollectionLiteralStartingWithLSquareLit();

  /// Parse an object literal.
  ///
  /// \param LK The literal kind as determined by the first token.
  ParserResult<Expr> parseExprObjectLiteral(ObjectLiteralExpr::LiteralKind LK,
                                            bool isExprBasic);
  ParserResult<Expr> parseExprCallSuffix(ParserResult<Expr> fn,
                                         bool isExprBasic);
  ParserResult<Expr> parseExprCollection();
  ParserResult<Expr> parseExprArray(SourceLoc LSquareLoc);
  ParserResult<Expr> parseExprDictionary(SourceLoc LSquareLoc);
  ParserResult<Expr> parseExprPoundAssert();
  ParserResult<Expr> parseExprPoundUnknown(SourceLoc LSquareLoc);
  ParserResult<Expr>
  parseExprPoundCodeCompletion(Optional<StmtKind> ParentKind);

  UnresolvedDeclRefExpr *parseExprOperator();

  //===--------------------------------------------------------------------===//
  // Statement Parsing

  bool isStartOfStmt();
  bool isTerminatorForBraceItemListKind(BraceItemListKind Kind,
                                        ArrayRef<ASTNode> ParsedDecls);
  ParserResult<Stmt> parseStmt();
  ParserStatus parseExprOrStmt(ASTNode &Result);
  ParserResult<Stmt> parseStmtBreak();
  ParserResult<Stmt> parseStmtContinue();
  ParserResult<Stmt> parseStmtReturn(SourceLoc tryLoc);
  ParserResult<Stmt> parseStmtYield(SourceLoc tryLoc);
  ParserResult<Stmt> parseStmtThrow(SourceLoc tryLoc);
  ParserResult<Stmt> parseStmtDefer();
  ParserStatus
  parseStmtConditionElement(SmallVectorImpl<StmtConditionElement> &result,
                            Diag<> DefaultID, StmtKind ParentKind,
                            StringRef &BindingKindStr);
  ParserStatus parseStmtCondition(StmtCondition &Result, Diag<> ID,
                                  StmtKind ParentKind);
  ParserResult<PoundAvailableInfo> parseStmtConditionPoundAvailable();
  ParserResult<Stmt> parseStmtIf(LabeledStmtInfo LabelInfo,
                                 bool IfWasImplicitlyInserted = false);
  ParserResult<Stmt> parseStmtGuard();
  ParserResult<Stmt> parseStmtWhile(LabeledStmtInfo LabelInfo);
  ParserResult<Stmt> parseStmtRepeat(LabeledStmtInfo LabelInfo);
  ParserResult<Stmt> parseStmtDo(LabeledStmtInfo LabelInfo);
  ParserResult<CatchStmt> parseStmtCatch();
  ParserResult<Stmt> parseStmtForEach(LabeledStmtInfo LabelInfo);
  ParserResult<Stmt> parseStmtSwitch(LabeledStmtInfo LabelInfo);
  ParserStatus parseStmtCases(SmallVectorImpl<ASTNode> &cases, bool IsActive);
  ParserResult<CaseStmt> parseStmtCase(bool IsActive);
  ParserResult<Stmt> parseStmtPoundAssert();

  //===--------------------------------------------------------------------===//
  // Generics Parsing

  ParserResult<GenericParamList> parseGenericParameters();
  ParserResult<GenericParamList> parseGenericParameters(SourceLoc LAngleLoc);
  ParserStatus parseGenericParametersBeforeWhere(SourceLoc LAngleLoc,
                        SmallVectorImpl<GenericTypeParamDecl *> &GenericParams);
  ParserResult<GenericParamList> maybeParseGenericParams();
  void
  diagnoseWhereClauseInGenericParamList(const GenericParamList *GenericParams);

  enum class WhereClauseKind : unsigned {
    Declaration,
    Protocol,
    AssociatedType
  };
  ParserStatus
  parseFreestandingGenericWhereClause(GenericParamList *&GPList,
                             WhereClauseKind kind=WhereClauseKind::Declaration);

  ParserStatus parseGenericWhereClause(
      SourceLoc &WhereLoc, SmallVectorImpl<RequirementRepr> &Requirements,
      bool &FirstTypeInComplete, bool AllowLayoutConstraints = false);

  ParserStatus
  parseProtocolOrAssociatedTypeWhereClause(TrailingWhereClause *&trailingWhere,
                                           bool isProtocol);

  //===--------------------------------------------------------------------===//
  // Availability Specification Parsing

  /// Parse a comma-separated list of availability specifications.
  ParserStatus
  parseAvailabilitySpecList(SmallVectorImpl<AvailabilitySpec *> &Specs);

  ParserResult<AvailabilitySpec> parseAvailabilitySpec();
  ParserResult<PlatformVersionConstraintAvailabilitySpec>
  parsePlatformVersionConstraintSpec();
  ParserResult<LanguageVersionConstraintAvailabilitySpec>
  parseLanguageVersionConstraintSpec();

  bool canDelayMemberDeclParsing();
};

/// Describes a parsed declaration name.
struct ParsedDeclName {
  /// The name of the context of which the corresponding entity should
  /// become a member.
  StringRef ContextName;

  /// The base name of the declaration.
  StringRef BaseName;

  /// The argument labels for a function declaration.
  SmallVector<StringRef, 4> ArgumentLabels;

  /// Whether this is a function name (vs. a value name).
  bool IsFunctionName = false;

  /// Whether this is a getter for the named property.
  bool IsGetter = false;

  /// Whether this is a setter for the named property.
  bool IsSetter = false;

  /// For a declaration name that makes the declaration into an
  /// instance member, the index of the "Self" parameter.
  Optional<unsigned> SelfIndex;

  /// Determine whether this is a valid name.
  explicit operator bool() const { return !BaseName.empty(); }

  /// Whether this declaration name turns the declaration into a
  /// member of some named context.
  bool isMember() const { return !ContextName.empty(); }

  /// Whether the result is translated into an instance member.
  bool isInstanceMember() const {
    return isMember() && static_cast<bool>(SelfIndex);
  }

  /// Whether the result is translated into a static/class member.
  bool isClassMember() const {
    return isMember() && !static_cast<bool>(SelfIndex);
  }

  /// Whether this is a property accessor.
  bool isPropertyAccessor() const { return IsGetter || IsSetter; }

  /// Whether this is an operator.
  bool isOperator() const {
    return Lexer::isOperator(BaseName);
  }

  /// Form a declaration name from this parsed declaration name.
  DeclName formDeclName(ASTContext &ctx) const;
};

/// Parse a stringified Swift declaration name,
/// e.g. "Foo.translateBy(self:x:y:)".
ParsedDeclName parseDeclName(StringRef name) LLVM_READONLY;

/// Form a Swift declaration name from its constituent parts.
DeclName formDeclName(ASTContext &ctx,
                      StringRef baseName,
                      ArrayRef<StringRef> argumentLabels,
                      bool isFunctionName,
                      bool isInitializer);

/// Parse a stringified Swift declaration name, e.g. "init(frame:)".
DeclName parseDeclName(ASTContext &ctx, StringRef name);

/// Whether a given token can be the start of a decl.
bool isKeywordPossibleDeclStart(const Token &Tok);

/// \brief Lex and return a vector of `TokenSyntax` tokens, which include
/// leading and trailing trivia.
std::vector<std::pair<RC<syntax::RawSyntax>,
                                 syntax::AbsolutePosition>>
tokenizeWithTrivia(const LangOptions &LangOpts,
                   const SourceManager &SM,
                   unsigned BufferID,
                   unsigned Offset = 0,
                   unsigned EndOffset = 0,
                   DiagnosticEngine *Diags = nullptr);
} // end namespace swift

#endif
