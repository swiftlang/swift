//===--- ParseStmt.cpp - Swift Language Parser for Statements -------------===//
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
// Statement Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Version.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

/// isStartOfStmt - Return true if the current token starts a statement.
///
bool Parser::isStartOfStmt() {
  switch (Tok.getKind()) {
  default: return false;
  case tok::kw_return:
  case tok::kw_throw:
  case tok::kw_defer:
  case tok::kw_if:
  case tok::kw_guard:
  case tok::kw_while:
  case tok::kw_do:
  case tok::kw_repeat:
  case tok::kw_for:
  case tok::kw_break:
  case tok::kw_continue:
  case tok::kw_fallthrough:
  case tok::kw_switch:
  case tok::kw_case:
  case tok::kw_default:
  case tok::pound_if:
  case tok::pound_sourceLocation:
    return true;

  case tok::pound_line:
    // #line at the start of a line is a directive, when within, it is an expr.
    return Tok.isAtStartOfLine();

  case tok::kw_try: {
    // "try" cannot actually start any statements, but we parse it there for
    // better recovery.
    Parser::BacktrackingScope backtrack(*this);
    consumeToken(tok::kw_try);
    return isStartOfStmt();
  }
      
  case tok::identifier: {
    // "identifier ':' for/while/do/switch" is a label on a loop/switch.
    if (!peekToken().is(tok::colon)) return false;

    // To disambiguate other cases of "identifier :", which might be part of a
    // question colon expression or something else, we look ahead to the second
    // token.
    Parser::BacktrackingScope backtrack(*this);
    consumeToken(tok::identifier);
    consumeToken(tok::colon);
    // For better recovery, we just accept a label on any statement.  We reject
    // putting a label on something inappropriate in parseStmt().
    return isStartOfStmt();
  }
  }
}

ParserStatus Parser::parseExprOrStmt(ASTNode &Result) {
  if (Tok.is(tok::semi)) {
    diagnose(Tok, diag::illegal_semi_stmt)
      .fixItRemove(SourceRange(Tok.getLoc()));
    consumeToken();
    return makeParserError();
  }
  
  if (isStartOfStmt()) {
    ParserResult<Stmt> Res = parseStmt();
    if (Res.isNonNull())
      Result = Res.get();
    return Res;
  }

  // Note that we're parsing a statement.
  StructureMarkerRAII ParsingStmt(*this, Tok.getLoc(),
                                  StructureMarkerKind::Statement);

  if (CodeCompletion)
    CodeCompletion->setExprBeginning(getParserPosition());

  if (Tok.is(tok::code_complete)) {
    if (CodeCompletion)
      CodeCompletion->completeStmtOrExpr();
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionStatus();
  }

  ParserResult<Expr> ResultExpr = parseExpr(diag::expected_expr);
  if (ResultExpr.isNonNull()) {
    Result = ResultExpr.get();
  } else if (!ResultExpr.hasCodeCompletion()) {
    // If we've consumed any tokens at all, build an error expression
    // covering the consumed range.
    SourceLoc startLoc = StructureMarkers.back().Loc;
    if (startLoc != Tok.getLoc()) {
      Result = new (Context) ErrorExpr(SourceRange(startLoc, PreviousLoc));
    }
  }

  if (ResultExpr.hasCodeCompletion() && CodeCompletion) {
    CodeCompletion->completeExpr();
  }

  return ResultExpr;
}

static bool isTerminatorForBraceItemListKind(const Token &Tok,
                                             BraceItemListKind Kind,
                                             ArrayRef<ASTNode> ParsedDecls) {
  switch (Kind) {
  case BraceItemListKind::Brace:
    return false;
  case BraceItemListKind::Case:
    return Tok.is(tok::kw_case) || Tok.is(tok::kw_default);
  case BraceItemListKind::TopLevelCode:
    // When parsing the top level executable code for a module, if we parsed
    // some executable code, then we're done.  We want to process (name bind,
    // type check, etc) decls one at a time to make sure that there are not
    // forward type references, etc.  There is an outer loop around the parser
    // that will reinvoke the parser at the top level on each statement until
    // EOF.  In contrast, it is ok to have forward references between classes,
    // functions, etc.
    for (auto I : ParsedDecls) {
      if (isa<TopLevelCodeDecl>(I.get<Decl*>()))
        // Only bail out if the next token is at the start of a line.  If we
        // don't, then we may accidentally allow things like "a = 1 b = 4".
        // FIXME: This is really dubious.  This will reject some things, but
        // allow other things we don't want.
        if (Tok.isAtStartOfLine())
          return true;
    }
    return false;
  case BraceItemListKind::TopLevelLibrary:
    return false;
  case BraceItemListKind::ActiveConditionalBlock:
  case BraceItemListKind::InactiveConditionalBlock:
    return Tok.isNot(tok::pound_else) && Tok.isNot(tok::pound_endif) &&
           Tok.isNot(tok::pound_elseif);
  }

  llvm_unreachable("Unhandled BraceItemListKind in switch.");
}

void Parser::consumeTopLevelDecl(ParserPosition BeginParserPosition,
                                 TopLevelCodeDecl *TLCD) {
  backtrackToPosition(BeginParserPosition);
  SourceLoc BeginLoc = Tok.getLoc();
  // Consume tokens up to code completion token.
  while (Tok.isNot(tok::code_complete, tok::eof)) {
    consumeToken();
  }
  // Consume the code completion token, if there is one.
  consumeIf(tok::code_complete);
  // Also perform the same recovery as the main parser to capture tokens from
  // this decl that are past the code completion token.
  skipUntilDeclStmtRBrace(tok::l_brace);
  SourceLoc EndLoc = Tok.getLoc();
  State->delayTopLevel(TLCD, { BeginLoc, EndLoc },
                       BeginParserPosition.PreviousLoc);

  // Skip the rest of the file to prevent the parser from constructing the AST
  // for it.  Forward references are not allowed at the top level.
  skipUntil(tok::eof);
}

///   brace-item:
///     decl
///     expr
///     stmt
///   stmt:
///     ';'
///     stmt-assign
///     stmt-if
///     stmt-guard
///     stmt-for-c-style
///     stmt-for-each
///     stmt-switch
///     stmt-control-transfer
///  stmt-control-transfer:
///     stmt-return
///     stmt-break
///     stmt-continue
///     stmt-fallthrough
///   stmt-assign:
///     expr '=' expr
ParserStatus Parser::parseBraceItems(SmallVectorImpl<ASTNode> &Entries,
                                     BraceItemListKind Kind,
                                     BraceItemListKind ConditionalBlockKind) {
  
  bool IsTopLevel = (Kind == BraceItemListKind::TopLevelCode) ||
                    (Kind == BraceItemListKind::TopLevelLibrary);
  bool isActiveConditionalBlock =
    ConditionalBlockKind == BraceItemListKind::ActiveConditionalBlock;
  bool isConditionalBlock = isActiveConditionalBlock ||
    ConditionalBlockKind == BraceItemListKind::InactiveConditionalBlock;

  // If we're not parsing an active #if block, form a new lexical scope.
  Optional<Scope> initScope;
  if (!isActiveConditionalBlock) {
    auto scopeKind =  IsTopLevel ? ScopeKind::TopLevel : ScopeKind::Brace;
    initScope.emplace(this, scopeKind,
                      ConditionalBlockKind ==
                        BraceItemListKind::InactiveConditionalBlock);
  }

  ParserStatus BraceItemsStatus;
  SmallVector<Decl*, 8> TmpDecls;

  bool PreviousHadSemi = true;
  while ((Kind == BraceItemListKind::TopLevelLibrary ||
          Tok.isNot(tok::r_brace)) &&
         Tok.isNot(tok::pound_endif) &&
         Tok.isNot(tok::pound_elseif) &&
         Tok.isNot(tok::pound_else) &&
         Tok.isNot(tok::eof) &&
         Tok.isNot(tok::kw_sil) &&
         Tok.isNot(tok::kw_sil_scope) &&
         Tok.isNot(tok::kw_sil_stage) &&
         Tok.isNot(tok::kw_sil_vtable) &&
         Tok.isNot(tok::kw_sil_global) &&
         Tok.isNot(tok::kw_sil_witness_table) &&
         Tok.isNot(tok::kw_sil_default_witness_table) &&
         (isConditionalBlock ||
          !isTerminatorForBraceItemListKind(Tok, Kind, Entries))) {
    if (Kind == BraceItemListKind::TopLevelLibrary &&
        skipExtraTopLevelRBraces())
      continue;

    // Eat invalid tokens instead of allowing them to produce downstream errors.
    if (consumeIf(tok::unknown))
      continue;
           
    bool NeedParseErrorRecovery = false;
    ASTNode Result;

    // If the previous statement didn't have a semicolon and this new
    // statement doesn't start a line, complain.
    if (!PreviousHadSemi && !Tok.isAtStartOfLine()) {
      SourceLoc EndOfPreviousLoc = getEndOfPreviousLoc();
      diagnose(EndOfPreviousLoc, diag::statement_same_line_without_semi)
        .fixItInsert(EndOfPreviousLoc, ";");
      // FIXME: Add semicolon to the AST?
    }

    ParserPosition BeginParserPosition;
    if (isCodeCompletionFirstPass())
      BeginParserPosition = getParserPosition();

    // Parse the decl, stmt, or expression.
    PreviousHadSemi = false;
    if (isStartOfDecl()
        && Tok.isNot(tok::pound_if, tok::pound_sourceLocation)) {
      ParserResult<Decl> DeclResult = 
          parseDecl(IsTopLevel ? PD_AllowTopLevel : PD_Default,
                    [&](Decl *D) {TmpDecls.push_back(D);});
      if (DeclResult.isParseError()) {
        NeedParseErrorRecovery = true;
        if (DeclResult.hasCodeCompletion() && IsTopLevel &&
            isCodeCompletionFirstPass()) {
          consumeDecl(BeginParserPosition, None, IsTopLevel);
          return DeclResult;
        }
      }
      Result = DeclResult.getPtrOrNull();

      for (Decl *D : TmpDecls)
        Entries.push_back(D);
      TmpDecls.clear();
    } else if (Tok.is(tok::pound_if)) {
      SourceLoc StartLoc = Tok.getLoc();
      
      // We'll want to parse the #if block, but not wrap it in a top-level
      // code declaration immediately.
      auto IfConfigResult = parseStmtIfConfig(Kind);
      
      if (IfConfigResult.isParseError()) {
        NeedParseErrorRecovery = true;
        continue;
      }
      
      Result = IfConfigResult.get();
      
      if (!Result) {
        NeedParseErrorRecovery = true;
        continue;
      }

      // Add the #if block itself as a TLCD if necessary
      if (Kind == BraceItemListKind::TopLevelCode) {
        auto *TLCD = new (Context) TopLevelCodeDecl(CurDeclContext);
        auto Brace = BraceStmt::create(Context, StartLoc,
                                       {Result}, PreviousLoc);
        TLCD->setBody(Brace);
        Entries.push_back(TLCD);
      } else {
        Entries.push_back(Result);
      }

      IfConfigStmt *ICS = cast<IfConfigStmt>(Result.get<Stmt*>());
      for (auto &Entry : ICS->getActiveClauseElements()) {
        Entries.push_back(Entry);
        if (Entry.is<Decl*>()) {
          Entry.get<Decl*>()->setEscapedFromIfConfig(true);
        }
      }
    } else if (Tok.is(tok::pound_line)) {
      ParserStatus Status = parseLineDirective(true);
      BraceItemsStatus |= Status;
      NeedParseErrorRecovery = Status.isError();
    } else if (Tok.is(tok::pound_sourceLocation)) {
      ParserStatus Status = parseLineDirective(false);
      BraceItemsStatus |= Status;
      NeedParseErrorRecovery = Status.isError();
    } else if (IsTopLevel) {
      // If this is a statement or expression at the top level of the module,
      // Parse it as a child of a TopLevelCodeDecl.
      auto *TLCD = new (Context) TopLevelCodeDecl(CurDeclContext);
      ContextChange CC(*this, TLCD, &State->getTopLevelContext());
      SourceLoc StartLoc = Tok.getLoc();

      // Expressions can't begin with a closure literal at statement position.
      // This prevents potential ambiguities with trailing closure syntax.
      if (Tok.is(tok::l_brace)) {
        diagnose(Tok, diag::statement_begins_with_closure);
      }

      ParserStatus Status = parseExprOrStmt(Result);
      if (Status.hasCodeCompletion() && isCodeCompletionFirstPass()) {
        consumeTopLevelDecl(BeginParserPosition, TLCD);
        auto Brace = BraceStmt::create(Context, StartLoc, {}, Tok.getLoc());
        TLCD->setBody(Brace);
        Entries.push_back(TLCD);
        return Status;
      }
      if (Status.isError())
        NeedParseErrorRecovery = true;
      else if (!allowTopLevelCode()) {
        diagnose(StartLoc,
                 Result.is<Stmt*>() ? diag::illegal_top_level_stmt
                                    : diag::illegal_top_level_expr);
      }

      if (!Result.isNull()) {
        // NOTE: this is a 'virtual' brace statement which does not have
        //       explicit '{' or '}', so the start and end locations should be
        //       the same as those of the result node
        auto Brace = BraceStmt::create(Context, Result.getStartLoc(),
                                       Result, Result.getEndLoc());
        TLCD->setBody(Brace);
        Entries.push_back(TLCD);
      }
    } else if (Tok.is(tok::kw_init) && isa<ConstructorDecl>(CurDeclContext)) {
      SourceLoc StartLoc = Tok.getLoc();
      auto CD = cast<ConstructorDecl>(CurDeclContext);
      // Hint at missing 'self.' or 'super.' then skip this statement.
      bool isSelf = !CD->isDesignatedInit() || !isa<ClassDecl>(CD->getParent());
      diagnose(StartLoc, diag::invalid_nested_init, isSelf)
        .fixItInsert(StartLoc, isSelf ? "self." : "super.");
      NeedParseErrorRecovery = true;
    } else {
      ParserStatus ExprOrStmtStatus = parseExprOrStmt(Result);
      BraceItemsStatus |= ExprOrStmtStatus;
      if (ExprOrStmtStatus.isError())
        NeedParseErrorRecovery = true;
      if (!Result.isNull())
        Entries.push_back(Result);
    }

    if (!NeedParseErrorRecovery && Tok.is(tok::semi)) {
      PreviousHadSemi = true;
      if (Expr *E = Result.dyn_cast<Expr*>())
        E->TrailingSemiLoc = consumeToken(tok::semi);
      else if (Stmt *S = Result.dyn_cast<Stmt*>())
        S->TrailingSemiLoc = consumeToken(tok::semi);
      else if (Decl *D = Result.dyn_cast<Decl*>())
        D->TrailingSemiLoc = consumeToken(tok::semi);
      else
        assert(!Result && "Unsupported AST node");
    }

    if (NeedParseErrorRecovery) {
      // If we had a parse error, skip to the start of the next stmt, decl or
      // '{'.
      //
      // It would be ideal to stop at the start of the next expression (e.g.
      // "X = 4"), but distinguishing the start of an expression from the middle
      // of one is "hard".
      skipUntilDeclStmtRBrace(tok::l_brace);

      // If we have to recover, pretend that we had a semicolon; it's less
      // noisy that way.
      PreviousHadSemi = true;
    }
  }

  return BraceItemsStatus;
}

void Parser::parseTopLevelCodeDeclDelayed() {
  auto DelayedState = State->takeDelayedDeclState();
  assert(DelayedState.get() && "should have delayed state");

  auto BeginParserPosition = getParserPosition(DelayedState->BodyPos);
  auto EndLexerState = L->getStateForEndOfTokenLoc(DelayedState->BodyEnd);

  // ParserPositionRAII needs a primed parser to restore to.
  if (Tok.is(tok::NUM_TOKENS))
    consumeToken();

  // Ensure that we restore the parser state at exit.
  ParserPositionRAII PPR(*this);

  // Create a lexer that cannot go past the end state.
  Lexer LocalLex(*L, BeginParserPosition.LS, EndLexerState);

  // Temporarily swap out the parser's current lexer with our new one.
  llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

  // Rewind to the beginning of the top-level code.
  restoreParserPosition(BeginParserPosition);

  // Re-enter the lexical scope.
  Scope S(this, DelayedState->takeScope());

  // Re-enter the top-level decl context.
  // FIXME: this can issue discriminators out-of-order?
  auto *TLCD = cast<TopLevelCodeDecl>(DelayedState->ParentContext);
  ContextChange CC(*this, TLCD, &State->getTopLevelContext());

  SourceLoc StartLoc = Tok.getLoc();
  ASTNode Result;

  // Expressions can't begin with a closure literal at statement position. This
  // prevents potential ambiguities with trailing closure syntax.
  if (Tok.is(tok::l_brace)) {
    diagnose(Tok, diag::statement_begins_with_closure);
  }

  parseExprOrStmt(Result);
  if (!Result.isNull()) {
    auto Brace = BraceStmt::create(Context, StartLoc, Result, Tok.getLoc());
    TLCD->setBody(Brace);
  }
}

/// Recover from a 'case' or 'default' outside of a 'switch' by consuming up to
/// the next ':'.
static ParserResult<Stmt> recoverFromInvalidCase(Parser &P) {
  assert(P.Tok.is(tok::kw_case) || P.Tok.is(tok::kw_default)
         && "not case or default?!");
  P.diagnose(P.Tok, diag::case_outside_of_switch, P.Tok.getText());
  P.skipUntil(tok::colon);
  // FIXME: Return an ErrorStmt?
  return nullptr;
}

ParserResult<Stmt> Parser::parseStmt() {

  // Note that we're parsing a statement.
  StructureMarkerRAII ParsingStmt(*this, Tok.getLoc(),
                                  StructureMarkerKind::Statement);
  
  LabeledStmtInfo LabelInfo;
  
  // If this is a label on a loop/switch statement, consume it and pass it into
  // parsing logic below.
  if (Tok.is(tok::identifier) && peekToken().is(tok::colon)) {
    LabelInfo.Loc = consumeIdentifier(&LabelInfo.Name);
    consumeToken(tok::colon);
  }

  SourceLoc tryLoc;
  (void)consumeIf(tok::kw_try, tryLoc);
  
  switch (Tok.getKind()) {
  case tok::pound_line:
  case tok::pound_sourceLocation:
  case tok::pound_if:
    assert((LabelInfo || tryLoc.isValid()) &&
           "unlabeled directives should be handled earlier");
    // Bailout, and let parseBraceItems() parse them.
    LLVM_FALLTHROUGH;
  default:
    diagnose(Tok, tryLoc.isValid() ? diag::expected_expr : diag::expected_stmt);
    return nullptr;
  case tok::kw_return:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtReturn(tryLoc);
  case tok::kw_throw:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtThrow(tryLoc);
  case tok::kw_defer:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtDefer();
  case tok::kw_if:
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtIf(LabelInfo);
  case tok::kw_guard:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtGuard();
  case tok::kw_while:
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtWhile(LabelInfo);
  case tok::kw_repeat:
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtRepeat(LabelInfo);
  case tok::kw_do:
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtDo(LabelInfo);
  case tok::kw_for:
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtFor(LabelInfo);
  case tok::kw_switch:
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtSwitch(LabelInfo);
  /// 'case' and 'default' are only valid at the top level of a switch.
  case tok::kw_case:
  case tok::kw_default:
    return recoverFromInvalidCase(*this);
  case tok::kw_break:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtBreak();
  case tok::kw_continue:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return parseStmtContinue();
  case tok::kw_fallthrough:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    if (tryLoc.isValid()) diagnose(tryLoc, diag::try_on_stmt, Tok.getText());
    return makeParserResult(
        new (Context) FallthroughStmt(consumeToken(tok::kw_fallthrough)));
  }
}

/// parseBraceItemList - A brace enclosed expression/statement/decl list.  For
/// example { 1; 4+5; } or { 1; 2 }.  Always occurs as part of some other stmt
/// or decl.
///
///   brace-item-list:
///     '{' brace-item* '}'
///
ParserResult<BraceStmt> Parser::parseBraceItemList(Diag<> ID) {
  if (Tok.isNot(tok::l_brace)) {
    diagnose(Tok, ID);
    return nullptr;
  }
  SourceLoc LBLoc = consumeToken(tok::l_brace);

  SmallVector<ASTNode, 16> Entries;
  SourceLoc RBLoc;

  ParserStatus Status = parseBraceItems(Entries, BraceItemListKind::Brace,
                                        BraceItemListKind::Brace);
  parseMatchingToken(tok::r_brace, RBLoc,
                     diag::expected_rbrace_in_brace_stmt, LBLoc);

  return makeParserResult(Status,
                          BraceStmt::create(Context, LBLoc, Entries, RBLoc));
}

/// parseStmtBreak
///
///   stmt-break:
///     'break' identifier?
///
ParserResult<Stmt> Parser::parseStmtBreak() {
  SourceLoc Loc = consumeToken(tok::kw_break);
  SourceLoc TargetLoc;
  Identifier Target;

  // If we have an identifier after this, which is not the start of another
  // stmt or decl, we assume it is the label to break to, unless there is a
  // line break.  There is ambiguity with expressions (e.g. "break x+y") but
  // since the expression after the break is dead, we don't feel bad eagerly
  // parsing this.
  if (Tok.is(tok::identifier) && !Tok.isAtStartOfLine() &&
      !isStartOfStmt() && !isStartOfDecl())
    TargetLoc = consumeIdentifier(&Target);

  return makeParserResult(new (Context) BreakStmt(Loc, Target, TargetLoc));
}

/// parseStmtContinue
///
///   stmt-continue:
///     'continue' identifier?
///
ParserResult<Stmt> Parser::parseStmtContinue() {
  SourceLoc Loc = consumeToken(tok::kw_continue);
  SourceLoc TargetLoc;
  Identifier Target;

  // If we have an identifier after this, which is not the start of another
  // stmt or decl, we assume it is the label to continue to, unless there is a
  // line break.  There is ambiguity with expressions (e.g. "continue x+y") but
  // since the expression after the continue is dead, we don't feel bad eagerly
  // parsing this.
  if (Tok.is(tok::identifier) && !Tok.isAtStartOfLine() &&
      !isStartOfStmt() && !isStartOfDecl())
    TargetLoc = consumeIdentifier(&Target);

  return makeParserResult(new (Context) ContinueStmt(Loc, Target, TargetLoc));
}


/// parseStmtReturn
///
///   stmt-return:
///     'return' expr?
///   
ParserResult<Stmt> Parser::parseStmtReturn(SourceLoc tryLoc) {
  SourceLoc ReturnLoc = consumeToken(tok::kw_return);

  if (Tok.is(tok::code_complete)) {
    auto CCE = new (Context) CodeCompletionExpr(SourceRange(Tok.getLoc()));
    auto Result = makeParserResult(new (Context) ReturnStmt(ReturnLoc, CCE));
    if (CodeCompletion) {
      CodeCompletion->completeReturnStmt(CCE);
    }
    Result.setHasCodeCompletion();
    consumeToken();
    return Result;
  }

  // Handle the ambiguity between consuming the expression and allowing the
  // enclosing stmt-brace to get it by eagerly eating it unless the return is
  // followed by a '}', ';', statement or decl start keyword sequence.
  if (Tok.isNot(tok::r_brace, tok::semi, tok::eof, tok::pound_if,
                tok::pound_endif, tok::pound_else, tok::pound_elseif) &&
      !isStartOfStmt() && !isStartOfDecl()) {
    SourceLoc ExprLoc = Tok.getLoc();

    // Issue a warning when the returned expression is on a different line than
    // the return keyword, but both have the same indentation.
    if (SourceMgr.getLineAndColumn(ReturnLoc).second ==
        SourceMgr.getLineAndColumn(ExprLoc).second) {
      diagnose(ExprLoc, diag::unindented_code_after_return);
      diagnose(ExprLoc, diag::indent_expression_to_silence);
    }

    ParserResult<Expr> Result = parseExpr(diag::expected_expr_return);
    if (Result.isNull()) {
      // Create an ErrorExpr to tell the type checker that this return
      // statement had an expression argument in the source.  This suppresses
      // the error about missing return value in a non-void function.
      Result = makeParserErrorResult(new (Context) ErrorExpr(ExprLoc));
    }

    if (tryLoc.isValid()) {
      diagnose(tryLoc, diag::try_on_return_throw, /*isThrow=*/false)
        .fixItInsert(ExprLoc, "try ")
        .fixItRemoveChars(tryLoc, ReturnLoc);

      // Note: We can't use tryLoc here because that's outside the ReturnStmt's
      // source range.
      if (Result.isNonNull() && !isa<ErrorExpr>(Result.get()))
        Result = makeParserResult(new (Context) TryExpr(ExprLoc, Result.get()));
    }

    return makeParserResult(
        Result, new (Context) ReturnStmt(ReturnLoc, Result.getPtrOrNull()));
  }

  if (tryLoc.isValid())
    diagnose(tryLoc, diag::try_on_stmt, "return");

  return makeParserResult(new (Context) ReturnStmt(ReturnLoc, nullptr));
}

/// parseStmtThrow
///
/// stmt-throw
///   'throw' expr
///
ParserResult<Stmt> Parser::parseStmtThrow(SourceLoc tryLoc) {
  SourceLoc throwLoc = consumeToken(tok::kw_throw);
  SourceLoc exprLoc;
  if (Tok.isNot(tok::eof))
    exprLoc = Tok.getLoc();

  ParserResult<Expr> Result = parseExpr(diag::expected_expr_throw);

  if (Result.hasCodeCompletion())
    return makeParserCodeCompletionResult<Stmt>();

  if (Result.isNull())
    Result = makeParserErrorResult(new (Context) ErrorExpr(throwLoc));

  if (tryLoc.isValid() && exprLoc.isValid()) {
    diagnose(tryLoc, diag::try_on_return_throw, /*isThrow=*/true)
      .fixItInsert(exprLoc, "try ")
      .fixItRemoveChars(tryLoc, throwLoc);

    // Note: We can't use tryLoc here because that's outside the ThrowStmt's
    // source range.
    if (Result.isNonNull() && !isa<ErrorExpr>(Result.get()))
      Result = makeParserResult(new (Context) TryExpr(exprLoc, Result.get()));
  }

  return makeParserResult(Result,
              new (Context) ThrowStmt(throwLoc, Result.get()));
}

/// parseStmtDefer
///
///   stmt-defer:
///     'defer' brace-stmt
///
ParserResult<Stmt> Parser::parseStmtDefer() {
  SourceLoc DeferLoc = consumeToken(tok::kw_defer);
  
  // Macro expand out the defer into a closure and call, which we can typecheck
  // and emit where needed.
  //
  // The AST representation for a defer statement is a bit weird.  We retain the
  // brace statement that the user wrote, but actually model this as if they
  // wrote:
  //
  //    func tmpClosure() { body }
  //    tmpClosure()   // This is emitted on each path that needs to run this.
  //
  // As such, the body of the 'defer' is actually type checked within the
  // closure's DeclContext.
  auto params = ParameterList::createEmpty(Context);
  DeclName name(Context, Context.getIdentifier("$defer"), params);
  auto tempDecl
    = FuncDecl::create(Context,
                       /*StaticLoc=*/ SourceLoc(),
                       StaticSpellingKind::None,
                       /*FuncLoc=*/ SourceLoc(),
                       name,
                       /*NameLoc=*/ SourceLoc(),
                       /*Throws=*/ false, /*ThrowsLoc=*/ SourceLoc(),
                       /*AccessorKeywordLoc=*/SourceLoc(),
                       /*generic params*/ nullptr,
                       params,
                       TypeLoc(),
                       CurDeclContext);
  tempDecl->setImplicit();
  setLocalDiscriminator(tempDecl);
  ParserStatus Status;
  {
    // Change the DeclContext for any variables declared in the defer to be within
    // the defer closure.
    ParseFunctionBody cc(*this, tempDecl);
    
    ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_lbrace_after_defer);
    if (Body.isNull())
      return nullptr;
    Status |= Body;
    tempDecl->setBody(Body.get());
  }
  
  SourceLoc loc = tempDecl->getBody()->getStartLoc();

  // Form the call, which will be emitted on any path that needs to run the
  // code.
  auto DRE = new (Context) DeclRefExpr(tempDecl, DeclNameLoc(loc),
                                       /*Implicit*/true,
                                       AccessSemantics::DirectToStorage);
  auto call = CallExpr::createImplicit(Context, DRE, { }, { });
  
  auto DS = new (Context) DeferStmt(DeferLoc, tempDecl, call);
  return makeParserResult(Status, DS);
}

namespace {
  struct GuardedPattern {
    Pattern *ThePattern = nullptr;
    SourceLoc WhereLoc;
    Expr *Guard = nullptr;
  };
  
  /// Contexts in which a guarded pattern can appear.
  enum class GuardedPatternContext {
    Case,
    Catch,
  };
} // unnamed namespace

/// Parse a pattern-matching clause for a case or catch statement,
/// including the guard expression:
///
///    pattern 'where' expr
static void parseGuardedPattern(Parser &P, GuardedPattern &result,
                                ParserStatus &status,
                                SmallVectorImpl<VarDecl *> &boundDecls,
                                GuardedPatternContext parsingContext,
                                bool isFirstPattern) {
  ParserResult<Pattern> patternResult;
  auto setErrorResult = [&] () {
    patternResult = makeParserErrorResult(new (P.Context)
      AnyPattern(SourceLoc()));
  };
  bool isExprBasic = [&]() -> bool {
    switch (parsingContext) {
    // 'case' is terminated with a colon and so allows a trailing closure.
    case GuardedPatternContext::Case:
      return false;
    // 'catch' is terminated with a brace and so cannot.
    case GuardedPatternContext::Catch:
      return true;
    }
    llvm_unreachable("bad pattern context");
  }();

  // Do some special-case code completion for the start of the pattern.
  if (P.Tok.is(tok::code_complete)) {
    setErrorResult();
    if (P.CodeCompletion) {
      switch (parsingContext) {
      case GuardedPatternContext::Case:
        P.CodeCompletion->completeCaseStmtBeginning();
        break;
      case GuardedPatternContext::Catch:
        P.CodeCompletion->completePostfixExprBeginning(nullptr);
        break;
      }
      P.consumeToken();
    } else {
      result.ThePattern = patternResult.get();
      status.setHasCodeCompletion();
      return;
    }
  }
  if (parsingContext == GuardedPatternContext::Case &&
      P.Tok.isAny(tok::period_prefix, tok::period) &&
      P.peekToken().is(tok::code_complete)) {
    setErrorResult();
    if (P.CodeCompletion) {
      P.consumeToken();
      P.CodeCompletion->completeCaseStmtDotPrefix();
      P.consumeToken();
    } else {
      result.ThePattern = patternResult.get();
      status.setHasCodeCompletion();
      return;
    }
  }

  // If this is a 'catch' clause and we have "catch {" or "catch where...",
  // then we get an implicit "let error" pattern.
  if (parsingContext == GuardedPatternContext::Catch &&
      P.Tok.isAny(tok::l_brace, tok::kw_where)) {
    auto loc = P.Tok.getLoc();
    auto errorName = P.Context.Id_error;
    auto var = new (P.Context) VarDecl(/*IsStatic*/false, /*IsLet*/true,
                                       /*IsCaptureList*/false, loc, errorName,
                                       Type(), P.CurDeclContext);
    var->setImplicit();
    auto namePattern = new (P.Context) NamedPattern(var);
    auto varPattern = new (P.Context) VarPattern(loc, /*isLet*/true,
                                                 namePattern, /*implicit*/true);
    patternResult = makeParserResult(varPattern);
  }


  // Okay, if the special code-completion didn't kick in, parse a
  // matching pattern.
  if (patternResult.isNull()) {
    llvm::SaveAndRestore<decltype(P.InVarOrLetPattern)>
      T(P.InVarOrLetPattern, Parser::IVOLP_InMatchingPattern);
    patternResult = P.parseMatchingPattern(isExprBasic);
  }

  // If that didn't work, use a bogus pattern so that we can fill out
  // the AST.
  if (patternResult.isNull())
    patternResult =
      makeParserErrorResult(new (P.Context) AnyPattern(P.PreviousLoc));

  // Fill in the pattern.
  status |= patternResult;
  result.ThePattern = patternResult.get();

  if (isFirstPattern) {
    // Add variable bindings from the pattern to the case scope.  We have
    // to do this with a full AST walk, because the freshly parsed pattern
    // represents tuples and var patterns as tupleexprs and
    // unresolved_pattern_expr nodes, instead of as proper pattern nodes.
    patternResult.get()->forEachVariable([&](VarDecl *VD) {
      if (VD->hasName()) P.addToScope(VD);
      boundDecls.push_back(VD);
    });
  } else {
    // If boundDecls already contains variables, then we must match the
    // same number and same names in this pattern as were declared in a
    // previous pattern (and later we will make sure they have the same
    // types).
    SmallVector<VarDecl*, 4> repeatedDecls;
    patternResult.get()->forEachVariable([&](VarDecl *VD) {
      if (!VD->hasName())
        return;
      
      for (auto repeat : repeatedDecls)
        if (repeat->getName() == VD->getName())
          P.addToScope(VD); // will diagnose a duplicate declaration

      bool found = false;
      for (auto previous : boundDecls) {
        if (previous->hasName() && previous->getName() == VD->getName()) {
          found = true;
          break;
        }
      }
      if (!found) {
        // Diagnose a declaration that doesn't match a previous pattern.
        P.diagnose(VD->getLoc(), diag::extra_var_in_multiple_pattern_list, VD->getName());
        status.setIsParseError();
      }
      repeatedDecls.push_back(VD);
    });
    
    for (auto previous : boundDecls) {
      bool found = false;
      for (auto repeat : repeatedDecls) {
        if (previous->hasName() && previous->getName() == repeat->getName()) {
          found = true;
          break;
        }
      }
      if (!found) {
        // Diagnose a previous declaration that is missing in this pattern.
        P.diagnose(previous->getLoc(), diag::extra_var_in_multiple_pattern_list, previous->getName());
        status.setIsParseError();
      }
    }
    
    for (auto VD : repeatedDecls) {
      VD->setHasNonPatternBindingInit();
      VD->setImplicit();
    }
  }
  
  // Now that we have them, mark them as being initialized without a PBD.
  for (auto VD : boundDecls)
    VD->setHasNonPatternBindingInit();

  // Parse the optional 'where' guard.
  if (P.consumeIf(tok::kw_where, result.WhereLoc)) {
    SourceLoc startOfGuard = P.Tok.getLoc();

    auto diagKind = [=]() -> Diag<> {
      switch (parsingContext) {
      case GuardedPatternContext::Case:
        return diag::expected_case_where_expr;
      case GuardedPatternContext::Catch:
        return diag::expected_catch_where_expr;
      }
      llvm_unreachable("bad context");
    }();
    ParserResult<Expr> guardResult = P.parseExprImpl(diagKind, isExprBasic);
    status |= guardResult;

    // Use the parsed guard expression if possible.
    if (guardResult.isNonNull()) {
      result.Guard = guardResult.get();

    // Otherwise, fake up an ErrorExpr.
    } else {
      // If we didn't consume any tokens failing to parse the
      // expression, don't put in the source range of the ErrorExpr.
      SourceRange errorRange;
      if (startOfGuard == P.Tok.getLoc()) {
        errorRange = result.WhereLoc;
      } else {
        errorRange = SourceRange(startOfGuard, P.PreviousLoc);
      }
      result.Guard = new (P.Context) ErrorExpr(errorRange);
    }
  }
}

/// Validate availability spec list, emitting diagnostics if necessary.
static void validateAvailabilitySpecList(Parser &P,
                                         ArrayRef<AvailabilitySpec *> Specs) {
  llvm::SmallSet<PlatformKind, 4> Platforms;
  bool HasOtherPlatformSpec = false;

  if (Specs.size() == 1 &&
      isa<LanguageVersionConstraintAvailabilitySpec>(Specs[0])) {
    // @available(swift N) is allowed only in isolation; it cannot
    // be combined with other availability specs in a single list.
    return;
  }

  for (auto *Spec : Specs) {
    if (isa<OtherPlatformAvailabilitySpec>(Spec)) {
      HasOtherPlatformSpec = true;
      continue;
    }

    if (auto *LangSpec =
        dyn_cast<LanguageVersionConstraintAvailabilitySpec>(Spec)) {
      P.diagnose(LangSpec->getSwiftLoc(),
                 diag::availability_swift_must_occur_alone);
      continue;
    }

    auto *VersionSpec = cast<PlatformVersionConstraintAvailabilitySpec>(Spec);
    bool Inserted = Platforms.insert(VersionSpec->getPlatform()).second;
    if (!Inserted) {
      // Rule out multiple version specs referring to the same platform.
      // For example, we emit an error for
      /// #available(OSX 10.10, OSX 10.11, *)
      PlatformKind Platform = VersionSpec->getPlatform();
      P.diagnose(VersionSpec->getPlatformLoc(),
                 diag::availability_query_repeated_platform,
                 platformString(Platform));
    }
  }

  if (!HasOtherPlatformSpec) {
    SourceLoc InsertWildcardLoc = Specs.back()->getSourceRange().End;
    P.diagnose(InsertWildcardLoc, diag::availability_query_wildcard_required)
        .fixItInsertAfter(InsertWildcardLoc, ", *");
  }
}

// #available(...)
ParserResult<PoundAvailableInfo> Parser::parseStmtConditionPoundAvailable() {
  SourceLoc PoundLoc = consumeToken(tok::pound_available);

  if (!Tok.isFollowingLParen()) {
    diagnose(Tok, diag::avail_query_expected_condition);
    return makeParserError();
  }

  StructureMarkerRAII ParsingAvailabilitySpecList(*this, Tok);
  SourceLoc LParenLoc = consumeToken(tok::l_paren);

  SmallVector<AvailabilitySpec *, 5> Specs;
  ParserStatus Status = parseAvailabilitySpecList(Specs);

  for (auto *Spec : Specs) {
    if (auto *Lang =
        dyn_cast<LanguageVersionConstraintAvailabilitySpec>(Spec)) {
      diagnose(Lang->getSwiftLoc(),
               diag::pound_available_swift_not_allowed);
      Status.setIsParseError();
    }
  }

  SourceLoc RParenLoc;
  if (parseMatchingToken(tok::r_paren, RParenLoc,
                         diag::avail_query_expected_rparen, LParenLoc))
    Status.setIsParseError();

  auto *result = PoundAvailableInfo::create(Context, PoundLoc, Specs,RParenLoc);
  return makeParserResult(Status, result);
}

ParserStatus
Parser::parseAvailabilitySpecList(SmallVectorImpl<AvailabilitySpec *> &Specs) {
  ParserStatus Status = makeParserSuccess();

  // We don't use parseList() because we want to provide more specific
  // diagnostics disallowing operators in version specs.
  while (1) {
    auto SpecResult = parseAvailabilitySpec();
    if (auto *Spec = SpecResult.getPtrOrNull()) {
      Specs.push_back(Spec);
    } else {
      if (SpecResult.hasCodeCompletion()) {
        return makeParserCodeCompletionStatus();
      }
      Status.setIsParseError();
    }

    // We don't allow binary operators to combine specs.
    if (Tok.isBinaryOperator()) {
      diagnose(Tok, diag::avail_query_disallowed_operator, Tok.getText());
      consumeToken();
      Status.setIsParseError();
    } else if (consumeIf(tok::comma)) {
      // keep going.
    } else {
      break;
    }
  }

  if (Status.isSuccess())
    validateAvailabilitySpecList(*this, Specs);

  return Status;
}

/// Parse the condition of an 'if' or 'while'.
///
///   condition:
///     condition-clause (',' condition-clause)*
///   condition-clause:
///     expr-basic
///     ('var' | 'let' | 'case') pattern '=' expr-basic
///     '#available' '(' availability-spec (',' availability-spec)* ')'
///
/// The use of expr-basic here disallows trailing closures, which are
/// problematic given the curly braces around the if/while body.
///
ParserStatus Parser::parseStmtCondition(StmtCondition &Condition,
                                        Diag<> DefaultID, StmtKind ParentKind) {
  ParserStatus Status;
  Condition = StmtCondition();

  SmallVector<StmtConditionElement, 4> result;

  // This little helper function is used to consume a separator comma if
  // present, it returns false if it isn't there.  It also gracefully handles
  // the case when the user used && instead of comma, since that is a common
  // error.
  auto consumeSeparatorComma = [&]() -> bool {
    // If we have an "&&" token followed by a continuation of the statement
    // condition, then fixit the "&&" to "," and keep going.
    if (Tok.isAny(tok::oper_binary_spaced, tok::oper_binary_unspaced) &&
        Tok.getText() == "&&") {
      diagnose(Tok, diag::expected_comma_stmtcondition)
        .fixItReplaceChars(getEndOfPreviousLoc(), Tok.getRange().getEnd(), ",");
      consumeToken();
      return true;
    }

    // Boolean conditions are separated by commas, not the 'where' keyword, as
    // they were in Swift 2 and earlier.
    if (Tok.is(tok::kw_where)) {
      diagnose(Tok, diag::expected_comma_stmtcondition)
        .fixItReplaceChars(getEndOfPreviousLoc(), Tok.getRange().getEnd(), ",");
      consumeToken();
      return true;
    }
    
    // Otherwise, if a comma exists consume it and succeed.
    return consumeIf(tok::comma);
  };
  
 
  // For error recovery purposes, keep track of the disposition of the last
  // pattern binding we saw ('let', 'var', or 'case').
  StringRef BindingKindStr;
  
  // We have a simple comma separated list of clauses, but also need to handle
  // a variety of common errors situations (including migrating from Swift 2
  // syntax).
  bool isFirstIteration = true;
  while (isFirstIteration || consumeSeparatorComma()) {
    isFirstIteration = false;
    
    // Parse a leading #available condition if present.
    if (Tok.is(tok::pound_available)) {
      auto res = parseStmtConditionPoundAvailable();
      if (res.isNull() || res.hasCodeCompletion()) {
        Status |= res;
        return Status;
      }
      
      result.push_back({res.get()});
      BindingKindStr = StringRef();
      continue;
    }
    
    // Handle code completion after the #.
    if (Tok.is(tok::pound) && peekToken().is(tok::code_complete)) {
      consumeToken(); // '#' token.
      auto CodeCompletionPos = consumeToken();
      auto Expr = new (Context) CodeCompletionExpr(CodeCompletionPos);
      if (CodeCompletion)
        CodeCompletion->completeAfterPound(Expr, ParentKind);
      result.push_back(Expr);
      Status.setHasCodeCompletion();
      return Status;
    }
    
    // Parse the basic expression case.  If we have a leading let/var/case
    // keyword or an assignment, then we know this is a binding.
    if (Tok.isNot(tok::kw_let, tok::kw_var, tok::kw_case)) {
      // If we lack it, then this is theoretically a boolean condition.
      // However, we also need to handle migrating from Swift 2 syntax, in
      // which a comma followed by an expression could actually be a pattern
      // clause followed by a binding.  Determine what we have by checking for a
      // syntactically valid pattern followed by an '=', which can never be a
      // boolean condition.
      //
      // However, if this is the first clause, and we see "x = y", then this is
      // almost certainly a typo for '==' and definitely not a continuation of
      // another clause, so parse it as an expression.  This also avoids
      // lookahead + backtracking on simple if conditions that are obviously
      // boolean conditions.
      auto isBooleanExpr = [&]() -> bool {
        Parser::BacktrackingScope Backtrack(*this);
        return !canParseTypedPattern() || Tok.isNot(tok::equal);
      };

      if (BindingKindStr.empty() || isBooleanExpr()) {
        auto diagID = result.empty() ? DefaultID :
              diag::expected_expr_conditional;
        auto BoolExpr = parseExprBasic(diagID);
        Status |= BoolExpr;
        if (BoolExpr.isNull() || BoolExpr.hasCodeCompletion())
          return Status;
        result.push_back(BoolExpr.get());
        BindingKindStr = StringRef();
        continue;
      }
    }
    
    SourceLoc IntroducerLoc;
    if (Tok.isAny(tok::kw_let, tok::kw_var, tok::kw_case)) {
      BindingKindStr = Tok.getText();
      IntroducerLoc = consumeToken();
    } else {
      // If we lack the leading let/var/case keyword, then we're here because
      // the user wrote something like "if let x = foo(), y = bar() {".  Fix
      // this by inserting a new 'let' keyword before y.
      IntroducerLoc = Tok.getLoc();
      assert(!BindingKindStr.empty() &&
             "Shouldn't get here without a leading binding");
      diagnose(Tok.getLoc(), diag::expected_binding_keyword, BindingKindStr)
        .fixItInsert(Tok.getLoc(), BindingKindStr.str()+" ");
    }

    
    // We're parsing a conditional binding.
    assert(CurDeclContext->isLocalContext() &&
           "conditional binding in non-local context?!");
    
    ParserResult<Pattern> ThePattern;
    
    if (BindingKindStr == "case") {
      // In our recursive parse, remember that we're in a matching pattern.
      llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
        T(InVarOrLetPattern, IVOLP_InMatchingPattern);
      ThePattern = parseMatchingPattern(/*isExprBasic*/ true);
    } else if ((BindingKindStr == "let" || BindingKindStr == "var") &&
               Tok.is(tok::kw_case)) {
      // If will probably be a common typo to write "if let case" instead of
      // "if case let" so detect this and produce a nice fixit.
      diagnose(IntroducerLoc, diag::wrong_condition_case_location,
               BindingKindStr)
      .fixItRemove(IntroducerLoc)
      .fixItInsertAfter(Tok.getLoc(), " " + BindingKindStr.str());

      consumeToken(tok::kw_case);
      
      bool wasLet = BindingKindStr == "let";
      
      // In our recursive parse, remember that we're in a var/let pattern.
      llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
        T(InVarOrLetPattern, wasLet ? IVOLP_InLet : IVOLP_InVar);
      
      BindingKindStr = "case";
      ThePattern = parseMatchingPattern(/*isExprBasic*/ true);
      
      if (ThePattern.isNonNull()) {
        auto *P = new (Context) VarPattern(IntroducerLoc, wasLet,
                                           ThePattern.get(), /*impl*/false);
        ThePattern = makeParserResult(P);
      }

    } else {
      // Otherwise, this is an implicit optional binding "if let".
      ThePattern = parseMatchingPatternAsLetOrVar(BindingKindStr == "let",
                                                  IntroducerLoc,
                                                  /*isExprBasic*/ true);
      // The let/var pattern is part of the statement.
      if (Pattern *P = ThePattern.getPtrOrNull())
        P->setImplicit();
    }

    ThePattern = parseOptionalPatternTypeAnnotation(ThePattern,
                                                    BindingKindStr != "case");
    Status |= ThePattern;
    
    if (ThePattern.isNull() || ThePattern.hasCodeCompletion())
      return Status;

    // Conditional bindings must have an initializer.
    Expr *Init;
    if (consumeIf(tok::equal)) {
      ParserResult<Expr> InitExpr
        = parseExprBasic(diag::expected_expr_conditional_var);
      Status |= InitExpr;
      if (InitExpr.isNull() || InitExpr.hasCodeCompletion())
        return Status;
      Init = InitExpr.get();
      
    } else {
      // Although we require an initializer, recover by parsing as if it were
      // merely omitted.
      diagnose(Tok, diag::conditional_var_initializer_required);
      Init = new (Context) ErrorExpr(ThePattern.get()->getEndLoc());
    }
    
    result.push_back({IntroducerLoc, ThePattern.get(), Init});
    IntroducerLoc = SourceLoc();
    
    // Add variable bindings from the pattern to our current scope and mark
    // them as being having a non-pattern-binding initializer.
    ThePattern.get()->forEachVariable([&](VarDecl *VD) {
      if (VD->hasName())
        addToScope(VD);
      VD->setHasNonPatternBindingInit();
    });
    
  } while (consumeSeparatorComma());
  
  Condition = Context.AllocateCopy(result);
  return Status;
}

/// 
///   stmt-if:
///     'if' condition stmt-brace stmt-if-else?
///   stmt-if-else:
///    'else' stmt-brace
///    'else' stmt-if
ParserResult<Stmt> Parser::parseStmtIf(LabeledStmtInfo LabelInfo) {
  SourceLoc IfLoc = consumeToken(tok::kw_if);

  ParserStatus Status;
  StmtCondition Condition;
  ParserResult<BraceStmt> NormalBody;
  
  // A scope encloses the condition and true branch for any variables bound
  // by a conditional binding. The else branch does *not* see these variables.
  {
    Scope S(this, ScopeKind::IfVars);

    auto recoverWithCond = [&](ParserStatus Status,
                               StmtCondition Condition) -> ParserResult<Stmt> {
      if (Condition.empty()) {
        SmallVector<StmtConditionElement, 1> ConditionElems;
        ConditionElems.emplace_back(new (Context) ErrorExpr(IfLoc));
        Condition = Context.AllocateCopy(ConditionElems);
      }
      auto EndLoc = Condition.back().getEndLoc();
      return makeParserResult(
          Status,
          new (Context) IfStmt(
              LabelInfo, IfLoc, Condition,
              BraceStmt::create(Context, EndLoc, {}, EndLoc, /*implicit=*/true),
              SourceLoc(), nullptr));
    };

    if (Tok.is(tok::l_brace)) {
      SourceLoc LBraceLoc = Tok.getLoc();
      diagnose(IfLoc, diag::missing_condition_after_if)
        .highlight(SourceRange(IfLoc, LBraceLoc));
      SmallVector<StmtConditionElement, 1> ConditionElems;
      ConditionElems.emplace_back(new (Context) ErrorExpr(LBraceLoc));
      Condition = Context.AllocateCopy(ConditionElems);
    } else {
      Status |= parseStmtCondition(Condition, diag::expected_condition_if,
                                   StmtKind::If);
      if (Status.isError() || Status.hasCodeCompletion())
        return recoverWithCond(Status, Condition);
    }

    if (Tok.is(tok::kw_else)) {
      SourceLoc ElseLoc = Tok.getLoc();
      diagnose(ElseLoc, diag::unexpected_else_after_if);
      diagnose(ElseLoc, diag::suggest_removing_else)
        .fixItRemove(ElseLoc);
      consumeToken(tok::kw_else);
    }

    NormalBody = parseBraceItemList(diag::expected_lbrace_after_if);
    Status |= NormalBody;
    if (NormalBody.isNull())
      return recoverWithCond(Status, Condition);
  }

  // The else branch, if any, is outside of the scope of the condition.
  SourceLoc ElseLoc;
  ParserResult<Stmt> ElseBody;
  if (Tok.is(tok::kw_else)) {
    ElseLoc = consumeToken(tok::kw_else);
    if (Tok.is(tok::kw_if))
      ElseBody = parseStmtIf(LabeledStmtInfo());
    else
      ElseBody = parseBraceItemList(diag::expected_lbrace_after_else);
    Status |= ElseBody;
  }

  return makeParserResult(
      Status, new (Context) IfStmt(LabelInfo,
                                   IfLoc, Condition, NormalBody.get(),
                                   ElseLoc, ElseBody.getPtrOrNull()));
}

///   stmt-guard:
///     'guard' condition 'else' stmt-brace
///
ParserResult<Stmt> Parser::parseStmtGuard() {
  SourceLoc GuardLoc = consumeToken(tok::kw_guard);
  
  ParserStatus Status;
  StmtCondition Condition;
  ParserResult<BraceStmt> Body;

  auto recoverWithCond = [&](ParserStatus Status,
                             StmtCondition Condition) -> ParserResult<Stmt> {
    if (Condition.empty()) {
      SmallVector<StmtConditionElement, 1> ConditionElems;
      ConditionElems.emplace_back(new (Context) ErrorExpr(GuardLoc));
      Condition = Context.AllocateCopy(ConditionElems);
    }
    auto EndLoc = Condition.back().getEndLoc();
    return makeParserResult(
        Status,
        new (Context) GuardStmt(
            GuardLoc, Condition,
            BraceStmt::create(Context, EndLoc, {}, EndLoc, /*implicit=*/true)));
  };

  if (Tok.is(tok::l_brace)) {
    SourceLoc LBraceLoc = Tok.getLoc();
    diagnose(GuardLoc, diag::missing_condition_after_guard)
      .highlight(SourceRange(GuardLoc, LBraceLoc));
    SmallVector<StmtConditionElement, 1> ConditionElems;
    ConditionElems.emplace_back(new (Context) ErrorExpr(LBraceLoc));
    Condition = Context.AllocateCopy(ConditionElems);
  } else {
    Status |= parseStmtCondition(Condition, diag::expected_condition_guard,
                                 StmtKind::Guard);
    if (Status.isError() || Status.hasCodeCompletion()) {
      // FIXME: better recovery
      return recoverWithCond(Status, Condition);
    }
  }

  // Parse the 'else'.  If it is missing, and if the following token isn't a {
  // then the parser is hopelessly lost - just give up instead of spewing.
  if (parseToken(tok::kw_else, diag::expected_else_after_guard) &&
      Tok.isNot(tok::l_brace))
    return recoverWithCond(Status, Condition);

  // Before parsing the body, disable all of the bound variables so that they
  // cannot be used unbound.
  SmallVector<VarDecl *, 4> Vars;
  for (auto &elt : Condition)
    if (auto pattern = elt.getPatternOrNull())
      pattern->collectVariables(Vars);

  llvm::SaveAndRestore<decltype(DisabledVars)>
  RestoreCurVars(DisabledVars, Vars);

  llvm::SaveAndRestore<decltype(DisabledVarReason)>
  RestoreReason(DisabledVarReason, diag::bound_var_guard_body);

  Body = parseBraceItemList(diag::expected_lbrace_after_guard);
  if (Body.isNull())
    return recoverWithCond(Status, Condition);

  Status |= Body;
  
  return makeParserResult(Status,
              new (Context) GuardStmt(GuardLoc, Condition, Body.get()));
}


// Evaluate a subset of expression types suitable for build configuration
// conditional expressions.  The accepted expression types are:
//  - The magic constants "true" and "false".
//  - Named decl ref expressions ("FOO")
//  - Parenthesized expressions ("(FOO)")
//  - Binary "&&" or "||" operations applied to other build configuration
//    conditional expressions
//  - Unary "!" expressions applied to other build configuration conditional
//    expressions
//  - Single-argument call expressions, where the function being invoked is a
//    supported target configuration (currently "os", "arch", and
//    "_compiler_version"), and whose argument is a named decl ref expression
ConditionalCompilationExprState
Parser::classifyConditionalCompilationExpr(Expr *condition,
                                           ASTContext &Context,
                                           DiagnosticEngine &D) {
  assert(condition && "Cannot classify a NULL condition expression!");

  // Evaluate a ParenExpr.
  if (auto *PE = dyn_cast<ParenExpr>(condition))
    return classifyConditionalCompilationExpr(PE->getSubExpr(), Context, D);

  // Evaluate a "&&" or "||" expression.
  if (auto *SE = dyn_cast<SequenceExpr>(condition)) {
    // Check for '&&' or '||' as the expression type.
    if (SE->getNumElements() < 3) {
      D.diagnose(SE->getLoc(),
                 diag::unsupported_conditional_compilation_binary_expression);
      return ConditionalCompilationExprState::error();
    }
    // Before type checking, chains of binary expressions will not be fully
    // parsed, so associativity has not yet been encoded in the subtree.
    auto elements = SE->getElements();
    auto numElements = SE->getNumElements();
    size_t iOperator = 1;
    size_t iOperand = 2;

    auto result = classifyConditionalCompilationExpr(elements[0], Context, D);

    while (iOperand < numElements) {

      if (auto *UDREOp = dyn_cast<UnresolvedDeclRefExpr>(elements[iOperator])) {
        auto name = UDREOp->getName().getBaseName().str();

        if (name.equals("||") || name.equals("&&")) {
          auto rhs = classifyConditionalCompilationExpr(elements[iOperand],
                                                        Context, D);

          if (name.equals("||")) {
            result = result || rhs;
            if (result.isConditionActive())
              break;
          }

          if (name.equals("&&")) {
            result = result && rhs;
            if (!result.isConditionActive())
              break;
          }
        } else {
          D.diagnose(
              SE->getLoc(),
              diag::unsupported_conditional_compilation_binary_expression);
          return ConditionalCompilationExprState::error();
        }
      } else {
        // Swift3 didn't have this branch. the operator and the RHS are
        // silently ignored.
        if (!Context.isSwiftVersion3()) {
          D.diagnose(
              elements[iOperator]->getLoc(),
              diag::unsupported_conditional_compilation_expression_type);
          return ConditionalCompilationExprState::error();
        } else {
          SourceRange ignoredRange(elements[iOperator]->getLoc(),
                                   elements[iOperand]->getEndLoc());
          D.diagnose(
              elements[iOperator]->getLoc(),
              diag::swift3_unsupported_conditional_compilation_expression_type)
            .highlight(ignoredRange);
        }
      }

      iOperator += 2;
      iOperand += 2;
    }

    return result;
  }

  // Evaluate a named reference expression.
  if (auto *UDRE = dyn_cast<UnresolvedDeclRefExpr>(condition)) {
    auto name = UDRE->getName().getBaseName().str();
    return {Context.LangOpts.isCustomConditionalCompilationFlagSet(name),
      ConditionalCompilationExprKind::DeclRef};
  }

  // Evaluate a Boolean literal.
  if (auto *boolLit = dyn_cast<BooleanLiteralExpr>(condition)) {
    return {boolLit->getValue(), ConditionalCompilationExprKind::Boolean};
  }

  // Evaluate a negation (unary "!") expression.
  if (auto *PUE = dyn_cast<PrefixUnaryExpr>(condition)) {
    // If the PUE is not a negation expression, return false
    auto name =
    cast<UnresolvedDeclRefExpr>(PUE->getFn())->getName().getBaseName().str();
    if (name != "!") {
      D.diagnose(PUE->getLoc(),
                 diag::unsupported_conditional_compilation_unary_expression);
      return ConditionalCompilationExprState::error();
    }

    return !classifyConditionalCompilationExpr(PUE->getArg(), Context, D);
  }

  // Evaluate a target config call expression.
  if (auto *CE = dyn_cast<CallExpr>(condition)) {
    // look up target config, and compare value
    auto fnNameExpr = dyn_cast<UnresolvedDeclRefExpr>(CE->getFn());

    // Get the arg, which should be in a paren expression.
    if (!fnNameExpr) {
      D.diagnose(CE->getLoc(), diag::unsupported_platform_condition_expression);
      return ConditionalCompilationExprState::error();
    }

    auto fnName = fnNameExpr->getName().getBaseName().str();

    auto *PE = dyn_cast<ParenExpr>(CE->getArg());
    if (!PE) {
      auto diag = D.diagnose(CE->getLoc(),
                             diag::platform_condition_expected_one_argument);
      return ConditionalCompilationExprState::error();
    }

    if (!fnName.equals("arch") && !fnName.equals("os") &&
        !fnName.equals("_endian") &&
        !fnName.equals("_runtime") &&
        !fnName.equals("swift") &&
        !fnName.equals("_compiler_version")) {
      D.diagnose(CE->getLoc(), diag::unsupported_platform_condition_expression);
      return ConditionalCompilationExprState::error();
    }

    if (fnName.equals("_compiler_version")) {
      if (auto SLE = dyn_cast<StringLiteralExpr>(PE->getSubExpr())) {
        if (SLE->getValue().empty()) {
          D.diagnose(CE->getLoc(), diag::empty_version_string);
          return ConditionalCompilationExprState::error();
        }
        auto versionRequirement =
        version::Version::parseCompilerVersionString(SLE->getValue(),
                                                     SLE->getLoc(),
                                                     &D);
        auto thisVersion = version::Version::getCurrentCompilerVersion();
        auto VersionNewEnough = thisVersion >= versionRequirement;
        return {VersionNewEnough,
          ConditionalCompilationExprKind::CompilerVersion};
      } else {
        D.diagnose(CE->getLoc(), diag::unsupported_platform_condition_argument,
                 "string literal");
        return ConditionalCompilationExprState::error();
      }
    } else if (fnName.equals("swift")) {
      auto PUE = dyn_cast<PrefixUnaryExpr>(PE->getSubExpr());
      if (!PUE) {
        D.diagnose(PE->getSubExpr()->getLoc(),
                   diag::unsupported_platform_condition_argument,
                   "a unary comparison, such as '>=2.2'");
        return ConditionalCompilationExprState::error();
      }

      auto prefix = dyn_cast<UnresolvedDeclRefExpr>(PUE->getFn());
      auto versionArg = PUE->getArg();
      auto versionStartLoc = versionArg->getStartLoc();
      auto endLoc = Lexer::getLocForEndOfToken(Context.SourceMgr,
                                               versionArg->getSourceRange().End);
      CharSourceRange versionCharRange(Context.SourceMgr, versionStartLoc,
                                       endLoc);
      auto versionString = Context.SourceMgr.extractText(versionCharRange);

      auto versionRequirement =
        version::Version::parseVersionString(versionString,
                                             versionStartLoc,
                                             &D);

      if (!versionRequirement.hasValue())
        return ConditionalCompilationExprState::error();

      auto thisVersion = Context.LangOpts.EffectiveLanguageVersion;

      if (!prefix->getName().getBaseName().str().equals(">=")) {
        D.diagnose(PUE->getFn()->getLoc(),
                   diag::unexpected_version_comparison_operator)
          .fixItReplace(PUE->getFn()->getLoc(), ">=");
        return ConditionalCompilationExprState::error();
      }

      auto VersionNewEnough = thisVersion >= versionRequirement.getValue();
      return {VersionNewEnough,
        ConditionalCompilationExprKind::LanguageVersion};
    } else {
      if (auto UDRE = dyn_cast<UnresolvedDeclRefExpr>(PE->getSubExpr())) {
        // The sub expression should be an UnresolvedDeclRefExpr (we won't
        // tolerate extra parens).
        auto argumentIdent = UDRE->getName().getBaseName();
        auto argument = argumentIdent.str();

        // Error for values that don't make sense if there's a clear definition
        // of the possible values (as there is for _runtime).
        if (fnName.equals("_runtime") &&
            !argument.equals("_ObjC") && !argument.equals("_Native")) {
          D.diagnose(CE->getLoc(),
                     diag::unsupported_platform_runtime_condition_argument);
          return ConditionalCompilationExprState::error();
        }

        std::vector<StringRef> suggestions;
        SWIFT_DEFER {
          for (const StringRef& suggestion : suggestions) {
            D.diagnose(UDRE->getLoc(), diag::note_typo_candidate,
                       suggestion)
            .fixItReplace(UDRE->getSourceRange(), suggestion);
          }
        };
        if (fnName == "os") {
          if (!LangOptions::checkPlatformConditionOS(argument,
                                                     suggestions)) {
            D.diagnose(UDRE->getLoc(), diag::unknown_platform_condition_argument,
                       "operating system", fnName);
            return ConditionalCompilationExprState::error();
          }
        } else if (fnName == "arch") {
          if (!LangOptions::isPlatformConditionArchSupported(argument,
                                                             suggestions)) {
            D.diagnose(UDRE->getLoc(), diag::unknown_platform_condition_argument,
                       "architecture", fnName);
            return ConditionalCompilationExprState::error();
          }
        } else if (fnName == "_endian") {
          if (!LangOptions::isPlatformConditionEndiannessSupported(argument,
                                                                   suggestions)) {
            D.diagnose(UDRE->getLoc(), diag::unknown_platform_condition_argument,
                       "endianness", fnName);
          }
        }

        // FIXME: Perform the replacement macOS -> OSX elsewhere.
        if (fnName == "os" && argument == "macOS")
          argument = "OSX";

        auto target = Context.LangOpts.getPlatformConditionValue(fnName);
        return {target == argument, ConditionalCompilationExprKind::DeclRef};
      } else {
        D.diagnose(CE->getLoc(), diag::unsupported_platform_condition_argument,
                   "identifier");
        return ConditionalCompilationExprState::error();
      }
    }
  }

  // "#if 0" isn't valid, but it is common, so recognize it and handle it
  // with a fixit elegantly.
  if (auto *IL = dyn_cast<IntegerLiteralExpr>(condition))
    if (IL->getDigitsText() == "0" || IL->getDigitsText() == "1") {
      StringRef replacement = IL->getDigitsText() == "0" ? "false" :"true";
      D.diagnose(IL->getLoc(), diag::unsupported_conditional_compilation_integer,
                 IL->getDigitsText(), replacement)
       .fixItReplace(IL->getLoc(), replacement);
      return {IL->getDigitsText() == "1",
        ConditionalCompilationExprKind::Integer};
    }


  // If we've gotten here, it's an unsupported expression type.
  D.diagnose(condition->getLoc(),
             diag::unsupported_conditional_compilation_expression_type);
  return ConditionalCompilationExprState::error();
}

ParserResult<Stmt> Parser::parseStmtIfConfig(BraceItemListKind Kind) {
  StructureMarkerRAII ParsingDecl(*this, Tok.getLoc(),
                                  StructureMarkerKind::IfConfig);
  SmallVector<IfConfigStmtClause, 4> Clauses;

  bool foundActive = false;
  ConditionalCompilationExprState ConfigState;
  while (1) {
    bool isElse = Tok.is(tok::pound_else);
    SourceLoc ClauseLoc = consumeToken();
    Expr *Condition = nullptr;

    if (isElse) {
      ConfigState.setConditionActive(!foundActive);
    } else {
      // Evaluate the condition.
      llvm::SaveAndRestore<bool> S(InPoundIfEnvironment, true);
      ParserResult<Expr> Result = parseExprSequence(diag::expected_expr,
                                                    /*basic*/true,
                                                    /*isForDirective*/true);
      if (Result.isNull())
        return makeParserError();

      Condition = Result.get();

      // Evaluate the condition, to validate it.
      ConfigState = classifyConditionalCompilationExpr(Condition, Context,
                                                       Diags);
      if (foundActive)
        ConfigState.setConditionActive(false);
    }

    foundActive |= ConfigState.isConditionActive();

    if (!Tok.isAtStartOfLine() && Tok.isNot(tok::eof)) {
      diagnose(Tok.getLoc(),
               diag::extra_tokens_conditional_compilation_directive);
    }

    SmallVector<ASTNode, 16> Elements;
    if (ConfigState.shouldParse()) {
      parseBraceItems(Elements, Kind,
                      ConfigState.isConditionActive()
                        ? BraceItemListKind::ActiveConditionalBlock
                        : BraceItemListKind::InactiveConditionalBlock);
    } else {
      DiagnosticTransaction DT(Diags);
      skipUntilConditionalBlockClose();
      DT.abort();
    }

    Clauses.push_back(IfConfigStmtClause(ClauseLoc, Condition,
                                         Context.AllocateCopy(Elements),
                                         ConfigState.isConditionActive()));

    if (Tok.isNot(tok::pound_elseif) && Tok.isNot(tok::pound_else))
      break;

    if (isElse)
      diagnose(Tok, diag::expected_close_after_else_directive);
  }

  SourceLoc EndLoc;
  bool HadMissingEnd = parseEndIfDirective(EndLoc);

  auto *ICS = new (Context) IfConfigStmt(Context.AllocateCopy(Clauses),
                                         EndLoc, HadMissingEnd);
  return makeParserResult(ICS);
}

/// 
///   stmt-while:
///     (identifier ':')? 'while' expr-basic stmt-brace
ParserResult<Stmt> Parser::parseStmtWhile(LabeledStmtInfo LabelInfo) {
  SourceLoc WhileLoc = consumeToken(tok::kw_while);

  Scope S(this, ScopeKind::WhileVars);
  
  ParserStatus Status;
  StmtCondition Condition;

  auto recoverWithCond = [&](ParserStatus Status,
                             StmtCondition Condition) -> ParserResult<Stmt> {
    if (Condition.empty()) {
      SmallVector<StmtConditionElement, 1> ConditionElems;
      ConditionElems.emplace_back(new (Context) ErrorExpr(WhileLoc));
      Condition = Context.AllocateCopy(ConditionElems);
    }
    auto EndLoc = Condition.back().getEndLoc();
    return makeParserResult(
        Status,
        new (Context) WhileStmt(
            LabelInfo, WhileLoc, Condition,
            BraceStmt::create(Context, EndLoc, {}, EndLoc, /*implicit=*/true)));
  };

  if (Tok.is(tok::l_brace)) {
    SourceLoc LBraceLoc = Tok.getLoc();
    diagnose(WhileLoc, diag::missing_condition_after_while)
      .highlight(SourceRange(WhileLoc, LBraceLoc));
    SmallVector<StmtConditionElement, 1> ConditionElems;
    ConditionElems.emplace_back(new (Context) ErrorExpr(LBraceLoc));
    Condition = Context.AllocateCopy(ConditionElems);
  } else {
    Status |= parseStmtCondition(Condition, diag::expected_condition_while,
                                 StmtKind::While);
    if (Status.isError() || Status.hasCodeCompletion())
      return recoverWithCond(Status, Condition);
  }

  ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_lbrace_after_while);
  Status |= Body;
  if (Body.isNull())
    return recoverWithCond(Status, Condition);

  return makeParserResult(
      Status, new (Context) WhileStmt(LabelInfo, WhileLoc, Condition,
                                      Body.get()));
}

///
///   stmt-repeat:
///     (identifier ':')? 'repeat' stmt-brace 'while' expr
ParserResult<Stmt> Parser::parseStmtRepeat(LabeledStmtInfo labelInfo) {
  SourceLoc repeatLoc = consumeToken(tok::kw_repeat);

  ParserStatus status;

  ParserResult<BraceStmt> body =
      parseBraceItemList(diag::expected_lbrace_after_repeat);
  status |= body;
  if (body.isNull())
    body = makeParserResult(
        body, BraceStmt::create(Context, repeatLoc, {}, PreviousLoc, true));

  SourceLoc whileLoc;

  if (!consumeIf(tok::kw_while, whileLoc)) {
    diagnose(body.getPtrOrNull()->getEndLoc(),
             diag::expected_while_after_repeat_body);
    return body;
  }

  ParserResult<Expr> condition;
  if (Tok.is(tok::l_brace)) {
    SourceLoc lbraceLoc = Tok.getLoc();
    diagnose(whileLoc, diag::missing_condition_after_while);
    condition = makeParserErrorResult(new (Context) ErrorExpr(lbraceLoc));
  } else {
    condition = parseExpr(diag::expected_expr_repeat_while);
    status |= condition;
    if (condition.isNull()) {
      condition = makeParserErrorResult(new (Context) ErrorExpr(whileLoc));
    }
  }

  return makeParserResult(
      status,
      new (Context) RepeatWhileStmt(labelInfo, repeatLoc, condition.get(),
                                    whileLoc, body.get()));
}

/// 
///   stmt-do:
///     (identifier ':')? 'do' stmt-brace
///     (identifier ':')? 'do' stmt-brace stmt-catch+
ParserResult<Stmt> Parser::parseStmtDo(LabeledStmtInfo labelInfo) {
  SourceLoc doLoc = consumeToken(tok::kw_do);

  ParserStatus status;

  ParserResult<BraceStmt> body =
      parseBraceItemList(diag::expected_lbrace_after_do);
  status |= body;
  if (body.isNull())
    body = makeParserResult(
        body, BraceStmt::create(Context, doLoc, {}, PreviousLoc, true));

  // If the next token is 'catch', this is a 'do'/'catch' statement.
  if (Tok.is(tok::kw_catch)) {
    // Parse 'catch' clauses 
    SmallVector<CatchStmt*, 4> allClauses;
    do {
      ParserResult<CatchStmt> clause = parseStmtCatch();
      status |= clause;
      if (status.hasCodeCompletion() && clause.isNull())
        return makeParserResult<Stmt>(status, nullptr);

      // parseStmtCatch promises to return non-null unless we are
      // completing inside the catch's pattern.
      allClauses.push_back(clause.get());
    } while (Tok.is(tok::kw_catch) && !status.hasCodeCompletion());

    // Recover from all of the clauses failing to parse by returning a
    // normal do-statement.
    if (allClauses.empty()) {
      assert(status.isError());
      return makeParserResult(status,
                        new (Context) DoStmt(labelInfo, doLoc, body.get()));
    }

    return makeParserResult(status,
      DoCatchStmt::create(Context, labelInfo, doLoc, body.get(), allClauses));
  }

  SourceLoc whileLoc;

  // If we don't see a 'while', this is just the bare 'do' scoping
  // statement.
  if (!consumeIf(tok::kw_while, whileLoc)) {
    return makeParserResult(status,
                         new (Context) DoStmt(labelInfo, doLoc, body.get()));
  }

  // But if we do, advise the programmer that it's 'repeat' now.
  diagnose(doLoc, diag::do_while_now_repeat_while)
    .fixItReplace(doLoc, "repeat");
  status.setIsParseError();
  ParserResult<Expr> condition;
  if (Tok.is(tok::l_brace)) {
    SourceLoc lbraceLoc = Tok.getLoc();
    diagnose(whileLoc, diag::missing_condition_after_while);
    condition = makeParserErrorResult(new (Context) ErrorExpr(lbraceLoc));
  } else {
    condition = parseExpr(diag::expected_expr_repeat_while);
    status |= condition;
    if (condition.isNull() || condition.hasCodeCompletion())
      return makeParserResult<Stmt>(status, nullptr); // FIXME: better recovery
  }

  return makeParserResult(
      status,
      new (Context) RepeatWhileStmt(labelInfo, doLoc, condition.get(), whileLoc,
                                body.get()));
}

///  stmt-catch:
///    'catch' pattern ('where' expr)? stmt-brace
///
/// Note that this is not a "first class" statement; it can only
/// appear following a 'do' statement.
///
/// This routine promises to return a non-null result unless there was
/// a code-completion token in the pattern.
ParserResult<CatchStmt> Parser::parseStmtCatch() {
  // A catch block has its own scope for variables bound out of the pattern.
  Scope S(this, ScopeKind::CatchVars);

  SourceLoc catchLoc = consumeToken(tok::kw_catch);

  SmallVector<VarDecl*, 4> boundDecls;

  ParserStatus status;
  GuardedPattern pattern;
  parseGuardedPattern(*this, pattern, status, boundDecls,
                      GuardedPatternContext::Catch, /* isFirst */ true);
  if (status.hasCodeCompletion()) {
    return makeParserCodeCompletionResult<CatchStmt>();
  }

  auto bodyResult = parseBraceItemList(diag::expected_lbrace_after_catch);
  status |= bodyResult;
  if (bodyResult.isNull()) {
    bodyResult = makeParserErrorResult(BraceStmt::create(Context, PreviousLoc,
                                                         {}, PreviousLoc,
                                                         /*implicit=*/ true));
  }

  auto result =
    new (Context) CatchStmt(catchLoc, pattern.ThePattern, pattern.WhereLoc,
                            pattern.Guard, bodyResult.get());
  return makeParserResult(status, result);
}

ParserResult<Stmt> Parser::parseStmtFor(LabeledStmtInfo LabelInfo) {
  SourceLoc ForLoc = consumeToken(tok::kw_for);

  // The c-style-for loop and foreach-style-for loop are conflated together into
  // a single keyword, so we have to do some lookahead to resolve what is going
  // on.
  
  // If we have a leading identifier followed by a ':' or 'in', then this is
  // obviously a for-each loop.  For error recovery, also parse "for in ..." as
  // foreach.
  if ((Tok.isIdentifierOrUnderscore() &&
         peekToken().isAny(tok::colon, tok::kw_in)) ||
      Tok.is(tok::kw_in))
    return parseStmtForEach(ForLoc, LabelInfo);

  // If we have "for ;" then this is clearly a c-style for loop.
  if (Tok.is(tok::semi))
    return parseStmtForCStyle(ForLoc, LabelInfo);

  // Otherwise, we have to do lookahead.  An unparenthesized valid C-style
  // for-each loop will start with "let/var <irrefutable pattern> =".  Check for
  // that.
  bool isCStyleFor = false;
  {
    Parser::BacktrackingScope Backtrack(*this);

    // The condition of a foreach loop can be parenthesized.
    consumeIf(tok::l_paren);

    // Skip until we see eof, "in" (in which case we have a for-in loop),
    // ";" in which case we have a simple expression as the first part of a
    // c-style for loop, or "{" in which case we have a malformed statement.
    while (Tok.isNot(tok::eof, tok::kw_in, tok::semi, tok::l_brace))
      skipSingle();

    isCStyleFor = Tok.isAny(tok::semi, tok::l_brace, tok::eof);
  }
  
  // Otherwise, this is some sort of c-style for loop.
  if (isCStyleFor)
    return parseStmtForCStyle(ForLoc, LabelInfo);

  return parseStmtForEach(ForLoc, LabelInfo);
}


/// Given an expression, check to see if it is a set of braces "{...}" parsed as
/// a ClosureExpr that is probably a body of a statement.  If so, convert it
/// into a BraceStmt that can be used as the body of a control flow statement
/// to improve error recovery.
///
/// If this expression isn't a ClosureExpr or isn't convertible, this returns
/// null.
///
static BraceStmt *ConvertClosureToBraceStmt(Expr *E, ASTContext &Ctx) {
  if (!E) return nullptr;
  
  auto *CE = dyn_cast<ClosureExpr>(E);
  if (!CE) return nullptr;
  
  // If this had a signature or anon-closure parameters (like $0) used, then it
  // doesn't "look" like the body of a control flow statement, it looks like a
  // closure.
  if (CE->getInLoc().isValid() || CE->hasExplicitResultType() ||
      CE->getParameters()->size() != 0)
    return nullptr;

  // Silence downstream errors by giving it type ()->(), to match up with the
  // call we will produce.
  CE->setImplicit();
  auto empty = TupleTypeRepr::createEmpty(Ctx, CE->getStartLoc());
  CE->setExplicitResultType(CE->getStartLoc(), empty);
  
  // The trick here is that the ClosureExpr provides a DeclContext for stuff
  // inside of it, so it isn't safe to just drop it and rip the BraceStmt
  // from inside of it.  While we could try to walk the body and update any
  // Decls, ClosureExprs, etc within the body of the ClosureExpr, it is easier
  // to just turn it into BraceStmt(CallExpr(TheClosure, VoidTuple)).  This also
  // more correctly handles the implicit ReturnStmt injected into single-expr
  // closures.
  ASTNode theCall = CallExpr::createImplicit(Ctx, CE, { }, { });
  return BraceStmt::create(Ctx, CE->getStartLoc(), theCall, CE->getEndLoc(),
                           /*implicit*/true);
}


///   stmt-for-c-style:
///     (identifier ':')? 'for' stmt-for-c-style-init? ';' expr-basic? ';'
///           (expr-basic (',' expr-basic)*)? stmt-brace
///     (identifier ':')? 'for' '(' stmt-for-c-style-init? ';' expr-basic? ';'
///           (expr-basic (',' expr-basic)*)? ')' stmt-brace
///   stmt-for-c-style-init:
///     decl-var
///     expr (',' expr)*
ParserResult<Stmt> Parser::parseStmtForCStyle(SourceLoc ForLoc,
                                              LabeledStmtInfo LabelInfo) {
  SourceLoc Semi1Loc, Semi2Loc;
  SourceLoc LPLoc, RPLoc;
  bool LPLocConsumed = false;

  ParserStatus Status;

  ParserResult<Expr> First;
  SmallVector<Decl*, 2> FirstDecls;
  ParserResult<Expr> Second;
  ParserResult<Expr> Third;
  ParserResult<BraceStmt> Body;
  
  // Introduce a new scope to contain any var decls in the init value.
  Scope S(this, ScopeKind::ForVars);
  
  if (Tok.is(tok::l_paren)) {
    LPLoc = consumeToken();
    LPLocConsumed = true;
  }
  // Parse the first part, either a var, let, expr, or stmt-assign.
  if (Tok.is(tok::kw_var) || Tok.is(tok::kw_let) || Tok.is(tok::at_sign)) {
    DeclAttributes Attributes;
    bool FoundCCToken;
    parseDeclAttributeList(Attributes, FoundCCToken);

    // After parsing optional attributes above we should be at 'var' or 'let'
    if (!Tok.is(tok::kw_var) && !Tok.is(tok::kw_let)) {
      diagnose(Tok.getLoc(), diag::expected_var_decl_for_stmt);
      return makeParserError();
    }

    ParserStatus VarDeclStatus = parseDeclVar(PD_InLoop, Attributes, FirstDecls,
                                              SourceLoc(),
                                              StaticSpellingKind::None,
                                              SourceLoc());
    if (VarDeclStatus.isError())
      return VarDeclStatus; // FIXME: better recovery
  } else if (Tok.isNot(tok::semi)) {
    SmallVector<Expr *, 1> FirstExprs;

    // Parse the first expression.
    First = parseExpr(diag::expected_init_for_stmt);
    Status |= First;
    if (First.isNull() || First.hasCodeCompletion())
      return makeParserResult<Stmt>(Status, nullptr); // FIXME: better recovery

    FirstExprs.push_back(First.get());

    // Parse additional expressions.
    while (Tok.is(tok::comma)) {
      consumeToken(tok::comma);

      First = parseExpr(diag::expected_expr);
      Status |= First;

      if (First.isNull() || First.hasCodeCompletion())
        return makeParserResult<Stmt>(Status, nullptr); // FIXME: better recovery

      if (First.isNonNull())
        FirstExprs.push_back(First.get());
    }

    // If we had more than one expression, form a tuple.
    if (FirstExprs.size() > 1) {
      First = makeParserResult(
                TupleExpr::createImplicit(Context, FirstExprs, { }));
    }
  }

  ArrayRef<Decl *> FirstDeclsContext;
  if (!FirstDecls.empty())
    FirstDeclsContext = Context.AllocateCopy(FirstDecls);
  VarDecl *IterationVariable = nullptr;
  for (auto *D : FirstDeclsContext) {
    if (auto *VD = dyn_cast<VarDecl>(D)) {
      IterationVariable = VD;
      break;
    }
  }

  // If we're missing a semicolon, try to recover.
  if (Tok.isNot(tok::semi)) {
    // Provide a reasonable default location for the first semicolon.
    Semi1Loc = PreviousLoc;

    if (auto *BS = ConvertClosureToBraceStmt(First.getPtrOrNull(), Context)) {
      // We have seen:
      //     for { ... }
      // and there's no semicolon after that.
      //
      // We parsed the brace statement as a closure.  Recover by using the
      // brace statement as a 'for' body.
      First = makeParserErrorResult(new (Context) ErrorExpr(BS->getStartLoc()));
      Second = nullptr;
      Third = nullptr;
      Body = makeParserErrorResult(BS);
      diagnose(ForLoc, diag::missing_init_for_stmt)
          .highlight(SourceRange(ForLoc, BS->getStartLoc()));
      Status.setIsParseError();

      return makeParserResult(
          Status, new (Context) ForStmt(LabelInfo, ForLoc, First.getPtrOrNull(),
                                        FirstDeclsContext,
                                        Semi1Loc, Second.getPtrOrNull(),
                                        Semi2Loc, Third.getPtrOrNull(),
                                        Body.get()));
    }
  }

  // Consume the first semicolon.
  if (parseToken(tok::semi, Semi1Loc, diag::expected_semi_for_stmt))
    Status.setIsParseError();

  CodeCompletionCallbacks::InCStyleForExprRAII InCStyleForExpr(
      CodeCompletion, IterationVariable);

  if (Tok.isNot(tok::semi)) {
    Second = parseExprBasic(diag::expected_cond_for_stmt);
    Status |= Second;
  }

  if (Tok.isNot(tok::semi) && Second.isNonNull()) {
    Expr *RecoveredCondition = nullptr;
    BraceStmt *RecoveredBody = ConvertClosureToBraceStmt(Second.get(), Context);

    if (auto *CE = dyn_cast<CallExpr>(Second.get())) {
      if (auto *PE = dyn_cast<ParenExpr>(CE->getArg())) {
        if (PE->hasTrailingClosure() && !RecoveredBody) {
          // We have seen:
          //     for ... ; ... { ... }
          // and there's no semicolon after that.
          //
          // We parsed the condition as a CallExpr with a brace statement as a
          // trailing closure.  Recover by using the original expression as the
          // condition and brace statement as a 'for' body.
          RecoveredBody = ConvertClosureToBraceStmt(PE->getSubExpr(), Context);
          RecoveredCondition = CE->getFn();
        }
      }
    }
    if (RecoveredBody) {
      SourceLoc LBraceLoc = RecoveredBody->getStartLoc();
      Second = makeParserErrorResult(RecoveredCondition);
      Third = nullptr;
      Body = makeParserErrorResult(RecoveredBody);
      diagnose(LBraceLoc, diag::expected_semi_for_stmt)
          .highlight(SourceRange(ForLoc, LBraceLoc));
      Status.setIsParseError();

      return makeParserResult(
          Status, new (Context) ForStmt(LabelInfo, ForLoc, First.getPtrOrNull(),
                                        FirstDeclsContext,
                                        Semi1Loc, Second.getPtrOrNull(),
                                        Semi2Loc, Third.getPtrOrNull(),
                                        Body.get()));
    }
  }

  // Consume the second semicolon.
  if (parseToken(tok::semi, Semi2Loc, diag::expected_semi_for_stmt))
    Status.setIsParseError();

  if (Tok.isNot(tok::l_brace, tok::r_paren)) {
    SmallVector<Expr *, 1> ThirdExprs;

    // Parse the first expression.
    Third = parseExprBasic(diag::expected_expr);
    Status |= Third;
    if (Third.isNonNull())
      ThirdExprs.push_back(Third.get());

    // Parse additional expressions.
    while (Tok.is(tok::comma)) {
      consumeToken(tok::comma);

      Third = parseExprBasic(diag::expected_expr);
      Status |= Third;

      if (Third.isNonNull())
        ThirdExprs.push_back(Third.get());
    }

    // If we had more than one expression, form a tuple.
    if (ThirdExprs.size() > 1) {
      Third = makeParserResult(
                TupleExpr::createImplicit(Context, ThirdExprs, { }));
    }
  }

  InCStyleForExpr.finished();

  if (LPLocConsumed && parseMatchingToken(tok::r_paren, RPLoc,
                                          diag::expected_rparen_for_stmt,LPLoc))
    Status.setIsParseError();

  Body = parseBraceItemList(diag::expected_lbrace_after_for);
  Status |= Body;
  if (Body.isNull())
    Body = makeParserResult(
        Body, BraceStmt::create(Context, PreviousLoc, {}, PreviousLoc, true));

  return makeParserResult(
      Status,
      new (Context) ForStmt(LabelInfo, ForLoc, First.getPtrOrNull(),
                            FirstDeclsContext,
                            Semi1Loc, Second.getPtrOrNull(), Semi2Loc,
                            Third.getPtrOrNull(), Body.get()));
}

/// 
///   stmt-for-each:
///     (identifier ':')? 'for' pattern 'in' expr-basic \
///             ('where' expr-basic)? stmt-brace
ParserResult<Stmt> Parser::parseStmtForEach(SourceLoc ForLoc,
                                            LabeledStmtInfo LabelInfo) {
  ParserStatus Status;

  ParserResult<Pattern> pattern;

  // Parse the pattern.  This is either 'case <refutable pattern>' or just a
  // normal pattern.
  if (consumeIf(tok::kw_case)) {
    llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
      T(InVarOrLetPattern, Parser::IVOLP_InMatchingPattern);
    pattern = parseMatchingPattern(/*isExprBasic*/true);
    pattern = parseOptionalPatternTypeAnnotation(pattern, /*isOptional*/false);
  } else {
    // Change the parser state to know that the pattern we're about to parse is
    // implicitly mutable.  Bound variables can be changed to mutable explicitly
    // if desired by using a 'var' pattern.
    assert(InVarOrLetPattern == IVOLP_NotInVarOrLet &&
           "for-each loops cannot exist inside other patterns");
    InVarOrLetPattern = IVOLP_ImplicitlyImmutable;
    pattern = parseTypedPattern();
    assert(InVarOrLetPattern == IVOLP_ImplicitlyImmutable);
    InVarOrLetPattern = IVOLP_NotInVarOrLet;
  }
  
  if (pattern.isNull())
    // Recover by creating a "_" pattern.
    pattern = makeParserErrorResult(new (Context) AnyPattern(SourceLoc()));

  // Bound variables all get their initial values from the generator.
  pattern.get()->markHasNonPatternBindingInit();
  
  SourceLoc InLoc;
  parseToken(tok::kw_in, InLoc, diag::expected_foreach_in);

  ParserResult<Expr> Container;
  if (Tok.is(tok::l_brace)) {
    SourceLoc LBraceLoc = Tok.getLoc();
    diagnose(LBraceLoc, diag::expected_foreach_container);
    Container = makeParserErrorResult(new (Context) ErrorExpr(LBraceLoc));
  } else {
    Container = parseExprBasic(diag::expected_foreach_container);
    if (Container.isNull())
      Container = makeParserErrorResult(new (Context) ErrorExpr(Tok.getLoc()));
    Status |= Container;
  }

  // Introduce a new scope and place the variables in the pattern into that
  // scope.
  // FIXME: We may want to merge this scope with the scope introduced by
  // the stmt-brace, as in C++.
  Scope S(this, ScopeKind::ForeachVars);
  
  // Introduce variables to the current scope.
  addPatternVariablesToScope(pattern.get());

  // Parse the 'where' expression if present.
  ParserResult<Expr> Where;
  if (consumeIf(tok::kw_where)) {
    Where = parseExprBasic(diag::expected_foreach_where_expr);
    if (Where.isNull())
      Where = makeParserErrorResult(new (Context) ErrorExpr(Tok.getLoc()));
    Status |= Where;
  }


  // stmt-brace
  ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_foreach_lbrace);
  Status |= Body;
  if (Body.isNull())
    Body = makeParserResult(
        Body, BraceStmt::create(Context, ForLoc, {}, PreviousLoc, true));

  return makeParserResult(
      Status,
      new (Context) ForEachStmt(LabelInfo, ForLoc, pattern.get(), InLoc,
                                Container.get(), Where.getPtrOrNull(),
                                Body.get()));
}

///
///    stmt-switch:
///      (identifier ':')? 'switch' expr-basic '{' stmt-case+ '}'
ParserResult<Stmt> Parser::parseStmtSwitch(LabeledStmtInfo LabelInfo) {
  SourceLoc SwitchLoc = consumeToken(tok::kw_switch);

  ParserStatus Status;
  ParserResult<Expr> SubjectExpr;
  SourceLoc SubjectLoc = Tok.getLoc();
  if (Tok.is(tok::l_brace)) {
    diagnose(SubjectLoc, diag::expected_switch_expr);
    SubjectExpr = makeParserErrorResult(new (Context) ErrorExpr(SubjectLoc));
  } else {
    SubjectExpr = parseExprBasic(diag::expected_switch_expr);
    if (SubjectExpr.hasCodeCompletion()) {
      return makeParserCodeCompletionResult<Stmt>();
    }
    if (SubjectExpr.isNull()) {
      SubjectExpr = makeParserErrorResult(new (Context) ErrorExpr(SubjectLoc));
    }
    Status |= SubjectExpr;
  }

  if (!Tok.is(tok::l_brace)) {
    diagnose(Tok, diag::expected_lbrace_after_switch);
    return nullptr;
  }
  SourceLoc lBraceLoc = consumeToken(tok::l_brace);
  SourceLoc rBraceLoc;
  
  // If there are non-case-label statements at the start of the switch body,
  // raise an error and recover by discarding them.
  bool DiagnosedNotCoveredStmt = false;
  while (!Tok.is(tok::kw_case) && !Tok.is(tok::kw_default)
         && !Tok.is(tok::r_brace) && !Tok.is(tok::eof)) {
    if (!DiagnosedNotCoveredStmt) {
      diagnose(Tok, diag::stmt_in_switch_not_covered_by_case);
      DiagnosedNotCoveredStmt = true;
    }
    skipSingle();
  }
  
  SmallVector<CaseStmt*, 8> cases;
  bool parsedDefault = false;
  bool parsedBlockAfterDefault = false;
  while (Tok.is(tok::kw_case) || Tok.is(tok::kw_default)) {
    // We cannot have additional cases after a default clause. Complain on
    // the first offender.
    if (parsedDefault && !parsedBlockAfterDefault) {
      parsedBlockAfterDefault = true;
      diagnose(Tok, diag::case_after_default);
    }

    ParserResult<CaseStmt> Case = parseStmtCase();
    Status |= Case;
    if (Case.isNonNull()) {
      cases.push_back(Case.get());
      if (Case.get()->isDefault())
        parsedDefault = true;
    }
  }

  if (parseMatchingToken(tok::r_brace, rBraceLoc,
                         diag::expected_rbrace_switch, lBraceLoc)) {
    Status.setIsParseError();
  }

  // Reject an empty 'switch'.
  if (!DiagnosedNotCoveredStmt && cases.empty())
    diagnose(rBraceLoc, diag::empty_switch_stmt);

  return makeParserResult(
      Status, SwitchStmt::create(LabelInfo, SwitchLoc, SubjectExpr.get(),
                                 lBraceLoc, cases, rBraceLoc, Context));
}

static ParserStatus parseStmtCase(Parser &P, SourceLoc &CaseLoc,
                                  SmallVectorImpl<CaseLabelItem> &LabelItems,
                                  SmallVectorImpl<VarDecl *> &BoundDecls,
                                  SourceLoc &ColonLoc) {
  ParserStatus Status;
  bool isFirst = true;
  
  CaseLoc = P.consumeToken(tok::kw_case);

  do {
    GuardedPattern PatternResult;
    parseGuardedPattern(P, PatternResult, Status, BoundDecls,
                        GuardedPatternContext::Case, isFirst);
    LabelItems.push_back(CaseLabelItem(/*IsDefault=*/false,
                                       PatternResult.ThePattern,
                                       PatternResult.WhereLoc,
                                       PatternResult.Guard));
    isFirst = false;
  } while (P.consumeIf(tok::comma));

  ColonLoc = P.Tok.getLoc();
  if (!P.Tok.is(tok::colon)) {
    P.diagnose(P.Tok, diag::expected_case_colon, "case");
    Status.setIsParseError();
  } else
    P.consumeToken(tok::colon);

  return Status;
}

static ParserStatus
parseStmtCaseDefault(Parser &P, SourceLoc &CaseLoc,
                     SmallVectorImpl<CaseLabelItem> &LabelItems,
                     SourceLoc &ColonLoc) {
  ParserStatus Status;

  CaseLoc = P.consumeToken(tok::kw_default);

  // We don't allow 'where' guards on a 'default' block. For recovery
  // parse one if present.
  SourceLoc WhereLoc;
  ParserResult<Expr> Guard;
  if (P.Tok.is(tok::kw_where)) {
    P.diagnose(P.Tok, diag::default_with_where);
    WhereLoc = P.consumeToken(tok::kw_where);
    Guard = P.parseExpr(diag::expected_case_where_expr);
    Status |= Guard;
  }

  ColonLoc = P.Tok.getLoc();
  if (!P.Tok.is(tok::colon)) {
    P.diagnose(P.Tok, diag::expected_case_colon, "default");
    Status.setIsParseError();
  } else
    P.consumeToken(tok::colon);

  // Create an implicit AnyPattern to represent the default match.
  auto Any = new (P.Context) AnyPattern(CaseLoc);
  LabelItems.push_back(
      CaseLabelItem(/*IsDefault=*/true, Any, WhereLoc, Guard.getPtrOrNull()));

  return Status;
}

ParserResult<CaseStmt> Parser::parseStmtCase() {
  // A case block has its own scope for variables bound out of the pattern.
  Scope S(this, ScopeKind::CaseVars);

  ParserStatus Status;

  SmallVector<CaseLabelItem, 2> CaseLabelItems;
  SmallVector<VarDecl *, 4> BoundDecls;

  SourceLoc CaseLoc;
  SourceLoc ColonLoc;
  if (Tok.is(tok::kw_case)) {
    Status |=
        ::parseStmtCase(*this, CaseLoc, CaseLabelItems, BoundDecls, ColonLoc);
  } else {
    Status |= parseStmtCaseDefault(*this, CaseLoc, CaseLabelItems, ColonLoc);
  }

  assert(!CaseLabelItems.empty() && "did not parse any labels?!");

  SmallVector<ASTNode, 8> BodyItems;

  SourceLoc StartOfBody = Tok.getLoc();
  if (Tok.isNot(tok::kw_case) && Tok.isNot(tok::kw_default) &&
      Tok.isNot(tok::r_brace)) {
    Status |= parseBraceItems(BodyItems, BraceItemListKind::Case);
  } else if (Status.isSuccess()) {
    diagnose(CaseLoc, diag::case_stmt_without_body,
             CaseLabelItems.back().isDefault())
        .highlight(SourceRange(CaseLoc, ColonLoc))
        .fixItInsertAfter(ColonLoc, " break");
  }
  BraceStmt *Body;
  if (BodyItems.empty()) {
    Body = BraceStmt::create(Context, PreviousLoc, ArrayRef<ASTNode>(),
                             PreviousLoc, /*implicit=*/true);
  } else {
    Body = BraceStmt::create(Context, StartOfBody, BodyItems,
                             PreviousLoc, /*implicit=*/true);
  }

  return makeParserResult(
      Status, CaseStmt::create(Context, CaseLoc, CaseLabelItems,
                               !BoundDecls.empty(), ColonLoc, Body));
}
