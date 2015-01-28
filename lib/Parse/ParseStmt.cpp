//===--- ParseStmt.cpp - Swift Language Parser for Statements -------------===//
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
// Statement Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/Version.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

/// isStartOfStmt - Return true if the current token starts a statement.
///
bool Parser::isStartOfStmt() {
  switch (Tok.getKind()) {
  default: return false;
  case tok::kw_return:
  case tok::kw_if:
  case tok::kw_while:
  case tok::kw_do:
  case tok::kw_for:
  case tok::kw_break:
  case tok::kw_continue:
  case tok::kw_fallthrough:
  case tok::kw_switch:
  case tok::kw_case:
  case tok::kw_default:
  case tok::pound_if:
  case tok::pound_line:
    return true;
      
  case tok::identifier:
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

  ParserResult<Expr> ResultExpr = parseExpr(diag::expected_expr);
  if (ResultExpr.isNonNull())
    Result = ResultExpr.get();

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
  case BraceItemListKind::ActiveConfigBlock:
  case BraceItemListKind::InactiveConfigBlock:
    return Tok.isNot(tok::pound_else) && Tok.isNot(tok::pound_endif) &&
           Tok.isNot(tok::pound_elseif);
  }
}

void Parser::consumeTopLevelDecl(ParserPosition BeginParserPosition,
                                 TopLevelCodeDecl *TLCD) {
  backtrackToPosition(BeginParserPosition);
  SourceLoc BeginLoc = Tok.getLoc();
  // Consume tokens up to code completion token.
  while (Tok.isNot(tok::code_complete)) {
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

static void diagnoseDiscardedClosure(Parser &P, ASTNode &Result) {
  // If we parsed a bare closure as an expression, it will be a discarded value
  // expression and the type checker will complain.

  if (isa<AbstractClosureExpr>(P.CurDeclContext))
    // Inside a closure expression, an expression which syntactically looks
    // like a discarded value expression, can become the return value of the
    // closure.  Don't attempt recovery.
    return;

  if (auto *E = Result.dyn_cast<Expr *>()) {
    if (auto *CE = dyn_cast<ClosureExpr>(E)) {
      if (!CE->hasAnonymousClosureVars())
        // Parameters are explicitly specified, and could be used in the body,
        // don't attempt recovery.
        return;
      P.diagnose(CE->getBody()->getLBraceLoc(), diag::brace_stmt_invalid);
    }
  }
}

///   brace-item:
///     decl
///     expr
///     stmt
///   stmt:
///     ';'
///     stmt-assign
///     stmt-if
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
                                     BraceItemListKind ConfigKind) {
  
  bool IsTopLevel = (Kind == BraceItemListKind::TopLevelCode) ||
                    (Kind == BraceItemListKind::TopLevelLibrary);
  bool isActiveConfigBlock = ConfigKind == BraceItemListKind::ActiveConfigBlock;
  bool isConfigBlock = isActiveConfigBlock ||
                          ConfigKind == BraceItemListKind::InactiveConfigBlock;

  // If we're not parsing an active #if block, form a new lexical scope.
  Optional<Scope> initScope;
  if (!isActiveConfigBlock) {
    auto scopeKind =  IsTopLevel ? ScopeKind::TopLevel : ScopeKind::Brace;
    initScope.emplace(this, scopeKind);
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
         Tok.isNot(tok::kw_sil) && Tok.isNot(tok::kw_sil_stage) &&
         Tok.isNot(tok::kw_sil_vtable) && Tok.isNot(tok::kw_sil_global) &&
         Tok.isNot(tok::kw_sil_witness_table) &&
         (isConfigBlock ||
          !isTerminatorForBraceItemListKind(Tok, Kind, Entries))) {
    if (Kind == BraceItemListKind::TopLevelLibrary &&
        skipExtraTopLevelRBraces())
      continue;

    bool NeedParseErrorRecovery = false;
    ASTNode Result;

    // If the previous statement didn't have a semicolon and this new
    // statement doesn't start a line, complain.
    if (!PreviousHadSemi && !Tok.isAtStartOfLine()) {
      SourceLoc EndOfPreviousLoc = Lexer::getLocForEndOfToken(SourceMgr,
                                                              PreviousLoc);
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
        && Tok.isNot(tok::pound_if)
        && Tok.isNot(tok::pound_line)) {
      ParserStatus Status =
          parseDecl(TmpDecls, IsTopLevel ? PD_AllowTopLevel : PD_Default);
      if (Status.isError()) {
        NeedParseErrorRecovery = true;
        if (Status.hasCodeCompletion() && IsTopLevel &&
            isCodeCompletionFirstPass()) {
          consumeDecl(BeginParserPosition, None, IsTopLevel);
          return Status;
        }
      }

      for (Decl *D : TmpDecls)
        Entries.push_back(D);
      if (!TmpDecls.empty())
        PreviousHadSemi = TmpDecls.back()->TrailingSemiLoc.isValid();
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
      }

    } else if (Tok.is(tok::pound_line)) {
      ParserStatus Status = parseLineDirective();
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
        diagnose(Tok, diag::discard_result_of_closure)
          .fixItInsert(Tok.getLoc(), "_ = ");
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
      diagnoseDiscardedClosure(*this, Result);
      if (!Result.isNull()) {
        // NOTE: this is a 'virtual' brace statement which does not have
        //       explicit '{' or '}', so the start and end locations should be
        //       the same as those of the result node
        auto Brace = BraceStmt::create(Context, Result.getStartLoc(),
                                       Result, Result.getEndLoc());
        TLCD->setBody(Brace);
        Entries.push_back(TLCD);
      }
    } else {
      SourceLoc StartLoc = Tok.getLoc();
      ParserStatus ExprOrStmtStatus = parseExprOrStmt(Result);
      BraceItemsStatus |= ExprOrStmtStatus;
      if (ExprOrStmtStatus.isError())
        NeedParseErrorRecovery = true;
      diagnoseDiscardedClosure(*this, Result);
      if (ExprOrStmtStatus.isSuccess() && IsTopLevel) {
        // If this is a normal library, you can't have expressions or statements
        // outside at the top level.
        diagnose(StartLoc,
                 Result.is<Stmt*>() ? diag::illegal_top_level_stmt
                                    : diag::illegal_top_level_expr);
        Result = ASTNode();
      }

      if (!Result.isNull())
        Entries.push_back(Result);
    }

    if (!NeedParseErrorRecovery && !PreviousHadSemi && Tok.is(tok::semi)) {
      if (Result) {
        if (Result.is<Expr*>()) {
          Result.get<Expr*>()->TrailingSemiLoc = consumeToken(tok::semi);
        } else {
          Result.get<Stmt*>()->TrailingSemiLoc = consumeToken(tok::semi);
        }
      }
      PreviousHadSemi = true;
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

  // Create a lexer that can not go past the end state.
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
    diagnose(Tok, diag::discard_result_of_closure)
      .fixItInsert(Tok.getLoc(), "_ = ");
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
  
  
  switch (Tok.getKind()) {
  default:
    diagnose(Tok, diag::expected_stmt);
    return nullptr;
  case tok::kw_return:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtReturn();
  case tok::kw_if:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtIf();
  case tok::pound_if:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtIfConfig();
  case tok::pound_line:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseLineDirective();
  case tok::kw_while:  return parseStmtWhile(LabelInfo);
  case tok::kw_do:     return parseStmtDoWhile(LabelInfo);
  case tok::kw_for:    return parseStmtFor(LabelInfo);
  case tok::kw_switch: return parseStmtSwitch(LabelInfo);
  /// 'case' and 'default' are only valid at the top level of a switch.
  case tok::kw_case:
  case tok::kw_default:
    return recoverFromInvalidCase(*this);
  case tok::kw_break:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtBreak();
  case tok::kw_continue:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtContinue();
  case tok::kw_fallthrough:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
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

  ParserStatus Status = parseBraceItems(Entries);
  parseMatchingToken(tok::r_brace, RBLoc,
                     diag::expected_rbrace_in_brace_stmt, LBLoc);

  return makeParserResult(Status,
                          BraceStmt::create(Context, LBLoc, Entries, RBLoc));
}

/// \brief Parses the elements in active or inactive if config clauses.
void Parser::parseIfConfigClauseElements(bool isActive,
                                         BraceItemListKind Kind,
                                         SmallVectorImpl<ASTNode> &Elements) {
  parseBraceItems(Elements,
                  Kind,
                  isActive
                    ? BraceItemListKind::ActiveConfigBlock
                    : BraceItemListKind::InactiveConfigBlock);
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
ParserResult<Stmt> Parser::parseStmtReturn() {
  SourceLoc ReturnLoc = consumeToken(tok::kw_return);

  // Handle the ambiguity between consuming the expression and allowing the
  // enclosing stmt-brace to get it by eagerly eating it unless the return is
  // followed by a '}', ';', statement or decl start keyword sequence.
  if (Tok.isNot(tok::r_brace) && Tok.isNot(tok::semi) &&
      !isStartOfStmt() && !isStartOfDecl()) {
    SourceLoc ExprLoc;
    if (Tok.isNot(tok::eof))
      ExprLoc = Tok.getLoc();
    ParserResult<Expr> Result = parseExpr(diag::expected_expr_return);
    if (Result.isNull() && ExprLoc.isValid()) {
      // Create an ErrorExpr to tell the type checker that this return
      // statement had an expression argument in the source.  This supresses
      // the error about missing return value in a non-void function.
      Result = makeParserErrorResult(new (Context) ErrorExpr(ExprLoc));
    }
    return makeParserResult(
        Result, new (Context) ReturnStmt(ReturnLoc, Result.getPtrOrNull()));
  }

  return makeParserResult(new (Context) ReturnStmt(ReturnLoc, nullptr));
}

/// Parse the condition of an 'if' or 'while'.
///
///   condition:
///     expr-basic
///     conditional-binding (',' conditional-binding)*
///   condition-binding:
///     ('var' | 'let') condition-bind (',' condition-bind)* condition-where
///   condition-bind:
///     pattern '=' expr-basic
///   condition-where:
///     'where' expr-basic
///
/// The use of expr-basic here disallows trailing closures, which are
/// problematic given the curly braces around the if/while body.
///
ParserStatus Parser::parseStmtCondition(StmtCondition &Condition,
                                        Diag<> ID) {
  ParserStatus Status;
  Condition = StmtCondition();

  SmallVector<StmtConditionElement, 4> result;
  if (Tok.isNot(tok::kw_var) && Tok.isNot(tok::kw_let)) {
    ParserResult<Expr> Expr = parseExprBasic(ID);
    Status |= Expr;
    result.push_back(Expr.getPtrOrNull());
    Condition = Context.AllocateCopy(result);
    return Status;
  }

  // We're parsing a conditional binding.
  assert(CurDeclContext->isLocalContext() &&
         "conditional binding in non-local context?!");

  // Parse the list of condition-bindings, each of which can have a where.
  do {
    bool IsLet = Tok.is(tok::kw_let);
    SourceLoc VarLoc = consumeToken();

    // Parse the list of name binding's within a let/var clauses.
    do {
      auto Pattern = parsePattern(IsLet);
      Status |= Pattern;
      if (Pattern.isNull() || Pattern.hasCodeCompletion())
        return Status;

      Expr *Init;
      // Conditional bindings must have an initializer.
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
        Init = new (Context) ErrorExpr(Tok.getLoc());
      }
    
      auto PBD = new (Context) PatternBindingDecl(SourceLoc(),
                                                  StaticSpellingKind::None,
                                                  VarLoc, Pattern.get(),
                                                  Init,
                                                  /*isConditional*/ true,
                                                  /*parent*/CurDeclContext);
      result.push_back(PBD);
      // Introduce variables to the current scope.
      addPatternVariablesToScope(Pattern.get());

    } while (Tok.is(tok::comma) && peekToken().isNot(tok::kw_let, tok::kw_var)&&
             consumeIf(tok::comma));

    // If there is a where clause on this let/var specification, parse and
    // remember it.
    if (consumeIf(tok::kw_where)) {
      ParserResult<Expr> InitExpr
        = parseExprBasic(diag::expected_expr_conditional_where);
      Status |= InitExpr;
      if (InitExpr.isNull() || InitExpr.hasCodeCompletion())
        return Status;
      result.push_back(InitExpr.get());
    }

  } while (Tok.is(tok::comma) && peekToken().isAny(tok::kw_let, tok::kw_var) &&
           consumeIf(tok::comma));

  Condition = Context.AllocateCopy(result);
  return Status;
}

/// 
///   stmt-if:
///     'if' condition stmt-brace stmt-if-else?
///   stmt-if-else:
///    'else' stmt-brace
///    'else' stmt-if
ParserResult<Stmt> Parser::parseStmtIf() {
  SourceLoc IfLoc = consumeToken(tok::kw_if);

  ParserStatus Status;
  StmtCondition Condition;
  ParserResult<BraceStmt> NormalBody;
  
  // A scope encloses the condition and true branch for any variables bound
  // by a conditional binding. The else branch does *not* see these variables.
  {
    Scope S(this, ScopeKind::IfVars);
  
    Status |= parseStmtCondition(Condition, diag::expected_condition_if);
    if (Status.isError() || Status.hasCodeCompletion())
      return makeParserResult<Stmt>(Status, nullptr); // FIXME: better recovery

    if (Condition.size() == 1 && Condition[0].isCondition() &&
        isa<ClosureExpr>(Condition[0].getCondition())) {
      auto *CE = cast<ClosureExpr>(Condition[0].getCondition());
      // If we parsed closure after 'if', then it was not the condition, but the
      // 'if' statement body.  We could not have had a bare closure in an 'if'
      // condition because closures don't conform to BooleanType.
      auto ClosureBody = CE->getBody();
      SourceLoc LBraceLoc = ClosureBody->getStartLoc();
      NormalBody = makeParserErrorResult(ClosureBody);
      Condition[0] = StmtConditionElement(new (Context) ErrorExpr(LBraceLoc));
      diagnose(IfLoc, diag::missing_condition_after_if)
          .highlight(SourceRange(IfLoc, LBraceLoc));
    }
    if (NormalBody.isNull())
      NormalBody = parseBraceItemList(diag::expected_lbrace_after_if);
    if (NormalBody.isNull())
      return nullptr; // FIXME: better recovery

    Status |= NormalBody;
  }

  // The else branch, if any, is outside of the scope of the condition.
  SourceLoc ElseLoc;
  ParserResult<Stmt> ElseBody;
  if (Tok.is(tok::kw_else)) {
    ElseLoc = consumeToken(tok::kw_else);
    if (Tok.is(tok::kw_if))
      ElseBody = parseStmtIf();
    else
      ElseBody = parseBraceItemList(diag::expected_lbrace_after_else);
    Status |= ElseBody;
  }

  return makeParserResult(
      Status, new (Context) IfStmt(IfLoc, Condition, NormalBody.get(),
                                   ElseLoc, ElseBody.getPtrOrNull()));
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
//    supported target configuration (currently "os" and "arch"), and whose
//    argument is a named decl ref expression
bool Parser::evaluateConfigConditionExpr(Expr *configExpr) {
    // Evaluate a ParenExpr.
  if (auto *PE = dyn_cast<ParenExpr>(configExpr))
    return evaluateConfigConditionExpr(PE->getSubExpr());
  
  // Evaluate a "&&" or "||" expression.
  if (auto *SE = dyn_cast<SequenceExpr>(configExpr)) {
    // Check for '&&' or '||' as the expression type.
    if (SE->getNumElements() < 3) {
      diagnose(SE->getLoc(), diag::unsupported_build_config_binary_expression);
      return false;
    }
    // Before type checking, chains of binary expressions will not be fully
    // parsed, so associativity has not yet been encoded in the subtree.
    auto elements = SE->getElements();
    auto numElements = SE->getNumElements();
    size_t iOperator = 1;
    size_t iOperand = 2;
    
    bool result = evaluateConfigConditionExpr(elements[0]);
    
    while (iOperand < numElements) {
      
      if (auto *UDREOp = dyn_cast<UnresolvedDeclRefExpr>(elements[iOperator])) {
        auto name = UDREOp->getName().str();
        
        if (name.equals("||")) {
          result = result || evaluateConfigConditionExpr(elements[iOperand]);
          
          if (result)
            break;
          
        } else if (name.equals("&&")) {
          if (!result) {
            break;
          }
          
          result = result && evaluateConfigConditionExpr(elements[iOperand]);
        } else {
          diagnose(SE->getLoc(),
                   diag::unsupported_build_config_binary_expression);
          return false;
        }
      }
      
      iOperator += 2;
      iOperand += 2;
    }
    
    return result;
  }
  
  // Evaluate a named reference expression.
  if (auto *UDRE = dyn_cast<UnresolvedDeclRefExpr>(configExpr)) {
    auto name = UDRE->getName().str();
    return Context.LangOpts.hasBuildConfigOption(name);
  }

  // Evaluate a Boolean literal.
  if (auto *boolLit = dyn_cast<BooleanLiteralExpr>(configExpr)) {
    return boolLit->getValue();
  }

  // Evaluate a negation (unary "!") expression.
  if (auto *PUE = dyn_cast<PrefixUnaryExpr>(configExpr)) {
    // If the PUE is not a negation expression, return false
    auto name = cast<UnresolvedDeclRefExpr>(PUE->getFn())->getName().str();
    if (name != "!") {
      diagnose(PUE->getLoc(), diag::unsupported_build_config_unary_expression);
      return false;
    }
    
    return !evaluateConfigConditionExpr(PUE->getArg());
  }
  
  // Evaluate a target config call expression.
  if (auto *CE = dyn_cast<CallExpr>(configExpr)) {
    // look up target config, and compare value
    auto fnNameExpr = dyn_cast<UnresolvedDeclRefExpr>(CE->getFn());
    
    // Get the arg, which should be in a paren expression.
    auto *PE = dyn_cast<ParenExpr>(CE->getArg());
    if (!fnNameExpr || !PE || !isa<UnresolvedDeclRefExpr>(PE->getSubExpr())) {
      diagnose(CE->getLoc(), diag::unsupported_target_config_argument_type);
      return false;
    }

    auto targetValue = fnNameExpr->getName().str();
    
    if (!targetValue.equals("arch") && !targetValue.equals("os") &&
        !targetValue.equals("_runtime")) {
      diagnose(CE->getLoc(), diag::unsupported_target_config_expression);
      return false;
    }

    // The sub expression should be an UnresolvedDeclRefExpr (we won't
    // tolerate extra parens).
    auto *UDRE = cast<UnresolvedDeclRefExpr>(PE->getSubExpr());
    auto target = Context.LangOpts.getTargetConfigOption(targetValue);
    return target == UDRE->getName().str();
  }
  
  // If we've gotten here, it's an unsupported expression type.
  diagnose(configExpr->getLoc(),
           diag::unsupported_config_conditional_expression_type);
  return false;
}

ParserResult<Stmt> Parser::parseStmtIfConfig(BraceItemListKind Kind) {
  StructureMarkerRAII ParsingDecl(*this, Tok.getLoc(),
                                  StructureMarkerKind::IfConfig);
  
  bool foundActive = false;
  SmallVector<IfConfigStmtClause, 4> Clauses;
  
  while (1) {
    bool isElse = Tok.is(tok::pound_else);
    SourceLoc ClauseLoc = consumeToken();
    Expr *Condition = nullptr;
    
    bool ClauseIsActive;
    if (isElse) {
      ClauseIsActive = !foundActive;
    } else {
      if (Tok.isAtStartOfLine())
        diagnose(ClauseLoc, diag::expected_build_configuration_expression);
      
      // Evaluate the condition.
      ParserResult<Expr> Configuration = parseExprSequence(diag::expected_expr,
                                                           true, true);
      if (Configuration.isNull())
        return makeParserError();
      
      Condition = Configuration.get();
      
      // Evaluate the condition, to validate it.
      bool condActive = evaluateConfigConditionExpr(Condition);
      ClauseIsActive = condActive && !foundActive;
    }

    foundActive |= ClauseIsActive;
    
    if (!Tok.isAtStartOfLine())
      diagnose(Tok.getLoc(), diag::extra_tokens_config_directive);

    SmallVector<ASTNode, 16> Elements;
    parseIfConfigClauseElements(ClauseIsActive, Kind, Elements);
    
    Clauses.push_back(IfConfigStmtClause(ClauseLoc, Condition,
                                         Context.AllocateCopy(Elements),
                                         ClauseIsActive));
    
    if (Tok.isNot(tok::pound_elseif) && Tok.isNot(tok::pound_else))
      break;
    
    if (isElse)
      diagnose(Tok, diag::expected_close_after_else);
  }

  // Parse the #endif
  SourceLoc EndLoc = Tok.getLoc();
  bool HadMissingEnd = false;
  if (parseToken(tok::pound_endif, diag::expected_close_to_config_stmt)) {
    HadMissingEnd = true;
    EndLoc = PreviousLoc;
    skipUntilConfigBlockClose();
  }
  else if (!Tok.isAtStartOfLine())
    diagnose(Tok.getLoc(), diag::extra_tokens_config_directive);

  
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
  Status |= parseStmtCondition(Condition, diag::expected_condition_while);
  if (Status.isError() || Status.hasCodeCompletion())
    return makeParserResult<Stmt>(Status, nullptr); // FIXME: better recovery

  ParserResult<BraceStmt> Body;
  if (Condition.size() == 1 && Condition[0].isCondition() &&
      isa<ClosureExpr>(Condition[0].getCondition())) {
    auto *CE = cast<ClosureExpr>(Condition[0].getCondition());
    // If we parsed a closure after 'while', then it was not the condition, but
    // the 'while' statement body.  We can not have a bare closure in a 'while'
    // condition because closures don't conform to LogicValue.
    auto ClosureBody = CE->getBody();
    SourceLoc LBraceLoc = ClosureBody->getStartLoc();
    Body = makeParserErrorResult(ClosureBody);
    Condition[0] = StmtConditionElement(new (Context) ErrorExpr(LBraceLoc));
    diagnose(WhileLoc, diag::missing_condition_after_while)
        .highlight(SourceRange(WhileLoc, LBraceLoc));
  }
  if (Body.isNull())
    Body = parseBraceItemList(diag::expected_lbrace_after_while);
  if (Body.isNull())
    return nullptr; // FIXME: better recovery

  Status |= Body;

  return makeParserResult(
      Status, new (Context) WhileStmt(LabelInfo, WhileLoc, Condition,
                                      Body.get()));
}

/// 
///   stmt-do-while:
///     (identifier ':')? 'do' stmt-brace 'while' expr
ParserResult<Stmt> Parser::parseStmtDoWhile(LabeledStmtInfo LabelInfo) {
  SourceLoc DoLoc = consumeToken(tok::kw_do);

  ParserStatus Status;

  ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_lbrace_after_do);
  Status |= Body;
  if (Body.isNull())
    Body = makeParserResult(
        Body, BraceStmt::create(Context, DoLoc, {}, PreviousLoc, true));

  SourceLoc WhileLoc;
  if (parseToken(tok::kw_while, WhileLoc, diag::expected_while_in_dowhile))
    return nullptr; // FIXME: better recovery

  ParserPosition ConditionStartState;
  if (Tok.is(tok::l_brace)) {
    // It is unusual for the condition expression to start with a left brace,
    // and we anticipate the need to do recovery.  Save the parser state so
    // that we can rewind.
    ConditionStartState = getParserPosition();
  }

  ParserResult<Expr> Condition = parseExpr(diag::expected_expr_do_while);
  Status |= Condition;
  if (Condition.isNull() || Condition.hasCodeCompletion())
    return makeParserResult<Stmt>(Status, nullptr); // FIXME: better recovery

  if (auto *CE = dyn_cast<ClosureExpr>(Condition.get())) {
    // If we parsed a closure after 'do ... while', then it was not the
    // condition, but a beginning of the next statement.  We can not have a
    // bare closure in a 'do ... while' condition because closures don't
    // conform to LogicValue.
    SourceLoc LBraceLoc = CE->getBody()->getStartLoc();
    Condition = makeParserErrorResult(new (Context) ErrorExpr(LBraceLoc));
    diagnose(WhileLoc, diag::missing_condition_after_while);

    // We did not actually want to parse the next statement.
    backtrackToPosition(ConditionStartState);
  }

  return makeParserResult(
      Status,
      new (Context) DoWhileStmt(LabelInfo, DoLoc, Condition.get(), WhileLoc,
                                Body.get()));
}

ParserResult<Stmt> Parser::parseStmtFor(LabeledStmtInfo LabelInfo) {
  SourceLoc ForLoc = consumeToken(tok::kw_for);

  // The c-style-for loop and foreach-style-for loop are conflated together into
  // a single keyword, so we have to do some lookahead to resolve what is going
  // on.

  if (Tok.is(tok::l_paren)) {
    auto SavedPosition = getParserPosition();
    consumeToken(tok::l_paren);
    skipUntil(tok::r_paren);
    bool IsCStyle = peekToken().is(tok::l_brace);
    backtrackToPosition(SavedPosition);
    if (IsCStyle)
      return parseStmtForCStyle(ForLoc, LabelInfo);
    return parseStmtForEach(ForLoc, LabelInfo);
  }

  // If we have a leading identifier followed by a ':' or 'in', then this is a
  // pattern, so it is foreach.
  //
  // For error recovery, also parse "for in ..." as foreach.
  if ((isAtStartOfBindingName() &&
       (peekToken().is(tok::colon) || peekToken().is(tok::kw_in))) ||
      Tok.is(tok::kw_in))
    return parseStmtForEach(ForLoc, LabelInfo);

  // Otherwise, this is some sort of c-style for loop.
  return parseStmtForCStyle(ForLoc, LabelInfo);
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

  bool HaveFirst = false;
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
    parseDeclAttributeList(Attributes);

    // After parsing optional attributes above we should be at 'var' or 'let'
    if (!Tok.is(tok::kw_var) && !Tok.is(tok::kw_let)) {
      diagnose(Tok.getLoc(), diag::expected_var_decl_for_stmt);
      return makeParserError();
    }

    ParserStatus VarDeclStatus = parseDeclVar(PD_InLoop, Attributes, FirstDecls,
                                              SourceLoc(),
                                              StaticSpellingKind::None);
    if (VarDeclStatus.isError())
      return VarDeclStatus; // FIXME: better recovery
  } else if (Tok.isNot(tok::semi)) {
    SmallVector<Expr *, 1> FirstExprs;

    // Parse the first expression.
    HaveFirst = true;
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

  if (Tok.isNot(tok::semi)) {
    if (auto *CE = dyn_cast_or_null<ClosureExpr>(First.getPtrOrNull())) {
      // We have seen:
      //     for { ... }
      // and there's no semicolon after that.
      //
      // We parsed the brace statement as a closure.  Recover by using the
      // brace statement as a 'for' body.
      auto ClosureBody = CE->getBody();
      SourceLoc LBraceLoc = ClosureBody->getStartLoc();
      First = makeParserErrorResult(new (Context) ErrorExpr(LBraceLoc));
      Second = nullptr;
      Third = nullptr;
      Body = makeParserErrorResult(ClosureBody);
      diagnose(ForLoc, diag::missing_init_for_stmt)
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
    BraceStmt *RecoveredBody = nullptr;
    if (auto *CE = dyn_cast<ClosureExpr>(Second.get())) {
      // We have seen:
      //     for ... ; { ... }
      // and there's no semicolon after that.
      //
      // We parsed the brace statement as a closure.  Recover by using the
      // brace statement as a 'for' body.
      RecoveredCondition = nullptr;
      RecoveredBody = CE->getBody();
    }
    if (auto *CE = dyn_cast<CallExpr>(Second.get())) {
      if (auto *PE = dyn_cast<ParenExpr>(CE->getArg())) {
        if (PE->hasTrailingClosure()) {
          // We have seen:
          //     for ... ; ... { ... }
          // and there's no semicolon after that.
          //
          // We parsed the condition as a CallExpr with a brace statement as a
          // trailing closure.  Recover by using the original expression as the
          // condition and brace statement as a 'for' body.
          RecoveredBody = cast<ClosureExpr>(PE->getSubExpr())->getBody();
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

  if (Tok.isNot(tok::l_brace)) {
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
        Body, BraceStmt::create(Context, ForLoc, {}, PreviousLoc, true));

  return makeParserResult(
      Status,
      new (Context) ForStmt(LabelInfo, ForLoc, First.getPtrOrNull(),
                            FirstDeclsContext,
                            Semi1Loc, Second.getPtrOrNull(), Semi2Loc,
                            Third.getPtrOrNull(), Body.get()));
}

/// 
///   stmt-for-each:
///     (identifier ':')? 'for' pattern 'in' expr-basic stmt-brace
ParserResult<Stmt> Parser::parseStmtForEach(SourceLoc ForLoc,
                                            LabeledStmtInfo LabelInfo) {
  ParserResult<Pattern> Pattern = parsePattern(/*isLet*/true);
  if (Pattern.isNull())
    // Recover by creating a "_" pattern.
    Pattern = makeParserErrorResult(new (Context) AnyPattern(SourceLoc()));

  // Bound variables all get their initial values from the generator.
  Pattern.get()->markHasNonPatternBindingInit();
  
  SourceLoc InLoc;
  parseToken(tok::kw_in, InLoc, diag::expected_foreach_in);

  ParserPosition ContainerStartState;
  if (Tok.is(tok::l_brace)) {
    // It is unusual for the container expression to start with a left brace,
    // and we anticipate the need to do recovery.  Save the parser state so
    // that we can rewind.
    ContainerStartState = getParserPosition();
  }

  ParserResult<Expr> Container =
      parseExprBasic(diag::expected_foreach_container);
  if (Container.hasCodeCompletion())
    return makeParserCodeCompletionResult<Stmt>();
  if (Container.isNull())
    Container = makeParserErrorResult(new (Context) ErrorExpr(Tok.getLoc()));

  if (auto *CE = dyn_cast<ClosureExpr>(Container.get())) {
    diagnose(CE->getStartLoc(), diag::expected_foreach_container);

    // If the container expression turns out to be a closure, then it was not
    // the container expression, but the 'for' statement body.  We can not have
    // a bare closure as a container expression because closures don't conform
    // to Sequence.
    Container =
        makeParserErrorResult(new (Context) ErrorExpr(CE->getStartLoc()));

    // Backtrack to the '{' so that we can re-parse the body in the correct
    // lexical scope.
    backtrackToPosition(ContainerStartState);
  }

  // Introduce a new scope and place the variables in the pattern into that
  // scope.
  // FIXME: We may want to merge this scope with the scope introduced by
  // the stmt-brace, as in C++.
  Scope S(this, ScopeKind::ForeachVars);
  
  // Introduce variables to the current scope.
  addPatternVariablesToScope(Pattern.get());

  ParserStatus Status;

  // stmt-brace
  ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_foreach_lbrace);
  Status |= Body;
  if (Body.isNull())
    Body = makeParserResult(
        Body, BraceStmt::create(Context, ForLoc, {}, PreviousLoc, true));

  return makeParserResult(
      Status,
      new (Context) ForEachStmt(LabelInfo, ForLoc, Pattern.get(), InLoc,
                                Container.get(), Body.get()));
}

///
///    stmt-switch:
///      (identifier ':')? 'switch' expr-basic '{' stmt-case+ '}'
ParserResult<Stmt> Parser::parseStmtSwitch(LabeledStmtInfo LabelInfo) {
  SourceLoc SwitchLoc = consumeToken(tok::kw_switch);

  bool SubjectStartsWithLBrace = Tok.is(tok::l_brace);
  ParserPosition SubjectStartState;
  if (SubjectStartsWithLBrace) {
    // It is unusual for the subject expression to start with a left brace, and
    // we anticipate the need to do recovery.  Save the parser state so that we
    // can rewind.
    SubjectStartState = getParserPosition();
  }

  ParserResult<Expr> SubjectExpr = parseExprBasic(diag::expected_switch_expr);
  if (SubjectExpr.hasCodeCompletion())
    return makeParserCodeCompletionResult<Stmt>();

  if (!Tok.is(tok::l_brace)) {
    if (!SubjectStartsWithLBrace) {
      diagnose(Tok, diag::expected_lbrace_after_switch);
      return nullptr;
    }

    diagnose(SwitchLoc, diag::expected_switch_expr);

    // We are going to reparse what we parsed as subject expr.
    SubjectExpr = nullptr;

    // Backtrack to the '{' so that we can re-parse the switch body correctly.
    //
    // FIXME: Even though we are going to re-parse the body, we have already
    // emitted errors about 'case' outside of switch, when we were parsing this
    // as a subject expr.
    backtrackToPosition(SubjectStartState);
  }

  if (SubjectExpr.isNull())
    SubjectExpr = makeParserErrorResult(new (Context) ErrorExpr(Tok.getLoc()));

  SourceLoc lBraceLoc = consumeToken(tok::l_brace);
  SourceLoc rBraceLoc;
  
  // Reject an empty 'switch'.
  if (Tok.is(tok::r_brace))
    diagnose(Tok.getLoc(), diag::empty_switch_stmt);

  ParserStatus Status;

  // If there are non-case-label statements at the start of the switch body,
  // raise an error and recover by parsing and discarding them.
  bool DiagnosedNotCoveredStmt = false;
  bool ErrorAtNotCoveredStmt = false;
  while (!Tok.is(tok::kw_case) && !Tok.is(tok::kw_default)
         && !Tok.is(tok::r_brace) && !Tok.is(tok::eof)) {
    if (ErrorAtNotCoveredStmt) {
      // Error recovery.
      consumeToken();
      continue;
    }
    if (!DiagnosedNotCoveredStmt) {
      diagnose(Tok, diag::stmt_in_switch_not_covered_by_case);
      DiagnosedNotCoveredStmt = true;
    }
    ASTNode NotCoveredStmt;
    ParserStatus CurrStat = parseExprOrStmt(NotCoveredStmt);
    if (CurrStat.isError())
      ErrorAtNotCoveredStmt = true;
    Status |= CurrStat;
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

  return makeParserResult(
      Status, SwitchStmt::create(LabelInfo, SwitchLoc, SubjectExpr.get(),
                                 lBraceLoc, cases, rBraceLoc, Context));
}

namespace {
class CollectVarsAndAddToScope : public ASTWalker {
public:
  Parser &TheParser;
  SmallVectorImpl<VarDecl*> &Decls;

  CollectVarsAndAddToScope(Parser &P, SmallVectorImpl<VarDecl*> &Decls)
      : TheParser(P), Decls(Decls) {}

  Pattern *walkToPatternPost(Pattern *P) override {
    // Handle vars.
    if (auto *Named = dyn_cast<NamedPattern>(P)) {
      VarDecl *VD = Named->getDecl();
      Decls.push_back(VD);
      TheParser.addToScope(VD);
    }
    return P;
  }
};
} // unnamed namespace

static ParserStatus parseStmtCase(Parser &P, SourceLoc &CaseLoc,
                                  SmallVectorImpl<CaseLabelItem> &LabelItems,
                                  SmallVectorImpl<VarDecl *> &BoundDecls,
                                  SourceLoc &ColonLoc) {
  ParserStatus Status;

  CaseLoc = P.consumeToken(tok::kw_case);

  do {
    ParserResult<Pattern> CasePattern;
    if (P.CodeCompletion) {
      if (P.Tok.is(tok::code_complete)) {
        CasePattern =
            makeParserErrorResult(new (P.Context) AnyPattern(SourceLoc()));
        P.CodeCompletion->completeCaseStmtBeginning();
        P.consumeToken();
      }
      if (P.Tok.is(tok::period) && P.peekToken().is(tok::code_complete)) {
        CasePattern =
            makeParserErrorResult(new (P.Context) AnyPattern(SourceLoc()));
        P.consumeToken();
        P.CodeCompletion->completeCaseStmtDotPrefix();
        P.consumeToken();
      }
    }

    if (CasePattern.isNull())
      CasePattern = P.parseMatchingPattern();

    if (CasePattern.isNull())
      CasePattern =
          makeParserErrorResult(new (P.Context) AnyPattern(P.PreviousLoc));

    Status |= CasePattern;
    if (CasePattern.isNonNull()) {
      // Add variable bindings from the pattern to the case scope.  We have
      // to do this with a full AST walk, because the freshly parsed pattern
      // represents tuples and var patterns as tupleexprs and
      // unresolved_pattern_expr nodes, instead of as proper pattern nodes.
      CasePattern.get()->walk(CollectVarsAndAddToScope(P, BoundDecls));
      
      // Now that we have them, mark them as being initialized without a PBD.
      for (auto VD : BoundDecls)
        VD->setHasNonPatternBindingInit();
    }

    // Parse an optional 'where' guard.
    SourceLoc WhereLoc;
    ParserResult<Expr> Guard;
    if (P.Tok.is(tok::kw_where)) {
      WhereLoc = P.consumeToken(tok::kw_where);
      Guard = P.parseExpr(diag::expected_case_where_expr);
      Status |= Guard;
    }

    LabelItems.push_back(CaseLabelItem(/*IsDefault=*/false,
                                       CasePattern.get(), WhereLoc,
                                       Guard.getPtrOrNull()));
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

  // Case blocks with multiple patterns cannot bind variables.
  if (!BoundDecls.empty() && CaseLabelItems.size() > 1)
    diagnose(BoundDecls[0]->getLoc(),
             diag::var_binding_with_multiple_case_patterns);

  SmallVector<ASTNode, 8> BodyItems;

  SourceLoc StartOfBody = Tok.getLoc();
  if (Tok.isNot(tok::kw_case) && Tok.isNot(tok::kw_default) &&
      Tok.isNot(tok::r_brace)) {
    Status |= parseBraceItems(BodyItems, BraceItemListKind::Case);
  } else if (Status.isSuccess()) {
    diagnose(CaseLoc, diag::case_stmt_without_body,
             CaseLabelItems.back().isDefault())
        .highlight(SourceRange(CaseLoc, ColonLoc));
  }
  BraceStmt *Body;
  if (BodyItems.empty()) {
    Body = BraceStmt::create(Context, PreviousLoc, ArrayRef<ASTNode>(),
                             PreviousLoc, /*implicit=*/true);
  } else {
    Body = BraceStmt::create(Context, StartOfBody, BodyItems, PreviousLoc);
  }

  return makeParserResult(
      Status, CaseStmt::create(Context, CaseLoc, CaseLabelItems,
                               !BoundDecls.empty(), ColonLoc, Body));
}

