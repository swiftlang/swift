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

  auto parseTopLevelExpr = [&](Diag<> ID) {
    return (Tok.is(tok::kw_throw) ? parseExprThrow(ID) : parseExpr(ID));
  };

  ParserResult<Expr> ResultExpr = parseTopLevelExpr(diag::expected_expr);
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
    initScope.emplace(this, scopeKind,
                      ConfigKind == BraceItemListKind::InactiveConfigBlock);
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
  case tok::kw_defer:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtDefer();
  case tok::kw_if:
    return parseStmtIf(LabelInfo);
  case tok::kw_guard:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtGuard();
  case tok::pound_if:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseStmtIfConfig();
  case tok::pound_line:
    if (LabelInfo) diagnose(LabelInfo.Loc, diag::invalid_label_on_stmt);
    return parseLineDirective();
  case tok::kw_while:  return parseStmtWhile(LabelInfo);
  case tok::kw_repeat: return parseStmtRepeat(LabelInfo);
  case tok::kw_do:     return parseStmtDo(LabelInfo);
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
  //    @noescape let tmpClosure = { body }
  //    tmpClosure()   // This is emitted on each path that needs to run this.
  //
  // As such, the body of the 'defer' is actually type checked within the
  // closure's DeclContext.
  unsigned discriminator = CurLocalContext->claimNextClosureDiscriminator();
  auto params = TuplePattern::create(Context, SourceLoc(), {}, SourceLoc());
  
  auto closure
    = new (Context) ClosureExpr(params, /*throwsLoc*/SourceLoc(),
                                /*arrowLoc*/SourceLoc(), /*inLoc*/SourceLoc(),
                                /*resultType*/TypeLoc(), discriminator,
                                CurDeclContext);
  closure->setIsDeferBody();
  
  ParserStatus Status;
  {
    // Change the DeclContext for any variables declared in the defer to be within
    // the defer closure.
    ParseFunctionBody cc(*this, closure);
    
    ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_lbrace_after_defer);
    if (Body.isNull())
      return nullptr;
    Status |= Body;
    closure->setBody(Body.get(), /*hasSingleExprBody*/false);
  }
  
  SourceLoc loc = closure->getBody()->getStartLoc();
  
  // Create the tmpClosure variable and pattern binding.
  auto tempDecl = new (Context) VarDecl(/*static*/ false, /*let*/ true,
                                        loc,Context.getIdentifier("tmpClosure"),
                                        Type(), CurDeclContext);
  tempDecl->setImplicit(true);
  auto bindingPattern = new (Context) NamedPattern(tempDecl, /*implicit*/true);
  
  // The type of the closure is forced to be "@noescape () -> ()".
  auto voidTy = TupleType::getEmpty(Context);
  auto closureTy =
    FunctionType::get(voidTy, voidTy, FunctionType::ExtInfo().withNoEscape());
  
  auto typePattern = new (Context) TypedPattern(bindingPattern,
                                                TypeLoc::withoutLoc(closureTy),
                                                /*implicit*/true);
  auto bindingDecl = PatternBindingDecl::create(Context, /*static*/ SourceLoc(),
                                                StaticSpellingKind::None, loc,
                                                typePattern, closure,
                                                CurDeclContext);
  bindingDecl->setImplicit(true);
  
  // Form the call, which will be emitted on any path that needs to run the
  // code.
  auto DRE = new (Context) DeclRefExpr(tempDecl, loc, /*Implicit*/true,
                                       AccessSemantics::DirectToStorage);
  auto args = TupleExpr::createEmpty(Context, loc, loc, true);
  auto call = new (Context) CallExpr(DRE, args, /*implicit*/true);
  
  auto DS = new (Context) DeferStmt(DeferLoc, bindingDecl, tempDecl, call);
  return makeParserResult(Status, DS);
}

namespace {
  struct GuardedPattern {
    Pattern *ThePattern = nullptr;
    SourceLoc WhereLoc;
    Expr *Guard = nullptr;
  };
  
  /// Contexts in which a guarded pattern can appears.
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
                                GuardedPatternContext parsingContext) {
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
    if (P.CodeCompletion) {
      setErrorResult();
      switch (parsingContext) {
      case GuardedPatternContext::Case:
        P.CodeCompletion->completeCaseStmtBeginning();
        break;
      case GuardedPatternContext::Catch:
        P.CodeCompletion->completeCatchStmtBeginning();
        break;
      }
      P.consumeToken();
    } else {
      status.setHasCodeCompletion();
      return;
    }
  }
  if (parsingContext == GuardedPatternContext::Case &&
      P.Tok.is(tok::period) && P.peekToken().is(tok::code_complete)) {
    if (P.CodeCompletion) {
      setErrorResult();
      P.consumeToken();
      P.CodeCompletion->completeCaseStmtDotPrefix();
      P.consumeToken();
    } else {
      status.setHasCodeCompletion();
      return;
    }
  }

  // If this is a 'catch' clause and we have "catch {" or "catch where...",
  // then we get an implicit "let error" pattern.
  if (parsingContext == GuardedPatternContext::Catch &&
      P.Tok.isAny(tok::l_brace, tok::kw_where)) {
    auto loc = P.Tok.getLoc();
    auto errorName = P.Context.getIdentifier("error");
    auto var = new (P.Context) VarDecl(/*static*/ false, /*IsLet*/true,
                                       loc, errorName, Type(),
                                       P.CurDeclContext);
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

  // Add variable bindings from the pattern to the case scope.  We have
  // to do this with a full AST walk, because the freshly parsed pattern
  // represents tuples and var patterns as tupleexprs and
  // unresolved_pattern_expr nodes, instead of as proper pattern nodes.
  patternResult.get()->forEachVariable([&](VarDecl *VD) {
    if (VD->hasName()) P.addToScope(VD);
    boundDecls.push_back(VD);
  });
  
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

/// Parse the condition of an 'if' or 'while'.
///
///   condition:
///     expr-basic
///     expr-basic ',' conditional-binding (',' conditional-binding)*
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
  // Parse the leading boolean condition if present.
  if (Tok.isNot(tok::kw_var, tok::kw_let, tok::kw_case)) {
    ParserResult<Expr> CondExpr = parseExprBasic(ID);
    Status |= CondExpr;
    result.push_back(CondExpr.getPtrOrNull());
    
    // If there is a comma after the expression, parse a list of let/var
    // bindings.
    SourceLoc CommaLoc = Tok.getLoc();
    if (!consumeIf(tok::comma)) {
      Condition = Context.AllocateCopy(result);
      return Status;
    }
    
    // If a let-binding doesn't follow, diagnose the problem with a tailored
    // error message.
    if (Tok.isNot(tok::kw_var, tok::kw_let, tok::kw_case)) {
      // If an { exists after the comma, assume it is a stray comma and this is
      // the start of the if/while body.  If a non-expression thing exists after
      // the comma, then we don't know what is going on.
      if (Tok.is(tok::l_brace) || isStartOfDecl() || isStartOfStmt()) {
        diagnose(Tok, diag::expected_expr_conditional_letbinding);
        Condition = Context.AllocateCopy(result);
        if (Tok.isNot(tok::l_brace)) Status.setIsParseError();
        return Status;
      }
     
      // If an expression follows the comma, then it is a second boolean
      // condition.  Produce a fix-it hint to rewrite the comma to &&.
      diagnose(CommaLoc,
               diag::expected_expr_conditional_letbinding_bool_conditions)
        .fixItReplace(CommaLoc, " &&");
      do {
        ParserResult<Expr> CondExpr = parseExprBasic(ID);
        Status |= CondExpr;
        result.push_back(CondExpr.getPtrOrNull());
      } while (consumeIf(tok::comma) &&
               Tok.isNot(tok::kw_var, tok::kw_let));
      
      if (Tok.isNot(tok::kw_var, tok::kw_let)) {
        Condition = Context.AllocateCopy(result);
        return Status;
      }
    }
  }

  // We're parsing a conditional binding.
  assert(CurDeclContext->isLocalContext() &&
         "conditional binding in non-local context?!");

  // For error recovery purposes, keep track of the disposition of the last
  // pattern binding we saw ('let' vs 'var') in multiple PBD cases.
  enum BK_BindingKind {
    BK_Let, BK_Var, BK_Case, BK_LetCase, BK_VarCase
  } BindingKind = BK_Let;
  StringRef BindingKindStr = "let";
 
  // Parse the list of condition-bindings, each of which can have a 'where'.
  do {
    SourceLoc VarLoc;
    
    if (Tok.isAny(tok::kw_let, tok::kw_var, tok::kw_case)) {
      BindingKind =
        Tok.is(tok::kw_let) ? BK_Let : Tok.is(tok::kw_var) ? BK_Var : BK_Case;
      BindingKindStr = Tok.getText();
      VarLoc = consumeToken();

      // If will probably be a common typo to write "if let case" instead of
      // "if case let" so detect this and produce a nice fixit.
      if ((BindingKind == BK_Let || BindingKind == BK_Var) &&
          Tok.is(tok::kw_case)) {
        diagnose(VarLoc, diag::wrong_condition_case_location, BindingKindStr)
          .fixItRemove(VarLoc)
          .fixItInsertAfter(Tok.getLoc(), " " + BindingKindStr.str());

        BindingKindStr = "case";
        BindingKind = BindingKind == BK_Let ? BK_LetCase : BK_VarCase;
        VarLoc = consumeToken(tok::kw_case);
      }

    } else {
      // We get here with erroneous code like:
      //    if let x = foo() where cond(), y? = bar()
      // which is a common typo for:
      //    if let x = foo() where cond(),
      //       LET y? = bar()
      // diagnose this specifically and produce a nice fixit.
      diagnose(Tok, diag::where_end_of_binding_use_letvar, BindingKindStr)
        .fixItInsert(Tok.getLoc(), BindingKindStr.str() + " ");
      VarLoc = Tok.getLoc();
    }

    // The first pattern entry we parse will record the location of the
    // let/var/case into the StmtCondition.
    SourceLoc IntroducerLoc = VarLoc;

    // Parse the list of name bindings within a let/var clauses.
    do {
      ParserResult<Pattern> ThePattern;

      if (BindingKind == BK_Case) {
        // In our recursive parse, remember that we're in a matching pattern.
        llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
          T(InVarOrLetPattern, IVOLP_InMatchingPattern);
        ThePattern = parseMatchingPattern(/*isExprBasic*/ true);
      } else if (BindingKind == BK_LetCase || BindingKind == BK_VarCase) {
        // Recover from the 'if let case' typo gracefully.

        // In our recursive parse, remember that we're in a var/let pattern.
        llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
        T(InVarOrLetPattern,
          BindingKind == BK_LetCase ? IVOLP_InLet : IVOLP_InVar);
        ThePattern = parseMatchingPattern(/*isExprBasic*/ true);

        if (ThePattern.isNonNull()) {
          auto *P = new (Context) VarPattern(VarLoc, BindingKind == BK_LetCase,
                                             ThePattern.get(), /*impl*/false);
          ThePattern = makeParserResult(P);
        }
      } else {
        // Otherwise, this is an implicit optional binding "if let".
        ThePattern =
          parseMatchingPatternAsLetOrVar(BindingKind == BK_Let, VarLoc,
                                         /*isExprBasic*/ true);
        // The let/var pattern is part of the statement.
        if (Pattern *P = ThePattern.getPtrOrNull())
          P->setImplicit();
      }

      ThePattern = parseOptionalPatternTypeAnnotation(ThePattern,
                                                      BindingKind != BK_Case);
      Status |= ThePattern;

      if (ThePattern.isNull() || ThePattern.hasCodeCompletion())
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

      result.push_back({IntroducerLoc, ThePattern.get(), Init});
      IntroducerLoc = SourceLoc();

      // Add variable bindings from the pattern to our current scope and mark
      // them as being having a non-pattern-binding initializer.
      ThePattern.get()->forEachVariable([&](VarDecl *VD) {
        if (VD->hasName())
          addToScope(VD);
        VD->setHasNonPatternBindingInit();
      });

    } while (Tok.is(tok::comma) &&
             peekToken().isNot(tok::kw_let, tok::kw_var, tok::kw_case) &&
             consumeIf(tok::comma));

    // If there is a where clause on this let/var specification, parse and
    // remember it.
    if (consumeIf(tok::kw_where)) {
      ParserResult<Expr> WhereExpr
        = parseExprBasic(diag::expected_expr_conditional_where);
      Status |= WhereExpr;
      if (WhereExpr.isNull() || WhereExpr.hasCodeCompletion())
        return Status;
      result.push_back(WhereExpr.get());
    }

  } while (consumeIf(tok::comma));

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
  
    if (Tok.is(tok::l_brace)) {
      SourceLoc LBraceLoc = Tok.getLoc();
      diagnose(IfLoc, diag::missing_condition_after_if)
        .highlight(SourceRange(IfLoc, LBraceLoc));
      SmallVector<StmtConditionElement, 1> ConditionElems;
      ConditionElems.emplace_back(new (Context) ErrorExpr(LBraceLoc));
      Condition = Context.AllocateCopy(ConditionElems);
    } else {
      Status |= parseStmtCondition(Condition, diag::expected_condition_if);
      if (Status.isError() || Status.hasCodeCompletion()) {
        // FIXME: better recovery
        return makeParserResult<Stmt>(Status, nullptr);
      }
    }

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
  
  if (Tok.is(tok::l_brace)) {
    SourceLoc LBraceLoc = Tok.getLoc();
    diagnose(GuardLoc, diag::missing_condition_after_guard)
      .highlight(SourceRange(GuardLoc, LBraceLoc));
    SmallVector<StmtConditionElement, 1> ConditionElems;
    ConditionElems.emplace_back(new (Context) ErrorExpr(LBraceLoc));
    Condition = Context.AllocateCopy(ConditionElems);
  } else {
    Status |= parseStmtCondition(Condition, diag::expected_condition_guard);
    if (Status.isError() || Status.hasCodeCompletion()) {
      // FIXME: better recovery
      return makeParserResult<Stmt>(Status, nullptr);
    }
  }

  // Parse the 'else'.  If it is missing, and if the following token isn't a {
  // then the parser is hopelessly lost - just give up instead of spewing.
  if (parseToken(tok::kw_else, diag::expected_else_after_guard) &&
      Tok.isNot(tok::l_brace))
    return makeParserError();

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
    return nullptr; // FIXME: better recovery
  
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
    
    if (!Tok.isAtStartOfLine() && Tok.isNot(tok::eof))
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
  } else if (!Tok.isAtStartOfLine() && Tok.isNot(tok::eof))
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

  if (Tok.is(tok::l_brace)) {
    SourceLoc LBraceLoc = Tok.getLoc();
    diagnose(WhileLoc, diag::missing_condition_after_while)
      .highlight(SourceRange(WhileLoc, LBraceLoc));
    SmallVector<StmtConditionElement, 1> ConditionElems;
    ConditionElems.emplace_back(new (Context) ErrorExpr(LBraceLoc));
    Condition = Context.AllocateCopy(ConditionElems);
  } else {
    Status |= parseStmtCondition(Condition, diag::expected_condition_while);
    if (Status.isError() || Status.hasCodeCompletion()) {
      // FIXME: better recovery
      return makeParserResult<Stmt>(Status, nullptr);
    }
  }

  ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_lbrace_after_while);
  if (Body.isNull())
    return nullptr; // FIXME: better recovery

  Status |= Body;

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
  if (status.hasCodeCompletion())
    return makeParserResult<Stmt>(status, nullptr);
  if (body.isNull())
    body = makeParserResult(
        body, BraceStmt::create(Context, repeatLoc, {}, PreviousLoc, true));

  SourceLoc whileLoc;

  if (!consumeIf(tok::kw_while, whileLoc)) {
    diagnose(whileLoc, diag::expected_while_after_repeat_body);
    return makeParserError();
  }

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
  if (status.hasCodeCompletion())
    return makeParserResult<Stmt>(status, nullptr);
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
      if (status.hasCodeCompletion())
        return makeParserResult<Stmt>(status, nullptr);

      // parseStmtCatch promises to return non-null.
      assert(!clause.isNull());
      allClauses.push_back(clause.get());
    } while (Tok.is(tok::kw_catch));

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
/// a code-completion token.
ParserResult<CatchStmt> Parser::parseStmtCatch() {
  // A catch block has its own scope for variables bound out of the pattern.
  Scope S(this, ScopeKind::CatchVars);

  SourceLoc catchLoc = consumeToken(tok::kw_catch);

  SmallVector<VarDecl*, 4> boundDecls;

  ParserStatus status;
  GuardedPattern pattern;
  parseGuardedPattern(*this, pattern, status, boundDecls,
                      GuardedPatternContext::Catch);
  if (status.hasCodeCompletion()) {
    return makeParserCodeCompletionResult<CatchStmt>();
  }

  SourceLoc startOfBody = Tok.getLoc();
  auto bodyResult = parseBraceItemList(diag::expected_lbrace_after_catch);
  status |= bodyResult;
  if (status.hasCodeCompletion()) {
    return makeParserCodeCompletionResult<CatchStmt>();
  } else if (bodyResult.isNull()) {
    bodyResult = makeParserErrorResult(BraceStmt::create(Context, startOfBody,
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
    bool FoundCCToken;
    parseDeclAttributeList(Attributes, FoundCCToken);

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
///     (identifier ':')? 'for' pattern 'in' expr-basic \
///             ('where' expr-basic)? stmt-brace
ParserResult<Stmt> Parser::parseStmtForEach(SourceLoc ForLoc,
                                            LabeledStmtInfo LabelInfo) {
  ParserStatus Status;

  // Change the parser state to know that the pattern we're about to parse is
  // implicitly mutable.  Bound variables can be changed to mutable explicitly
  // if desired by using a 'var' pattern.
  assert(InVarOrLetPattern == IVOLP_NotInVarOrLet &&
         "for-each loops cannot exist inside other patterns");
  InVarOrLetPattern = IVOLP_ImplicitlyImmutable;
  ParserResult<Pattern> Pattern = parseTypedPattern();
  assert(InVarOrLetPattern == IVOLP_ImplicitlyImmutable);
  InVarOrLetPattern = IVOLP_NotInVarOrLet;
  
  
  if (Pattern.isNull())
    // Recover by creating a "_" pattern.
    Pattern = makeParserErrorResult(new (Context) AnyPattern(SourceLoc()));

  // Bound variables all get their initial values from the generator.
  Pattern.get()->markHasNonPatternBindingInit();
  
  SourceLoc InLoc;
  parseToken(tok::kw_in, InLoc, diag::expected_foreach_in);

  ParserResult<Expr> Container;
  if (Tok.is(tok::l_brace)) {
    SourceLoc LBraceLoc = Tok.getLoc();
    diagnose(LBraceLoc, diag::expected_foreach_container);
    Container = makeParserErrorResult(new (Context) ErrorExpr(LBraceLoc));
  } else {
    Container = parseExprBasic(diag::expected_foreach_container);
    if (Container.hasCodeCompletion())
      return makeParserCodeCompletionResult<Stmt>();
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
  addPatternVariablesToScope(Pattern.get());

  // Parse the 'where' expression if present.
  ParserResult<Expr> Where;
  if (consumeIf(tok::kw_where)) {
    Where = parseExprBasic(diag::expected_foreach_where_expr);
    if (Where.hasCodeCompletion())
      return makeParserCodeCompletionResult<Stmt>();
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
      new (Context) ForEachStmt(LabelInfo, ForLoc, Pattern.get(), InLoc,
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
  
  // Reject an empty 'switch'.
  if (Tok.is(tok::r_brace))
    diagnose(Tok.getLoc(), diag::empty_switch_stmt);

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

static ParserStatus parseStmtCase(Parser &P, SourceLoc &CaseLoc,
                                  SmallVectorImpl<CaseLabelItem> &LabelItems,
                                  SmallVectorImpl<VarDecl *> &BoundDecls,
                                  SourceLoc &ColonLoc) {
  ParserStatus Status;

  CaseLoc = P.consumeToken(tok::kw_case);

  do {
    GuardedPattern PatternResult;
    parseGuardedPattern(P, PatternResult, Status, BoundDecls,
                        GuardedPatternContext::Case);
    LabelItems.push_back(CaseLabelItem(/*IsDefault=*/false,
                                       PatternResult.ThePattern,
                                       PatternResult.WhereLoc,
                                       PatternResult.Guard));
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
    Body = BraceStmt::create(Context, StartOfBody, BodyItems,
                             PreviousLoc, /*implicit=*/true);
  }

  return makeParserResult(
      Status, CaseStmt::create(Context, CaseLoc, CaseLabelItems,
                               !BoundDecls.empty(), ColonLoc, Body));
}

