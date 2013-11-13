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
#include "swift/Parse/Lexer.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

/// isStartOfStmt - Return true if the specified token starts
/// a statement.
///
bool Parser::isStartOfStmt(const Token &Tok) {
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
    return true;
  }
}

/// isStartOfDecl - Return true if this is the start of a decl or decl-import.
bool Parser::isStartOfDecl(const Token &Tok, const Token &Tok2) {
  switch (Tok.getKind()) {
  case tok::at_sign:
  case tok::kw_static:
  case tok::kw_extension:
  case tok::kw_var:
  case tok::kw_typealias:
  case tok::kw_enum:
  case tok::kw_case:
  case tok::kw_struct:
  case tok::kw_class:
  case tok::kw_import:
  case tok::kw_subscript:
  case tok::kw_init:
  case tok::kw_destructor:
  case tok::kw_def:
    return true;
  case tok::kw_protocol:
    return !(Tok2.isAnyOperator() && Tok2.getText().equals("<"));
  default:
    return isStartOfOperatorDecl(Tok, Tok2);
  }
}

ParserStatus Parser::parseExprOrStmt(ASTNode &Result) {
  if (Tok.is(tok::semi)) {
    diagnose(Tok, diag::illegal_semi_stmt)
      .fixItRemove(SourceRange(Tok.getLoc()));
    consumeToken();
    return makeParserError();
  }
  if (isStartOfStmt(Tok)) {
    ParserResult<Stmt> Res = parseStmt();
    if (Res.isNonNull())
      Result = Res.get();
    return Res;
  }

  if (CodeCompletion)
    CodeCompletion->setExprBeginning(getParserPosition());

  ParserResult<Expr> ResultExpr = parseExpr(diag::expected_expr,
                                            /*usesExprBasic*/ false);
  if (ResultExpr.hasCodeCompletion() && CodeCompletion) {
    CodeCompletion->completeExpr();
    return ResultExpr;
  }

  if (ResultExpr.isNonNull())
    Result = ResultExpr.get();

  return ResultExpr;
}

static bool isTerminatorForBraceItemListKind(const Token &Tok,
                                             BraceItemListKind Kind,
                                             ArrayRef<ASTNode> ParsedDecls) {
  switch (Kind) {
  case BraceItemListKind::Brace:
    return false;
  case BraceItemListKind::Variable:
    return Tok.isContextualKeyword("get") || Tok.isContextualKeyword("set") ||
           Tok.is(tok::at_sign); // Attribute on get/set specifier.
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

static void unwrapIfDiscardedClosure(Parser &P,
                                     ASTNode &Result) {
  // If we parsed a bare closure as an expression, it will be a discarded value
  // expression and the type checker will complain.
  //
  // Instead, recover by unwrapping the BraceStmt that is contained inside.

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
      auto *BS = CE->getBody();
      Result = BS;
      P.diagnose(BS->getLBraceLoc(), diag::brace_stmt_invalid);
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
                                     BraceItemListKind Kind) {
  bool IsTopLevel = (Kind == BraceItemListKind::TopLevelCode) ||
                    (Kind == BraceItemListKind::TopLevelLibrary);

  // This forms a lexical scope.
  Scope S(this, IsTopLevel ? ScopeKind::TopLevel : ScopeKind::Brace);

  ParserStatus BraceItemsStatus;
  SmallVector<Decl*, 8> TmpDecls;

  bool PreviousHadSemi = true;
  while ((Kind == BraceItemListKind::TopLevelLibrary ||
          Tok.isNot(tok::r_brace)) &&
         Tok.isNot(tok::eof) &&
         Tok.isNot(tok::kw_sil) && Tok.isNot(tok::kw_sil_stage) &&
         Tok.isNot(tok::kw_sil_vtable) &&
         !isTerminatorForBraceItemListKind(Tok, Kind, Entries)) {
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
    if (isStartOfDecl(Tok, peekToken())) {
      ParserStatus Status =
          parseDecl(TmpDecls, IsTopLevel ? PD_AllowTopLevel : PD_Default);
      if (Status.isError()) {
        NeedParseErrorRecovery = true;
        if (Status.hasCodeCompletion() && IsTopLevel &&
            isCodeCompletionFirstPass()) {
          consumeDecl(BeginParserPosition, 0U, IsTopLevel);
          return Status;
        }
      }

      for (Decl *D : TmpDecls)
        Entries.push_back(D);
      if (!TmpDecls.empty())
        PreviousHadSemi = TmpDecls.back()->TrailingSemiLoc.isValid();
      TmpDecls.clear();
    } else if (IsTopLevel && allowTopLevelCode()) {
      // If this is a statement or expression at the top level of the module,
      // Parse it as a child of a TopLevelCodeDecl.
      auto *TLCD = new (Context) TopLevelCodeDecl(CurDeclContext);
      ContextChange CC(*this, TLCD);
      SourceLoc StartLoc = Tok.getLoc();

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
      unwrapIfDiscardedClosure(*this, Result);
      if (!Result.isNull()) {
        auto Brace = BraceStmt::create(Context, StartLoc, Result, Tok.getLoc());
        TLCD->setBody(Brace);
        Entries.push_back(TLCD);
      }
    } else {
      SourceLoc StartLoc = Tok.getLoc();
      ParserStatus ExprOrStmtStatus = parseExprOrStmt(Result);
      BraceItemsStatus |= ExprOrStmtStatus;
      if (ExprOrStmtStatus.isError())
        NeedParseErrorRecovery = true;
      unwrapIfDiscardedClosure(*this, Result);
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
      if (Result.is<Expr*>()) {
        Result.get<Expr*>()->TrailingSemiLoc = consumeToken(tok::semi);
      } else {
        Result.get<Stmt*>()->TrailingSemiLoc = consumeToken(tok::semi);
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
  auto *TLCD = cast<TopLevelCodeDecl>(DelayedState->ParentContext);
  ContextChange CC(*this, TLCD);

  SourceLoc StartLoc = Tok.getLoc();
  ASTNode Result;
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
  switch (Tok.getKind()) {
  default:
    diagnose(Tok, diag::expected_stmt);
    return nullptr;
  case tok::kw_return: return parseStmtReturn();
  case tok::kw_if:     return parseStmtIf();
  case tok::kw_while:  return parseStmtWhile();
  case tok::kw_do:     return parseStmtDoWhile();
  case tok::kw_for:    return parseStmtFor();
  case tok::kw_switch: return parseStmtSwitch();
  /// 'case' and 'default' are only valid at the top level of a switch.
  case tok::kw_case:
  case tok::kw_default: return recoverFromInvalidCase(*this);
  case tok::kw_break:
    return makeParserResult(
        new (Context) BreakStmt(consumeToken(tok::kw_break)));
  case tok::kw_continue:
    return makeParserResult(
        new (Context) ContinueStmt(consumeToken(tok::kw_continue)));
  case tok::kw_fallthrough:
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
  if (parseMatchingToken(tok::r_brace, RBLoc,
                         diag::expected_rbrace_in_brace_stmt, LBLoc)) {
    RBLoc = PreviousLoc;
  }

  return makeParserResult(Status,
                          BraceStmt::create(Context, LBLoc, Entries, RBLoc));
}

/// parseStmtReturn
///
///   stmt-return:
///     return expr?
///   
ParserResult<Stmt> Parser::parseStmtReturn() {
  SourceLoc ReturnLoc = consumeToken(tok::kw_return);

  // Handle the ambiguity between consuming the expression and allowing the
  // enclosing stmt-brace to get it by eagerly eating it unless the return is
  // followed by a '}', ';', statement or decl start keyword sequence.
  if (Tok.isNot(tok::r_brace) && Tok.isNot(tok::semi) &&
      !isStartOfStmt(Tok) && !isStartOfDecl(Tok, peekToken())) {
    ParserResult<Expr> Result = parseExpr(diag::expected_expr_return);
    if (Result.isNull()) {
      // Create an ErrorExpr to tell the type checker that this return
      // statement had an expression argument in the source.  This supresses
      // the error about missing return value in a non-void function.
      Result = makeParserErrorResult(new (Context) ErrorExpr(Tok.getLoc()));
    }
    return makeParserResult(
        Result, new (Context) ReturnStmt(ReturnLoc, Result.getPtrOrNull()));
  }

  return makeParserResult(new (Context) ReturnStmt(ReturnLoc, nullptr));
}

/// 
///   stmt-if:
///     'if' expr-basic stmt-brace stmt-if-else?
///   stmt-if-else:
///    'else' stmt-brace
///    'else' stmt-if
ParserResult<Stmt> Parser::parseStmtIf() {
  SourceLoc IfLoc = consumeToken(tok::kw_if);

  ParserStatus Status;

  ParserResult<Expr> Condition = parseExprBasic(diag::expected_expr_if);
  Status |= Condition;
  if (Condition.isNull() || Condition.hasCodeCompletion())
    return makeParserResult<Stmt>(Condition, nullptr); // FIXME: better recovery

  ParserResult<BraceStmt> NormalBody;
  if (auto *CE = dyn_cast<ClosureExpr>(Condition.get())) {
    // If we parsed closure after 'if', then it was not the condition, but the
    // 'if' statement body.  We can not have a bare closure in an 'if'
    // condition because closures don't conform to LogicValue.
    auto ClosureBody = CE->getBody();
    SourceLoc LBraceLoc = ClosureBody->getStartLoc();
    NormalBody = makeParserErrorResult(ClosureBody);
    Condition = makeParserErrorResult(new (Context) ErrorExpr(LBraceLoc));
    diagnose(IfLoc, diag::missing_condition_after_if)
        .highlight(SourceRange(IfLoc, LBraceLoc));
  }
  if (NormalBody.isNull())
    NormalBody = parseBraceItemList(diag::expected_lbrace_after_if);
  if (NormalBody.isNull())
    return nullptr; // FIXME: better recovery

  Status |= NormalBody;

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
      Status, new (Context) IfStmt(IfLoc, Condition.get(), NormalBody.get(),
                                   ElseLoc, ElseBody.getPtrOrNull()));
}

/// 
///   stmt-while:
///     'while' expr-basic stmt-brace
ParserResult<Stmt> Parser::parseStmtWhile() {
  SourceLoc WhileLoc = consumeToken(tok::kw_while);

  ParserStatus Status;

  ParserResult<Expr> Condition = parseExprBasic(diag::expected_expr_while);
  Status |= Condition;
  if (Condition.isNull() || Condition.hasCodeCompletion())
    return makeParserResult<Stmt>(Status, nullptr); // FIXME: better recovery

  ParserResult<BraceStmt> Body;
  if (auto *CE = dyn_cast<ClosureExpr>(Condition.get())) {
    // If we parsed a closure after 'while', then it was not the condition, but
    // the 'while' statement body.  We can not have a bare closure in a 'while'
    // condition because closures don't conform to LogicValue.
    auto ClosureBody = CE->getBody();
    SourceLoc LBraceLoc = ClosureBody->getStartLoc();
    Body = makeParserErrorResult(ClosureBody);
    Condition = makeParserErrorResult(new (Context) ErrorExpr(LBraceLoc));
    diagnose(WhileLoc, diag::missing_condition_after_while)
        .highlight(SourceRange(WhileLoc, LBraceLoc));
  }
  if (Body.isNull())
    Body = parseBraceItemList(diag::expected_lbrace_after_while);
  if (Body.isNull())
    return nullptr; // FIXME: better recovery

  Status |= Body;

  return makeParserResult(
      Status, new (Context) WhileStmt(WhileLoc, Condition.get(), Body.get()));
}

/// 
///   stmt-do-while:
///     'do' stmt-brace 'while' expr
ParserResult<Stmt> Parser::parseStmtDoWhile() {
  SourceLoc DoLoc = consumeToken(tok::kw_do);

  ParserStatus Status;

  ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_lbrace_after_do);
  Status |= Body;
  if (Body.isNull())
    Body = makeParserResult(
        Body, BraceStmt::create(Context, Tok.getLoc(), {}, Tok.getLoc()));

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
      new (Context) DoWhileStmt(DoLoc, Condition.get(), WhileLoc, Body.get()));
}

ParserResult<Stmt> Parser::parseStmtFor() {
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
      return parseStmtForCStyle(ForLoc);
    return parseStmtForEach(ForLoc);
  }

  // If we have a leading identifier followed by a ':' or 'in', then this is a
  // pattern, so it is foreach.
  //
  // For error recovery, also parse "for in ..." as foreach.
  if ((isStartOfBindingName(Tok) &&
       (peekToken().is(tok::colon) || peekToken().is(tok::kw_in))) ||
      Tok.is(tok::kw_in))
    return parseStmtForEach(ForLoc);

  // Otherwise, this is some sort of c-style for loop.
  return parseStmtForCStyle(ForLoc);
}

///   stmt-for-c-style:
///     'for' stmt-for-c-style-init? ';' expr? ';'
///           (expr-basic (',' expr-basic)*)? stmt-brace
///     'for' '(' stmt-for-c-style-init? ';' expr? ';'
///           (expr-basic (',' expr-basic)*)? ')' stmt-brace
///   stmt-for-c-style-init:
///     decl-var
///     expr-basic-or-stmt-assign
ParserResult<Stmt> Parser::parseStmtForCStyle(SourceLoc ForLoc) {
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
  // Parse the first part, either a var, expr, or stmt-assign.
  if (Tok.is(tok::kw_var) || Tok.is(tok::at_sign)) {
    DeclAttributes Attributes;
    parseDeclAttributeList(Attributes);
    ParserStatus VarDeclStatus = parseDeclVar(false, Attributes, FirstDecls,
                                              SourceLoc());
    if (VarDeclStatus.isError())
      return VarDeclStatus; // FIXME: better recovery
  } else if (Tok.isNot(tok::semi)) {
    HaveFirst = true;
    First = parseExpr(diag::expected_init_for_stmt);
    Status |= First;
    if (First.isNull() || First.hasCodeCompletion())
      return makeParserResult<Stmt>(Status, nullptr); // FIXME: better recovery
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
          Status, new (Context) ForStmt(ForLoc, First.getPtrOrNull(),
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
    Second = parseExpr(diag::expected_cond_for_stmt);
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
          Status, new (Context) ForStmt(ForLoc, First.getPtrOrNull(),
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
    Third = parseExpr(diag::expected_expr, /*isExprBasic=*/true);
    Status |= Third;
    if (Third.isNonNull())
      ThirdExprs.push_back(Third.get());

    // Parse additional expressions.
    while (Tok.is(tok::comma)) {
      consumeToken(tok::comma);

      Third = parseExpr(diag::expected_expr, /*isExprBasic=*/true);
      Status |= Third;

      if (Third.isNonNull())
        ThirdExprs.push_back(Third.get());
    }

    // If we had more than one expression, form a tuple.
    if (ThirdExprs.size() > 1) {
      Third = makeParserResult(new (Context) TupleExpr(
                                               SourceLoc(),
                                               Context.AllocateCopy(ThirdExprs),
                                               nullptr, SourceLoc(),
                                               /*hasTrailingClosure=*/false,
                                               /*Implicit=*/true));
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
        Body, BraceStmt::create(Context, Tok.getLoc(), {}, Tok.getLoc()));

  return makeParserResult(
      Status,
      new (Context) ForStmt(ForLoc, First.getPtrOrNull(), FirstDeclsContext,
                            Semi1Loc, Second.getPtrOrNull(), Semi2Loc,
                            Third.getPtrOrNull(), Body.get()));
}

/// 
///   stmt-for-each:
///     'for' pattern 'in' expr-basic stmt-brace
ParserResult<Stmt> Parser::parseStmtForEach(SourceLoc ForLoc) {
  ParserResult<Pattern> Pattern = parsePattern();
  if (Pattern.isNull())
    // Recover by creating a "_" pattern.
    Pattern = makeParserErrorResult(new (Context) AnyPattern(SourceLoc()));

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
    Container =
        makeParserErrorResult(new (Context) ErrorExpr(Tok.getLoc()));

  if (auto *CE = dyn_cast<ClosureExpr>(Container.get())) {
    diagnose(CE->getStartLoc(), diag::expected_foreach_container);

    // If the container expression turns out to be a closure, then it was not
    // the container expression, but the 'for' statement body.  We can not have
    // a bare closure as a container expression because closures don't conform
    // to Enumerable.
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
  SmallVector<Decl *, 2> Decls;
  DeclAttributes Attributes;
  addVarsToScope(Pattern.get(), Decls, /*static*/ false, Attributes);

  ParserStatus Status;

  // stmt-brace
  ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::expected_foreach_lbrace);
  Status |= Body;
  if (Body.isNull())
    Body = makeParserResult(
        Body, BraceStmt::create(Context, Tok.getLoc(), {}, Tok.getLoc()));

  return makeParserResult(
      Status,
      new (Context) ForEachStmt(ForLoc, Pattern.get(), InLoc,
                                Container.get(), Body.get()));
}

///
///    stmt-switch:
///      'switch' expr-basic '{' stmt-case+ '}'
ParserResult<Stmt> Parser::parseStmtSwitch() {
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
  while (!Tok.is(tok::kw_case) && !Tok.is(tok::kw_default)
         && !Tok.is(tok::r_brace) && !Tok.is(tok::eof)) {
    if (!DiagnosedNotCoveredStmt) {
      diagnose(Tok, diag::stmt_in_switch_not_covered_by_case);
      DiagnosedNotCoveredStmt = true;
    }
    ASTNode NotCoveredStmt;
    Status |= parseExprOrStmt(NotCoveredStmt);
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
    rBraceLoc = Tok.getLoc();
  }

  return makeParserResult(
      Status, SwitchStmt::create(SwitchLoc, SubjectExpr.get(), lBraceLoc,
                                 cases, rBraceLoc, Context));
}

ParserStatus Parser::parseStmtCaseLabels(SmallVectorImpl<CaseLabel *> &labels,
                                         SmallVectorImpl<Decl *> &boundDecls) {
  // We must have at least one case label.
  assert(Tok.is(tok::kw_case) || Tok.is(tok::kw_default));

  ParserStatus Status;

  bool parsedDefault = false;
  bool parsedOtherLabelWithDefault = false;
  do {
    // 'default' should label a block by itself.
    if (parsedDefault && !parsedOtherLabelWithDefault) {
      diagnose(Tok, diag::default_with_other_labels);
      parsedOtherLabelWithDefault = true;
    }

    // case-label ::= 'case' matching-pattern (',' matching-pattern)*
    //                ('where' expr)? ':'
    if (Tok.is(tok::kw_case)) {
      SourceLoc caseLoc = consumeToken();
      
      // Parse comma-separated patterns.
      SmallVector<Pattern *, 2> patterns;
      do {
        if (CodeCompletion) {
          if (Tok.is(tok::code_complete)) {
            CodeCompletion->completeCaseStmtBeginning();
            consumeToken();
            continue;
          }
          if (Tok.is(tok::period) && peekToken().is(tok::code_complete)) {
            consumeToken();
            CodeCompletion->completeCaseStmtDotPrefix();
            consumeToken();
            continue;
          }
        }

        ParserResult<Pattern> pattern = parseMatchingPattern();
        Status |= pattern;
        if (pattern.isNonNull()) {
          // Add variable bindings from the pattern to the case scope.
          DeclAttributes defaultAttributes;
          addVarsToScope(pattern.get(), boundDecls,
                         /*static*/ false, defaultAttributes);
          patterns.push_back(pattern.get());
        }
      } while (consumeIf(tok::comma));
      
      // Parse an optional 'where' guard.
      SourceLoc whereLoc;
      Expr *guardExpr = nullptr;
      
      if (Tok.is(tok::kw_where)) {
        whereLoc = consumeToken();
        ParserResult<Expr> guard = parseExpr(diag::expected_case_where_expr);
        Status |= guard;
        if (guard.isNonNull())
          guardExpr = guard.get();
      }
      
      SourceLoc colonLoc = Tok.getLoc();
      if (!Tok.is(tok::colon))
        diagnose(Tok, diag::expected_case_colon, "case");
      else
        colonLoc = consumeToken();
      
      auto label = CaseLabel::create(Context, /*isDefault*/false,
                                     caseLoc, patterns, whereLoc, guardExpr,
                                     colonLoc);
      labels.push_back(label);
      continue;
    }
    
    // case-label ::= 'default' ':'
    
    // 'default' should label a block by itself.
    if (!labels.empty() && !parsedOtherLabelWithDefault) {
      diagnose(Tok, diag::default_with_other_labels);
      parsedOtherLabelWithDefault = true;
    }
    
    parsedDefault = true;
    SourceLoc defaultLoc = consumeToken(tok::kw_default);
    
    // We don't allow 'where' guards on a 'default' block. For recovery
    // parse one if present.
    SourceLoc whereLoc;
    Expr *guardExpr = nullptr;
    if (Tok.is(tok::kw_where)) {
      diagnose(Tok, diag::default_with_where);
      whereLoc = consumeToken();
      ParserResult<Expr> guard = parseExpr(diag::expected_case_where_expr);
      Status |= guard;
      if (guard.isNonNull())
        guardExpr = guard.get();
    }
    
    SourceLoc colonLoc = Tok.getLoc();
    if (!Tok.is(tok::colon))
      diagnose(Tok, diag::expected_case_colon, "default");
    else
      consumeToken(tok::colon);
    
    // Create an implicit AnyPattern to represent the default match.
    auto any = new (Context) AnyPattern(defaultLoc);
    auto label = CaseLabel::create(Context, /*isDefault*/true,
                                   defaultLoc, any, whereLoc, guardExpr,
                                   colonLoc);
    labels.push_back(label);
  } while (Tok.is(tok::kw_case) || Tok.is(tok::kw_default));
  return Status;
}

// stmt-case ::= case-label+ brace-item*
ParserResult<CaseStmt> Parser::parseStmtCase() {
  // A case block has its own scope for variables bound out of the pattern.
  Scope scope(this, ScopeKind::CaseVars);

  ParserStatus Status;

  SmallVector<CaseLabel*, 2> labels;
  SmallVector<Decl*, 4> boundDecls;
  Status |= parseStmtCaseLabels(labels, boundDecls);

  assert(!labels.empty() && "did not parse any labels?!");

  // Case blocks with multiple patterns cannot bind variables.
  if (!boundDecls.empty()
      && (labels.size() > 1 || labels[0]->getPatterns().size() > 1))
    diagnose(boundDecls[0]->getLoc(),
             diag::var_binding_with_multiple_case_patterns);

  SmallVector<ASTNode, 8> bodyItems;

  SourceLoc startOfBody = Tok.getLoc();
  Status |= parseBraceItems(bodyItems, BraceItemListKind::Case);
  BraceStmt *body = BraceStmt::create(Context,
                                      startOfBody, bodyItems, Tok.getLoc());

  return makeParserResult(
      Status, CaseStmt::create(Context, labels, !boundDecls.empty(), body));
}
