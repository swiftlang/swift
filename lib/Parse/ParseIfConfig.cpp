//===--- ParseIfConfig.cpp - Swift Language Parser for #if directives -----===//
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
// Conditional Compilation Block Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTVisitor.h"
#include "swift/Parse/Parser.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Version.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

namespace {

/// Get PlatformConditionKind from platform condition name.
static
Optional<PlatformConditionKind> getPlatformConditionKind(StringRef Name) {
  return llvm::StringSwitch<llvm::Optional<PlatformConditionKind>>(Name)
    .Case("os", PlatformConditionKind::OS)
    .Case("arch", PlatformConditionKind::Arch)
    .Case("_endian", PlatformConditionKind::Endianness)
    .Case("_runtime", PlatformConditionKind::Runtime)
    .Default(None);
}

/// Extract source text of the expression.
static StringRef extractExprSource(SourceManager &SM, Expr *E) {
  CharSourceRange Range =
    Lexer::getCharSourceRangeFromSourceRange(SM, E->getSourceRange());
  return SM.extractText(Range);
}

/// Get the identifier string of the UnresolvedDeclRefExpr.
/// Returns \c true if failed.
static llvm::Optional<StringRef> getDeclRefStr(Expr *E, DeclRefKind Kind) {
  auto UDRE = dyn_cast<UnresolvedDeclRefExpr>(E);
  if (!UDRE ||
      !UDRE->hasName() ||
      UDRE->getRefKind() != Kind ||
      UDRE->getName().isCompoundName())
    return None;
  return UDRE->getName().getBaseName().str();
}

/// The condition validator.
class ValidateIfConfigCondition :
  public ExprVisitor<ValidateIfConfigCondition, Expr*> {
  ASTContext &Ctx;
  DiagnosticEngine &D;

  bool HasError;

  Expr *diagnoseUnsupportedExpr(Expr *E) {
    D.diagnose(E->getLoc(),
               diag::unsupported_conditional_compilation_expression_type);
    return nullptr;
  }

  // Support '||' and '&&' operator. The procedence of '&&' is higher than '||'.
  // Invalid operator and the next operand are diagnosed and removed from AST.
  Expr *foldSequence(Expr *LHS, ArrayRef<Expr*> &S, bool isRecurse = false) {
    assert(!S.empty() && ((S.size() & 1) == 0));

    auto getNextOperator = [&]() -> llvm::Optional<StringRef> {
      assert((S.size() & 1) == 0);
      while (!S.empty()) {
        auto Name = getDeclRefStr(S[0], DeclRefKind::BinaryOperator);
        if (Name.hasValue() && (*Name == "||" || *Name == "&&"))
          return Name;

        auto DiagID = isa<UnresolvedDeclRefExpr>(S[0])
          ? diag::unsupported_conditional_compilation_binary_expression
          : diag::unsupported_conditional_compilation_expression_type;
        D.diagnose(S[0]->getLoc(), DiagID);
        HasError |= true;
        // Consume invalid operator and the immediate RHS.
        S = S.slice(2);
      }
      return None;
    };

    // Extract out the first operator name.
    auto OpName = getNextOperator();
    if (!OpName.hasValue())
      // If failed, it's not a sequence anymore.
      return LHS;
    Expr *Op = S[0];
  
    // We will definitely be consuming at least one operator.
    // Pull out the prospective RHS and slice off the first two elements.
    Expr *RHS = validate(S[1]);
    S = S.slice(2);

    while (true) {
      // Pull out the next binary operator.
      auto NextOpName = getNextOperator();
      bool IsEnd = !NextOpName.hasValue();
      if (!IsEnd && *OpName == "||" && *NextOpName == "&&") {
        RHS = foldSequence(RHS, S, /*isRecurse*/true);
        continue;
      }

      // Apply the operator with left-associativity by folding the first two
      // operands.
      TupleExpr *Arg = TupleExpr::create(Ctx, SourceLoc(), { LHS, RHS },
                                         { }, { }, SourceLoc(),
                                         /*HasTrailingClosure=*/false,
                                         /*Implicit=*/true);
      LHS = new (Ctx) BinaryExpr(Op, Arg, /*implicit*/false);

      // If we don't have the next operator, we're done.
      if (IsEnd)
        break;
      if (isRecurse && *OpName == "&&" && *NextOpName == "||")
        break;

      OpName = NextOpName;
      Op = S[0];
      RHS = validate(S[1]);
      S = S.slice(2);
    }

    return LHS;
  }

  // In Swift3 mode, leave sequence as a sequence because it has strange
  // evaluation rule. See 'EvaluateIfConfigCondition::visitSequenceExpr'.
  Expr *validateSequence(ArrayRef<Expr *> &S) {
    assert(Ctx.isSwiftVersion3());

    SmallVector<Expr *, 3> Filtered;
    SmallVector<unsigned, 2> AndIdxs;
    Filtered.push_back(validate(S[0]));
    S = S.slice(1);

    while (!S.empty()) {
      auto OpName = getDeclRefStr(S[0], DeclRefKind::BinaryOperator);
      if (!OpName.hasValue() || (*OpName != "||" && *OpName != "&&")) {
        // Warning and ignore in Swift3 mode.
        D.diagnose(
            S[0]->getLoc(),
            diag::swift3_unsupported_conditional_compilation_expression_type)
          .highlight({ S[0]->getLoc(), S[1]->getEndLoc() });
      } else {
        // Remember the start and end of '&&' sequence.
        bool InAnd = (AndIdxs.size() & 1) == 1;
        if ((*OpName == "&&" && !InAnd) || (*OpName == "||" && InAnd))
          AndIdxs.push_back(Filtered.size() - 1);

        Filtered.push_back(S[0]);
        Filtered.push_back(validate(S[1]));
      }
      S = S.slice(2);
    }
    assert((Filtered.size() & 1) == 1);

    // If the last OpName is '&&', close it with a parenthesis, except if the
    // operators are '&&' only.
    if ((1 == (AndIdxs.size() & 1)) && AndIdxs.back() > 0)
      AndIdxs.push_back(Filtered.size() - 1);
    // Emit fix-its to make this sequence compatilble with Swift >=4 even in
    // Swift3 mode.
    if (AndIdxs.size() >= 2) {
      assert((AndIdxs.size() & 1) == 0);
      auto diag = D.diagnose(
          Filtered[AndIdxs[0]]->getStartLoc(),
          diag::swift3_conditional_compilation_expression_precedence);
      for (unsigned i = 0, e = AndIdxs.size(); i < e; i += 2) {
        diag.fixItInsert(Filtered[AndIdxs[i]]->getStartLoc(), "(");
        diag.fixItInsertAfter(Filtered[AndIdxs[i + 1]]->getEndLoc(), ")");
      }
    }

    if (Filtered.size() == 1)
      return Filtered[0];
    return SequenceExpr::create(Ctx, Filtered);
  }

public:
  ValidateIfConfigCondition(ASTContext &Ctx, DiagnosticEngine &D)
    : Ctx(Ctx), D(D), HasError(false) {}

  // Explicit configuration flag.
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    if (!getDeclRefStr(E, DeclRefKind::Ordinary).hasValue())
      return diagnoseUnsupportedExpr(E);
    return E;
  }

  // 'true' or 'false' constant.
  Expr *visitBooleanLiteralExpr(BooleanLiteralExpr *E) {
    return E;
  }

  // '0' and '1' are warned, but we accept it.
  Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    if (E->isNegative() ||
        (E->getDigitsText() != "0" && E->getDigitsText() != "1")) {
      return diagnoseUnsupportedExpr(E);
    }
    // "#if 0" isn't valid, but it is common, so recognize it and handle it
    // with a fixit.
    StringRef replacement = E->getDigitsText() == "0" ? "false" :"true";
    D.diagnose(E->getLoc(), diag::unsupported_conditional_compilation_integer,
               E->getDigitsText(), replacement)
      .fixItReplace(E->getLoc(), replacement);
    return E;
  }

  // Platform conditions.
  Expr *visitCallExpr(CallExpr *E) {
    auto KindName = getDeclRefStr(E->getFn(), DeclRefKind::Ordinary);
    if (!KindName.hasValue()) {
      D.diagnose(E->getLoc(), diag::unsupported_platform_condition_expression);
      return nullptr;
    }

    auto *ArgP = dyn_cast<ParenExpr>(E->getArg());
    if (!ArgP) {
      D.diagnose(E->getLoc(), diag::platform_condition_expected_one_argument);
      return nullptr;
    }
    Expr *Arg = ArgP->getSubExpr();

    // '_compiler_version' '(' string-literal ')'
    if (*KindName == "_compiler_version") {
      auto SLE = dyn_cast<StringLiteralExpr>(Arg);
      if (!SLE) {
        D.diagnose(Arg->getLoc(),
                   diag::unsupported_platform_condition_argument,
                   "string literal");
        return nullptr;
      }

      auto ValStr = SLE->getValue();
      if (ValStr.empty()) {
        D.diagnose(SLE->getLoc(), diag::empty_version_string);
        return nullptr;
      }

      auto Val = version::Version::parseCompilerVersionString(
          SLE->getValue(), SLE->getLoc(), &D);
      if(!Val.hasValue())
        return nullptr;
      return E;
    }

    // 'swift' '(' '>=' float-literal ( '.' integer-literal )* ')'
    if (*KindName == "swift") {
      auto PUE = dyn_cast<PrefixUnaryExpr>(Arg);
      llvm::Optional<StringRef> PrefixName = PUE ?
        getDeclRefStr(PUE->getFn(), DeclRefKind::PrefixOperator) : None;
      if (!PrefixName || *PrefixName != ">=") {
        D.diagnose(Arg->getLoc(),
                   diag::unsupported_platform_condition_argument,
                   "a unary comparison, such as '>=2.2'");
        return nullptr;
      }
      auto versionString = extractExprSource(Ctx.SourceMgr, PUE->getArg());
      auto Val = version::Version::parseVersionString(
          versionString, PUE->getArg()->getStartLoc(), &D);
      if(!Val.hasValue())
        return nullptr;
      return E;
    }

    // ( 'os' | 'arch' | '_endian' | '_runtime' ) '(' identifier ')''
    auto Kind = getPlatformConditionKind(*KindName);
    if (!Kind.hasValue()) {
      D.diagnose(E->getLoc(), diag::unsupported_platform_condition_expression);
      return nullptr;
    }

    auto ArgStr = getDeclRefStr(Arg, DeclRefKind::Ordinary);
    if (!ArgStr.hasValue()) {
      D.diagnose(E->getLoc(), diag::unsupported_platform_condition_argument,
                 "identifier");
      return nullptr;
    }

    // FIXME: Perform the replacement macOS -> OSX elsewhere.
    if (Kind == PlatformConditionKind::OS && *ArgStr == "macOS") {
      *ArgStr = "OSX";
      ArgP->setSubExpr(
          new (Ctx) UnresolvedDeclRefExpr(Ctx.getIdentifier(*ArgStr),
                                          DeclRefKind::Ordinary,
                                          DeclNameLoc(Arg->getLoc())));
    }

    std::vector<StringRef> suggestions;
    if (!LangOptions::checkPlatformConditionSupported(*Kind, *ArgStr,
                                                      suggestions)) {
      if (Kind == PlatformConditionKind::Runtime) {
        // Error for _runtime()
        D.diagnose(Arg->getLoc(),
                   diag::unsupported_platform_runtime_condition_argument);
        return nullptr;
      }
      StringRef DiagName;
      switch (*Kind) {
      case PlatformConditionKind::OS:
        DiagName = "operating system"; break;
      case PlatformConditionKind::Arch:
        DiagName = "architecture"; break;
      case PlatformConditionKind::Endianness:
        DiagName = "endianness"; break;
      case PlatformConditionKind::Runtime:
        llvm_unreachable("handled above");
      }
      auto Loc = Arg->getLoc();
      D.diagnose(Loc, diag::unknown_platform_condition_argument,
                 DiagName, *KindName);
      for (auto suggestion : suggestions)
        D.diagnose(Loc, diag::note_typo_candidate, suggestion)
          .fixItReplace(Arg->getSourceRange(), suggestion);
      return nullptr;
    }

    return E;
  }

  // Grouped condition. e.g. '(FLAG)'
  Expr *visitParenExpr(ParenExpr *E) {
    E->setSubExpr(validate(E->getSubExpr()));
    return E;
  }

  // Prefix '!'. Other prefix operators are rejected.
  Expr *visitPrefixUnaryExpr(PrefixUnaryExpr *E) {
    auto OpName = getDeclRefStr(E->getFn(), DeclRefKind::PrefixOperator);
    if (!OpName.hasValue() || *OpName != "!") {
      D.diagnose(E->getLoc(),
                 diag::unsupported_conditional_compilation_unary_expression);
      return nullptr;
    }
    E->setArg(validate(E->getArg()));
    return E;
  }

  // Fold sequence expression for non-Swift3 mode.
  Expr *visitSequenceExpr(SequenceExpr *E) {
    ArrayRef<Expr*> Elts = E->getElements();
    Expr *foldedExpr;
    if (Ctx.isSwiftVersion3()) {
      foldedExpr = validateSequence(Elts);
    } else {
      auto LHS = validate(Elts[0]);
      Elts = Elts.slice(1);
      foldedExpr = foldSequence(LHS, Elts);
    }
    assert(Elts.empty());
    return foldedExpr;
  }

  // Other expression types are unsupported.
  Expr *visitExpr(Expr *E) {
    return diagnoseUnsupportedExpr(E);
  }

  Expr *validate(Expr *E) {
    if (auto E2 = visit(E))
      return E2;
    HasError |= true;
    return E;
  }

  bool hasError() const {
    return HasError;
  }
};

/// Validate and modify the condition expression.
/// Returns \c true if the condition contains any error.
static bool validateIfConfigCondition(Expr *&condition,
                                      ASTContext &Context,
                                      DiagnosticEngine &D) {
  ValidateIfConfigCondition Validator(Context, D);
  condition = Validator.validate(condition);
  return Validator.hasError();
}

/// The condition evaluator.
/// The condition must be validated with validateIfConfigCondition().
class EvaluateIfConfigCondition :
  public ExprVisitor<EvaluateIfConfigCondition, bool> {
  ASTContext &Ctx;

public:
  EvaluateIfConfigCondition(ASTContext &Ctx) : Ctx(Ctx) {}

  bool visitBooleanLiteralExpr(BooleanLiteralExpr *E) {
    return E->getValue();
  }

  bool visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    return E->getDigitsText() != "0";
  }

  bool visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    auto Name = getDeclRefStr(E, DeclRefKind::Ordinary).getValue();
    return Ctx.LangOpts.isCustomConditionalCompilationFlagSet(Name);
  }

  bool visitCallExpr(CallExpr *E) {
    auto KindName = getDeclRefStr(E->getFn(), DeclRefKind::Ordinary).getValue();
    auto *Arg = cast<ParenExpr>(E->getArg())->getSubExpr();

    if (KindName == "_compiler_version") {
      auto Str = cast<StringLiteralExpr>(Arg)->getValue();
      auto Val = version::Version::parseCompilerVersionString(
          Str, SourceLoc(), nullptr).getValue();
      auto thisVersion = version::Version::getCurrentCompilerVersion();
      return thisVersion >= Val;
    } else if (KindName == "swift") {
      auto PUE = cast<PrefixUnaryExpr>(Arg);
      auto Str = extractExprSource(Ctx.SourceMgr, PUE->getArg());
      auto Val = version::Version::parseVersionString(
          Str, SourceLoc(), nullptr).getValue();
      auto thisVersion = Ctx.LangOpts.EffectiveLanguageVersion;
      return thisVersion >= Val;
    }

    auto Val = getDeclRefStr(Arg, DeclRefKind::Ordinary).getValue();
    auto Kind = getPlatformConditionKind(KindName).getValue();
    auto Target = Ctx.LangOpts.getPlatformConditionValue(Kind);
    return Target == Val;
  }

  bool visitPrefixUnaryExpr(PrefixUnaryExpr *E) {
    return !visit(E->getArg());
  }

  bool visitParenExpr(ParenExpr *E) {
    return visit(E->getSubExpr());
  }

  bool visitBinaryExpr(BinaryExpr *E) {
    assert(!Ctx.isSwiftVersion3() && "BinaryExpr in Swift3 mode");
    auto OpName =
      getDeclRefStr(E->getFn(), DeclRefKind::BinaryOperator).getValue();
    auto Args = E->getArg()->getElements();
    if (OpName == "||") return visit(Args[0]) || visit(Args[1]);
    if (OpName == "&&") return visit(Args[0]) && visit(Args[1]);
    llvm_unreachable("unsupported binary operator");
  }

  bool visitSequenceExpr(SequenceExpr *E) {
    assert(Ctx.isSwiftVersion3() && "SequenceExpr in non-Swift3 mode");
    ArrayRef<Expr *> Elems = E->getElements();
    auto Result = visit(Elems[0]);
    Elems = Elems.slice(1);
    while (!Elems.empty()) {
      auto OpName =
        getDeclRefStr(Elems[0], DeclRefKind::BinaryOperator).getValue();

      if (OpName == "||") {
        Result = Result || visit(Elems[1]);
        if (Result)
          // Note that this is the Swift3 behavior.
          // e.g. 'false || true && false' evaluates to 'true'.
          return true;
      } else if (OpName == "&&") {
        Result = Result && visit(Elems[1]);
        if (!Result)
          // Ditto.
          // e.g. 'false && true || true' evaluates to 'false'.
          return false;
      } else {
        llvm_unreachable("must be removed in validation phase");
      }
      Elems = Elems.slice(2);
    }
    return Result;
  }

  bool visitExpr(Expr *E) { llvm_unreachable("Unvalidated condition?"); }
};

/// Evaluate the condition.
/// \c true if success, \c false if failed.
static bool evaluateIfConfigCondition(Expr *Condition, ASTContext &Context) {
  return EvaluateIfConfigCondition(Context).visit(Condition);
}

/// Version condition checker.
class IsVersionIfConfigCondition :
  public ExprVisitor<IsVersionIfConfigCondition, bool> {

public:
  IsVersionIfConfigCondition() {}

  bool visitBinaryExpr(BinaryExpr *E) {
    auto OpName =
      getDeclRefStr(E->getFn(), DeclRefKind::BinaryOperator).getValue();
    auto Args = E->getArg()->getElements();
    if (OpName == "||") return visit(Args[0]) && visit(Args[1]);
    if (OpName == "&&") return visit(Args[0]) || visit(Args[1]);
    llvm_unreachable("unsupported binary operator");
  }

  bool visitCallExpr(CallExpr *E) {
    auto KindName = getDeclRefStr(E->getFn(), DeclRefKind::Ordinary).getValue();
    return KindName == "_compiler_version" || KindName == "swift";
  }

  bool visitPrefixUnaryExpr(PrefixUnaryExpr *E) { return visit(E->getArg()); }
  bool visitParenExpr(ParenExpr *E) { return visit(E->getSubExpr()); }
  bool visitExpr(Expr *E) { return false; }
};

/// Returns \c true if the condition is a version check.
static bool isVersionIfConfigCondition(Expr *Condition) {
  return IsVersionIfConfigCondition().visit(Condition);
}

} // end anonymous namespace

/// Parse and populate a list of #if/#elseif/#else/#endif clauses.
/// Delegate callback function to parse elements in the blocks.
template <typename ElemTy, unsigned N>
static ParserStatus parseIfConfig(
    Parser &P, SmallVectorImpl<IfConfigClause<ElemTy>> &Clauses,
    SourceLoc &EndLoc, bool HadMissingEnd,
    llvm::function_ref<void(SmallVectorImpl<ElemTy> &, bool)> parseElements) {
  Parser::StructureMarkerRAII ParsingDecl(
      P, P.Tok.getLoc(), Parser::StructureMarkerKind::IfConfig);

  bool foundActive = false;
  bool isVersionCondition = false;
  while (1) {
    bool isElse = P.Tok.is(tok::pound_else);
    SourceLoc ClauseLoc = P.consumeToken();
    Expr *Condition = nullptr;
    bool isActive = false;

    // Parse and evaluate the directive.
    if (isElse) {
      isActive = !foundActive;
    } else {
      llvm::SaveAndRestore<bool> S(P.InPoundIfEnvironment, true);
      ParserResult<Expr> Result = P.parseExprSequence(diag::expected_expr,
                                                      /*isBasic*/true,
                                                      /*isForDirective*/true);
      if (Result.isNull())
        return makeParserError();
      Condition = Result.get();
      if (validateIfConfigCondition(Condition, P.Context, P.Diags)) {
        // Error in the condition;
        isActive = false;
        isVersionCondition = false;
      } else if (!foundActive) {
        // Evaludate the condition only if we haven't found any active one.
        isActive = evaluateIfConfigCondition(Condition, P.Context);
        isVersionCondition = isVersionIfConfigCondition(Condition);
      }
    }

    foundActive |= isActive;

    if (!P.Tok.isAtStartOfLine() && P.Tok.isNot(tok::eof)) {
      P.diagnose(P.Tok.getLoc(),
                 diag::extra_tokens_conditional_compilation_directive);
    }

    // Parse elements
    SmallVector<ElemTy, N> Elements;
    if (isActive || !isVersionCondition) {
      parseElements(Elements, isActive);
    } else {
      DiagnosticTransaction DT(P.Diags);
      P.skipUntilConditionalBlockClose();
      DT.abort();
    }

    Clauses.push_back(IfConfigClause<ElemTy>(ClauseLoc, Condition,
                                             P.Context.AllocateCopy(Elements),
                                             isActive));

    if (P.Tok.isNot(tok::pound_elseif, tok::pound_else))
      break;

    if (isElse)
      P.diagnose(P.Tok, diag::expected_close_after_else_directive);
  }

  HadMissingEnd = P.parseEndIfDirective(EndLoc);
  return makeParserSuccess();
}

/// Parse #if ... #endif in declarations position.
ParserResult<IfConfigDecl> Parser::parseDeclIfConfig(ParseDeclOptions Flags) {
  SmallVector<IfConfigClause<Decl *>, 4> Clauses;
  SourceLoc EndLoc;
  bool HadMissingEnd = false;
  auto Status = parseIfConfig<Decl *, 8>(
      *this, Clauses, EndLoc, HadMissingEnd,
      [&](SmallVectorImpl<Decl *> &Decls, bool IsActive) {
    Optional<Scope> scope;
    if (!IsActive)
      scope.emplace(this, getScopeInfo().getCurrentScope()->getKind(),
                    /*inactiveConfigBlock=*/true);

    ParserStatus Status;
    bool PreviousHadSemi = true;
    while (Tok.isNot(tok::pound_else, tok::pound_endif, tok::pound_elseif,
                      tok::eof)) {
      if (Tok.is(tok::r_brace)) {
        diagnose(Tok.getLoc(),
                  diag::unexpected_rbrace_in_conditional_compilation_block);
        // If we see '}', following declarations don't look like belong to
        // the current decl context; skip them.
        skipUntilConditionalBlockClose();
        break;
      }
      Status |= parseDeclItem(PreviousHadSemi, Flags,
                              [&](Decl *D) {Decls.push_back(D);});
    }
  });
  if (Status.isError())
    return makeParserErrorResult<IfConfigDecl>();

  IfConfigDecl *ICD = new (Context) IfConfigDecl(CurDeclContext,
                                                 Context.AllocateCopy(Clauses),
                                                 EndLoc, HadMissingEnd);
  return makeParserResult(ICD);
}

/// Parse #if ... #endif in statements position.
ParserResult<Stmt> Parser::parseStmtIfConfig(BraceItemListKind Kind) {
  SmallVector<IfConfigClause<ASTNode>, 4> Clauses;
  SourceLoc EndLoc;
  bool HadMissingEnd = false;
  auto Status = parseIfConfig<ASTNode, 16>(
      *this, Clauses, EndLoc, HadMissingEnd,
      [&](SmallVectorImpl<ASTNode> &Elements, bool IsActive) {
    parseBraceItems(Elements, Kind, IsActive
                      ? BraceItemListKind::ActiveConditionalBlock
                      : BraceItemListKind::InactiveConditionalBlock);
  });
  if (Status.isError())
    return makeParserErrorResult<Stmt>();

  auto *ICS = new (Context) IfConfigStmt(Context.AllocateCopy(Clauses),
                                         EndLoc, HadMissingEnd);
  return makeParserResult(ICS);
}
