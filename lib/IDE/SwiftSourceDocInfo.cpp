//===----------------------------------------------------------------------===//
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

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/Utils.h"
#include "swift/Markup/XMLUtils.h"
#include "swift/Subsystems.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/Lexer.h"
#include "clang/Basic/CharInfo.h"

#include "llvm/Support/MemoryBuffer.h"

#include <numeric>

using namespace swift;
using namespace swift::ide;

Optional<std::pair<unsigned, unsigned>>
swift::ide::parseLineCol(StringRef LineCol) {
  unsigned Line, Col;
  size_t ColonIdx = LineCol.find(':');
  if (ColonIdx == StringRef::npos) {
    llvm::errs() << "wrong pos format, it should be '<line>:<column>'\n";
    return None;
  }
  if (LineCol.substr(0, ColonIdx).getAsInteger(10, Line)) {
    llvm::errs() << "wrong pos format, it should be '<line>:<column>'\n";
    return None;
  }
  if (LineCol.substr(ColonIdx+1).getAsInteger(10, Col)) {
    llvm::errs() << "wrong pos format, it should be '<line>:<column>'\n";
    return None;
  }

  if (Line == 0 || Col == 0) {
    llvm::errs() << "wrong pos format, line/col should start from 1\n";
    return None;
  }

  return std::make_pair(Line, Col);
}

void XMLEscapingPrinter::printText(StringRef Text) {
  swift::markup::appendWithXMLEscaping(OS, Text);
}

void XMLEscapingPrinter::printXML(StringRef Text) {
  OS << Text;
}

SourceManager &NameMatcher::getSourceMgr() const {
  return SrcFile.getASTContext().SourceMgr;
}

std::vector<ResolvedLoc> NameMatcher::resolve(ArrayRef<UnresolvedLoc> Locs, ArrayRef<Token> Tokens) {

  // Note the original indices and sort them in reverse source order
  std::vector<size_t> MapToOriginalIndex(Locs.size());
  std::iota(MapToOriginalIndex.begin(), MapToOriginalIndex.end(), 0);
  std::sort(MapToOriginalIndex.begin(), MapToOriginalIndex.end(),
            [this, Locs](size_t first, size_t second) {
              return first != second && !getSourceMgr()
                .isBeforeInBuffer(Locs[first].Loc, Locs[second].Loc);
            });

  // Add the locs themselves
  LocsToResolve.clear();
  std::transform(MapToOriginalIndex.begin(), MapToOriginalIndex.end(),
                 std::back_inserter(LocsToResolve),
                 [&](size_t index){ return Locs[index]; });

  InactiveConfigRegionNestings = 0;
  SelectorNestings = 0;
  TokensToCheck = Tokens;
  ResolvedLocs.clear();
  SrcFile.walk(*this);
  checkComments();

  // handle any unresolved locs past the end of the last AST node or comment
  std::vector<ResolvedLoc> Remaining(Locs.size() - ResolvedLocs.size(), {
    ASTWalker::ParentTy(), CharSourceRange(), {}, LabelRangeType::None,
    /*isActice*/true, /*isInSelector*/false});
  ResolvedLocs.insert(ResolvedLocs.end(), Remaining.begin(), Remaining.end());

  // return in the original order
  std::vector<ResolvedLoc> Ordered(ResolvedLocs.size());
  for(size_t Index = 0; Index < ResolvedLocs.size(); ++Index) {
    size_t Flipped = ResolvedLocs.size() - 1 - Index;
    Ordered[MapToOriginalIndex[Flipped]] = ResolvedLocs[Index];
  }
  return Ordered;
}

static std::vector<CharSourceRange> getLabelRanges(const ParameterList* List, 
                                                   const SourceManager &SM) {
  std::vector<CharSourceRange> LabelRanges;
  for (ParamDecl *Param: *List) {
    if (Param->isImplicit())
      continue;

    SourceLoc NameLoc = Param->getArgumentNameLoc();
    SourceLoc ParamLoc = Param->getNameLoc();
    size_t NameLength;
    if (NameLoc.isValid()) {
      LabelRanges.push_back(Lexer::getCharSourceRangeFromSourceRange(
                                SM, SourceRange(NameLoc, ParamLoc)));
    } else {
      NameLoc = ParamLoc;
      NameLength = Param->getNameStr().size();
      LabelRanges.push_back(CharSourceRange(NameLoc, NameLength));
    }
  }
  return LabelRanges;
}

static std::vector<CharSourceRange> getEnumParamListInfo(SourceManager &SM, 
                                                         ParameterList *PL) {
  std::vector<CharSourceRange> LabelRanges;
  for (ParamDecl *Param: *PL) {
    if (Param->isImplicit())
      continue;

    SourceLoc LabelStart;
    if (auto *repr = Param->getTypeRepr())
      LabelStart = repr->getLoc();
    SourceLoc LabelEnd(LabelStart);
    
    if (Param->getNameLoc().isValid()) {
      LabelStart = Param->getNameLoc();
    }
    LabelRanges.push_back(CharSourceRange(SM, LabelStart, LabelEnd));
  }
  return LabelRanges;
}

bool NameMatcher::handleCustomAttrs(Decl *D) {
  // CustomAttrs of non-param VarDecls are handled when this method is called
  // on their containing PatternBindingDecls (see below).
  if (isa<VarDecl>(D) && !isa<ParamDecl>(D))
    return true;

  if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    if (auto *SingleVar = PBD->getSingleVar()) {
      D = SingleVar;
    } else {
        return true;
    }
  }

  for (auto *customAttr : D->getAttrs().getAttributes<CustomAttr, true>()) {
    if (shouldSkip(customAttr->getRangeWithAt()))
      continue;
    auto *Arg = customAttr->getArg();
    if (auto *Repr = customAttr->getTypeLoc().getTypeRepr()) {
      // Note the associated call arguments of the semantic initializer call
      // in case we're resolving an explicit initializer call within the
      // CustomAttr's type, e.g. on `Wrapper` in `@Wrapper(wrappedValue: 10)`.
      SWIFT_DEFER { CustomAttrArg = None; };
      if (Arg && !Arg->isImplicit())
        CustomAttrArg = {Repr->getLoc(), Arg};
      if (!Repr->walk(*this))
        return false;
    }
    if (Arg && !Arg->isImplicit()) {
      if (!Arg->walk(*this))
        return false;
    }
  }
  return !isDone();
}

bool NameMatcher::walkToDeclPre(Decl *D) {
  // Handle occurrences in any preceding doc comments
  RawComment R = D->getRawComment();
  if (!R.isEmpty()) {
    for(SingleRawComment C: R.Comments) {
      while(!shouldSkip(C.Range))
        tryResolve(ASTWalker::ParentTy(), nextLoc());
    }
  }

  // FIXME: Even implicit Decls should have proper ranges if they include any
  // non-implicit children (fix implicit Decls created for lazy vars).
  if (D->isImplicit())
    return !isDone();

  if (shouldSkip(D->getSourceRangeIncludingAttrs()))
    return false;
  
  if (!handleCustomAttrs(D))
    return false;

  if (auto *ICD = dyn_cast<IfConfigDecl>(D)) {
    for (auto Clause : ICD->getClauses()) {
      if (!Clause.isActive)
        ++InactiveConfigRegionNestings;
      
      for (auto Member : Clause.Elements) {
        Member.walk(*this);
      }
      
      if (!Clause.isActive) {
        assert(InactiveConfigRegionNestings > 0);
        --InactiveConfigRegionNestings;
      }
    }
    return false;
  } else if (AbstractFunctionDecl *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
    std::vector<CharSourceRange> LabelRanges;
    if (AFD->getNameLoc() == nextLoc()) {
      auto ParamList = AFD->getParameters();
      LabelRanges = getLabelRanges(ParamList, getSourceMgr());
    }
    tryResolve(ASTWalker::ParentTy(D), D->getLoc(), LabelRangeType::Param,
               LabelRanges);
  } else if (SubscriptDecl *SD = dyn_cast<SubscriptDecl>(D)) {
    tryResolve(ASTWalker::ParentTy(D), D->getLoc(), LabelRangeType::NoncollapsibleParam,
               getLabelRanges(SD->getIndices(), getSourceMgr()));
  } else if (EnumElementDecl *EED = dyn_cast<EnumElementDecl>(D)) {
    if (auto *ParamList = EED->getParameterList()) {
      auto LabelRanges = getEnumParamListInfo(getSourceMgr(), ParamList);
      tryResolve(ASTWalker::ParentTy(D), D->getLoc(), LabelRangeType::CallArg,
                 LabelRanges);
    } else {
      tryResolve(ASTWalker::ParentTy(D), D->getLoc());
    }
  } else if (ImportDecl *ID = dyn_cast<ImportDecl>(D)) {
    for(const ImportDecl::AccessPathElement &Element: ID->getFullAccessPath()) {
      tryResolve(ASTWalker::ParentTy(D), Element.second);
      if (isDone())
        break;
    }
  } else if (isa<ValueDecl>(D) || isa<ExtensionDecl>(D) ||
             isa<PrecedenceGroupDecl>(D)) {
    tryResolve(ASTWalker::ParentTy(D), D->getLoc());
  }
  return !isDone();
}

bool NameMatcher::walkToDeclPost(Decl *D) {
  return !isDone();
}

std::pair<bool, Stmt *> NameMatcher::walkToStmtPre(Stmt *S) {
  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() && shouldSkip(S->getSourceRange()))
    return std::make_pair(false, isDone()? nullptr : S);
  return std::make_pair(true, S);
}

Stmt *NameMatcher::walkToStmtPost(Stmt *S) {
  if (isDone())
    return nullptr;
  return S;
}

std::pair<bool, Expr*> NameMatcher::walkToExprPre(Expr *E) {
  if (shouldSkip(E))
    return std::make_pair(false, isDone()? nullptr : E);

  if (isa<ObjCSelectorExpr>(E)) {
      ++SelectorNestings;
  }

  // only match name locations of expressions apparent in the original source
  if (!E->isImplicit()) {
    // Try to resolve against the below kinds *before* their children are
    // visited to ensure visitation happens in source order.
    switch (E->getKind()) {
      case ExprKind::UnresolvedMember: {
        auto UME = cast<UnresolvedMemberExpr>(E);
        tryResolve(ASTWalker::ParentTy(E), UME->getNameLoc(), UME->getArgument(), !UME->getArgument());
      } break;
      case ExprKind::DeclRef: {
        auto DRE = cast<DeclRefExpr>(E);
        tryResolve(ASTWalker::ParentTy(E), DRE->getNameLoc(), nullptr, true);
        break;
      }
      case ExprKind::UnresolvedDeclRef: {
        auto UDRE = cast<UnresolvedDeclRefExpr>(E);
        tryResolve(ASTWalker::ParentTy(E), UDRE->getNameLoc(), nullptr, true);
        break;
      }
      case ExprKind::StringLiteral:
        // Handle multple locations in a single string literal
        do {
          tryResolve(ASTWalker::ParentTy(E), nextLoc());
        } while (!shouldSkip(E));
        break;
      case ExprKind::Subscript: {
        auto SubExpr = cast<SubscriptExpr>(E);
        // visit and check in source order
        if (!SubExpr->getBase()->walk(*this))
          return {false, nullptr};

        auto Labels = getCallArgLabelRanges(getSourceMgr(), SubExpr->getIndex(),
                                            LabelRangeEndAt::BeforeElemStart);
        tryResolve(ASTWalker::ParentTy(E), E->getLoc(), LabelRangeType::CallArg, Labels);
        if (isDone())
            break;
        if (!SubExpr->getIndex()->walk(*this))
          return {false, nullptr};

        // We already visited the children.
        if (!walkToExprPost(E))
          return {false, nullptr};
        return {false, E};
      }
      case ExprKind::Tuple: {
        TupleExpr *T = cast<TupleExpr>(E);
        // Handle arg label locations (the index reports property occurrences
        // on them for memberwise inits)
        for (unsigned i = 0, e = T->getNumElements(); i != e; ++i) {
          auto Name = T->getElementName(i);
          if (!Name.empty()) {
            tryResolve(ASTWalker::ParentTy(E), T->getElementNameLoc(i));
            if (isDone())
              break;
          }
          if (auto *Elem = T->getElement(i)) {
            if (!Elem->walk(*this))
              return {false, nullptr};
          }
        }
        // We already visited the children.
        if (!walkToExprPost(E))
          return {false, nullptr};
        return {false, E};
      }
      case ExprKind::Binary: {
        BinaryExpr *BinE = cast<BinaryExpr>(E);
        // Visit in source order.
        if (!BinE->getArg()->getElement(0)->walk(*this))
          return {false, nullptr};
        if (!BinE->getFn()->walk(*this))
          return {false, nullptr};
        if (!BinE->getArg()->getElement(1)->walk(*this))
          return {false, nullptr};

        // We already visited the children.
        if (!walkToExprPost(E))
          return {false, nullptr};
        return {false, E};
      }
      default: // ignored
        break;
    }
  }
  return std::make_pair(!isDone(), isDone()? nullptr : E);
}

Expr *NameMatcher::walkToExprPost(Expr *E) {
  if (isDone())
    return nullptr;

  if (!E->isImplicit()) {
    // Try to resolve against the below kinds *after* their children have been
    // visited to ensure visitation happens in source order.
    switch (E->getKind()) {
      case ExprKind::MemberRef:
        tryResolve(ASTWalker::ParentTy(E), E->getLoc());
        break;
      case ExprKind::UnresolvedDot: {
        auto UDE = cast<UnresolvedDotExpr>(E);
        tryResolve(ASTWalker::ParentTy(E), UDE->getNameLoc(), nullptr, true);
        break;
      }
      default:
        break;
    }
  }

  if (isa<ObjCSelectorExpr>(E)) {
    assert(SelectorNestings > 0);
    --SelectorNestings;
  }

  return E;
}

bool NameMatcher::walkToTypeLocPre(TypeLoc &TL) {
  if (isDone() || shouldSkip(TL.getSourceRange()))
    return false;
  return true;
}

bool NameMatcher::walkToTypeLocPost(TypeLoc &TL) {
  return !isDone();
}

bool NameMatcher::walkToTypeReprPre(TypeRepr *T) {
  if (isDone() || shouldSkip(T->getSourceRange()))
    return false;

  if (isa<ComponentIdentTypeRepr>(T)) {
    // If we're walking a CustomAttr's type we may have an associated call
    // argument to resolve with from its semantic initializer.
    if (CustomAttrArg.hasValue() && CustomAttrArg->first == T->getLoc()) {
      tryResolve(ASTWalker::ParentTy(T), T->getLoc(), LabelRangeType::CallArg,
                 getCallArgLabelRanges(getSourceMgr(), CustomAttrArg->second, LabelRangeEndAt::BeforeElemStart));
    } else {
      tryResolve(ASTWalker::ParentTy(T), T->getLoc());
    }
  }
  return !isDone();
}

bool NameMatcher::walkToTypeReprPost(TypeRepr *T) {
  return !isDone();
}

std::pair<bool, Pattern*> NameMatcher::walkToPatternPre(Pattern *P) {
  if (isDone() || shouldSkip(P->getSourceRange()))
    return std::make_pair(false, P);

  tryResolve(ASTWalker::ParentTy(P), P->getStartLoc());
  return std::make_pair(!isDone(), P);
}

bool NameMatcher::checkComments() {
  if (isDone())
    return false;
  TokensToCheck = TokensToCheck.drop_while([this](const Token &tok) -> bool {
    return getSourceMgr().isBeforeInBuffer(tok.getRange().getEnd(), nextLoc());
  });
  if (TokensToCheck.empty())
    return false;

  const Token &next = TokensToCheck.front();
  if (next.is(swift::tok::comment) && next.getRange().contains(nextLoc()) &&
      !next.getText().startswith("///"))
    return tryResolve(ASTWalker::ParentTy(), nextLoc());
  return false;
}

void NameMatcher::skipLocsBefore(SourceLoc Start) {
  while (!isDone() && getSourceMgr().isBeforeInBuffer(nextLoc(), Start)) {
    if (!checkComments()) {
      LocsToResolve.pop_back();
      ResolvedLocs.push_back({ASTWalker::ParentTy(), CharSourceRange(), {},
        LabelRangeType::None, isActive(), isInSelector()});
    }
  }
}

bool NameMatcher::shouldSkip(Expr *E) {
  if (isa<StringLiteralExpr>(E) && Parent.getAsExpr()) {
    // Attempting to get the CharSourceRange from the SourceRange of a
    // StringLiteralExpr that is a segment of an interpolated string gives
    // incorrect ranges. Use the CharSourceRange of the corresponding token
    // instead.

    auto ExprStart = E->getStartLoc();
    auto RemaingTokens = TokensToCheck.drop_while([&](const Token &tok) -> bool {
      return getSourceMgr().isBeforeInBuffer(tok.getRange().getStart(), ExprStart);
    });

    if (!RemaingTokens.empty() && RemaingTokens.front().getLoc() == ExprStart)
      return shouldSkip(RemaingTokens.front().getRange());
  }
  return shouldSkip(E->getSourceRange());
}

bool NameMatcher::shouldSkip(SourceRange Range) {
  return shouldSkip(Lexer::getCharSourceRangeFromSourceRange(getSourceMgr(),
                                                             Range));
}

bool NameMatcher::shouldSkip(CharSourceRange Range) {
  if (isDone())
    return true;
  if (Range.isInvalid())
    return false;

  skipLocsBefore(Range.getStart());
  return isDone() || !Range.contains(nextLoc());
}

SourceLoc NameMatcher::nextLoc() const {
  assert(!LocsToResolve.empty());
  return LocsToResolve.back().Loc;
}

std::vector<CharSourceRange> getSelectorLabelRanges(SourceManager &SM,
                                                    DeclNameLoc NameLoc) {
  SourceLoc Loc;
  std::vector<CharSourceRange> Ranges;
  size_t index = 0;
  while((Loc = NameLoc.getArgumentLabelLoc(index++)).isValid()) {
    CharSourceRange Range = Lexer::getCharSourceRangeFromSourceRange(SM,
                                                                     SourceRange(Loc));
    Ranges.push_back(Range);
  }

  return Ranges;
}

bool NameMatcher::tryResolve(ASTWalker::ParentTy Node, DeclNameLoc NameLoc,
                             Expr *Arg, bool checkParentForLabels) {
  if (NameLoc.isInvalid())
    return false;

  if (NameLoc.isCompound()) {
    auto Labels = getSelectorLabelRanges(getSourceMgr(), NameLoc);
    bool Resolved = tryResolve(Node, NameLoc.getBaseNameLoc(),
                               LabelRangeType::Selector, Labels);
    if (!isDone()) {
      for (auto Label: Labels) {
        if (tryResolve(Node, Label.getStart())) {
          Resolved = true;
          if (isDone())
            break;
        }
      }
    }
    return Resolved;
  }

  if (LocsToResolve.back().ResolveArgLocs) {
    if (Arg)
      return tryResolve(Node, NameLoc.getBaseNameLoc(), LabelRangeType::CallArg,
                        getCallArgLabelRanges(getSourceMgr(), Arg,
                                              LabelRangeEndAt::BeforeElemStart));

    if (checkParentForLabels) {
      if (auto P = dyn_cast_or_null<ApplyExpr>(Parent.getAsExpr())) {
        if (P->getFn() == Node.getAsExpr())
          return tryResolve(Node, NameLoc.getBaseNameLoc(),
                            LabelRangeType::CallArg,
                            getCallArgLabelRanges(getSourceMgr(), P->getArg(),
                                            LabelRangeEndAt::BeforeElemStart));
      }
    }
  }

  return tryResolve(Node, NameLoc.getBaseNameLoc());
}

bool NameMatcher::tryResolve(ASTWalker::ParentTy Node, SourceLoc NameLoc) {
  assert(!isDone());
  return tryResolve(Node, NameLoc, LabelRangeType::None, None);
}

bool NameMatcher::tryResolve(ASTWalker::ParentTy Node, SourceLoc NameLoc,
                             LabelRangeType RangeType,
                             ArrayRef<CharSourceRange> LabelRanges) {
  skipLocsBefore(NameLoc);
  if (isDone())
    return false;

  CharSourceRange Range = Lexer::getCharSourceRangeFromSourceRange(getSourceMgr(),
                                                                   NameLoc);
  UnresolvedLoc &Next = LocsToResolve.back();
  bool WasResolved = false;
  if (Range.isValid()) {
    if (NameLoc == Next.Loc) {
      LocsToResolve.pop_back();
      ResolvedLocs.push_back({Node, Range, LabelRanges, RangeType,
        isActive(), isInSelector()});
      if (isDone())
        return true;
      WasResolved = true;
    }

    if (Range.getByteLength() > 1 &&
        (Range.str().front() == '_' || Range.str().front() == '$')) {
      // Also try after any leading _ or $ for name references of wrapped
      // properties, e.g. 'foo' in '_foo' and '$foo' occurrences.
      auto NewRange = CharSourceRange(Range.getStart().getAdvancedLoc(1),
                                      Range.getByteLength() - 1);
      if (NewRange.getStart() == Next.Loc) {
        LocsToResolve.pop_back();
        ResolvedLocs.push_back({Node, NewRange, {}, LabelRangeType::None,
          isActive(), isInSelector()});
        WasResolved = true;
      }
    }
  }
  return WasResolved;
};

void ResolvedRangeInfo::print(llvm::raw_ostream &OS) const {
  OS << "<Kind>";
  switch (Kind) {
  case RangeKind::SingleExpression: OS << "SingleExpression"; break;
  case RangeKind::SingleDecl: OS << "SingleDecl"; break;
  case RangeKind::MultiTypeMemberDecl: OS << "MultiTypeMemberDecl"; break;
  case RangeKind::MultiStatement: OS << "MultiStatement"; break;
  case RangeKind::PartOfExpression: OS << "PartOfExpression"; break;
  case RangeKind::SingleStatement: OS << "SingleStatement"; break;
  case RangeKind::Invalid: OS << "Invalid"; break;
  }
  OS << "</Kind>\n";

  OS << "<Content>" << ContentRange.str() << "</Content>\n";

  if (auto Ty = getType()) {
    OS << "<Type>";
    Ty->print(OS);
    OS << "</Type>";
    switch(exit()) {
    case ExitState::Positive: OS << "<Exit>true</Exit>"; break;
    case ExitState::Unsure: OS << "<Exit>unsure</Exit>"; break;
    case ExitState::Negative: OS << "<Exit>false</Exit>"; break;
    }
    OS << "\n";
  }

  if (RangeContext) {
    OS << "<Context>";
    printContext(OS, RangeContext);
    OS << "</Context>\n";
  }

  if (CommonExprParent) {
    OS << "<Parent>";
    OS << Expr::getKindName(CommonExprParent->getKind());
    OS << "</Parent>\n";
  }

  if (!HasSingleEntry) {
    OS << "<Entry>Multi</Entry>\n";
  }

  if (ThrowingUnhandledError) {
    OS << "<Error>Throwing</Error>\n";
  }

  if (Orphan != OrphanKind::None) {
    OS << "<Orphan>";
    switch (Orphan) {
    case OrphanKind::Continue:
      OS << "Continue";
      break;
    case OrphanKind::Break:
      OS << "Break";
      break;
    case OrphanKind::None:
      llvm_unreachable("cannot enter here.");
    }
    OS << "</Orphan>";
  }

  for (auto &VD : DeclaredDecls) {
    OS << "<Declared>" << VD.VD->getBaseName() << "</Declared>";
    OS << "<OutscopeReference>";
    if (VD.ReferredAfterRange)
      OS << "true";
    else
      OS << "false";
    OS << "</OutscopeReference>\n";
  }
  for (auto &RD : ReferencedDecls) {
    OS << "<Referenced>" << RD.VD->getBaseName() << "</Referenced>";
    OS << "<Type>";
    RD.Ty->print(OS);
    OS << "</Type>\n";
  }

  OS << "<ASTNodes>" << ContainedNodes.size() << "</ASTNodes>\n";
  OS << "<end>\n";
}

CharSourceRange ResolvedRangeInfo::
calculateContentRange(ArrayRef<Token> Tokens) {
  if (Tokens.empty())
    return CharSourceRange();
  auto StartTok = Tokens.front();
  auto EndTok = Tokens.back();
  auto StartLoc = StartTok.hasComment() ?
    StartTok.getCommentStart() : StartTok.getLoc();
  auto EndLoc = EndTok.getRange().getEnd();
  auto Length = static_cast<const char *>(EndLoc.getOpaquePointerValue()) -
                static_cast<const char *>(StartLoc.getOpaquePointerValue());
  return CharSourceRange(StartLoc, Length);
}

bool DeclaredDecl::operator==(const DeclaredDecl& Other) {
  return VD == Other.VD;
}

ReturnInfo::
ReturnInfo(ASTContext &Ctx, ArrayRef<ReturnInfo> Branches):
    ReturnType(Ctx.TheErrorType.getPointer()), Exit(ExitState::Unsure) {
  std::set<TypeBase*> AllTypes;
  std::set<ExitState> AllExitStates;
  for (auto I : Branches) {
    AllTypes.insert(I.ReturnType);
    AllExitStates.insert(I.Exit);
  }
  if (AllTypes.size() == 1) {
    ReturnType = *AllTypes.begin();
  }
  if (AllExitStates.size() == 1) {
    Exit = *AllExitStates.begin();
  }
}

void swift::ide::getLocationInfoForClangNode(ClangNode ClangNode,
                                             ClangImporter *Importer,
                  llvm::Optional<std::pair<unsigned, unsigned>> &DeclarationLoc,
                                             StringRef &Filename) {
  clang::ASTContext &ClangCtx = Importer->getClangASTContext();
  clang::SourceManager &ClangSM = ClangCtx.getSourceManager();

  clang::SourceRange SR = ClangNode.getLocation();
  if (auto MD = dyn_cast_or_null<clang::ObjCMethodDecl>(ClangNode.getAsDecl())) {
    SR = clang::SourceRange(MD->getSelectorStartLoc(),
                            MD->getDeclaratorEndLoc());
  }

  clang::CharSourceRange CharRange =
      clang::Lexer::makeFileCharRange(clang::CharSourceRange::getTokenRange(SR),
                                      ClangSM, ClangCtx.getLangOpts());
  if (CharRange.isInvalid())
    return;

  std::pair<clang::FileID, unsigned>
      Decomp = ClangSM.getDecomposedLoc(CharRange.getBegin());
  if (!Decomp.first.isInvalid()) {
    if (auto FE = ClangSM.getFileEntryForID(Decomp.first)) {
      Filename = FE->getName();

      std::pair<clang::FileID, unsigned>
          EndDecomp = ClangSM.getDecomposedLoc(CharRange.getEnd());

      DeclarationLoc = { Decomp.second, EndDecomp.second-Decomp.second };
    }
  }
}

static unsigned getCharLength(SourceManager &SM, SourceRange TokenRange) {
  SourceLoc CharEndLoc = Lexer::getLocForEndOfToken(SM, TokenRange.End);
  return SM.getByteDistance(TokenRange.Start, CharEndLoc);
}

void swift::ide::getLocationInfo(const ValueDecl *VD,
                  llvm::Optional<std::pair<unsigned, unsigned>> &DeclarationLoc,
                                 StringRef &Filename) {
  ASTContext &Ctx = VD->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;

  auto ClangNode = VD->getClangNode();

  if (VD->getLoc().isValid()) {
    unsigned NameLen;
    if (auto FD = dyn_cast<AbstractFunctionDecl>(VD)) {
      SourceRange R = FD->getSignatureSourceRange();
      if (R.isInvalid())
        return;
      NameLen = getCharLength(SM, R);
    } else {
      if (VD->hasName()) {
        NameLen = VD->getBaseName().userFacingName().size();
      } else {
        NameLen = getCharLength(SM, VD->getLoc());
      }
    }

    unsigned DeclBufID = SM.findBufferContainingLoc(VD->getLoc());
    DeclarationLoc = { SM.getLocOffsetInBuffer(VD->getLoc(), DeclBufID),
                       NameLen };
    Filename = SM.getIdentifierForBuffer(DeclBufID);

  } else if (ClangNode) {
    ClangImporter *Importer =
        static_cast<ClangImporter*>(Ctx.getClangModuleLoader());
    return getLocationInfoForClangNode(ClangNode, Importer,
                                       DeclarationLoc, Filename);
  }
}

CharSourceRange CallArgInfo::getEntireCharRange(const SourceManager &SM) const {
  return CharSourceRange(SM, LabelRange.getStart(),
                         Lexer::getLocForEndOfToken(SM, ArgExp->getEndLoc()));
}

static Expr* getSingleNonImplicitChild(Expr *Parent) {
  // If this expr is non-implicit, we are done.
  if (!Parent->isImplicit())
    return Parent;

  // Collect all immediate children.
  llvm::SmallVector<Expr*, 4> Children;
  Parent->forEachImmediateChildExpr([&](Expr *E) {
    Children.push_back(E);
    return E;
  });

  // If more than one children are found, we are not sure the non-implicit node.
  if (Children.size() != 1)
    return Parent;

  // Dig deeper if necessary.
  return getSingleNonImplicitChild(Children[0]);
}

std::vector<CallArgInfo> swift::ide::
getCallArgInfo(SourceManager &SM, Expr *Arg, LabelRangeEndAt EndKind) {
  std::vector<CallArgInfo> InfoVec;
  if (auto *TE = dyn_cast<TupleExpr>(Arg)) {
    size_t ElemIndex = 0;
    for (Expr *Elem : TE->getElements()) {
      SourceLoc LabelStart(Elem->getStartLoc());
      SourceLoc LabelEnd(LabelStart);

      auto NameIdentifier = TE->getElementName(ElemIndex);
      if (!NameIdentifier.empty()) {
        LabelStart = TE->getElementNameLoc(ElemIndex);
        if (EndKind == LabelRangeEndAt::LabelNameOnly)
          LabelEnd = LabelStart.getAdvancedLoc(NameIdentifier.getLength());
      }
      bool IsTrailingClosure = TE->hasTrailingClosure() &&
        ElemIndex == TE->getNumElements() - 1;
      InfoVec.push_back({getSingleNonImplicitChild(Elem),
        CharSourceRange(SM, LabelStart, LabelEnd), IsTrailingClosure});
      ++ElemIndex;
    }
  } else if (auto *PE = dyn_cast<ParenExpr>(Arg)) {
    if (auto Sub = PE->getSubExpr())
      InfoVec.push_back({getSingleNonImplicitChild(Sub),
        CharSourceRange(Sub->getStartLoc(), 0),
        PE->hasTrailingClosure()
      });
  }
  return InfoVec;
}

std::vector<CharSourceRange> swift::ide::
getCallArgLabelRanges(SourceManager &SM, Expr *Arg, LabelRangeEndAt EndKind) {
  std::vector<CharSourceRange> Ranges;
  auto InfoVec = getCallArgInfo(SM, Arg, EndKind);

  auto EndWithoutTrailing = std::remove_if(InfoVec.begin(), InfoVec.end(),
                                           [](CallArgInfo &Info) {
                                             return Info.IsTrailingClosure;
                                           });
  std::transform(InfoVec.begin(), EndWithoutTrailing,
                 std::back_inserter(Ranges),
                 [](CallArgInfo &Info) { return Info.LabelRange; });
  return Ranges;
}
