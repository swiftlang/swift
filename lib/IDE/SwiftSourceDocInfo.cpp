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

SourceManager &CursorInfoResolver::getSourceMgr() const
{
  return SrcFile.getASTContext().SourceMgr;
}

bool CursorInfoResolver::tryResolve(ValueDecl *D, TypeDecl *CtorTyRef,
                                 ExtensionDecl *ExtTyRef, SourceLoc Loc,
                                 bool IsRef, Type Ty) {
  if (!D->hasName())
    return false;

  if (Loc == LocToResolve) {
    CursorInfo.setValueRef(D, CtorTyRef, ExtTyRef, IsRef, Ty, ContainerType);
    return true;
  }
  return false;
}

bool CursorInfoResolver::tryResolve(ModuleEntity Mod, SourceLoc Loc) {
  if (Loc == LocToResolve) {
    CursorInfo.setModuleRef(Mod);
    return true;
  }
  return false;
}

bool CursorInfoResolver::tryResolve(Stmt *St) {
  if (auto *LST = dyn_cast<LabeledStmt>(St)) {
    if (LST->getStartLoc() == LocToResolve) {
      CursorInfo.setTrailingStmt(St);
      return true;
    }
  }
  if (auto *CS = dyn_cast<CaseStmt>(St)) {
    if (CS->getStartLoc() == LocToResolve) {
      CursorInfo.setTrailingStmt(St);
      return true;
    }
  }
  return false;
}

bool CursorInfoResolver::visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                                                 Optional<AccessKind> AccKind,
                                                 bool IsOpenBracket) {
  // We should treat both open and close brackets equally
  return visitDeclReference(D, Range, nullptr, nullptr, Type(),
                    ReferenceMetaData(SemaReferenceKind::SubscriptRef, AccKind));
}

ResolvedCursorInfo CursorInfoResolver::resolve(SourceLoc Loc) {
  assert(Loc.isValid());
  LocToResolve = Loc;
  CursorInfo.Loc = Loc;
  walk(SrcFile);
  return CursorInfo;
}

bool CursorInfoResolver::walkToDeclPre(Decl *D, CharSourceRange Range) {
  if (!rangeContainsLoc(D->getSourceRange()))
    return false;

  if (isa<ExtensionDecl>(D))
    return true;

  if (auto *VD = dyn_cast<ValueDecl>(D))
    return !tryResolve(VD, /*CtorTyRef=*/nullptr, /*ExtTyRef=*/nullptr,
                       Range.getStart(), /*IsRef=*/false);

  return true;
}

bool CursorInfoResolver::walkToDeclPost(Decl *D) {
  if (isDone())
    return false;
  if (getSourceMgr().isBeforeInBuffer(LocToResolve, D->getStartLoc()))
    return false;
  return true;
}

bool CursorInfoResolver::walkToStmtPre(Stmt *S) {
  // Getting the character range for the statement, to account for interpolation
  // strings. The token range for the interpolation string is the whole string,
  // with begin/end locations pointing at the beginning of the string, so if
  // there is a token location inside the string, it will seem as if it is out
  // of the source range, unless we convert to character range.

  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() &&
      !rangeContainsLoc(Lexer::getCharSourceRangeFromSourceRange(
          getSourceMgr(), S->getSourceRange())))
    return false;
  return !tryResolve(S);
}

bool CursorInfoResolver::walkToStmtPost(Stmt *S) {
  if (isDone())
    return false;
  // FIXME: Even implicit Stmts should have proper ranges that include any
  // non-implicit Stmts (fix Stmts created for lazy vars).
  if (!S->isImplicit() && getSourceMgr().isBeforeInBuffer(LocToResolve,
                                                          S->getStartLoc()))
    return false;
  return true;
}

bool CursorInfoResolver::visitDeclReference(ValueDecl *D,
                                            CharSourceRange Range,
                                            TypeDecl *CtorTyRef,
                                            ExtensionDecl *ExtTyRef, Type T,
                                            ReferenceMetaData Data) {
  if (isDone())
    return false;
  return !tryResolve(D, CtorTyRef, ExtTyRef, Range.getStart(), /*IsRef=*/true, T);
}

bool CursorInfoResolver::walkToExprPre(Expr *E) {
  if (!isDone()) {
    if (auto SAE = dyn_cast<SelfApplyExpr>(E)) {
      if (SAE->getFn()->getStartLoc() == LocToResolve) {
        ContainerType = SAE->getBase()->getType();
      }
    } else if (auto ME = dyn_cast<MemberRefExpr>(E)) {
      SourceLoc MemberLoc = ME->getNameLoc().getBaseNameLoc();
      if (MemberLoc.isValid() && MemberLoc == LocToResolve) {
        ContainerType = ME->getBase()->getType();
      }
    }
    auto IsProperCursorLocation = E->getStartLoc() == LocToResolve;
    // Handle cursor placement after `try` in ForceTry and OptionalTry Expr.
    auto CheckLocation = [&IsProperCursorLocation, this](SourceLoc Loc) {
      IsProperCursorLocation = Loc == LocToResolve || IsProperCursorLocation;
    };
    if (auto *FTE = dyn_cast<ForceTryExpr>(E)) {
      CheckLocation(FTE->getExclaimLoc());
    }
    if (auto *OTE = dyn_cast<OptionalTryExpr>(E)) {
      CheckLocation(OTE->getQuestionLoc());
    }
    // Keep track of trailing expressions.
    if (!E->isImplicit() && IsProperCursorLocation)
      TrailingExprStack.push_back(E);
  }
  return true;
}

bool CursorInfoResolver::walkToExprPost(Expr *E) {
  if (isDone())
    return false;
  if (!TrailingExprStack.empty() && TrailingExprStack.back() == E) {
    // We return the outtermost expression in the token info.
    CursorInfo.setTrailingExpr(TrailingExprStack.front());
    return false;
  }
  return true;
}

bool CursorInfoResolver::visitCallArgName(Identifier Name,
                                          CharSourceRange Range,
                                          ValueDecl *D) {
  if (isDone())
    return false;
  bool Found = tryResolve(D, nullptr, nullptr, Range.getStart(), /*IsRef=*/true);
  if (Found)
    CursorInfo.IsKeywordArgument = true;
  return !Found;
}

bool CursorInfoResolver::
visitDeclarationArgumentName(Identifier Name, SourceLoc StartLoc, ValueDecl *D) {
  if (isDone())
    return false;
  return !tryResolve(D, nullptr, nullptr, StartLoc, /*IsRef=*/false);
}

bool CursorInfoResolver::visitModuleReference(ModuleEntity Mod,
                                              CharSourceRange Range) {
  if (isDone())
    return false;
  if (Mod.isBuiltinModule())
    return true; // Ignore.
  return !tryResolve(Mod, Range.getStart());
}

SourceManager &NameMatcher::getSourceMgr() const {
  return SrcFile.getASTContext().SourceMgr;
}

bool CursorInfoResolver::rangeContainsLoc(SourceRange Range) const {
  return getSourceMgr().rangeContainsTokenLoc(Range, LocToResolve);
}

bool CursorInfoResolver::rangeContainsLoc(CharSourceRange Range) const {
  return Range.contains(LocToResolve);
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
    
    SourceLoc LabelStart(Param->getTypeLoc().getLoc());
    SourceLoc LabelEnd(LabelStart);
    
    if (Param->getNameLoc().isValid()) {
      LabelStart = Param->getNameLoc();
    }
    LabelRanges.push_back(CharSourceRange(SM, LabelStart, LabelEnd));
  }
  return LabelRanges;
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

  if (shouldSkip(D->getSourceRange()))
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

  if (isa<ComponentIdentTypeRepr>(T))
    tryResolve(ASTWalker::ParentTy(T), T->getLoc());
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
  if (Range.isValid() && NameLoc == Next.Loc) {
    LocsToResolve.pop_back();
    ResolvedLocs.push_back({Node, Range, LabelRanges, RangeType,
      isActive(), isInSelector()});
    return true;
  }
  return false;
};

void ResolvedRangeInfo::print(llvm::raw_ostream &OS) {
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

static bool hasUnhandledError(ArrayRef<ASTNode> Nodes) {
  class ThrowingEntityAnalyzer : public SourceEntityWalker {
    bool Throwing;
  public:
    ThrowingEntityAnalyzer(): Throwing(false) {}
    bool walkToStmtPre(Stmt *S) override {
      if (auto DCS = dyn_cast<DoCatchStmt>(S)) {
        if (DCS->isSyntacticallyExhaustive())
          return false;
        Throwing = true;
      } else if (isa<ThrowStmt>(S)) {
        Throwing = true;
      }
      return !Throwing;
    }
    bool walkToExprPre(Expr *E) override {
      if (isa<TryExpr>(E)) {
        Throwing = true;
      }
      return !Throwing;
    }
    bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
      return false;
    }
    bool walkToDeclPost(Decl *D) override { return !Throwing; }
    bool walkToStmtPost(Stmt *S) override { return !Throwing; }
    bool walkToExprPost(Expr *E) override { return !Throwing; }
    bool isThrowing() { return Throwing; }
  };

  return Nodes.end() != std::find_if(Nodes.begin(), Nodes.end(), [](ASTNode N) {
    ThrowingEntityAnalyzer Analyzer;
    Analyzer.walk(N);
    return Analyzer.isThrowing();
  });
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

struct RangeResolver::Implementation {
  SourceFile &File;
  ASTContext &Ctx;
  SourceManager &SM;
private:
  enum class RangeMatchKind : int8_t {
    NoneMatch,
    StartMatch,
    EndMatch,
    RangeMatch,
  };

  struct ContextInfo {
    ASTNode Parent;

    // Whether the context is entirely contained in the given range under
    // scrutiny.
    bool ContainedInRange;
    std::vector<ASTNode> StartMatches;
    std::vector<ASTNode> EndMatches;
    ContextInfo(ASTNode Parent, bool ContainedInRange) : Parent(Parent),
      ContainedInRange(ContainedInRange) {}

    bool isMultiStatement() {
      if (StartMatches.empty() || EndMatches.empty())
        return false;

      // Multi-statement should have a common parent of brace statement, this
      // can be implicit brace statement, e.g. in case statement.
      if (Parent.isStmt(StmtKind::Brace))
        return true;

      // Explicitly allow the selection of multiple case statements.
      auto IsCase = [](ASTNode N) { return N.isStmt(StmtKind::Case); };
      return llvm::any_of(StartMatches, IsCase) &&
          llvm::any_of(EndMatches, IsCase);
    }

    bool isMultiTypeMemberDecl() {
      if (StartMatches.empty() || EndMatches.empty())
        return false;

      // Multi-decls should have the same nominal type as a common parent
      if (auto ParentDecl = Parent.dyn_cast<Decl *>())
        return isa<NominalTypeDecl>(ParentDecl);

      return false;
    }
  };


  ArrayRef<Token> TokensInRange;
  SourceLoc Start;
  SourceLoc End;

  Optional<ResolvedRangeInfo> Result;
  std::vector<ContextInfo> ContextStack;
  ContextInfo &getCurrentDC() {
    assert(!ContextStack.empty());
    return ContextStack.back();
  }

  std::vector<DeclaredDecl> DeclaredDecls;
  std::vector<ReferencedDecl> ReferencedDecls;

  // Keep track of the AST nodes contained in the range under question.
  std::vector<ASTNode> ContainedASTNodes;

  /// Collect the type that an ASTNode should be evaluated to.
  ReturnInfo resolveNodeType(ASTNode N, RangeKind Kind) {
    auto *VoidTy = Ctx.getVoidDecl()->getDeclaredInterfaceType().getPointer();
    if (N.isNull())
      return {VoidTy, ExitState::Negative};
    switch(Kind) {
    case RangeKind::Invalid:
    case RangeKind::SingleDecl:
    case RangeKind::MultiTypeMemberDecl:
    case RangeKind::PartOfExpression:
      llvm_unreachable("cannot get type.");

    // For a single expression, its type is apparent.
    case RangeKind::SingleExpression:
      return {N.get<Expr*>()->getType().getPointer(), ExitState::Negative};

    // For statements, we either resolve to the returning type or Void.
    case RangeKind::SingleStatement:
    case RangeKind::MultiStatement: {
      if (N.is<Stmt*>()) {
        if (auto RS = dyn_cast<ReturnStmt>(N.get<Stmt*>())) {
          return {
            resolveNodeType(RS->hasResult() ? RS->getResult() : nullptr,
              RangeKind::SingleExpression).ReturnType, ExitState::Positive };
        }

        // Unbox the brace statement to find its type.
        if (auto BS = dyn_cast<BraceStmt>(N.get<Stmt*>())) {
          if (!BS->getElements().empty()) {
            return resolveNodeType(BS->getElements().back(),
                                   RangeKind::SingleStatement);
          }
        }

        // Unbox the if statement to find its type.
        if (auto *IS = dyn_cast<IfStmt>(N.get<Stmt*>())) {
          llvm::SmallVector<ReturnInfo, 2> Branches;
          Branches.push_back(resolveNodeType(IS->getThenStmt(),
                             RangeKind::SingleStatement));
          Branches.push_back(resolveNodeType(IS->getElseStmt(),
                             RangeKind::SingleStatement));
          return {Ctx, Branches};
        }

        // Unbox switch statement to find return information.
        if (auto *SWS = dyn_cast<SwitchStmt>(N.get<Stmt*>())) {
          llvm::SmallVector<ReturnInfo, 4> Branches;
          for (auto *CS : SWS->getCases()) {
            Branches.push_back(resolveNodeType(CS->getBody(),
              RangeKind::SingleStatement));
          }
          return {Ctx, Branches};
        }
      }
      // For other statements, the type should be void.
      return {VoidTy, ExitState::Negative};
    }
    }
    llvm_unreachable("unhandled kind");
  }

  ResolvedRangeInfo getSingleNodeKind(ASTNode Node) {
    assert(!Node.isNull());
    assert(ContainedASTNodes.size() == 1);
    // Single node implies single entry point, or is it?
    bool SingleEntry = true;
    bool UnhandledError = hasUnhandledError({Node});
    OrphanKind Kind = getOrphanKind(ContainedASTNodes);
    if (Node.is<Expr*>())
      return ResolvedRangeInfo(RangeKind::SingleExpression,
                               resolveNodeType(Node, RangeKind::SingleExpression),
                               TokensInRange,
                               getImmediateContext(),
                               /*Common Parent Expr*/nullptr,
                               SingleEntry,
                               UnhandledError, Kind,
                               llvm::makeArrayRef(ContainedASTNodes),
                               llvm::makeArrayRef(DeclaredDecls),
                               llvm::makeArrayRef(ReferencedDecls));
    else if (Node.is<Stmt*>())
      return ResolvedRangeInfo(RangeKind::SingleStatement,
                               resolveNodeType(Node, RangeKind::SingleStatement),
                               TokensInRange,
                               getImmediateContext(),
                               /*Common Parent Expr*/nullptr,
                               SingleEntry,
                               UnhandledError, Kind,
                               llvm::makeArrayRef(ContainedASTNodes),
                               llvm::makeArrayRef(DeclaredDecls),
                               llvm::makeArrayRef(ReferencedDecls));
    else {
      assert(Node.is<Decl*>());
      return ResolvedRangeInfo(RangeKind::SingleDecl,
                               ReturnInfo(),
                               TokensInRange,
                               getImmediateContext(),
                               /*Common Parent Expr*/nullptr,
                               SingleEntry,
                               UnhandledError, Kind,
                               llvm::makeArrayRef(ContainedASTNodes),
                               llvm::makeArrayRef(DeclaredDecls),
                               llvm::makeArrayRef(ReferencedDecls));
    }
  }

  bool isContainedInSelection(CharSourceRange Range) {
    if (SM.isBeforeInBuffer(Range.getStart(), Start))
      return false;
    if (SM.isBeforeInBuffer(End, Range.getEnd()))
      return false;
    return true;
  }

  DeclContext *getImmediateContext() {
    for (auto It = ContextStack.rbegin(); It != ContextStack.rend(); It ++) {
      if (auto *DC = It->Parent.getAsDeclContext())
        return DC;
    }
    return static_cast<DeclContext*>(&File);
  }

  Implementation(SourceFile &File, ArrayRef<Token> TokensInRange) :
    File(File), Ctx(File.getASTContext()), SM(Ctx.SourceMgr),
    TokensInRange(TokensInRange),
    Start(TokensInRange.front().getLoc()),
    End(TokensInRange.back().getLoc()) {
      assert(Start.isValid() && End.isValid());
  }

public:
  bool hasResult() { return Result.hasValue(); }

  void enter(ASTNode Node) {
    bool ContainedInRange;
    if (!Node.getOpaqueValue()) {
      // If the node is the root, it's not contained for sure.
      ContainedInRange = false;
    } else if (ContextStack.back().ContainedInRange) {
      // If the node's parent is contained in the range, so is the node.
      ContainedInRange = true;
    } else {
      // If the node's parent is not contained in the range, check if this node is.
      ContainedInRange = isContainedInSelection(CharSourceRange(SM,
                                                            Node.getStartLoc(),
                                                            Node.getEndLoc()));
    }
    ContextStack.emplace_back(Node, ContainedInRange);
  }

  void leave(ASTNode Node) {
    if (!hasResult() && !Node.isImplicit() && nodeContainSelection(Node)) {
      if (auto Parent = Node.is<Expr*>() ? Node.get<Expr*>() : nullptr) {
        Result = {
          RangeKind::PartOfExpression,
          ReturnInfo(),
          TokensInRange,
          getImmediateContext(),
          Parent,
          hasSingleEntryPoint(ContainedASTNodes),
          hasUnhandledError(ContainedASTNodes),
          getOrphanKind(ContainedASTNodes),
          llvm::makeArrayRef(ContainedASTNodes),
          llvm::makeArrayRef(DeclaredDecls),
          llvm::makeArrayRef(ReferencedDecls)
        };
      }
    }

    assert(ContextStack.back().Parent.getOpaqueValue() == Node.getOpaqueValue());
    ContextStack.pop_back();
  }

  static std::unique_ptr<Implementation>
  createInstance(SourceFile &File, unsigned StartOff, unsigned Length) {
    SourceManager &SM = File.getASTContext().SourceMgr;
    unsigned BufferId = File.getBufferID().getValue();
    auto AllTokens = File.getAllTokens();
    SourceLoc StartRaw = SM.getLocForOffset(BufferId, StartOff);
    SourceLoc EndRaw = SM.getLocForOffset(BufferId, StartOff + Length);

    // This points to the first token after or on the start loc.
    auto StartIt = token_lower_bound(AllTokens, StartRaw);

    // Skip all the comments.
    while(StartIt != AllTokens.end()) {
      if (StartIt->getKind() != tok::comment)
        break;
      StartIt ++;
    }

    // Erroneous case.
    if (StartIt == AllTokens.end())
      return nullptr;

    // This points to the first token after or on the end loc;
    auto EndIt = token_lower_bound(AllTokens, EndRaw);

    // Adjust end token to skip comments.
    while (EndIt != AllTokens.begin()) {
      EndIt --;
      if (EndIt->getKind() != tok::comment)
        break;
    }

    // Erroneous case.
    if (EndIt < StartIt)
      return nullptr;

    unsigned StartIdx = StartIt - AllTokens.begin();
    return std::unique_ptr<Implementation>(new Implementation(File,
      AllTokens.slice(StartIdx, EndIt - StartIt + 1)));
  }

  static std::unique_ptr<Implementation>
  createInstance(SourceFile &File, SourceLoc Start, SourceLoc End) {
    if (Start.isInvalid() || End.isInvalid())
      return nullptr;
    SourceManager &SM = File.getASTContext().SourceMgr;
    unsigned BufferId = File.getBufferID().getValue();
    unsigned StartOff = SM.getLocOffsetInBuffer(Start, BufferId);
    unsigned EndOff = SM.getLocOffsetInBuffer(End, BufferId);
    return createInstance(File, StartOff, EndOff - StartOff);
  }

  void analyzeDecl(Decl *D) {
    // Collect declared decls in the range.
    if (auto *VD = dyn_cast_or_null<ValueDecl>(D)) {
      if (isContainedInSelection(CharSourceRange(SM, VD->getStartLoc(),
                                                 VD->getEndLoc())))
        if (std::find(DeclaredDecls.begin(), DeclaredDecls.end(),
                      DeclaredDecl(VD)) == DeclaredDecls.end())
          DeclaredDecls.push_back(VD);
    }
  }

  class CompleteWalker : public SourceEntityWalker {
    Implementation *Impl;
    bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
      if (D->isImplicit())
        return false;
      Impl->analyzeDecl(D);
      return true;
    }
    bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                            TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                            ReferenceMetaData Data) override {
      Impl->analyzeDeclRef(D, Range.getStart(), T, Data);
      return true;
    }
  public:
    CompleteWalker(Implementation *Impl) : Impl(Impl) {}
  };

  /// This walker walk the current decl context and analyze whether declared
  /// decls in the range is referenced after it.
  class FurtherReferenceWalker : public SourceEntityWalker {
    Implementation *Impl;
    bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                            TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                            ReferenceMetaData Data) override {
      // If the reference is after the given range, continue logic.
      if (!Impl->SM.isBeforeInBuffer(Impl->End, Range.getStart()))
        return true;

      // If the referenced decl is declared in the range, than the declared decl
      // is referenced out of scope/range.
      auto It = std::find(Impl->DeclaredDecls.begin(),
                          Impl->DeclaredDecls.end(), D);
      if (It != Impl->DeclaredDecls.end()) {
        It->ReferredAfterRange = true;
      }
      return true;
    }
  public:
    FurtherReferenceWalker(Implementation *Impl) : Impl(Impl) {}
  };

  void postAnalysis(ASTNode EndNode) {
    // Visit the content of this node thoroughly, because the walker may
    // abort early.
    CompleteWalker(this).walk(EndNode);

    // Analyze whether declared decls in the range is referenced outside of it.
    FurtherReferenceWalker(this).walk(getImmediateContext());
  }

  bool hasSingleEntryPoint(ArrayRef<ASTNode> Nodes) {
    unsigned CaseCount = 0;
    // Count the number of case/default statements.
    for (auto N : Nodes) {
      if (Stmt *S = N.is<Stmt*>() ? N.get<Stmt*>() : nullptr) {
        if (S->getKind() == StmtKind::Case)
          CaseCount++;
      }
    }
    // If there are more than one case/default statements, there are more than
    // one entry point.
    return CaseCount == 0;
  }

  OrphanKind getOrphanKind(ArrayRef<ASTNode> Nodes) {
    if (Nodes.empty())
      return OrphanKind::None;

    // Prepare the entire range.
    SourceRange WholeRange(Nodes.front().getStartLoc(),
                           Nodes.back().getEndLoc());
    struct ControlFlowStmtSelector : public SourceEntityWalker {
      std::vector<std::pair<SourceRange, OrphanKind>> Ranges;
      bool walkToStmtPre(Stmt *S) override {
        // For each continue/break statement, record its target's range and the
        // orphan kind.
        if (auto *CS = dyn_cast<ContinueStmt>(S)) {
          if (auto *Target = CS->getTarget()) {
            Ranges.emplace_back(Target->getSourceRange(), OrphanKind::Continue);
          }
        } else if (auto *BS = dyn_cast<BreakStmt>(S)) {
          if (auto *Target = BS->getTarget()) {
            Ranges.emplace_back(Target->getSourceRange(), OrphanKind::Break);
          }
        }
        return true;
      }
    };
    for (auto N : Nodes) {
      ControlFlowStmtSelector TheWalker;
      TheWalker.walk(N);
      for (auto Pair : TheWalker.Ranges) {

        // If the entire range does not include the target's range, we find
        // an orphan.
        if (!SM.rangeContains(WholeRange, Pair.first))
          return Pair.second;
      }
    }

    // We find no orphan.
    return OrphanKind::None;
  }

  void analyze(ASTNode Node) {
    if (!shouldAnalyze(Node))
      return;
    Decl *D = Node.is<Decl*>() ? Node.get<Decl*>() : nullptr;
    analyzeDecl(D);
    auto &DCInfo = getCurrentDC();

    auto NodeRange = Node.getSourceRange();

    // Widen the node's source range to include all attributes to get a range
    // match if a function with its attributes has been selected.
    if (auto D = Node.dyn_cast<Decl *>())
      NodeRange = D->getSourceRangeIncludingAttrs();

    switch (getRangeMatchKind(NodeRange)) {
    case RangeMatchKind::NoneMatch: {
      // PatternBindingDecl is not visited; we need to explicitly analyze here.
      if (auto *VA = dyn_cast_or_null<VarDecl>(D))
        if (auto PBD = VA->getParentPatternBinding())
          analyze(PBD);
      break;
    }
    case RangeMatchKind::RangeMatch: {
      postAnalysis(Node);

      // The node is contained in the given range.
      ContainedASTNodes.push_back(Node);
      Result = getSingleNodeKind(Node);
      return;
    }
    case RangeMatchKind::StartMatch:
      DCInfo.StartMatches.emplace_back(Node);
      break;
    case RangeMatchKind::EndMatch:
      DCInfo.EndMatches.emplace_back(Node);
      break;
    }

    // If no parent is considered as a contained node; this node should be
    // a top-level contained node.
    if (std::none_of(ContainedASTNodes.begin(), ContainedASTNodes.end(),
      [&](ASTNode N) { return SM.rangeContains(N.getSourceRange(),
                                               Node.getSourceRange()); })) {
      ContainedASTNodes.push_back(Node);
    }

    if (DCInfo.isMultiStatement()) {
      postAnalysis(DCInfo.EndMatches.back());
      Result = {RangeKind::MultiStatement,
                /* Last node has the type */
                resolveNodeType(DCInfo.EndMatches.back(),
                                RangeKind::MultiStatement),
                TokensInRange,
                getImmediateContext(), nullptr,
                hasSingleEntryPoint(ContainedASTNodes),
                hasUnhandledError(ContainedASTNodes),
                getOrphanKind(ContainedASTNodes),
                llvm::makeArrayRef(ContainedASTNodes),
                llvm::makeArrayRef(DeclaredDecls),
                llvm::makeArrayRef(ReferencedDecls)};
    }

    if (DCInfo.isMultiTypeMemberDecl()) {
      postAnalysis(DCInfo.EndMatches.back());
      Result = {RangeKind::MultiTypeMemberDecl,
                ReturnInfo(),
                TokensInRange,
                getImmediateContext(),
                /*Common Parent Expr*/ nullptr,
                /*SinleEntry*/ true,
                hasUnhandledError(ContainedASTNodes),
                getOrphanKind(ContainedASTNodes),
                llvm::makeArrayRef(ContainedASTNodes),
                llvm::makeArrayRef(DeclaredDecls),
                llvm::makeArrayRef(ReferencedDecls)};
    }
  }

  bool shouldEnter(ASTNode Node) {
    if (hasResult())
      return false;
    if (SM.isBeforeInBuffer(End, Node.getSourceRange().Start))
      return false;
    if (SM.isBeforeInBuffer(Node.getSourceRange().End, Start))
      return false;
    return true;
  }

  bool nodeContainSelection(ASTNode Node) {
    // If the selection starts before the node, return false.
    if (SM.isBeforeInBuffer(Start, Node.getStartLoc()))
      return false;
    // If the node ends before the selection, return false.
    if (SM.isBeforeInBuffer(Lexer::getLocForEndOfToken(SM, Node.getEndLoc()), End))
      return false;
    // Contained.
    return true;
  }

  bool shouldAnalyze(ASTNode Node) {
    // Avoid analyzing implicit nodes.
    if (Node.isImplicit())
      return false;
    // Avoid analyzing nodes that are not enclosed.
    if (SM.isBeforeInBuffer(End, Node.getEndLoc()))
      return false;
    if (SM.isBeforeInBuffer(Node.getStartLoc(), Start))
      return false;
    return true;
  }

  ResolvedRangeInfo getResult() {
    if (Result.hasValue())
      return Result.getValue();
    return ResolvedRangeInfo(TokensInRange);
  }

  void analyzeDeclRef(ValueDecl *VD, SourceLoc Start, Type Ty,
                      ReferenceMetaData Data) {
    // Add defensive check in case the given type is null.
    // FIXME: we should receive error type instead of null type.
    if (Ty.isNull())
      return;

    // Only collect decl ref.
    if (Data.Kind != SemaReferenceKind::DeclRef)
      return;

    if (!isContainedInSelection(CharSourceRange(Start, 0)))
      return;

    // If the VD is declared outside of current file, exclude such decl.
    if (VD->getDeclContext()->getParentSourceFile() != &File)
      return;

    // Down-grade LValue type to RValue type if it's read-only.
    if (auto Access = Data.AccKind) {
      switch (Access.getValue()) {
      case AccessKind::Read:
        Ty = Ty->getRValueType();
        break;
      case AccessKind::Write:
      case AccessKind::ReadWrite:
        break;
      }
    }

    auto It = llvm::find_if(ReferencedDecls,
                            [&](ReferencedDecl D) { return D.VD == VD; });
    if (It == ReferencedDecls.end()) {
      ReferencedDecls.emplace_back(VD, Ty);
    } else {
      // LValue type should take precedence.
      if (!It->Ty->hasLValueType() && Ty->hasLValueType()) {
        It->Ty = Ty;
      }
    }
  }

private:
  RangeMatchKind getRangeMatchKind(SourceRange Input) {
    bool StartMatch = Input.Start == Start;
    bool EndMatch = Input.End == End;
    if (StartMatch && EndMatch)
      return RangeMatchKind::RangeMatch;
    else if (StartMatch)
      return RangeMatchKind::StartMatch;
    else if (EndMatch)
      return RangeMatchKind::EndMatch;
    else
      return RangeMatchKind::NoneMatch;
  }
};

RangeResolver::RangeResolver(SourceFile &File, SourceLoc Start, SourceLoc End) :
  Impl(Implementation::createInstance(File, Start, End)) {}

RangeResolver::RangeResolver(SourceFile &File, unsigned Offset, unsigned Length) :
  Impl(Implementation::createInstance(File, Offset, Length)) {}

RangeResolver::~RangeResolver() = default;

bool RangeResolver::walkToExprPre(Expr *E) {
  if (!Impl->shouldEnter(E))
    return false;
  Impl->analyze(E);
  Impl->enter(E);
  return true;
}

bool RangeResolver::walkToStmtPre(Stmt *S) {
  if (!Impl->shouldEnter(S))
    return false;
  Impl->analyze(S);
  Impl->enter(S);
  return true;
};

bool RangeResolver::walkToDeclPre(Decl *D, CharSourceRange Range) {
  if (D->isImplicit())
    return false;
  if (!Impl->shouldEnter(D))
    return false;
  Impl->analyze(D);
  Impl->enter(D);
  return true;
}

bool RangeResolver::walkToExprPost(Expr *E) {
  Impl->leave(E);
  return !Impl->hasResult();
}

bool RangeResolver::walkToStmtPost(Stmt *S) {
  Impl->leave(S);
  return !Impl->hasResult();
};

bool RangeResolver::walkToDeclPost(Decl *D) {
  Impl->leave(D);
  return !Impl->hasResult();
}


bool RangeResolver::
visitDeclReference(ValueDecl *D, CharSourceRange Range, TypeDecl *CtorTyRef,
                   ExtensionDecl *ExtTyRef, Type T, ReferenceMetaData Data) {
  Impl->analyzeDeclRef(D, Range.getStart(), T, Data);
  return true;
}

ResolvedRangeInfo RangeResolver::resolve() {
  if (!Impl)
    return ResolvedRangeInfo({});
  Impl->enter(ASTNode());
  walk(Impl->File);
  return Impl->getResult();
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

      InfoVec.push_back({getSingleNonImplicitChild(Elem),
        CharSourceRange(SM, LabelStart, LabelEnd)});
      ++ElemIndex;
    }
  } else if (auto *PE = dyn_cast<ParenExpr>(Arg)) {
    if (auto Sub = PE->getSubExpr())
      InfoVec.push_back({getSingleNonImplicitChild(Sub),
        CharSourceRange(Sub->getStartLoc(), 0)});
  }
  return InfoVec;
}

std::vector<CharSourceRange> swift::ide::
getCallArgLabelRanges(SourceManager &SM, Expr *Arg, LabelRangeEndAt EndKind) {
  std::vector<CharSourceRange> Ranges;
  auto InfoVec = getCallArgInfo(SM, Arg, EndKind);
  std::transform(InfoVec.begin(), InfoVec.end(), std::back_inserter(Ranges),
                 [](CallArgInfo &Info) { return Info.LabelRange; });
  return Ranges;
}
