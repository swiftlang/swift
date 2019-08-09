//===--- ASTScopeSourceRange.cpp - Swift Object-Oriented AST Scope --------===//
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
///
/// This file implements the source range queries for the ASTScopeImpl ontology.
///
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTScope.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>

using namespace swift;
using namespace ast_scope;

static SourceLoc getStartOfFirstParam(ClosureExpr *closure);

SourceRange ASTScopeImpl::widenSourceRangeForIgnoredASTNodes(
    const SourceRange range) const {
  if (range.isInvalid())
    return sourceRangeOfIgnoredASTNodes;
  auto r = range;
  if (sourceRangeOfIgnoredASTNodes.isValid())
    r.widen(sourceRangeOfIgnoredASTNodes);
  return r;
}

SourceRange
ASTScopeImpl::widenSourceRangeForChildren(const SourceRange range,
                                          const bool omitAssertions) const {
  if (getChildren().empty()) {
    assert(omitAssertions || range.Start.isValid());
    return range;
  }
  const auto childStart =
      getChildren().front()->getSourceRange(omitAssertions).Start;
  const auto childEnd =
      getChildren().back()->getSourceRange(omitAssertions).End;
  auto childRange = SourceRange(childStart, childEnd);
  assert(omitAssertions || childRange.isValid());

  if (range.isInvalid())
    return childRange;
  auto r = range;
  r.widen(childRange);
  return r;
}

#pragma mark validation

bool ASTScopeImpl::hasValidSourceRange() const {
  const auto sourceRange = getSourceRange();
  return sourceRange.Start.isValid() && sourceRange.End.isValid() &&
         !getSourceManager().isBeforeInBuffer(sourceRange.End,
                                              sourceRange.Start);
}

bool ASTScopeImpl::hasValidSourceRangeOfIgnoredASTNodes() const {
  return sourceRangeOfIgnoredASTNodes.isValid();
}

bool ASTScopeImpl::precedesInSource(const ASTScopeImpl *next) const {
  if (!hasValidSourceRange() || !next->hasValidSourceRange())
    return false;
  return !getSourceManager().isBeforeInBuffer(next->getSourceRange().Start,
                                              getSourceRange().End);
}

bool ASTScopeImpl::verifyThatChildrenAreContainedWithin(
    const SourceRange range) const {
  // assumes children are already in order
  if (getChildren().empty())
    return true;
  const SourceRange rangeOfChildren =
      SourceRange(getChildren().front()->getSourceRange().Start,
                  getChildren().back()->getSourceRange().End);
  if (getSourceManager().rangeContains(range, rangeOfChildren))
    return true;
  auto &out = verificationError() << "children not contained in its parent\n";
  if (getChildren().size() == 1) {
    out << "\n***Only Child node***\n";
    getChildren().front()->print(out);
  } else {
    out << "\n***First Child node***\n";
    getChildren().front()->print(out);
    out << "\n***Last Child node***\n";
    getChildren().back()->print(out);
  }
  out << "\n***Parent node***\n";
  this->print(out);
  abort();
}

bool ASTScopeImpl::verifyThatThisNodeComeAfterItsPriorSibling() const {
  auto priorSibling = getPriorSibling();
  if (!priorSibling)
    return true;
  if (priorSibling.get()->precedesInSource(this))
    return true;
  auto &out = verificationError() << "unexpected out-of-order nodes\n";
  out << "\n***Penultimate child node***\n";
  priorSibling.get()->print(out);
  out << "\n***Last Child node***\n";
  print(out);
  out << "\n***Parent node***\n";
  getParent().get()->print(out);
  //  llvm::errs() << "\n\nsource:\n"
  //               << getSourceManager()
  //                      .getRangeForBuffer(
  //                          getSourceFile()->getBufferID().getValue())
  //                      .str();
  assert(false && "unexpected out-of-order nodes");
}

NullablePtr<ASTScopeImpl> ASTScopeImpl::getPriorSibling() const {
  auto parent = getParent();
  if (!parent)
    return nullptr;
  auto const &siblingsAndMe = parent.get()->getChildren();
  // find myIndex, which is probably the last one
  int myIndex = -1;
  for (int i = siblingsAndMe.size() - 1; i >= 0; --i) {
    if (siblingsAndMe[i] == this) {
      myIndex = i;
      break;
    }
  }
  assert(myIndex != -1 && "I have been disowned!");
  if (myIndex == 0)
    return nullptr;
  return siblingsAndMe[myIndex - 1];
}

#pragma mark getChildlessSourceRange

SourceRange SpecializeAttributeScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  return specializeAttr->getRange();
}

SourceRange AbstractFunctionBodyScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  return decl->getBodySourceRange();
}

SourceRange
TopLevelCodeScope::getChildlessSourceRange(const bool omitAssertions) const {
  return decl->getSourceRange();
}

SourceRange
SubscriptDeclScope::getChildlessSourceRange(const bool omitAssertions) const {
  return decl->getSourceRange();
}

SourceRange
EnumElementScope::getChildlessSourceRange(const bool omitAssertions) const {
  return decl->getSourceRange();
}

SourceRange
WholeClosureScope::getChildlessSourceRange(const bool omitAssertions) const {
  return closureExpr->getSourceRange();
}

SourceRange
AbstractStmtScope::getChildlessSourceRange(const bool omitAssertions) const {
  return getStmt()->getSourceRange();
}

SourceRange DefaultArgumentInitializerScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  if (auto *dv = decl->getDefaultValue())
    return dv->getSourceRange();
  return SourceRange();
}

SourceRange PatternEntryDeclScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  // TODO: Once rdar://53627317 is accomplished, the following may be able to be
  // simplified.
  if (!getChildren().empty()) { // why needed???
    bool hasOne = false;
    getPattern()->forEachVariable([&](VarDecl *) { hasOne = true; });
    if (!hasOne)
      return SourceRange(); // just the init
    if (!getPatternEntry().getInit())
      return SourceRange(); // just the var decls
  }
  return getPatternEntry().getSourceRange();
}

SourceRange PatternEntryInitializerScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  // See rdar://53921703
  // Note: grep for "When the initializer is removed we don't actually clear the
  // pointer" because we do!
  return initAsWrittenWhenCreated->getSourceRange();
}

SourceRange
VarDeclScope::getChildlessSourceRange(const bool omitAssertions) const {
  const auto br = decl->getBracesRange();
  return br.isValid() ? br : decl->getSourceRange();
}

SourceRange
GenericParamScope::getChildlessSourceRange(const bool omitAssertions) const {
  auto nOrE = holder;
  // A protocol's generic parameter list is not written in source, and
  // is visible from the start of the body.
  if (auto *protoDecl = dyn_cast<ProtocolDecl>(nOrE))
    return SourceRange(protoDecl->getBraces().Start, protoDecl->getEndLoc());
  // Explicitly-written generic parameters are in scope *following* their
  // definition and through the end of the body.
  // ensure that this isn't an extension where there is no end loc
  auto startLoc = paramList->getParams()[index]->getEndLoc();
  if (startLoc.isInvalid())
    startLoc = holder->getStartLoc();
  return SourceRange(startLoc, holder->getEndLoc());
}

SourceRange
ASTSourceFileScope::getChildlessSourceRange(const bool omitAssertions) const {
  if (auto bufferID = SF->getBufferID()) {
    auto charRange = getSourceManager().getRangeForBuffer(*bufferID);
    return SourceRange(charRange.getStart(), charRange.getEnd());
  }

  if (SF->Decls.empty())
    return SourceRange();

  // Use the source ranges of the declarations in the file.
  return SourceRange(SF->Decls.front()->getStartLoc(),
                     SF->Decls.back()->getEndLoc());
}

SourceRange GenericTypeOrExtensionScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  return portion->getChildlessSourceRangeOf(this, omitAssertions);
}

SourceRange GenericTypeOrExtensionWholePortion::getChildlessSourceRangeOf(
    const GenericTypeOrExtensionScope *scope, const bool omitAssertions) const {
  auto *d = scope->getDecl();
  auto r = d->getSourceRangeIncludingAttrs();
  if (r.Start.isValid()) {
    assert(r.End.isValid());
    return r;
  }
  return d->getSourceRange();
}

SourceRange GenericTypeOrExtensionWherePortion::getChildlessSourceRangeOf(
    const GenericTypeOrExtensionScope *scope, const bool omitAssertions) const {
  return scope->getGenericContext()->getTrailingWhereClause()->getSourceRange();
}

SourceRange IterableTypeBodyPortion::getChildlessSourceRangeOf(
    const GenericTypeOrExtensionScope *scope, const bool omitAssertions) const {
  auto *d = scope->getDecl();
  if (auto *nt = dyn_cast<NominalTypeDecl>(d))
    return nt->getBraces();
  if (auto *e = dyn_cast<ExtensionDecl>(d))
    return e->getBraces();
  if (omitAssertions)
    return SourceRange();
  llvm_unreachable("No body!");
}

SourceRange AbstractFunctionDeclScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  // For a get/put accessor	 all of the parameters are implicit, so start
  // them at the start location of the accessor.
  auto r = decl->getSourceRangeIncludingAttrs();
  if (r.Start.isValid()) {
    assert(r.End.isValid());
    return r;
  }
  return decl->getBodySourceRange();
}

SourceRange
ParameterListScope::getChildlessSourceRange(const bool omitAssertions) const {
  const auto rangeForGoodInput = getSourceRangeOfEnclosedParams(omitAssertions);
  auto r = SourceRange(rangeForGoodInput.Start,
                       fixupEndForBadInput(rangeForGoodInput));
  assert(getSourceManager().rangeContains(
             getParent().get()->getChildlessSourceRange(true), r) &&
         "Parameters not within function?!");
  return r;
}

SourceLoc ParameterListScope::fixupEndForBadInput(
    const SourceRange rangeForGoodInput) const {
  const auto s = rangeForGoodInput.Start;
  const auto e = rangeForGoodInput.End;
  return getSourceManager().isBeforeInBuffer(s, e) ? e : s;
}

SourceRange
ForEachPatternScope::getChildlessSourceRange(const bool omitAssertions) const {
  // The scope of the pattern extends from the 'where' expression (if present)
  // until the end of the body.
  if (stmt->getWhere())
    return SourceRange(stmt->getWhere()->getStartLoc(),
                       stmt->getBody()->getEndLoc());

  // Otherwise, scope of the pattern covers the body.
  return stmt->getBody()->getSourceRange();
}

SourceRange
CatchStmtScope::getChildlessSourceRange(const bool omitAssertions) const {
  // The scope of the pattern extends from the 'where' (if present)
  // to the end of the body.
  if (stmt->getGuardExpr())
    return SourceRange(stmt->getWhereLoc(), stmt->getBody()->getEndLoc());

  // Otherwise, the scope of the pattern encompasses the body.
  return stmt->getBody()->getSourceRange();
}
SourceRange
CaseStmtScope::getChildlessSourceRange(const bool omitAssertions) const {
  // The scope of the case statement begins at the first guard expression,
  // if there is one, and extends to the end of the body.
  // FIXME: Figure out what to do about multiple pattern bindings. We might
  // want a more restrictive rule in those cases.
  for (const auto &caseItem : stmt->getCaseLabelItems()) {
    if (auto guardExpr = caseItem.getGuardExpr())
      return SourceRange(guardExpr->getStartLoc(),
                         stmt->getBody()->getEndLoc());
  }

  // Otherwise, it covers the body.
  return stmt->getBody()
      ->getSourceRange(); // The scope of the case statement begins
}

SourceRange
BraceStmtScope::getChildlessSourceRange(const bool omitAssertions) const {
  // The brace statements that represent closures start their scope at the
  // 'in' keyword, when present.
  if (auto closure = parentClosureIfAny()) {
    if (closure.get()->getInLoc().isValid())
      return SourceRange(closure.get()->getInLoc(), stmt->getEndLoc());
  }
  return stmt->getSourceRange();
}

SourceRange ConditionalClauseScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  // From the start of this particular condition to the start of the
  // then/body part.
  const auto startLoc = getStmtConditionElement().getStartLoc();
  return startLoc.isValid()
         ? SourceRange(startLoc, endLoc)
         : SourceRange(endLoc);
}

SourceRange ConditionalClausePatternUseScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  // For a guard continuation, the scope extends from the end of the 'else'
  // to the end of the continuation.
  return SourceRange(startLoc);
}

SourceRange
CaptureListScope::getChildlessSourceRange(const bool omitAssertions) const {
  auto *const closure = expr->getClosureBody();
  return SourceRange(expr->getStartLoc(), getStartOfFirstParam(closure));
}

SourceRange ClosureParametersScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  if (!omitAssertions)
    assert(closureExpr->getInLoc().isValid() &&
           "We don't create these if no in loc");
  return SourceRange(getStartOfFirstParam(closureExpr),
                     closureExpr->getInLoc());
}

SourceRange
ClosureBodyScope::getChildlessSourceRange(const bool omitAssertions) const {
  if (closureExpr->getInLoc().isValid())
    return SourceRange(closureExpr->getInLoc(), closureExpr->getEndLoc());

  return closureExpr->getSourceRange();
}

SourceRange AttachedPropertyWrapperScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  return sourceRangeWhenCreated;
}

SourceRange LookupParentDiversionScope::getChildlessSourceRange(
    const bool omitAssertions) const {
  return SourceRange(startLoc);
}

#pragma mark source range caching

SourceRange ASTScopeImpl::getSourceRange(const bool omitAssertions) const {
  if (!isSourceRangeCached(omitAssertions))
    cacheSourceRangeOfMeAndDescendants(omitAssertions);
  return *cachedSourceRange;
}

bool ASTScopeImpl::isSourceRangeCached(const bool omitAssertions) const {
  const bool isCached = cachedSourceRange.hasValue();
  assert(omitAssertions || isCached || ensureNoAncestorsSourceRangeIsCached());
  return isCached;
}

bool ASTScopeImpl::ensureNoAncestorsSourceRangeIsCached() const {
  if (const auto *const p = getParent().getPtrOrNull()) {
    auto r = !p->isSourceRangeCached(true) &&
             p->ensureNoAncestorsSourceRangeIsCached();
    if (!r)
      llvm_unreachable("found a violation");
    return true;
  }
  return true;
}

void ASTScopeImpl::cacheSourceRangeOfMeAndDescendants(
    const bool omitAssertions) const {
  // In order to satisfy the invariant that, if my range is uncached,
  // my parent's range is uncached, (which is needed to optimize invalidation
  // by obviating the need to uncache all the way to the root every time),
  // when caching a range, must ensure all children's ranges are cached.
  for (auto *c : getChildren())
    c->cacheSourceRangeOfMeAndDescendants(omitAssertions);

  cachedSourceRange = getUncachedSourceRange(omitAssertions);
}

SourceRange
ASTScopeImpl::getUncachedSourceRange(const bool omitAssertions) const {
  const auto childlessRange = getChildlessSourceRange(omitAssertions);
  const auto rangeIncludingIgnoredNodes =
      widenSourceRangeForIgnoredASTNodes(childlessRange);
  return widenSourceRangeForChildren(rangeIncludingIgnoredNodes,
                                     omitAssertions);
}

void ASTScopeImpl::clearCachedSourceRangesOfMeAndAncestors() {
  // An optimization: if my range isn't cached, my ancestors must not be
  if (!isSourceRangeCached())
    return;
  cachedSourceRange = None;
  if (auto p = getParent())
    p.get()->clearCachedSourceRangesOfMeAndAncestors();
}

#pragma mark ignored nodes and compensating for InterpolatedStringLiteralExprs and EditorPlaceHolders

namespace {
class EffectiveEndFinder : public ASTWalker {
  SourceLoc end;
  const SourceManager &SM;

public:
  EffectiveEndFinder(const SourceManager &SM) : SM(SM) {}

  std::pair<bool, Expr *> walkToExprPre(Expr *E) {
    if (!E)
      return {true, E};
    if (auto *isl = dyn_cast<InterpolatedStringLiteralExpr>(E)) {
      if (end.isInvalid() ||
          SM.isBeforeInBuffer(end, isl->getTrailingQuoteLoc()))
        end = isl->getTrailingQuoteLoc();
    } else if (auto *epl = dyn_cast<EditorPlaceholderExpr>(E)) {
      if (end.isInvalid() ||
          SM.isBeforeInBuffer(end, epl->getTrailingAngleBracketLoc()))
        end = epl->getTrailingAngleBracketLoc();
    }
    return ASTWalker::walkToExprPre(E);
  }
  SourceLoc getTrailingQuoteLoc() const { return end; }
};
} // namespace

// FIXME: Alter how EditorPlaceHolder and InterpolgatedStringLiteralExpr are
// parsed so getSourceRange is enough.
SourceRange ASTScopeImpl::getEffectiveSourceRange(const ASTNode n) const {
  if (const auto *d = n.dyn_cast<Decl *>())
    return d->getSourceRange();
  if (const auto *s = n.dyn_cast<Stmt *>())
    return s->getSourceRange();
  auto *e = n.dyn_cast<Expr *>();
  assert(e);
  EffectiveEndFinder finder(getSourceManager());
  e->walk(finder);
  return SourceRange(e->getLoc(), finder.getTrailingQuoteLoc().isValid()
                                      ? finder.getTrailingQuoteLoc()
                                      : e->getEndLoc());
}

void ASTScopeImpl::widenSourceRangeForIgnoredASTNode(const ASTNode n) {
  // The pattern scopes will include the source ranges for VarDecls.
  // Doing the default here would cause a pattern initializer scope's range
  // to overlap the pattern use scope's range.

  if (PatternEntryDeclScope::isHandledSpecially(n))
    return;

  SourceRange r = getEffectiveSourceRange(n);
  if (r.isInvalid())
    return;
  if (sourceRangeOfIgnoredASTNodes.isInvalid())
    sourceRangeOfIgnoredASTNodes = r;
  else
    sourceRangeOfIgnoredASTNodes.widen(r);
}

static SourceLoc getStartOfFirstParam(ClosureExpr *closure) {
  if (auto *parms = closure->getParameters()) {
    if (parms->size())
      return parms->get(0)->getStartLoc();
  }
  if (closure->getInLoc().isValid())
    return closure->getInLoc();
  if (closure->getBody())
    return closure->getBody()->getLBraceLoc();
  return closure->getStartLoc();
}

#pragma mark getSourceRangeOfEnclosedParams

SourceRange
ASTScopeImpl::getSourceRangeOfEnclosedParams(const bool omitAssertions) const {
  return getParent().get()->getSourceRangeOfEnclosedParams(omitAssertions);
}

SourceRange
EnumElementScope::getSourceRangeOfEnclosedParams(bool omitAssertions) const {
  auto *pl = decl->getParameterList();
  return pl ? pl->getSourceRange() : SourceRange();
}

SourceRange SubscriptDeclScope::getSourceRangeOfEnclosedParams(
    const bool omitAssertions) const {
  auto r = SourceRange(decl->getIndices()->getLParenLoc(), decl->getEndLoc());
  // Because of "subscript(x: MyStruct#^PARAM_1^#) -> Int { return 0 }"
  // Cannot just use decl->getEndLoc()
  r.widen(decl->getIndices()->getRParenLoc());
  return r;
}

SourceRange AbstractFunctionDeclScope::getSourceRangeOfEnclosedParams(
    const bool omitAssertions) const {
  const auto s = getParamsSourceLoc(decl);
  const auto e = getChildlessSourceRange(omitAssertions).End;
  return s.isInvalid() || e.isInvalid() ? SourceRange() : SourceRange(s, e);
}

SourceLoc
AbstractFunctionDeclScope::getParamsSourceLoc(AbstractFunctionDecl *decl) {
  if (auto *c = dyn_cast<ConstructorDecl>(decl))
    return c->getParameters()->getLParenLoc();

  if (auto *dd = dyn_cast<DestructorDecl>(decl))
    return dd->getNameLoc();

  auto *fd = cast<FuncDecl>(decl);
  // clang-format off
  return isa<AccessorDecl>(fd) ? fd->getLoc()
       : fd->isDeferBody()     ? fd->getNameLoc()
       :                         fd->getParameters()->getLParenLoc();
  // clang-format on
}
