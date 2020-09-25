//===--- ASTScopeLookup.cpp - Swift Object-Oriented AST Scope -------------===//
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
/// This file implements the lookup functionality of the ASTScopeImpl ontology.
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
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/Compiler.h"

using namespace swift;
using namespace namelookup;
using namespace ast_scope;

void ASTScopeImpl::unqualifiedLookup(
    SourceFile *sourceFile, const SourceLoc loc, DeclConsumer consumer) {
  const auto *start =
      findStartingScopeForLookup(sourceFile, loc);
  if (start)
    start->lookup(nullptr, nullptr, consumer);
}

const ASTScopeImpl *ASTScopeImpl::findStartingScopeForLookup(
    SourceFile *sourceFile, const SourceLoc loc) {
  auto *const fileScope = sourceFile->getScope().impl;
  const auto *innermost = fileScope->findInnermostEnclosingScope(loc, nullptr);
  ASTScopeAssert(innermost->getWasExpanded(),
                 "If looking in a scope, it must have been expanded.");

  return innermost;
}

ASTScopeImpl *
ASTScopeImpl::findInnermostEnclosingScope(SourceLoc loc,
                                          NullablePtr<raw_ostream> os) {
  return findInnermostEnclosingScopeImpl(loc, os, getSourceManager(),
                                         getScopeCreator());
}

ASTScopeImpl *ASTScopeImpl::findInnermostEnclosingScopeImpl(
    SourceLoc loc, NullablePtr<raw_ostream> os, SourceManager &sourceMgr,
    ScopeCreator &scopeCreator) {
  expandAndBeCurrentDetectingRecursion(scopeCreator);
  auto child = findChildContaining(loc, sourceMgr);
  if (!child)
    return this;
  return child.get()->findInnermostEnclosingScopeImpl(loc, os, sourceMgr,
                                                      scopeCreator);
}

bool ASTScopeImpl::checkSourceRangeOfThisASTNode() const {
  const auto r = getSourceRangeOfThisASTNode();
  (void)r;
  ASTScopeAssert(!getSourceManager().isBeforeInBuffer(r.End, r.Start),
                 "Range is backwards.");
  return true;
}

/// If the \p loc is in a new buffer but \p range is not, consider the location
/// is at the start of replaced range. Otherwise, returns \p loc as is.
static SourceLoc translateLocForReplacedRange(SourceManager &sourceMgr,
                                              SourceRange range,
                                              SourceLoc loc) {
  if (const auto &replacedRange = sourceMgr.getReplacedRange()) {
    if (sourceMgr.rangeContainsTokenLoc(replacedRange.New, loc) &&
        !sourceMgr.rangeContains(replacedRange.New, range)) {
      return replacedRange.Original.Start;
    }
  }
  return loc;
}

NullablePtr<ASTScopeImpl>
ASTScopeImpl::findChildContaining(SourceLoc loc,
                                  SourceManager &sourceMgr) const {
  // Use binary search to find the child that contains this location.
  auto *const *child = llvm::lower_bound(
      getChildren(), loc,
      [&sourceMgr](const ASTScopeImpl *scope, SourceLoc loc) {
        ASTScopeAssert(scope->checkSourceRangeOfThisASTNode(), "Bad range.");
        auto rangeOfScope = scope->getSourceRangeOfScope();
        loc = translateLocForReplacedRange(sourceMgr, rangeOfScope, loc);
        return -1 == ASTScopeImpl::compare(rangeOfScope, loc, sourceMgr,
                                           /*ensureDisjoint=*/false);
      });

  if (child != getChildren().end()) {
    auto rangeOfScope = (*child)->getSourceRangeOfScope();
    loc = translateLocForReplacedRange(sourceMgr, rangeOfScope, loc);
    if (sourceMgr.rangeContainsTokenLoc(rangeOfScope, loc))
      return *child;
  }

  return nullptr;
}

#pragma mark lookup methods that run once per scope

void ASTScopeImpl::lookup(const NullablePtr<const ASTScopeImpl> limit,
                          NullablePtr<const GenericParamList> lastListSearched,
                          DeclConsumer consumer) const {

#ifndef NDEBUG
  consumer.startingNextLookupStep();
#endif

  // Certain illegal nestings, e.g. protocol nestled inside a struct,
  // require that lookup stop at the outer scope.
  if (this == limit.getPtrOrNull()) {
#ifndef NDEBUG
    consumer.finishingLookup("limit return");
#endif
    return;
  }

  // Look for generics before members in violation of lexical ordering because
  // you can say "self.name" to get a name shadowed by a generic but you
  // can't do the opposite to get a generic shadowed by a name.
  const auto doneAndListSearched =
      lookInMyGenericParameters(lastListSearched, consumer);
  if (doneAndListSearched.first)
    return;

  if (lookupLocalsOrMembers(consumer))
    return;

  const auto *const lookupParent = getLookupParent().getPtrOrNull();
  if (!lookupParent) {
#ifndef NDEBUG
    consumer.finishingLookup("Finished lookup; no parent");
#endif
    return;
  }

  // If there is no limit and this scope induces one, pass that on.
  const NullablePtr<const ASTScopeImpl> limitForParent =
      limit ? limit : getLookupLimit();

  return lookupParent->lookup(limitForParent, lastListSearched,
                              consumer);
}

#pragma mark genericParams()

NullablePtr<const GenericParamList> ASTScopeImpl::genericParams() const {
  return nullptr;
}
NullablePtr<const GenericParamList>
AbstractFunctionDeclScope::genericParams() const {
  return decl->getGenericParams();
}
NullablePtr<const GenericParamList> SubscriptDeclScope::genericParams() const {
  return decl->getGenericParams();
}
NullablePtr<const GenericParamList> GenericTypeScope::genericParams() const {
  // For Decls:
  // WAIT, WHAT?! Isn't this covered by the GenericParamScope
  // lookupLocalsOrMembers? No, that's for use of generics in the body. This is
  // for generic restrictions.

  // For Bodies:
  // Sigh... These must be here so that from body, we search generics before
  // members. But they also must be on the Decl scope for lookups starting from
  // generic parameters, where clauses, etc.
  auto *context = getGenericContext();
  if (isa<TypeAliasDecl>(context))
    return context->getParsedGenericParams();
  return context->getGenericParams();
}
NullablePtr<const GenericParamList> ExtensionScope::genericParams() const {
  return decl->getGenericParams();
}

#pragma mark lookInMyGenericParameters

std::pair<bool, NullablePtr<const GenericParamList>>
ASTScopeImpl::lookInMyGenericParameters(
    NullablePtr<const GenericParamList> formerListSearched,
    ASTScopeImpl::DeclConsumer consumer) const {
  auto listToSearch = genericParams();
  if (listToSearch == formerListSearched)
    return std::make_pair(false, formerListSearched);

  // For extensions of nested types, must check outer parameters
  for (auto *params = listToSearch.getPtrOrNull(); params;
       params = params->getOuterParameters()) {
    if (lookInGenericParametersOf(params, consumer))
      return std::make_pair(true, listToSearch);
  }
  return std::make_pair(false, listToSearch);
}

bool ASTScopeImpl::lookInGenericParametersOf(
    const NullablePtr<const GenericParamList> paramList,
    ASTScopeImpl::DeclConsumer consumer) {
  if (!paramList)
    return false;
  SmallVector<ValueDecl *, 32> bindings;
  for (auto *param : paramList.get()->getParams())
    bindings.push_back(param);
  if (consumer.consume(bindings, DeclVisibilityKind::GenericParameter))
    return true;
  return false;
}

#pragma mark looking in locals or members - members

bool ASTScopeImpl::lookupLocalsOrMembers(DeclConsumer) const {
  return false; // many kinds of scopes have none
}

bool GenericTypeOrExtensionScope::lookupLocalsOrMembers(
    ASTScopeImpl::DeclConsumer consumer) const {
  return portion->lookupMembersOf(this, consumer);
}

bool Portion::lookupMembersOf(const GenericTypeOrExtensionScope *,
                              ASTScopeImpl::DeclConsumer) const {
  return false;
}

bool GenericTypeOrExtensionWhereOrBodyPortion::lookupMembersOf(
    const GenericTypeOrExtensionScope *scope,
    ASTScopeImpl::DeclConsumer consumer) const {
  auto nt = scope->getCorrespondingNominalTypeDecl().getPtrOrNull();
  if (!nt)
    return false;
  return consumer.lookInMembers(scope->getGenericContext(), nt);
}

bool GenericTypeOrExtensionWherePortion::lookupMembersOf(
    const GenericTypeOrExtensionScope *scope,
    ASTScopeImpl::DeclConsumer consumer) const {
  if (!scope->areMembersVisibleFromWhereClause())
    return false;

  return GenericTypeOrExtensionWhereOrBodyPortion::lookupMembersOf(
    scope, consumer);
}

bool GenericTypeOrExtensionScope::areMembersVisibleFromWhereClause() const {
  auto *decl = getDecl();
  return isa<ProtocolDecl>(decl) || isa<ExtensionDecl>(decl);
}

#pragma mark custom lookup parent behavior

NullablePtr<const ASTScopeImpl>
PatternEntryInitializerScope::getLookupParent() const {
  auto parent = getParent().get();
  assert(parent->getClassName() == "PatternEntryDeclScope");

  // Lookups from inside a pattern binding initializer skip the parent
  // scope that introduces bindings bound by the pattern, since we
  // want this to work:
  //
  // func f(x: Int) {
  //   let x = x
  //   print(x)
  // }
  return parent->getLookupParent();
}

#pragma mark looking in locals or members - locals

bool GenericParamScope::lookupLocalsOrMembers(DeclConsumer consumer) const {
  auto *param = paramList->getParams()[index];
  return consumer.consume({param}, DeclVisibilityKind::GenericParameter);
}

bool PatternEntryDeclScope::lookupLocalsOrMembers(DeclConsumer consumer) const {
  if (vis != DeclVisibilityKind::LocalVariable)
    return false; // look in self type will find this later
  return lookupLocalBindingsInPattern(getPattern(), vis, consumer);
}

bool ForEachPatternScope::lookupLocalsOrMembers(DeclConsumer consumer) const {
  return lookupLocalBindingsInPattern(
      stmt->getPattern(), DeclVisibilityKind::LocalVariable, consumer);
}

bool CaseLabelItemScope::lookupLocalsOrMembers(DeclConsumer consumer) const {
  return lookupLocalBindingsInPattern(
      item.getPattern(), DeclVisibilityKind::LocalVariable, consumer);
}

bool CaseStmtBodyScope::lookupLocalsOrMembers(DeclConsumer consumer) const {
  for (auto *var : stmt->getCaseBodyVariablesOrEmptyArray())
    if (consumer.consume({var}, DeclVisibilityKind::LocalVariable))
        return true;

  return false;
}

bool FunctionBodyScope::lookupLocalsOrMembers(
    DeclConsumer consumer) const {
  if (auto *paramList = decl->getParameters()) {
    for (auto *paramDecl : *paramList)
      if (consumer.consume({paramDecl}, DeclVisibilityKind::FunctionParameter))
        return true;
  }

  if (decl->getDeclContext()->isTypeContext()) {
    return consumer.consume({decl->getImplicitSelfDecl()},
                            DeclVisibilityKind::FunctionParameter);
  }

  // Consider \c var t: T { (did/will/)get/set { ... t }}
  // Lookup needs to find t, but if the var is inside of a type the baseDC needs
  // to be set. It all works fine, except: if the var is not inside of a type,
  // then t needs to be found as a local binding:
  if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
    if (auto *storage = accessor->getStorage())
      if (consumer.consume({storage}, DeclVisibilityKind::LocalVariable))
        return true;
  }

  return false;
}

bool SpecializeAttributeScope::lookupLocalsOrMembers(
    DeclConsumer consumer) const {
  if (auto *params = whatWasSpecialized->getGenericParams())
    for (auto *param : params->getParams())
      if (consumer.consume({param}, DeclVisibilityKind::GenericParameter))
        return true;
  return false;
}

bool DifferentiableAttributeScope::lookupLocalsOrMembers(
    DeclConsumer consumer) const {
  auto visitAbstractFunctionDecl = [&](AbstractFunctionDecl *afd) {
    if (auto *params = afd->getGenericParams())
      for (auto *param : params->getParams())
        if (consumer.consume({param}, DeclVisibilityKind::GenericParameter))
          return true;
    return false;
  };
  if (auto *afd = dyn_cast<AbstractFunctionDecl>(attributedDeclaration)) {
    return visitAbstractFunctionDecl(afd);
  } else if (auto *asd = dyn_cast<AbstractStorageDecl>(attributedDeclaration)) {
    if (auto *accessor = asd->getParsedAccessor(AccessorKind::Get))
      if (visitAbstractFunctionDecl(accessor))
        return true;
  }
  return false;
}

bool BraceStmtScope::lookupLocalsOrMembers(DeclConsumer consumer) const {
  // All types and functions are visible anywhere within a brace statement
  // scope. When ordering matters (i.e. var decl) we will have split the brace
  // statement into nested scopes.
  //
  // Don't stop at the first one, there may be local funcs with same base name
  // and want them all.
  SmallVector<ValueDecl *, 32> localBindings;
  for (auto braceElement : stmt->getElements()) {
    if (auto localBinding = braceElement.dyn_cast<Decl *>()) {
      if (auto *vd = dyn_cast<ValueDecl>(localBinding))
        localBindings.push_back(vd);
    }
  }
  return consumer.consume(localBindings, DeclVisibilityKind::LocalVariable);
}

bool PatternEntryInitializerScope::lookupLocalsOrMembers(
    DeclConsumer consumer) const {
  // 'self' is available within the pattern initializer of a 'lazy' variable.
  auto *initContext = dyn_cast_or_null<PatternBindingInitializer>(
      decl->getInitContext(0));
  if (initContext) {
    if (auto *selfParam = initContext->getImplicitSelfDecl()) {
      return consumer.consume({selfParam},
                              DeclVisibilityKind::FunctionParameter);
    }
  }
  return false;
}

bool CaptureListScope::lookupLocalsOrMembers(DeclConsumer consumer) const {
  for (auto &e : expr->getCaptureList()) {
    if (consumer.consume(
            {e.Var},
            DeclVisibilityKind::LocalVariable)) // or FunctionParameter??
      return true;
  }
  return false;
}

bool ClosureParametersScope::lookupLocalsOrMembers(
    DeclConsumer consumer) const {
  for (auto param : *closureExpr->getParameters())
    if (consumer.consume({param}, DeclVisibilityKind::FunctionParameter))
      return true;
  return false;
}

bool ConditionalClausePatternUseScope::lookupLocalsOrMembers(
    DeclConsumer consumer) const {
  return lookupLocalBindingsInPattern(
      pattern, DeclVisibilityKind::LocalVariable, consumer);
}

bool ASTScopeImpl::lookupLocalBindingsInPattern(const Pattern *p,
                                                DeclVisibilityKind vis,
                                                DeclConsumer consumer) {
  if (!p)
    return false;
  bool isDone = false;
  p->forEachVariable([&](VarDecl *var) {
    if (!isDone)
      isDone = consumer.consume({var}, vis);
  });
  return isDone;
}

#pragma mark getLookupLimit

NullablePtr<const ASTScopeImpl> ASTScopeImpl::getLookupLimit() const {
  return nullptr;
}

NullablePtr<const ASTScopeImpl>
GenericTypeOrExtensionScope::getLookupLimit() const {
  return portion->getLookupLimitFor(this);
}

NullablePtr<const ASTScopeImpl>
Portion::getLookupLimitFor(const GenericTypeOrExtensionScope *) const {
  return nullptr;
}
NullablePtr<const ASTScopeImpl>
GenericTypeOrExtensionWholePortion::getLookupLimitFor(
    const GenericTypeOrExtensionScope *scope) const {
  return scope->getLookupLimitForDecl();
}

NullablePtr<const ASTScopeImpl>
GenericTypeOrExtensionScope::getLookupLimitForDecl() const {
  return nullptr;
}

NullablePtr<const ASTScopeImpl>
NominalTypeScope::getLookupLimitForDecl() const {
  if (isa<ProtocolDecl>(decl)) {
    // ProtocolDecl can only be legally nested in a SourceFile,
    // so any other kind of Decl is illegal
    return parentIfNotChildOfTopScope();
  }
  // AFAICT, a struct, decl, or enum can be nested inside anything
  // but a ProtocolDecl.
  return ancestorWithDeclSatisfying(
      [&](const Decl *const d) { return isa<ProtocolDecl>(d); });
}

NullablePtr<const ASTScopeImpl> ExtensionScope::getLookupLimitForDecl() const {
  // Extensions can only be legally nested in a SourceFile,
  // so any other kind of Decl is illegal
  return parentIfNotChildOfTopScope();
}

NullablePtr<const ASTScopeImpl> ASTScopeImpl::ancestorWithDeclSatisfying(
    function_ref<bool(const Decl *)> predicate) const {
  for (NullablePtr<const ASTScopeImpl> s = getParent(); s;
       s = s.get()->getParent()) {
    if (Decl *d = s.get()->getDeclIfAny().getPtrOrNull()) {
      if (predicate(d))
        return s;
    }
  }
  return nullptr;
}

#pragma mark isLabeledStmtLookupTerminator implementations
bool ASTScopeImpl::isLabeledStmtLookupTerminator() const {
  return true;
}

bool LookupParentDiversionScope::isLabeledStmtLookupTerminator() const {
  return false;
}

bool ConditionalClauseScope::isLabeledStmtLookupTerminator() const {
  return false;
}

bool ConditionalClausePatternUseScope::isLabeledStmtLookupTerminator() const {
  return false;
}

bool AbstractStmtScope::isLabeledStmtLookupTerminator() const {
  return false;
}

bool ForEachPatternScope::isLabeledStmtLookupTerminator() const {
  return false;
}

bool CaseStmtBodyScope::isLabeledStmtLookupTerminator() const {
  return false;
}

bool PatternEntryDeclScope::isLabeledStmtLookupTerminator() const {
  return false;
}

llvm::SmallVector<LabeledStmt *, 4>
ASTScopeImpl::lookupLabeledStmts(SourceFile *sourceFile, SourceLoc loc) {
  // Find the innermost scope from which to start our search.
  auto *const fileScope = sourceFile->getScope().impl;
  const auto *innermost = fileScope->findInnermostEnclosingScope(loc, nullptr);
  ASTScopeAssert(innermost->getWasExpanded(),
                 "If looking in a scope, it must have been expanded.");

  llvm::SmallVector<LabeledStmt *, 4> labeledStmts;
  for (auto scope = innermost; scope && !scope->isLabeledStmtLookupTerminator();
       scope = scope->getParent().getPtrOrNull()) {
    // If we have a labeled statement, record it.
    auto stmt = scope->getStmtIfAny();
    if (!stmt) continue;

    auto labeledStmt = dyn_cast<LabeledStmt>(stmt.get());
    if (!labeledStmt) continue;

    // Skip guard statements; they aren't actually targets for break or
    // continue.
    if (isa<GuardStmt>(labeledStmt)) continue;

    labeledStmts.push_back(labeledStmt);
  }

  return labeledStmts;
}

std::pair<CaseStmt *, CaseStmt *> ASTScopeImpl::lookupFallthroughSourceAndDest(
    SourceFile *sourceFile, SourceLoc loc) {
  // Find the innermost scope from which to start our search.
  auto *const fileScope = sourceFile->getScope().impl;
  const auto *innermost = fileScope->findInnermostEnclosingScope(loc, nullptr);
  ASTScopeAssert(innermost->getWasExpanded(),
                 "If looking in a scope, it must have been expanded.");

  // Look for the enclosing case statement of a 'switch'.
  for (auto scope = innermost; scope && !scope->isLabeledStmtLookupTerminator();
       scope = scope->getParent().getPtrOrNull()) {
    // If we have a case statement, record it.
    auto stmt = scope->getStmtIfAny();
    if (!stmt) continue;

    // If we've found the first case statement of a switch, record it as the
    // fallthrough source. do-catch statements don't support fallthrough.
    if (auto caseStmt = dyn_cast<CaseStmt>(stmt.get())) {
      if (caseStmt->getParentKind() == CaseParentKind::Switch)
        return { caseStmt, caseStmt->findNextCaseStmt() };

      continue;
    }
  }

  return { nullptr, nullptr };
}
