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
#include <algorithm>

using namespace swift;
using namespace namelookup;
using namespace ast_scope;

static bool isLocWithinAnInactiveClause(const SourceLoc loc, SourceFile *SF);

llvm::SmallVector<const ASTScopeImpl *, 0> ASTScopeImpl::unqualifiedLookup(
    SourceFile *sourceFile, const DeclNameRef name, const SourceLoc loc,
    const DeclContext *const startingContext, DeclConsumer consumer) {
  SmallVector<const ASTScopeImpl *, 0> history;
  const auto *start =
      findStartingScopeForLookup(sourceFile, name, loc, startingContext);
  if (start)
    start->lookup(history, nullptr, nullptr, consumer);
  return history;
}

const ASTScopeImpl *ASTScopeImpl::findStartingScopeForLookup(
    SourceFile *sourceFile, const DeclNameRef name, const SourceLoc loc,
    const DeclContext *const startingContext) {
  // At present, use legacy code in unqualifiedLookup.cpp to handle module-level
  // lookups
  // TODO: implement module scope someday
  if (startingContext->getContextKind() == DeclContextKind::Module)
    return nullptr;

  auto *const fileScope = sourceFile->getScope().impl;
  // Parser may have added decls to source file, since previous lookup
  if (name.isOperator())
    return fileScope; // operators always at file scope

  const auto *innermost = fileScope->findInnermostEnclosingScope(loc, nullptr);
  ASTScopeAssert(innermost->getWasExpanded(),
                 "If looking in a scope, it must have been expanded.");

  // The legacy lookup code gets passed both a SourceLoc and a starting context.
  // However, our ultimate intent is for clients to not have to pass in a
  // DeclContext at all, since the SourceLoc should be enough. While we are
  // debugging the new ASTScope lookup code, we can catch bugs by comparing the
  // DeclContext of the ASTScope found from the desired SourceLoc to the
  // DeclContext passed in by the client.

  const auto *startingScope = innermost;
  for (; startingScope &&
         !startingScope->doesContextMatchStartingContext(startingContext);
       startingScope = startingScope->getParent().getPtrOrNull()) {
  }
  // Someday, just use the assertion below. For now, print out lots of info for
  // debugging.
  if (!startingScope) {
    llvm::errs() << "ASTScopeImpl: resorting to startingScope hack, file: "
                 << sourceFile->getFilename() << "\n";
    // The check is costly, and inactive lookups will end up here, so don't
    // do the check unless we can't find the startingScope.
    const bool isInInactiveClause =
        isLocWithinAnInactiveClause(loc, sourceFile);
    if (isInInactiveClause)
      llvm::errs() << "  because location is within an inactive clause\n";
    llvm::errs() << "'";
    name.print(llvm::errs());
    llvm::errs() << "' ";
    llvm::errs() << "loc: ";
    loc.print(llvm::errs(), sourceFile->getASTContext().SourceMgr);
    llvm::errs() << "\nstarting context:\n ";
    startingContext->printContext(llvm::errs());
    //    llvm::errs() << "\ninnermost: ";
    //    innermost->dump();
    //    llvm::errs() << "in: \n";
    //    fileScope->dump();
    llvm::errs() << "\n\n";

    // Might distort things
    //    if (fileScope->crossCheckWithAST())
    //      llvm::errs() << "Tree creation missed some DeclContexts.\n";

    // Crash compilation even if NDEBUG
    if (isInInactiveClause)
      llvm::report_fatal_error(
          "A lookup was attempted into an inactive clause");
  }

  ASTScopeAssert(startingScope, "ASTScopeImpl: could not find startingScope");
  return startingScope;
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

NullablePtr<ASTScopeImpl>
ASTScopeImpl::findChildContaining(SourceLoc loc,
                                  SourceManager &sourceMgr) const {
  // Use binary search to find the child that contains this location.
  struct CompareLocs {
    SourceManager &sourceMgr;

    bool operator()(const ASTScopeImpl *scope, SourceLoc loc) {
      ASTScopeAssert(scope->checkSourceRangeOfThisASTNode(), "Bad range.");
      return -1 == ASTScopeImpl::compare(scope->getSourceRangeOfScope(), loc,
                                         sourceMgr,
                                         /*ensureDisjoint=*/false);
    }
    bool operator()(SourceLoc loc, const ASTScopeImpl *scope) {
      ASTScopeAssert(scope->checkSourceRangeOfThisASTNode(), "Bad range.");
      // Alternatively, we could check that loc < start-of-scope
      return 0 >= ASTScopeImpl::compare(loc, scope->getSourceRangeOfScope(),
                                        sourceMgr,
                                        /*ensureDisjoint=*/false);
    }
  };
  auto *const *child = std::lower_bound(
      getChildren().begin(), getChildren().end(), loc, CompareLocs{sourceMgr});

  if (child != getChildren().end() &&
      sourceMgr.rangeContainsTokenLoc((*child)->getSourceRangeOfScope(), loc))
    return *child;

  return nullptr;
}

#pragma mark doesContextMatchStartingContext
// Match existing UnqualifiedLookupBehavior

bool ASTScopeImpl::doesContextMatchStartingContext(
    const DeclContext *context) const {
  // Why are we not checking the loc for this--because already did binary search
  // on loc to find the start First, try MY DeclContext
  if (auto myDCForL = getDeclContext())
    return myDCForL == context;
  // If I don't have one, ask my parent.
  // (Choose innermost scope with matching loc & context.)
  if (auto p = getParent())
    return p.get()->doesContextMatchStartingContext(context);
  // Topmost scope always has a context, the SourceFile.
  ASTScope_unreachable("topmost scope always has a context, the SourceFile");
}

// For a SubscriptDecl with generic parameters, the call tries to do lookups
// with startingContext equal to either the get or set subscript
// AbstractFunctionDecls. Since the generic parameters are in the
// SubscriptDeclScope, and not the AbstractFunctionDecl scopes (after all how
// could one parameter be in two scopes?), GenericParamScope intercepts the
// match query here and tests against the accessor DeclContexts.
bool GenericParamScope::doesContextMatchStartingContext(
    const DeclContext *context) const {
  if (auto *asd = dyn_cast<AbstractStorageDecl>(holder)) {
    for (auto accessor : asd->getAllAccessors()) {
      if (up_cast<DeclContext>(accessor) == context)
        return true;
    }
  }
  return false;
}

bool DifferentiableAttributeScope::doesContextMatchStartingContext(
    const DeclContext *context) const {
  // Need special logic to handle case where `attributedDeclaration` is an
  // `AbstractStorageDecl` (`SubscriptDecl` or `VarDecl`). The initial starting
  // context in `ASTScopeImpl::findStartingScopeForLookup` will be an accessor
  // of the `attributedDeclaration`.
  if (auto *asd = dyn_cast<AbstractStorageDecl>(attributedDeclaration))
    for (auto accessor : asd->getAllAccessors())
      if (up_cast<DeclContext>(accessor) == context)
        return true;
  return false;
}

#pragma mark lookup methods that run once per scope

void ASTScopeImpl::lookup(SmallVectorImpl<const ASTScopeImpl *> &history,
                          const NullablePtr<const ASTScopeImpl> limit,
                          NullablePtr<const GenericParamList> lastListSearched,
                          DeclConsumer consumer) const {

  history.push_back(this);

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

  if (lookupLocalsOrMembers(history, consumer))
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

  return lookupParent->lookup(history, limitForParent, lastListSearched,
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
  return getGenericContext()->getGenericParams();
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

bool ASTScopeImpl::lookupLocalsOrMembers(ArrayRef<const ASTScopeImpl *>,
                                         DeclConsumer) const {
  return false; // many kinds of scopes have none
}

bool GenericTypeOrExtensionScope::lookupLocalsOrMembers(
    ArrayRef<const ASTScopeImpl *> history,
    ASTScopeImpl::DeclConsumer consumer) const {
  // isCascadingUseArg must have already been resolved, for a real lookup
  // but may be \c None for dumping.
  return portion->lookupMembersOf(this, history, consumer);
}

bool Portion::lookupMembersOf(const GenericTypeOrExtensionScope *,
                              ArrayRef<const ASTScopeImpl *>,
                              ASTScopeImpl::DeclConsumer) const {
  return false;
}

bool GenericTypeOrExtensionWhereOrBodyPortion::lookupMembersOf(
    const GenericTypeOrExtensionScope *scope,
    ArrayRef<const ASTScopeImpl *> history,
    ASTScopeImpl::DeclConsumer consumer) const {
  auto nt = scope->getCorrespondingNominalTypeDecl().getPtrOrNull();
  if (!nt)
    return false;
  auto selfDC = computeSelfDC(history);
  return consumer.lookInMembers(selfDC, scope->getDeclContext().get(), nt,
                                [&](Optional<bool> initialIsCascadingUse) {
                                  return ASTScopeImpl::computeIsCascadingUse(
                                             history, initialIsCascadingUse)
                                      .getValueOr(true);
                                });
}

#pragma mark looking in locals or members - locals

bool GenericParamScope::lookupLocalsOrMembers(ArrayRef<const ASTScopeImpl *>,
                                              DeclConsumer consumer) const {
  auto *param = paramList->getParams()[index];
  return consumer.consume({param}, DeclVisibilityKind::GenericParameter);
}

bool PatternEntryDeclScope::lookupLocalsOrMembers(
    ArrayRef<const ASTScopeImpl *>, DeclConsumer consumer) const {
  if (vis != DeclVisibilityKind::LocalVariable)
    return false; // look in self type will find this later
  return lookupLocalBindingsInPattern(getPattern(), vis, consumer);
}

bool ForEachPatternScope::lookupLocalsOrMembers(ArrayRef<const ASTScopeImpl *>,
                                                DeclConsumer consumer) const {
  return lookupLocalBindingsInPattern(
      stmt->getPattern(), DeclVisibilityKind::LocalVariable, consumer);
}

bool CaseStmtScope::lookupLocalsOrMembers(ArrayRef<const ASTScopeImpl *>,
                                          DeclConsumer consumer) const {
  for (auto &item : stmt->getMutableCaseLabelItems())
    if (lookupLocalBindingsInPattern(
            item.getPattern(), DeclVisibilityKind::LocalVariable, consumer))
      return true;
  return false;
}

bool AbstractFunctionBodyScope::lookupLocalsOrMembers(
    ArrayRef<const ASTScopeImpl *>, DeclConsumer consumer) const {
  if (auto *paramList = decl->getParameters()) {
    for (auto *paramDecl : *paramList)
      if (consumer.consume({paramDecl}, DeclVisibilityKind::FunctionParameter))
        return true;
  }
  return false;
}

bool MethodBodyScope::lookupLocalsOrMembers(
    ArrayRef<const ASTScopeImpl *> history, DeclConsumer consumer) const {
  ASTScopeAssert(isAMethod(decl), "Asking for members of a non-method.");
  if (AbstractFunctionBodyScope::lookupLocalsOrMembers(history, consumer))
    return true;
  return consumer.consume({decl->getImplicitSelfDecl()},
                          DeclVisibilityKind::FunctionParameter);
}

bool PureFunctionBodyScope::lookupLocalsOrMembers(
    ArrayRef<const ASTScopeImpl *> history, DeclConsumer consumer) const {
  ASTScopeAssert(
      !isAMethod(decl),
      "Should have called lookupLocalsOrMembers instead of this function.");
  if (AbstractFunctionBodyScope::lookupLocalsOrMembers(history, consumer))
    return true;

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
    ArrayRef<const ASTScopeImpl *>, DeclConsumer consumer) const {
  if (auto *params = whatWasSpecialized->getGenericParams())
    for (auto *param : params->getParams())
      if (consumer.consume({param}, DeclVisibilityKind::GenericParameter))
        return true;
  return false;
}

bool DifferentiableAttributeScope::lookupLocalsOrMembers(
    ArrayRef<const ASTScopeImpl *>, DeclConsumer consumer) const {
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
    for (auto *accessor : asd->getAllAccessors())
      if (visitAbstractFunctionDecl(accessor))
        return true;
  }
  return false;
}

bool BraceStmtScope::lookupLocalsOrMembers(ArrayRef<const ASTScopeImpl *>,
                                           DeclConsumer consumer) const {
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
    ArrayRef<const ASTScopeImpl *>, DeclConsumer consumer) const {
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

bool ClosureParametersScope::lookupLocalsOrMembers(
    ArrayRef<const ASTScopeImpl *>, DeclConsumer consumer) const {
  if (auto *cl = captureList.getPtrOrNull()) {
    CaptureListExpr *mutableCL =
        const_cast<CaptureListExpr *>(captureList.get());
    for (auto &e : mutableCL->getCaptureList()) {
      if (consumer.consume(
              {e.Var},
              DeclVisibilityKind::LocalVariable)) // or FunctionParameter??
        return true;
    }
  }
  for (auto param : *closureExpr->getParameters())
    if (consumer.consume({param}, DeclVisibilityKind::FunctionParameter))
      return true;
  return false;
}

bool ConditionalClausePatternUseScope::lookupLocalsOrMembers(
    ArrayRef<const ASTScopeImpl *>, DeclConsumer consumer) const {
  return lookupLocalBindingsInPattern(
      pattern, DeclVisibilityKind::LocalVariable, consumer);
}

bool ASTScopeImpl::lookupLocalBindingsInPattern(Pattern *p,
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

#pragma mark computeSelfDC

NullablePtr<DeclContext>
GenericTypeOrExtensionWhereOrBodyPortion::computeSelfDC(
    ArrayRef<const ASTScopeImpl *> history) {
  ASTScopeAssert(history.size() != 0, "includes current scope");
  size_t i = history.size() - 1; // skip last entry (this scope)
  while (i != 0) {
    Optional<NullablePtr<DeclContext>> maybeSelfDC =
        history[--i]->computeSelfDCForParent();
    if (maybeSelfDC) {
      // If we've found a selfDC, we'll definitely be returning something.
      // However, we may have captured 'self' somewhere down the tree, so we
      // can't return outright without checking the nested scopes.
      NullablePtr<DeclContext> nestedCapturedSelfDC =
          checkNestedScopesForSelfCapture(history, i);
      return nestedCapturedSelfDC ? nestedCapturedSelfDC : *maybeSelfDC;
    }
  }
  return nullptr;
}

#pragma mark checkNestedScopesForSelfCapture

NullablePtr<DeclContext>
GenericTypeOrExtensionWhereOrBodyPortion::checkNestedScopesForSelfCapture(
    ArrayRef<const ASTScopeImpl *> history, size_t start) {
  NullablePtr<DeclContext> innerCapturedSelfDC;
  // Start with the next scope down the tree.
  size_t j = start;

  // Note: even though having this loop nested inside the while loop from
  // GenericTypeOrExtensionWhereOrBodyPortion::computeSelfDC may appear to
  // result in quadratic blowup, complexity actually remains linear with respect
  // to the size of history. This relies on the fact that
  // GenericTypeOrExtensionScope::computeSelfDCForParent returns a null pointer,
  // which will cause this method to bail out of the search early. Thus, this
  // method is called once per type body in the lookup history, and will not
  // end up re-checking the bodies of nested types that have already been
  // covered by earlier calls, so the total impact of this method across all
  // calls in a single lookup is O(n).
  while (j != 0) {
      auto *entry = history[--j];
    Optional<NullablePtr<DeclContext>> selfDCForParent =
      entry->computeSelfDCForParent();

    // If we encounter a scope that should cause us to forget the self
    // context (such as a nested type), bail out and use whatever the
    // the last inner captured context was.
    if (selfDCForParent && (*selfDCForParent).isNull())
      break;

    // Otherwise, if we have a captured self context for this scope, then
    // remember it since it is now the innermost scope we have encountered.
    NullablePtr<DeclContext> capturedSelfDC = entry->capturedSelfDC();
    if (!capturedSelfDC.isNull())
      innerCapturedSelfDC = entry->capturedSelfDC();

    // Continue searching in the next scope down.
  }
  return innerCapturedSelfDC;
}

#pragma mark compute isCascadingUse

Optional<bool> ASTScopeImpl::computeIsCascadingUse(
    ArrayRef<const ASTScopeImpl *> history,
    const Optional<bool> initialIsCascadingUse) {
  Optional<bool> isCascadingUse = initialIsCascadingUse;
  for (const auto *scope : history)
    isCascadingUse = scope->resolveIsCascadingUseForThisScope(isCascadingUse);
  return isCascadingUse;
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

#pragma mark computeSelfDCForParent

// If the lookup depends on implicit self, selfDC is its context.
// (Names in extensions never depend on self.)
// Lookup can propagate it up from, say a method to the enclosing type body.

// By default, propagate the selfDC up to a NomExt decl, body,
// or where clause
Optional<NullablePtr<DeclContext>>
ASTScopeImpl::computeSelfDCForParent() const {
  return None;
}

// Forget the "self" declaration:
Optional<NullablePtr<DeclContext>>
GenericTypeOrExtensionScope::computeSelfDCForParent() const {
  return NullablePtr<DeclContext>();
}

Optional<NullablePtr<DeclContext>>
PatternEntryInitializerScope::computeSelfDCForParent() const {
  // Pattern binding initializers are only interesting insofar as they
  // affect lookup in an enclosing nominal type or extension thereof.
  if (auto *ic = getPatternEntry().getInitContext()) {
    if (auto *bindingInit = dyn_cast<PatternBindingInitializer>(ic)) {
      // Lazy variable initializer contexts have a 'self' parameter for
      // instance member lookup.
      if (bindingInit->getImplicitSelfDecl()) {
        return NullablePtr<DeclContext>(bindingInit);
      }
    }
  }
  return None;
}

Optional<NullablePtr<DeclContext>>
MethodBodyScope::computeSelfDCForParent() const {
  return NullablePtr<DeclContext>(decl);
}

#pragma mark capturedSelfDC

// Closures may explicitly capture the self param, in which case the lookup
// should use the closure as the context for implicit self lookups.

// By default, there is no such context to return.
NullablePtr<DeclContext> ASTScopeImpl::capturedSelfDC() const {
  return NullablePtr<DeclContext>();
}

// Closures track this information explicitly.
NullablePtr<DeclContext> ClosureParametersScope::capturedSelfDC() const {
  if (closureExpr->capturesSelfEnablingImplictSelf())
    return NullablePtr<DeclContext>(closureExpr);
  return NullablePtr<DeclContext>();
}

#pragma mark ifUnknownIsCascadingUseAccordingTo

static bool isCascadingUseAccordingTo(const DeclContext *const dc) {
  return dc->isCascadingContextForLookup(false);
}

static bool ifUnknownIsCascadingUseAccordingTo(Optional<bool> isCascadingUse,
                                               const DeclContext *const dc) {
  return isCascadingUse.getValueOr(isCascadingUseAccordingTo(dc));
}

#pragma mark resolveIsCascadingUseForThisScope

Optional<bool> ASTScopeImpl::resolveIsCascadingUseForThisScope(
    Optional<bool> isCascadingUse) const {
  return isCascadingUse;
}

Optional<bool> GenericParamScope::resolveIsCascadingUseForThisScope(
    Optional<bool> isCascadingUse) const {
  if (auto *dc = getDeclContext().getPtrOrNull())
    return ifUnknownIsCascadingUseAccordingTo(isCascadingUse, dc);
  ASTScope_unreachable("generic what?");
}

Optional<bool> AbstractFunctionDeclScope::resolveIsCascadingUseForThisScope(
    Optional<bool> isCascadingUse) const {
  return decl->isCascadingContextForLookup(false) &&
         isCascadingUse.getValueOr(true);
}

Optional<bool> AbstractFunctionBodyScope::resolveIsCascadingUseForThisScope(
    Optional<bool> isCascadingUse) const {
  return false;
}

Optional<bool> GenericTypeOrExtensionScope::resolveIsCascadingUseForThisScope(
    Optional<bool> isCascadingUse) const {
  // Could override for ExtensionScope and just return true
  return ifUnknownIsCascadingUseAccordingTo(isCascadingUse,
                                            getDeclContext().get());
}

Optional<bool>
DefaultArgumentInitializerScope::resolveIsCascadingUseForThisScope(
    Optional<bool>) const {
  return false;
}

Optional<bool> ClosureParametersScope::resolveIsCascadingUseForThisScope(
    Optional<bool> isCascadingUse) const {
  return ifUnknownIsCascadingUseAccordingTo(isCascadingUse, closureExpr);
}
Optional<bool> ClosureBodyScope::resolveIsCascadingUseForThisScope(
    Optional<bool> isCascadingUse) const {
  return ifUnknownIsCascadingUseAccordingTo(isCascadingUse, closureExpr);
}

Optional<bool> PatternEntryInitializerScope::resolveIsCascadingUseForThisScope(
    Optional<bool> isCascadingUse) const {
  auto *const initContext = getPatternEntry().getInitContext();
  auto *PBI = dyn_cast_or_null<PatternBindingInitializer>(initContext);
  auto *isd = PBI ? PBI->getImplicitSelfDecl() : nullptr;

  // 'self' is available within the pattern initializer of a 'lazy' variable.
  if (isd)
    return ifUnknownIsCascadingUseAccordingTo(isCascadingUse, PBI);

  // initializing stored property of a type
  auto *const patternDeclContext = decl->getDeclContext();
  if (patternDeclContext->isTypeContext())
    return isCascadingUseAccordingTo(PBI->getParent());

  // initializing global or local
  if (PBI)
    return ifUnknownIsCascadingUseAccordingTo(isCascadingUse, PBI);

  return isCascadingUse;
}

bool isLocWithinAnInactiveClause(const SourceLoc loc, SourceFile *SF) {
  class InactiveClauseTester : public ASTWalker {
    const SourceLoc loc;
    const SourceManager &SM;

  public:
    bool wasFoundWithinInactiveClause = false;

    InactiveClauseTester(const SourceLoc loc, const SourceManager &SM)
        : loc(loc), SM(SM) {}

    bool walkToDeclPre(Decl *D) override {
      if (const auto *ifc = dyn_cast<IfConfigDecl>(D)) {
        for (const auto &clause : ifc->getClauses()) {
          if (clause.isActive)
            continue;
          for (const auto &n : clause.Elements) {
            SourceRange sr = n.getSourceRange();
            if (sr.isValid() && SM.rangeContainsTokenLoc(sr, loc)) {
              wasFoundWithinInactiveClause = true;
              return false;
            }
          }
        }
      }
      return ASTWalker::walkToDeclPre(D);
    }
  };
  InactiveClauseTester tester(loc, SF->getASTContext().SourceMgr);
  SF->walk(tester);
  return tester.wasFoundWithinInactiveClause;
}
