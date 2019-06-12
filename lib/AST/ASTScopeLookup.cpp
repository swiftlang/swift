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
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>

using namespace swift;
using namespace namelookup;
using namespace ast_scope;

Optional<bool> ASTScopeImpl::unqualifiedLookup(
    SourceFile *sourceFile, const DeclName name, const SourceLoc loc,
    const DeclContext *const startingContext,
    const Optional<bool> isCascadingUseArg, DeclConsumer consumer) {
  const auto *start =
      findStartingScopeForLookup(sourceFile, name, loc, startingContext);
  if (!start)
    return isCascadingUseArg;

  return start->lookup(NullablePtr<DeclContext>(), nullptr, nullptr,
                       isCascadingUseArg, consumer);
}

const ASTScopeImpl *ASTScopeImpl::findStartingScopeForLookup(
    SourceFile *sourceFile, const DeclName name, const SourceLoc loc,
    const DeclContext *const startingContext) {
  // At present, use legacy code in unqualifiedLookup.cpp to handle module-level
  // lookups
  // TODO: implement module scope someday
  if (startingContext->getContextKind() == DeclContextKind::Module)
    return nullptr;

  auto *const fileScope = sourceFile->getScope().impl;
  // Parser may have added decls to source file, since previous lookup
  sourceFile->getScope().impl->addNewDeclsToTree();
  if (name.isOperator())
    return fileScope; // operators always at file scope

  const auto innermost = fileScope->findInnermostEnclosingScope(loc);

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
    llvm::errs() << "'";
    name.print(llvm::errs());
    llvm::errs() << "' ";
    llvm::errs() << "loc: ";
    loc.dump(sourceFile->getASTContext().SourceMgr);
    llvm::errs() << "\nstarting context:\n ";
    startingContext->dumpContext();
    //    llvm::errs() << "\ninnermost: ";
    //    innermost->dump();
    //    llvm::errs() << "in: \n";
    //    fileScope->dump();
    llvm::errs() << "\n\n";
  }

  assert(startingScope && "ASTScopeImpl: could not find startingScope");
  return startingScope;
}

const ASTScopeImpl *
ASTScopeImpl::findInnermostEnclosingScope(SourceLoc loc) const {
  SourceManager &sourceMgr = getSourceManager();

  const auto *s = this;
  for (NullablePtr<const ASTScopeImpl> c;
       (c = s->findChildContaining(loc, sourceMgr)); s = c.get()) {
  }
  return s;
}

NullablePtr<const ASTScopeImpl>
ASTScopeImpl::findChildContaining(SourceLoc loc,
                                  SourceManager &sourceMgr) const {
  // Use binary search to find the child that contains this location.
  struct CompareLocs {
    SourceManager &sourceMgr;

    bool operator()(const ASTScopeImpl *scope, SourceLoc loc) {
      return sourceMgr.isBeforeInBuffer(scope->getSourceRange().End, loc);
    }
    bool operator()(SourceLoc loc, const ASTScopeImpl *scope) {
      return sourceMgr.isBeforeInBuffer(loc, scope->getSourceRange().End);
    }
  };
  auto *const *child = std::lower_bound(
      getChildren().begin(), getChildren().end(), loc, CompareLocs{sourceMgr});

  if (child != getChildren().end() &&
      sourceMgr.rangeContainsTokenLoc((*child)->getSourceRange(), loc))
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
  llvm_unreachable("topmost scope always has a context, the SourceFile");
}

// For a SubscriptDecl with generic parameters, the call tries to do lookups
// with startingContext equal to either the get or set subscript
// AbstractFunctionDecls. Since the generic parameters are in the
// SubScriptDeclScope, and not the AbstractFunctionDecl scopes (after all how
// could one parameter be in two scopes?), GenericParamScoped intercepts the
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

#pragma mark lookup methods that run once per scope

Optional<bool> ASTScopeImpl::lookup(
    const NullablePtr<DeclContext> selfDC,
    const NullablePtr<const ASTScopeImpl> limit,
    const NullablePtr<const Decl> scopeWhoseTypeWasAlreadySearched,
    const Optional<bool> isCascadingUseArg, DeclConsumer consumer) const {
#ifndef NDEBUG
  consumer.stopForDebuggingIfTargetLookup();
#endif

  // Certain illegal nestings, e.g. protocol nestled inside a struct,
  // require that lookup stop at the outer scope.
  if (this == limit.getPtrOrNull())
    return isCascadingUseArg;

  const Optional<bool> isCascadingUseForThisScope =
      resolveIsCascadingUseForThisScope(isCascadingUseArg);
  // Check local variables, etc. first.
  if (lookupLocalBindings(isCascadingUseForThisScope, consumer))
    return isCascadingUseForThisScope;

  /// Because a body scope nests in a generic param scope, etc, we might look in
  /// the self type twice. That's why we pass scopeWhoseTypeWasAlreadySearched.
  /// Look in the generics and self type only iff haven't already looked there.
  const bool skipSearchForGenericsAndMembers =
      scopeWhoseTypeWasAlreadySearched &&
      scopeWhoseTypeWasAlreadySearched == getDecl().getPtrOrNull();

  // Look for generics before members in violation of lexical ordering because
  // you can say "self.name" to get a name shadowed by a generic but you
  // can't do the opposite to get a generic shadowed by a name.
  if (!skipSearchForGenericsAndMembers) {
    if (lookInGenericParameters(isCascadingUseForThisScope, consumer))
      return isCascadingUseForThisScope;
  }
  // Dig out the type we're looking into.
  // Perform lookup into the type
  Optional<bool> isCascadingUseResult = isCascadingUseForThisScope;
  if (!skipSearchForGenericsAndMembers) {
    bool isDone;
    std::tie(isDone, isCascadingUseResult) =
        lookupInSelfType(selfDC, isCascadingUseResult, consumer);
    if (isDone)
      return isCascadingUseResult;
  }

  return lookupInParent(selfDC, limit, scopeWhoseTypeWasAlreadySearched,
                        isCascadingUseResult, consumer);
}

Optional<bool> ASTScopeImpl::lookupInParent(
    const NullablePtr<DeclContext> selfDC,
    const NullablePtr<const ASTScopeImpl> limit,
    const NullablePtr<const Decl> scopeWhoseTypeWasAlreadySearched,
    const Optional<bool> isCascadingUse, DeclConsumer consumer) const {

  const auto *const lookupParent = getLookupParent().getPtrOrNull();
  if (!lookupParent)
    return isCascadingUse;

  // If this scope has an associated Decl, we have already searched its generics
  // and selfType, so no need to look again.
  NullablePtr<const Decl> haveAlreadyLookedHereForParent =
      getDecl() ? getDecl().getPtrOrNull() : scopeWhoseTypeWasAlreadySearched;

  // If there is no limit and this scope induces one, pass that on.
  const NullablePtr<const ASTScopeImpl> limitForParent =
      limit ? limit : getLookupLimit();

  return lookupParent->lookup(computeSelfDCForParent(selfDC), limitForParent,
                              haveAlreadyLookedHereForParent, isCascadingUse,
                              consumer);
}

#pragma mark lookInGenericParameters

bool ASTScopeImpl::lookInGenericParameters(Optional<bool> isCascadingUse,
                                           ASTScopeImpl::DeclConsumer) const {
  return false;
}

bool AbstractFunctionDeclScope::lookInGenericParameters(
    Optional<bool> isCascadingUse, ASTScopeImpl::DeclConsumer consumer) const {
  return lookInMyAndOuterGenericParameters(decl, isCascadingUse, consumer);
}
bool SubscriptDeclScope::lookInGenericParameters(
    Optional<bool> isCascadingUse, ASTScopeImpl::DeclConsumer consumer) const {
  return lookInMyAndOuterGenericParameters(decl, isCascadingUse, consumer);
}

bool GenericTypeOrExtensionScope::lookInGenericParameters(
    Optional<bool> isCascadingUse, ASTScopeImpl::DeclConsumer consumer) const {
  // For Decls:
  // WAIT, WHAT?! Isn't this covered by the GenericParamScope
  // lookupLocalBindings? No, that's for use of generics in the body. This is
  // for generic restrictions.

  // For Bodies:
  // Sigh... These must be here so that from body, we search generics before
  // members. But they also must be on the Decl scope for lookups starting from
  // generic parameters, where clauses, etc.
  return lookInMyAndOuterGenericParameters(getGenericContext(), isCascadingUse,
                                           consumer);
}

bool ASTScopeImpl::lookInMyAndOuterGenericParameters(
    const GenericContext *const gc, Optional<bool> isCascadingUse,
    ASTScopeImpl::DeclConsumer consumer) {
  for (auto *params = gc->getGenericParams(); params;
       params = params->getOuterParameters()) {
    SmallVector<ValueDecl *, 32> bindings;
    for (auto *param : params->getParams())
      bindings.push_back(param);
    if (consumer.consume(bindings, DeclVisibilityKind::GenericParameter,
                         isCascadingUse))
      return true;
  }
  return false;
}

#pragma mark lookupInSelfType

std::pair<bool, Optional<bool>>
ASTScopeImpl::lookupInSelfType(NullablePtr<DeclContext>,
                               const Optional<bool> isCascadingUse,
                               DeclConsumer) const {
  return doNotLookupInSelfType(isCascadingUse);
}

std::pair<bool, Optional<bool>> GenericTypeOrExtensionScope::lookupInSelfType(
    NullablePtr<DeclContext> selfDC, const Optional<bool> isCascadingUse,
    ASTScopeImpl::DeclConsumer consumer) const {
  return portion->lookupInSelfTypeOf(this, selfDC, isCascadingUse, consumer);
}

std::pair<bool, Optional<bool>> Portion::lookupInSelfTypeOf(
    const GenericTypeOrExtensionScope *scope, NullablePtr<DeclContext> selfDC,
    const Optional<bool> isCascadingUse, ASTScopeImpl::DeclConsumer) const {
  return scope->doNotLookupInSelfType(isCascadingUse);
}

std::pair<bool, Optional<bool>>
GenericTypeOrExtensionWhereOrBodyPortion::lookupInSelfTypeOf(
    const GenericTypeOrExtensionScope *scope, NullablePtr<DeclContext> selfDC,
    const Optional<bool> isCascadingUse,
    ASTScopeImpl::DeclConsumer consumer) const {
  auto nt = scope->getCorrespondingNominalTypeDecl().getPtrOrNull();
  if (!nt)
    return Portion::lookupInSelfTypeOf(scope, selfDC, isCascadingUse, consumer);
  return consumer.lookupInSelfType(selfDC, scope->getDeclContext().get(), nt,
                                   isCascadingUse);
}

#pragma mark lookupLocalBindings

bool ASTScopeImpl::lookupLocalBindings(Optional<bool> isCascadingUse,
                                       DeclConsumer consumer) const {
  return false; // most kinds of scopes have none
}

bool GenericParamScope::lookupLocalBindings(Optional<bool> isCascadingUse,
                                            DeclConsumer consumer) const {
  auto *param = paramList->getParams()[index];
  return consumer.consume({param}, DeclVisibilityKind::GenericParameter,
                          isCascadingUse);
}

bool PatternEntryUseScope::lookupLocalBindings(Optional<bool> isCascadingUse,
                                               DeclConsumer consumer) const {
  if (vis != DeclVisibilityKind::LocalVariable)
    return false; // look in self type will find this later
  return lookupLocalBindingsInPattern(getPattern(), isCascadingUse, vis,
                                      consumer);
}

bool ForEachPatternScope::lookupLocalBindings(Optional<bool> isCascadingUse,
                                              DeclConsumer consumer) const {
  return lookupLocalBindingsInPattern(stmt->getPattern(), isCascadingUse,
                                      DeclVisibilityKind::LocalVariable,
                                      consumer);
}

bool CatchStmtScope::lookupLocalBindings(Optional<bool> isCascadingUse,
                                         DeclConsumer consumer) const {
  return lookupLocalBindingsInPattern(stmt->getErrorPattern(), isCascadingUse,
                                      DeclVisibilityKind::LocalVariable,
                                      consumer);
}

bool CaseStmtScope::lookupLocalBindings(Optional<bool> isCascadingUse,
                                        DeclConsumer consumer) const {
  for (auto &item : stmt->getMutableCaseLabelItems())
    if (lookupLocalBindingsInPattern(item.getPattern(), isCascadingUse,
                                     DeclVisibilityKind::LocalVariable,
                                     consumer))
      return true;
  return false;
}

bool AbstractFunctionBodyScope::lookupLocalBindings(
    Optional<bool> isCascadingUse, DeclConsumer consumer) const {
  if (auto *paramList = decl->getParameters()) {
    for (auto *paramDecl : *paramList)
      if (consumer.consume({paramDecl}, DeclVisibilityKind::FunctionParameter,
                           isCascadingUse))
        return true;
  }
  return false;
}

bool MethodBodyScope::lookupLocalBindings(Optional<bool> isCascadingUse,
                                          DeclConsumer consumer) const {
  assert(decl->getImplicitSelfDecl());
  if (AbstractFunctionBodyScope::lookupLocalBindings(isCascadingUse, consumer))
    return true;
  return consumer.consume({decl->getImplicitSelfDecl()},
                          DeclVisibilityKind::FunctionParameter,
                          isCascadingUse);
}

bool PureFunctionBodyScope::lookupLocalBindings(Optional<bool> isCascadingUse,
                                                DeclConsumer consumer) const {
  assert(!decl->getImplicitSelfDecl());
  if (AbstractFunctionBodyScope::lookupLocalBindings(isCascadingUse, consumer))
    return true;

  // Consider \c var t: T { (did/will/)get/set { ... t }}
  // Lookup needs to find t, but if the var is inside of a type the baseDC needs
  // to be set. It all works fine, except: if the var is not inside of a type,
  // then t needs to be found as a local binding:
  if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
    if (auto *storage = accessor->getStorage())
      if (consumer.consume({storage}, DeclVisibilityKind::LocalVariable,
                           isCascadingUse))
        return true;
  }
  return false;
}

bool SpecializeAttributeScope::lookupLocalBindings(
    Optional<bool> isCascadingUse, DeclConsumer consumer) const {
  if (auto *params = whatWasSpecialized->getGenericParams())
    for (auto *param : params->getParams())
      if (consumer.consume({param}, DeclVisibilityKind::GenericParameter,
                           isCascadingUse))
        return true;
  return false;
}

bool BraceStmtScope::lookupLocalBindings(Optional<bool> isCascadingUse,
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
      if (isa<AbstractFunctionDecl>(localBinding) ||
          isa<TypeDecl>(localBinding))
        localBindings.push_back(cast<ValueDecl>(localBinding));
    }
  }
  return consumer.consume(localBindings, DeclVisibilityKind::LocalVariable,
                          isCascadingUse);
}

bool PatternEntryInitializerScope::lookupLocalBindings(
    Optional<bool> isCascadingUse, DeclConsumer consumer) const {
  // 'self' is available within the pattern initializer of a 'lazy' variable.
  auto *initContext = cast_or_null<PatternBindingInitializer>(
      decl->getPatternList()[0].getInitContext());
  if (initContext) {
    if (auto *selfParam = initContext->getImplicitSelfDecl()) {
      return consumer.consume(
          {selfParam}, DeclVisibilityKind::FunctionParameter, isCascadingUse);
    }
  }
  return false;
}

bool ClosureParametersScope::lookupLocalBindings(Optional<bool> isCascadingUse,
                                                 DeclConsumer consumer) const {
  if (auto *cl = captureList.getPtrOrNull()) {
    CaptureListExpr *mutableCL =
        const_cast<CaptureListExpr *>(captureList.get());
    for (auto &e : mutableCL->getCaptureList()) {
      if (consumer.consume({e.Var}, DeclVisibilityKind::LocalVariable,
                           isCascadingUse)) // or FunctionParameter??
        return true;
    }
  }
  for (auto param : *closureExpr->getParameters())
    if (consumer.consume({param}, DeclVisibilityKind::FunctionParameter,
                         isCascadingUse))
      return true;
  return false;
}

bool ConditionalClausePatternUseScope::lookupLocalBindings(
    Optional<bool> isCascadingUse, DeclConsumer consumer) const {
  return lookupLocalBindingsInPattern(
      pattern, isCascadingUse, DeclVisibilityKind::LocalVariable, consumer);
}

bool ASTScopeImpl::lookupLocalBindingsInPattern(Pattern *p,
                                                Optional<bool> isCascadingUse,
                                                DeclVisibilityKind vis,
                                                DeclConsumer consumer) {
  if (!p)
    return false;
  bool isDone = false;
  p->forEachVariable([&](VarDecl *var) {
    if (!isDone)
      isDone = consumer.consume({var}, vis, isCascadingUse);
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

NullablePtr<const ASTScopeImpl> ASTScopeImpl::ancestorWithDeclSatisfying(
    function_ref<bool(const Decl *)> predicate) const {
  for (NullablePtr<const ASTScopeImpl> s = getParent(); s;
       s = s.get()->getParent()) {
    if (Decl *d = s.get()->getDecl().getPtrOrNull()) {
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
NullablePtr<DeclContext>
ASTScopeImpl::computeSelfDCForParent(NullablePtr<DeclContext> selfDC) const {
  return selfDC;
}

// Forget the "self" declaration:
NullablePtr<DeclContext> GenericTypeOrExtensionScope::computeSelfDCForParent(
    NullablePtr<DeclContext>) const {
  return nullptr;
}

NullablePtr<DeclContext> PatternEntryInitializerScope::computeSelfDCForParent(
    NullablePtr<DeclContext> selfDC) const {
  // Pattern binding initializers are only interesting insofar as they
  // affect lookup in an enclosing nominal type or extension thereof.
  if (auto *ic = getPatternEntry().getInitContext()) {
    if (auto *bindingInit = dyn_cast<PatternBindingInitializer>(ic)) {
      // Lazy variable initializer contexts have a 'self' parameter for
      // instance member lookup.
      if (bindingInit->getImplicitSelfDecl()) {
        assert((selfDC.isNull() || selfDC == bindingInit) &&
               "Would lose information");
        return bindingInit;
      }
    }
  }
  return selfDC;
}

NullablePtr<DeclContext>
MethodBodyScope::computeSelfDCForParent(NullablePtr<DeclContext> selfDC) const {
  assert(!selfDC && "Losing selfDC");
  return decl;
}
NullablePtr<DeclContext> PureFunctionBodyScope::computeSelfDCForParent(
    NullablePtr<DeclContext> selfDC) const {
  return selfDC;
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
  llvm_unreachable("generic what?");
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
  auto *PBI = cast_or_null<PatternBindingInitializer>(initContext);
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
