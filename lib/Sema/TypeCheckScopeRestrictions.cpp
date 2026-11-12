//===--- TypeCheckScopeRestrictions.cpp - Scopes in signatures ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Resolves scopes in function signatures to be referenced in types.
//
// A function type binds scope parameters for each of `@_scopeParams` and then
// accesses the signature refers to that don't correspond to a written scope.
// The scopes of parameter access are determined syntactically to avoid really
// messy cycles, which we will need to revisit to support elision in types.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckType.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ScopeRestrictions.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/TypeWalker.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallVectorExtras.h"

using namespace swift;

/// Whether a parameter is borrowed or mutated for the call rather than
/// consumed, so that it has an access scope.
static bool hasAccess(ParamDecl *param) {
  return param->getLoweredOwnership() != LoweredOwnership::Owned;
}

/// Whether \p fn has a `self` whose access `&self` can refer to. A consuming
/// method's is diagnosed when checking the declaration.
static bool canHaveSelfAccess(AbstractFunctionDecl *fn) {
  return fn->hasImplicitSelfDecl() && !fn->isStatic() &&
         !isa<ConstructorDecl>(fn);
}

//===----------------------------------------------------------------------===//
// MARK: - The syntax of signatures
//===----------------------------------------------------------------------===//

/// Peel off a layer of attribute, specifier, or parens from \p repr.
///
/// TODO: Share this with the other code that looks through these, as a
/// `TypeRepr` method since otherwise new wrappers are unevenly supported...
static TypeRepr *getWrappedRepr(TypeRepr *repr) {
  if (auto *attributed = dyn_cast<AttributedTypeRepr>(repr))
    return attributed->getTypeRepr();
  if (auto *specifier = dyn_cast<SpecifierTypeRepr>(repr))
    return specifier->getBase();
  if (auto *nonsending = dyn_cast<NonisolatedNonsendingTypeRepr>(repr))
    return nonsending->getBase();
  if (repr->isParenType())
    return repr->getWithoutParens();
  return nullptr;
}

static TypeRepr *lookThroughAllWrappers(TypeRepr *repr) {
  while (auto *wrapped = getWrappedRepr(repr))
    repr = wrapped;
  return repr;
}

static ArrayRef<Located<ScopeName>>
getDeclaredScopeParams(AbstractFunctionDecl *fn) {
  if (auto *attr = fn->getAttrs().getAttribute<ScopeParamsAttr>())
    return attr->getParamList()->getParams();
  return {};
}

static std::optional<unsigned> findParam(AbstractFunctionDecl *fn,
                                         Identifier name) {
  for (auto [index, param] : llvm::enumerate(*fn->getParameters()))
    if (param->getParameterName() == name)
      return index;
  return std::nullopt;
}

/// Get the types written for \p fn's parameters and result, leaving out an
/// opaque result type.
static SmallVector<TypeRepr *> getSignatureReprs(AbstractFunctionDecl *fn) {
  SmallVector<TypeRepr *> reprs;
  for (auto *param : *fn->getParameters())
    if (auto *repr = param->getTypeRepr())
      reprs.push_back(repr);
  // An opaque result type isn't resolved with scopes. This is decided from
  // syntax, because resolving the opaque type depends on the signature.
  if (auto *func = dyn_cast<FuncDecl>(fn))
    if (auto *repr = func->getResultTypeRepr();
        repr && !func->getOpaqueResultTypeRepr())
      reprs.push_back(repr);
  return reprs;
}

/// Get the `@_scoped`s and `@_accessScope`s written anywhere in \p reprs.
///
/// This wants to be a generator...
static SmallVector<TypeAttribute *, 4>
getScopeAttrs(ArrayRef<TypeRepr *> reprs) {
  SmallVector<TypeAttribute *, 4> attrs;
  for (auto *repr : reprs)
    repr->findIf([&](TypeRepr *nested) {
      if (auto *attributed = dyn_cast<AttributedTypeRepr>(nested))
        for (auto attr : attributed->getAttrs())
          if (auto *typeAttr = attr.dyn_cast<TypeAttribute *>();
              isa_and_nonnull<ScopedTypeAttr, AccessScopeTypeAttr>(typeAttr))
            attrs.push_back(typeAttr);
      return false;
    });
  return attrs;
}

static SmallVector<ScopeRepr, 2> getWrittenScopes(TypeAttribute *attr) {
  if (auto *scoped = dyn_cast<ScopedTypeAttr>(attr))
    return llvm::map_to_vector<2>(scoped->getRestrictions(),
                                  &ScopeRestrictionRepr::getScope);
  if (auto *access = dyn_cast<AccessScopeTypeAttr>(attr))
    return {access->getScope()};
  return {};
}

/// Find the `@_accessScope` that binds the access of a parameter of type
/// \p repr, if one is written. Diagnoses and invalidates repeated or illegal
/// access scopes.
static AccessScopeTypeAttr *getAccessBinding(DiagnosticEngine &diags,
                                             TypeRepr *repr) {
  AccessScopeTypeAttr *binding = nullptr;
  for (auto *layer = repr; layer; layer = getWrappedRepr(layer)) {
    auto *attributed = dyn_cast<AttributedTypeRepr>(layer);
    if (!attributed)
      continue;
    auto *access = cast_or_null<AccessScopeTypeAttr>(
        attributed->get(TypeAttrKind::AccessScope));
    if (!access)
      continue;
    if (!binding) {
      binding = access;
    } else if (!access->isInvalid()) {
      diags.diagnose(access->getAttrLoc(),
                     diag::access_scope_bound_repeatedly);
      access->setInvalid();
    }
  }
  if (!binding)
    return nullptr;

  auto scope = binding->getScope();
  if (!binding->isInvalid() && scope.getKind() == ScopeRepr::Kind::Immortal) {
    // TODO: Support accesses that may last forever, like borrows of globals.
    diags.diagnose(scope.getLoc(), diag::access_scope_immortal_unsupported);
    binding->setInvalid();
  }
  return binding;
}

bool swift::resolvesScopesInSignature(AbstractFunctionDecl *fn) {
  if (!fn->getASTContext().LangOpts.hasFeature(Feature::ScopeRestrictions))
    return false;
  // TODO: Support accessors, whose parameters are consumed by default.
  if (isa<AccessorDecl>(fn))
    return false;
  // Imported and deserialized functions have no written signature.
  return fn->getParentSourceFile() && !fn->hasClangNode();
}

/// Whether \p fn can leave out the scopes of its types. Scopes aren't part of
/// the ABI, so an ABI-only declaration doesn't need to saturate types with
/// scopes. Avoid diagnosing missing scopes when this is true!
static bool canOmitScopes(AbstractFunctionDecl *fn) {
  return !ABIRoleInfo(fn).providesAPI();
}

//===----------------------------------------------------------------------===//
// MARK: - Binders
//===----------------------------------------------------------------------===//

ScopeBinder::ScopeBinder(AbstractFunctionDecl *fn)
    : ScopeBinder(fn, evaluateOrFatal(fn->getASTContext().evaluator,
                                      ParamAccessScopesRequest{fn})) {}

ScopeBinder::ScopeBinder(AbstractFunctionDecl *fn,
                         const ParamAccessScopes &accesses)
    : Fn(fn), Repr(nullptr), Declared(getDeclaredScopeParams(fn)),
      Parent(nullptr), ParamAccesses(accesses.Params),
      IsSelfReferenced(accesses.IsSelfReferenced) {}

bool ScopeBinder::namesFunctionTypeParam(Identifier name) const {
  for (auto *at = this; at->Repr; at = at->Parent)
    if (llvm::any_of(at->Repr->getArgsTypeRepr()->getElements(),
                     [&](auto &elt) {
                       return elt.Name == name || elt.SecondName == name;
                     }))
      return true;
  return false;
}

std::optional<ScopeRef> ScopeBinder::lookUpScopeParam(ScopeName name) const {
  auto depth = ScopeDepth::zero();
  for (auto *at = this; at; at = at->Parent, depth = depth.deeper()) {
    auto found = llvm::find_if(
        at->Declared, [&](auto &param) { return param.Item == name; });
    if (found != at->Declared.end())
      return ScopeRef::forParam(depth, found - at->Declared.begin());
  }
  return std::nullopt;
}

std::optional<ScopeRef> ScopeBinder::lookUpParamAccess(unsigned index) const {
  auto depth = ScopeDepth::zero();
  auto *at = this;
  for (; at->Parent; at = at->Parent)
    depth = depth.deeper();
  ASSERT(index < at->ParamAccesses.size() &&
         "the accesses of the parameters are resolved");
  auto access = at->ParamAccesses[index];
  if (!access)
    return std::nullopt;
  ASSERT(*access != ScopeRef::forCallBody() &&
         "every access the signature refers to has a scope");
  return access->shifted(depth);
}

ScopeRef ScopeBinder::lookUpSelfAccess() const {
  auto depth = ScopeDepth::zero();
  auto *at = this;
  for (; at->Parent; at = at->Parent)
    depth = depth.deeper();
  ASSERT(at->IsSelfReferenced &&
         "every `&self` in the signature gets a scope");
  // It's the only scope parameter of the method's outer type, which is one
  // function type outside the declaration's.
  return ScopeRef::forParam(depth.deeper(), /*index=*/0);
}

/// Walk \p fnTy to find which of its scope parameters are referred to in its
/// parameter and result types. Binding the access of one of \p fnTy's params
/// isn't a use, but binding the access of a function type's param or
/// restricting a non-escapable type to one is.
static SmallBitVector getUsedScopeParams(AnyFunctionType *fnTy) {
  class UseFinder : public TypeWalker {
    SmallBitVector &Used;
    /// The number of function types entered within `fnTy`.
    ScopeDepth Depth = ScopeDepth::zero();

    void use(ScopeRef scope) {
      if (scope.getKind() != ScopeRef::Kind::Param || scope.getDepth() != Depth)
        return;
      ASSERT(scope.getIndex() < Used.size() &&
             "a scope in a signature refers to a scope parameter it binds");
      Used.set(scope.getIndex());
    }

  public:
    explicit UseFinder(SmallBitVector &used) : Used(used) {}

    Action walkToTypePre(Type ty) override {
      if (auto *nominal = dyn_cast<NominalType>(ty.getPointer()))
        for (auto scope : nominal->getScopeArgs())
          use(scope);
      if (auto *nested = dyn_cast<AnyFunctionType>(ty.getPointer())) {
        Depth = Depth.deeper();
        for (auto &param : nested->getParams())
          use(param.getAccessScope());
      }
      return Action::Continue;
    }

    Action walkToTypePost(Type ty) override {
      if (isa<AnyFunctionType>(ty.getPointer()))
        Depth = Depth.shallower();
      return Action::Continue;
    }
  };

  SmallBitVector used(fnTy->getScopeSignature().getNumParams());
  UseFinder finder(used);
  // Canonical types don't hide scopes behind sugar like type aliases.
  for (auto &param : fnTy->getParams())
    param.getPlainType()->getCanonicalType().walk(finder);
  fnTy->getResult()->getCanonicalType().walk(finder);
  return used;
}

/// Diagnose the scope parameters in \p declared of \p fnTy that are declared
/// repeatedly or unused.
static void checkDeclaredScopeParams(AbstractFunctionDecl *fn,
                                     ArrayRef<Located<ScopeName>> declared,
                                     Type fnTy) {
  auto &diags = fn->getASTContext().Diags;
  SmallBitVector redeclared(declared.size());
  for (auto [index, param] : llvm::enumerate(declared)) {
    auto earlier = declared.take_front(index);
    auto previous = llvm::find_if(
        earlier, [item = param.Item](auto p) { return p.Item == item; });
    if (previous != earlier.end()) {
      auto name = param.Item.getIdentifier();
      diags.diagnose(param.Loc, diag::scope_param_redeclared, name);
      diags.diagnose(previous->Loc, diag::invalid_redecl_prev_name, name);
      redeclared.set(index);
    }
  }

  // Resolving a type that was diagnosed drops the scopes written in it.
  if (declared.empty() || canOmitScopes(fn) || fnTy->hasError())
    return;
  auto used = getUsedScopeParams(fnTy->castTo<AnyFunctionType>());
  // A name refers to its first declaration, so a redeclaration is never used.
  for (auto [index, param] : llvm::enumerate(declared))
    if (!used[index] && !redeclared[index])
      diags.diagnose(param.Loc, diag::scope_param_unused,
                     param.Item.getIdentifier());
}

void swift::checkDeclaredScopeParams(const ScopeBinder &binder,
                                     AnyFunctionType *fnTy) {
  ASSERT(binder.getRepr() &&
         "checkScopedSignature checks the declaration's scope parameters");
  ::checkDeclaredScopeParams(binder.getFunction(), binder.getDeclared(), fnTy);
}

//===----------------------------------------------------------------------===//
// MARK: - Resolving written scopes
//===----------------------------------------------------------------------===//

/// Resolves \p repr, a scope written on a parameter or result of \p binder's
/// function type, returning `nullopt` if it doesn't resolve. Diagnoses unknown
/// scopes if they haven't already been diagnosed.
static std::optional<ScopeRef> resolveWrittenScope(const ScopeBinder &binder,
                                                   ScopeRepr repr) {
  auto *fn = binder.getFunction();
  auto &ctx = fn->getASTContext();
  auto &diags = ctx.Diags;
  auto loc = repr.getLoc();

  switch (repr.getKind()) {
  case ScopeRepr::Kind::Immortal:
    return ScopeRef::forImmortal();

  case ScopeRepr::Kind::ScopeName: {
    auto name = repr.getScopeName().getIdentifier();
    if (binder.namesFunctionTypeParam(name)) {
      diags.diagnose(loc, diag::scoped_function_type_param);
      return std::nullopt;
    }
    if (auto scope = binder.lookUpScopeParam(repr.getScopeName()))
      return scope;
    // TODO: Support the scope of a parameter's type as a shorthand for a scope
    // parameter?
    if (findParam(fn, name))
      diags.diagnose(loc, diag::scoped_param_scope_unsupported, name);
    else
      diags.diagnose(loc, diag::scope_unknown, name);
    return std::nullopt;
  }

  case ScopeRepr::Kind::Self:
    if (fn->hasImplicitSelfDecl())
      diags.diagnose(loc, diag::scoped_param_scope_unsupported, ctx.Id_self);
    else
      diags.diagnose(loc, diag::scoped_unknown_param, ctx.Id_self);
    return std::nullopt;

  case ScopeRepr::Kind::AccessedValue: {
    auto name = repr.getAccessedValue();
    if (binder.namesFunctionTypeParam(name)) {
      diags.diagnose(loc, diag::scoped_function_type_param);
      return std::nullopt;
    }
    auto index = findParam(fn, name);
    if (!index) {
      diags.diagnose(loc, diag::scoped_unknown_param, name);
      return std::nullopt;
    }
    return binder.lookUpParamAccess(*index);
  }

  case ScopeRepr::Kind::AccessedSelf:
    ASSERT(canHaveSelfAccess(fn) &&
           "resolving the accesses diagnoses a `&self` without one");
    return binder.lookUpSelfAccess();
  }
  llvm_unreachable("unhandled kind");
}

Type swift::applyScopedAttr(const ScopeBinder &binder, ScopedTypeAttr *attr,
                            Type ty) {
  auto &ctx = binder.getFunction()->getASTContext();
  auto &diags = ctx.Diags;
  if (ty->hasError())
    return ty;

  // FIXME: Diagnose scope restrictions on types without scope parameters,
  // such as escapable types and function types.
  auto *nominal = ty->getAs<NominalType>();
  if (!nominal || getNumScopeParams(nominal->getDecl()) == 0)
    return ty;
  // TODO: Whether a type with several scope parameters elides each argument
  // into its own scope parameter decides whether a type's scope also needs
  // the argument's index.
  ASSERT(getNumScopeParams(nominal->getDecl()) == 1 &&
         "a type has at most one scope parameter");

  if (nominal->hasScopeArgs()) {
    diags.diagnose(attr->getAttrLoc(), diag::scoped_applied_repeatedly);
    return ErrorType::get(ctx);
  }
  // FIXME: Support types with several scope parameters.
  auto restrictions = attr->getRestrictions();
  if (restrictions.size() > 1) {
    diags.diagnose(restrictions[1].getScope().getLoc(), diag::scoped_multiple);
    return ErrorType::get(ctx);
  }
  auto restriction = restrictions.front();
  if (auto label = restriction.getLabel()) {
    diags.diagnose(label->Loc, diag::scoped_labeled);
    return ErrorType::get(ctx);
  }
  auto scope = resolveWrittenScope(binder, restriction.getScope());
  if (!scope)
    return ErrorType::get(ctx);
  return NominalType::get(nominal->getDecl(), nominal->getParent(),
                          nominal->getGenericArgs(), ctx, *scope);
}

//===----------------------------------------------------------------------===//
// MARK: - Binding the accesses of parameters
//===----------------------------------------------------------------------===//

/// Resolve the scope \p binding binds its access to, relative to \p binder.
/// Returns `nullopt` if it doesn't resolve or was already diagnosed, and
/// diagnoses it if it wasn't.
static std::optional<ScopeRef> resolveBinding(const ScopeBinder &binder,
                                              AccessScopeTypeAttr *binding) {
  if (binding->isInvalid())
    return std::nullopt;
  auto scope = binding->getScope();
  ASSERT(scope.getKind() != ScopeRepr::Kind::Immortal &&
         "a binding to 'immortal' is invalid");
  ASSERT((binder.getRepr() ||
          scope.getKind() != ScopeRepr::Kind::AccessedValue ||
          !findParam(binder.getFunction(), scope.getAccessedValue())) &&
         "a binding to another parameter's access is invalid");
  auto resolved = resolveWrittenScope(binder, scope);
  if (!resolved)
    binding->setInvalid();
  return resolved;
}

std::optional<ScopeRef> swift::resolveAccessBinding(const ScopeBinder &binder,
                                                    TypeRepr *repr,
                                                    ParameterTypeFlags flags) {
  auto &diags = binder.getFunction()->getASTContext().Diags;
  auto *binding = getAccessBinding(diags, repr);
  if (!binding)
    return ScopeRef::forCallBody();
  auto scope = resolveBinding(binder, binding);
  if (scope && flags.getValueOwnership() == ValueOwnership::Owned) {
    diags.diagnose(binding->getAttrLoc(), diag::scoped_consuming_param);
    binding->setInvalid();
    return std::nullopt;
  }
  return scope;
}

ParamAccessScopes
ParamAccessScopesRequest::evaluate(Evaluator &evaluator,
                                   AbstractFunctionDecl *fn) const {
  ASSERT(resolvesScopesInSignature(fn));
  auto reprs = getSignatureReprs(fn);
  auto *params = fn->getParameters();
  SmallVector<bool> isReferenced(params->size());
  bool isSelfReferenced = false;
  auto &ctx = fn->getASTContext();
  auto &diags = ctx.Diags;
  for (auto *attr : getScopeAttrs(reprs)) {
    for (auto scope : getWrittenScopes(attr)) {
      if (scope.getKind() == ScopeRepr::Kind::AccessedSelf) {
        if (canHaveSelfAccess(fn)) {
          isSelfReferenced = true;
        } else if (!attr->isInvalid()) {
          if (fn->hasImplicitSelfDecl())
            diags.diagnose(scope.getLoc(), diag::scoped_self_no_access);
          else
            diags.diagnose(scope.getLoc(), diag::scoped_unknown_param,
                           ctx.Id_self);
          attr->setInvalid();
        }
      } else if (scope.getKind() == ScopeRepr::Kind::AccessedValue) {
        if (auto index = findParam(fn, scope.getAccessedValue()))
          isReferenced[*index] = true;
      }
    }
  }

  ParamAccessScopes accesses;
  accesses.IsSelfReferenced = isSelfReferenced;

  // A parameter's access can't be bound to another's, so the bindings resolve
  // without the accesses of the parameters.
  ScopeBinder binder(fn, accesses);
  unsigned next = binder.getDeclared().size();
  SmallVector<std::optional<ScopeRef>> paramAccesses;
  for (auto [param, referenced] : llvm::zip_equal(*params, isReferenced)) {
    auto *repr = param->getTypeRepr();
    auto *binding = repr ? getAccessBinding(diags, repr) : nullptr;
    if (binding &&
        binding->getScope().getKind() == ScopeRepr::Kind::AccessedValue &&
        findParam(fn, binding->getScope().getAccessedValue())) {
      // TODO: Support binding a parameter's access to another's, which can be
      // written with a scope parameter bound to both instead. Supporting this
      // complicates the checking needed to determine which access to promote.
      diags.diagnose(binding->getScope().getLoc(),
                     diag::access_scope_bound_to_param_unsupported);
      binding->setInvalid();
    }
    if (binding)
      paramAccesses.push_back(resolveBinding(binder, binding));
    else if (referenced)
      paramAccesses.push_back(ScopeRef::forParam(ScopeDepth::zero(), next++));
    else
      paramAccesses.push_back(ScopeRef::forCallBody());
  }
  accesses.Signature = ScopeSignature(next);
  accesses.Params = ctx.AllocateCopy(paramAccesses);
  return accesses;
}

//===----------------------------------------------------------------------===//
// MARK: - Scope saturation
//===----------------------------------------------------------------------===//

/// Whether \p ty, a type in \p fn's signature, is missing scopes \p fn must
/// write. Returns false if \p ty contains an error.
static bool isMissingScopes(AbstractFunctionDecl *fn, Type ty) {
  return !ty->hasError() && !canOmitScopes(fn) && hasScopeUnsaturatedType(ty);
}

Type swift::checkScopeSaturated(AbstractFunctionDecl *fn, TypeRepr *repr,
                                Type ty) {
  if (!isMissingScopes(fn, ty))
    return ty;
  auto &ctx = fn->getASTContext();
  auto &diags = ctx.Diags;
  auto *base = lookThroughAllWrappers(repr);
  // A scope can only be written on a type named directly as a parameter or
  // result, so only there was one left out. Elsewhere, like in an optional or
  // a tuple, the type isn't supported.
  auto *nominal = ty->getInOutObjectType()->getAs<NominalType>();
  if (nominal && isa<DeclRefTypeRepr>(base) && !nominal->hasScopeArgs())
    diags.diagnose(base->getStartLoc(), diag::scoped_elided);
  else
    diags.diagnose(repr->getStartLoc(),
                   diag::scoped_nonescapable_in_param_position);
  return ErrorType::get(ctx);
}

Type swift::checkOpaqueResultScopes(FuncDecl *fn, TypeRepr *repr, Type ty) {
  // TODO: Support scopes in opaque result types, which aren't resolved with
  // them.
  if (!isMissingScopes(fn, ty))
    return ty;
  auto &ctx = fn->getASTContext();
  ctx.Diags.diagnose(repr->getStartLoc(),
                     diag::scoped_nonescapable_in_param_position);
  return ErrorType::get(ctx);
}

/// Whether the types \p params and \p result of \p fn, whose signature
/// resolves scopes, are saturated with scopes where they must be, or were
/// diagnosed.
static bool areScopesSaturated(AbstractFunctionDecl *fn,
                               ArrayRef<AnyFunctionType::Param> params,
                               Type result) {
  for (auto [param, paramDecl] : llvm::zip_equal(params, *fn->getParameters()))
    if (paramDecl->getTypeRepr() && isMissingScopes(fn, param.getPlainType()))
      return false;
  // FIXME: Diagnose the result of an initializer of a non-escapable type,
  // which isn't written.
  auto *func = dyn_cast<FuncDecl>(fn);
  return !func || !func->getResultTypeRepr() || !isMissingScopes(fn, result);
}

//===----------------------------------------------------------------------===//
// MARK: - The function's type
//===----------------------------------------------------------------------===//

void swift::addScopes(AbstractFunctionDecl *fn,
                      AnyFunctionType::ExtInfoBuilder &info,
                      MutableArrayRef<AnyFunctionType::Param> params,
                      Type result) {
  if (!resolvesScopesInSignature(fn))
    return;
  CONDITIONAL_ASSERT(areScopesSaturated(fn, params, result) &&
                     "a scope-unsaturated type escaped");
  auto accesses = evaluateOrFatal(fn->getASTContext().evaluator,
                                  ParamAccessScopesRequest{fn});
  for (auto [param, paramDecl, access] :
       llvm::zip_equal(params, *fn->getParameters(), accesses.Params))
    // Checking the declaration diagnoses the access of a consumed parameter,
    // and a parameter whose binding was diagnosed has an error type.
    param = param.withAccessScope(hasAccess(paramDecl) && access
                                      ? *access
                                      : ScopeRef::forCallBody());
  info = info.withScopeSignature(accesses.Signature);
}

void swift::addSelfScopes(AbstractFunctionDecl *fn,
                          AnyFunctionType::ExtInfoBuilder &info,
                          AnyFunctionType::Param &self) {
  if (!resolvesScopesInSignature(fn))
    return;
  auto accesses = evaluateOrFatal(fn->getASTContext().evaluator,
                                  ParamAccessScopesRequest{fn});
  if (!accesses.IsSelfReferenced)
    return;
  // The access of `self` is the only scope parameter of the outer type. A
  // consumed `self` has no access, which checking the declaration diagnoses,
  // but the outer type still binds the scope parameter `&self` refers to.
  info = info.withScopeSignature(ScopeSignature(1));
  if (hasAccess(fn->getImplicitSelfDecl()))
    self = self.withAccessScope(
        ScopeRef::forParam(ScopeDepth::zero(), /*index=*/0));
}

//===----------------------------------------------------------------------===//
// MARK: - Accesses of consumed parameters
//===----------------------------------------------------------------------===//

/// Diagnoses \p loc, a scope in \p fn's signature that refers to or binds the
/// access of \p param, if it's consumed, so that it has no access.
static void checkAccess(AbstractFunctionDecl *fn, ParamDecl *param,
                        SourceLoc loc) {
  if (hasAccess(param))
    return;
  auto &ctx = fn->getASTContext();
  auto &diags = ctx.Diags;
  diags.diagnose(loc, diag::scoped_consuming_param);
  if (param->isSelfParameter()) {
    diags.diagnose(fn->getLoc(), diag::scoped_consuming_param_here,
                   ctx.Id_self);
  } else if (param->getValueOwnership() != ValueOwnership::Default) {
    diags.diagnose(param->getLoc(), diag::scoped_consuming_param_here,
                   param->getParameterName());
  } else {
    ASSERT(isa<ConstructorDecl>(fn) &&
           "only initializers consume parameters by default");
    diags.diagnose(param->getLoc(),
                   diag::scoped_init_param_consuming_by_default);
  }
}

void swift::checkScopedSignature(AbstractFunctionDecl *fn) {
  if (!resolvesScopesInSignature(fn))
    return;
  auto &ctx = fn->getASTContext();
  auto *params = fn->getParameters();
  // Resolving the accesses diagnoses and invalidates the scopes that can't
  // refer to an access, like `&self` in a static function.
  auto accesses = evaluateOrFatal(ctx.evaluator, ParamAccessScopesRequest{fn});
  ::checkDeclaredScopeParams(fn, getDeclaredScopeParams(fn),
                             fn->hasImplicitSelfDecl()
                                 ? fn->getMethodInterfaceType()
                                 : fn->getInterfaceType());

  auto reprs = getSignatureReprs(fn);

  for (auto *attr : getScopeAttrs(reprs)) {
    if (attr->isInvalid())
      continue;
    for (auto scope : getWrittenScopes(attr)) {
      if (scope.getKind() == ScopeRepr::Kind::AccessedValue) {
        if (auto index = findParam(fn, scope.getAccessedValue()))
          checkAccess(fn, params->get(*index), scope.getLoc());
      } else if (scope.getKind() == ScopeRepr::Kind::AccessedSelf) {
        ASSERT(canHaveSelfAccess(fn) &&
               "resolving the accesses diagnoses a `&self` without one");
        checkAccess(fn, fn->getImplicitSelfDecl(), scope.getLoc());
      }
    }
  }

  for (auto [param, access] : llvm::zip_equal(*params, accesses.Params))
    if (auto *repr = param->getTypeRepr())
      // A binding that doesn't resolve was diagnosed.
      if (auto *binding = getAccessBinding(ctx.Diags, repr); binding && access)
        checkAccess(fn, param, binding->getAttrLoc());
}
