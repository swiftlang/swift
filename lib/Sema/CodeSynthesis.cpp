//===--- CodeSynthesis.cpp - Type Checking for Declarations ---------------===//
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
// This file implements semantic analysis for declarations.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"

#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Availability.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
using namespace swift;

const bool IsImplicit = true;

/// Should a particular accessor for the given storage be synthesized
/// on-demand, or is it always defined eagerly in the file that declared
/// the storage?
static bool isOnDemandAccessor(AbstractStorageDecl *storage,
                               AccessorKind kind) {
  assert(kind == AccessorKind::Get ||
         kind == AccessorKind::Set ||
         kind == AccessorKind::Read ||
         kind == AccessorKind::Modify);

  // If the accessor isn't in the inherent opaque-accessor set of the
  // declaration, it's on-demand.
  if (!storage->requiresOpaqueAccessor(kind))
    return true;

  // Currently this only applies to imported declarations because we
  // eagerly create accessors for all other member storage.
  //
  // Note that we can't just use hasClangNode() because the importer
  // sometimes synthesizes things that lack clang nodes.
  auto *mod = storage->getDeclContext()->getModuleScopeContext();
  return (cast<FileUnit>(mod)->getKind() == FileUnitKind::ClangModule ||
          cast<FileUnit>(mod)->getKind() == FileUnitKind::DWARFModule);
}

/// Insert the specified decl into the DeclContext's member list.  If the hint
/// decl is specified, the new decl is inserted next to the hint.
static void addMemberToContextIfNeeded(Decl *D, DeclContext *DC,
                                       Decl *Hint = nullptr) {
  if (auto *ntd = dyn_cast<NominalTypeDecl>(DC)) {
    ntd->addMember(D, Hint);
  } else if (auto *ed = dyn_cast<ExtensionDecl>(DC)) {
    ed->addMember(D, Hint);
  } else {
    assert((DC->isLocalContext() || isa<FileUnit>(DC)) &&
           "Unknown declcontext");
  }
}

static ParamDecl *getParamDeclAtIndex(FuncDecl *fn, unsigned index) {
  return fn->getParameters()->get(index);
}

static VarDecl *getFirstParamDecl(FuncDecl *fn) {
  return getParamDeclAtIndex(fn, 0);
};


static ParamDecl *buildArgument(SourceLoc loc, DeclContext *DC,
                                StringRef name,
                                Type interfaceType,
                                VarDecl::Specifier specifier,
                                ASTContext &context) {
  auto *param = new (context) ParamDecl(specifier, SourceLoc(), SourceLoc(),
                                        Identifier(), loc,
                                        context.getIdentifier(name),
                                        DC);
  param->setImplicit();
  param->setInterfaceType(interfaceType);
  return param;
}

/// Build a parameter list which can forward the formal index parameters of a
/// declaration.
///
/// \param prefix optional arguments to be prefixed onto the index
///   forwarding pattern.
static ParameterList *
buildIndexForwardingParamList(AbstractStorageDecl *storage,
                              ArrayRef<ParamDecl*> prefix,
                              ASTContext &context) {
  auto subscript = dyn_cast<SubscriptDecl>(storage);

  // Fast path: if this isn't a subscript, just use whatever we have.
  if (!subscript)
    return ParameterList::create(context, prefix);

  // Clone the parameter list over for a new decl, so we get new ParamDecls.
  auto indices = subscript->getIndices()->clone(context,
                                                ParameterList::Implicit|
                                                ParameterList::WithoutTypes);

  // Give all of the parameters meaningless names so that we can forward
  // them properly.  If it's declared anonymously, SILGen will think
  // it's unused.
  // TODO: use some special DeclBaseName for this?
  for (auto param : indices->getArray()) {
    if (!param->hasName())
      param->setName(context.getIdentifier("anonymous"));
    assert(param->hasName());
  }

  if (prefix.empty())
    return indices;
  
  
  // Otherwise, we need to build up a new parameter list.
  SmallVector<ParamDecl*, 4> elements;

  // Start with the fields we were given, if there are any.
  elements.append(prefix.begin(), prefix.end());
  elements.append(indices->begin(), indices->end());
  return ParameterList::create(context, elements);
}

/// Create the generic parameters needed for the given accessor, if any.
static GenericParamList *createAccessorGenericParams(
                                              AbstractStorageDecl *storage) {
  // Accessors of generic subscripts get a copy of the subscript's
  // generic parameter list, because they're not nested inside the
  // subscript.
  if (auto *subscript = dyn_cast<SubscriptDecl>(storage)) {
    if (auto genericParams = subscript->getGenericParams())
      return genericParams->clone(subscript->getDeclContext());
  }

  return nullptr;
}

static AccessorDecl *createGetterPrototype(AbstractStorageDecl *storage,
                                           ASTContext &ctx) {
  assert(!storage->getGetter());

  SourceLoc loc = storage->getLoc();

  GenericEnvironment *genericEnvironmentOfLazyAccessor = nullptr;

  ParamDecl *selfDecl = nullptr;
  if (storage->getDeclContext()->isTypeContext()) {
    if (storage->getAttrs().hasAttribute<LazyAttr>()) {
      // For lazy properties, steal the 'self' from the initializer context.
      auto *varDecl = cast<VarDecl>(storage);
      auto *bindingDecl = varDecl->getParentPatternBinding();
      auto *bindingInit = cast<PatternBindingInitializer>(
        bindingDecl->getPatternEntryForVarDecl(varDecl).getInitContext());

      selfDecl = bindingInit->getImplicitSelfDecl();
      genericEnvironmentOfLazyAccessor =
        bindingInit->getGenericEnvironmentOfContext();
    }
  }

  GenericParamList *genericParams = createAccessorGenericParams(storage);

  // Add an index-forwarding clause.
  auto *getterParams = buildIndexForwardingParamList(storage, {}, ctx);

  SourceLoc staticLoc;
  if (storage->isStatic())
    staticLoc = storage->getLoc();

  auto storageInterfaceType = storage->getValueInterfaceType();

  auto getter = AccessorDecl::create(
      ctx, loc, /*AccessorKeywordLoc*/ loc,
      AccessorKind::Get, storage,
      staticLoc, StaticSpellingKind::None,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      genericParams,
      getterParams,
      TypeLoc::withoutLoc(storageInterfaceType),
      storage->getDeclContext());
  getter->setImplicit();

  // If we're stealing the 'self' from a lazy initializer, set it now.
  // Note that we don't re-parent the 'self' declaration to be part of
  // the getter until we synthesize the body of the getter later.
  if (selfDecl)
    *getter->getImplicitSelfDeclStorage() = selfDecl;

  // We need to install the generic environment here because:
  // 1) validating the getter will change the implicit self decl's DC to it,
  // 2) it's likely that the initializer will be type-checked before the
  //    accessor (and therefore before the normal installation happens), and
  // 3) type-checking a reference to the self decl will map its type into
  //    its context, which requires an environment to be installed on that
  //    context.
  // We can safely use the enclosing environment because properties are never
  // differently generic.
  if (genericEnvironmentOfLazyAccessor)
    getter->setGenericEnvironment(genericEnvironmentOfLazyAccessor);

  if (storage->isGetterMutating())
    getter->setSelfAccessKind(SelfAccessKind::Mutating);
  else
    getter->setSelfAccessKind(SelfAccessKind::NonMutating);

  if (storage->isStatic())
    getter->setStatic();

  if (!storage->requiresOpaqueAccessor(AccessorKind::Get))
    getter->setForcedStaticDispatch(true);

  // Always add the getter to the context immediately after the storage.
  addMemberToContextIfNeeded(getter, storage->getDeclContext(), storage);

  return getter;
}

static AccessorDecl *createSetterPrototype(AbstractStorageDecl *storage,
                                           ASTContext &ctx,
                                           AccessorDecl *getter = nullptr) {
  assert(!storage->getSetter());
  assert(storage->supportsMutation());

  SourceLoc loc = storage->getLoc();

  bool isStatic = storage->isStatic();
  bool isMutating = storage->isSetterMutating();

  GenericParamList *genericParams = createAccessorGenericParams(storage);

  // Add a "(value : T, indices...)" argument list.
  auto storageInterfaceType = storage->getValueInterfaceType();
  auto valueDecl = buildArgument(storage->getLoc(), storage->getDeclContext(),
                                 "value", storageInterfaceType,
                                 VarDecl::Specifier::Default, ctx);
  auto *params = buildIndexForwardingParamList(storage, valueDecl, ctx);

  Type setterRetTy = TupleType::getEmpty(ctx);
  auto setter = AccessorDecl::create(
      ctx, loc, /*AccessorKeywordLoc*/ SourceLoc(),
      AccessorKind::Set, storage,
      /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      genericParams, params,
      TypeLoc::withoutLoc(setterRetTy),
      storage->getDeclContext());
  setter->setImplicit();

  if (isMutating)
    setter->setSelfAccessKind(SelfAccessKind::Mutating);
  else
    setter->setSelfAccessKind(SelfAccessKind::NonMutating);

  if (isStatic)
    setter->setStatic();

  // All mutable storage requires a setter.
  assert(storage->requiresOpaqueAccessor(AccessorKind::Set));

  // Always add the setter to the context immediately after the getter.
  if (!getter) getter = storage->getGetter();
  if (!getter) getter = storage->getReadCoroutine();
  assert(getter && "always synthesize setter prototype after get/read");
  addMemberToContextIfNeeded(setter, storage->getDeclContext(), getter);

  return setter;
}

/// Mark the accessor as transparent if we can.
///
/// If the storage is inside a fixed-layout nominal type, we can mark the
/// accessor as transparent, since in this case we just want it for abstraction
/// purposes (i.e., to make access to the variable uniform and to be able to
/// put the getter in a vtable).
///
/// If the storage is for a global stored property or a stored property of a
/// resilient type, we are synthesizing accessors to present a resilient
/// interface to the storage and they should not be transparent.
static void maybeMarkTransparent(AccessorDecl *accessor, ASTContext &ctx) {
  auto *DC = accessor->getDeclContext();
  auto *nominalDecl = DC->getSelfNominalTypeDecl();

  // Global variable accessors are not @_transparent.
  if (!nominalDecl)
    return;

  auto *storage = accessor->getStorage();

  // Accessors for resilient properties are not @_transparent.
  if (storage->isResilient())
    return;

  // Accessors for protocol storage requirements are never @_transparent
  // since they do not have bodies.
  //
  // FIXME: Revisit this if we ever get 'real' default implementations.
  if (isa<ProtocolDecl>(nominalDecl))
    return;

  // Accessors for classes with @objc ancestry are not @_transparent,
  // since they use a field offset variable which is not exported.
  if (auto *classDecl = dyn_cast<ClassDecl>(nominalDecl))
    if (classDecl->checkAncestry(AncestryFlags::ObjC))
      return;

  // Accessors synthesized on-demand are never transaprent.
  if (accessor->hasForcedStaticDispatch())
    return;

  if (accessor->getAccessorKind() == AccessorKind::Get ||
      accessor->getAccessorKind() == AccessorKind::Set) {
    // Getters and setters for lazy properties are not @_transparent.
    if (storage->getAttrs().hasAttribute<LazyAttr>())
      return;

    // Getters/setters for a property with a wrapper are not @_transparent if
    // the backing variable has more-restrictive access than the original
    // property. The same goes for its storage wrapper.
    if (auto var = dyn_cast<VarDecl>(storage)) {
      if (auto backingVar = var->getPropertyWrapperBackingProperty()) {
        if (backingVar->getFormalAccess() < var->getFormalAccess())
          return;
      }

      if (auto original = var->getOriginalWrappedProperty(
              PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
        if (var->getFormalAccess() < original->getFormalAccess())
          return;
      }
    }
  }

  switch (accessor->getAccessorKind()) {
  case AccessorKind::Get:
    break;

  case AccessorKind::Set:

    switch (storage->getWriteImpl()) {
    case WriteImplKind::Set:
      // Setters for property wrapper are OK, unless there are observers.
      // FIXME: This should be folded into the WriteImplKind below.
      if (auto var = dyn_cast<VarDecl>(storage)) {
        if (var->hasAttachedPropertyWrapper()) {
          if (var->getAccessor(AccessorKind::DidSet) ||
              var->getAccessor(AccessorKind::WillSet))
            return;

          break;
        } else if (var->getOriginalWrappedProperty(
                     PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
          break;
        }
      }

      // Anything else should not have a synthesized setter.
      LLVM_FALLTHROUGH;
    case WriteImplKind::Immutable:
      llvm_unreachable("should not be synthesizing accessor in this case");

    case WriteImplKind::StoredWithObservers:
    case WriteImplKind::InheritedWithObservers:
      // Setters for observed properties are not @_transparent (because the
      // observers are private) and cannot be referenced from a transparent
      // method).
      return;

    case WriteImplKind::Stored:
    case WriteImplKind::MutableAddress:
    case WriteImplKind::Modify:
      break;
    }
    break;

  case AccessorKind::Read:
  case AccessorKind::Modify:
    break;

  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::Address:
  case AccessorKind::MutableAddress:
    llvm_unreachable("bad synthesized function kind");
  }

  accessor->getAttrs().add(new (ctx) TransparentAttr(IsImplicit));
}

static AccessorDecl *
createCoroutineAccessorPrototype(AbstractStorageDecl *storage,
                                 AccessorKind kind,
                                ASTContext &ctx) {
  assert(kind == AccessorKind::Read || kind == AccessorKind::Modify);

  SourceLoc loc = storage->getLoc();

  bool isStatic = storage->isStatic();
  bool isMutating = storage->isGetterMutating();
  if (kind == AccessorKind::Modify)
    isMutating |= storage->isSetterMutating();

  auto dc = storage->getDeclContext();

  // The forwarding index parameters.
  auto *params = buildIndexForwardingParamList(storage, {}, ctx);

  // Coroutine accessors always return ().
  Type retTy = TupleType::getEmpty(ctx);

  GenericParamList *genericParams = createAccessorGenericParams(storage);

  auto *accessor = AccessorDecl::create(
      ctx, loc, /*AccessorKeywordLoc=*/SourceLoc(),
      kind, storage,
      /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      genericParams, params, TypeLoc::withoutLoc(retTy), dc);
  accessor->setImplicit();
  
  if (isMutating)
    accessor->setSelfAccessKind(SelfAccessKind::Mutating);
  else
    accessor->setSelfAccessKind(SelfAccessKind::NonMutating);

  if (isStatic)
    accessor->setStatic();

  // If the storage does not provide this accessor as an opaque accessor,
  // we can't add a dynamically-dispatched method entry for the accessor,
  // so force it to be statically dispatched. ("final" would be inappropriate
  // because the property can still be overridden.)
  if (!storage->requiresOpaqueAccessor(kind))
    accessor->setForcedStaticDispatch(true);

  // Make sure the coroutine is available enough to access
  // the storage (and its getters/setters if it has them).
  SmallVector<const Decl *, 2> asAvailableAs;
  asAvailableAs.push_back(storage);
  if (FuncDecl *getter = storage->getGetter()) {
    asAvailableAs.push_back(getter);
  }
  if (kind == AccessorKind::Modify) {
    if (FuncDecl *setter = storage->getSetter()) {
      asAvailableAs.push_back(setter);
    }
  }

  AvailabilityInference::applyInferredAvailableAttrs(accessor,
                                                     asAvailableAs, ctx);

  Decl *afterDecl;
  if (kind == AccessorKind::Read) {
    // Add the synthesized read coroutine after the getter, if one exists,
    // or else immediately after the storage.
    afterDecl = storage->getGetter();
    if (!afterDecl) afterDecl = storage;
  } else {
    // Add the synthesized modify coroutine after the setter.
    afterDecl = storage->getSetter();
  }

  addMemberToContextIfNeeded(accessor, dc, afterDecl);

  return accessor;
}

static AccessorDecl *
createReadCoroutinePrototype(AbstractStorageDecl *storage,
                             ASTContext &ctx) {
  return createCoroutineAccessorPrototype(storage, AccessorKind::Read, ctx);
}

static AccessorDecl *
createModifyCoroutinePrototype(AbstractStorageDecl *storage,
                               ASTContext &ctx) {
  return createCoroutineAccessorPrototype(storage, AccessorKind::Modify, ctx);
}

/// Build an expression that evaluates the specified parameter list as a tuple
/// or paren expr, suitable for use in an apply expr.
static Expr *buildArgumentForwardingExpr(ArrayRef<ParamDecl*> params,
                                         ASTContext &ctx) {
  SmallVector<Identifier, 4> labels;
  SmallVector<SourceLoc, 4> labelLocs;
  SmallVector<Expr *, 4> args;
  SmallVector<AnyFunctionType::Param, 4> elts;

  for (auto param : params) {
    auto type = param->getType();
    elts.push_back(param->toFunctionParam(type));

    Expr *ref = new (ctx) DeclRefExpr(param, DeclNameLoc(), /*implicit*/ true);
    ref->setType(param->isInOut() ? LValueType::get(type) : type);

    if (param->isInOut()) {
      ref = new (ctx) InOutExpr(SourceLoc(), ref, type, /*isImplicit=*/true);
    } else if (param->isVariadic()) {
      ref = new (ctx) VarargExpansionExpr(ref, /*implicit*/ true);
      ref->setType(type);
    }

    args.push_back(ref);
    
    labels.push_back(param->getArgumentName());
    labelLocs.push_back(SourceLoc());
  }

  Expr *argExpr;
  if (args.size() == 1 &&
      labels[0].empty() &&
      !isa<VarargExpansionExpr>(args[0])) {
    argExpr = new (ctx) ParenExpr(SourceLoc(), args[0], SourceLoc(),
                                  /*hasTrailingClosure=*/false);
    argExpr->setImplicit();
  } else {
    argExpr = TupleExpr::create(ctx, SourceLoc(), args, labels, labelLocs,
                                SourceLoc(), false, IsImplicit);
  }

  auto argTy = AnyFunctionType::composeInput(ctx, elts, /*canonical*/false);
  argExpr->setType(argTy);

  return argExpr;
}


/// Build a reference to the subscript index variables for this subscript
/// accessor.
static Expr *buildSubscriptIndexReference(ASTContext &ctx,
                                          AccessorDecl *accessor) {
  // Pull out the body parameters, which we should have cloned
  // previously to be forwardable.  Drop the initial buffer/value
  // parameter in accessors that have one.
  auto params = accessor->getParameters()->getArray();
  auto accessorKind = accessor->getAccessorKind();

  // Ignore the value parameter of a setter.
  if (accessorKind == AccessorKind::Set) {
    params = params.slice(1);
  }

  // Okay, everything else should be forwarded, build the expression.
  return buildArgumentForwardingExpr(params, ctx);
}

enum class SelfAccessorKind {
  /// We're building a derived accessor on top of whatever this
  /// class provides.
  Peer,

  /// We're building a setter or something around an underlying
  /// implementation, which might be storage or inherited from a
  /// superclass.
  Super,
};

static Expr *buildSelfReference(VarDecl *selfDecl,
                                SelfAccessorKind selfAccessorKind,
                                bool isLValue,
                                ASTContext &ctx) {
  switch (selfAccessorKind) {
  case SelfAccessorKind::Peer:
    return new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(), IsImplicit,
                                 AccessSemantics::Ordinary,
                                 isLValue
                                  ? LValueType::get(selfDecl->getType())
                                  : selfDecl->getType());

  case SelfAccessorKind::Super:
    assert(!isLValue);
    return new (ctx) SuperRefExpr(selfDecl, SourceLoc(), IsImplicit,
                                  selfDecl->getType()->getSuperclass());
  }
  llvm_unreachable("bad self access kind");
}

namespace {
  enum class TargetImpl {
    /// We're doing an ordinary storage reference.
    Ordinary,
    /// We're referencing the physical storage created for the storage.
    Storage,
    /// We're referencing this specific implementation of the storage, not
    /// an override of it.
    Implementation,
    /// We're referencing the superclass's implementation of the storage.
    Super,
    /// We're referencing the backing property for a property with a wrapper
    /// through the 'value' property.
    Wrapper,
    /// We're referencing the backing property for a property with a wrapper
    /// through the 'projectedValue' property.
    WrapperStorage,
  };
} // end anonymous namespace

namespace  {
  /// Describes the information needed to perform property wrapper access via
  /// the enclosing self.
  struct EnclosingSelfPropertyWrapperAccess {
    /// The (genreric) subscript that will be used to perform the access.
    SubscriptDecl *subscript;

    /// The property being accessed.
    VarDecl *accessedProperty;
  };
}

/// Determine whether the given property should be accessed via the enclosing-self access pattern.
static Optional<EnclosingSelfPropertyWrapperAccess>
getEnclosingSelfPropertyWrapperAccess(VarDecl *property, bool forProjected) {
  // The enclosing-self pattern only applies to instance properties of
  // classes.
  if (!property->isInstanceMember())
    return None;
  auto classDecl = property->getDeclContext()->getSelfClassDecl();
  if (!classDecl)
    return None;

  // The pattern currently only works with the outermost property wrapper.
  Type outermostWrapperType = property->getPropertyWrapperBackingPropertyType();
  if (!outermostWrapperType)
    return None;
  NominalTypeDecl *wrapperTypeDecl = outermostWrapperType->getAnyNominal();
  if (!wrapperTypeDecl)
    return None;

  // Look for a generic subscript that fits the general form we need.
  auto wrapperInfo = wrapperTypeDecl->getPropertyWrapperTypeInfo();
  auto subscript =
      forProjected ? wrapperInfo.enclosingInstanceProjectedSubscript
                   : wrapperInfo.enclosingInstanceWrappedSubscript;
  if (!subscript)
    return None;

  EnclosingSelfPropertyWrapperAccess result;
  result.subscript = subscript;

  if (forProjected) {
    result.accessedProperty =
        property->getPropertyWrapperBackingPropertyInfo().storageWrapperVar;
  } else {
    result.accessedProperty = property;
  }
  return result;
}

/// Build an l-value for the storage of a declaration.
static Expr *buildStorageReference(AccessorDecl *accessor,
                                   AbstractStorageDecl *storage,
                                   TargetImpl target,
                                   bool isLValue,
                                   ASTContext &ctx) {
  // Local function to "finish" the expression, creating a member reference
  // to the given sequence of underlying variables.
  Optional<EnclosingSelfPropertyWrapperAccess> enclosingSelfAccess;
  llvm::TinyPtrVector<VarDecl *> underlyingVars;
  auto finish = [&](Expr *result) -> Expr * {
    for (auto underlyingVar : underlyingVars) {
      auto subs = result->getType()
          ->getWithoutSpecifierType()
          ->getContextSubstitutionMap(
            accessor->getParentModule(),
            underlyingVar->getDeclContext());

      ConcreteDeclRef memberRef(underlyingVar, subs);
      auto *memberRefExpr = new (ctx) MemberRefExpr(
          result, SourceLoc(), memberRef, DeclNameLoc(), /*Implicit=*/true);
      auto type = underlyingVar->getValueInterfaceType()
          .subst(subs, SubstFlags::UseErrorType);
      if (isLValue)
        type = LValueType::get(type);
      memberRefExpr->setType(type);
      
      result = memberRefExpr;
    }
    
    return result;
  };

  VarDecl *selfDecl = accessor->getImplicitSelfDecl();

  AccessSemantics semantics;
  SelfAccessorKind selfAccessKind;
  Type selfTypeForAccess = (selfDecl ? selfDecl->getType() : Type());

  auto *genericEnv = accessor->getGenericEnvironment();
  SubstitutionMap subs;
  if (genericEnv)
    subs = genericEnv->getForwardingSubstitutionMap();

  switch (target) {
  case TargetImpl::Ordinary:
    semantics = AccessSemantics::Ordinary;
    selfAccessKind = SelfAccessorKind::Peer;
    break;

  case TargetImpl::Storage:
    semantics = AccessSemantics::DirectToStorage;
    selfAccessKind = SelfAccessorKind::Peer;
    break;

  case TargetImpl::Implementation:
    semantics = AccessSemantics::DirectToImplementation;
    selfAccessKind = SelfAccessorKind::Peer;
    break;

  case TargetImpl::Super:
    // If this really is an override, use a super-access.
    if (auto override = storage->getOverriddenDecl()) {
      semantics = AccessSemantics::Ordinary;
      selfAccessKind = SelfAccessorKind::Super;

      auto *baseClass = override->getDeclContext()->getSelfClassDecl();
      selfTypeForAccess = selfTypeForAccess->getSuperclassForDecl(baseClass);
      subs =
        selfTypeForAccess->getContextSubstitutionMap(
          accessor->getParentModule(),
          baseClass);

      storage = override;

    // Otherwise do a self-reference, which is dynamically bogus but
    // should be statically valid.  This should only happen in invalid cases.    
    } else {
      assert(storage->isInvalid());
      semantics = AccessSemantics::Ordinary;
      selfAccessKind = SelfAccessorKind::Peer;
    }
    break;

  case TargetImpl::Wrapper: {
    auto var = cast<VarDecl>(accessor->getStorage());
    storage = var->getPropertyWrapperBackingProperty();

    // If the outermost property wrapper uses the enclosing self pattern,
    // record that.
    unsigned lastWrapperIdx = var->getAttachedPropertyWrappers().size();
    unsigned firstWrapperIdx = 0;
    enclosingSelfAccess =
        getEnclosingSelfPropertyWrapperAccess(var, /*forProjected=*/false);
    if (enclosingSelfAccess)
      firstWrapperIdx = 1;

    // Perform accesses to the wrappedValues along the composition chain.
    for (unsigned i : range(firstWrapperIdx, lastWrapperIdx)) {
      auto wrapperInfo = var->getAttachedPropertyWrapperTypeInfo(i);
      underlyingVars.push_back(wrapperInfo.valueVar);
    }
    semantics = AccessSemantics::DirectToStorage;
    selfAccessKind = SelfAccessorKind::Peer;
    break;
  }

  case TargetImpl::WrapperStorage: {
    auto var =
        cast<VarDecl>(accessor->getStorage())->getOriginalWrappedProperty();
    storage = var->getPropertyWrapperBackingProperty();
    enclosingSelfAccess =
        getEnclosingSelfPropertyWrapperAccess(var, /*forProjected=*/true);
    if (!enclosingSelfAccess) {
      underlyingVars.push_back(
        var->getAttachedPropertyWrapperTypeInfo(0).projectedValueVar);
    }
    semantics = AccessSemantics::DirectToStorage;
    selfAccessKind = SelfAccessorKind::Peer;
    break;
  }
  }

  if (!selfDecl) {
    assert(target != TargetImpl::Super);
    auto *storageDRE = new (ctx) DeclRefExpr(storage, DeclNameLoc(),
                                             IsImplicit, semantics);
    auto type = storage->getValueInterfaceType()
        .subst(subs, SubstFlags::UseErrorType);
    if (isLValue)
      type = LValueType::get(type);
    storageDRE->setType(type);

    return finish(storageDRE);
  }

  bool isMemberLValue = isLValue;

  // If we're acessing a property wrapper, determine if the
  // intermediate access requires an lvalue.
  if (underlyingVars.size() > 0) {
    isMemberLValue = underlyingVars[0]->isGetterMutating();
    if (isLValue)
      isMemberLValue |= underlyingVars[0]->isSetterMutating();
  }

  bool isSelfLValue = storage->isGetterMutating();
  if (isMemberLValue)
    isSelfLValue |= storage->isSetterMutating();

  Expr *selfDRE =
    buildSelfReference(selfDecl, selfAccessKind, isSelfLValue,
                       ctx);
  if (isSelfLValue)
    selfTypeForAccess = LValueType::get(selfTypeForAccess);

  if (!selfDRE->getType()->isEqual(selfTypeForAccess)) {
    assert(selfAccessKind == SelfAccessorKind::Super);
    selfDRE = new (ctx) DerivedToBaseExpr(selfDRE, selfTypeForAccess);
  }

  Expr *lookupExpr;
  ConcreteDeclRef memberRef(storage, subs);
  auto type = storage->getValueInterfaceType()
      .subst(subs, SubstFlags::UseErrorType);
  if (isMemberLValue)
    type = LValueType::get(type);

  // When we are performing access via a property wrapper's static subscript
  // that accepts the enclosing self along with key paths, form that subscript
  // operation now.
  if (enclosingSelfAccess) {
    Type storageType = storage->getValueInterfaceType()
        .subst(subs, SubstFlags::UseErrorType);
    // Metatype instance for the wrapper type itself.
    TypeExpr *wrapperMetatype = TypeExpr::createImplicit(storageType, ctx);

    // Key path referring to the property being accessed.
    Expr *propertyKeyPath = new (ctx) KeyPathDotExpr(SourceLoc());
    propertyKeyPath = new (ctx) UnresolvedDotExpr(
        propertyKeyPath, SourceLoc(),
        enclosingSelfAccess->accessedProperty->getFullName(), DeclNameLoc(),
        /*Implicit=*/true);
    propertyKeyPath = new (ctx) KeyPathExpr(
        SourceLoc(), nullptr, propertyKeyPath);

    // Key path referring to the backing storage property.
    Expr *storageKeyPath = new (ctx) KeyPathDotExpr(SourceLoc());
    storageKeyPath = new (ctx) UnresolvedDotExpr(
        storageKeyPath, SourceLoc(), storage->getFullName(), DeclNameLoc(),
        /*Implicit=*/true);
    storageKeyPath = new (ctx) KeyPathExpr(
        SourceLoc(), nullptr, storageKeyPath);
    Expr *args[3] = {
      selfDRE,
      propertyKeyPath,
      storageKeyPath
    };

    SubscriptDecl *subscriptDecl = enclosingSelfAccess->subscript;
    auto &tc = static_cast<TypeChecker&>(*ctx.getLazyResolver());
    lookupExpr = SubscriptExpr::create(
        ctx, wrapperMetatype, SourceLoc(), args,
        subscriptDecl->getFullName().getArgumentNames(), { }, SourceLoc(),
        nullptr, subscriptDecl, /*Implicit=*/true);
    tc.typeCheckExpression(lookupExpr, accessor);

    // Make sure we produce an lvalue only when desired.
    if (isMemberLValue != lookupExpr->getType()->is<LValueType>()) {
      if (isMemberLValue) {
        // Strip off an extraneous load.
        if (auto load = dyn_cast<LoadExpr>(lookupExpr))
          lookupExpr = load->getSubExpr();
      } else {
        lookupExpr = new (ctx) LoadExpr(
            lookupExpr, lookupExpr->getType()->getRValueType());
      }
    }
  } else if (auto subscript = dyn_cast<SubscriptDecl>(storage)) {
    Expr *indices = buildSubscriptIndexReference(ctx, accessor);
    lookupExpr = SubscriptExpr::create(ctx, selfDRE, indices, memberRef,
                                       IsImplicit, semantics);

    if (selfAccessKind == SelfAccessorKind::Super)
      cast<LookupExpr>(lookupExpr)->setIsSuper(true);

    lookupExpr->setType(type);

  } else {
    lookupExpr = new (ctx) MemberRefExpr(selfDRE, SourceLoc(), memberRef,
                                         DeclNameLoc(), IsImplicit, semantics);

    if (selfAccessKind == SelfAccessorKind::Super)
      cast<LookupExpr>(lookupExpr)->setIsSuper(true);

    lookupExpr->setType(type);
  }

  return finish(lookupExpr);
}

/// Load the value of VD.  If VD is an @override of another value, we call the
/// superclass getter.  Otherwise, we do a direct load of the value.
static Expr *
createPropertyLoadOrCallSuperclassGetter(AccessorDecl *accessor,
                                         AbstractStorageDecl *storage,
                                         TargetImpl target,
                                         ASTContext &ctx) {
  return buildStorageReference(accessor, storage, target, /*isLValue=*/false,
                               ctx);
}

/// Look up the NSCopying protocol from the Foundation module, if present.
/// Otherwise return null.
static ProtocolDecl *getNSCopyingProtocol(ASTContext &ctx,
                                          DeclContext *DC) {
  auto foundation = ctx.getLoadedModule(ctx.Id_Foundation);
  if (!foundation)
    return nullptr;

  SmallVector<ValueDecl *, 2> results;
  DC->lookupQualified(foundation,
                      ctx.getSwiftId(KnownFoundationEntity::NSCopying),
                      NL_QualifiedDefault | NL_KnownNonCascadingDependency,
                      results);

  if (results.size() != 1)
    return nullptr;

  return dyn_cast<ProtocolDecl>(results.front());
}

static Optional<ProtocolConformanceRef>
checkConformanceToNSCopying(ASTContext &ctx, VarDecl *var, Type type) {
  auto dc = var->getDeclContext();
  auto proto = getNSCopyingProtocol(ctx, dc);

  if (proto) {
    auto result = TypeChecker::conformsToProtocol(type, proto, dc, None);
    if (result)
      return result;
  }

  ctx.Diags.diagnose(var->getLoc(), diag::nscopying_doesnt_conform);
  return None;
}

static std::pair<Type, bool> getUnderlyingTypeOfVariable(VarDecl *var) {
  Type type = var->getType()->getReferenceStorageReferent();

  if (Type objectType = type->getOptionalObjectType()) {
    return {objectType, true};
  } else {
    return {type, false};
  }
}

Optional<ProtocolConformanceRef>
TypeChecker::checkConformanceToNSCopying(VarDecl *var) {
  Type type = getUnderlyingTypeOfVariable(var).first;
  return ::checkConformanceToNSCopying(Context, var, type);
}

/// Synthesize the code to store 'Val' to 'VD', given that VD has an @NSCopying
/// attribute on it.  We know that VD is a stored property in a class, so we
/// just need to generate something like "self.property = val.copy(zone: nil)"
/// here.  This does some type checking to validate that the call will succeed.
static Expr *synthesizeCopyWithZoneCall(Expr *Val, VarDecl *VD,
                                        ASTContext &Ctx) {
  // We support @NSCopying on class types (which conform to NSCopying),
  // protocols which conform, and option types thereof.
  auto underlyingTypeAndIsOptional = getUnderlyingTypeOfVariable(VD);
  auto underlyingType = underlyingTypeAndIsOptional.first;
  auto isOptional = underlyingTypeAndIsOptional.second;

  // The element type must conform to NSCopying.  If not, emit an error and just
  // recovery by synthesizing without the copy call.
  auto conformance = checkConformanceToNSCopying(Ctx, VD, underlyingType);
  if (!conformance)
    return Val;

  //- (id)copyWithZone:(NSZone *)zone;
  DeclName copyWithZoneName(Ctx, Ctx.getIdentifier("copy"), { Ctx.Id_with });
  FuncDecl *copyMethod = nullptr;
  for (auto member : conformance->getRequirement()->getMembers()) {
    if (auto func = dyn_cast<FuncDecl>(member)) {
      if (func->getFullName() == copyWithZoneName) {
        copyMethod = func;
        break;
      }
    }
  }
  assert(copyMethod != nullptr);

  // If we have an optional type, we have to "?" the incoming value to only
  // evaluate the subexpression if the incoming value is non-null.
  if (isOptional) {
    Val = new (Ctx) BindOptionalExpr(Val, SourceLoc(), 0);
    Val->setType(underlyingType);
  }

  SubstitutionMap subs =
    SubstitutionMap::get(copyMethod->getGenericSignature(),
                         {underlyingType},
                         ArrayRef<ProtocolConformanceRef>(*conformance));
  ConcreteDeclRef copyMethodRef(copyMethod, subs);
  auto copyMethodType = copyMethod->getInterfaceType()
                           ->castTo<GenericFunctionType>()
                           ->substGenericArgs(subs);
  auto DRE = new (Ctx) DeclRefExpr(copyMethodRef, DeclNameLoc(), IsImplicit);
  DRE->setType(copyMethodType);

  // Drop the self type
  copyMethodType = copyMethodType->getResult()->castTo<FunctionType>();

  auto DSCE = new (Ctx) DotSyntaxCallExpr(DRE, SourceLoc(), Val);
  DSCE->setImplicit();
  DSCE->setType(copyMethodType);
  DSCE->setThrows(false);

  Expr *Nil = new (Ctx) NilLiteralExpr(SourceLoc(), /*implicit*/true);
  Nil->setType(copyMethodType->getParams()[0].getParameterType());

  auto *Call = CallExpr::createImplicit(Ctx, DSCE, { Nil }, { Ctx.Id_with });
  Call->setType(copyMethodType->getResult());
  Call->setThrows(false);

  TypeLoc ResultTy;
  ResultTy.setType(VD->getType());

  // If we're working with non-optional types, we're forcing the cast.
  if (!isOptional) {
    auto *Cast =
      new (Ctx) ForcedCheckedCastExpr(Call, SourceLoc(), SourceLoc(),
                                      TypeLoc::withoutLoc(underlyingType));
    Cast->setCastKind(CheckedCastKind::ValueCast);
    Cast->setType(underlyingType);
    Cast->setImplicit();

    return Cast;
  }

  // We're working with optional types, so perform a conditional checked
  // downcast.
  auto *Cast =
    new (Ctx) ConditionalCheckedCastExpr(Call, SourceLoc(), SourceLoc(),
                                         TypeLoc::withoutLoc(underlyingType));
  Cast->setCastKind(CheckedCastKind::ValueCast);
  Cast->setType(OptionalType::get(underlyingType));
  Cast->setImplicit();

  // Use OptionalEvaluationExpr to evaluate the "?".
  auto *Result = new (Ctx) OptionalEvaluationExpr(Cast);
  Result->setType(OptionalType::get(underlyingType));

  return Result;
}

/// In a synthesized accessor body, store 'value' to the appropriate element.
///
/// If the property is an override, we call the superclass setter.
/// Otherwise, we do a direct store of the value.
static
void createPropertyStoreOrCallSuperclassSetter(AccessorDecl *accessor,
                                               Expr *value,
                                               AbstractStorageDecl *storage,
                                               TargetImpl target,
                                               SmallVectorImpl<ASTNode> &body,
                                               ASTContext &ctx) {
  // If the storage is an @NSCopying property, then we store the
  // result of a copyWithZone call on the value, not the value itself.
  if (auto property = dyn_cast<VarDecl>(storage)) {
    if (property->getAttrs().hasAttribute<NSCopyingAttr>())
      value = synthesizeCopyWithZoneCall(value, property, ctx);
  }

  Expr *dest = buildStorageReference(accessor, storage, target,
                                     /*isLValue=*/true, ctx);

  // A lazy property setter will store a value of type T into underlying storage
  // of type T?.
  auto destType = dest->getType()->getWithoutSpecifierType();
  if (!destType->isEqual(value->getType())) {
    assert(destType->getOptionalObjectType()->isEqual(value->getType()));
    value = new (ctx) InjectIntoOptionalExpr(value, destType);
  }

  auto *assign = new (ctx) AssignExpr(dest, SourceLoc(), value,
                                      IsImplicit);
  assign->setType(ctx.TheEmptyTupleType);

  body.push_back(assign);
}

LLVM_ATTRIBUTE_UNUSED
static bool isSynthesizedComputedProperty(AbstractStorageDecl *storage) {
  return (storage->getAttrs().hasAttribute<LazyAttr>() ||
          storage->getAttrs().hasAttribute<NSManagedAttr>() ||
          (isa<VarDecl>(storage) &&
           cast<VarDecl>(storage)->hasAttachedPropertyWrapper()));
}

/// Synthesize the body of a trivial getter.  For a non-member vardecl or one
/// which is not an override of a base class property, it performs a direct
/// storage load.  For an override of a base member property, it chains up to
/// super.
static std::pair<BraceStmt *, bool>
synthesizeTrivialGetterBody(AccessorDecl *getter, TargetImpl target,
                            ASTContext &ctx) {
  auto storage = getter->getStorage();
  assert(!isSynthesizedComputedProperty(storage) ||
         target == TargetImpl::Wrapper ||
         target == TargetImpl::WrapperStorage);

  SourceLoc loc = storage->getLoc();

  Expr *result =
    createPropertyLoadOrCallSuperclassGetter(getter, storage, target, ctx);
  ASTNode returnStmt = new (ctx) ReturnStmt(SourceLoc(), result, IsImplicit);

  return { BraceStmt::create(ctx, loc, returnStmt, loc, true),
           /*isTypeChecked=*/true };
}

/// Synthesize the body of a getter which just directly accesses the
/// underlying storage.
static std::pair<BraceStmt *, bool>
synthesizeTrivialGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  assert(getter->getStorage()->hasStorage());
  return synthesizeTrivialGetterBody(getter, TargetImpl::Storage, ctx);
}

/// Synthesize the body of a getter which just delegates to its superclass
/// implementation.
static std::pair<BraceStmt *, bool>
synthesizeInheritedGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  // This should call the superclass getter.
  return synthesizeTrivialGetterBody(getter, TargetImpl::Super, ctx);
}

/// Synthesize the body of a getter which just delegates to an addressor.
static std::pair<BraceStmt *, bool>
synthesizeAddressedGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  assert(getter->getStorage()->getAddressor());

  // This should call the addressor.
  return synthesizeTrivialGetterBody(getter, TargetImpl::Implementation, ctx);
}

/// Synthesize the body of a getter which just delegates to a read
/// coroutine accessor.
static std::pair<BraceStmt *, bool>
synthesizeReadCoroutineGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  assert(getter->getStorage()->getReadCoroutine());

  // This should call the read coroutine.
  return synthesizeTrivialGetterBody(getter, TargetImpl::Implementation, ctx);
}

/// Synthesize the body of a getter for a property wrapper, which
/// delegates to the wrapper's "value" property.
static std::pair<BraceStmt *, bool>
synthesizePropertyWrapperGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  return synthesizeTrivialGetterBody(getter, TargetImpl::Wrapper, ctx);
}

/// Synthesize the body of a setter which just stores to the given storage
/// declaration (which doesn't have to be the storage for the setter).
static std::pair<BraceStmt *, bool>
synthesizeTrivialSetterBodyWithStorage(AccessorDecl *setter,
                                       TargetImpl target,
                                       AbstractStorageDecl *storageToUse,
                                       ASTContext &ctx) {
  SourceLoc loc = setter->getStorage()->getLoc();

  VarDecl *valueParamDecl = getFirstParamDecl(setter);

  auto *valueDRE =
    new (ctx) DeclRefExpr(valueParamDecl, DeclNameLoc(), IsImplicit);
  valueDRE->setType(valueParamDecl->getType());

  SmallVector<ASTNode, 1> setterBody;

  createPropertyStoreOrCallSuperclassSetter(setter, valueDRE, storageToUse,
                                            target, setterBody, ctx);
  return { BraceStmt::create(ctx, loc, setterBody, loc, true),
           /*isTypeChecked=*/true };
}

static std::pair<BraceStmt *, bool>
synthesizeTrivialSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  auto storage = setter->getStorage();
  assert(!isSynthesizedComputedProperty(storage));

  return synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Storage,
                                                storage, ctx);
}

/// Synthesize the body of a setter for a property wrapper, which
/// delegates to the wrapper's "value" property.
static std::pair<BraceStmt *, bool>
synthesizePropertyWrapperSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  return synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Wrapper,
                                                setter->getStorage(), ctx);
}

static Expr *maybeWrapInOutExpr(Expr *expr, ASTContext &ctx) {
  if (auto lvalueType = expr->getType()->getAs<LValueType>()) {
    auto type = lvalueType->getObjectType();
    return new (ctx) InOutExpr(SourceLoc(), expr, type, true);
  }

  return expr;
}

static std::pair<BraceStmt *, bool>
synthesizeCoroutineAccessorBody(AccessorDecl *accessor, ASTContext &ctx) {
  assert(accessor->isCoroutine());

  auto storage = accessor->getStorage();
  auto target = (accessor->hasForcedStaticDispatch()
                   ? TargetImpl::Ordinary
                   : TargetImpl::Implementation);

  SourceLoc loc = storage->getLoc();
  SmallVector<ASTNode, 1> body;

  bool isLValue = accessor->getAccessorKind() == AccessorKind::Modify;

  // Build a reference to the storage.
  Expr *ref = buildStorageReference(accessor, storage, target, isLValue, ctx);

  // Wrap it with an `&` marker if this is a modify.
  ref = maybeWrapInOutExpr(ref, ctx);

  // Yield it.
  YieldStmt *yield = YieldStmt::create(ctx, loc, loc, ref, loc, true);
  body.push_back(yield);

  return { BraceStmt::create(ctx, loc, body, loc, true),
           /*isTypeChecked=*/true };
}

/// Synthesize the body of a read coroutine.
static std::pair<BraceStmt *, bool>
synthesizeReadCoroutineBody(AccessorDecl *read, ASTContext &ctx) {
  assert(read->getStorage()->getReadImpl() != ReadImplKind::Read);
  return synthesizeCoroutineAccessorBody(read, ctx);
}

/// Synthesize the body of a modify coroutine.
static std::pair<BraceStmt *, bool>
synthesizeModifyCoroutineBody(AccessorDecl *modify, ASTContext &ctx) {
#ifndef NDEBUG
  auto impl = modify->getStorage()->getReadWriteImpl();
  assert(impl != ReadWriteImplKind::Modify &&
         impl != ReadWriteImplKind::Immutable);
#endif
  return synthesizeCoroutineAccessorBody(modify, ctx);
}

static void addGetterToStorage(AbstractStorageDecl *storage,
                               ASTContext &ctx) {
  auto getter = createGetterPrototype(storage, ctx);

  // Install the prototype.
  storage->setSynthesizedGetter(getter);
}

static void addSetterToStorage(AbstractStorageDecl *storage,
                               ASTContext &ctx) {
  auto setter = createSetterPrototype(storage, ctx);

  // Install the prototype.
  storage->setSynthesizedSetter(setter);
}

static void addReadCoroutineToStorage(AbstractStorageDecl *storage,
                                      ASTContext &ctx) {
  auto read = createReadCoroutinePrototype(storage, ctx);

  // Install the prototype.
  storage->setSynthesizedReadCoroutine(read);
}

static void addModifyCoroutineToStorage(AbstractStorageDecl *storage,
                                        ASTContext &ctx) {
  auto modify = createModifyCoroutinePrototype(storage, ctx);

  // Install the prototype.
  storage->setSynthesizedModifyCoroutine(modify);
}


static void addOpaqueAccessorToStorage(AbstractStorageDecl *storage,
                                       AccessorKind kind,
                                       ASTContext &ctx) {
  switch (kind) {
  case AccessorKind::Get:
    return addGetterToStorage(storage, ctx);

  case AccessorKind::Set:
    return addSetterToStorage(storage, ctx);

  case AccessorKind::Read:
    return addReadCoroutineToStorage(storage, ctx);

  case AccessorKind::Modify:
    return addModifyCoroutineToStorage(storage, ctx);

#define OPAQUE_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
  case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
    llvm_unreachable("not an opaque accessor");
  }
}

static void addExpectedOpaqueAccessorsToStorage(AbstractStorageDecl *storage,
                                                ASTContext &ctx) {
  // Nameless vars from interface files should not have any accessors.
  // TODO: Replace this check with a broader check that all storage decls
  //       from interface files have all their accessors up front.
  if (storage->getBaseName().empty())
    return;
  storage->visitExpectedOpaqueAccessors([&](AccessorKind kind) {
    // If the accessor is already present, there's nothing to do.
    if (storage->getAccessor(kind))
      return;

    addOpaqueAccessorToStorage(storage, kind, ctx);
  });
}

/// Synthesize the body of a setter which just delegates to a mutable
/// addressor.
static std::pair<BraceStmt *, bool>
synthesizeMutableAddressSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  // This should call the mutable addressor.
  return synthesizeTrivialSetterBodyWithStorage(setter,
                                                TargetImpl::Implementation,
                                                setter->getStorage(), ctx);
}

/// Synthesize the body of a setter which just delegates to a modify
/// coroutine accessor.
static std::pair<BraceStmt *, bool>
synthesizeModifyCoroutineSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  // This should call the modify coroutine.
  return synthesizeTrivialSetterBodyWithStorage(setter,
                                                TargetImpl::Implementation,
                                                setter->getStorage(), ctx);
}

std::pair<BraceStmt *, bool>
synthesizeAccessorBody(AbstractFunctionDecl *fn, void *);

/// The specified AbstractStorageDecl was just found to satisfy a
/// protocol property requirement.  Ensure that it has the full
/// complement of accessors.
void TypeChecker::synthesizeWitnessAccessorsForStorage(
                                             AbstractStorageDecl *requirement,
                                             AbstractStorageDecl *storage) {
  bool addedAccessor = false;

  // Make sure the protocol requirement itself has the right accessors.
  // FIXME: This should be a request kicked off by SILGen.
  DeclsToFinalize.insert(requirement);

  requirement->visitExpectedOpaqueAccessors([&](AccessorKind kind) {
    // If the accessor already exists, we have nothing to do.
    if (storage->getAccessor(kind))
      return;

    // Otherwise, synthesize it.
    addOpaqueAccessorToStorage(storage, kind, Context);

    // Flag that we've added an accessor.
    addedAccessor = true;

    // Trigger synthesize of the accessor body if it's created on-demand.
    if (isOnDemandAccessor(storage, kind)) {
      auto *accessor = storage->getAccessor(kind);
      assert(!accessor->hasBody());
      accessor->setBodySynthesizer(&synthesizeAccessorBody);

      maybeMarkTransparent(accessor, Context);
      DeclsToFinalize.insert(accessor);
    }
  });

  // Cue (delayed) validation of any accessors we just added, just
  // in case this is coming after the normal delayed validation finished.
  if (addedAccessor) {
    DeclsToFinalize.insert(storage);
  }
}

/// Given a VarDecl with a willSet: and/or didSet: specifier, synthesize the
/// setter which calls them.
static std::pair<BraceStmt *, bool>
synthesizeObservedSetterBody(AccessorDecl *Set, TargetImpl target,
                             ASTContext &Ctx) {
  auto VD = cast<VarDecl>(Set->getStorage());

  SourceLoc Loc = VD->getLoc();

  // Start by finding the decls for 'self' and 'value'.
  auto *SelfDecl = Set->getImplicitSelfDecl();
  VarDecl *ValueDecl = Set->getParameters()->get(0);

  bool IsSelfLValue = VD->isSetterMutating();

  SubstitutionMap subs;
  if (auto *genericEnv = Set->getGenericEnvironment())
    subs = genericEnv->getForwardingSubstitutionMap();

  // The setter loads the oldValue, invokes willSet with the incoming value,
  // does a direct store, then invokes didSet with the oldValue.
  SmallVector<ASTNode, 6> SetterBody;

  auto callObserver = [&](AccessorDecl *observer, VarDecl *arg) {
    ConcreteDeclRef ref(observer, subs);
    auto type = observer->getInterfaceType()
                  .subst(subs, SubstFlags::UseErrorType);
    Expr *Callee = new (Ctx) DeclRefExpr(ref, DeclNameLoc(), /*imp*/true);
    Callee->setType(type);
    auto *ValueDRE = new (Ctx) DeclRefExpr(arg, DeclNameLoc(), /*imp*/true);
    ValueDRE->setType(arg->getType());

    if (SelfDecl) {
      auto *SelfDRE = buildSelfReference(SelfDecl, SelfAccessorKind::Peer,
                                         IsSelfLValue, Ctx);
      SelfDRE = maybeWrapInOutExpr(SelfDRE, Ctx);
      auto *DSCE = new (Ctx) DotSyntaxCallExpr(Callee, SourceLoc(), SelfDRE);

      if (auto funcType = type->getAs<FunctionType>())
        type = funcType->getResult();
      DSCE->setType(type);
      DSCE->setThrows(false);
      Callee = DSCE;
    }

    auto *Call = CallExpr::createImplicit(Ctx, Callee, { ValueDRE },
                                          { Identifier() });
    if (auto funcType = type->getAs<FunctionType>())
      type = funcType->getResult();
    Call->setType(type);
    Call->setThrows(false);

    SetterBody.push_back(Call);
  };

  // If there is a didSet, it will take the old value.  Load it into a temporary
  // 'let' so we have it for later.
  // TODO: check the body of didSet to only do this load (which may call the
  // superclass getter) if didSet takes an argument.
  VarDecl *OldValue = nullptr;
  if (VD->getDidSetFunc()) {
    Expr *OldValueExpr
      = buildStorageReference(Set, VD, target, /*isLValue=*/true, Ctx);
    OldValueExpr = new (Ctx) LoadExpr(OldValueExpr, VD->getType());

    OldValue = new (Ctx) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Let,
                                 /*IsCaptureList*/false, SourceLoc(),
                                 Ctx.getIdentifier("tmp"), Set);
    OldValue->setImplicit();
    OldValue->setInterfaceType(VD->getValueInterfaceType());
    auto *tmpPattern = new (Ctx) NamedPattern(OldValue, /*implicit*/ true);
    auto *tmpPBD = PatternBindingDecl::createImplicit(
        Ctx, StaticSpellingKind::None, tmpPattern, OldValueExpr, Set);
    SetterBody.push_back(tmpPBD);
    SetterBody.push_back(OldValue);
  }

  if (auto willSet = VD->getWillSetFunc())
    callObserver(willSet, ValueDecl);
  
  // Create an assignment into the storage or call to superclass setter.
  auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, DeclNameLoc(), true);
  ValueDRE->setType(ValueDecl->getType());
  createPropertyStoreOrCallSuperclassSetter(Set, ValueDRE, VD, target,
                                            SetterBody, Ctx);

  if (auto didSet = VD->getDidSetFunc())
    callObserver(didSet, OldValue);

  return { BraceStmt::create(Ctx, Loc, SetterBody, Loc, true),
           /*isTypeChecked=*/true };
}

static std::pair<BraceStmt *, bool>
synthesizeStoredWithObserversSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  return synthesizeObservedSetterBody(setter, TargetImpl::Storage, ctx);
}

static std::pair<BraceStmt *, bool>
synthesizeInheritedWithObserversSetterBody(AccessorDecl *setter,
                                           ASTContext &ctx) {
  return synthesizeObservedSetterBody(setter, TargetImpl::Super, ctx);
}

namespace {
  /// This ASTWalker explores an expression tree looking for expressions (which
  /// are DeclContext's) and changes their parent DeclContext to NewDC.
  class RecontextualizeClosures : public ASTWalker {
    DeclContext *NewDC;
  public:
    RecontextualizeClosures(DeclContext *NewDC) : NewDC(NewDC) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // If we find a closure, update its declcontext and do *not* walk into it.
      if (auto CE = dyn_cast<AbstractClosureExpr>(E)) {
        CE->setParent(NewDC);
        return { false, E };
      }
      
      if (auto CLE = dyn_cast<CaptureListExpr>(E)) {
        // Make sure to recontextualize any decls in the capture list as well.
        for (auto &CLE : CLE->getCaptureList()) {
          CLE.Var->setDeclContext(NewDC);
          CLE.Init->setDeclContext(NewDC);
        }
      }
      
      // Unlike a closure, a TapExpr is not a DeclContext, so we need to
      // recontextualize its variable and then anything else in its body.
      // FIXME: Might be better to change walkToDeclPre() and walkToStmtPre()
      // below, but I don't know what other effects that might have.
      if (auto TE = dyn_cast<TapExpr>(E)) {
        TE->getVar()->setDeclContext(NewDC);
        for (auto node : TE->getBody()->getElements())
          node.walk(RecontextualizeClosures(NewDC));
      }

      return { true, E };
    }

    /// We don't want to recurse into declarations or statements.
    bool walkToDeclPre(Decl *) override { return false; }
    std::pair<bool, Stmt*> walkToStmtPre(Stmt *S) override { return {false,S}; }
  };
} // end anonymous namespace

/// Synthesize the getter for a lazy property with the specified storage
/// vardecl.
static std::pair<BraceStmt *, bool>
synthesizeLazyGetterBody(AccessorDecl *Get, VarDecl *VD, VarDecl *Storage,
                         ASTContext &Ctx) {
  // FIXME: Remove TypeChecker dependencies below.
  auto &TC = *(TypeChecker *) Ctx.getLazyResolver();

  // The getter checks the optional, storing the initial value in if nil.  The
  // specific pattern we generate is:
  //   get {
  //     if let tmp1 = storage {
  //       return tmp1
  //     }
  //     let tmp2 : Ty = <<initializer expression>>
  //     storage = tmp2
  //     return tmp2
  //   }
  SmallVector<ASTNode, 6> Body;

  // Load the existing storage and store it into the 'tmp1' temporary.
  auto *Tmp1VD = new (Ctx) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Let,
                                   /*IsCaptureList*/false, SourceLoc(),
                                   Ctx.getIdentifier("tmp1"), Get);
  Tmp1VD->setInterfaceType(VD->getValueInterfaceType());
  Tmp1VD->setHasNonPatternBindingInit();
  Tmp1VD->setImplicit();

  auto *Named = new (Ctx) NamedPattern(Tmp1VD, /*implicit*/true);
  Named->setType(Tmp1VD->getType());
  auto *Let = new (Ctx) VarPattern(SourceLoc(), /*let*/true, Named,
                                   /*implict*/true);
  Let->setType(Named->getType());
  auto *Some = new (Ctx) OptionalSomePattern(Let, SourceLoc(),
                                             /*implicit*/true);
  Some->setElementDecl(Ctx.getOptionalSomeDecl());
  Some->setType(OptionalType::get(Let->getType()));

  auto *StoredValueExpr =
    createPropertyLoadOrCallSuperclassGetter(Get, Storage,
                                             TargetImpl::Storage, Ctx);
  SmallVector<StmtConditionElement, 1> Cond;
  Cond.emplace_back(SourceLoc(), Some, StoredValueExpr);

  // Build the early return inside the if.
  auto *Tmp1DRE = new (Ctx) DeclRefExpr(Tmp1VD, DeclNameLoc(), /*Implicit*/true,
                                        AccessSemantics::Ordinary);
  Tmp1DRE->setType(Tmp1VD->getType());
  auto *Return = new (Ctx) ReturnStmt(SourceLoc(), Tmp1DRE,
                                      /*implicit*/true);


  // Build the "if" around the early return.
  Body.push_back(new (Ctx) IfStmt(LabeledStmtInfo(),
                                  SourceLoc(), Ctx.AllocateCopy(Cond), Return,
                                  /*elseloc*/SourceLoc(), /*else*/nullptr,
                                  /*implicit*/ true));


  auto *Tmp2VD = new (Ctx) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Let,
                                   /*IsCaptureList*/false, SourceLoc(),
                                   Ctx.getIdentifier("tmp2"),
                                   Get);
  Tmp2VD->setInterfaceType(VD->getValueInterfaceType());
  Tmp2VD->setImplicit();


  // Take the initializer from the PatternBindingDecl for VD.
  // TODO: This doesn't work with complicated patterns like:
  //   lazy var (a,b) = foo()
  auto *InitValue = VD->getParentInitializer();
  auto PBD = VD->getParentPatternBinding();
  unsigned entryIndex = PBD->getPatternEntryIndexForVarDecl(VD);
  assert(PBD->isInitializerSubsumed(entryIndex));

  if (!PBD->isInitializerChecked(entryIndex))
    TC.typeCheckPatternBinding(PBD, entryIndex);

  // Recontextualize any closure declcontexts nested in the initializer to
  // realize that they are in the getter function.
  Get->getImplicitSelfDecl()->setDeclContext(Get);
  InitValue->walk(RecontextualizeClosures(Get));

  // Wrap the initializer in a LazyInitializerExpr to avoid walking it twice.
  auto initType = InitValue->getType();
  InitValue = new (Ctx) LazyInitializerExpr(InitValue);
  InitValue->setType(initType);

  Pattern *Tmp2PBDPattern = new (Ctx) NamedPattern(Tmp2VD, /*implicit*/true);
  Tmp2PBDPattern =
    TypedPattern::createImplicit(Ctx, Tmp2PBDPattern, Tmp2VD->getType());

  auto *Tmp2PBD = PatternBindingDecl::createImplicit(
      Ctx, StaticSpellingKind::None, Tmp2PBDPattern, InitValue, Get,
      /*VarLoc*/ InitValue->getStartLoc());
  Body.push_back(Tmp2PBD);
  Body.push_back(Tmp2VD);

  // Assign tmp2 into storage.
  auto Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, DeclNameLoc(), /*Implicit*/true,
                                       AccessSemantics::DirectToStorage);
  Tmp2DRE->setType(Tmp2VD->getType());
  createPropertyStoreOrCallSuperclassSetter(Get, Tmp2DRE, Storage,
                                            TargetImpl::Storage, Body, Ctx);

  // Return tmp2.
  Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, DeclNameLoc(), /*Implicit*/true,
                                  AccessSemantics::DirectToStorage);
  Tmp2DRE->setType(Tmp2VD->getType());

  Body.push_back(new (Ctx) ReturnStmt(SourceLoc(), Tmp2DRE, /*implicit*/true));

  return { BraceStmt::create(Ctx, VD->getLoc(), Body, VD->getLoc(),
                             /*implicit*/true),
           /*isTypeChecked=*/true };
}

llvm::Expected<VarDecl *>
LazyStoragePropertyRequest::evaluate(Evaluator &evaluator,
                                     VarDecl *VD) const {
  assert(isa<SourceFile>(VD->getDeclContext()->getModuleScopeContext()));
  assert(VD->getAttrs().hasAttribute<LazyAttr>());
  auto &Context = VD->getASTContext();

  // Create the storage property as an optional of VD's type.
  SmallString<64> NameBuf;
  NameBuf += "$__lazy_storage_$_";
  NameBuf += VD->getName().str();
  auto StorageName = Context.getIdentifier(NameBuf);

  if (!VD->hasInterfaceType())
    Context.getLazyResolver()->resolveDeclSignature(VD);

  auto StorageTy = OptionalType::get(VD->getType());
  auto StorageInterfaceTy = OptionalType::get(VD->getInterfaceType());

  auto *Storage = new (Context) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Var,
                                        /*IsCaptureList*/false, VD->getLoc(),
                                        StorageName,
                                        VD->getDeclContext());
  Storage->setInterfaceType(StorageInterfaceTy);
  Storage->setLazyStorageProperty(true);
  Storage->setUserAccessible(false);

  // The storage is implicit and private.
  Storage->setImplicit();
  Storage->overwriteAccess(AccessLevel::Private);
  Storage->overwriteSetterAccess(AccessLevel::Private);

  addMemberToContextIfNeeded(Storage, VD->getDeclContext(), VD);

  // Create the pattern binding decl for the storage decl.  This will get
  // default initialized to nil.
  Pattern *PBDPattern = new (Context) NamedPattern(Storage, /*implicit*/true);
  PBDPattern->setType(StorageTy);
  PBDPattern = TypedPattern::createImplicit(Context, PBDPattern, StorageTy);
  auto *InitExpr = new (Context) NilLiteralExpr(SourceLoc(), /*Implicit=*/true);
  InitExpr->setType(Storage->getType());

  auto *PBD = PatternBindingDecl::createImplicit(
      Context, StaticSpellingKind::None, PBDPattern, InitExpr,
      VD->getDeclContext(), /*VarLoc*/ VD->getLoc());
  PBD->setInitializerChecked(0);

  addMemberToContextIfNeeded(PBD, VD->getDeclContext(), Storage);

  return Storage;
}

/// Synthesize a computed property `$foo` for a property with an attached
/// wrapper that has a `projectedValue` property.
static VarDecl *synthesizePropertyWrapperStorageWrapperProperty(
    ASTContext &ctx, VarDecl *var, Type wrapperType,
    VarDecl *wrapperVar) {
  // If the original property has a @_projectedValueProperty attribute, use
  // that to find the storage wrapper property.
  if (auto attr = var->getAttrs().getAttribute<ProjectedValuePropertyAttr>()){
    SmallVector<ValueDecl *, 2> declsFound;
    auto projectionName = attr->ProjectionPropertyName;
    auto dc = var->getDeclContext();
    if (dc->isTypeContext()) {
      dc->lookupQualified(dc->getSelfNominalTypeDecl(), projectionName,
                          NL_QualifiedDefault, declsFound);
    } else if (dc->isModuleScopeContext()) {
      dc->lookupQualified(dc->getParentModule(), projectionName,
                          NL_QualifiedDefault, declsFound);
    } else {
      llvm_unreachable("Property wrappers don't work in local contexts");
    }

    if (declsFound.size() == 1 && isa<VarDecl>(declsFound.front())) {
      auto property = cast<VarDecl>(declsFound.front());
      property->setOriginalWrappedProperty(var);
      return property;
    }

    ctx.Diags.diagnose(attr->getLocation(),
                       diag::property_wrapper_projection_value_missing,
                       projectionName);
    attr->setInvalid();
  }

  // Compute the name of the storage type.
  SmallString<64> nameBuf;
  nameBuf = "$";
  nameBuf += var->getName().str();
  Identifier name = ctx.getIdentifier(nameBuf);

  // Determine the type of the property.
  if (!wrapperVar->hasInterfaceType()) {
    static_cast<TypeChecker &>(*ctx.getLazyResolver()).validateDecl(wrapperVar);
  }
  Type propertyType = wrapperType->getTypeOfMember(
      var->getModuleContext(), wrapperVar,
      wrapperVar->getValueInterfaceType());

  // Form the property.
  auto dc = var->getDeclContext();
  VarDecl *property = new (ctx) VarDecl(/*IsStatic=*/var->isStatic(),
                                        VarDecl::Specifier::Var,
                                        /*IsCaptureList=*/false,
                                        var->getLoc(),
                                        name, dc);
  property->setInterfaceType(propertyType);
  property->setImplicit();
  property->setOriginalWrappedProperty(var);
  addMemberToContextIfNeeded(property, dc, var);

  // Create the pattern binding declaration for the property.
  Pattern *pbdPattern = new (ctx) NamedPattern(property, /*implicit=*/true);
  pbdPattern->setType(propertyType);
  pbdPattern = TypedPattern::createImplicit(ctx, pbdPattern, propertyType);
  auto pbd = PatternBindingDecl::createImplicit(
      ctx, property->getCorrectStaticSpelling(), pbdPattern,
      /*init*/nullptr, dc, SourceLoc());
  addMemberToContextIfNeeded(pbd, dc, var);
  pbd->setStatic(var->isStatic());

  // Determine the access level for the property.
  property->overwriteAccess(var->getFormalAccess());

  // Determine setter access.
  property->overwriteSetterAccess(var->getSetterFormalAccess());

  // Add the accessors we need.
  bool hasSetter = wrapperVar->isSettable(nullptr) &&
      wrapperVar->isSetterAccessibleFrom(var->getInnermostDeclContext());
  if (hasSetter)
    property->overwriteImplInfo(StorageImplInfo::getMutableComputed());
  else
    property->overwriteImplInfo(StorageImplInfo::getImmutableComputed());
  addExpectedOpaqueAccessorsToStorage(property, ctx);

  var->getAttrs().add(
      new (ctx) ProjectedValuePropertyAttr(name, SourceLoc(), SourceRange(),
                                            /*Implicit=*/true));
  return property;
}

static void typeCheckSynthesizedWrapperInitializer(
    PatternBindingDecl *pbd, VarDecl *backingVar, PatternBindingDecl *parentPBD,
    Expr *&initializer) {
  // Figure out the context in which the initializer was written.
  DeclContext *originalDC = parentPBD->getDeclContext();
  if (!originalDC->isLocalContext()) {
    auto initContext =
        cast_or_null<PatternBindingInitializer>(parentPBD->getInitContext(0));
    if (initContext)
      originalDC = initContext;
  }

  // Type-check the initialization.
  ASTContext &ctx = pbd->getASTContext();
  auto &tc = *static_cast<TypeChecker *>(ctx.getLazyResolver());
  tc.typeCheckExpression(initializer, originalDC);
  if (auto initializerContext =
          dyn_cast_or_null<Initializer>(
            pbd->getPatternEntryForVarDecl(backingVar).getInitContext())) {
    tc.contextualizeInitializer(initializerContext, initializer);
  }
  tc.checkPropertyWrapperErrorHandling(pbd, initializer);
}

llvm::Expected<PropertyWrapperBackingPropertyInfo>
PropertyWrapperBackingPropertyInfoRequest::evaluate(Evaluator &evaluator,
                                                     VarDecl *var) const {
  // Determine the type of the backing property.
  auto wrapperType = var->getPropertyWrapperBackingPropertyType();
  if (!wrapperType || wrapperType->hasError())
    return PropertyWrapperBackingPropertyInfo();

  auto wrapperInfo = var->getAttachedPropertyWrapperTypeInfo(0);
  if (!wrapperInfo)
    return PropertyWrapperBackingPropertyInfo();

  // Compute the name of the storage type.
  ASTContext &ctx = var->getASTContext();
  SmallString<64> nameBuf;
  nameBuf = "_";
  nameBuf += var->getName().str();
  Identifier name = ctx.getIdentifier(nameBuf);

  // Determine the type of the storage.
  auto dc = var->getDeclContext();
  Type storageInterfaceType = wrapperType;
  Type storageType = dc->mapTypeIntoContext(storageInterfaceType);

  if (!var->hasInterfaceType()) {
    auto &tc = *static_cast<TypeChecker *>(ctx.getLazyResolver());
    tc.validateDecl(var);
    assert(var->hasInterfaceType());
  }

  // Make sure that the property type matches the value of the
  // wrapper type.
  if (!storageInterfaceType->hasError()) {
    Type expectedPropertyType =
        computeWrappedValueType(var, storageInterfaceType);
    Type propertyType = var->getValueInterfaceType();
    if (!expectedPropertyType->hasError() &&
        !propertyType->hasError() &&
        !propertyType->isEqual(expectedPropertyType)) {
      var->diagnose(diag::property_wrapper_incompatible_property,
                    propertyType, wrapperType);
      if (auto nominalWrapper = wrapperType->getAnyNominal()) {
        nominalWrapper->diagnose(diag::property_wrapper_declared_here,
                                 nominalWrapper->getFullName());
      }
    }
  }

  // Create the backing storage property and note it in the cache.
  VarDecl *backingVar = new (ctx) VarDecl(/*IsStatic=*/var->isStatic(),
                                          VarDecl::Specifier::Var,
                                          /*IsCaptureList=*/false,
                                          var->getLoc(),
                                          name, dc);
  backingVar->setInterfaceType(storageInterfaceType);
  backingVar->setImplicit();
  backingVar->setOriginalWrappedProperty(var);

  // The backing storage is 'private'.
  backingVar->overwriteAccess(AccessLevel::Private);
  backingVar->overwriteSetterAccess(AccessLevel::Private);

  addMemberToContextIfNeeded(backingVar, dc, var);

  // Create the pattern binding declaration for the backing property.
  Pattern *pbdPattern = new (ctx) NamedPattern(backingVar, /*implicit=*/true);
  pbdPattern->setType(storageType);
  pbdPattern = TypedPattern::createImplicit(ctx, pbdPattern, storageType);
  auto pbd = PatternBindingDecl::createImplicit(
      ctx, backingVar->getCorrectStaticSpelling(), pbdPattern,
      /*init*/nullptr, dc, SourceLoc());
  addMemberToContextIfNeeded(pbd, dc, var);
  pbd->setStatic(var->isStatic());

  // Take the initializer from the original property.
  auto parentPBD = var->getParentPatternBinding();
  unsigned patternNumber = parentPBD->getPatternEntryIndexForVarDecl(var);
  if (parentPBD->isInitialized(patternNumber) &&
      !parentPBD->isInitializerChecked(patternNumber)) {
    auto &tc = *static_cast<TypeChecker *>(ctx.getLazyResolver());
    tc.typeCheckPatternBinding(parentPBD, patternNumber);
  }

  Expr *originalInitialValue = nullptr;
  if (Expr *init = parentPBD->getInit(patternNumber)) {
    pbd->setInit(0, init);
    pbd->setInitializerChecked(0);
    originalInitialValue = findOriginalPropertyWrapperInitialValue(var, init);
  } else if (!parentPBD->isInitialized(patternNumber) &&
             wrapperInfo.defaultInit) {
    // FIXME: Record this expression somewhere so that DI can perform the
    // initialization itself.
    auto typeExpr = TypeExpr::createImplicit(storageType, ctx);
    Expr *initializer = CallExpr::createImplicit(ctx, typeExpr, {}, { });
    typeCheckSynthesizedWrapperInitializer(pbd, backingVar, parentPBD,
                                           initializer);
    pbd->setInit(0, initializer);
    pbd->setInitializerChecked(0);
  }

  // If there is a projection property (projectedValue) in the wrapper,
  // synthesize a computed property for '$foo'.
  VarDecl *storageVar = nullptr;
  if (wrapperInfo.projectedValueVar) {
    storageVar = synthesizePropertyWrapperStorageWrapperProperty(
        ctx, var, storageInterfaceType, wrapperInfo.projectedValueVar);
  }

  // Get the property wrapper information.
  if (!var->allAttachedPropertyWrappersHaveInitialValueInit() &&
      !originalInitialValue) {
    return PropertyWrapperBackingPropertyInfo(
        backingVar, storageVar, nullptr, nullptr, nullptr);
  }

  // Form the initialization of the backing property from a value of the
  // original property's type.
  OpaqueValueExpr *origValue =
      new (ctx) OpaqueValueExpr(var->getLoc(), var->getType(),
                                /*isPlaceholder=*/true);
  Expr *initializer = buildPropertyWrapperInitialValueCall(
      var, storageType, origValue,
      /*ignoreAttributeArgs=*/!originalInitialValue);
  typeCheckSynthesizedWrapperInitializer(
      pbd, backingVar, parentPBD, initializer);

  return PropertyWrapperBackingPropertyInfo(
      backingVar, storageVar, originalInitialValue, initializer, origValue);
}

static bool wouldBeCircularSynthesis(AbstractStorageDecl *storage,
                                     AccessorKind kind) {
  // All lazy property and property wrapper accessors are non-circular.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->getAttrs().hasAttribute<LazyAttr>())
      return false;

    if (var->hasAttachedPropertyWrapper())
      return false;

    if (var->getOriginalWrappedProperty(
            PropertyWrapperSynthesizedPropertyKind::StorageWrapper))
      return false;
  }

  switch (kind) {
  case AccessorKind::Get:
    return storage->getReadImpl() == ReadImplKind::Get;
  case AccessorKind::Read:
    return storage->getReadImpl() == ReadImplKind::Read;
  case AccessorKind::Set:
    return storage->getWriteImpl() == WriteImplKind::Set;
  case AccessorKind::Modify:
    return storage->getReadWriteImpl() == ReadWriteImplKind::Modify;
#define OPAQUE_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
  case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
    llvm_unreachable("unexpected opaque accessor");
  }
  llvm_unreachable("bad kind");
}

void swift::triggerAccessorSynthesis(TypeChecker &TC,
                                     AbstractStorageDecl *storage) {
  maybeAddAccessorsToStorage(storage);

  // Trigger accessor synthesis.
  storage->visitExpectedOpaqueAccessors([&](AccessorKind kind) {
    // Don't synthesize an accessor if the accessor is supposed to be
    // the basis of the storage implementation.
    if (wouldBeCircularSynthesis(storage, kind))
      return;

    // Don't try to synthesize an accessor that doesn't exist.
    // TODO: should this be an assertion?
    auto accessor = storage->getAccessor(kind);
    if (!accessor)
      return;

    if (!accessor->isImplicit())
      return;

    if (!accessor->hasBody()) {
      maybeMarkTransparent(accessor, TC.Context);
      accessor->setBodySynthesizer(&synthesizeAccessorBody);
      TC.DeclsToFinalize.insert(accessor);
    }
  });
}

static StorageImplInfo getProtocolStorageImpl(AbstractStorageDecl *storage) {
  auto protocol = cast<ProtocolDecl>(storage->getDeclContext());
  if (protocol->isObjC()) {
    return StorageImplInfo::getComputed(storage->supportsMutation());
  } else {
    return StorageImplInfo::getOpaque(storage->supportsMutation(),
                                      storage->getOpaqueReadOwnership());
  }
}

/// Given a storage declaration in a protocol, set it up with the right
/// StorageImpl and add the right set of opaque accessors.
static void finishProtocolStorageImplInfo(AbstractStorageDecl *storage) {
  if (auto *var = dyn_cast<VarDecl>(storage)) {
    if (var->hasStorage()) {
      auto &ctx = var->getASTContext();
      // Protocols cannot have stored properties.
      if (var->isLet()) {
        ctx.Diags.diagnose(var->getLoc(),
                           diag::protocol_property_must_be_computed_var)
          .fixItReplace(var->getParentPatternBinding()->getLoc(), "var")
          .fixItInsertAfter(var->getTypeLoc().getLoc(), " { get }");
      } else {
        auto diag = ctx.Diags.diagnose(var->getLoc(),
                                       diag::protocol_property_must_be_computed);
        auto braces = var->getBracesRange();

        if (braces.isValid())
          diag.fixItReplace(braces, "{ get <#set#> }");
        else
          diag.fixItInsertAfter(var->getTypeLoc().getLoc(), " { get <#set#> }");
      }
    }
  }

  storage->overwriteImplInfo(getProtocolStorageImpl(storage));
}

static void finishLazyVariableImplInfo(VarDecl *var) {
  // FIXME: Remove this once getStoredProperties() is a request
  (void) var->getLazyStorageProperty();

  // If there are already accessors, something is invalid; bail out.
  if (!var->getImplInfo().isSimpleStored())
    return;

  var->overwriteImplInfo(StorageImplInfo::getMutableComputed());
}

/// Determine whether all of the wrapped-value setters for the property
/// wrappers attached to this variable are available and accessible.
static bool allPropertyWrapperValueSettersAreAccessible(VarDecl *var) {
  auto wrapperAttrs = var->getAttachedPropertyWrappers();
  auto innermostDC = var->getInnermostDeclContext();
  for (unsigned i : indices(wrapperAttrs)) {
    auto wrapperInfo = var->getAttachedPropertyWrapperTypeInfo(i);
    auto valueVar = wrapperInfo.valueVar;
    if (!valueVar->isSettable(nullptr) ||
        !valueVar->isSetterAccessibleFrom(innermostDC))
      return false;
  }
  
  return true;
}

static void finishPropertyWrapperImplInfo(VarDecl *var) {
  auto backingVar = var->getPropertyWrapperBackingProperty();
  if (!backingVar || backingVar->isInvalid())
    return;

  auto parentSF = var->getDeclContext()->getParentSourceFile();
  bool wrapperSetterIsUsable =
    var->getSetter() ||
    (parentSF &&
     parentSF->Kind != SourceFileKind::Interface &&
     !var->isLet() &&
     allPropertyWrapperValueSettersAreAccessible(var));

  if (wrapperSetterIsUsable)
    var->overwriteImplInfo(StorageImplInfo::getMutableComputed());
  else
    var->overwriteImplInfo(StorageImplInfo::getImmutableComputed());
}

static void finishNSManagedImplInfo(VarDecl *VD) {
  // If it's not still stored, just bail out.
  if (!VD->getImplInfo().isSimpleStored())
    return;

  VD->overwriteImplInfo(StorageImplInfo::getMutableComputed());
}

static void finishStorageImplInfo(AbstractStorageDecl *storage) {
  if (isa<ProtocolDecl>(storage->getDeclContext())) {
    finishProtocolStorageImplInfo(storage);
    return;
  }

  auto var = dyn_cast<VarDecl>(storage);
  if (var == nullptr)
    return;

  if (var->getAttrs().hasAttribute<LazyAttr>()) {
    finishLazyVariableImplInfo(var);
  } else if (var->getAttrs().hasAttribute<NSManagedAttr>()) {
    finishNSManagedImplInfo(var);
  } else if (var->hasAttachedPropertyWrapper()) {
    finishPropertyWrapperImplInfo(var);
  }
}

/// Try to add the appropriate accessors required a storage declaration.
/// This needs to be idempotent.
void swift::maybeAddAccessorsToStorage(AbstractStorageDecl *storage) {
  finishStorageImplInfo(storage);

  // Implicit properties don't get accessors.
  if (storage->isImplicit() &&
      !(isa<VarDecl>(storage) &&
        cast<VarDecl>(storage)->getOriginalWrappedProperty()))
    return;

  if (storage->getImplInfo().isSimpleStored()) {
    auto *dc = storage->getDeclContext();

    // Local stored variables don't otherwise get accessors.
    if (dc->isLocalContext()) {
      return;

    } else if (dc->isModuleScopeContext()) {
      // Fixed-layout global variables don't get accessors.
      if (!storage->isResilient() && !storage->isNativeDynamic())
        return;

    // Stored properties imported from Clang don't get accessors.
    } else if (auto *structDecl = dyn_cast<StructDecl>(dc)) {
      if (structDecl->hasClangNode())
        return;
    }

    // Stored properties in SIL mode don't get accessors.
    // But we might need to create opaque accessors for them.
    if (auto sourceFile = dc->getParentSourceFile()) {
      if (sourceFile->Kind == SourceFileKind::SIL) {
        if (!storage->getGetter())
          return;
      }
    }
  }

  // Everything else gets mandatory accessors.
  auto &ctx = storage->getASTContext();
  addExpectedOpaqueAccessorsToStorage(storage, ctx);
}

static std::pair<BraceStmt *, bool>
synthesizeGetterBody(AccessorDecl *getter, ASTContext &ctx) {
  auto storage = getter->getStorage();

  // Synthesize the getter for a lazy property or property wrapper.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->getAttrs().hasAttribute<LazyAttr>()) {
      auto *storage = var->getLazyStorageProperty();
      return synthesizeLazyGetterBody(getter, var, storage, ctx);
    }

    if (var->hasAttachedPropertyWrapper()) {
      return synthesizePropertyWrapperGetterBody(getter, ctx);
    }

    if (var->getOriginalWrappedProperty(
            PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
      return synthesizeTrivialGetterBody(getter, TargetImpl::WrapperStorage,
                                         ctx);
    }
  }

  if (getter->hasForcedStaticDispatch()) {
    return synthesizeTrivialGetterBody(getter, TargetImpl::Ordinary, ctx);
  }

  switch (getter->getStorage()->getReadImpl()) {
  case ReadImplKind::Stored:
    return synthesizeTrivialGetterBody(getter, ctx);

  case ReadImplKind::Get:
    llvm_unreachable("synthesizing getter that already exists?");

  case ReadImplKind::Inherited:
    return synthesizeInheritedGetterBody(getter, ctx);

  case ReadImplKind::Address:
    return synthesizeAddressedGetterBody(getter, ctx);

  case ReadImplKind::Read:
    return synthesizeReadCoroutineGetterBody(getter, ctx);
  }
  llvm_unreachable("bad ReadImplKind");
}

static std::pair<BraceStmt *, bool>
synthesizeSetterBody(AccessorDecl *setter, ASTContext &ctx) {
  auto storage = setter->getStorage();

  // Synthesize the setter for a lazy property or property wrapper.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->getAttrs().hasAttribute<LazyAttr>()) {
      // Lazy property setters write to the underlying storage.
      auto *storage = var->getLazyStorageProperty();
      return synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Storage,
                                                    storage, ctx);
    }

    if (var->hasAttachedPropertyWrapper()) {
      if (var->getAccessor(AccessorKind::WillSet) ||
          var->getAccessor(AccessorKind::DidSet)) {
        return synthesizeObservedSetterBody(setter, TargetImpl::Wrapper, ctx);
      }

      return synthesizePropertyWrapperSetterBody(setter, ctx);
    }

    // Synthesize a getter for the storage wrapper property of a property
    // with an attached wrapper.
    if (auto original = var->getOriginalWrappedProperty(
            PropertyWrapperSynthesizedPropertyKind::StorageWrapper)) {
      auto backingVar = original->getPropertyWrapperBackingProperty();
      return synthesizeTrivialSetterBodyWithStorage(setter,
                                                    TargetImpl::WrapperStorage,
                                                    backingVar, ctx);
    }
  }

  switch (storage->getWriteImpl()) {
  case WriteImplKind::Immutable:
    llvm_unreachable("synthesizing setter from immutable storage");

  case WriteImplKind::Stored:
    return synthesizeTrivialSetterBody(setter, ctx);

  case WriteImplKind::StoredWithObservers:
    return synthesizeStoredWithObserversSetterBody(setter, ctx);

  case WriteImplKind::InheritedWithObservers:
    return synthesizeInheritedWithObserversSetterBody(setter, ctx);

  case WriteImplKind::Set:
    llvm_unreachable("synthesizing setter for unknown reason?");  

  case WriteImplKind::MutableAddress:
    return synthesizeMutableAddressSetterBody(setter, ctx);

  case WriteImplKind::Modify:
    return synthesizeModifyCoroutineSetterBody(setter, ctx);
  }
  llvm_unreachable("bad ReadImplKind");
}
 
std::pair<BraceStmt *, bool>
synthesizeAccessorBody(AbstractFunctionDecl *fn, void *) {
  auto *accessor = cast<AccessorDecl>(fn);
  auto &ctx = accessor->getASTContext();

  if (accessor->isInvalid() || ctx.hadError())
    return { nullptr, true };

  switch (accessor->getAccessorKind()) {
  case AccessorKind::Get:
    return synthesizeGetterBody(accessor, ctx);

  case AccessorKind::Set:
    return synthesizeSetterBody(accessor, ctx);

  case AccessorKind::Read:
    return synthesizeReadCoroutineBody(accessor, ctx);

  case AccessorKind::Modify:
    return synthesizeModifyCoroutineBody(accessor, ctx);

  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::Address:
  case AccessorKind::MutableAddress:
    break;
  }
  llvm_unreachable("bad synthesized function kind");
}

static void maybeAddMemberwiseDefaultArg(ParamDecl *arg, VarDecl *var,
                    SmallVectorImpl<DefaultArgumentInitializer *> &defaultInits,
                                         unsigned paramSize, ASTContext &ctx) {
  // First and foremost, if this is a constant don't bother.
  if (var->isLet())
    return;

  // We can only provide default values for patterns binding a single variable.
  // i.e. var (a, b) = getSomeTuple() is not allowed.
  if (!var->getParentPattern()->getSingleVar())
    return;

  // Whether we have explicit initialization.
  bool isExplicitlyInitialized = var->isParentInitialized();

  // Whether we can default-initialize this property.
  auto binding = var->getParentPatternBinding();
  bool isDefaultInitializable =
      var->getAttrs().hasAttribute<LazyAttr>() ||
      (binding && binding->isDefaultInitializable());

  // If this is neither explicitly initialized nor
  // default-initializable, don't add anything.
  if (!isExplicitlyInitialized && !isDefaultInitializable)
    return;

  // We can add a default value now.

  // Give this some bogus context right now, we'll fix it after making
  // the constructor.
  auto *initDC = new (ctx) DefaultArgumentInitializer(
    arg->getDeclContext(), paramSize);

  defaultInits.push_back(initDC);

  // If the variable has a type T? and no initial value, return a nil literal
  // default arg. All lazy variables return a nil literal as well. *Note* that
  // the type will always be a sugared T? because we don't default init an
  // explicit Optional<T>.
  bool isNilInitialized =
    var->getAttrs().hasAttribute<LazyAttr>() ||
    (!isExplicitlyInitialized && isDefaultInitializable &&
     var->getValueInterfaceType()->getAnyNominal() == ctx.getOptionalDecl() &&
     !var->getAttachedPropertyWrapperTypeInfo(0).defaultInit);
  if (isNilInitialized) {
    arg->setDefaultArgumentKind(DefaultArgumentKind::NilLiteral);
    return;
  }

  // If there's a backing storage property, the memberwise initializer
  // will be in terms of that.
  VarDecl *backingStorageVar = var->getPropertyWrapperBackingProperty();

  // Set the default value to the variable. When we emit this in silgen
  // we're going to call the variable's initializer expression.
  arg->setStoredProperty(backingStorageVar ? backingStorageVar : var);
  arg->setDefaultArgumentKind(DefaultArgumentKind::StoredProperty);
}

/// Create an implicit struct or class constructor.
///
/// \param decl The struct or class for which a constructor will be created.
/// \param ICK The kind of implicit constructor to create.
///
/// \returns The newly-created constructor, which has already been type-checked
/// (but has not been added to the containing struct or class).
ConstructorDecl *swift::createImplicitConstructor(TypeChecker &tc,
                                                  NominalTypeDecl *decl,
                                                  ImplicitConstructorKind ICK) {
  assert(!decl->hasClangNode());

  ASTContext &ctx = tc.Context;
  SourceLoc Loc = decl->getLoc();
  auto accessLevel = AccessLevel::Internal;

  // Determine the parameter type of the implicit constructor.
  SmallVector<ParamDecl*, 8> params;
  SmallVector<DefaultArgumentInitializer *, 8> defaultInits;
  if (ICK == ImplicitConstructorKind::Memberwise) {
    assert(isa<StructDecl>(decl) && "Only struct have memberwise constructor");

    for (auto member : decl->getMembers()) {
      auto var = dyn_cast<VarDecl>(member);
      if (!var)
        continue;

      if (!var->isMemberwiseInitialized(/*preferDeclaredProperties=*/true))
        continue;

      accessLevel = std::min(accessLevel, var->getFormalAccess());

      tc.validateDecl(var);
      auto varInterfaceType = var->getValueInterfaceType();

      if (var->getAttrs().hasAttribute<LazyAttr>()) {
        // If var is a lazy property, its value is provided for the underlying
        // storage.  We thus take an optional of the property's type.  We only
        // need to do this because the implicit initializer is added before all
        // the properties are type checked.  Perhaps init() synth should be
        // moved later.
        varInterfaceType = OptionalType::get(varInterfaceType);
      } else if (Type backingPropertyType =
                     var->getPropertyWrapperBackingPropertyType()) {
        // For a property that has a wrapper, writing the initializer
        // with an '=' implies that the memberwise initializer should also
        // accept a value of the original property type. Otherwise, the
        // memberwise initializer will be in terms of the backing storage
        // type.
        if (!var->isPropertyMemberwiseInitializedWithWrappedType()) {
          varInterfaceType = backingPropertyType;
        }
      }

      // Create the parameter.
      auto *arg = new (ctx)
          ParamDecl(VarDecl::Specifier::Default, SourceLoc(), Loc,
                    var->getName(), Loc, var->getName(), decl);
      arg->setInterfaceType(varInterfaceType);
      arg->setImplicit();
      
      maybeAddMemberwiseDefaultArg(arg, var, defaultInits, params.size(), ctx);
      
      params.push_back(arg);
    }
  }

  auto paramList = ParameterList::create(ctx, params);
  
  // Create the constructor.
  DeclName name(ctx, DeclBaseName::createConstructor(), paramList);
  auto *ctor =
    new (ctx) ConstructorDecl(name, Loc,
                              OTK_None, /*FailabilityLoc=*/SourceLoc(),
                              /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                              paramList, /*GenericParams=*/nullptr, decl);

  // Mark implicit.
  ctor->setImplicit();
  ctor->setAccess(accessLevel);

  if (ICK == ImplicitConstructorKind::Memberwise) {
    ctor->setIsMemberwiseInitializer();

    // Fix default argument init contexts now that we have a constructor.
    for (auto initDC : defaultInits) {
      initDC->changeFunction(ctor, paramList);
    }
  }

  // If we are defining a default initializer for a class that has a superclass,
  // it overrides the default initializer of its superclass. Add an implicit
  // 'override' attribute.
  if (auto classDecl = dyn_cast<ClassDecl>(decl)) {
    if (classDecl->getSuperclass())
      ctor->getAttrs().add(new (ctx) OverrideAttr(/*IsImplicit=*/true));
  }

  return ctor;
}

/// Create a stub body that emits a fatal error message.
static std::pair<BraceStmt *, bool>
synthesizeStubBody(AbstractFunctionDecl *fn, void *) {
  auto *ctor = cast<ConstructorDecl>(fn);
  auto &ctx = ctor->getASTContext();

  auto unimplementedInitDecl = ctx.getUnimplementedInitializer();
  auto classDecl = ctor->getDeclContext()->getSelfClassDecl();
  if (!unimplementedInitDecl) {
    ctx.Diags.diagnose(classDecl->getLoc(),
                       diag::missing_unimplemented_init_runtime);
    return { nullptr, true };
  }

  auto *staticStringDecl = ctx.getStaticStringDecl();
  auto staticStringType = staticStringDecl->getDeclaredType();
  auto staticStringInit = ctx.getStringBuiltinInitDecl(staticStringDecl);

  auto *uintDecl = ctx.getUIntDecl();
  auto uintType = uintDecl->getDeclaredType();
  auto uintInit = ctx.getIntBuiltinInitDecl(uintDecl);

  // Create a call to Swift._unimplementedInitializer
  auto loc = classDecl->getLoc();
  Expr *ref = new (ctx) DeclRefExpr(unimplementedInitDecl,
                                   DeclNameLoc(loc),
                                   /*Implicit=*/true);
  ref->setType(unimplementedInitDecl->getInterfaceType()
                                    ->removeArgumentLabels(1));

  llvm::SmallString<64> buffer;
  StringRef fullClassName = ctx.AllocateCopy(
                              (classDecl->getModuleContext()->getName().str() +
                               "." +
                               classDecl->getName().str()).toStringRef(buffer));

  auto *className = new (ctx) StringLiteralExpr(fullClassName, loc,
                                                /*Implicit=*/true);
  className->setBuiltinInitializer(staticStringInit);
  assert(isa<ConstructorDecl>(className->getBuiltinInitializer().getDecl()));
  className->setType(staticStringType);

  auto *initName = new (ctx) MagicIdentifierLiteralExpr(
    MagicIdentifierLiteralExpr::Function, loc, /*Implicit=*/true);
  initName->setType(staticStringType);
  initName->setBuiltinInitializer(staticStringInit);

  auto *file = new (ctx) MagicIdentifierLiteralExpr(
    MagicIdentifierLiteralExpr::File, loc, /*Implicit=*/true);
  file->setType(staticStringType);
  file->setBuiltinInitializer(staticStringInit);

  auto *line = new (ctx) MagicIdentifierLiteralExpr(
    MagicIdentifierLiteralExpr::Line, loc, /*Implicit=*/true);
  line->setType(uintType);
  line->setBuiltinInitializer(uintInit);

  auto *column = new (ctx) MagicIdentifierLiteralExpr(
    MagicIdentifierLiteralExpr::Column, loc, /*Implicit=*/true);
  column->setType(uintType);
  column->setBuiltinInitializer(uintInit);

  auto *call = CallExpr::createImplicit(
      ctx, ref, { className, initName, file, line, column }, {});
  call->setType(ctx.getNeverType());
  call->setThrows(false);

  SmallVector<ASTNode, 2> stmts;
  stmts.push_back(call);
  stmts.push_back(new (ctx) ReturnStmt(SourceLoc(), /*Result=*/nullptr));
  return { BraceStmt::create(ctx, SourceLoc(), stmts, SourceLoc(),
                             /*implicit=*/true),
           /*isTypeChecked=*/true };
}

static std::tuple<GenericEnvironment *, GenericParamList *, SubstitutionMap>
configureGenericDesignatedInitOverride(ASTContext &ctx,
                                       ClassDecl *classDecl,
                                       Type superclassTy,
                                       ConstructorDecl *superclassCtor) {
  auto *superclassDecl = superclassTy->getAnyNominal();

  auto *moduleDecl = classDecl->getParentModule();
  auto subMap = superclassTy->getContextSubstitutionMap(
      moduleDecl, superclassDecl);

  GenericEnvironment *genericEnv;

  // Inheriting initializers that have their own generic parameters
  auto *genericParams = superclassCtor->getGenericParams();
  if (genericParams) {
    SmallVector<GenericTypeParamDecl *, 4> newParams;

    // First, clone the superclass constructor's generic parameter list,
    // but change the depth of the generic parameters to be one greater
    // than the depth of the subclass.
    unsigned depth = 0;
    if (auto *genericSig = classDecl->getGenericSignature())
      depth = genericSig->getGenericParams().back()->getDepth() + 1;

    for (auto *param : genericParams->getParams()) {
      auto *newParam = new (ctx) GenericTypeParamDecl(classDecl,
                                                      param->getName(),
                                                      SourceLoc(),
                                                      depth,
                                                      param->getIndex());
      newParams.push_back(newParam);
    }

    // We don't have to clone the requirements, because they're not
    // used for anything.
    genericParams = GenericParamList::create(ctx,
                                             SourceLoc(),
                                             newParams,
                                             SourceLoc(),
                                             ArrayRef<RequirementRepr>(),
                                             SourceLoc());

    // Build a generic signature for the derived class initializer.
    GenericSignatureBuilder builder(ctx);
    builder.addGenericSignature(classDecl->getGenericSignature());

    // Add the generic parameters.
    for (auto *newParam : newParams)
      builder.addGenericParameter(newParam);

    auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
    auto *superclassSig = superclassCtor->getGenericSignature();

    unsigned superclassDepth = 0;
    if (auto *genericSig = superclassDecl->getGenericSignature())
      superclassDepth = genericSig->getGenericParams().back()->getDepth() + 1;

    // We're going to be substituting the requirements of the base class
    // initializer to form the requirements of the derived class initializer.
    auto substFn = [&](SubstitutableType *type) -> Type {
      auto *gp = cast<GenericTypeParamType>(type);
      if (gp->getDepth() < superclassDepth)
        return Type(gp).subst(subMap);
      return CanGenericTypeParamType::get(
        gp->getDepth() - superclassDepth + depth,
          gp->getIndex(),
          ctx);
    };

    auto lookupConformanceFn =
      [&](CanType depTy, Type substTy, ProtocolDecl *proto)
        -> Optional<ProtocolConformanceRef> {
      if (auto conf = subMap.lookupConformance(depTy, proto))
        return conf;

      return ProtocolConformanceRef(proto);
    };

    for (auto reqt : superclassSig->getRequirements())
      if (auto substReqt = reqt.subst(substFn, lookupConformanceFn))
        builder.addRequirement(*substReqt, source, nullptr);

    // Now form the substitution map that will be used to remap parameter
    // types.
    subMap = SubstitutionMap::get(superclassSig,
                                  substFn, lookupConformanceFn);

    auto *genericSig = std::move(builder).computeGenericSignature(SourceLoc());
    genericEnv = genericSig->createGenericEnvironment();
  } else {
    genericEnv = classDecl->getGenericEnvironment();
  }

  return std::make_tuple(genericEnv, genericParams, subMap);
}

static void
configureInheritedDesignatedInitAttributes(TypeChecker &tc,
                                           ClassDecl *classDecl,
                                           ConstructorDecl *ctor,
                                           ConstructorDecl *superclassCtor) {
  assert(ctor->getDeclContext() == classDecl);
  auto &ctx = tc.Context;

  AccessLevel access = classDecl->getFormalAccess();
  access = std::max(access, AccessLevel::Internal);
  access = std::min(access, superclassCtor->getFormalAccess());

  ctor->setAccess(access);

  AccessScope superclassInliningAccessScope =
      superclassCtor->getFormalAccessScope(/*useDC*/nullptr,
                                           /*usableFromInlineAsPublic=*/true);

  if (superclassInliningAccessScope.isPublic()) {
    if (superclassCtor->getAttrs().hasAttribute<InlinableAttr>()) {
      // Inherit the @inlinable attribute.
      auto *clonedAttr = new (ctx) InlinableAttr(/*implicit=*/true);
      ctor->getAttrs().add(clonedAttr);

    } else if (access == AccessLevel::Internal && !superclassCtor->isDynamic()){
      // Inherit the @usableFromInline attribute.
      auto *clonedAttr = new (ctx) UsableFromInlineAttr(/*implicit=*/true);
      ctor->getAttrs().add(clonedAttr);
    }
  }

  // Inherit the @discardableResult attribute.
  if (superclassCtor->getAttrs().hasAttribute<DiscardableResultAttr>()) {
    auto *clonedAttr = new (ctx) DiscardableResultAttr(/*implicit=*/true);
    ctor->getAttrs().add(clonedAttr);
  }

  // If the superclass has its own availability, make sure the synthesized
  // constructor is only as available as its superclass's constructor.
  if (superclassCtor->getAttrs().hasAttribute<AvailableAttr>()) {
    SmallVector<Decl *, 2> asAvailableAs;

    // We don't have to look at enclosing contexts of the superclass constructor,
    // because designated initializers must always be defined in the superclass
    // body, and we already enforce that a superclass is at least as available as
    // a subclass.
    asAvailableAs.push_back(superclassCtor);
    Decl *parentDecl = classDecl;
    while (parentDecl != nullptr) {
      asAvailableAs.push_back(parentDecl);
      parentDecl = parentDecl->getDeclContext()->getAsDecl();
    }
    AvailabilityInference::applyInferredAvailableAttrs(
        ctor, asAvailableAs, ctx);
  }

  // Wire up the overrides.
  ctor->setOverriddenDecl(superclassCtor);

  if (superclassCtor->isRequired())
    ctor->getAttrs().add(new (ctx) RequiredAttr(/*IsImplicit=*/false));
  else
    ctor->getAttrs().add(new (ctx) OverrideAttr(/*IsImplicit=*/false));

  // If the superclass constructor is @objc but the subclass constructor is
  // not representable in Objective-C, add @nonobjc implicitly.
  Optional<ForeignErrorConvention> errorConvention;
  if (superclassCtor->isObjC() &&
      !isRepresentableInObjC(ctor, ObjCReason::MemberOfObjCSubclass,
                             errorConvention))
    ctor->getAttrs().add(new (ctx) NonObjCAttr(/*isImplicit=*/true));
}

static std::pair<BraceStmt *, bool>
synthesizeDesignatedInitOverride(AbstractFunctionDecl *fn, void *context) {
  auto *ctor = cast<ConstructorDecl>(fn);
  auto &ctx = ctor->getASTContext();

  auto *superclassCtor = (ConstructorDecl *) context;

  if (!superclassCtor->hasValidSignature())
    ctx.getLazyResolver()->resolveDeclSignature(superclassCtor);

  // Reference to super.init.
  auto *selfDecl = ctor->getImplicitSelfDecl();
  auto *superRef = buildSelfReference(selfDecl, SelfAccessorKind::Super,
                                      /*isLValue=*/false, ctx);

  SubstitutionMap subs;
  if (auto *genericEnv = fn->getGenericEnvironment())
    subs = genericEnv->getForwardingSubstitutionMap();
  subs = SubstitutionMap::getOverrideSubstitutions(superclassCtor, fn, subs);
  ConcreteDeclRef ctorRef(superclassCtor, subs);

  auto type = superclassCtor->getInitializerInterfaceType()
      .subst(subs, SubstFlags::UseErrorType);
  auto *ctorRefExpr =
      new (ctx) OtherConstructorDeclRefExpr(ctorRef, DeclNameLoc(),
                                            IsImplicit, type);

  if (auto *funcTy = type->getAs<FunctionType>())
    type = funcTy->getResult();
  auto *superclassCtorRefExpr =
      new (ctx) DotSyntaxCallExpr(ctorRefExpr, SourceLoc(), superRef, type);
  superclassCtorRefExpr->setIsSuper(true);
  superclassCtorRefExpr->setThrows(false);

  auto *bodyParams = ctor->getParameters();
  auto ctorArgs = buildArgumentForwardingExpr(bodyParams->getArray(), ctx);
  auto *superclassCallExpr =
    CallExpr::create(ctx, superclassCtorRefExpr, ctorArgs,
                     superclassCtor->getFullName().getArgumentNames(), { },
                     /*hasTrailingClosure=*/false, /*implicit=*/true);

  if (auto *funcTy = type->getAs<FunctionType>())
    type = funcTy->getResult();
  superclassCallExpr->setType(type);
  superclassCallExpr->setThrows(superclassCtor->hasThrows());

  Expr *expr = superclassCallExpr;

  if (superclassCtor->hasThrows()) {
    expr = new (ctx) TryExpr(SourceLoc(), expr, type, /*implicit=*/true);
  }

  auto *rebindSelfExpr =
    new (ctx) RebindSelfInConstructorExpr(expr, selfDecl);

  SmallVector<ASTNode, 2> stmts;
  stmts.push_back(rebindSelfExpr);
  stmts.push_back(new (ctx) ReturnStmt(SourceLoc(), /*Result=*/nullptr));
  return { BraceStmt::create(ctx, SourceLoc(), stmts, SourceLoc(),
                            /*implicit=*/true),
           /*isTypeChecked=*/true };
}

ConstructorDecl *
swift::createDesignatedInitOverride(TypeChecker &tc,
                                    ClassDecl *classDecl,
                                    ConstructorDecl *superclassCtor,
                                    DesignatedInitKind kind) {
  auto &ctx = tc.Context;

  // Lookup will sometimes give us initializers that are from the ancestors of
  // our immediate superclass.  So, from the superclass constructor, we look
  // one level up to the enclosing type context which will either be a class
  // or an extension.  We can use the type declared in that context to check
  // if it's our immediate superclass and give up if we didn't.
  //
  // FIXME: Remove this when lookup of initializers becomes restricted to our
  // immediate superclass.
  auto *superclassCtorDecl =
      superclassCtor->getDeclContext()->getSelfNominalTypeDecl();
  Type superclassTy = classDecl->getSuperclass();
  NominalTypeDecl *superclassDecl = superclassTy->getAnyNominal();
  if (superclassCtorDecl != superclassDecl) {
    return nullptr;
  }

  GenericEnvironment *genericEnv;
  GenericParamList *genericParams;
  SubstitutionMap subMap;

  std::tie(genericEnv, genericParams, subMap) =
      configureGenericDesignatedInitOverride(ctx,
                                             classDecl,
                                             superclassTy,
                                             superclassCtor);

  // Determine the initializer parameters.

  // Create the initializer parameter patterns.
  OptionSet<ParameterList::CloneFlags> options = ParameterList::Implicit;
  options |= ParameterList::Inherited;
  auto *bodyParams = superclassCtor->getParameters()->clone(ctx, options);

  // If the superclass is generic, we need to map the superclass constructor's
  // parameter types into the generic context of our class.
  //
  // We might have to apply substitutions, if for example we have a declaration
  // like 'class A : B<Int>'.
  for (auto *decl : *bodyParams) {
    auto paramTy = decl->getInterfaceType();
    auto substTy = paramTy.subst(subMap, SubstFlags::UseErrorType);
    decl->setInterfaceType(substTy);
    decl->getTypeLoc() = TypeLoc::withoutLoc(substTy);
  }

  // Create the initializer declaration, inheriting the name,
  // failability, and throws from the superclass initializer.
  auto ctor =
    new (ctx) ConstructorDecl(superclassCtor->getFullName(),
                              classDecl->getBraces().Start,
                              superclassCtor->getFailability(),
                              /*FailabilityLoc=*/SourceLoc(),
                              /*Throws=*/superclassCtor->hasThrows(),
                              /*ThrowsLoc=*/SourceLoc(),
                              bodyParams, genericParams, classDecl);

  ctor->setImplicit();

  // Set the interface type of the initializer.
  ctor->setGenericEnvironment(genericEnv);
  ctor->computeType();

  if (ctor->getFailability() == OTK_ImplicitlyUnwrappedOptional) {
    ctor->getAttrs().add(
      new (ctx) ImplicitlyUnwrappedOptionalAttr(/*implicit=*/true));
  }

  ctor->setValidationToChecked();

  configureInheritedDesignatedInitAttributes(tc, classDecl, ctor,
                                             superclassCtor);

  if (kind == DesignatedInitKind::Stub) {
    // Make this a stub implementation.
    ctor->setBodySynthesizer(synthesizeStubBody);

    // Note that this is a stub implementation.
    ctor->setStubImplementation(true);

    // Stub constructors don't appear in the vtable.
    ctor->setNeedsNewVTableEntry(false);
    return ctor;
  }

  // Form the body of a chaining designated initializer.
  assert(kind == DesignatedInitKind::Chaining);
  ctor->setBodySynthesizer(synthesizeDesignatedInitOverride, superclassCtor);

  return ctor;
}
