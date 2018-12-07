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
#include "swift/AST/ProtocolConformance.h"
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
    assert((isa<AbstractFunctionDecl>(DC) || isa<FileUnit>(DC)) &&
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
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->isStatic())
      staticLoc = var->getLoc();
  }

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

  // Accessors for resilient properties are not @_transparent.
  if (accessor->getStorage()->isResilient())
    return;

  // Setters for lazy properties are not @_transparent (because the storage
  // is not ABI-exposed).
  if (accessor->getStorage()->getAttrs().hasAttribute<LazyAttr>() &&
      accessor->getAccessorKind() == AccessorKind::Set)
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
    if (classDecl->checkObjCAncestry() != ObjCClassKind::NonObjC)
      return;

  // Accessors synthesized on-demand are never transaprent.
  if (accessor->hasForcedStaticDispatch())
    return;

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

  if (isStatic)
    accessor->setStatic();

  // The accessor is final if the storage is.
  if (storage->isFinal())
    makeFinal(ctx, accessor);

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

  maybeMarkTransparent(accessor, ctx);

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
/// or paren expr, suitable for use in an applyexpr.
///
/// NOTE: This returns null if a varargs parameter exists in the list, as it
/// cannot be forwarded correctly yet.
///
static Expr *buildArgumentForwardingExpr(ArrayRef<ParamDecl*> params,
                                         ASTContext &ctx) {
  SmallVector<Identifier, 4> labels;
  SmallVector<SourceLoc, 4> labelLocs;
  SmallVector<Expr *, 4> args;
  
  for (auto param : params) {
    Expr *ref = new (ctx) DeclRefExpr(param, DeclNameLoc(), /*implicit*/ true);
    if (param->isInOut())
      ref = new (ctx) InOutExpr(SourceLoc(), ref, Type(), /*isImplicit=*/true);
    else if (param->isVariadic())
      ref = new (ctx) VarargExpansionExpr(ref, /*implicit*/ true);
    else if (param->isAutoClosure()) {
      // If parameter is marked as `@autoclosure` it means
      // that it has to be called.
      auto arg = TupleExpr::createEmpty(ctx, SourceLoc(), SourceLoc(),
                                        /*implicit=*/true);
      ref = CallExpr::create(ctx, ref, arg, {}, {},
                             /*hasTrailingClosure=*/false,
                             /*implicit=*/true);
    }

    args.push_back(ref);
    
    labels.push_back(param->getArgumentName());
    labelLocs.push_back(SourceLoc());
  }
  
  // A single unlabeled value is not a tuple.
  if (args.size() == 1 && labels[0].empty()) {
    return new (ctx) ParenExpr(SourceLoc(), args[0], SourceLoc(),
                               /*hasTrailingClosure=*/false);
  }
  
  return TupleExpr::create(ctx, SourceLoc(), args, labels, labelLocs,
                           SourceLoc(), false, IsImplicit);
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
  auto result = buildArgumentForwardingExpr(params, ctx);
  assert(result && "FIXME: Cannot forward expression");
  return result;
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
                                ASTContext &ctx) {
  switch (selfAccessorKind) {
  case SelfAccessorKind::Peer:
    return new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(), IsImplicit);

  case SelfAccessorKind::Super:
    return new (ctx) SuperRefExpr(selfDecl, SourceLoc(), IsImplicit);
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
    Super
  };
} // end anonymous namespace

/// Build an l-value for the storage of a declaration.
static Expr *buildStorageReference(AccessorDecl *accessor,
                                   AbstractStorageDecl *storage,
                                   TargetImpl target,
                                   ASTContext &ctx) {
  AccessSemantics semantics;
  SelfAccessorKind selfAccessKind;
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
      storage = override;

    // Otherwise do a self-reference, which is dynamically bogus but
    // should be statically valid.  This should only happen in invalid cases.    
    } else {
      assert(storage->isInvalid());
      semantics = AccessSemantics::Ordinary;
      selfAccessKind = SelfAccessorKind::Peer;
    }
    break;
  }

  VarDecl *selfDecl = accessor->getImplicitSelfDecl();
  if (!selfDecl) {
    assert(target != TargetImpl::Super);
    return new (ctx) DeclRefExpr(storage, DeclNameLoc(), IsImplicit, semantics);
  }

  Expr *selfDRE =
    buildSelfReference(selfDecl, selfAccessKind, ctx);

  if (auto subscript = dyn_cast<SubscriptDecl>(storage)) {
    Expr *indices = buildSubscriptIndexReference(ctx, accessor);
    return SubscriptExpr::create(ctx, selfDRE, indices, storage,
                                 IsImplicit, semantics);
  }

  return new (ctx) MemberRefExpr(selfDRE, SourceLoc(), storage,
                                 DeclNameLoc(), IsImplicit, semantics);
}

/// Load the value of VD.  If VD is an @override of another value, we call the
/// superclass getter.  Otherwise, we do a direct load of the value.
static Expr *
createPropertyLoadOrCallSuperclassGetter(AccessorDecl *accessor,
                                         AbstractStorageDecl *storage,
                                         TargetImpl target,
                                         ASTContext &ctx) {
  return buildStorageReference(accessor, storage, target, ctx);
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

static bool checkConformanceToNSCopying(ASTContext &ctx, VarDecl *var,
                                        Type type) {
  auto dc = var->getDeclContext();
  auto proto = getNSCopyingProtocol(ctx, dc);

  // FIXME: Remove this usage of TypeChecker
  auto &TC = *(TypeChecker *) ctx.getLazyResolver();
  if (!proto || !TC.conformsToProtocol(type, proto, dc, None)) {
    TC.diagnose(var->getLoc(), diag::nscopying_doesnt_conform);
    return true;
  }
  return false;
}

static std::pair<Type, bool> getUnderlyingTypeOfVariable(VarDecl *var) {
  Type type = var->getType()->getReferenceStorageReferent();

  if (Type objectType = type->getOptionalObjectType()) {
    return {objectType, true};
  } else {
    return {type, false};
  }
}

bool TypeChecker::checkConformanceToNSCopying(VarDecl *var) {
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
  if (checkConformanceToNSCopying(Ctx, VD, underlyingType)) {
    return Val;
  }

  // If we have an optional type, we have to "?" the incoming value to only
  // evaluate the subexpression if the incoming value is non-null.
  if (isOptional)
    Val = new (Ctx) BindOptionalExpr(Val, SourceLoc(), 0);

  // Generate:
  // (force_value_expr type='<null>'
  //   (call_expr type='<null>'
  //     (unresolved_dot_expr type='<null>' field 'copy'
  //       "Val")
  //     (paren_expr type='<null>'
  //       (nil_literal_expr type='<null>'))))
  auto UDE = new (Ctx) UnresolvedDotExpr(Val, SourceLoc(),
                                         Ctx.getIdentifier("copy"),
                                         DeclNameLoc(), /*implicit*/true);
  Expr *Nil = new (Ctx) NilLiteralExpr(SourceLoc(), /*implicit*/true);

  //- (id)copyWithZone:(NSZone *)zone;
  Expr *Call = CallExpr::createImplicit(Ctx, UDE, { Nil }, { Ctx.Id_with });

  TypeLoc ResultTy;
  ResultTy.setType(VD->getType());

  // If we're working with non-optional types, we're forcing the cast.
  if (!isOptional) {
    Call = new (Ctx) ForcedCheckedCastExpr(Call, SourceLoc(), SourceLoc(),
                                           TypeLoc::withoutLoc(underlyingType));
    Call->setImplicit();
    return Call;
  }

  // We're working with optional types, so perform a conditional checked
  // downcast.
  Call = new (Ctx) ConditionalCheckedCastExpr(Call, SourceLoc(), SourceLoc(),
                                           TypeLoc::withoutLoc(underlyingType));
  Call->setImplicit();

  // Use OptionalEvaluationExpr to evaluate the "?".
  return new (Ctx) OptionalEvaluationExpr(Call);
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

  // Create:
  //   (assign (decl_ref_expr(VD)), decl_ref_expr(value))
  // or:
  //   (assign (member_ref_expr(decl_ref_expr(self), VD)), decl_ref_expr(value))
  Expr *dest = buildStorageReference(accessor, storage, target, ctx);

  body.push_back(new (ctx) AssignExpr(dest, SourceLoc(), value,
                                      IsImplicit));
}

LLVM_ATTRIBUTE_UNUSED
static bool isSynthesizedComputedProperty(AbstractStorageDecl *storage) {
  return (storage->getAttrs().hasAttribute<LazyAttr>() ||
          storage->getAttrs().hasAttribute<NSManagedAttr>());
}

/// Synthesize the body of a trivial getter.  For a non-member vardecl or one
/// which is not an override of a base class property, it performs a direct
/// storage load.  For an override of a base member property, it chains up to
/// super.
static void synthesizeTrivialGetterBody(AccessorDecl *getter,
                                        TargetImpl target,
                                        ASTContext &ctx) {
  auto storage = getter->getStorage();
  assert(!storage->getAttrs().hasAttribute<LazyAttr>() &&
         !storage->getAttrs().hasAttribute<NSManagedAttr>());

  SourceLoc loc = storage->getLoc();

  Expr *result =
    createPropertyLoadOrCallSuperclassGetter(getter, storage, target, ctx);
  ASTNode returnStmt = new (ctx) ReturnStmt(SourceLoc(), result, IsImplicit);

  getter->setBody(BraceStmt::create(ctx, loc, returnStmt, loc, true));

  maybeMarkTransparent(getter, ctx);
}

/// Synthesize the body of a getter which just directly accesses the
/// underlying storage.
static void synthesizeTrivialGetterBody(AccessorDecl *getter,
                                        ASTContext &ctx) {
  assert(getter->getStorage()->hasStorage());
  synthesizeTrivialGetterBody(getter, TargetImpl::Storage, ctx);
}

/// Synthesize the body of a getter which just delegates to its superclass
/// implementation.
static void synthesizeInheritedGetterBody(AccessorDecl *getter,
                                          ASTContext &ctx) {
  // This should call the superclass getter.
  synthesizeTrivialGetterBody(getter, TargetImpl::Super, ctx);
}

/// Synthesize the body of a getter which just delegates to an addressor.
static void synthesizeAddressedGetterBody(AccessorDecl *getter,
                                          ASTContext &ctx) {
  assert(getter->getStorage()->getAddressor());

  // This should call the addressor.
  synthesizeTrivialGetterBody(getter, TargetImpl::Implementation, ctx);
}

/// Synthesize the body of a getter which just delegates to a read
/// coroutine accessor.
static void synthesizeReadCoroutineGetterBody(AccessorDecl *getter,
                                              ASTContext &ctx) {
  assert(getter->getStorage()->getReadCoroutine());

  // This should call the read coroutine.
  synthesizeTrivialGetterBody(getter, TargetImpl::Implementation, ctx);
}

/// Synthesize the body of a setter which just stores to the given storage
/// declaration (which doesn't have to be the storage for the setter).
static void
synthesizeTrivialSetterBodyWithStorage(AccessorDecl *setter,
                                       TargetImpl target,
                                       AbstractStorageDecl *storageToUse,
                                       ASTContext &ctx) {
  SourceLoc loc = setter->getStorage()->getLoc();

  VarDecl *valueParamDecl = getFirstParamDecl(setter);

  auto *valueDRE =
    new (ctx) DeclRefExpr(valueParamDecl, DeclNameLoc(), IsImplicit);
  SmallVector<ASTNode, 1> setterBody;

  createPropertyStoreOrCallSuperclassSetter(setter, valueDRE, storageToUse,
                                            target, setterBody, ctx);
  setter->setBody(BraceStmt::create(ctx, loc, setterBody, loc, true));

  maybeMarkTransparent(setter, ctx);
}

static void synthesizeTrivialSetterBody(AccessorDecl *setter,
                                        ASTContext &ctx) {
  auto storage = setter->getStorage();
  assert(!isSynthesizedComputedProperty(storage));
  synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Storage,
                                         storage, ctx);
}

static void synthesizeCoroutineAccessorBody(AccessorDecl *accessor,
                                            ASTContext &ctx) {
  assert(accessor->isCoroutine());

  auto storage = accessor->getStorage();
  auto target = (accessor->hasForcedStaticDispatch()
                   ? TargetImpl::Ordinary
                   : TargetImpl::Implementation);

  SourceLoc loc = storage->getLoc();
  SmallVector<ASTNode, 1> body;

  // Build a reference to the storage.
  Expr *ref = buildStorageReference(accessor, storage, target, ctx);

  // Wrap it with an `&` marker if this is a modify.
  if (accessor->getAccessorKind() == AccessorKind::Modify) {
    ref = new (ctx) InOutExpr(SourceLoc(), ref, Type(), true);
  }

  // Yield it.
  YieldStmt *yield = YieldStmt::create(ctx, loc, loc, ref, loc, true);
  body.push_back(yield);

  accessor->setBody(BraceStmt::create(ctx, loc, body, loc, true));

  maybeMarkTransparent(accessor, ctx);
}

/// Synthesize the body of a read coroutine.
static void synthesizeReadCoroutineBody(AccessorDecl *read,
                                        ASTContext &ctx) {
  assert(read->getStorage()->getReadImpl() != ReadImplKind::Read);
  synthesizeCoroutineAccessorBody(read, ctx);
}

/// Synthesize the body of a modify coroutine.
static void synthesizeModifyCoroutineBody(AccessorDecl *modify,
                                          ASTContext &ctx) {
#ifndef NDEBUG
  auto impl = modify->getStorage()->getReadWriteImpl();
  assert(impl != ReadWriteImplKind::Modify &&
         impl != ReadWriteImplKind::Immutable);
#endif
  synthesizeCoroutineAccessorBody(modify, ctx);
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

/// Add trivial accessors to a Stored or Addressed property.
static void addTrivialAccessorsToStorage(AbstractStorageDecl *storage,
                                         ASTContext &ctx) {
  assert(!isSynthesizedComputedProperty(storage));
  addExpectedOpaqueAccessorsToStorage(storage, ctx);
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
static void setProtocolStorageImpl(AbstractStorageDecl *storage,
                                   ASTContext &ctx) {
  addExpectedOpaqueAccessorsToStorage(storage, ctx);

  storage->overwriteImplInfo(getProtocolStorageImpl(storage));
}

/// Synthesize the body of a setter which just delegates to a mutable
/// addressor.
static void synthesizeMutableAddressSetterBody(AccessorDecl *setter,
                                               ASTContext &ctx) {
  // This should call the mutable addressor.
  synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Implementation,
                                         setter->getStorage(), ctx);
}

/// Synthesize the body of a setter which just delegates to a modify
/// coroutine accessor.
static void synthesizeModifyCoroutineSetterBody(AccessorDecl *setter,
                                                ASTContext &ctx) {
  // This should call the modify coroutine.
  synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Implementation,
                                         setter->getStorage(), ctx);
}

static void convertNSManagedStoredVarToComputed(VarDecl *VD, ASTContext &ctx) {
  // If it's not still stored, just bail out.
  if (!VD->getImplInfo().isSimpleStored())
    return;

  // We might already have synthesized the getter and setter declarations
  // from e.g. type-checking a conformance, or just from an invalid earlier
  // declaration.

  // Creating these this way will not trigger synthesis of implementations
  // because of the NSManaged attribute.

  // Create the getter.
  if (!VD->getGetter()) {
    addGetterToStorage(VD, ctx);
  }

  // Create the setter.
  if (!VD->getSetter()) {
    addSetterToStorage(VD, ctx);
  }

  // Okay, we have both a getter and setter; overwrite the impl info.
  VD->overwriteImplInfo(StorageImplInfo::getMutableComputed());

  addExpectedOpaqueAccessorsToStorage(VD, ctx);
}

void synthesizeAccessorBody(AbstractFunctionDecl *fn, void *);

/// The specified AbstractStorageDecl was just found to satisfy a
/// protocol property requirement.  Ensure that it has the full
/// complement of accessors.
void TypeChecker::synthesizeWitnessAccessorsForStorage(
                                             AbstractStorageDecl *requirement,
                                             AbstractStorageDecl *storage) {
  bool addedAccessor = false;

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

      // Make sure SILGen emits the accessor; on-demand accessors have shared
      // linkage, and if its defined in a different translation unit from the
      // conformance we cannot simply generate an external declaration.
      Context.addExternalDecl(accessor);
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
static void synthesizeObservedSetterBody(AccessorDecl *Set,
                                         TargetImpl target,
                                         ASTContext &Ctx) {
  auto VD = cast<VarDecl>(Set->getStorage());

  SourceLoc Loc = VD->getLoc();

  // We have to be paranoid about the accessors already having bodies
  // because there might be an (invalid) existing definition.
  
  // Okay, the getter is done, create the setter now.  Start by finding the
  // decls for 'self' and 'value'.
  auto *SelfDecl = Set->getImplicitSelfDecl();
  VarDecl *ValueDecl = Set->getParameters()->get(0);

  // The setter loads the oldValue, invokes willSet with the incoming value,
  // does a direct store, then invokes didSet with the oldValue.
  SmallVector<ASTNode, 6> SetterBody;

  // If there is a didSet, it will take the old value.  Load it into a temporary
  // 'let' so we have it for later.
  // TODO: check the body of didSet to only do this load (which may call the
  // superclass getter) if didSet takes an argument.
  VarDecl *OldValue = nullptr;
  if (VD->getDidSetFunc()) {
    Expr *OldValueExpr
      = createPropertyLoadOrCallSuperclassGetter(Set, VD, target, Ctx);

    OldValue = new (Ctx) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Let,
                                 /*IsCaptureList*/false, SourceLoc(),
                                 Ctx.getIdentifier("tmp"), Set);
    OldValue->setImplicit();
    auto *tmpPattern = new (Ctx) NamedPattern(OldValue, /*implicit*/ true);
    auto *tmpPBD = PatternBindingDecl::createImplicit(
        Ctx, StaticSpellingKind::None, tmpPattern, OldValueExpr, Set);
    SetterBody.push_back(tmpPBD);
    SetterBody.push_back(OldValue);
  }

  // Create:
  //   (call_expr (dot_syntax_call_expr (decl_ref_expr(willSet)),
  //                                    (decl_ref_expr(self))),
  //              (declrefexpr(value)))
  // or:
  //   (call_expr (decl_ref_expr(willSet)), (declrefexpr(value)))
  if (auto willSet = VD->getWillSetFunc()) {
    Expr *Callee = new (Ctx) DeclRefExpr(willSet, DeclNameLoc(), /*imp*/true);
    auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, DeclNameLoc(),
                                           /*imp*/true);
    if (SelfDecl) {
      auto *SelfDRE = new (Ctx) DeclRefExpr(SelfDecl, DeclNameLoc(),
                                            /*imp*/true);
      Callee = new (Ctx) DotSyntaxCallExpr(Callee, SourceLoc(), SelfDRE);
    }
    SetterBody.push_back(CallExpr::createImplicit(Ctx, Callee, { ValueDRE },
                                                  { Identifier() }));
  }
  
  // Create an assignment into the storage or call to superclass setter.
  auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, DeclNameLoc(), true);
  createPropertyStoreOrCallSuperclassSetter(Set, ValueDRE, VD, target,
                                            SetterBody, Ctx);

  // Create:
  //   (call_expr (dot_syntax_call_expr (decl_ref_expr(didSet)),
  //                                    (decl_ref_expr(self))),
  //              (decl_ref_expr(tmp)))
  // or:
  //   (call_expr (decl_ref_expr(didSet)), (decl_ref_expr(tmp)))
  if (auto didSet = VD->getDidSetFunc()) {
    auto *OldValueExpr = new (Ctx) DeclRefExpr(OldValue, DeclNameLoc(),
                                               /*impl*/true);
    Expr *Callee = new (Ctx) DeclRefExpr(didSet, DeclNameLoc(), /*imp*/true);
    if (SelfDecl) {
      auto *SelfDRE = new (Ctx) DeclRefExpr(SelfDecl, DeclNameLoc(),
                                            /*imp*/true);
      Callee = new (Ctx) DotSyntaxCallExpr(Callee, SourceLoc(), SelfDRE);
    }
    SetterBody.push_back(CallExpr::createImplicit(Ctx, Callee, { OldValueExpr },
                                                  { Identifier() }));
  }

  Set->setBody(BraceStmt::create(Ctx, Loc, SetterBody, Loc, true));
}

static void synthesizeStoredWithObserversSetterBody(AccessorDecl *setter,
                                                    ASTContext &ctx) {
  synthesizeObservedSetterBody(setter, TargetImpl::Storage, ctx);
}

static void synthesizeInheritedWithObserversSetterBody(AccessorDecl *setter,
                                                      ASTContext &ctx) {
  synthesizeObservedSetterBody(setter, TargetImpl::Super, ctx);
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
static void synthesizeLazyGetterBody(AbstractFunctionDecl *fn, void *context) {
  auto &Ctx = fn->getASTContext();

  // FIXME: Remove TypeChecker dependencies below.
  auto &TC = *(TypeChecker *) Ctx.getLazyResolver();

  // The stored property backing the lazy var.
  AccessorDecl *Get = cast<AccessorDecl>(fn);
  VarDecl *Storage = (VarDecl *) context;

  // The lazy var itself.
  auto VD = cast<VarDecl>(Get->getStorage());

  if (Get->isInvalid() || Ctx.hadError())
    return;

  // The getter checks the optional, storing the initial value in if nil.  The
  // specific pattern we generate is:
  //   get {
  //     let tmp1 = storage
  //     if tmp1 {
  //       return tmp1!
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
  Tmp1VD->setImplicit();

  auto *Tmp1PBDPattern = new (Ctx) NamedPattern(Tmp1VD, /*implicit*/true);
  auto *Tmp1Init =
    createPropertyLoadOrCallSuperclassGetter(Get, Storage,
                                             TargetImpl::Storage, Ctx);
  auto *Tmp1PBD = PatternBindingDecl::createImplicit(
      Ctx, StaticSpellingKind::None, Tmp1PBDPattern, Tmp1Init, Get);
  Body.push_back(Tmp1PBD);
  Body.push_back(Tmp1VD);

  // Build the early return inside the if.
  auto *Tmp1DRE = new (Ctx) DeclRefExpr(Tmp1VD, DeclNameLoc(), /*Implicit*/true,
                                        AccessSemantics::DirectToStorage);
  auto *EarlyReturnVal = new (Ctx) ForceValueExpr(Tmp1DRE, SourceLoc());
  auto *Return = new (Ctx) ReturnStmt(SourceLoc(), EarlyReturnVal,
                                      /*implicit*/true);

  // Build the "if" around the early return.
  Tmp1DRE = new (Ctx) DeclRefExpr(Tmp1VD, DeclNameLoc(), /*Implicit*/true,
                                  AccessSemantics::DirectToStorage);
  
  // Call through "hasValue" on the decl ref.
  Tmp1DRE->setType(OptionalType::get(VD->getType()));
  constraints::ConstraintSystem cs(TC,
                                   VD->getDeclContext(),
                                   constraints::ConstraintSystemOptions());
  constraints::Solution solution(cs, constraints::Score());
  auto HasValueExpr = solution.convertOptionalToBool(Tmp1DRE, nullptr);

  Body.push_back(new (Ctx) IfStmt(SourceLoc(), HasValueExpr, Return,
                                  /*elseloc*/SourceLoc(), /*else*/nullptr,
                                  /*implicit*/ true, Ctx));


  auto *Tmp2VD = new (Ctx) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Let,
                                   /*IsCaptureList*/false, SourceLoc(),
                                   Ctx.getIdentifier("tmp2"),
                                   Get);
  Tmp2VD->setType(VD->getType());
  Tmp2VD->setInterfaceType(VD->getInterfaceType());
  Tmp2VD->setImplicit();


  // Take the initializer from the PatternBindingDecl for VD.
  // TODO: This doesn't work with complicated patterns like:
  //   lazy var (a,b) = foo()
  auto *InitValue = VD->getParentInitializer();
  auto PBD = VD->getParentPatternBinding();
  unsigned entryIndex = PBD->getPatternEntryIndexForVarDecl(VD);
  assert(PBD->isInitializerLazy(entryIndex));
  bool wasInitializerChecked = PBD->isInitializerChecked(entryIndex);
  PBD->setInitializerChecked(entryIndex);

  // Recontextualize any closure declcontexts nested in the initializer to
  // realize that they are in the getter function.
  Get->getImplicitSelfDecl()->setDeclContext(Get);
  InitValue->walk(RecontextualizeClosures(Get));

  // Wrap the initializer in a LazyInitializerExpr to avoid problems with
  // re-typechecking it if it was already type-checked.
  // FIXME: we should really have stronger invariants than this.  Leaving it
  // unwrapped may expose both expressions to naive walkers
  if (wasInitializerChecked) {
    InitValue = new (Ctx) LazyInitializerExpr(InitValue);
  }

  Pattern *Tmp2PBDPattern = new (Ctx) NamedPattern(Tmp2VD, /*implicit*/true);
  Tmp2PBDPattern =
    TypedPattern::createImplicit(Ctx, Tmp2PBDPattern, VD->getType());

  auto *Tmp2PBD = PatternBindingDecl::createImplicit(
      Ctx, StaticSpellingKind::None, Tmp2PBDPattern, InitValue, Get,
      /*VarLoc*/ InitValue->getStartLoc());
  Body.push_back(Tmp2PBD);
  Body.push_back(Tmp2VD);

  // Assign tmp2 into storage.
  auto Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, DeclNameLoc(), /*Implicit*/true,
                                       AccessSemantics::DirectToStorage);
  createPropertyStoreOrCallSuperclassSetter(Get, Tmp2DRE, Storage,
                                            TargetImpl::Storage, Body, Ctx);

  // Return tmp2.
  Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, DeclNameLoc(), /*Implicit*/true,
                                  AccessSemantics::DirectToStorage);

  Body.push_back(new (Ctx) ReturnStmt(SourceLoc(), Tmp2DRE, /*implicit*/true));

  Get->setBody(BraceStmt::create(Ctx, VD->getLoc(), Body, VD->getLoc(),
                                 /*implicit*/true));
}

static void synthesizeLazySetterBody(AbstractFunctionDecl *fn, void *context) {
  auto *setter = cast<AccessorDecl>(fn);
  auto *underlyingStorage = (VarDecl *) context;
  auto &ctx = setter->getASTContext();

  if (setter->isInvalid() || ctx.hadError())
    return;

  synthesizeTrivialSetterBodyWithStorage(setter, TargetImpl::Storage,
                                         underlyingStorage, ctx);
}

void TypeChecker::completePropertyBehaviorStorage(VarDecl *VD,
                               VarDecl *BehaviorStorage,
                               FuncDecl *DefaultInitStorage,
                               FuncDecl *ParamInitStorage,
                               Type SelfTy,
                               Type StorageTy,
                               NormalProtocolConformance *BehaviorConformance,
                               SubstitutionMap interfaceMap,
                               SubstitutionMap contextMap) {
  assert(BehaviorStorage);
  assert((bool)DefaultInitStorage != (bool)ParamInitStorage);

  // Substitute the storage type into the conforming context.
  auto SubstStorageInterfaceTy = StorageTy.subst(interfaceMap);
  assert(SubstStorageInterfaceTy && "storage type substitution failed?!");

  auto SubstStorageContextTy = StorageTy.subst(contextMap);
  assert(SubstStorageContextTy && "storage type substitution failed?!");

  auto DC = VD->getDeclContext();
  SmallString<64> NameBuf = VD->getName().str();
  NameBuf += ".storage";
  auto StorageName = Context.getIdentifier(NameBuf);
  auto storageSpecifier = BehaviorStorage->isSettable(DC)
                        ? VarDecl::Specifier::Var
                        : VarDecl::Specifier::Let;
  auto *Storage = new (Context) VarDecl(
      /*IsStatic*/VD->isStatic(), storageSpecifier,
      /*IsCaptureList*/false, VD->getLoc(), StorageName,
      DC);
  Storage->setInterfaceType(SubstStorageInterfaceTy);
  Storage->setUserAccessible(false);
  // Mark the vardecl to be final, implicit, and private.  In a class, this
  // prevents it from being dynamically dispatched.
  if (VD->getDeclContext()->getSelfClassDecl())
    makeFinal(Context, Storage);
  Storage->setImplicit();
  Storage->setAccess(AccessLevel::Private);
  Storage->setSetterAccess(AccessLevel::Private);
  
  addMemberToContextIfNeeded(Storage, DC);
  
  // Initialize the storage immediately, if we can.
  Expr *InitStorageExpr = nullptr;
  auto Method = DefaultInitStorage ? DefaultInitStorage : ParamInitStorage;
  auto SpecializeInitStorage = ConcreteDeclRef(Method, contextMap);

  if (DefaultInitStorage ||
      (ParamInitStorage && VD->getParentInitializer())) {
    
    // Build the initializer expression, 'Self.initStorage()', using the
    // conformance.
    auto SelfTypeRef = TypeExpr::createImplicit(SelfTy, Context);
    
    auto InitStorageRef = new (Context) DeclRefExpr(SpecializeInitStorage,
                                                    DeclNameLoc(),
                                                    /*implicit*/ true);
    auto InitStorageMethodTy = FunctionType::get({}, SubstStorageContextTy);

    FunctionType::Param SelfParam(SelfTypeRef->getType());
    auto InitStorageRefTy = FunctionType::get({SelfParam}, InitStorageMethodTy);
    InitStorageRef->setType(InitStorageRefTy);

    auto SelfApply = new (Context) DotSyntaxCallExpr(InitStorageRef,
                                                     SourceLoc(),
                                                     SelfTypeRef);
    SelfApply->setImplicit();
    SelfApply->setType(InitStorageMethodTy);
    SelfApply->setThrows(false);
    
    SmallVector<Expr *, 1> InitStorageArgs;
    SmallVector<Identifier, 1> InitStorageArgLabels;
    if (ParamInitStorage) {
      // Claim the var initializer as the parameter to the `initStorage`
      // method.
      auto InitValue = VD->getParentInitializer();
      auto PBD = VD->getParentPatternBinding();
      unsigned entryIndex = PBD->getPatternEntryIndexForVarDecl(VD);
      PBD->setInit(entryIndex, nullptr);
      PBD->setInitializerChecked(entryIndex);

      // Recontextualize any closure declcontexts nested in the initializer to
      // realize that they are in the initialization context.
      InitValue->walk(RecontextualizeClosures(DC));
      
      // Coerce to the property type.
      auto PropertyType =
        Type(contextMap.getGenericSignature()->getGenericParams()[1])
          .subst(contextMap);
      InitValue = new (Context) CoerceExpr(InitValue, SourceLoc(),
                      TypeLoc::withoutLoc(PropertyType));
      // Type-check the expression.
      typeCheckExpression(InitValue, DC);

      InitStorageArgs.push_back(InitValue);
      InitStorageArgLabels.push_back(Identifier());
    }
    
    auto InitStorageExpr = CallExpr::createImplicit(Context,SelfApply,
                                                    InitStorageArgs,
                                                    InitStorageArgLabels);
    InitStorageExpr->setType(SubstStorageContextTy);
    InitStorageExpr->setThrows(false);
    
  } else {
    // Save the storage property and the initStorage reference for later.
    // We'll leave it to DI analysis to insert the initializer call at the
    // right place.
    auto *Behavior = VD->getMutableBehavior();
    Behavior->StorageDecl = Storage;
    Behavior->InitStorageDecl = SpecializeInitStorage;
  }
  
  // Create the pattern binding decl for the storage decl.  This will get
  // default initialized using the protocol's initStorage() method.
  Pattern *PBDPattern = new (Context) NamedPattern(Storage, /*implicit*/true);
  PBDPattern = TypedPattern::createImplicit(Context, PBDPattern,
                                            SubstStorageContextTy);
  auto *PBD = PatternBindingDecl::createImplicit(
      Context, VD->getParentPatternBinding()->getStaticSpelling(), PBDPattern,
      InitStorageExpr, VD->getDeclContext(), /*VarLoc*/ VD->getLoc());
  PBD->setInitializerChecked(0);
  addMemberToContextIfNeeded(PBD, VD->getDeclContext(), VD);
  
  // Add accessors to the storage, since we'll need them to satisfy the
  // conformance requirements.
  addTrivialAccessorsToStorage(Storage, Context);

  // FIXME: Hack to eliminate spurious diagnostics.
  if (BehaviorStorage->isStatic() != Storage->isStatic()) return;

  // Add the witnesses to the conformance.
  recordKnownWitness(BehaviorConformance, BehaviorStorage, Storage);
  recordKnownWitness(BehaviorConformance, BehaviorStorage->getGetter(),
                     Storage->getGetter());
  if (BehaviorStorage->isSettable(DC))
    recordKnownWitness(BehaviorConformance, BehaviorStorage->getSetter(),
                       Storage->getSetter());
}

void TypeChecker::completePropertyBehaviorParameter(VarDecl *VD,
                                 FuncDecl *BehaviorParameter,
                                 NormalProtocolConformance *BehaviorConformance,
                                 SubstitutionMap interfaceMap) {
  // Create a method to witness the requirement.
  auto DC = VD->getDeclContext();
  SmallString<64> NameBuf = VD->getName().str();
  NameBuf += ".parameter";
  auto ParameterBaseName = Context.getIdentifier(NameBuf);

  // Substitute the requirement type into the conforming context.
  auto ParameterTy = BehaviorParameter->getInterfaceType()
    ->castTo<AnyFunctionType>()
    ->getResult();

  GenericSignature *genericSig = nullptr;
  GenericEnvironment *genericEnv = nullptr;

  auto SubstInterfaceTy = ParameterTy.subst(interfaceMap);
  assert(SubstInterfaceTy && "storage type substitution failed?!");
  
  auto SubstBodyResultTy = SubstInterfaceTy->castTo<AnyFunctionType>()
    ->getResult();
  
  // Add the Self type back to the interface and context types.
  if (DC->isTypeContext()) {
    FunctionType::Param SelfParam(DC->getSelfInterfaceType());

    if (DC->isGenericContext()) {
      genericSig = DC->getGenericSignatureOfContext();
      genericEnv = DC->getGenericEnvironmentOfContext();
      SubstInterfaceTy =
        GenericFunctionType::get(genericSig, {SelfParam}, SubstInterfaceTy);
    } else {
      SubstInterfaceTy =
        FunctionType::get({SelfParam}, SubstInterfaceTy);
    }
  }
  
  // Borrow the parameters from the requirement declaration.
  SmallVector<ParamDecl *, 4> Params;
  SmallVector<Identifier, 4> NameComponents;
  
  auto *DeclaredParams = BehaviorParameter->getParameters();
  for (unsigned i : indices(*DeclaredParams)) {
    auto declaredParam = DeclaredParams->get(i);
    auto declaredParamTy = declaredParam->getInterfaceType();
    auto interfaceTy = declaredParamTy.subst(interfaceMap);
    assert(interfaceTy);
    auto declaredSpecifier = declaredParam->getSpecifier();

    SmallString<64> ParamNameBuf;
    {
      llvm::raw_svector_ostream names(ParamNameBuf);
      names << "%arg." << i;
    }
    auto param = new (Context) ParamDecl(
        declaredSpecifier, SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
        Context.getIdentifier(ParamNameBuf), DC);
    param->setInterfaceType(interfaceTy);
    param->setImplicit();
    Params.push_back(param);
    NameComponents.push_back(Identifier());
  }
  auto *ParamList = ParameterList::create(Context, Params);

  auto *Parameter =
    FuncDecl::create(Context, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
                     /*FuncLoc=*/SourceLoc(),
                     DeclName(Context, ParameterBaseName, NameComponents),
                     /*NameLoc=*/SourceLoc(),
                     /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                     /*GenericParams=*/nullptr, ParamList,
                     TypeLoc::withoutLoc(SubstBodyResultTy), DC);

  Parameter->setInterfaceType(SubstInterfaceTy);
  Parameter->setGenericEnvironment(genericEnv);
  Parameter->setValidationToChecked();

  // Mark the method to be final, implicit, and private.  In a class, this
  // prevents it from being dynamically dispatched.
  if (DC->getSelfClassDecl())
    makeFinal(Context, Parameter);
  Parameter->setImplicit();
  Parameter->setAccess(AccessLevel::Private);

  // Recontextualize any closure declcontexts nested in the initializer to
  // realize that they are in the parameter function.
  assert(VD->getBehavior()->Param);
  VD->getBehavior()->Param->walk(RecontextualizeClosures(Parameter));
  
  // Apply and return the closure in the function context.
  SmallVector<Expr *, 4> argRefs;
  SmallVector<Identifier, 4> argNames;
  for (unsigned i : indices(Params)) {
    auto param = Params[i];
    auto expr = new (Context) DeclRefExpr(param, DeclNameLoc(),
                                          /*implicit*/ true);
    argRefs.push_back(expr);
    argNames.push_back(DeclaredParams->get(i)->getName());
  }
  auto apply = CallExpr::createImplicit(Context, VD->getBehavior()->Param,
                                        argRefs, argNames);
  
  // Return the expression value.
  auto Ret = new (Context) ReturnStmt(SourceLoc(), apply,
                                      /*implicit*/ true);
  auto Body = BraceStmt::create(Context, SourceLoc(), ASTNode(Ret),
                                SourceLoc(), /*implicit*/ true);
  Parameter->setBody(Body);
  
  typeCheckDecl(Parameter);
  addMemberToContextIfNeeded(Parameter, DC);

  // Add the witnesses to the conformance.
  recordKnownWitness(BehaviorConformance, BehaviorParameter, Parameter);
}

void TypeChecker::completePropertyBehaviorAccessors(VarDecl *VD,
                                       VarDecl *ValueImpl,
                                       Type valueTy,
                                       SubstitutionMap SelfInterfaceSubs,
                                       SubstitutionMap SelfContextSubs) {
  auto selfGenericParamTy = Type(GenericTypeParamType::get(0, 0, Context));
  auto selfTy = selfGenericParamTy.subst(SelfContextSubs);
  auto selfIfaceTy = selfGenericParamTy.subst(SelfInterfaceSubs);

  SmallVector<ASTNode, 3> bodyStmts;
  
  auto makeSelfExpr = [&](FuncDecl *fromAccessor,
                          FuncDecl *toAccessor) -> Expr * {
    Expr *selfExpr;
    if (VD->getDeclContext()->isTypeContext()) {
      ConcreteDeclRef selfRef = fromAccessor->getImplicitSelfDecl();
      selfExpr = new (Context) DeclRefExpr(selfRef, DeclNameLoc(),
                                           /*implicit*/ true);
      
    } else {
      // self is the empty tuple outside of a type.
      selfExpr = TupleExpr::createEmpty(Context, SourceLoc(), SourceLoc(),
                                        /*implicit*/ true);
    }

    // If forwarding from a nonmutating to a mutating accessor, we need to put
    // `self` in a mutable temporary.
    auto fromMutating = VD->getDeclContext()->isTypeContext()
      && fromAccessor->getImplicitSelfDecl()->isSettable(fromAccessor);
    
    if (!fromMutating
        && toAccessor->getImplicitSelfDecl()->isSettable(toAccessor)) {
      selfExpr->setType(selfTy);
      auto var = new (Context) VarDecl(/*IsStatic*/false,
                                       VarDecl::Specifier::Var,
                                       /*IsCaptureList*/false, SourceLoc(),
                                       Context.getIdentifier("tempSelf"),
                                       fromAccessor);
      var->setInterfaceType(selfIfaceTy);
      var->setImplicit();

      auto varPat = new (Context) NamedPattern(var, /*implicit*/ true);
      auto *pbd = PatternBindingDecl::createImplicit(
          Context, StaticSpellingKind::None, varPat, selfExpr, fromAccessor);
      bodyStmts.push_back(var);
      bodyStmts.push_back(pbd);
      selfExpr = new (Context) DeclRefExpr(var, DeclNameLoc(),
                                           /*implicit*/ true);
    }
    assert((!fromMutating
            || toAccessor->getImplicitSelfDecl()->isSettable(toAccessor))
           && "can't forward from mutating to nonmutating");
    if (!toAccessor->isMutating()) {
      selfExpr->setType(selfTy);
    } else {
      // Access the base as inout if the accessor is mutating.
      auto lvTy = LValueType::get(selfTy);
      selfExpr->setType(lvTy);
      selfExpr = new (Context) InOutExpr(SourceLoc(),
                                         selfExpr, selfTy, /*implicit*/ true);
    }
    return selfExpr;
  };
  
  {
    auto getter = VD->getGetter();
    assert(getter);

    Expr *selfExpr = makeSelfExpr(getter, ValueImpl->getGetter());
    
    auto implRef = ConcreteDeclRef(ValueImpl, SelfContextSubs);
    auto implMemberExpr = new (Context) MemberRefExpr(selfExpr,
                                                      SourceLoc(),
                                                      implRef,
                                                      DeclNameLoc(),
                                                      /*implicit*/ true);
    Expr *returnExpr;
    if (ValueImpl->isSettable(VD->getDeclContext())) {
      auto valueLVTy = LValueType::get(valueTy);
      implMemberExpr->setType(valueLVTy);
      returnExpr = new (Context) LoadExpr(implMemberExpr,
                                          valueTy);
      returnExpr->setImplicit();
    } else {
      implMemberExpr->setType(valueTy);
      returnExpr = implMemberExpr;
    }
    auto returnStmt = new (Context) ReturnStmt(SourceLoc(), returnExpr,
                                               /*implicit*/ true);
    bodyStmts.push_back(returnStmt);
    auto body = BraceStmt::create(Context, SourceLoc(), bodyStmts, SourceLoc(),
                                  /*implicit*/ true);
    getter->setBody(body);
    getter->setBodyTypeCheckedIfPresent();
  }
  
  bodyStmts.clear();
  
  if (auto setter = VD->getSetter()) {
    Expr *selfExpr = makeSelfExpr(setter, ValueImpl->getSetter());
    auto implRef = ConcreteDeclRef(ValueImpl, SelfContextSubs);
    auto implMemberExpr = new (Context) MemberRefExpr(selfExpr,
                                                      SourceLoc(),
                                                      implRef,
                                                      DeclNameLoc(),
                                                      /*implicit*/ true);
    auto valueLVTy = LValueType::get(valueTy);
    implMemberExpr->setType(valueLVTy);

    ConcreteDeclRef newValueRef = getFirstParamDecl(setter);
    auto newValueExpr = new (Context) DeclRefExpr(newValueRef, DeclNameLoc(),
                                                  /*implicit*/ true);
    newValueExpr->setType(valueTy);
    
    auto assign = new (Context) AssignExpr(implMemberExpr, SourceLoc(),
                                           newValueExpr, /*implicit*/ true);
    assign->setType(TupleType::getEmpty(Context));
    
    bodyStmts.push_back(assign);
    auto body = BraceStmt::create(Context, SourceLoc(), bodyStmts, SourceLoc(),
                                  /*implicit*/ true);
    setter->setBody(body);
    setter->setBodyTypeCheckedIfPresent();
  }
}

void TypeChecker::completeLazyVarImplementation(VarDecl *VD) {
  assert(VD->getAttrs().hasAttribute<LazyAttr>());
  assert(VD->getReadImpl() == ReadImplKind::Get);
  assert(VD->getWriteImpl() == WriteImplKind::Set);
  assert(!VD->isStatic() && "Static vars are already lazy on their own");

  // Create the storage property as an optional of VD's type.
  SmallString<64> NameBuf = VD->getName().str();
  NameBuf += ".storage";
  auto StorageName = Context.getIdentifier(NameBuf);
  auto StorageTy = OptionalType::get(VD->getType());
  auto StorageInterfaceTy = OptionalType::get(VD->getInterfaceType());

  auto *Storage = new (Context) VarDecl(/*IsStatic*/false, VarDecl::Specifier::Var,
                                        /*IsCaptureList*/false, VD->getLoc(),
                                        StorageName,
                                        VD->getDeclContext());
  Storage->setInterfaceType(StorageInterfaceTy);
  Storage->setUserAccessible(false);
  addMemberToContextIfNeeded(Storage, VD->getDeclContext(), VD);

  // Create the pattern binding decl for the storage decl.  This will get
  // default initialized to nil.
  Pattern *PBDPattern = new (Context) NamedPattern(Storage, /*implicit*/true);
  PBDPattern = TypedPattern::createImplicit(Context, PBDPattern, StorageTy);
  auto *PBD = PatternBindingDecl::createImplicit(
      Context, StaticSpellingKind::None, PBDPattern, /*init*/ nullptr,
      VD->getDeclContext(), /*VarLoc*/ VD->getLoc());
  addMemberToContextIfNeeded(PBD, VD->getDeclContext(), VD);

  // Now that we've got the storage squared away, enqueue the getter and
  // setter to be synthesized.
  VD->getGetter()->setBodySynthesizer(&synthesizeLazyGetterBody, Storage);
  VD->getSetter()->setBodySynthesizer(&synthesizeLazySetterBody, Storage);

  // Mark the vardecl to be final, implicit, and private.  In a class, this
  // prevents it from being dynamically dispatched.  Note that we do this after
  // the accessors are set up, because we don't want the setter for the lazy
  // property to inherit these properties from the storage.
  if (VD->getDeclContext()->getSelfClassDecl())
    makeFinal(Context, Storage);
  Storage->setImplicit();
  Storage->overwriteAccess(AccessLevel::Private);
  Storage->overwriteSetterAccess(AccessLevel::Private);
}

static bool wouldBeCircularSynthesis(AbstractStorageDecl *storage,
                                     AccessorKind kind) {
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
  auto VD = dyn_cast<VarDecl>(storage);
  maybeAddAccessorsToStorage(TC, storage);

  // Synthesize accessors for lazy, all checking already been performed.
  bool lazy = false;
  if (VD && VD->getAttrs().hasAttribute<LazyAttr>() && !VD->isStatic() &&
      !VD->getGetter()->hasBody()) {
    TC.completeLazyVarImplementation(VD);
    lazy = true;
  }

  // Trigger accessor synthesis.
  storage->visitExpectedOpaqueAccessors([&](AccessorKind kind) {
    // Ignore 'get' and 'set' for variables that we triggered above.
    // TODO: just record the lazy-storage link in the AST, don't trigger
    // in completeLazyVarImplementation, and remove this special case.
    if (lazy && (kind == AccessorKind::Get || kind == AccessorKind::Set))
      return;

    // Don't synthesize an accessor if the accessor is supposed to be
    // the basis of the storage implementation.
    if (wouldBeCircularSynthesis(storage, kind))
      return;

    // Don't try to synthesize an accessor that doesn't exist.
    // TODO: should this be an assertion?
    auto accessor = storage->getAccessor(kind);
    if (!accessor)
      return;

    accessor->setBodySynthesizer(&synthesizeAccessorBody);

    TC.Context.addSynthesizedDecl(accessor);
    TC.DeclsToFinalize.insert(accessor);
  });
}

static void maybeAddAccessorsToBehaviorStorage(TypeChecker &TC, VarDecl *var) {
  // If there's already a getter, we're done.
  if (var->getGetter())
    return;

  auto *dc = var->getDeclContext();

  assert(!var->getBehavior()->Conformance.hasValue());
  
  // The property should be considered computed by the time we're through.
  SWIFT_DEFER {
    assert(!var->hasStorage() && "behavior var was not made computed");
  };
  
  auto behavior = var->getMutableBehavior();
  NormalProtocolConformance *conformance = nullptr;
  VarDecl *valueProp = nullptr;

  bool mightBeMutating = dc->isTypeContext()
    && !var->isStatic()
    && !dc->getDeclaredInterfaceType()->hasReferenceSemantics();

  auto makeBehaviorAccessors = [&]{
    AccessorDecl *getter;
    AccessorDecl *setter = nullptr;
    if (valueProp && valueProp->getGetter()) {
      getter = createGetterPrototype(var, TC.Context);
      // The getter is mutating if the behavior implementation is, unless
      // we're in a class or non-instance context.
      if (mightBeMutating && valueProp->isGetterMutating())
        getter->setSelfAccessKind(SelfAccessKind::Mutating);

      getter->setAccess(var->getFormalAccess());

      // Make a setter if the behavior property has one.
      if (valueProp->getSetter()) {
        setter = createSetterPrototype(var, TC.Context, getter);
        if (mightBeMutating && valueProp->isSetterMutating())
          setter->setSelfAccessKind(SelfAccessKind::Mutating);
        // TODO: max of property and implementation setter visibility?
        setter->setAccess(var->getFormalAccess());
      }
    } else {
      // Even if we couldn't find a value property, still make up a stub
      // getter and setter, so that subsequent diagnostics make sense for a
      // computed-ish property.
      getter = createGetterPrototype(var, TC.Context);
      getter->setAccess(var->getFormalAccess());
      setter = createSetterPrototype(var, TC.Context, getter);
      setter->setSelfAccessKind(SelfAccessKind::NonMutating);
      setter->setAccess(var->getFormalAccess());
    }

    SmallVector<AccessorDecl*, 2> accessors;
    accessors.push_back(getter);
    auto isMutable = StorageIsMutable_t(setter != nullptr);
    if (isMutable) accessors.push_back(setter);
    var->setAccessors(StorageImplInfo::getComputed(isMutable),
                      SourceLoc(), accessors, SourceLoc());
    
    // Save the conformance and 'value' decl for later type checking.
    behavior->Conformance = conformance;
    behavior->ValueDecl = valueProp;
  };

  // Try to resolve the behavior to a protocol.
  auto resolution = TypeResolution::forContextual(dc);
  auto behaviorType = resolution.resolveType(behavior->ProtocolName, None);
  if (!behaviorType) {
    return makeBehaviorAccessors();
  }
  
  {
    // The type must refer to a protocol.
    auto behaviorProtoTy = behaviorType->getAs<ProtocolType>();
    if (!behaviorProtoTy) {
      TC.diagnose(behavior->getLoc(),
                  diag::property_behavior_not_protocol);
      behavior->Conformance = (NormalProtocolConformance*)nullptr;
      return makeBehaviorAccessors();
    }
    auto behaviorProto = behaviorProtoTy->getDecl();

    // Validate the behavior protocol and all its extensions so we can do
    // name lookup.
    TC.validateDecl(behaviorProto);
    for (auto ext : behaviorProto->getExtensions()) {
      TC.validateExtension(ext);
    }
    
    // Look up the behavior protocol's "value" property, or bail if it doesn't
    // have one. The property's accessors will decide whether the getter
    // is mutating, and whether there's a setter. We'll type-check to make
    // sure the property type matches later after validation.
    auto lookup = TC.lookupMember(dc, behaviorProtoTy, TC.Context.Id_value);
    for (auto found : lookup) {
      if (auto foundVar = dyn_cast<VarDecl>(found.getValueDecl())) {
        if (valueProp) {
          TC.diagnose(behavior->getLoc(),
                      diag::property_behavior_protocol_reqt_ambiguous,
                      TC.Context.Id_value);
          TC.diagnose(valueProp->getLoc(), diag::identifier_declared_here,
                      TC.Context.Id_value);
          TC.diagnose(foundVar->getLoc(), diag::identifier_declared_here,
                      TC.Context.Id_value);
          break;
        }
          
        valueProp = foundVar;
      }
    }
    
    if (!valueProp) {
      TC.diagnose(behavior->getLoc(),
                  diag::property_behavior_protocol_no_value);
      return makeBehaviorAccessors();
    }
    
    TC.validateDecl(valueProp);
    var->setIsGetterMutating(mightBeMutating &&
                             valueProp->isGetterMutating());
    var->setIsSetterMutating(mightBeMutating &&
                             valueProp->isSetterMutating());
    
    // Set up a conformance to represent the behavior instantiation.
    // The conformance will be on the containing 'self' type, or '()' if the
    // property is in a non-type context.
    Type behaviorSelf;
    if (dc->isTypeContext()) {
      behaviorSelf = dc->getSelfInterfaceType();
      assert(behaviorSelf && "type context doesn't have self type?!");
      if (var->isStatic())
        behaviorSelf = MetatypeType::get(behaviorSelf);
    } else {
      behaviorSelf = TC.Context.TheEmptyTupleType;
    }
    
    conformance = TC.Context.getBehaviorConformance(behaviorSelf,
                                          behaviorProto,
                                          behavior->getLoc(), var,
                                          ProtocolConformanceState::Checking);
  }
  return makeBehaviorAccessors();
}

static void maybeAddAccessorsToLazyVariable(VarDecl *var, ASTContext &ctx) {
  // If there are already accessors, something is invalid; bail out.
  if (!var->getImplInfo().isSimpleStored())
    return;

  if (!var->getGetter()) {
    addGetterToStorage(var, ctx);
  }

  if (!var->getSetter()) {
    addSetterToStorage(var, ctx);
  }

  var->overwriteImplInfo(StorageImplInfo::getMutableComputed());

  addExpectedOpaqueAccessorsToStorage(var, ctx);
}

/// Try to add the appropriate accessors required a storage declaration.
/// This needs to be idempotent.
///
/// Note that the parser synthesizes accessors in some cases:
///   - it synthesizes a getter and setter for an observing property
///   - it synthesizes a setter for get+mutableAddress
void swift::maybeAddAccessorsToStorage(TypeChecker &TC,
                                       AbstractStorageDecl *storage) {
  // Introduce accessors for a property with behaviors.
  if (storage->hasBehavior()) {
    maybeAddAccessorsToBehaviorStorage(TC, cast<VarDecl>(storage));
    return;
  }

  // Lazy properties require special handling.
  if (storage->getAttrs().hasAttribute<LazyAttr>()) {
    maybeAddAccessorsToLazyVariable(cast<VarDecl>(storage), TC.Context);
    return;
  }

  auto *dc = storage->getDeclContext();

  // Local variables don't otherwise get accessors.
  if (dc->isLocalContext())
    return;

  // Implicit properties don't get accessors.
  if (storage->isImplicit())
    return;

  if (!dc->isTypeContext()) {
    // dynamic globals need accessors.
    if (dc->isModuleScopeContext() && storage->isNativeDynamic()) {
      addTrivialAccessorsToStorage(storage, TC.Context);
      return;
    }
    // Fixed-layout global variables don't get accessors.
    if (!storage->isResilient())
      return;

  // In a protocol context, variables written as just "var x : Int" or
  // "let x : Int" are errors and recovered by building a computed property
  // with just a getter. Diagnose this and create the getter decl now.
  } else if (isa<ProtocolDecl>(dc)) {
    if (storage->hasStorage()) {
      auto var = cast<VarDecl>(storage);

      if (var->isLet()) {
        TC.diagnose(var->getLoc(),
                   diag::protocol_property_must_be_computed_var)
          .fixItReplace(var->getParentPatternBinding()->getLoc(), "var")
          .fixItInsertAfter(var->getTypeLoc().getLoc(), " { get }");
      } else {
        auto diag = TC.diagnose(var->getLoc(), diag::protocol_property_must_be_computed);
        auto braces = var->getBracesRange();

        if (braces.isValid())
          diag.fixItReplace(braces, "{ get <#set#> }");
        else
          diag.fixItInsertAfter(var->getTypeLoc().getLoc(), " { get <#set#> }");
      }
    }

    setProtocolStorageImpl(storage, TC.Context);
    return;

  // NSManaged properties on classes require special handling.
  } else if (dc->getSelfClassDecl()) {
    auto var = dyn_cast<VarDecl>(storage);
    if (var && var->getAttrs().hasAttribute<NSManagedAttr>()) {
      convertNSManagedStoredVarToComputed(var, TC.Context);
      return;
    }

  // Stored properties imported from Clang don't get accessors.
  } else if (auto *structDecl = dyn_cast<StructDecl>(dc)) {
    if (structDecl->hasClangNode())
      return;
  }

  // Stored properties in SIL mode don't get accessors.
  // But we might need to create opaque accessors for them.
  if (auto sourceFile = dc->getParentSourceFile())
    if (sourceFile->Kind == SourceFileKind::SIL) {
      if (storage->getGetter()) {
        addExpectedOpaqueAccessorsToStorage(storage, TC.Context);
      }
      return;
    }

  // Everything else gets mandatory accessors.
  addTrivialAccessorsToStorage(storage, TC.Context);
}

static void synthesizeGetterBody(AccessorDecl *getter,
                                 ASTContext &ctx) {
  if (getter->hasForcedStaticDispatch()) {
    synthesizeTrivialGetterBody(getter, TargetImpl::Ordinary, ctx);
    return;
  }

  switch (getter->getStorage()->getReadImpl()) {
  case ReadImplKind::Stored:
    synthesizeTrivialGetterBody(getter, ctx);
    return;

  case ReadImplKind::Get:
    llvm_unreachable("synthesizing getter that already exists?");

  case ReadImplKind::Inherited:
    synthesizeInheritedGetterBody(getter, ctx);
    return;

  case ReadImplKind::Address:
    synthesizeAddressedGetterBody(getter, ctx);
    return;

  case ReadImplKind::Read:
    synthesizeReadCoroutineGetterBody(getter, ctx);
    return;
  }
  llvm_unreachable("bad ReadImplKind");
}

static void synthesizeSetterBody(AccessorDecl *setter,
                                 ASTContext &ctx) {
  switch (setter->getStorage()->getWriteImpl()) {
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
 
void synthesizeAccessorBody(AbstractFunctionDecl *fn, void *) {
  auto *accessor = cast<AccessorDecl>(fn);
  auto &ctx = accessor->getASTContext();

  if (accessor->isInvalid() || ctx.hadError())
    return;

  switch (accessor->getAccessorKind()) {
  case AccessorKind::Get:
    synthesizeGetterBody(accessor, ctx);
    return;

  case AccessorKind::Set:
    synthesizeSetterBody(accessor, ctx);
    return;

  case AccessorKind::Read:
    synthesizeReadCoroutineBody(accessor, ctx);
    return;

  case AccessorKind::Modify:
    synthesizeModifyCoroutineBody(accessor, ctx);
    return;

  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::Address:
  case AccessorKind::MutableAddress:
    break;
  }
  llvm_unreachable("bad synthesized function kind");
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

  ASTContext &context = tc.Context;
  SourceLoc Loc = decl->getLoc();
  auto accessLevel = AccessLevel::Internal;

  // Determine the parameter type of the implicit constructor.
  SmallVector<ParamDecl*, 8> params;
  if (ICK == ImplicitConstructorKind::Memberwise) {
    assert(isa<StructDecl>(decl) && "Only struct have memberwise constructor");

    for (auto member : decl->getMembers()) {
      auto var = dyn_cast<VarDecl>(member);
      if (!var)
        continue;
      
      // Implicit, computed, and static properties are not initialized.
      // The exception is lazy properties, which due to batch mode we may or
      // may not have yet finalized, so they may currently be "stored" or
      // "computed" in the current AST state.
      if (var->isImplicit() || var->isStatic())
        continue;
      tc.validateDecl(var);
      if (!var->hasStorage() && !var->getAttrs().hasAttribute<LazyAttr>())
        continue;

      // Initialized 'let' properties have storage, but don't get an argument
      // to the memberwise initializer since they already have an initial
      // value that cannot be overridden.
      if (var->isLet() && var->getParentInitializer())
        continue;
      
      accessLevel = std::min(accessLevel, var->getFormalAccess());

      auto varInterfaceType = var->getValueInterfaceType();

      // If var is a lazy property, its value is provided for the underlying
      // storage.  We thus take an optional of the properties type.  We only
      // need to do this because the implicit constructor is added before all
      // the properties are type checked.  Perhaps init() synth should be moved
      // later.
      if (var->getAttrs().hasAttribute<LazyAttr>())
        varInterfaceType = OptionalType::get(varInterfaceType);

      // Create the parameter.
      auto *arg = new (context)
          ParamDecl(VarDecl::Specifier::Default, SourceLoc(), Loc,
                    var->getName(), Loc, var->getName(), decl);
      arg->setInterfaceType(varInterfaceType);
      arg->setImplicit();
      
      params.push_back(arg);
    }
  }

  auto paramList = ParameterList::create(context, params);
  
  // Create the constructor.
  DeclName name(context, DeclBaseName::createConstructor(), paramList);
  auto *ctor =
    new (context) ConstructorDecl(name, Loc,
                                  OTK_None, /*FailabilityLoc=*/SourceLoc(),
                                  /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                                  paramList, /*GenericParams=*/nullptr, decl);

  // Mark implicit.
  ctor->setImplicit();
  ctor->setAccess(accessLevel);

  if (ICK == ImplicitConstructorKind::Memberwise)
    ctor->setIsMemberwiseInitializer();

  // If we are defining a default initializer for a class that has a superclass,
  // it overrides the default initializer of its superclass. Add an implicit
  // 'override' attribute.
  if (auto classDecl = dyn_cast<ClassDecl>(decl)) {
    if (classDecl->getSuperclass())
      ctor->getAttrs().add(new (tc.Context) OverrideAttr(/*IsImplicit=*/true));
  }

  // Type-check the constructor declaration.
  tc.validateDecl(ctor);

  return ctor;
}

/// Create a stub body that emits a fatal error message.
static void createStubBody(TypeChecker &tc, ConstructorDecl *ctor) {
  auto unimplementedInitDecl = tc.Context.getUnimplementedInitializerDecl(&tc);
  auto classDecl = ctor->getDeclContext()->getSelfClassDecl();
  if (!unimplementedInitDecl) {
    tc.diagnose(classDecl->getLoc(), diag::missing_unimplemented_init_runtime);
    return;
  }

  // Create a call to Swift._unimplementedInitializer
  auto loc = classDecl->getLoc();
  Expr *fn = new (tc.Context) DeclRefExpr(unimplementedInitDecl,
                                          DeclNameLoc(loc),
                                          /*Implicit=*/true);

  llvm::SmallString<64> buffer;
  StringRef fullClassName = tc.Context.AllocateCopy(
                              (classDecl->getModuleContext()->getName().str() +
                               "." +
                               classDecl->getName().str()).toStringRef(buffer));

  Expr *className = new (tc.Context) StringLiteralExpr(fullClassName, loc,
                                                       /*Implicit=*/true);
  Expr *call = CallExpr::createImplicit(tc.Context, fn, { className },
                                        { tc.Context.Id_className });
  ctor->setBody(BraceStmt::create(tc.Context, SourceLoc(),
                                  ASTNode(call),
                                  SourceLoc(),
                                  /*implicit=*/true));

  // Note that this is a stub implementation.
  ctor->setStubImplementation(true);
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
    genericParams->setOuterParameters(classDecl->getGenericParamsOfContext());

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
    auto substTy = paramTy.subst(subMap);
    decl->setInterfaceType(substTy);
    decl->setType(GenericEnvironment::mapTypeIntoContext(genericEnv, substTy));
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
    createStubBody(tc, ctor);
    ctor->setNeedsNewVTableEntry(false);
    return ctor;
  }

  // Form the body of a chaining designated initializer.
  assert(kind == DesignatedInitKind::Chaining);

  // Reference to super.init.
  auto *selfDecl = ctor->getImplicitSelfDecl();
  Expr *superRef = new (ctx) SuperRefExpr(selfDecl, SourceLoc(),
                                          /*Implicit=*/true);
  Expr *ctorRef  = new (ctx) UnresolvedDotExpr(superRef, SourceLoc(),
                                               superclassCtor->getFullName(),
                                               DeclNameLoc(),
                                               /*Implicit=*/true);

  auto ctorArgs = buildArgumentForwardingExpr(bodyParams->getArray(), ctx);

  Expr *superCall =
    CallExpr::create(ctx, ctorRef, ctorArgs,
                     superclassCtor->getFullName().getArgumentNames(), { },
                     /*hasTrailingClosure=*/false, /*implicit=*/true);
  if (superclassCtor->hasThrows()) {
    superCall = new (ctx) TryExpr(SourceLoc(), superCall, Type(),
                                  /*implicit=*/true);
  }
  ctor->setBody(BraceStmt::create(tc.Context, SourceLoc(),
                                  ASTNode(superCall),
                                  SourceLoc(),
                                  /*implicit=*/true));

  return ctor;
}
