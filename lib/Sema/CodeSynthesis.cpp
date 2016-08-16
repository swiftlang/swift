//===--- CodeSynthesis.cpp - Type Checking for Declarations ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for declarations.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"

#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Availability.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
using namespace swift;

const bool IsImplicit = true;

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
  return fn->getParameterLists().back()->get(index);
}

static VarDecl *getFirstParamDecl(FuncDecl *fn) {
  return getParamDeclAtIndex(fn, 0);
};


static ParamDecl *buildArgument(SourceLoc loc, DeclContext *DC,
                               StringRef name, Type type, bool isLet) {
  auto &context = DC->getASTContext();
  auto *param = new (context) ParamDecl(isLet, SourceLoc(), SourceLoc(),
                                        Identifier(), loc,
                                        context.getIdentifier(name),Type(), DC);
  param->setImplicit();
  param->getTypeLoc().setType(type);
  return param;
}

static ParamDecl *buildLetArgument(SourceLoc loc, DeclContext *DC,
                                   StringRef name, Type type) {
  return buildArgument(loc, DC, name, type, /*isLet*/ true);
}

static ParamDecl *buildInOutArgument(SourceLoc loc, DeclContext *DC,
                                     StringRef name, Type type) {
  return buildArgument(loc, DC, name, InOutType::get(type), /*isLet*/ false);
}

static Type getTypeOfStorage(AbstractStorageDecl *storage,
                             TypeChecker &TC) {
  if (auto var = dyn_cast<VarDecl>(storage)) {
    return TC.getTypeOfRValue(var, /*want interface type*/ false);
  } else {
    // None of the transformations done by getTypeOfRValue are
    // necessary for subscripts.
    auto subscript = cast<SubscriptDecl>(storage);
    return subscript->getElementType();
  }
}

/// Build a parameter list which can forward the formal index parameters of a
/// declaration.
///
/// \param prefix optional arguments to be prefixed onto the index
///   forwarding pattern.
static ParameterList *
buildIndexForwardingParamList(AbstractStorageDecl *storage,
                              ArrayRef<ParamDecl*> prefix) {
  auto &context = storage->getASTContext();
  auto subscript = dyn_cast<SubscriptDecl>(storage);

  // Fast path: if this isn't a subscript, just use whatever we have.
  if (!subscript)
    return ParameterList::create(context, prefix);

  // Clone the parameter list over for a new decl, so we get new ParamDecls.
  auto indices = subscript->getIndices()->clone(context,
                                                ParameterList::Implicit);
  if (prefix.empty())
    return indices;
  
  
  // Otherwise, we need to build up a new parameter list.
  SmallVector<ParamDecl*, 4> elements;

  // Start with the fields we were given, if there are any.
  elements.append(prefix.begin(), prefix.end());
  elements.append(indices->begin(), indices->end());
  return ParameterList::create(context, elements);
}

static FuncDecl *createGetterPrototype(AbstractStorageDecl *storage,
                                       TypeChecker &TC) {
  SourceLoc loc = storage->getLoc();

  // Create the parameter list for the getter.
  SmallVector<ParameterList*, 2> getterParams;

  // The implicit 'self' argument if in a type context.
  if (storage->getDeclContext()->isTypeContext())
    getterParams.push_back(ParameterList::createUnboundSelf(loc,
                                                     storage->getDeclContext(),
                                                     /*isStatic*/false));
    
  // Add an index-forwarding clause.
  getterParams.push_back(buildIndexForwardingParamList(storage, {}));

  SourceLoc staticLoc;
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->isStatic())
      staticLoc = var->getLoc();
  }

  auto storageType = getTypeOfStorage(storage, TC);

  auto getter = FuncDecl::create(
      TC.Context, staticLoc, StaticSpellingKind::None, loc, Identifier(), loc,
      /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), /*GenericParams=*/nullptr,
      getterParams, Type(), TypeLoc::withoutLoc(storageType),
      storage->getDeclContext());
  getter->setImplicit();

  if (storage->isGetterMutating())
    getter->setMutating();

  // If the var is marked final, then so is the getter.
  if (storage->isFinal())
    makeFinal(TC.Context, getter);

  if (storage->isStatic())
    getter->setStatic();

  return getter;
}

static FuncDecl *createSetterPrototype(AbstractStorageDecl *storage,
                                       ParamDecl *&valueDecl,
                                       TypeChecker &TC) {
  SourceLoc loc = storage->getLoc();

  // Create the parameter list for the setter.
  SmallVector<ParameterList*, 2> params;

  // The implicit 'self' argument if in a type context.
  if (storage->getDeclContext()->isTypeContext()) {
    params.push_back(ParameterList::createUnboundSelf(loc,
                                               storage->getDeclContext(),
                                               /*isStatic*/false));
  }
  
  // Add a "(value : T, indices...)" argument list.
  auto storageType = getTypeOfStorage(storage, TC);
  valueDecl = buildLetArgument(storage->getLoc(),
                               storage->getDeclContext(), "value",
                               storageType);
  params.push_back(buildIndexForwardingParamList(storage, valueDecl));

  Type setterRetTy = TupleType::getEmpty(TC.Context);
  FuncDecl *setter = FuncDecl::create(
      TC.Context, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None, loc,
      Identifier(), loc, /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), /*GenericParams=*/nullptr,
      params, Type(), TypeLoc::withoutLoc(setterRetTy),
      storage->getDeclContext());
  setter->setImplicit();

  if (!storage->isSetterNonMutating())
    setter->setMutating();

  // If the var is marked final, then so is the getter.
  if (storage->isFinal())
    makeFinal(TC.Context, setter);

  if (storage->isStatic())
    setter->setStatic();

  return setter;
}

// True if the storage is dynamic or imported from Objective-C. In these cases,
// we need to emit a static materializeForSet thunk that dynamically dispatches
// to 'get' and 'set', rather than the normal dynamically dispatched
// materializeForSet that peer dispatches to 'get' and 'set'.
static bool needsDynamicMaterializeForSet(AbstractStorageDecl *storage) {
  return storage->isDynamic() || storage->hasClangNode();
}

// True if a generated accessor needs to be registered as an external decl.
bool needsToBeRegisteredAsExternalDecl(AbstractStorageDecl *storage) {
  // Either the storage itself was imported from Clang...
  if (storage->hasClangNode())
    return true;

  // ...or it was synthesized into an imported type.
  auto nominal = dyn_cast<NominalTypeDecl>(storage->getDeclContext());
  if (!nominal)
    return false;
  return nominal->hasClangNode();
}

static FuncDecl *createMaterializeForSetPrototype(AbstractStorageDecl *storage,
                                                  TypeChecker &TC) {
  auto &ctx = storage->getASTContext();
  SourceLoc loc = storage->getLoc();

  // Create the parameter list:
  SmallVector<ParameterList*, 2> params;

  //  - The implicit 'self' argument if in a type context.
  auto DC = storage->getDeclContext();
  if (DC->isTypeContext())
    params.push_back(ParameterList::createUnboundSelf(loc, DC, /*isStatic*/false));

  //  - The buffer parameter, (buffer: Builtin.RawPointer,
  //                           inout storage: Builtin.UnsafeValueBuffer,
  //                           indices...).
  ParamDecl *bufferElements[] = {
    buildLetArgument(loc, DC, "buffer", ctx.TheRawPointerType),
    buildInOutArgument(loc, DC, "callbackStorage", ctx.TheUnsafeValueBufferType)
  };
  params.push_back(buildIndexForwardingParamList(storage, bufferElements));

  // The accessor returns (temporary: Builtin.RawPointer,
  //                       callback: Builtin.RawPointer),
  // where the first pointer is the materialized address and the
  // second is the address of an optional callback.
  TupleTypeElt retElts[] = {
    { ctx.TheRawPointerType },
    { OptionalType::get(ctx.TheRawPointerType) },
  };
  Type retTy = TupleType::get(retElts, ctx);

  auto *materializeForSet = FuncDecl::create(
      ctx, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None, loc,
      Identifier(), loc, /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(), /*GenericParams=*/nullptr,
      params, Type(), TypeLoc::withoutLoc(retTy), DC);
  materializeForSet->setImplicit();
  
  // materializeForSet is mutating and static if the setter is.
  auto setter = storage->getSetter();

  // Open-code the setMutating() calculation since we might run before
  // the setter has been type checked. Also as a hack, always mark the
  // setter mutating if we're inside a protocol, because it seems some
  // things break otherwise -- the root cause should be fixed eventually.
  materializeForSet->setMutating(
      setter->getDeclContext()->getAsProtocolOrProtocolExtensionContext() ||
      (!setter->getAttrs().hasAttribute<NonMutatingAttr>() &&
       !storage->isSetterNonMutating()));

  materializeForSet->setStatic(setter->isStatic());

  // materializeForSet is final if the storage is.
  if (storage->isFinal())
    makeFinal(ctx, materializeForSet);
  
  // If the storage is dynamic or ObjC-native, we can't add a dynamically-
  // dispatched method entry for materializeForSet, so force it to be
  // statically dispatched. ("final" would be inappropriate because the
  // property can still be overridden.)
  if (needsDynamicMaterializeForSet(storage))
    materializeForSet->setForcedStaticDispatch(true);

  // Make sure materializeForSet is available enough to access
  // the storage (and its getters/setters if it has them).
  SmallVector<const Decl *, 2> asAvailableAs;
  asAvailableAs.push_back(storage);
  if (FuncDecl *getter = storage->getGetter()) {
    asAvailableAs.push_back(getter);
  }
  if (FuncDecl *setter = storage->getSetter()) {
    asAvailableAs.push_back(setter);
  }

  AvailabilityInference::applyInferredAvailableAttrs(materializeForSet,
                                                        asAvailableAs, ctx);

  // If the property came from ObjC, we need to register this as an external
  // definition to be compiled.
  if (needsToBeRegisteredAsExternalDecl(storage))
    TC.Context.addExternalDecl(materializeForSet);
  
  return materializeForSet;
}

void swift::convertStoredVarInProtocolToComputed(VarDecl *VD, TypeChecker &TC) {
  auto *Get = createGetterPrototype(VD, TC);
  
  // Okay, we have both the getter and setter.  Set them in VD.
  VD->makeComputed(VD->getLoc(), Get, nullptr, nullptr, VD->getLoc());
  
  // We've added some members to our containing class, add them to the members
  // list.
  addMemberToContextIfNeeded(Get, VD->getDeclContext());

  // Type check the getter declaration.
  TC.typeCheckDecl(VD->getGetter(), true);
  TC.typeCheckDecl(VD->getGetter(), false);
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
    // We cannot express how to forward variadic parameters yet.
    if (param->isVariadic())
      return nullptr;
    
    Expr *ref = new (ctx) DeclRefExpr(param, DeclNameLoc(), /*implicit*/ true);
    if (param->getType()->is<InOutType>())
      ref = new (ctx) InOutExpr(SourceLoc(), ref, Type(), /*implicit=*/true);
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
static Expr *buildSubscriptIndexReference(ASTContext &ctx, FuncDecl *accessor) {
  // Pull out the body parameters, which we should have cloned
  // previously to be forwardable.  Drop the initial buffer/value
  // parameter in accessors that have one.
  auto params = accessor->getParameterLists().back()->getArray();
  auto accessorKind = accessor->getAccessorKind();

  // Ignore the value/buffer parameter.
  if (accessorKind != AccessorKind::IsGetter)
    params = params.slice(1);

  // Ignore the materializeForSet callback storage parameter.
  if (accessorKind == AccessorKind::IsMaterializeForSet)
    params = params.slice(1);
  
  // Okay, everything else should be forwarded, build the expression.
  auto result = buildArgumentForwardingExpr(params, ctx);
  assert(result && "FIXME: Cannot forward varargs");
  return result;
}

enum class SelfAccessKind {
  /// We're building a derived accessor on top of whatever this
  /// class provides.
  Peer,

  /// We're building a setter or something around an underlying
  /// implementation, which might be storage or inherited from a
  /// superclass.
  Super,
};

static Expr *buildSelfReference(VarDecl *selfDecl,
                                SelfAccessKind selfAccessKind,
                                TypeChecker &TC) {
  switch (selfAccessKind) {
  case SelfAccessKind::Peer:
    return new (TC.Context) DeclRefExpr(selfDecl, DeclNameLoc(), IsImplicit);

  case SelfAccessKind::Super:
    return new (TC.Context) SuperRefExpr(selfDecl, SourceLoc(), IsImplicit);
  }
  llvm_unreachable("bad self access kind");
}

namespace {
  /// A simple helper interface for buildStorageReference.
  class StorageReferenceContext {
    StorageReferenceContext(const StorageReferenceContext &) = delete;
  public:
    StorageReferenceContext() = default;
    virtual ~StorageReferenceContext() = default;

    /// Returns the declaration of the entity to use as the base of
    /// the access, or nil if no base is required.
    virtual VarDecl *getSelfDecl() const = 0;

    /// Returns an expression producing the index value, assuming that
    /// the storage is a subscript declaration.
    virtual Expr *getIndexRefExpr(ASTContext &ctx,
                                  SubscriptDecl *subscript) const = 0;
  };

  /// A reference to storage from within an accessor.
  class AccessorStorageReferenceContext : public StorageReferenceContext {
    FuncDecl *Accessor;
  public:
    AccessorStorageReferenceContext(FuncDecl *accessor) : Accessor(accessor) {}
    virtual ~AccessorStorageReferenceContext() = default;

    VarDecl *getSelfDecl() const override {
      return Accessor->getImplicitSelfDecl();
    }
    Expr *getIndexRefExpr(ASTContext &ctx,
                          SubscriptDecl *subscript) const override {
      return buildSubscriptIndexReference(ctx, Accessor);
    }
  };
}

/// Build an l-value for the storage of a declaration.
static Expr *buildStorageReference(
                             const StorageReferenceContext &referenceContext,
                                   AbstractStorageDecl *storage,
                                   AccessSemantics semantics,
                                   SelfAccessKind selfAccessKind,
                                   TypeChecker &TC) {
  ASTContext &ctx = TC.Context;

  VarDecl *selfDecl = referenceContext.getSelfDecl();
  if (!selfDecl) {
    return new (ctx) DeclRefExpr(storage, DeclNameLoc(), IsImplicit, semantics);
  }

  // If we should use a super access if applicable, and we have an
  // overridden decl, then use ordinary access to it.
  if (selfAccessKind == SelfAccessKind::Super) {
    if (auto overridden = storage->getOverriddenDecl()) {
      storage = overridden;
      semantics = AccessSemantics::Ordinary;
    } else {
      selfAccessKind = SelfAccessKind::Peer;
    }
  }

  Expr *selfDRE = buildSelfReference(selfDecl, selfAccessKind, TC);

  if (auto subscript = dyn_cast<SubscriptDecl>(storage)) {
    Expr *indices = referenceContext.getIndexRefExpr(ctx, subscript);
    return SubscriptExpr::create(ctx, selfDRE, indices, storage,
                                 IsImplicit, semantics);
  }

  // This is a potentially polymorphic access, which is unnecessary;
  // however, it shouldn't be problematic because any overrides
  // should also redefine materializeForSet.
  return new (ctx) MemberRefExpr(selfDRE, SourceLoc(), storage,
                                 DeclNameLoc(), IsImplicit, semantics);
}

static Expr *buildStorageReference(FuncDecl *accessor,
                                   AbstractStorageDecl *storage,
                                   AccessSemantics semantics,
                                   SelfAccessKind selfAccessKind,
                                   TypeChecker &TC) {
  return buildStorageReference(AccessorStorageReferenceContext(accessor),
                               storage, semantics, selfAccessKind, TC);
}

/// Load the value of VD.  If VD is an @override of another value, we call the
/// superclass getter.  Otherwise, we do a direct load of the value.
static Expr *createPropertyLoadOrCallSuperclassGetter(FuncDecl *accessor,
                                              AbstractStorageDecl *storage,
                                                      TypeChecker &TC) {
  return buildStorageReference(accessor, storage,
                               AccessSemantics::DirectToStorage,
                               SelfAccessKind::Super, TC);
}

/// Look up the NSCopying protocol from the Foundation module, if present.
/// Otherwise return null.
static ProtocolDecl *getNSCopyingProtocol(TypeChecker &TC,
                                          DeclContext *DC) {
  ASTContext &ctx = TC.Context;
  auto foundation = ctx.getLoadedModule(ctx.Id_Foundation);
  if (!foundation)
    return nullptr;

  SmallVector<ValueDecl *, 2> results;
  DC->lookupQualified(ModuleType::get(foundation),
                      ctx.getSwiftId(KnownFoundationEntity::NSCopying),
                      NL_QualifiedDefault | NL_KnownNonCascadingDependency,
                      /*resolver=*/nullptr,
                      results);

  if (results.size() != 1)
    return nullptr;

  return dyn_cast<ProtocolDecl>(results.front());
}



/// Synthesize the code to store 'Val' to 'VD', given that VD has an @NSCopying
/// attribute on it.  We know that VD is a stored property in a class, so we
/// just need to generate something like "self.property = val.copy(zone: nil)"
/// here.  This does some type checking to validate that the call will succeed.
static Expr *synthesizeCopyWithZoneCall(Expr *Val, VarDecl *VD,
                                        TypeChecker &TC) {
  auto &Ctx = TC.Context;

  // We support @NSCopying on class types (which conform to NSCopying),
  // protocols which conform, and option types thereof.
  Type UnderlyingType = TC.getTypeOfRValue(VD, /*want interface type*/false);

  bool isOptional = false;
  if (Type optionalEltTy = UnderlyingType->getAnyOptionalObjectType()) {
    UnderlyingType = optionalEltTy;
    isOptional = true;
  }

  // The element type must conform to NSCopying.  If not, emit an error and just
  // recovery by synthesizing without the copy call.
  auto *CopyingProto = getNSCopyingProtocol(TC, VD->getDeclContext());
  if (!CopyingProto || !TC.conformsToProtocol(UnderlyingType, CopyingProto,
                                              VD->getDeclContext(), None)) {
    TC.diagnose(VD->getLoc(), diag::nscopying_doesnt_conform);
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
  ResultTy.setType(VD->getType(), true);

  // If we're working with non-optional types, we're forcing the cast.
  if (!isOptional) {
    Call = new (Ctx) ForcedCheckedCastExpr(Call, SourceLoc(), SourceLoc(),
                                           TypeLoc::withoutLoc(UnderlyingType));
    Call->setImplicit();
    return Call;
  }

  // We're working with optional types, so perform a conditional checked
  // downcast.
  Call = new (Ctx) ConditionalCheckedCastExpr(Call, SourceLoc(), SourceLoc(),
                                           TypeLoc::withoutLoc(UnderlyingType));
  Call->setImplicit();

  // Use OptionalEvaluationExpr to evaluate the "?".
  return new (Ctx) OptionalEvaluationExpr(Call);
}

/// In a synthesized accessor body, store 'value' to the appropriate element.
///
/// If the property is an override, we call the superclass setter.
/// Otherwise, we do a direct store of the value.
static void createPropertyStoreOrCallSuperclassSetter(FuncDecl *accessor,
                                                      Expr *value,
                                               AbstractStorageDecl *storage,
                                               SmallVectorImpl<ASTNode> &body,
                                                      TypeChecker &TC) {
  // If the storage is an @NSCopying property, then we store the
  // result of a copyWithZone call on the value, not the value itself.
  if (auto property = dyn_cast<VarDecl>(storage)) {
    if (property->getAttrs().hasAttribute<NSCopyingAttr>())
      value = synthesizeCopyWithZoneCall(value, property, TC);
  }

  // Create:
  //   (assign (decl_ref_expr(VD)), decl_ref_expr(value))
  // or:
  //   (assign (member_ref_expr(decl_ref_expr(self), VD)), decl_ref_expr(value))
  Expr *dest = buildStorageReference(accessor, storage,
                                     AccessSemantics::DirectToStorage,
                                     SelfAccessKind::Super, TC);

  body.push_back(new (TC.Context) AssignExpr(dest, SourceLoc(), value,
                                             IsImplicit));
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
static void maybeMarkTransparent(FuncDecl *accessor,
                                 AbstractStorageDecl *storage,
                                 TypeChecker &TC) {
  auto *nominal = storage->getDeclContext()
      ->getAsNominalTypeOrNominalTypeExtensionContext();
  if (nominal && nominal->hasFixedLayout())
    accessor->getAttrs().add(new (TC.Context) TransparentAttr(IsImplicit));
}

/// Synthesize the body of a trivial getter.  For a non-member vardecl or one
/// which is not an override of a base class property, it performs a direct
/// storage load.  For an override of a base member property, it chains up to
/// super.
static void synthesizeTrivialGetter(FuncDecl *getter,
                                    AbstractStorageDecl *storage,
                                    TypeChecker &TC) {
  auto &ctx = TC.Context;
  
  Expr *result = createPropertyLoadOrCallSuperclassGetter(getter, storage, TC);
  ASTNode returnStmt = new (ctx) ReturnStmt(SourceLoc(), result, IsImplicit);

  SourceLoc loc = storage->getLoc();
  getter->setBody(BraceStmt::create(ctx, loc, returnStmt, loc, true));

  maybeMarkTransparent(getter, storage, TC);
 
  // Register the accessor as an external decl if the storage was imported.
  if (needsToBeRegisteredAsExternalDecl(storage))
    TC.Context.addExternalDecl(getter);
}

/// Synthesize the body of a trivial setter.
static void synthesizeTrivialSetter(FuncDecl *setter,
                                    AbstractStorageDecl *storage,
                                    VarDecl *valueVar,
                                    TypeChecker &TC) {
  if (storage->isInvalid()) return;

  auto &ctx = TC.Context;
  SourceLoc loc = storage->getLoc();

  auto *valueDRE = new (ctx) DeclRefExpr(valueVar, DeclNameLoc(), IsImplicit);
  SmallVector<ASTNode, 1> setterBody;
  createPropertyStoreOrCallSuperclassSetter(setter, valueDRE, storage,
                                            setterBody, TC);
  setter->setBody(BraceStmt::create(ctx, loc, setterBody, loc, true));

  maybeMarkTransparent(setter, storage, TC);

  // Register the accessor as an external decl if the storage was imported.
  if (needsToBeRegisteredAsExternalDecl(storage))
    TC.Context.addExternalDecl(setter);
}

/// Does a storage decl currently lacking accessor functions require a
/// setter to be synthesized?
static bool doesStorageNeedSetter(AbstractStorageDecl *storage) {
  assert(!storage->hasAccessorFunctions());
  switch (storage->getStorageKind()) {
  // Add a setter to a stored variable unless it's a let.
  case AbstractStorageDecl::Stored:
    return !cast<VarDecl>(storage)->isLet();

  // Addressed storage gets a setter if it has a mutable addressor.
  case AbstractStorageDecl::Addressed:
    return storage->getMutableAddressor() != nullptr;

  // These should already have accessor functions.
  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
    llvm_unreachable("already has accessor functions");

  case AbstractStorageDecl::Computed:
    llvm_unreachable("not stored");
  }
  llvm_unreachable("bad storage kind");
}

/// Add a materializeForSet accessor to the given declaration.
static FuncDecl *addMaterializeForSet(AbstractStorageDecl *storage,
                                      TypeChecker &TC) {
  auto materializeForSet = createMaterializeForSetPrototype(storage, TC);
  addMemberToContextIfNeeded(materializeForSet, storage->getDeclContext(),
                             storage->getSetter());
  storage->setMaterializeForSetFunc(materializeForSet);

  TC.computeAccessibility(materializeForSet);

  TC.validateDecl(materializeForSet);

  return materializeForSet;
}

/// Add trivial accessors to a Stored or Addressed property.
void swift::addTrivialAccessorsToStorage(AbstractStorageDecl *storage,
                                         TypeChecker &TC) {
  assert(!storage->hasAccessorFunctions() && "already has accessors?");

  // Create the getter.
  auto *getter = createGetterPrototype(storage, TC);
  if (storage->hasAccessorFunctions()) return;

  // Create the setter.
  FuncDecl *setter = nullptr;
  ParamDecl *setterValueParam = nullptr;
  if (doesStorageNeedSetter(storage)) {
    setter = createSetterPrototype(storage, setterValueParam, TC);
    if (storage->hasAccessorFunctions()) return;
  }
  
  // Okay, we have both the getter and setter.  Set them in VD.
  storage->addTrivialAccessors(getter, setter, nullptr);

  bool isDynamic = (storage->isDynamic() && storage->isObjC());
  if (isDynamic)
    getter->getAttrs().add(new (TC.Context) DynamicAttr(IsImplicit));

  // Synthesize and type-check the body of the getter.
  synthesizeTrivialGetter(getter, storage, TC);
  TC.typeCheckDecl(getter, true);
  TC.typeCheckDecl(getter, false);

  if (setter) {
    if (isDynamic)
      setter->getAttrs().add(new (TC.Context) DynamicAttr(IsImplicit));

    // Synthesize and type-check the body of the setter.
    synthesizeTrivialSetter(setter, storage, setterValueParam, TC);
    TC.typeCheckDecl(setter, true);
    TC.typeCheckDecl(setter, false);
  }

  auto *DC = storage->getDeclContext();

  // We've added some members to our containing context, add them to
  // the right list.
  addMemberToContextIfNeeded(getter, DC);
  if (setter)
    addMemberToContextIfNeeded(setter, DC);

  // If we're creating trivial accessors for a stored property of a
  // nominal type, the stored property is either witnessing a
  // protocol requirement or the nominal type is resilient. In both
  // cases, we need to expose a materializeForSet.
  //
  // Global stored properties don't get a materializeForSet.
  if (setter && DC->getAsNominalTypeOrNominalTypeExtensionContext()) {
    FuncDecl *materializeForSet = addMaterializeForSet(storage, TC);
    synthesizeMaterializeForSet(materializeForSet, storage, TC);
    TC.typeCheckDecl(materializeForSet, false);
  }
}

/// Add a trivial setter and materializeForSet to a
/// ComputedWithMutableAddress storage decl.
void swift::
synthesizeSetterForMutableAddressedStorage(AbstractStorageDecl *storage,
                                           TypeChecker &TC) {
  auto setter = storage->getSetter();
  assert(setter);
  assert(!storage->getSetter()->getBody());
  assert(storage->getStorageKind() ==
           AbstractStorageDecl::ComputedWithMutableAddress);

  // Synthesize and type-check the body of the setter.
  VarDecl *valueParamDecl = getFirstParamDecl(setter);
  synthesizeTrivialSetter(setter, storage, valueParamDecl, TC);
  TC.typeCheckDecl(setter, true);
  TC.typeCheckDecl(setter, false);
}

void TypeChecker::synthesizeAccessorsForStorage(AbstractStorageDecl *storage,
                                                bool wantMaterializeForSet) {
  // If the decl is stored, convert it to StoredWithTrivialAccessors
  // by synthesizing the full set of accessors.
  if (!storage->hasAccessorFunctions()) {
    addTrivialAccessorsToStorage(storage, *this);
    return;
  }

  // If we want wantMaterializeForSet, create it now.
  if (wantMaterializeForSet && !storage->getMaterializeForSetFunc()) {
    FuncDecl *materializeForSet = addMaterializeForSet(storage, *this);
    synthesizeMaterializeForSet(materializeForSet, storage, *this);
    typeCheckDecl(materializeForSet, false);
  }
}

/// The specified AbstractStorageDecl was just found to satisfy a
/// protocol property requirement.  Ensure that it has the full
/// complement of accessors.
void TypeChecker::synthesizeWitnessAccessorsForStorage(
                                             AbstractStorageDecl *requirement,
                                             AbstractStorageDecl *storage) {
  // @objc protocols don't need a materializeForSet since ObjC doesn't
  // have that concept.
  bool wantMaterializeForSet =
    !requirement->isObjC() && requirement->getSetter();

  synthesizeAccessorsForStorage(storage, wantMaterializeForSet);
}

void swift::synthesizeMaterializeForSet(FuncDecl *materializeForSet,
                                        AbstractStorageDecl *storage,
                                        TypeChecker &TC) {
  // The body is actually emitted by SILGen

  maybeMarkTransparent(materializeForSet, storage, TC);

  TC.typeCheckDecl(materializeForSet, true);
  
  // Register the accessor as an external decl if the storage was imported.
  if (needsToBeRegisteredAsExternalDecl(storage))
    TC.Context.addExternalDecl(materializeForSet);
}

/// Given a VarDecl with a willSet: and/or didSet: specifier, synthesize the
/// (trivial) getter and the setter, which calls these.
void swift::synthesizeObservingAccessors(VarDecl *VD, TypeChecker &TC) {
  assert(VD->hasObservers());
  assert(VD->getGetter() && VD->getSetter() &&
         !VD->getGetter()->hasBody() && !VD->getSetter()->hasBody() &&
         "willSet/didSet var already has a getter or setter");
  
  auto &Ctx = VD->getASTContext();
  SourceLoc Loc = VD->getLoc();
  
  // The getter is always trivial: just perform a (direct!) load of storage, or
  // a call of a superclass getter if this is an override.
  auto *Get = VD->getGetter();
  synthesizeTrivialGetter(Get, VD, TC);

  // Okay, the getter is done, create the setter now.  Start by finding the
  // decls for 'self' and 'value'.
  auto *Set = VD->getSetter();
  auto *SelfDecl = Set->getImplicitSelfDecl();
  VarDecl *ValueDecl = Set->getParameterLists().back()->get(0);

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
      = createPropertyLoadOrCallSuperclassGetter(Set, VD, TC);
    
    OldValue = new (Ctx) VarDecl(/*isStatic*/false, /*isLet*/ true,
                                 SourceLoc(), Ctx.getIdentifier("tmp"),
                                 Type(), Set);
    OldValue->setImplicit();
    auto *tmpPattern = new (Ctx) NamedPattern(OldValue, /*implicit*/ true);
    auto tmpPBD = PatternBindingDecl::create(Ctx, SourceLoc(),
                                             StaticSpellingKind::None,
                                             SourceLoc(),
                                             tmpPattern, OldValueExpr, Set);
    tmpPBD->setImplicit();
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

    // Make sure the didSet/willSet accessors are marked final if in a class.
    if (!willSet->isFinal() &&
        VD->getDeclContext()->getAsClassOrClassExtensionContext())
      makeFinal(Ctx, willSet);
  }
  
  // Create an assignment into the storage or call to superclass setter.
  auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, DeclNameLoc(), true);
  createPropertyStoreOrCallSuperclassSetter(Set, ValueDRE, VD, SetterBody, TC);

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

    // Make sure the didSet/willSet accessors are marked final if in a class.
    if (!didSet->isFinal() &&
        VD->getDeclContext()->getAsClassOrClassExtensionContext())
      makeFinal(Ctx, didSet);
  }

  Set->setBody(BraceStmt::create(Ctx, Loc, SetterBody, Loc, true));

  // Type check the body of the getter and setter.
  TC.typeCheckDecl(Get, true);
  TC.typeCheckDecl(Get, false);
  TC.typeCheckDecl(Set, true);
  TC.typeCheckDecl(Set, false);
}

static void convertNSManagedStoredVarToComputed(VarDecl *VD, TypeChecker &TC) {
  assert(VD->getStorageKind() == AbstractStorageDecl::Stored);

  // Create the getter.
  auto *Get = createGetterPrototype(VD, TC);
  if (VD->hasAccessorFunctions()) return;

  // Create the setter.
  ParamDecl *SetValueDecl = nullptr;
  auto *Set = createSetterPrototype(VD, SetValueDecl, TC);
  if (VD->hasAccessorFunctions()) return;

  // Okay, we have both the getter and setter.  Set them in VD.
  VD->makeComputed(VD->getLoc(), Get, Set, nullptr, VD->getLoc());

  TC.validateDecl(Get);
  TC.validateDecl(Set);

  // We've added some members to our containing class/extension, add them to
  // the members list.
  addMemberToContextIfNeeded(Get, VD->getDeclContext());
  addMemberToContextIfNeeded(Set, VD->getDeclContext());
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

      return { true, E };
    }

    /// We don't want to recurse into declarations or statements.
    bool walkToDeclPre(Decl *) override { return false; }
    std::pair<bool, Stmt*> walkToStmtPre(Stmt *S) override { return {false,S}; }
  };
}

/// Synthesize the getter for a lazy property with the specified storage
/// vardecl.
static FuncDecl *completeLazyPropertyGetter(VarDecl *VD, VarDecl *Storage,
                                            TypeChecker &TC) {
  auto &Ctx = VD->getASTContext();

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
  auto *Get = VD->getGetter();
  TC.validateDecl(Get);

  SmallVector<ASTNode, 6> Body;

  // Load the existing storage and store it into the 'tmp1' temporary.
  auto *Tmp1VD = new (Ctx) VarDecl(/*isStatic*/false, /*isLet*/true,SourceLoc(),
                                   Ctx.getIdentifier("tmp1"), Type(), Get);
  Tmp1VD->setImplicit();

  auto *Tmp1PBDPattern = new (Ctx) NamedPattern(Tmp1VD, /*implicit*/true);
  auto *Tmp1Init = createPropertyLoadOrCallSuperclassGetter(Get, Storage, TC);
  auto *Tmp1PBD = PatternBindingDecl::create(Ctx, /*StaticLoc*/SourceLoc(),
                                             StaticSpellingKind::None,
                                             /*VarLoc*/SourceLoc(),
                                             Tmp1PBDPattern, Tmp1Init, Get);
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


  auto *Tmp2VD = new (Ctx) VarDecl(/*isStatic*/false, /*isLet*/true,
                                   SourceLoc(), Ctx.getIdentifier("tmp2"),
                                   VD->getType(), Get);
  Tmp2VD->setImplicit();


  // Take the initializer from the PatternBindingDecl for VD.
  // TODO: This doesn't work with complicated patterns like:
  //   lazy var (a,b) = foo()
  auto *InitValue = VD->getParentInitializer();
  auto PBD = VD->getParentPatternBinding();
  unsigned entryIndex = PBD->getPatternEntryIndexForVarDecl(VD);
  PBD->setInit(entryIndex, nullptr);
  PBD->setInitializerChecked(entryIndex);

  // Recontextualize any closure declcontexts nested in the initializer to
  // realize that they are in the getter function.
  InitValue->walk(RecontextualizeClosures(Get));


  Pattern *Tmp2PBDPattern = new (Ctx) NamedPattern(Tmp2VD, /*implicit*/true);
  Tmp2PBDPattern = new (Ctx) TypedPattern(Tmp2PBDPattern,
                                          TypeLoc::withoutLoc(VD->getType()),
                                          /*implicit*/true);

  auto *Tmp2PBD = PatternBindingDecl::create(Ctx, /*StaticLoc*/SourceLoc(),
                                             StaticSpellingKind::None,
                                             InitValue->getStartLoc(),
                                             Tmp2PBDPattern, InitValue, Get);
  Body.push_back(Tmp2PBD);
  Body.push_back(Tmp2VD);

  // Assign tmp2 into storage.
  auto Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, DeclNameLoc(), /*Implicit*/true,
                                       AccessSemantics::DirectToStorage);
  createPropertyStoreOrCallSuperclassSetter(Get, Tmp2DRE, Storage, Body, TC);

  // Return tmp2.
  Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, DeclNameLoc(), /*Implicit*/true,
                                  AccessSemantics::DirectToStorage);

  Body.push_back(new (Ctx) ReturnStmt(SourceLoc(), Tmp2DRE, /*implicit*/true));

  Get->setBody(BraceStmt::create(Ctx, VD->getLoc(), Body, VD->getLoc(),
                                 /*implicit*/true));

  return Get;
}

void TypeChecker::completePropertyBehaviorStorage(VarDecl *VD,
                               VarDecl *BehaviorStorage,
                               FuncDecl *DefaultInitStorage,
                               FuncDecl *ParamInitStorage,
                               Type SelfTy,
                               Type StorageTy,
                               NormalProtocolConformance *BehaviorConformance,
                               ArrayRef<Substitution> SelfInterfaceSubs,
                               ArrayRef<Substitution> SelfContextSubs) {
  assert(BehaviorStorage);
  assert((bool)DefaultInitStorage != (bool)ParamInitStorage);

  // Substitute the storage type into the conforming context.
  auto sig = BehaviorConformance->getProtocol()->getGenericSignatureOfContext();

  TypeSubstitutionMap interfaceMap = sig->getSubstitutionMap(SelfInterfaceSubs);
  auto SubstStorageInterfaceTy = StorageTy.subst(VD->getModuleContext(),
                                        interfaceMap, SubstOptions());
  assert(SubstStorageInterfaceTy && "storage type substitution failed?!");

  TypeSubstitutionMap contextMap = sig->getSubstitutionMap(SelfContextSubs);
  auto SubstStorageContextTy = StorageTy.subst(VD->getModuleContext(),
                                               contextMap, SubstOptions());
  assert(SubstStorageContextTy && "storage type substitution failed?!");

  auto DC = VD->getDeclContext();
  SmallString<64> NameBuf = VD->getName().str();
  NameBuf += ".storage";
  auto StorageName = Context.getIdentifier(NameBuf);
  auto *Storage = new (Context) VarDecl(VD->isStatic(),
                                       /*let*/ !BehaviorStorage->isSettable(DC),
                                       VD->getLoc(),
                                       StorageName,
                                       SubstStorageContextTy,
                                       DC);
  Storage->setInterfaceType(SubstStorageInterfaceTy);
  Storage->setUserAccessible(false);
  // Mark the vardecl to be final, implicit, and private.  In a class, this
  // prevents it from being dynamically dispatched.
  if (VD->getDeclContext()->getAsClassOrClassExtensionContext())
    makeFinal(Context, Storage);
  Storage->setImplicit();
  Storage->setAccessibility(Accessibility::Private);
  Storage->setSetterAccessibility(Accessibility::Private);
  
  addMemberToContextIfNeeded(Storage, DC);
  
  // Initialize the storage immediately, if we can.
  Expr *InitStorageExpr = nullptr;
  auto Method = DefaultInitStorage ? DefaultInitStorage : ParamInitStorage;
  auto SpecializeInitStorage = ConcreteDeclRef(Context, Method,
                                               SelfContextSubs);

  if (DefaultInitStorage ||
      (ParamInitStorage && VD->getParentInitializer())) {
    
    // Build the initializer expression, 'Self.initStorage()', using the
    // conformance.
    auto SelfTypeRef = TypeExpr::createImplicit(SelfTy, Context);
    
    auto InitStorageRef = new (Context) DeclRefExpr(SpecializeInitStorage,
                                                    DeclNameLoc(),
                                                    /*implicit*/ true);
    auto InitStorageMethodTy = FunctionType::get(Context.TheEmptyTupleType,
                                                 SubstStorageContextTy);
    auto InitStorageRefTy = FunctionType::get(SelfTypeRef->getType(),
                                              InitStorageMethodTy);
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
      InitValue = new (Context) CoerceExpr(InitValue, SourceLoc(),
                      TypeLoc::withoutLoc(SelfContextSubs[1].getReplacement()));
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
  PBDPattern = new (Context) TypedPattern(PBDPattern,
                                  TypeLoc::withoutLoc(SubstStorageContextTy),
                                  /*implicit*/true);
  auto *PBD = PatternBindingDecl::create(Context, /*staticloc*/SourceLoc(),
                             VD->getParentPatternBinding()->getStaticSpelling(),
                             /*varloc*/VD->getLoc(),
                             PBDPattern, /*init*/InitStorageExpr,
                             VD->getDeclContext());
  PBD->setImplicit();
  PBD->setInitializerChecked(0);
  addMemberToContextIfNeeded(PBD, VD->getDeclContext(), VD);
  
  // Add accessors to the storage, since we'll need them to satisfy the
  // conformance requirements.
  addTrivialAccessorsToStorage(Storage, *this);
  
  // Add the witnesses to the conformance.
  ArrayRef<Substitution> MemberSubs;
  if (DC->isGenericContext()) {
    MemberSubs = DC->getGenericParamsOfContext()
                   ->getForwardingSubstitutions(Context);
  }
  
  BehaviorConformance->setWitness(BehaviorStorage,
                                 ConcreteDeclRef(Context, Storage, MemberSubs));
  BehaviorConformance->setWitness(BehaviorStorage->getGetter(),
                    ConcreteDeclRef(Context, Storage->getGetter(), MemberSubs));
  if (BehaviorStorage->isSettable(DC))
    BehaviorConformance->setWitness(BehaviorStorage->getSetter(),
                    ConcreteDeclRef(Context, Storage->getSetter(), MemberSubs));
}

void TypeChecker::completePropertyBehaviorParameter(VarDecl *VD,
                                 FuncDecl *BehaviorParameter,
                                 NormalProtocolConformance *BehaviorConformance,
                                 ArrayRef<Substitution> SelfInterfaceSubs,
                                 ArrayRef<Substitution> SelfContextSubs) {
  // Create a method to witness the requirement.
  auto DC = VD->getDeclContext();
  SmallString<64> NameBuf = VD->getName().str();
  NameBuf += ".parameter";
  auto ParameterBaseName = Context.getIdentifier(NameBuf);

  // Substitute the requirement type into the conforming context.
  auto sig = BehaviorConformance->getProtocol()->getGenericSignatureOfContext();
  auto ParameterTy = BehaviorParameter->getInterfaceType()
    ->castTo<AnyFunctionType>()
    ->getResult();

  GenericSignature *genericSig = nullptr;

  TypeSubstitutionMap interfaceMap = sig->getSubstitutionMap(SelfInterfaceSubs);
  auto SubstInterfaceTy = ParameterTy.subst(VD->getModuleContext(),
                                            interfaceMap, SubstOptions());
  assert(SubstInterfaceTy && "storage type substitution failed?!");
  
  TypeSubstitutionMap contextMap = sig->getSubstitutionMap(SelfContextSubs);
  auto SubstContextTy = ParameterTy.subst(VD->getModuleContext(),
                                                 contextMap, SubstOptions());
  assert(SubstContextTy && "storage type substitution failed?!");

  auto SubstBodyResultTy = SubstContextTy->castTo<AnyFunctionType>()
    ->getResult();
  
  // Add the Self type back to the interface and context types.
  if (DC->isTypeContext()) {
    if (DC->isGenericContext()) {
      genericSig = DC->getGenericSignatureOfContext();
      SubstInterfaceTy = GenericFunctionType::get(genericSig,
                                                  DC->getSelfInterfaceType(),
                                                  SubstInterfaceTy,
                                                  AnyFunctionType::ExtInfo());
      auto genericParams = DC->getGenericParamsOfContext();
      SubstContextTy = PolymorphicFunctionType::get(DC->getSelfTypeInContext(),
                                                    SubstContextTy,
                                                    genericParams);
    } else {
      SubstInterfaceTy = FunctionType::get(DC->getSelfInterfaceType(),
                                           SubstInterfaceTy);
      SubstContextTy = FunctionType::get(DC->getSelfTypeInContext(),
                                         SubstContextTy);
    }
  }
  
  // Borrow the parameters from the requirement declaration.
  SmallVector<ParameterList *, 2> ParamLists;
  if (DC->isTypeContext()) {
    auto self = new (Context) ParamDecl(/*let*/ true, SourceLoc(), SourceLoc(),
                                        Identifier(), SourceLoc(),
                                        Context.Id_self,
                                        DC->getSelfTypeInContext(), DC);
    self->setInterfaceType(DC->getSelfInterfaceType());
    self->setImplicit();
    
    ParamLists.push_back(ParameterList::create(Context, SourceLoc(),
                                               self, SourceLoc()));
    ParamLists.back()->get(0)->setImplicit();
  }
  
  assert(BehaviorParameter->getParameterLists().size() == 2);
  SmallVector<ParamDecl *, 4> Params;
  SmallVector<Identifier, 4> NameComponents;
  
  auto *DeclaredParams = BehaviorParameter->getParameterList(1);
  for (unsigned i : indices(*DeclaredParams)) {
    auto declaredParam = DeclaredParams->get(i);
    auto declaredParamTy = declaredParam->getInterfaceType();
    auto interfaceTy = declaredParamTy.subst(DC->getParentModule(),
                                             interfaceMap,
                                             SubstOptions());
    assert(interfaceTy);
    auto contextTy = declaredParamTy.subst(DC->getParentModule(),
                                           contextMap, SubstOptions());
    assert(contextTy);

    SmallString<64> ParamNameBuf;
    {
      llvm::raw_svector_ostream names(ParamNameBuf);
      names << "%arg." << i;
    }
    auto param = new (Context) ParamDecl(/*let*/ true, SourceLoc(), SourceLoc(),
                                         Identifier(),
                                         SourceLoc(),
                                         Context.getIdentifier(ParamNameBuf),
                                         contextTy, DC);
    param->setInterfaceType(interfaceTy);
    param->setImplicit();
    Params.push_back(param);
    NameComponents.push_back(Identifier());
  }
  ParamLists.push_back(ParameterList::create(Context, Params));

  auto *Parameter =
    FuncDecl::create(Context, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
                     /*FuncLoc=*/SourceLoc(),
                     DeclName(Context, ParameterBaseName, NameComponents),
                     /*NameLoc=*/SourceLoc(),
                     /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                     /*AccessorKeywordLoc=*/SourceLoc(),
                     /*GenericParams=*/nullptr, ParamLists,
                     SubstContextTy,
                     TypeLoc::withoutLoc(SubstBodyResultTy), DC);

  Parameter->setInterfaceType(SubstInterfaceTy);
  Parameter->setGenericSignature(genericSig);

  // Mark the method to be final, implicit, and private.  In a class, this
  // prevents it from being dynamically dispatched.
  if (DC->getAsClassOrClassExtensionContext())
    makeFinal(Context, Parameter);
  Parameter->setImplicit();
  Parameter->setAccessibility(Accessibility::Private);
  Parameter->setIsBeingTypeChecked();
  Parameter->setBodyResultType(SubstBodyResultTy);

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
  
  Parameter->setIsBeingTypeChecked(false);
  typeCheckDecl(Parameter, true);
  typeCheckDecl(Parameter, false);
  addMemberToContextIfNeeded(Parameter, DC);

  // Add the witnesses to the conformance.
  ArrayRef<Substitution> MemberSubs;
  if (DC->isGenericContext()) {
    MemberSubs = DC->getGenericParamsOfContext()
    ->getForwardingSubstitutions(Context);
  }

  BehaviorConformance->setWitness(BehaviorParameter,
                               ConcreteDeclRef(Context, Parameter, MemberSubs));
}

void TypeChecker::completePropertyBehaviorAccessors(VarDecl *VD,
                                       VarDecl *ValueImpl,
                                       ArrayRef<Substitution> SelfInterfaceSubs,
                                       ArrayRef<Substitution> SelfContextSubs) {
  auto selfTy = SelfContextSubs[0].getReplacement();
  auto valueTy = SelfContextSubs[1].getReplacement();
  
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
      auto var = new (Context) VarDecl(/*static*/false, /*let*/false,
                                       SourceLoc(),
                                       Context.getIdentifier("tempSelf"),
                                       selfTy, fromAccessor);
      auto varPat = new (Context) NamedPattern(var);
      auto pbd = PatternBindingDecl::create(Context, SourceLoc(),
                                            StaticSpellingKind::None,
                                            SourceLoc(),
                                            varPat, selfExpr,
                                            fromAccessor);
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
      selfExpr->propagateLValueAccessKind(AccessKind::ReadWrite);
      auto inoutTy = InOutType::get(selfTy);
      selfExpr = new (Context) InOutExpr(SourceLoc(),
                                         selfExpr, inoutTy, /*implicit*/ true);
    }
    return selfExpr;
  };
  
  {
    auto getter = VD->getGetter();
    assert(getter);
    validateDecl(getter);
    
    Expr *selfExpr = makeSelfExpr(getter, ValueImpl->getGetter());
    
    auto implRef = ConcreteDeclRef(Context, ValueImpl, SelfContextSubs);
    auto implMemberExpr = new (Context) MemberRefExpr(selfExpr,
                                                      SourceLoc(),
                                                      implRef,
                                                      DeclNameLoc(),
                                                      /*implicit*/ true);
    Expr *returnExpr;
    if (ValueImpl->isSettable(VD->getDeclContext())) {
      auto valueLVTy = LValueType::get(valueTy);
      implMemberExpr->setType(valueLVTy);
      implMemberExpr->propagateLValueAccessKind(AccessKind::Read);
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
    validateDecl(setter);

    Expr *selfExpr = makeSelfExpr(setter, ValueImpl->getSetter());
    auto implRef = ConcreteDeclRef(Context, ValueImpl, SelfContextSubs);
    auto implMemberExpr = new (Context) MemberRefExpr(selfExpr,
                                                      SourceLoc(),
                                                      implRef,
                                                      DeclNameLoc(),
                                                      /*implicit*/ true);
    auto valueLVTy = LValueType::get(valueTy);
    implMemberExpr->setType(valueLVTy);
    implMemberExpr->propagateLValueAccessKind(AccessKind::Write);

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
  assert(VD->getStorageKind() == AbstractStorageDecl::Computed &&
         "variable not validated yet");
  assert(!VD->isStatic() && "Static vars are already lazy on their own");

  maybeAddMaterializeForSet(VD, *this);

  // Create the storage property as an optional of VD's type.
  SmallString<64> NameBuf = VD->getName().str();
  NameBuf += ".storage";
  auto StorageName = Context.getIdentifier(NameBuf);
  auto StorageTy = OptionalType::get(VD->getType());
  auto StorageInterfaceTy = OptionalType::get(VD->getInterfaceType());

  auto *Storage = new (Context) VarDecl(/*isStatic*/false, /*isLet*/false,
                                        VD->getLoc(), StorageName, StorageTy,
                                        VD->getDeclContext());
  Storage->setInterfaceType(StorageInterfaceTy);
  Storage->setUserAccessible(false);
  addMemberToContextIfNeeded(Storage, VD->getDeclContext(), VD);

  // Create the pattern binding decl for the storage decl.  This will get
  // default initialized to nil.
  Pattern *PBDPattern = new (Context) NamedPattern(Storage, /*implicit*/true);
  PBDPattern = new (Context) TypedPattern(PBDPattern,
                                          TypeLoc::withoutLoc(StorageTy),
                                          /*implicit*/true);
  auto *PBD = PatternBindingDecl::create(Context, /*staticloc*/SourceLoc(),
                                         StaticSpellingKind::None,
                                         /*varloc*/VD->getLoc(),
                                         PBDPattern, /*init*/nullptr,
                                         VD->getDeclContext());
  PBD->setImplicit();
  addMemberToContextIfNeeded(PBD, VD->getDeclContext());

  // Now that we've got the storage squared away, synthesize the getter.
  auto *Get = completeLazyPropertyGetter(VD, Storage, *this);
  validateDecl(Get);

  // The setter just forwards on to storage without materializing the initial
  // value.
  auto *Set = VD->getSetter();
  validateDecl(Set);
  VarDecl *SetValueDecl = getFirstParamDecl(Set);
  // FIXME: This is wrong for observed properties.
  synthesizeTrivialSetter(Set, Storage, SetValueDecl, *this);

  // Mark the vardecl to be final, implicit, and private.  In a class, this
  // prevents it from being dynamically dispatched.  Note that we do this after
  // the accessors are set up, because we don't want the setter for the lazy
  // property to inherit these properties from the storage.
  if (VD->getDeclContext()->getAsClassOrClassExtensionContext())
    makeFinal(Context, Storage);
  Storage->setImplicit();
  Storage->setAccessibility(Accessibility::Private);
  Storage->setSetterAccessibility(Accessibility::Private);
}


/// Consider add a materializeForSet accessor to the given storage
/// decl (which has accessors).
void swift::maybeAddMaterializeForSet(AbstractStorageDecl *storage,
                                      TypeChecker &TC) {
  assert(storage->hasAccessorFunctions());

  // Be idempotent.  There are a bunch of places where we want to
  // ensure that there's a materializeForSet accessor.
  if (storage->getMaterializeForSetFunc()) return;

  // Never add materializeForSet to readonly declarations.
  if (!storage->getSetter()) return;

  // Don't bother if the declaration is invalid.
  if (storage->isInvalid()) return;

  // We only need materializeForSet in type contexts.
  NominalTypeDecl *container = storage->getDeclContext()
      ->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!container) return;

  // Requirements of ObjC protocols don't need this.
  if (auto protocol = dyn_cast<ProtocolDecl>(container)) {
    if (protocol->isObjC()) return;

  // Types imported by Clang don't need this, because we can
  // synthesize it later.
  } else {
    assert(isa<NominalTypeDecl>(container));
    if (container->hasClangNode()) return;
  }

  addMaterializeForSet(storage, TC);
}

void swift::maybeAddAccessorsToVariable(VarDecl *var, TypeChecker &TC) {
  // If we've already synthesized accessors or are currently in the process
  // of doing so, don't proceed.
  if (var->getGetter() || var->isBeingTypeChecked())
    return;

  auto *dc = var->getDeclContext();

  assert(!var->hasAccessorFunctions());

  // Introduce accessors for a property with behaviors.
  if (var->hasBehavior()) {
    assert(!var->getBehavior()->Conformance.hasValue());
    
    // The property should be considered computed by the time we're through.
    SWIFT_DEFER {
      assert(!var->hasStorage() && "behavior var was not made computed");
    };
    
    var->setIsBeingTypeChecked();

    auto behavior = var->getMutableBehavior();
    NormalProtocolConformance *conformance = nullptr;
    VarDecl *valueProp = nullptr;

    bool mightBeMutating = dc->isTypeContext()
      && !var->isStatic()
      && !dc->getDeclaredInterfaceType()->getClassOrBoundGenericClass();

    auto makeBehaviorAccessors = [&]{
      FuncDecl *getter;
      FuncDecl *setter = nullptr;
      if (valueProp && valueProp->getGetter()) {
        getter = createGetterPrototype(var, TC);
        // The getter is mutating if the behavior implementation is, unless
        // we're in a class or non-instance context.
        getter->setMutating(mightBeMutating &&
                            valueProp->getGetter()->isMutating());
        getter->setAccessibility(var->getFormalAccess());
        
        // Make a setter if the behavior property has one.
        if (auto valueSetter = valueProp->getSetter()) {
          ParamDecl *newValueParam = nullptr;
          setter = createSetterPrototype(var, newValueParam, TC);
          setter->setMutating(mightBeMutating && valueSetter->isMutating());
          // TODO: max of property and implementation setter visibility?
          setter->setAccessibility(var->getFormalAccess());
        }
      } else {
        // Even if we couldn't find a value property, still make up a stub
        // getter and setter, so that subsequent diagnostics make sense for a
        // computed-ish property.
        getter = createGetterPrototype(var, TC);
        getter->setAccessibility(var->getFormalAccess());
        ParamDecl *newValueParam = nullptr;
        setter = createSetterPrototype(var, newValueParam, TC);
        setter->setMutating(false);
        setter->setAccessibility(var->getFormalAccess());
      }
      
      var->makeComputed(var->getLoc(), getter, setter, nullptr, var->getLoc());
      
      // Save the conformance and 'value' decl for later type checking.
      behavior->Conformance = conformance;
      behavior->ValueDecl = valueProp;
      var->setIsBeingTypeChecked(false);

      addMemberToContextIfNeeded(getter, dc);
      if (setter)
        addMemberToContextIfNeeded(setter, dc);
    };

    // Try to resolve the behavior to a protocol.
    auto behaviorType = TC.resolveType(behavior->ProtocolName, dc,
                                       TypeResolutionOptions());
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
        if (auto foundVar = dyn_cast<VarDecl>(found.Decl)) {
          if (valueProp) {
            TC.diagnose(behavior->getLoc(),
                        diag::property_behavior_protocol_reqt_ambiguous,
                        TC.Context.Id_value);
            TC.diagnose(valueProp->getLoc(),
                        diag::property_behavior_protocol_reqt_here,
                        TC.Context.Id_value);
            TC.diagnose(foundVar->getLoc(),
                        diag::property_behavior_protocol_reqt_here,
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
      
      // Set up a conformance to represent the behavior instantiation.
      // The conformance will be on the containing 'self' type, or '()' if the
      // property is in a non-type context.
      Type behaviorSelf;
      Type behaviorInterfaceSelf;
      if (dc->isTypeContext()) {
        behaviorSelf = dc->getSelfTypeInContext();
        behaviorInterfaceSelf = dc->getSelfInterfaceType();
        assert(behaviorSelf && "type context doesn't have self type?!");
        if (var->isStatic())
          behaviorSelf = MetatypeType::get(behaviorSelf);
      } else {
        behaviorSelf = behaviorInterfaceSelf = TC.Context.TheEmptyTupleType;
      }
      
      conformance = TC.Context.getBehaviorConformance(behaviorSelf,
                                            behaviorInterfaceSelf,
                                            behaviorProto,
                                            behavior->getLoc(), var,
                                            ProtocolConformanceState::Checking);
    }
    return makeBehaviorAccessors();
  }

  // Lazy properties require special handling.
  if (var->getAttrs().hasAttribute<LazyAttr>()) {
    var->setIsBeingTypeChecked();

    auto *getter = createGetterPrototype(var, TC);
    // lazy getters are mutating on an enclosing value type.
    if (!dc->getAsClassOrClassExtensionContext())
      getter->setMutating();
    getter->setAccessibility(var->getFormalAccess());

    ParamDecl *newValueParam = nullptr;
    auto *setter = createSetterPrototype(var, newValueParam, TC);
    var->makeComputed(var->getLoc(), getter, setter, nullptr,
                      var->getLoc());
    var->setIsBeingTypeChecked(false);

    TC.validateDecl(getter);
    TC.validateDecl(setter);

    addMemberToContextIfNeeded(getter, dc);
    addMemberToContextIfNeeded(setter, dc);
    return;
  }

  // Local variables don't otherwise get accessors.
  if (dc->isLocalContext())
    return;

  // Implicit properties don't get accessors.
  if (var->isImplicit())
    return;

  if (!dc->isTypeContext()) {
    // Fixed-layout global variables don't get accessors.
    if (var->hasFixedLayout())
      return;

  // In a protocol context, variables written as just "var x : Int" or
  // "let x : Int" are errors and recovered by building a computed property
  // with just a getter. Diagnose this and create the getter decl now.
  } else if (isa<ProtocolDecl>(dc)) {
    if (var->hasStorage()) {
      if (var->isLet())
        TC.diagnose(var->getLoc(),
                    diag::protocol_property_must_be_computed_var);
      else
        TC.diagnose(var->getLoc(), diag::protocol_property_must_be_computed);

      var->setIsBeingTypeChecked();
      convertStoredVarInProtocolToComputed(var, TC);
      var->setIsBeingTypeChecked(false);
    }
    return;

  // NSManaged properties on classes require special handling.
  } else if (dc->getAsClassOrClassExtensionContext()) {
    if (var->getAttrs().hasAttribute<NSManagedAttr>()) {
      var->setIsBeingTypeChecked();
      convertNSManagedStoredVarToComputed(var, TC);
      var->setIsBeingTypeChecked(false);
      return;
    }

  // Stored properties imported from Clang don't get accessors.
  } else if (auto *structDecl = dyn_cast<StructDecl>(dc)) {
    if (structDecl->hasClangNode())
      return;
  }

  // Stored properties in SIL mode don't get accessors.
  if (auto sourceFile = dc->getParentSourceFile())
    if (sourceFile->Kind == SourceFileKind::SIL)
      return;

  // Everything else gets accessors.
  var->setIsBeingTypeChecked();
  addTrivialAccessorsToStorage(var, TC);
  var->setIsBeingTypeChecked(false);
}

/// \brief Create an implicit struct or class constructor.
///
/// \param decl The struct or class for which a constructor will be created.
/// \param ICK The kind of implicit constructor to create.
///
/// \returns The newly-created constructor, which has already been type-checked
/// (but has not been added to the containing struct or class).
ConstructorDecl *swift::createImplicitConstructor(TypeChecker &tc,
                                                  NominalTypeDecl *decl,
                                                  ImplicitConstructorKind ICK) {
  ASTContext &context = tc.Context;
  SourceLoc Loc = decl->getLoc();
  auto accessLevel = Accessibility::Internal;
  if (decl->hasClangNode())
    accessLevel = std::max(accessLevel, decl->getFormalAccess());

  // Determine the parameter type of the implicit constructor.
  SmallVector<ParamDecl*, 8> params;
  if (ICK == ImplicitConstructorKind::Memberwise) {
    assert(isa<StructDecl>(decl) && "Only struct have memberwise constructor");

    // Computed and static properties are not initialized.
    for (auto var : decl->getStoredProperties()) {
      if (var->isImplicit())
        continue;
      tc.validateDecl(var);
      
      // Initialized 'let' properties have storage, but don't get an argument
      // to the memberwise initializer since they already have an initial
      // value that cannot be overridden.
      if (var->isLet() && var->getParentInitializer())
        continue;
      
      accessLevel = std::min(accessLevel, var->getFormalAccess());

      auto varType = tc.getTypeOfRValue(var);

      // If var is a lazy property, its value is provided for the underlying
      // storage.  We thus take an optional of the properties type.  We only
      // need to do this because the implicit constructor is added before all
      // the properties are type checked.  Perhaps init() synth should be moved
      // later.
      if (var->getAttrs().hasAttribute<LazyAttr>())
        varType = OptionalType::get(varType);

      // Create the parameter.
      auto *arg = new (context) ParamDecl(/*IsLet*/true, SourceLoc(), 
                                          Loc, var->getName(),
                                          Loc, var->getName(), varType, decl);
      arg->setImplicit();
      
      params.push_back(arg);
    }
  }

  auto paramList = ParameterList::create(context, params);
  
  // Create the constructor.
  DeclName name(context, context.Id_init, paramList);
  auto *selfParam = ParamDecl::createUnboundSelf(Loc, decl,
                                          /*static*/false, /*inout*/true);
  auto *ctor =
    new (context) ConstructorDecl(name, Loc,
                                  OTK_None, /*FailabilityLoc=*/SourceLoc(),
                                  /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                                  selfParam, paramList,
                                  nullptr, decl);

  // Mark implicit.
  ctor->setImplicit();
  ctor->setAccessibility(accessLevel);

  if (ICK == ImplicitConstructorKind::Memberwise)
    ctor->setIsMemberwiseInitializer();

  // If we are defining a default initializer for a class that has a superclass,
  // it overrides the default initializer of its superclass. Add an implicit
  // 'override' attribute.
  if (auto classDecl = dyn_cast<ClassDecl>(decl)) {
    if (classDecl->getSuperclass())
      ctor->getAttrs().add(new (tc.Context) OverrideAttr(/*implicit=*/true));
  }

  // Type-check the constructor declaration.
  tc.typeCheckDecl(ctor, /*isFirstPass=*/true);

  // If the struct in which this constructor is being added was imported,
  // add it as an external definition.
  if (decl->hasClangNode()) {
    tc.Context.addExternalDecl(ctor);
  }

  return ctor;
}

/// Create a stub body that emits a fatal error message.
static void createStubBody(TypeChecker &tc, ConstructorDecl *ctor) {
  auto unimplementedInitDecl = tc.Context.getUnimplementedInitializerDecl(&tc);
  auto classDecl = ctor->getExtensionType()->getClassOrBoundGenericClass();
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
                                  /*Implicit=*/true));

  // Note that this is a stub implementation.
  ctor->setStubImplementation(true);
}

ConstructorDecl *
swift::createDesignatedInitOverride(TypeChecker &tc,
                                    ClassDecl *classDecl,
                                    ConstructorDecl *superclassCtor,
                                    DesignatedInitKind kind) {
  // FIXME: Inheriting initializers that have their own generic parameters
  if (superclassCtor->getGenericParams())
    return nullptr;

  // Lookup will sometimes give us initializers that are from the ancestors of
  // our immediate superclass.  So, from the superclass constructor, we look
  // one level up to the enclosing type context which will either be a class
  // or an extension.  We can use the type declared in that context to check
  // if it's our immediate superclass and give up if we didn't.
  //
  // FIXME: Remove this when lookup of initializers becomes restricted to our
  // immediate superclass.
  Type superclassTyInCtor = superclassCtor->getDeclContext()->getDeclaredTypeOfContext();
  Type superclassTy = classDecl->getSuperclass();
  Type superclassTyInContext = ArchetypeBuilder::mapTypeIntoContext(
      classDecl, superclassTy);
  NominalTypeDecl *superclassDecl = superclassTy->getAnyNominal();
  if (superclassTyInCtor->getAnyNominal() != superclassDecl) {
    return nullptr;
  }

  // Determine the initializer parameters.
  auto &ctx = tc.Context;

  // Create the 'self' declaration and patterns.
  auto *selfDecl = ParamDecl::createUnboundSelf(SourceLoc(), classDecl);

  // Create the initializer parameter patterns.
  OptionSet<ParameterList::CloneFlags> options = ParameterList::Implicit;
  options |= ParameterList::Inherited;
  auto *bodyParams = superclassCtor->getParameterList(1)->clone(ctx,options);

  // If the superclass is generic, we need to map the superclass constructor's
  // parameter types into the generic context of our class.
  //
  // We might have to apply substitutions, if for example we have a declaration
  // like 'class A : B<Int>'.
  if (superclassDecl->isGenericContext()) {
    if (auto *superclassSig = superclassDecl->getGenericSignatureOfContext()) {
      auto *moduleDecl = classDecl->getParentModule();
      auto subs = superclassTyInContext->gatherAllSubstitutions(
          moduleDecl, nullptr, nullptr);
      auto subsMap = superclassSig->getSubstitutionMap(subs);

      for (auto *decl : *bodyParams) {
        // Map the parameter type to an interface type written in terms of
        // the superclass generic signature.
        auto paramTy = ArchetypeBuilder::mapTypeOutOfContext(
            superclassDecl, decl->getType());

        // Apply the superclass substitutions to produce a contextual
        // type in terms of the derived class archetypes.
        auto paramSubstTy = paramTy.subst(moduleDecl, subsMap, SubstOptions());
        decl->overwriteType(paramSubstTy);

        // Map it to an interface type in terms of the derived class
        // generic signature.
        decl->setInterfaceType(ArchetypeBuilder::mapTypeOutOfContext(
            classDecl, paramSubstTy));
      }
    }
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
                              selfDecl, bodyParams,
                              /*GenericParams=*/nullptr, classDecl);

  ctor->setImplicit();

  Accessibility access = classDecl->getFormalAccess();
  access = std::max(access, Accessibility::Internal);
  access = std::min(access, superclassCtor->getFormalAccess());
  ctor->setAccessibility(access);

  // Make sure the constructor is only as available as its superclass's
  // constructor.
  AvailabilityInference::applyInferredAvailableAttrs(ctor, superclassCtor, ctx);

  // Configure 'self'.
  auto selfType = configureImplicitSelf(tc, ctor);

  // Set the interface type of the initializer.
  ctor->setGenericSignature(classDecl->getGenericSignatureOfContext());
  tc.configureInterfaceType(ctor);

  // Set the contextual type of the initializer. This will go away one day.
  configureConstructorType(ctor, selfType, bodyParams->getType(ctx));

  if (superclassCtor->isObjC()) {
    // Inherit the @objc name from the superclass initializer, if it
    // has one.
    if (auto objcAttr = superclassCtor->getAttrs().getAttribute<ObjCAttr>()) {
      if (objcAttr->hasName()) {
        auto *clonedAttr = objcAttr->clone(ctx);
        // Set it to implicit to disable printing it for SIL.
        clonedAttr->setImplicit(true);
        ctor->getAttrs().add(clonedAttr);
      }
    }

    auto errorConvention = superclassCtor->getForeignErrorConvention();
    markAsObjC(tc, ctor, ObjCReason::ImplicitlyObjC, errorConvention);
  }
  if (superclassCtor->isRequired())
    ctor->getAttrs().add(new (tc.Context) RequiredAttr(/*implicit=*/true));

  // Wire up the overrides.
  ctor->getAttrs().add(new (tc.Context) OverrideAttr(/*Implicit=*/true));
  ctor->setOverriddenDecl(superclassCtor);

  if (kind == DesignatedInitKind::Stub) {
    // Make this a stub implementation.
    createStubBody(tc, ctor);
    return ctor;
  }

  // Form the body of a chaining designated initializer.
  assert(kind == DesignatedInitKind::Chaining);

  // Reference to super.init.
  Expr *superRef = new (ctx) SuperRefExpr(selfDecl, SourceLoc(),
                                          /*Implicit=*/true);
  Expr *ctorRef  = new (ctx) UnresolvedDotExpr(superRef, SourceLoc(),
                                               superclassCtor->getFullName(),
                                               DeclNameLoc(),
                                               /*Implicit=*/true);

  auto ctorArgs = buildArgumentForwardingExpr(bodyParams->getArray(), ctx);

  // If buildArgumentForwardingExpr failed, then it was because we tried to
  // forward varargs, which cannot be done yet.
  // TODO: We should be able to forward varargs!
  if (!ctorArgs) {
    tc.diagnose(classDecl->getLoc(),
                diag::unsupported_synthesize_init_variadic,
                classDecl->getDeclaredType());
    tc.diagnose(superclassCtor, diag::variadic_superclass_init_here);
    createStubBody(tc, ctor);
    return ctor;
  }

  Expr *superCall =
    CallExpr::create(ctx, ctorRef, ctorArgs,
                     superclassCtor->getFullName().getArgumentNames(), { },
                     /*hasTrailingClosure=*/false, /*implicit=*/true);
  if (superclassCtor->hasThrows()) {
    superCall = new (ctx) TryExpr(SourceLoc(), superCall, Type(),
                                  /*Implicit=*/true);
  }
  ctor->setBody(BraceStmt::create(tc.Context, SourceLoc(),
                                  ASTNode(superCall),
                                  SourceLoc(),
                                  /*Implicit=*/true));

  return ctor;
}

void TypeChecker::addImplicitDestructor(ClassDecl *CD) {
  if (CD->hasDestructor() || CD->isInvalid())
    return;

  auto *selfDecl = ParamDecl::createUnboundSelf(CD->getLoc(), CD);

  auto *DD = new (Context) DestructorDecl(Context.Id_deinit, CD->getLoc(),
                                          selfDecl, CD);

  DD->setImplicit();

  // Type-check the destructor declaration.
  typeCheckDecl(DD, /*isFirstPass=*/true);

  // Create an empty body for the destructor.
  DD->setBody(BraceStmt::create(Context, CD->getLoc(), { }, CD->getLoc(), true));
  CD->addMember(DD);
  CD->setHasDestructor();
}
