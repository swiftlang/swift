//===--- TypeCheckDecl.cpp - Type Checking for Declarations ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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
#include "swift/AST/Attr.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
using namespace swift;

const bool IsImplicit = true;

/// Insert the specified decl into the DeclContext's member list.  If the hint
/// decl is specified, the new decl is inserted next to the hint.
static void addMemberToContextIfNeeded(Decl *D, DeclContext *DC,
                                       Decl *Hint = nullptr) {
  if (auto *ntd = dyn_cast<NominalTypeDecl>(DC))
    ntd->addMember(D, Hint);
  else if (auto *ed = dyn_cast<ExtensionDecl>(DC))
    ed->addMember(D, Hint);
  else
    assert((isa<AbstractFunctionDecl>(DC) || isa<FileUnit>(DC)) &&
           "Unknown declcontext");
}

static VarDecl *getParamDeclAtIndex(FuncDecl *fn, unsigned index) {
  TuplePatternElt singleParam;
  Pattern *paramPattern = fn->getBodyParamPatterns().back();
  ArrayRef<TuplePatternElt> params;
  if (auto paramTuple = dyn_cast<TuplePattern>(paramPattern)) {
    params = paramTuple->getFields();
  } else {
    singleParam = TuplePatternElt(
                                  cast<ParenPattern>(paramPattern)->getSubPattern());
    params = singleParam;
  }

  auto firstParamPattern = params[index].getPattern();
  return firstParamPattern->getSingleVar();    
}

static VarDecl *getFirstParamDecl(FuncDecl *fn) {
  return getParamDeclAtIndex(fn, 0);
};

/// \brief Build an implicit 'self' parameter for the specified DeclContext.
static Pattern *buildImplicitSelfParameter(SourceLoc Loc, DeclContext *DC) {
  ASTContext &Ctx = DC->getASTContext();
  auto *SelfDecl = new (Ctx) ParamDecl(/*IsLet*/ true, Loc, Identifier(),
                                       Loc, Ctx.Id_self, Type(), DC);
  SelfDecl->setImplicit();
  Pattern *P = new (Ctx) NamedPattern(SelfDecl, /*Implicit=*/true);
  return new (Ctx) TypedPattern(P, TypeLoc());
}

static TuplePatternElt buildArgumentPattern(SourceLoc loc, DeclContext *DC,
                                            StringRef name, Type type,
                                            bool isLet,
                                            VarDecl **paramDecl,
                                            ASTContext &Context) {
  auto *param = new (Context) ParamDecl(isLet, SourceLoc(), Identifier(),
                                        loc, Context.getIdentifier(name),
                                        Type(), DC);
  if (paramDecl) *paramDecl = param;
  param->setImplicit();

  Pattern *valuePattern
    = new (Context) TypedPattern(new (Context) NamedPattern(param),
                                 TypeLoc::withoutLoc(type));
  valuePattern->setImplicit();
  
  return TuplePatternElt(valuePattern);
}

static TuplePatternElt buildLetArgumentPattern(SourceLoc loc, DeclContext *DC,
                                               StringRef name, Type type,
                                               VarDecl **paramDecl,
                                               ASTContext &ctx) {
  return buildArgumentPattern(loc, DC, name, type,
                              /*isLet*/ true, paramDecl, ctx);
}

static TuplePatternElt buildInOutArgumentPattern(SourceLoc loc, DeclContext *DC,
                                                 StringRef name, Type type,
                                                 VarDecl **paramDecl,
                                                 ASTContext &ctx) {
  return buildArgumentPattern(loc, DC, name, InOutType::get(type),
                              /*isLet*/ false, paramDecl, ctx);
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

static TuplePatternElt
buildSetterValueArgumentPattern(AbstractStorageDecl *storage,
                                VarDecl **valueDecl, TypeChecker &TC) {
  auto storageType = getTypeOfStorage(storage, TC);
  return buildLetArgumentPattern(storage->getLoc(),
                                 storage->getDeclContext(),
                                 "value", storageType, valueDecl, TC.Context);
}

/// Build a pattern which can forward the formal index parameters of a
/// declaration.
///
/// \param prefix optional arguments to be prefixed onto the index
///   forwarding pattern
static Pattern *buildIndexForwardingPattern(AbstractStorageDecl *storage,
                                     MutableArrayRef<TuplePatternElt> prefix,
                                            TypeChecker &TC) {
  auto subscript = dyn_cast<SubscriptDecl>(storage);

  // Fast path: if this isn't a subscript, and we have a first
  // pattern, we can just use that.
  if (!subscript) {
    auto tuple = TuplePattern::createSimple(TC.Context, SourceLoc(), prefix,
                                            SourceLoc());
    tuple->setImplicit();
    return tuple;
  }

  // Otherwise, we need to build up a new TuplePattern.
  SmallVector<TuplePatternElt, 4> elements;

  // Start with the fields from the first pattern, if there are any.
  elements.append(prefix.begin(), prefix.end());

  // Clone index patterns in a manner that allows them to be
  // perfectly forwarded.
  DeclContext *DC = storage->getDeclContext();
  auto addVarPatternFor = [&](Pattern *P) {
    Pattern *vp = P->cloneForwardable(TC.Context, DC, Pattern::Implicit);
    elements.push_back(TuplePatternElt(vp));
  };

  // This is the same breakdown the parser does.
  auto indices = subscript->getIndices();
  if (auto pp = dyn_cast<ParenPattern>(indices)) {
    addVarPatternFor(pp);
  } else {
    auto tp = cast<TuplePattern>(indices);
    for (auto &field : tp->getFields()) {
      addVarPatternFor(field.getPattern());
    }
  }

  return TuplePattern::createSimple(TC.Context, SourceLoc(), elements,
                                    SourceLoc());
}

static FuncDecl *createGetterPrototype(AbstractStorageDecl *storage,
                                       TypeChecker &TC) {
  SourceLoc loc = storage->getLoc();

  // Create the parameter list for the getter.
  SmallVector<Pattern *, 2> getterParams;

  // The implicit 'self' argument if in a type context.
  if (storage->getDeclContext()->isTypeContext())
    getterParams.push_back(
                  buildImplicitSelfParameter(loc, storage->getDeclContext()));
    
  // Add an index-forwarding clause.
  getterParams.push_back(buildIndexForwardingPattern(storage, {}, TC));

  SourceLoc staticLoc;
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (var->isStatic())
      staticLoc = var->getLoc();
  }

  auto storageType = getTypeOfStorage(storage, TC);

  auto getter = FuncDecl::create(
      TC.Context, staticLoc, StaticSpellingKind::None, loc, Identifier(), loc,
      /*GenericParams=*/nullptr, Type(), getterParams,
      TypeLoc::withoutLoc(storageType), storage->getDeclContext());
  getter->setImplicit();

  // Getters for truly stored properties default to non-mutating.
  // Getters for addressed properties follow the ordinary addressor.
  auto requiresMutatingGetter = [](const AbstractStorageDecl *storage) {
    switch (storage->getStorageKind()) {
    case AbstractStorageDecl::Stored:
    case AbstractStorageDecl::StoredWithObservers:
      return false;

    case AbstractStorageDecl::InheritedWithObservers:
      return storage->getOverriddenDecl()->getGetter()->isMutating();

    case AbstractStorageDecl::Addressed:
    case AbstractStorageDecl::AddressedWithObservers:
      return storage->getAddressor()->isMutating();

    case AbstractStorageDecl::ComputedWithMutableAddress:
    case AbstractStorageDecl::StoredWithTrivialAccessors:
    case AbstractStorageDecl::AddressedWithTrivialAccessors:
    case AbstractStorageDecl::Computed:      
      llvm_unreachable("shouldn't be synthesizing getter for storage"
                       " already in this state");
    }
    llvm_unreachable("bad storage kind!");
  };
  if (requiresMutatingGetter(storage)) {
    getter->setMutating();
  }

  // If the var is marked final, then so is the getter.
  if (storage->isFinal())
    makeFinal(TC.Context, getter);

  if (storage->isStatic())
    getter->setStatic();

  return getter;
}

static FuncDecl *createSetterPrototype(AbstractStorageDecl *storage,
                                       VarDecl *&valueDecl,
                                       TypeChecker &TC) {
  SourceLoc loc = storage->getLoc();

  // Create the parameter list for the setter.
  SmallVector<Pattern *, 2> params;

  // The implicit 'self' argument if in a type context.
  if (storage->getDeclContext()->isTypeContext()) {
    params.push_back(
                  buildImplicitSelfParameter(loc, storage->getDeclContext()));
  }

  // Add a "(value : T, indices...)" pattern.
  TuplePatternElt valuePattern =
    buildSetterValueArgumentPattern(storage, &valueDecl, TC);
  params.push_back(buildIndexForwardingPattern(storage, valuePattern, TC));

  Type setterRetTy = TupleType::getEmpty(TC.Context);
  FuncDecl *setter = FuncDecl::create(
      TC.Context, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None, loc,
      Identifier(), loc, /*generic=*/nullptr, Type(), params,
      TypeLoc::withoutLoc(setterRetTy), storage->getDeclContext());
  setter->setImplicit();

  // Setters for truly stored properties default to mutating.
  // Setters for addressed properties follow the mutable addressor.
  auto requiresMutatingSetter = [](const AbstractStorageDecl *storage) {
    switch (storage->getStorageKind()) {
    case AbstractStorageDecl::Stored:
    case AbstractStorageDecl::StoredWithObservers:
      return storage->isInstanceMember();

    case AbstractStorageDecl::InheritedWithObservers:
      return storage->getOverriddenDecl()->getSetter()->isMutating();

    case AbstractStorageDecl::Addressed:
    case AbstractStorageDecl::AddressedWithObservers:
      return storage->getMutableAddressor()->isMutating();

    case AbstractStorageDecl::ComputedWithMutableAddress:
    case AbstractStorageDecl::StoredWithTrivialAccessors:
    case AbstractStorageDecl::AddressedWithTrivialAccessors:
    case AbstractStorageDecl::Computed:      
      llvm_unreachable("shouldn't be synthesizing setter for storage"
                       " already in this state");
    }
    llvm_unreachable("bad storage kind!");
  };
  if (requiresMutatingSetter(storage)) {
    setter->setMutating();
  }

  // If the var is marked final, then so is the getter.
  if (storage->isFinal())
    makeFinal(TC.Context, setter);

  if (storage->isStatic())
    setter->setStatic();

  return setter;
}

/// Returns the type of the self argument of a materializeForSet
/// callback.  If we don't have a meaningful direct self type, just
/// use something meaningless and hope it doesn't matter.
static Type getSelfTypeForMaterializeForSetCallback(ASTContext &ctx,
                                                    DeclContext *DC,
                                                    bool isStatic) {
  Type selfType = DC->getDeclaredTypeInContext();
  if (!selfType) {
    // This restriction is theoretically liftable by writing the necessary
    // contextual information into the callback storage.
    assert(!DC->isGenericContext() &&
           "no enclosing type for generic materializeForSet; callback "
           "will not be able to bind type arguments!");
    return TupleType::getEmpty(ctx);
  }

  // If we're in a protocol, we want to actually use the Self type.
  if (auto protocolType = selfType->getAs<ProtocolType>()) {
    selfType = protocolType->getDecl()->getSelf()->getArchetype();
  }

  // Use the metatype if this is a static member.
  if (isStatic) {
    return MetatypeType::get(selfType, ctx);
  } else {
    return selfType;
  }
}

// True if the storage is dynamic or imported from Objective-C. In these cases,
// we need to emit a static materializeForSet thunk that dynamically dispatches
// to 'get' and 'set', rather than the normal dynamically dispatched
// materializeForSet that peer dispatches to 'get' and 'set'.
static bool needsDynamicMaterializeForSet(AbstractStorageDecl *storage) {
  return storage->isDynamic() || storage->hasClangNode();
}

static FuncDecl *createMaterializeForSetPrototype(AbstractStorageDecl *storage,
                                                  VarDecl *&bufferParamDecl,
                                                  TypeChecker &TC) {
  auto &ctx = storage->getASTContext();
  SourceLoc loc = storage->getLoc();

  // Create the parameter list:
  SmallVector<Pattern *, 2> params;

  //  - The implicit 'self' argument if in a type context.
  auto DC = storage->getDeclContext();
  if (DC->isTypeContext())
    params.push_back(buildImplicitSelfParameter(loc, DC));

  //  - The buffer parameter, (buffer: Builtin.RawPointer,
  //                           inout storage: Builtin.UnsafeValueBuffer,
  //                           indices...).
  TuplePatternElt bufferElements[] = {
    buildLetArgumentPattern(loc, DC, "buffer", ctx.TheRawPointerType,
                            &bufferParamDecl, TC.Context),
    buildInOutArgumentPattern(loc, DC, "callbackStorage",
                              ctx.TheUnsafeValueBufferType,
                              nullptr, TC.Context),
  };
  params.push_back(buildIndexForwardingPattern(storage, bufferElements, TC));

  // Construct the callback type.
  Type callbackSelfType =
    getSelfTypeForMaterializeForSetCallback(ctx, DC, storage->isStatic());
  TupleTypeElt callbackArgs[] = {
    ctx.TheRawPointerType,
    InOutType::get(ctx.TheUnsafeValueBufferType),
    InOutType::get(callbackSelfType),
    MetatypeType::get(callbackSelfType, MetatypeRepresentation::Thick),
  };
  auto callbackExtInfo = FunctionType::ExtInfo()
    .withRepresentation(FunctionType::Representation::Thin);
  auto callbackType = FunctionType::get(TupleType::get(callbackArgs, ctx),
                                        TupleType::getEmpty(ctx),
                                        callbackExtInfo);

  // Try to make the callback type optional.  Don't crash if it doesn't
  // work, though.
  auto optCallbackType = TC.getOptionalType(loc, callbackType);
  if (!optCallbackType) optCallbackType = callbackType;

  // The accessor returns (Builtin.RawPointer, (@thin (...) -> ())?),
  // where the first pointer is the materialized address and the
  // second is an optional callback.
  TupleTypeElt retElts[] = {
    { ctx.TheRawPointerType },
    { optCallbackType },
  };
  Type retTy = TupleType::get(retElts, ctx);

  auto *materializeForSet = FuncDecl::create(
      ctx, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None, loc,
      Identifier(), loc, /*generic=*/nullptr, Type(), params,
      TypeLoc::withoutLoc(retTy), DC);
  materializeForSet->setImplicit();
  
  // materializeForSet is mutating and static if the setter is.
  auto setter = storage->getSetter();
  materializeForSet->setMutating(setter->isMutating());
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
  
  // If the property came from ObjC, we need to register this as an external
  // definition to be compiled.
  if (storage->hasClangNode())
    TC.Context.addedExternalDecl(materializeForSet);
  
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

/// Build a tuple around the given arguments.
static Expr *buildTupleExpr(ASTContext &ctx, ArrayRef<Expr*> args) {
  if (args.size() == 1) {
    return args[0];
  }
  SmallVector<Identifier, 4> labels(args.size());
  SmallVector<SourceLoc, 4> labelLocs(args.size());
  return TupleExpr::create(ctx, SourceLoc(), args, labels, labelLocs,
                           SourceLoc(), false, IsImplicit);
}


static Expr *buildTupleForwardingRefExpr(ASTContext &ctx,
                                         ArrayRef<TuplePatternElt> params,
                                    ArrayRef<TupleTypeElt> formalIndexTypes) {
  assert(params.size() == formalIndexTypes.size());

  SmallVector<Identifier, 4> labels;
  SmallVector<SourceLoc, 4> labelLocs;
  SmallVector<Expr *, 4> args;

  for (unsigned i = 0, e = params.size(); i != e; ++i) {
    const Pattern *param = params[i].getPattern();
    args.push_back(param->buildForwardingRefExpr(ctx));
    labels.push_back(formalIndexTypes[i].getName());
    labelLocs.push_back(SourceLoc());
  }

  // A single unlabelled value is not a tuple.
  if (args.size() == 1 && labels[0].empty())
    return args[0];

  return TupleExpr::create(ctx, SourceLoc(), args, labels, labelLocs,
                           SourceLoc(), false, IsImplicit);
}

/// Build a reference to the subscript index variables for this
/// subscript accessor.
static Expr *buildSubscriptIndexReference(ASTContext &ctx, FuncDecl *accessor) {
  // Pull out the body parameters, which we should have cloned
  // previously to be forwardable.  Drop the initial buffer/value
  // parameter in accessors that have one.
  TuplePatternElt singleParam;
  Pattern *paramPattern = accessor->getBodyParamPatterns().back();
  ArrayRef<TuplePatternElt> params;
  if (auto paramTuple = dyn_cast<TuplePattern>(paramPattern)) {
    params = paramTuple->getFields();
  } else {
    singleParam = TuplePatternElt(
                    cast<ParenPattern>(paramPattern)->getSubPattern());
    params = singleParam;
  }
  auto accessorKind = accessor->getAccessorKind();

  // Ignore the value/buffer parameter.
  if (accessorKind != AccessorKind::IsGetter)
    params = params.slice(1);

  // Ignore the materializeForSet callback storage parameter.
  if (accessorKind == AccessorKind::IsMaterializeForSet)
    params = params.slice(1);

  // Look for formal subscript labels.
  auto subscript = cast<SubscriptDecl>(accessor->getAccessorStorageDecl());
  auto indexType = subscript->getIndicesType();
  if (auto indexTuple = indexType->getAs<TupleType>()) {
    return buildTupleForwardingRefExpr(ctx, params, indexTuple->getFields());
  } else {
    return buildTupleForwardingRefExpr(ctx, params, TupleTypeElt(indexType));
  }
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
    return new (TC.Context) DeclRefExpr(selfDecl, SourceLoc(), IsImplicit);

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
    return new (ctx) DeclRefExpr(storage, SourceLoc(), IsImplicit, semantics);
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
    return new (ctx) SubscriptExpr(selfDRE, indices, ConcreteDeclRef(),
                                   IsImplicit, semantics);
  }

  // This is a potentially polymorphic access, which is unnecessary;
  // however, it shouldn't be problematic because any overrides
  // should also redefine materializeForSet.
  return new (ctx) MemberRefExpr(selfDRE, SourceLoc(), storage,
                                 SourceLoc(), IsImplicit, semantics);
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
  auto foundation = ctx.getLoadedModule(ctx.getIdentifier("Foundation"));
  if (!foundation)
    return nullptr;

  SmallVector<ValueDecl *, 2> results;
  DC->lookupQualified(ModuleType::get(foundation),
                      ctx.getIdentifier("NSCopying"),
                      NL_QualifiedDefault | NL_KnownNonCascadingDependency,
                      /*resolver=*/nullptr,
                      results);

  if (results.size() != 1)
    return nullptr;

  return dyn_cast<ProtocolDecl>(results.front());
}



/// Synthesize the code to store 'Val' to 'VD', given that VD has an @NSCopying
/// attribute on it.  We know that VD is a stored property in a class, so we
/// just need to generate something like "self.property = val.copyWithZone(nil)"
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
                                              VD->getDeclContext(), false)) {
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
  //     (unresolved_dot_expr type='<null>' field 'copyWithZone'
  //       "Val")
  //     (paren_expr type='<null>'
  //       (nil_literal_expr type='<null>'))))
  auto UDE = new (Ctx) UnresolvedDotExpr(Val, SourceLoc(),
                                         Ctx.getIdentifier("copyWithZone"),
                                         SourceLoc(), /*implicit*/true);
  Expr *Nil = new (Ctx) NilLiteralExpr(SourceLoc(), /*implicit*/true);
  Nil = new (Ctx) ParenExpr(SourceLoc(), Nil, SourceLoc(), false);

  //- (id)copyWithZone:(NSZone *)zone;
  Expr *Call = new (Ctx) CallExpr(UDE, Nil, /*implicit*/true);

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


/// Synthesize the body of a trivial getter.  For a non-member vardecl or one
/// which is not an override of a base class property, it performs a a direct
/// storage load.  For an override of a base member property, it chains up to
/// super.
static void synthesizeTrivialGetter(FuncDecl *getter,
                                    AbstractStorageDecl *storage,
                                    TypeChecker &TC) {
  auto &ctx = TC.Context;
  
  Expr *result = createPropertyLoadOrCallSuperclassGetter(getter, storage, TC);
  ASTNode returnStmt = new (ctx) ReturnStmt(SourceLoc(), result, IsImplicit);

  SourceLoc loc = storage->getLoc();
  getter->setBody(BraceStmt::create(ctx, loc, returnStmt, loc));

  // Mark it transparent, there is no user benefit to this actually existing, we
  // just want it for abstraction purposes (i.e., to make access to the variable
  // uniform and to be able to put the getter in a vtable).
  getter->getAttrs().add(new (ctx) TransparentAttr(IsImplicit));
}

/// Synthesize the body of a trivial setter.
static void synthesizeTrivialSetter(FuncDecl *setter,
                                    AbstractStorageDecl *storage,
                                    VarDecl *valueVar,
                                    TypeChecker &TC) {
  if (storage->isInvalid()) return;

  auto &ctx = TC.Context;
  SourceLoc loc = storage->getLoc();

  auto *valueDRE = new (ctx) DeclRefExpr(valueVar, SourceLoc(), IsImplicit);
  SmallVector<ASTNode, 1> setterBody;
  createPropertyStoreOrCallSuperclassSetter(setter, valueDRE, storage,
                                            setterBody, TC);
  setter->setBody(BraceStmt::create(ctx, loc, setterBody, loc));

  // Mark it transparent, there is no user benefit to this actually existing.
  setter->getAttrs().add(new (ctx) TransparentAttr(IsImplicit));
}

/// Build the result expression of a materializeForSet accessor.
///
/// \param address an expression yielding the address to return
/// \param callbackFn an optional closure expression for the callback
static Expr *buildMaterializeForSetResult(ASTContext &ctx, Expr *address,
                                          Expr *callbackFn) {
  if (!callbackFn) {
    callbackFn = new (ctx) NilLiteralExpr(SourceLoc(), IsImplicit);
  }

  return TupleExpr::create(ctx, SourceLoc(), { address, callbackFn },
                           { Identifier(), Identifier() },
                           { SourceLoc(), SourceLoc() },
                           SourceLoc(), false, IsImplicit);
}

/// Create a call to the builtin function with the given name.
static Expr *buildCallToBuiltin(ASTContext &ctx, StringRef builtinName,
                                ArrayRef<Expr*> args) {
  auto builtin = getBuiltinValueDecl(ctx, ctx.getIdentifier(builtinName));
  Expr *builtinDRE = new (ctx) DeclRefExpr(builtin, SourceLoc(), IsImplicit);
  Expr *arg = buildTupleExpr(ctx, args);
  return new (ctx) CallExpr(builtinDRE, arg, IsImplicit);
}

/// Synthesize the body of a materializeForSet accessor for a stored
/// property.
static void synthesizeStoredMaterializeForSet(FuncDecl *materializeForSet,
                                              AbstractStorageDecl *storage,
                                              VarDecl *bufferDecl,
                                              TypeChecker &TC) {
  ASTContext &ctx = TC.Context;

  // return (Builtin.addressof(&self.property), nil)
  Expr *result = buildStorageReference(materializeForSet, storage,
                                       AccessSemantics::DirectToStorage,
                                       SelfAccessKind::Peer, TC);
  result = new (ctx) InOutExpr(SourceLoc(), result, Type(), IsImplicit);
  result = buildCallToBuiltin(ctx, "addressof", result);
  result = buildMaterializeForSetResult(ctx, result, /*callback*/ nullptr);

  ASTNode returnStmt = new (ctx) ReturnStmt(SourceLoc(), result, IsImplicit);

  SourceLoc loc = storage->getLoc();
  materializeForSet->setBody(BraceStmt::create(ctx, loc, returnStmt, loc));

  // Mark it transparent, there is no user benefit to this actually existing.
  materializeForSet->getAttrs().add(new (ctx) TransparentAttr(IsImplicit));

  TC.typeCheckDecl(materializeForSet, true);
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
  VarDecl *bufferDecl;
  auto materializeForSet =
    createMaterializeForSetPrototype(storage, bufferDecl, TC);
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

  // Create the setter.
  FuncDecl *setter = nullptr;
  VarDecl *setterValueParam = nullptr;
  if (doesStorageNeedSetter(storage)) {
    setter = createSetterPrototype(storage, setterValueParam, TC);
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

  // We've added some members to our containing type, add them to the
  // members list.
  addMemberToContextIfNeeded(getter, storage->getDeclContext());
  if (setter)
    addMemberToContextIfNeeded(setter, storage->getDeclContext());

  // Always add a materializeForSet when we're creating trivial
  // accessors for a mutable stored property.  We only do this when we
  // need to be able to access something polymorphicly, and we always
  // want a materializeForSet in such situations.
  if (setter) {
    FuncDecl *materializeForSet = addMaterializeForSet(storage, TC);
    synthesizeMaterializeForSet(materializeForSet, storage, TC);
    TC.typeCheckDecl(materializeForSet, true);
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

/// The specified AbstractStorageDecl was just found to satisfy a
/// protocol property requirement.  Ensure that it has the full
/// complement of accessors.
void TypeChecker::synthesizeWitnessAccessorsForStorage(
                                             AbstractStorageDecl *requirement,
                                             AbstractStorageDecl *storage) {
  // If the decl is stored, convert it to StoredWithTrivialAccessors
  // by synthesizing the full set of accessors.
  if (!storage->hasAccessorFunctions()) {
    addTrivialAccessorsToStorage(storage, *this);
    return;
  }
  
  // Otherwise, if the requirement is settable, ensure that there's a
  // materializeForSet function.
  //
  // @objc protocols don't need a materializeForSet since ObjC doesn't have
  // that concept.
  if (!requirement->isObjC() &&
      requirement->getSetter() && !storage->getMaterializeForSetFunc()) {
    FuncDecl *materializeForSet = addMaterializeForSet(storage, *this);
    synthesizeMaterializeForSet(materializeForSet, storage, *this);
    typeCheckDecl(materializeForSet, true);
    typeCheckDecl(materializeForSet, false);
  }
  return;
}

using CallbackGenerator =
  llvm::function_ref<void(SmallVectorImpl<ASTNode> &callbackBody,
                          VarDecl *selfDecl, VarDecl *bufferDecl,
                          VarDecl *callbackStorageDecl)>;

/// Build a materializeForSet callback function.
/// It should have type
///   (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer,
///    inout T, T.Type) -> ()
static Expr *buildMaterializeForSetCallback(ASTContext &ctx,
                                            FuncDecl *materializeForSet,
                                            AbstractStorageDecl *storage,
                                      const CallbackGenerator &generator) {

  auto DC = storage->getDeclContext();
  SourceLoc loc = storage->getLoc();

  Type selfType =
    getSelfTypeForMaterializeForSetCallback(ctx, DC,
                                            materializeForSet->isStatic());

  // Build the parameters pattern.
  //
  // Unexpected subtlety: it actually important to call the inout self
  // parameter something other than 'self' so that we don't trigger
  // the "implicit use of self" diagnostic.
  VarDecl *bufferDecl;
  VarDecl *callbackStorageDecl;
  VarDecl *selfDecl;
  TuplePatternElt argPatterns[] = {
    buildLetArgumentPattern(loc, DC, "buffer", ctx.TheRawPointerType,
                            &bufferDecl, ctx),
    buildInOutArgumentPattern(loc, DC, "callbackStorage",
                              ctx.TheUnsafeValueBufferType,
                              &callbackStorageDecl, ctx),
    buildInOutArgumentPattern(loc, DC, "selfValue", selfType, &selfDecl, ctx),
    buildLetArgumentPattern(loc, DC, "selfType", MetatypeType::get(selfType),
                            nullptr, ctx),
  };
  auto args = TuplePattern::createSimple(ctx, SourceLoc(), argPatterns,
                                         SourceLoc());
  args->setImplicit();

  // Create the closure expression itself.
  auto closure = new (ctx) ClosureExpr(args, SourceLoc(), SourceLoc(),
                                       TypeLoc(), /*discriminator*/ 0,
                                       materializeForSet);

  // Generate the body of the closure.
  SmallVector<ASTNode, 4> body;
  generator(body, selfDecl, bufferDecl, callbackStorageDecl);
  closure->setBody(BraceStmt::create(ctx, SourceLoc(), body, SourceLoc(),
                                     IsImplicit),
                   /*isSingleExpression*/ false);
  closure->setImplicit(IsImplicit);

  // Call our special builtin to turn that into an opaque thin function.
  auto result = buildCallToBuiltin(ctx, "makeMaterializeForSetCallback",
                                   { closure });
  return result;
}

/// Build a builtin operation on a Builtin.UnsafeValueBuffer.
static Expr *buildValueBufferOperation(ASTContext &ctx, StringRef builtinName,
                                       VarDecl *bufferDecl, Type valueType) {
  // &buffer
  Expr *bufferRef = new (ctx) DeclRefExpr(bufferDecl, SourceLoc(), IsImplicit);
  bufferRef = new (ctx) InOutExpr(SourceLoc(), bufferRef, Type(), IsImplicit);

  // T.self
  Expr *metatypeRef = TypeExpr::createImplicit(valueType, ctx);
  metatypeRef = new (ctx) DotSelfExpr(metatypeRef, SourceLoc(), SourceLoc());
  metatypeRef->setImplicit(IsImplicit);

  // Builtin.whatever(&buffer, T.self)
  return buildCallToBuiltin(ctx, builtinName, { bufferRef, metatypeRef });
}

/// Build a call to Builtin.take.
static Expr *buildBuiltinTake(ASTContext &ctx, Expr *address,
                              Type valueType) {
  // Builtin.take(address) as ValueType
  Expr *result = buildCallToBuiltin(ctx, "take", { address });
  result = new (ctx) CoerceExpr(result, SourceLoc(),
                                TypeLoc::withoutLoc(valueType));
  result->setImplicit(IsImplicit);
  return result;
}

/// Build an expression to initialize callback storage.
static ASTNode buildInitializeCallbackStorage(FuncDecl *materializeForSet,
                                              Expr *initializer,
                                              Type initializerType,
                                              ASTContext &ctx) {
  // let allocatedCallbackStorage =
  //   Builtin.allocateValueBuffer(&callbackStorage, IndexType.self))
  VarDecl *callbackStorageDecl = getParamDeclAtIndex(materializeForSet, 1);
  Expr *allocatedCallbackStorage =
    buildValueBufferOperation(ctx, "allocValueBuffer", callbackStorageDecl,
                              initializerType);

  // Builtin.initialize(indexArgs, allocatedCallbackStorage)
  return buildCallToBuiltin(ctx, "initialize",
                            { initializer, allocatedCallbackStorage });
}

/// Build an expression to take from callback storage.
static Expr *buildTakeFromCallbackStorage(VarDecl *storage, Type valueType,
                                          ASTContext &ctx) {
  Expr *address =
    buildValueBufferOperation(ctx, "projectValueBuffer", storage, valueType);
  return buildBuiltinTake(ctx, address, valueType);
}

namespace {
  /// A reference to storage from the context of a materializeForSet
  /// callback.
  class CallbackStorageReferenceContext : public StorageReferenceContext {
    VarDecl *Self;
    VarDecl *CallbackStorage;
  public:
    CallbackStorageReferenceContext(VarDecl *self, VarDecl *callbackStorage)
      : Self(self), CallbackStorage(callbackStorage) {}
    virtual ~CallbackStorageReferenceContext() = default;

    VarDecl *getSelfDecl() const override {
      return Self;
    }
    Expr *getIndexRefExpr(ASTContext &ctx,
                          SubscriptDecl *subscript) const override {
      return buildTakeFromCallbackStorage(CallbackStorage,
                                          subscript->getIndicesType(), ctx);
    }
  };
}

/// Synthesize the body of a materializeForSet accessor for a
/// computed property.
static void synthesizeComputedMaterializeForSet(FuncDecl *materializeForSet,
                                                AbstractStorageDecl *storage,
                                                VarDecl *bufferDecl,
                                                TypeChecker &TC) {
  ASTContext &ctx = TC.Context;

  SmallVector<ASTNode, 4> body;
  
  AccessSemantics semantics;
  // If the storage is dynamic, we must dynamically redispatch through the
  // accessor. Otherwise, we can do a direct peer access.
  if (needsDynamicMaterializeForSet(storage))
    semantics = AccessSemantics::Ordinary;
  else
    semantics = AccessSemantics::DirectToAccessor;

  // Builtin.initialize(self.property, buffer)
  Expr *curValue = buildStorageReference(materializeForSet, storage,
                                         semantics,
                                         SelfAccessKind::Peer, TC);
  Expr *bufferRef = new (ctx) DeclRefExpr(bufferDecl, SourceLoc(), IsImplicit);
  body.push_back(buildCallToBuiltin(ctx, "initialize",
                                    { curValue, bufferRef }));

  // If this is a subscript, preserve the index value:
  if (auto subscript = dyn_cast<SubscriptDecl>(storage)) {
    Expr *indices = buildSubscriptIndexReference(ctx, materializeForSet);
    ASTNode initialize =
      buildInitializeCallbackStorage(materializeForSet, indices,
                                     subscript->getIndicesType(), ctx);
    body.push_back(initialize);
  }

  // Build the callback.
  Expr *callback =
    buildMaterializeForSetCallback(ctx, materializeForSet, storage,
      [&](SmallVectorImpl<ASTNode> &body,
          VarDecl *selfDecl, VarDecl *bufferDecl,
          VarDecl *callbackStorageDecl) {
      // self.property = Builtin.take(buffer)
      Expr *bufferRef =
        new (ctx) DeclRefExpr(bufferDecl, SourceLoc(), IsImplicit);
      Expr *value = buildBuiltinTake(ctx, bufferRef,
                                     getTypeOfStorage(storage, TC));

      Expr *storageRef =
        buildStorageReference(
               CallbackStorageReferenceContext{selfDecl, callbackStorageDecl},
                              storage, semantics,
                              SelfAccessKind::Peer, TC);
      body.push_back(new (ctx) AssignExpr(storageRef, SourceLoc(),
                                          value, IsImplicit));

      // If this is a subscript, deallocate the subscript buffer:
      if (auto subscript = dyn_cast<SubscriptDecl>(storage)) {
        // Builtin.deallocValueBuffer(&callbackStorage, IndexType.self)
        body.push_back(buildValueBufferOperation(ctx, "deallocValueBuffer",
                                                 callbackStorageDecl,
                                                 subscript->getIndicesType()));
      }
    });

  // return (buffer, callback)
  Expr *result = new (ctx) DeclRefExpr(bufferDecl, SourceLoc(), IsImplicit);

  result = buildMaterializeForSetResult(ctx, result, callback);
  body.push_back(new (ctx) ReturnStmt(SourceLoc(), result, IsImplicit));

  SourceLoc loc = storage->getLoc();
  materializeForSet->setBody(BraceStmt::create(ctx, loc, body, loc));

  // Mark it transparent, there is no user benefit to this actually existing.
  materializeForSet->getAttrs().add(new (ctx) TransparentAttr(IsImplicit));

  TC.typeCheckDecl(materializeForSet, true);
}

/// Build a direct call to an addressor from within a
/// materializeForSet method.
static Expr *buildCallToAddressor(FuncDecl *materializeForSet,
                                  AbstractStorageDecl *storage,
                                  VarDecl *bufferDecl,
                                  FuncDecl *addressor,
                                  ASTContext &ctx) {
  // Build a direct reference to the addressor.
  Expr *fn;

  // Apply the self argument if applicable.
  if (auto self = materializeForSet->getImplicitSelfDecl()) {
    Expr *selfRef = new (ctx) DeclRefExpr(self, SourceLoc(), IsImplicit);
    // if (addressor->computeSelfType(nullptr)->is<LValueType>()) {
    //   selfRef = new (ctx) InOutExpr(SourceLoc(), selfRef, Type(), IsImplicit);
    // }

    ValueDecl *localMembers[] = { addressor };
    fn = new (ctx) OverloadedMemberRefExpr(selfRef, SourceLoc(),
                                           ctx.AllocateCopy(localMembers),
                                           SourceLoc(), IsImplicit, Type(),
                                           AccessSemantics::DirectToStorage);
  } else {
    fn = new (ctx) DeclRefExpr(addressor, SourceLoc(), IsImplicit,
                               AccessSemantics::DirectToStorage);
  }

  // Apply the rest of the addressor arguments.
  Expr *args;
  if (isa<SubscriptDecl>(storage)) {
    args = buildSubscriptIndexReference(ctx, materializeForSet);
  } else {
    args = TupleExpr::createImplicit(ctx, {}, {});
  }

  return new (ctx) CallExpr(fn, args, IsImplicit);
}

/// Given an expression of type UnsafeMutablePointer<T>, create an
/// expression of type Builtin.RawPointer.
static Expr *buildUnsafeMutablePointerToRawPointer(Expr *operand,
                                                   ASTContext &ctx) {
  // Just directly drill in.
  NominalTypeDecl *ptrType = ctx.getUnsafeMutablePointerDecl();
  
  // If that doesn't work, just bail out; the result probably won't
  // type-check, but there are worse failure modes for a broken stdlib.
  if (!ptrType) return operand;
  auto props = ptrType->getStoredProperties();
  if (props.begin() == props.end()) return operand;

  auto storageProp = props.front();
  return new (ctx) MemberRefExpr(operand, SourceLoc(), storageProp,
                                 SourceRange(), IsImplicit);
}

/// Synthesize the body of a materializeForSet accessor for an
/// addressed property.
static void synthesizeAddressedMaterializeForSet(FuncDecl *materializeForSet,
                                                 AbstractStorageDecl *storage,
                                                 VarDecl *bufferDecl,
                                                 TypeChecker &TC) {
  ASTContext &ctx = TC.Context;

  SmallVector<ASTNode, 4> body;

  // Call the mutable addressor.
  auto addressor = storage->getMutableAddressor();
  Expr *addressorResult = buildCallToAddressor(materializeForSet, storage,
                                               bufferDecl, addressor, ctx);

  Expr *result;
  Expr *callback;
  switch (addressor->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("addressor is not an addressor?");

  // If we have an unsafe addressor, this is easy: we just pull out
  // the raw pointer and use that as the result.
  case AddressorKind::Unsafe:
    result = buildUnsafeMutablePointerToRawPointer(addressorResult, ctx);
    callback = nullptr;
    break;

  case AddressorKind::Owning:
  case AddressorKind::NativeOwning:
  case AddressorKind::NativePinning: {
    // We need to bind the result to a temporary variable.
    //   let temporary = addressor(self)(indices)
    auto tempDecl = new (ctx) VarDecl(/*static*/ false, /*let*/ true,
                                      SourceLoc(), ctx.getIdentifier("tmp"),
                                      Type(), materializeForSet);
    tempDecl->setImplicit(IsImplicit);
    auto bindingPattern = new (ctx) NamedPattern(tempDecl, IsImplicit);
    auto bindingDecl = new (ctx) PatternBindingDecl(/*static*/ SourceLoc(),
                                                    StaticSpellingKind::None,
                                                    SourceLoc(),
                                                    bindingPattern,
                                                    addressorResult,
                                                    /*conditional*/ false,
                                                    materializeForSet);
    bindingDecl->setImplicit(IsImplicit);
    body.push_back(bindingDecl);
    body.push_back(tempDecl);

    // This should be Builtin.NativePointer or something like it.
    Type ownerType = [&]() -> Type {
      switch (addressor->getAddressorKind()){
      case AddressorKind::NotAddressor:
      case AddressorKind::Unsafe:
        llvm_unreachable("filtered out");
      case AddressorKind::Owning:
        return ctx.TheUnknownObjectType;
      case AddressorKind::NativeOwning:
        return ctx.TheNativeObjectType;
      case AddressorKind::NativePinning:
        return OptionalType::get(ctx.TheNativeObjectType);
      }
      llvm_unreachable("bad addressor kind");
    }();

    // Initialize the callback storage with the owner value, which is
    // the second elemenet of the addressor result.
    Expr *owner = new (ctx) DeclRefExpr(tempDecl, SourceLoc(), IsImplicit);
    owner = new (ctx) TupleElementExpr(owner, SourceLoc(), /*field index*/ 1,
                                       SourceLoc(), Type());
    owner->setImplicit(IsImplicit);
    body.push_back(buildInitializeCallbackStorage(materializeForSet, owner,
                                                  ownerType, ctx));

    // The result is the first element of the addressor result.
    result = new (ctx) DeclRefExpr(tempDecl, SourceLoc(), IsImplicit);
    result = new (ctx) TupleElementExpr(result, SourceLoc(), /*field index*/ 0,
                                        SourceLoc(), Type());
    result->setImplicit(IsImplicit);
    result = buildUnsafeMutablePointerToRawPointer(result, ctx);

    // Build the callback.
    callback = buildMaterializeForSetCallback(ctx, materializeForSet, storage,
      [&](SmallVectorImpl<ASTNode> &body, VarDecl *selfDecl,
          VarDecl *bufferDecl, VarDecl *callbackStorageDecl) {
      // Pull the owner out of callback storage.
      Expr *owner =
        buildTakeFromCallbackStorage(callbackStorageDecl, ownerType, ctx);

      // For an owning addressor, we can just drop the value we pulled out.
      if (addressor->getAddressorKind() != AddressorKind::NativePinning) {
        body.push_back(owner);

      // For a pinning addressor, we have to unpin it.
      } else {
        Expr *unpin = buildCallToBuiltin(ctx, "unpin", { owner });
        body.push_back(unpin);
      }

      // This should always be a no-op, but do it for the sake of formalism.
      // Builtin.deallocValueBuffer(&callbackStorage, OwnerType.self)
      body.push_back(buildValueBufferOperation(ctx, "deallocValueBuffer",
                                               callbackStorageDecl,
                                               ownerType));
    });
    break;
  }
  }

  // return (buffer, callback)
  result = buildMaterializeForSetResult(ctx, result, callback);
  body.push_back(new (ctx) ReturnStmt(SourceLoc(), result, IsImplicit));

  SourceLoc loc = storage->getLoc();
  materializeForSet->setBody(BraceStmt::create(ctx, loc, body, loc));

  // Mark it transparent, there is no user benefit to this actually existing.
  materializeForSet->getAttrs().add(new (ctx) TransparentAttr(IsImplicit));

  TC.typeCheckDecl(materializeForSet, true);
}

void swift::synthesizeMaterializeForSet(FuncDecl *materializeForSet,
                                        AbstractStorageDecl *storage,
                                        TypeChecker &TC) {
  VarDecl *bufferDecl = getFirstParamDecl(materializeForSet);

  switch (storage->getStorageKind()) {
  case AbstractStorageDecl::Stored:
  case AbstractStorageDecl::Addressed:
    llvm_unreachable("no accessors");

  // We can use direct access to stored variables, but not if they're
  // weak, unowned, or dynamic.
  case AbstractStorageDecl::StoredWithTrivialAccessors: {
    // Only variables can be Stored, and only variables can be weak/unowned.
    auto var = cast<VarDecl>(storage);
    if (var->getType()->is<ReferenceStorageType>()
        || needsDynamicMaterializeForSet(var)) {
      synthesizeComputedMaterializeForSet(materializeForSet, storage,
                                          bufferDecl, TC);
      return;
    }

    synthesizeStoredMaterializeForSet(materializeForSet, storage,
                                      bufferDecl, TC);
    return;
  }

  // We should access these by calling mutableAddress.
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::ComputedWithMutableAddress:
    synthesizeAddressedMaterializeForSet(materializeForSet, storage,
                                         bufferDecl, TC);
    return;

  // These must be accessed with a getter/setter pair.
  // TODO: StoredWithObservers and AddressedWithObservers could be
  // made to work with the callback as long as there isn't a willSet.
  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::Computed:
    synthesizeComputedMaterializeForSet(materializeForSet, storage,
                                        bufferDecl, TC);
    return;
  }
  llvm_unreachable("bad abstract storage kind");
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
  VarDecl *ValueDecl = nullptr;
  Set->getBodyParamPatterns().back()->forEachVariable([&](VarDecl *VD) {
    assert(!ValueDecl && "Already found 'value'?");
    ValueDecl = VD;
  });

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
    auto tmpPBD = new (Ctx) PatternBindingDecl(SourceLoc(),
                                               StaticSpellingKind::None,
                                               SourceLoc(),
                                               tmpPattern, OldValueExpr,
                                               /*conditional*/ false, Set);
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
    Expr *Callee = new (Ctx) DeclRefExpr(willSet, SourceLoc(), /*imp*/true);
    auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, SourceLoc(), /*imp*/true);
    if (SelfDecl) {
      auto *SelfDRE = new (Ctx) DeclRefExpr(SelfDecl, SourceLoc(), /*imp*/true);
      Callee = new (Ctx) DotSyntaxCallExpr(Callee, SourceLoc(), SelfDRE);
    }
    SetterBody.push_back(new (Ctx) CallExpr(Callee, ValueDRE, true));

    // Make sure the didSet/willSet accessors are marked final if in a class.
    if (!willSet->isFinal() &&
        VD->getDeclContext()->isClassOrClassExtensionContext())
      makeFinal(Ctx, willSet);
  }
  
  // Create an assignment into the storage or call to superclass setter.
  auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, SourceLoc(), true);
  createPropertyStoreOrCallSuperclassSetter(Set, ValueDRE, VD, SetterBody, TC);

  // Create:
  //   (call_expr (dot_syntax_call_expr (decl_ref_expr(didSet)),
  //                                    (decl_ref_expr(self))),
  //              (decl_ref_expr(tmp)))
  // or:
  //   (call_expr (decl_ref_expr(didSet)), (decl_ref_expr(tmp)))
  if (auto didSet = VD->getDidSetFunc()) {
    auto *OldValueExpr = new (Ctx) DeclRefExpr(OldValue, SourceLoc(),
                                               /*impl*/true);
    Expr *Callee = new (Ctx) DeclRefExpr(didSet, SourceLoc(), /*imp*/true);
    if (SelfDecl) {
      auto *SelfDRE = new (Ctx) DeclRefExpr(SelfDecl, SourceLoc(), /*imp*/true);
      Callee = new (Ctx) DotSyntaxCallExpr(Callee, SourceLoc(), SelfDRE);
    }
    SetterBody.push_back(new (Ctx) CallExpr(Callee, OldValueExpr, true));

    // Make sure the didSet/willSet accessors are marked final if in a class.
    if (!didSet->isFinal() &&
        VD->getDeclContext()->isClassOrClassExtensionContext())
      makeFinal(Ctx, didSet);
  }

  Set->setBody(BraceStmt::create(Ctx, Loc, SetterBody, Loc));

  // Type check the body of the getter and setter.
  TC.typeCheckDecl(Get, true);
  TC.typeCheckDecl(Set, true);
}

static void convertNSManagedStoredVarToComputed(VarDecl *VD, TypeChecker &TC) {
  assert(VD->getStorageKind() == AbstractStorageDecl::Stored);

  // Create the getter.
  auto *Get = createGetterPrototype(VD, TC);

  // Create the setter.
  VarDecl *SetValueDecl = nullptr;
  auto *Set = createSetterPrototype(VD, SetValueDecl, TC);

  // Okay, we have both the getter and setter.  Set them in VD.
  VD->makeComputed(VD->getLoc(), Get, Set, nullptr, VD->getLoc());

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

/// Synthesize the getter for an lazy property with the specified storage
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
  auto *Tmp1PBD = new (Ctx) PatternBindingDecl(/*StaticLoc*/SourceLoc(),
                                               StaticSpellingKind::None,
                                               /*VarLoc*/SourceLoc(),
                                               Tmp1PBDPattern, Tmp1Init,
                                               /*isConditional*/false,
                                               Get);
  Body.push_back(Tmp1PBD);
  Body.push_back(Tmp1VD);

  // Build the early return inside the if.
  auto *Tmp1DRE = new (Ctx) DeclRefExpr(Tmp1VD, SourceLoc(), /*Implicit*/true,
                                        AccessSemantics::DirectToStorage);
  auto *EarlyReturnVal = new (Ctx) ForceValueExpr(Tmp1DRE, SourceLoc());
  auto *Return = new (Ctx) ReturnStmt(SourceLoc(), EarlyReturnVal,
                                      /*implicit*/true);

  // Build the "if" around the early return.
  Tmp1DRE = new (Ctx) DeclRefExpr(Tmp1VD, SourceLoc(), /*Implicit*/true,
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
  auto *InitValue = VD->getParentPattern()->getInit();
  bool wasChecked = VD->getParentPattern()->wasInitChecked();
  VD->getParentPattern()->setInit(nullptr, true);

  // Recontextualize any closure declcontexts nested in the initializer to
  // realize that they are in the getter function.
  InitValue->walk(RecontextualizeClosures(Get));


  Pattern *Tmp2PBDPattern = new (Ctx) NamedPattern(Tmp2VD, /*implicit*/true);
  Tmp2PBDPattern = new (Ctx) TypedPattern(Tmp2PBDPattern,
                                          TypeLoc::withoutLoc(VD->getType()),
                                          /*implicit*/true);

  auto *Tmp2PBD = new (Ctx) PatternBindingDecl(/*StaticLoc*/SourceLoc(),
                                               StaticSpellingKind::None,
                                               InitValue->getStartLoc(),
                                               Tmp2PBDPattern, nullptr,
                                               /*isConditional*/false,
                                               Get);
  Tmp2PBD->setInit(InitValue, /*already type checked*/wasChecked);
  Body.push_back(Tmp2PBD);
  Body.push_back(Tmp2VD);

  // Assign tmp2 into storage.
  auto Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, SourceLoc(), /*Implicit*/true,
                                       AccessSemantics::DirectToStorage);
  createPropertyStoreOrCallSuperclassSetter(Get, Tmp2DRE, Storage, Body, TC);

  // Return tmp2.
  Tmp2DRE = new (Ctx) DeclRefExpr(Tmp2VD, SourceLoc(), /*Implicit*/true,
                                  AccessSemantics::DirectToStorage);

  Body.push_back(new (Ctx) ReturnStmt(SourceLoc(), Tmp2DRE, /*implicit*/true));

  Get->setBody(BraceStmt::create(Ctx, VD->getLoc(), Body, VD->getLoc(),
                                 /*implicit*/true));

  return Get;
}


void TypeChecker::completeLazyVarImplementation(VarDecl *VD) {
  assert(VD->getAttrs().hasAttribute<LazyAttr>());
  assert(VD->getStorageKind() == AbstractStorageDecl::Computed &&
         "variable not validated yet");
  assert(!VD->isStatic() && "Static vars are already lazy on their own");

  // Create the storage property as an optional of VD's type.
  SmallString<64> NameBuf = VD->getName().str();
  NameBuf += ".storage";
  auto StorageName = Context.getIdentifier(NameBuf);
  auto StorageTy = OptionalType::get(VD->getType());

  auto *Storage = new (Context) VarDecl(/*isStatic*/false, /*isLet*/false,
                                        VD->getLoc(), StorageName, StorageTy,
                                        VD->getDeclContext());
  Storage->setUserAccessible(false);
  addMemberToContextIfNeeded(Storage, VD->getDeclContext(), VD);

  // Create the pattern binding decl for the storage decl.  This will get
  // default initialized to nil.
  Pattern *PBDPattern = new (Context) NamedPattern(Storage, /*implicit*/true);
  PBDPattern = new (Context) TypedPattern(PBDPattern,
                                          TypeLoc::withoutLoc(StorageTy),
                                          /*implicit*/true);
  auto *PBD = new (Context) PatternBindingDecl(/*staticloc*/SourceLoc(),
                                               StaticSpellingKind::None,
                                               /*varloc*/VD->getLoc(),
                                               PBDPattern, /*init*/nullptr,
                                               /*isConditional*/false,
                                               VD->getDeclContext());
  PBD->setImplicit();
  addMemberToContextIfNeeded(PBD, VD->getDeclContext());


  // Now that we've got the storage squared away, synthesize the getter.
  auto *Get = completeLazyPropertyGetter(VD, Storage, *this);

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
  if (VD->getDeclContext()->isClassOrClassExtensionContext())
    makeFinal(Context, Storage);
  Storage->setImplicit();
  Storage->setAccessibility(Accessibility::Private);
  Storage->setSetterAccessibility(Accessibility::Private);

  typeCheckDecl(Get, true);
  typeCheckDecl(Get, false);

  typeCheckDecl(Set, true);
  typeCheckDecl(Set, false);
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

  // We only need materializeForSet in polymorphic contexts:
  auto containerTy =
    storage->getDeclContext()->getDeclaredTypeOfContext();
  if (!containerTy) return;

  NominalTypeDecl *container = containerTy->getAnyNominal();
  assert(container && "extension of non-nominal type?");

  //   - in non-ObjC protocols
  if (auto protocol = dyn_cast<ProtocolDecl>(container)) {
    if (protocol->isObjC()) return;

  //   - in classes when the storage decl is not final and does
  //     not override a decl that requires a materializeForSet
  } else if (isa<ClassDecl>(container)) {
    if (storage->isFinal()) {
      auto overridden = storage->getOverriddenDecl();
      if (!overridden || !overridden->getMaterializeForSetFunc())
        return;
    }

  // Structs and enums don't need this.
  } else {
    assert(isa<StructDecl>(container) || isa<EnumDecl>(container));
    return;
  }

  addMaterializeForSet(storage, TC);
}

void swift::maybeAddAccessorsToVariable(VarDecl *var, TypeChecker &TC) {
  if (var->getGetter() || var->isStatic() || var->isBeingTypeChecked())
    return;

  // Lazy variables need accessors.
  if (var->getAttrs().hasAttribute<LazyAttr>()) {
    var->setIsBeingTypeChecked();

    auto *getter = createGetterPrototype(var, TC);
    // lazy getters are mutating on an enclosing struct.
    getter->setMutating();
    getter->setAccessibility(var->getAccessibility());

    VarDecl *newValueParam = nullptr;
    auto *setter = createSetterPrototype(var, newValueParam, TC);
    var->makeComputed(var->getLoc(), getter, setter, nullptr,
                      var->getLoc());
    var->setIsBeingTypeChecked(false);
    TC.computeAccessibility(setter);

    addMemberToContextIfNeeded(getter, var->getDeclContext());
    addMemberToContextIfNeeded(setter, var->getDeclContext());
    return;

  }

  // Class instance variables also need accessors, because it affects
  // vtable layout.
  if (!var->hasAccessorFunctions() && !var->isImplicit() &&
      var->getDeclContext()->isClassOrClassExtensionContext()) {
    if (var->getAttrs().hasAttribute<NSManagedAttr>()) {
      var->setIsBeingTypeChecked();
      convertNSManagedStoredVarToComputed(var, TC);
      var->setIsBeingTypeChecked(false);
    } else {
      // Variables in SIL mode don't get auto-synthesized getters.
      bool isInSILMode = false;
      if (auto sourceFile = var->getDeclContext()->getParentSourceFile())
        isInSILMode = sourceFile->Kind == SourceFileKind::SIL;

      if (!isInSILMode) {
        var->setIsBeingTypeChecked();
        addTrivialAccessorsToStorage(var, TC);
        var->setIsBeingTypeChecked(false);
      }
    }
    return;
  }
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
  Accessibility accessLevel = decl->getAccessibility();
  if (!decl->hasClangNode())
    accessLevel = std::min(accessLevel, Accessibility::Internal);

  // Determine the parameter type of the implicit constructor.
  SmallVector<TuplePatternElt, 8> patternElts;
  SmallVector<Identifier, 8> argNames;
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
      if (var->isLet() && var->getParentPattern() &&
          var->getParentPattern()->hasInit())
        continue;
      
      accessLevel = std::min(accessLevel, var->getAccessibility());

      auto varType = tc.getTypeOfRValue(var);

      // If var is a lazy property, its value is provided for the underlying
      // storage.  We thus take an optional of the properties type.  We only
      // need to do this because the implicit constructor is added before all
      // the properties are type checked.  Perhaps init() synth should be moved
      // later.
      if (var->getAttrs().hasAttribute<LazyAttr>())
        varType = OptionalType::get(varType);

      // Create the parameter.
      auto *arg = new (context) ParamDecl(/*IsLet*/true, Loc, var->getName(),
                                          Loc, var->getName(), varType, decl);
      argNames.push_back(var->getName());
      Pattern *pattern = new (context) NamedPattern(arg);
      TypeLoc tyLoc = TypeLoc::withoutLoc(varType);
      pattern = new (context) TypedPattern(pattern, tyLoc);
      patternElts.push_back(TuplePatternElt(pattern));
    }
  }

  auto pattern = TuplePattern::create(context, Loc, patternElts, Loc);

  // Create the constructor.
  DeclName name(context, context.Id_init, argNames);
  Pattern *selfPat = buildImplicitSelfParameter(Loc, decl);
  auto *ctor = new (context) ConstructorDecl(name, Loc, OTK_None, SourceLoc(),
                                             selfPat, pattern,
                                             nullptr, decl);

  // Mark implicit.
  ctor->setImplicit();
  ctor->setAccessibility(accessLevel);

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
    tc.Context.ExternalDefinitions.insert(ctor);
  }

  return ctor;
}

/// Create an expression that references the variables in the given
/// pattern for, e.g., forwarding of these variables to another
/// function with the same signature.
static Expr *forwardArguments(TypeChecker &tc, ClassDecl *classDecl,
                              ConstructorDecl *toDecl,
                              Pattern *bodyPattern,
                              ArrayRef<Identifier> argumentNames) {
  switch (bodyPattern->getKind()) {
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    return nullptr;
    
  case PatternKind::Paren: {
    auto subExpr = forwardArguments(tc, classDecl, toDecl,
                              cast<ParenPattern>(bodyPattern)->getSubPattern(),
                                    { });
    if (!subExpr) return nullptr;

    // If there is a name for this single-argument thing, then form a tupleexpr.
    if (argumentNames.size() != 1 || argumentNames[0].empty())
      return new (tc.Context) ParenExpr(SourceLoc(), subExpr, SourceLoc(),
                                        /*hasTrailingClosure=*/false);

    return TupleExpr::createImplicit(tc.Context, subExpr, argumentNames);
  }


  case PatternKind::Tuple: {
    auto bodyTuple = cast<TuplePattern>(bodyPattern);
    SmallVector<Expr *, 4> values;

    // FIXME: Can't forward varargs yet.
    if (bodyTuple->hasVararg()) {
      tc.diagnose(classDecl->getLoc(),
                  diag::unsupported_synthesize_init_variadic,
                  classDecl->getDeclaredType());
      tc.diagnose(toDecl, diag::variadic_superclass_init_here);
      return nullptr;
    }

    for (unsigned i = 0, n = bodyTuple->getNumFields(); i != n; ++i) {
      // Forward the value.
      auto subExpr = forwardArguments(tc, classDecl, toDecl,
                                      bodyTuple->getFields()[i].getPattern(),
                                      { });
      if (!subExpr)
        return nullptr;
      values.push_back(subExpr);
      
      // Dig out the name.
      auto subPattern = bodyTuple->getFields()[i].getPattern();
      do {
        if (auto typed = dyn_cast<TypedPattern>(subPattern)) {
          subPattern = typed->getSubPattern();
          continue;
        }

        if (auto paren = dyn_cast<ParenPattern>(subPattern)) {
          subPattern = paren->getSubPattern();
          continue;
        }

        break;
      } while (true);
    }

    if (values.size() == 1 && 
        (argumentNames.empty() || argumentNames[0].empty()))
      return new (tc.Context) ParenExpr(SourceLoc(), values[0], SourceLoc(),
                                        /*hasTrailingClosure=*/false);

    return TupleExpr::createImplicit(tc.Context, values, argumentNames);
  }

  case PatternKind::Any:
  case PatternKind::Named: {
    auto decl = cast<NamedPattern>(bodyPattern)->getDecl();
    Expr *declRef = new (tc.Context) DeclRefExpr(decl, SourceLoc(),
                                                 /*Implicit=*/true);
    if (decl->getType()->is<InOutType>())
      declRef = new (tc.Context) InOutExpr(SourceLoc(), declRef,
                                           Type(), /*isImplicit=*/true);
    return declRef;
  }

  case PatternKind::Typed:
    return forwardArguments(tc, classDecl, toDecl,
                            cast<TypedPattern>(bodyPattern)->getSubPattern(),
                            argumentNames);

  case PatternKind::Var:
    return forwardArguments(tc, classDecl, toDecl,
                            cast<VarPattern>(bodyPattern)->getSubPattern(),
                            argumentNames);

  }
}

/// Create a stub body that emits a fatal error message.
static void createStubBody(TypeChecker &tc, ConstructorDecl *ctor) {
  auto unimplementedInitDecl = tc.Context.getUnimplementedInitializerDecl(&tc);
  auto classDecl = ctor->getExtensionType()->getClassOrBoundGenericClass();
  if (!unimplementedInitDecl) {
    tc.diagnose(classDecl->getLoc(), diag::missing_unimplemented_init_runtime);
    return;
  }

  // Create a call to Swift._unimplemented_initializer
  auto loc = classDecl->getLoc();
  Expr *fn = new (tc.Context) DeclRefExpr(unimplementedInitDecl, loc,
                                          /*Implicit=*/true);

  llvm::SmallString<64> buffer;
  StringRef fullClassName = tc.Context.AllocateCopy(
                              (classDecl->getModuleContext()->Name.str() + "." +
                               classDecl->getName().str()).toStringRef(buffer));

  Expr *className = new (tc.Context) StringLiteralExpr(fullClassName, loc);
  className = new (tc.Context) ParenExpr(loc, className, loc, false);
  Expr *call = new (tc.Context) CallExpr(fn, className, /*Implicit=*/true);
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
  // Determine the initializer parameters.
  Type superInitType = superclassCtor->getInitializerInterfaceType();
  if (superInitType->is<GenericFunctionType>() ||
      classDecl->getGenericParamsOfContext()) {
    // FIXME: Handle generic initializers as well.
    return nullptr;
  }

  auto &ctx = tc.Context;

  // Create the 'self' declaration and patterns.
  auto *selfDecl = new (ctx) ParamDecl(/*IsLet*/ true,
                                       SourceLoc(), Identifier(),
                                       SourceLoc(), ctx.Id_self,
                                       Type(), classDecl);
  selfDecl->setImplicit();
  Pattern *selfBodyPattern 
    = new (ctx) NamedPattern(selfDecl, /*Implicit=*/true);
  selfBodyPattern = new (ctx) TypedPattern(selfBodyPattern, TypeLoc());

  // Create the initializer parameter patterns.
  OptionSet<Pattern::CloneFlags> options = Pattern::Implicit;
  options |= Pattern::Inherited;
  Pattern *bodyParamPatterns
    = superclassCtor->getBodyParamPatterns()[1]->clone(ctx, options);

  // Fix up the default arguments in the type to refer to inherited default
  // arguments.
  // FIXME: If we weren't cloning the type along with the pattern, this would be
  // a lot more direct.
  Type argType = bodyParamPatterns->getType();

  // Local function that maps default arguments to inherited default arguments.
  std::function<Type(Type)> inheritDefaultArgs = [&](Type type) -> Type {
    auto tuple = type->getAs<TupleType>();
    if (!tuple)
      return type;

    bool anyChanged = false;
    SmallVector<TupleTypeElt, 4> elements;
    unsigned index = 0;
    for (const auto &elt : tuple->getFields()) {
      Type eltTy = elt.getType().transform(inheritDefaultArgs);
      if (!eltTy)
        return Type();

      // If nothing has changed, just keep going.
      if (!anyChanged && eltTy.getPointer() == elt.getType().getPointer() &&
          (elt.getDefaultArgKind() == DefaultArgumentKind::None ||
           elt.getDefaultArgKind() == DefaultArgumentKind::Inherited)) {
        ++index;
        continue;
      }

      // If this is the first change we've seen, copy all of the previous
      // elements.
      if (!anyChanged) {
        // Copy all of the previous elements.
        for (unsigned i = 0; i != index; ++i) {
          const TupleTypeElt &FromElt =tuple->getFields()[i];
          elements.push_back(TupleTypeElt(FromElt.getType(), FromElt.getName(),
                                          FromElt.getDefaultArgKind(),
                                          FromElt.isVararg()));
        }

        anyChanged = true;
      }

      // Add the new tuple element, with the new type, no initializer,
      auto defaultArgKind = elt.getDefaultArgKind();
      if (defaultArgKind != DefaultArgumentKind::None)
        defaultArgKind = DefaultArgumentKind::Inherited;
      elements.push_back(TupleTypeElt(eltTy, elt.getName(), defaultArgKind,
                                      elt.isVararg()));
      ++index;
    }

    if (!anyChanged)
      return type;

    return TupleType::get(elements, ctx);
  };

  argType = argType.transform(inheritDefaultArgs);
  bodyParamPatterns->setType(argType);

  // Create the initializer declaration.
  auto ctor = new (ctx) ConstructorDecl(superclassCtor->getFullName(), 
                                        SourceLoc(),
                                        superclassCtor->getFailability(),
                                        SourceLoc(),
                                        selfBodyPattern, bodyParamPatterns,
                                        nullptr, classDecl);
  ctor->setImplicit();
  ctor->setAccessibility(std::min(classDecl->getAccessibility(),
                                  superclassCtor->getAccessibility()));

  // Configure 'self'.
  GenericParamList *outerGenericParams = nullptr;
  Type selfType = configureImplicitSelf(tc, ctor, outerGenericParams);
  selfBodyPattern->setType(selfType);
  cast<TypedPattern>(selfBodyPattern)->getSubPattern()->setType(selfType);

  // Set the type of the initializer.
  configureConstructorType(ctor, outerGenericParams, selfType, 
                           bodyParamPatterns->getType());
  if (superclassCtor->isObjC()) {
    markAsObjC(tc, ctor, true);

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
  }
  if (superclassCtor->isRequired())
    ctor->getAttrs().add(new (tc.Context) RequiredAttr(/*implicit=*/true));

  // Wire up the overrides.
  ctor->getAttrs().add(new (tc.Context) OverrideAttr(/*Implicit=*/true));
  checkOverrides(tc, ctor);

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
  Expr *ctorRef  = new (ctx) UnresolvedConstructorExpr(superRef,
                                                       SourceLoc(),
                                                       SourceLoc(),
                                                       /*Implicit=*/true);

  Expr *ctorArgs = forwardArguments(tc, classDecl, superclassCtor,
                                    ctor->getBodyParamPatterns()[1],
                                    ctor->getFullName().getArgumentNames());
  if (!ctorArgs) {
    // FIXME: We should be able to assert that this never happens,
    // but there are currently holes when dealing with vararg
    // initializers and _ parameters. Fail somewhat gracefully by
    // generating a stub here.
    createStubBody(tc, ctor);
    return ctor;
  }

  Expr *superCall = new (ctx) CallExpr(ctorRef, ctorArgs, /*Implicit=*/true);
  superCall = new (ctx) RebindSelfInConstructorExpr(superCall, selfDecl);
  ctor->setBody(BraceStmt::create(tc.Context, SourceLoc(),
                                  ASTNode(superCall),
                                  SourceLoc(),
                                  /*Implicit=*/true));

  return ctor;
}

void TypeChecker::addImplicitDestructor(ClassDecl *CD) {
  if (CD->hasDestructor() || CD->isInvalid())
    return;

  Pattern *selfPat = buildImplicitSelfParameter(CD->getLoc(), CD);

  auto *DD = new (Context) DestructorDecl(Context.Id_deinit, CD->getLoc(),
                                          selfPat, CD);

  DD->setImplicit();

  // Type-check the constructor declaration.
  typeCheckDecl(DD, /*isFirstPass=*/true);

  // Create an empty body for the destructor.
  DD->setBody(BraceStmt::create(Context, CD->getLoc(), { }, CD->getLoc()));
  CD->addMember(DD);
  CD->setHasDestructor();
}
