//===--- SILGenExpr.cpp - Implements Lowering of ASTs -> SIL for Exprs ----===//
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

#include "SILGen.h"
#include "swift/AST/AST.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"
#include "Condition.h"
#include "Initialization.h"
#include "OwnershipConventions.h"
#include "LValue.h"
#include "RValue.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace Lowering;

namespace {
  class CleanupRValue : public Cleanup {
    SILValue rv;
  public:
    CleanupRValue(SILValue rv) : rv(rv) {}
    
    void emit(SILGenFunction &gen) override {
      gen.emitReleaseRValue(SILLocation(), rv);
    }
  };
  
  class CleanupTemporaryAllocation : public Cleanup {
    SILValue alloc;
  public:
    CleanupTemporaryAllocation(SILValue alloc) : alloc(alloc) {}
    
    void emit(SILGenFunction &gen) override {
      gen.B.createDeallocVar(SILLocation(), AllocKind::Stack, alloc);
    }
  };
  
  class CleanupMaterializedValue : public Cleanup {
    SILValue address;
  public:
    CleanupMaterializedValue(SILValue address) : address(address) {}
    
    void emit(SILGenFunction &gen) override {
      SILValue tmpValue = gen.B.createLoad(SILLocation(), address);
      gen.emitReleaseRValue(SILLocation(), tmpValue);
    }
  };
  
  class CleanupMaterializedAddressOnlyValue : public Cleanup {
    SILValue address;
  public:
    CleanupMaterializedAddressOnlyValue(SILValue address) : address(address) {}
    
    void emit(SILGenFunction &gen) override {
      gen.B.createDestroyAddr(SILLocation(), address);
    }
  };
} // end anonymous namespace

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v) {
  if (getTypeLoweringInfo(v.getType().getSwiftRValueType()).isTrivial()) {
    return ManagedValue(v, ManagedValue::Unmanaged);
  } else if (v.getType().isAddressOnly()) {
    Cleanups.pushCleanup<CleanupMaterializedAddressOnlyValue>(v);
    return ManagedValue(v, getCleanupsDepth());
  } else {
    Cleanups.pushCleanup<CleanupRValue>(v);
    return ManagedValue(v, getCleanupsDepth());
  }
}

void SILGenFunction::emitExprInto(Expr *E, Initialization *I) {
  // FIXME: actually emit into the initialization. The initialization should
  // be passed down in the context argument to visit, and it should be the
  // visit*Expr method's responsibility to store to it if possible.
  RValue result = visit(E, SGFContext(I));
  if (result)
    std::move(result).forwardInto(*this, I);
}

RValue SILGenFunction::visit(swift::Expr *E) {
  return visit(E, SGFContext());
}

RValue SILGenFunction::visitApplyExpr(ApplyExpr *E, SGFContext C) {
  return emitApplyExpr(E, C);
}

SILValue SILGenFunction::emitEmptyTuple(SILLocation loc) {
  return B.createTuple(loc,
                       getLoweredType(TupleType::getEmpty(SGM.M.getContext())),
                       {});
}

SILValue SILGenFunction::emitGlobalFunctionRef(SILLocation loc,
                                               SILConstant constant) {
  assert(!LocalConstants.count(constant) &&
         "emitting ref to local constant without context?!");
  if (constant.hasDecl() &&
      isa<BuiltinModule>(constant.getDecl()->getDeclContext())) {
    return B.createBuiltinFunctionRef(loc, cast<FuncDecl>(constant.getDecl()),
                                      SGM.getConstantType(constant));
  }
  
  return B.createFunctionRef(loc, SGM.getFunction(constant));
}

SILValue SILGenFunction::emitUnmanagedFunctionRef(SILLocation loc,
                                               SILConstant constant) {
  // If this is a reference to a local constant, grab it.
  if (LocalConstants.count(constant)) {
    return LocalConstants[constant];
  }
  
  // Otherwise, use a global FunctionRefInst.
  return emitGlobalFunctionRef(loc, constant);
}

ManagedValue SILGenFunction::emitFunctionRef(SILLocation loc,
                                             SILConstant constant) {
  // If this is a reference to a local constant, grab it.
  if (LocalConstants.count(constant)) {
    SILValue v = LocalConstants[constant];
    emitRetainRValue(loc, v);
    return emitManagedRValueWithCleanup(v);
  }
  
  // Otherwise, use a global FunctionRefInst.
  SILValue c = emitGlobalFunctionRef(loc, constant);
  return ManagedValue(c, ManagedValue::Unmanaged);
}

static ManagedValue emitGlobalVariable(SILGenFunction &gen,
                                       SILLocation loc, VarDecl *var) {
  assert(!var->getDeclContext()->isLocalContext() &&
         "not a global variable!");
  assert(!var->isProperty() &&
         "not a physical global variable!");
  
  // FIXME: Always emit global variables directly. Eventually we want "true"
  // global variables to be indirectly accessed so that they can be initialized
  // on demand.
  SILValue addr = gen.B.createGlobalAddr(loc, var,
                          gen.getLoweredType(var->getType()).getAddressType());
  return ManagedValue(addr, ManagedValue::LValue);
}

ManagedValue SILGenFunction::emitReferenceToDecl(SILLocation loc,
                                                 ValueDecl *decl,
                                                 Type declType,
                                                 unsigned uncurryLevel) {
  if (!declType) declType = decl->getType();
  
  // If this is a reference to a type, produce a metatype.
  if (isa<TypeDecl>(decl)) {
    assert(decl->getType()->is<MetaTypeType>() &&
           "type declref does not have metatype type?!");
    assert((uncurryLevel == SILConstant::ConstructAtNaturalUncurryLevel
            || uncurryLevel == 0)
           && "uncurry level doesn't make sense for types");
    return ManagedValue(B.createMetatype(loc, getLoweredType(declType)),
                        ManagedValue::Unmanaged);
  }
  
  // If this is a reference to a var, produce an address.
  if (VarDecl *var = dyn_cast<VarDecl>(decl)) {
    assert((uncurryLevel == SILConstant::ConstructAtNaturalUncurryLevel
            || uncurryLevel == 0)
           && "uncurry level doesn't make sense for vars");

    // If it's a property, invoke its getter.
    if (var->isProperty()) {
      SILConstant getter(decl, SILConstant::Kind::Getter);
      return ManagedValue(emitGetProperty(loc, getter, {},
                                      RValue(), RValue(),
                                      var->getType()->getRValueType()).address,
                          ManagedValue::LValue);
    }
    
    // For local decls, use the address we allocated.
    if (VarLocs.count(decl)) {
      return ManagedValue(VarLocs[decl].address, ManagedValue::LValue);
    }
    // If this is a global variable, invoke its accessor function to get its
    // address.
    return emitGlobalVariable(*this, loc, var);
  }
  
  // If the referenced decl isn't a VarDecl, it should be a constant of some
  // sort.
  assert(!decl->getTypeOfReference()->is<LValueType>() &&
         "unexpected lvalue decl ref?!");
  
  // If the referenced decl is a local func with context, then the SILConstant
  // uncurry level is one deeper (for the context vars).
  if (auto *fd = dyn_cast<FuncDecl>(decl)) {
    if (!fd->getCaptures().empty()
        && uncurryLevel != SILConstant::ConstructAtNaturalUncurryLevel)
      ++uncurryLevel;
  }

  return emitFunctionRef(loc, SILConstant(decl, uncurryLevel));
}

RValue SILGenFunction::visitDeclRefExpr(DeclRefExpr *E, SGFContext C) {
  return RValue(*this, emitReferenceToDecl(E, E->getDecl(), E->getType(), 0));
}

RValue SILGenFunction::visitSuperRefExpr(SuperRefExpr *E, SGFContext C) {
  return RValue(*this, emitReferenceToDecl(E, E->getThis(), E->getType(), 0));
}

RValue SILGenFunction::visitOtherConstructorDeclRefExpr(
                                OtherConstructorDeclRefExpr *E, SGFContext C) {
  // This should always be a child of an ApplyExpr and so will be emitted by
  // SILGenApply.
  llvm_unreachable("unapplied reference to constructor?!");
}

RValue SILGenFunction::visitIntegerLiteralExpr(IntegerLiteralExpr *E,
                                               SGFContext C) {
  return RValue(*this,
              ManagedValue(B.createIntegerLiteral(E), ManagedValue::Unmanaged));
}
RValue SILGenFunction::visitFloatLiteralExpr(FloatLiteralExpr *E,
                                             SGFContext C) {
  return RValue(*this,
              ManagedValue(B.createFloatLiteral(E), ManagedValue::Unmanaged));
}
RValue SILGenFunction::visitCharacterLiteralExpr(CharacterLiteralExpr *E,
                                                 SGFContext C)
{
  return RValue(*this,
            ManagedValue(B.createIntegerLiteral(E), ManagedValue::Unmanaged));
}
RValue SILGenFunction::visitStringLiteralExpr(StringLiteralExpr *E,
                                                    SGFContext C) {
  SILType ty = getLoweredLoadableType(E->getType());
  SILValue string = B.createStringLiteral(E, ty);
  return RValue(*this, ManagedValue(string, ManagedValue::Unmanaged));
}

ManagedValue SILGenFunction::emitLoad(SILLocation loc,
                                      SILValue addr,
                                      SGFContext C,
                                      bool isTake) {
  if (addr.getType().isAddressOnly()) {
    // Copy the address-only value.
    SILValue copy = getBufferForExprResult(loc, addr.getType(), C);
    B.createCopyAddr(loc, addr, copy,
                     isTake,
                     /*isInitialize*/ true);
    
    return emitManagedRValueWithCleanup(copy);
  }
  
  // Load the loadable value, and retain it if we aren't taking it.
  SILValue loadedV = B.createLoad(loc, addr);
  if (!isTake)
    emitRetainRValue(loc, loadedV);
  return emitManagedRValueWithCleanup(loadedV);
}

RValue SILGenFunction::visitLoadExpr(LoadExpr *E, SGFContext C) {
  RValue SubV = visit(E->getSubExpr());
  assert(SubV.isLValue() && "subexpr did not produce an lvalue");
  SILValue addr = SubV.getUnmanagedSingleValue(*this);
  
  return RValue(*this, emitLoad(E, addr, C, /*isTake*/ false));
}

SILValue SILGenFunction::emitTemporaryAllocation(SILLocation loc,
                                                 SILType ty) {
  SILValue tmpMem = B.createAllocVar(loc, AllocKind::Stack, ty);
  Cleanups.pushCleanup<CleanupTemporaryAllocation>(tmpMem);
  return tmpMem;
}

SILValue SILGenFunction::getBufferForExprResult(
                                    SILLocation loc, SILType ty, SGFContext C) {
  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (Initialization *I = C.getEmitInto()) {
    switch (I->kind) {
    case Initialization::Kind::AddressBinding:
      llvm_unreachable("can't emit into address binding");

    case Initialization::Kind::Ignored:
      break;
      
    case Initialization::Kind::Tuple:
      // FIXME: For a single-element tuple, we could emit into the single field.
      
      // The tuple initialization isn't contiguous, so we can't emit directly
      // into it.
      break;

    case Initialization::Kind::SingleBuffer:
      // Emit into the buffer.
      return I->getAddress();
    }
  }
  
  // If we couldn't emit into an Initialization, emit into a temporary
  // allocation.
  return emitTemporaryAllocation(loc, ty);
}

Materialize SILGenFunction::emitMaterialize(SILLocation loc, ManagedValue v) {
  assert(!v.isLValue() && "materializing an lvalue?!");
  // Address-only values are already materialized.
  if (v.getType().isAddressOnly()) {
    return Materialize{v.getValue(), v.getCleanup()};
  }
  
  assert(!v.getType().isAddress() &&
         "can't materialize a reference");
  
  // We don't use getBufferForExprResult here because the result of a
  // MaterializeExpr is *not* the value, but an lvalue reference to the value.
  SILValue tmpMem = emitTemporaryAllocation(loc, v.getType());
  v.forwardInto(*this, loc, tmpMem);
  
  CleanupsDepth valueCleanup = CleanupsDepth::invalid();
  if (!getTypeLoweringInfo(v.getType().getSwiftType()).isTrivial()) {
    Cleanups.pushCleanup<CleanupMaterializedValue>(tmpMem);
    valueCleanup = getCleanupsDepth();
  }
  
  return Materialize{tmpMem, valueCleanup};
}

ManagedValue Materialize::consume(SILGenFunction &gen, SILLocation loc) {
  assert(!address.getType().isAddressOnly() &&
         "address-only value must be consumed with consumeInto");
  if (valueCleanup.isValid())
    gen.Cleanups.setCleanupState(valueCleanup, CleanupState::Dead);
  return gen.emitManagedRValueWithCleanup(gen.B.createLoad(loc, address));
}

RValue SILGenFunction::visitMaterializeExpr(MaterializeExpr *E, SGFContext C) {
  // Evaluate the value, then use it to initialize a new temporary and return
  // the temp's address.
  ManagedValue V = visit(E->getSubExpr()).getAsSingleValue(*this);
  return RValue(*this,
            ManagedValue(emitMaterialize(E, V).address, ManagedValue::LValue));
}

RValue SILGenFunction::visitDerivedToBaseExpr(DerivedToBaseExpr *E,
                                                    SGFContext C) {
  ManagedValue original = visit(E->getSubExpr()).getAsSingleValue(*this);
  SILValue converted = B.createUpcast(E,
                                   original.getValue(),
                                   getLoweredType(E->getType()));
  return RValue(*this, ManagedValue(converted, original.getCleanup()));
}

RValue SILGenFunction::visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                                   SGFContext C) {
  SILValue metaBase = visit(E->getSubExpr()).getUnmanagedSingleValue(*this);
  return RValue(*this,
                ManagedValue(B.createUpcast(E, metaBase,
                                          getLoweredLoadableType(E->getType())),
                             ManagedValue::Unmanaged));
}

RValue SILGenFunction::visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E,
                                                 SGFContext C) {
  ManagedValue archetype = visit(E->getSubExpr()).getAsSingleValue(*this);
  SILValue base = B.createArchetypeToSuper(E,
                                        archetype.getValue(),
                                        getLoweredLoadableType(E->getType()));
  if (archetype.hasCleanup()) {
    // If an archetype has a base class, then it must have been of some
    // reference type, so releasing the resulting class-type value is equivalent
    // to destroying the archetype. We can thus "take" the archetype and disable
    // its destructor cleanup if it was a temporary.
    Cleanups.setCleanupState(archetype.getCleanup(), CleanupState::Dead);
  } else {
    // The archetype isn't a temporary, so we need our own retain.
    emitRetainRValue(E, base);
  }
  return RValue(*this, emitManagedRValueWithCleanup(base));
}

RValue SILGenFunction::visitRequalifyExpr(RequalifyExpr *E, SGFContext C) {
  assert(E->getType()->is<LValueType>() && "non-lvalue requalify");
  // Ignore lvalue qualifiers.
  return visit(E->getSubExpr());
}

RValue SILGenFunction::visitFunctionConversionExpr(FunctionConversionExpr *e,
                                                   SGFContext C)
{
  ManagedValue original = visit(e->getSubExpr()).getAsSingleValue(*this);
  
  // Retain the thinness of the original function type.
  Type destTy = e->getType();
  if (original.getType().castTo<FunctionType>()->isThin())
    destTy = getThinFunctionType(destTy);
  
  SILValue converted = B.createConvertFunction(e, original.getValue(),
                                               getLoweredType(destTy));
  return RValue(*this, ManagedValue(converted, original.getCleanup()));
}

namespace {
  /// An Initialization representing the concrete value buffer inside an
  /// existential container.
  class ExistentialValueInitialization : public SingleInitializationBase {
    SILValue valueAddr;
  public:
    ExistentialValueInitialization(SILValue valueAddr)
      : SingleInitializationBase(valueAddr.getType().getSwiftRValueType()),
        valueAddr(valueAddr)
    {}
    
    SILValue getAddressOrNull() override {
      return valueAddr;
    }
    
    void finishInitialization(SILGenFunction &gen) {
      // FIXME: Disable the DeinitExistential cleanup and enable the
      // DestroyAddr cleanup for the existential container.
    }
  };
}

RValue SILGenFunction::visitErasureExpr(ErasureExpr *E, SGFContext C) {
  // FIXME: Need to stage cleanups here. If code fails between
  // InitExistential and initializing the value, clean up using
  // DeinitExistential.
  
  // Allocate the existential.
  SILValue existential = getBufferForExprResult(E, getLoweredType(E->getType()),
                                                C);
  
  if (E->getSubExpr()->getType()->isExistentialType()) {
    // If the source value is already of a protocol type, we can use
    // upcast_existential to steal its already-initialized witness tables and
    // concrete value.
    ManagedValue subExistential = visit(E->getSubExpr()).getAsSingleValue(*this);

    B.createUpcastExistential(E, subExistential.getValue(), existential,
                              /*isTake=*/subExistential.hasCleanup(),
                              E->getConformances());
  } else {
    // Otherwise, we need to initialize a new existential container from
    // scratch.
    
    // Allocate the concrete value inside the container.
    SILValue valueAddr = B.createInitExistential(E, existential,
                                      getLoweredType(E->getSubExpr()->getType()),
                                      E->getConformances());
    // Initialize the concrete value in-place.
    InitializationPtr init(new ExistentialValueInitialization(valueAddr));
    emitExprInto(E->getSubExpr(), init.get());    
    init->finishInitialization(*this);
  }
  
  return RValue(*this, emitManagedRValueWithCleanup(existential));
}

RValue SILGenFunction::visitCoerceExpr(CoerceExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

RValue SILGenFunction::visitUncheckedDowncastExpr(UncheckedDowncastExpr *E,
                                                  SGFContext C) {
  ManagedValue original = visit(E->getSubExpr()).getAsSingleValue(*this);
  SILValue converted = B.createDowncast(E, original.getValue(),
                                     getLoweredLoadableType(E->getType()));
  return RValue(*this, ManagedValue(converted, original.getCleanup()));
}

RValue SILGenFunction::visitUncheckedSuperToArchetypeExpr(
                                              UncheckedSuperToArchetypeExpr *E,
                                              SGFContext C) {
  ManagedValue base = visit(E->getSubExpr()).getAsSingleValue(*this);
  // Allocate an archetype to hold the downcast value.
  SILValue archetype = getBufferForExprResult(E, getLoweredType(E->getType()), C);
  // Initialize it with a SuperToArchetypeInst.
  B.createSuperToArchetype(E, base.forward(*this), archetype);
  
  return RValue(*this, emitManagedRValueWithCleanup(archetype));
}

RValue SILGenFunction::visitIsSubtypeExpr(IsSubtypeExpr *E, SGFContext C)
{
  ManagedValue base = visit(E->getSubExpr()).getAsSingleValue(*this);
  SILValue res = B.createIsa(E, base.getValue(),
                             getLoweredType(E->getTypeLoc().getType()),
                             getLoweredLoadableType(E->getType()));
  return RValue(*this, emitManagedRValueWithCleanup(res));
}

RValue SILGenFunction::visitParenExpr(ParenExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

static ManagedValue emitVarargs(SILGenFunction &gen,
                                SILLocation loc,
                                Type baseTy,
                                ArrayRef<ManagedValue> elements,
                                Expr *VarargsInjectionFn) {
  SILValue numEltsVal = gen.B.createIntegerValueInst(elements.size(),
                      SILType::getBuiltinIntegerType(64, gen.F.getContext()));
  AllocArrayInst *allocArray = gen.B.createAllocArray(loc,
                                                  gen.getLoweredType(baseTy),
                                                  numEltsVal);
  // The first result is the owning ObjectPointer for the array.
  ManagedValue objectPtr
    = gen.emitManagedRValueWithCleanup(SILValue(allocArray, 0));
  // The second result is a RawPointer to the base address of the array.
  SILValue basePtr(allocArray, 1);

  for (size_t i = 0, size = elements.size(); i < size; ++i) {
    SILValue eltPtr = i == 0
      ? basePtr
      : gen.B.createIndexAddr(loc, basePtr, i);
    ManagedValue v = elements[i];
    v.forwardInto(gen, loc, eltPtr);
  }

  return gen.emitArrayInjectionCall(objectPtr, basePtr,
                                    numEltsVal, VarargsInjectionFn);
}

RValue SILGenFunction::visitTupleExpr(TupleExpr *E, SGFContext C) {
  // If we have an Initialization, emit the tuple elements into its elements.
  if (Initialization *I = C.getEmitInto()) {
    SmallVector<InitializationPtr, 4> subInitializationBuf;
    auto subInitializations = I->getSubInitializations(*this,
                                                       subInitializationBuf);
    assert(subInitializations.size() == E->getElements().size() &&
           "initialization for tuple has wrong number of elements");
    for (unsigned i = 0; i < subInitializations.size(); ++i) {
      emitExprInto(E->getElements()[i], subInitializations[i].get());
    }
    I->finishInitialization(*this);
    return RValue();
  }
  
  RValue result(E->getType()->getCanonicalType());
  for (Expr *elt : E->getElements()) {
    result.addElement(visit(elt));
  }
  return result;
}

RValue SILGenFunction::visitSpecializeExpr(SpecializeExpr *E,
                                           SGFContext C) {
  SILValue unspecialized
    = visit(E->getSubExpr()).getUnmanagedSingleValue(*this);
  SILType specializedType
    = getLoweredLoadableType(getThinFunctionType(E->getType()));
  SILValue spec = B.createSpecialize(E,
                                     unspecialized,
                                     E->getSubstitutions(),
                                     specializedType);
  return RValue(*this, ManagedValue(spec, ManagedValue::Unmanaged));
}

RValue SILGenFunction::visitAddressOfExpr(AddressOfExpr *E,
                                          SGFContext C) {
  return visit(E->getSubExpr(), C);
}

namespace {

static ManagedValue emitExtractLoadableElement(SILGenFunction &gen,
                                               Expr *e, ManagedValue base,
                                               unsigned elt) {
  assert(!base.getType().hasReferenceSemantics() &&
         "can't extract from reference types here");
  if (e->getType()->is<LValueType>()) {
    // Get the element address relative to the aggregate's address.
    SILValue address;
    assert(base.getType().isAddress() &&
           "base of lvalue member ref must be ref type or address");
    address = gen.B.createElementAddr(e,
                                    base.getUnmanagedValue(),
                                    elt,
                                    gen.getLoweredType(e->getType()));
    return ManagedValue(address, ManagedValue::LValue);
  } else {
    // Extract the element from the original aggregate value.
    SILValue extract = gen.B.createExtract(e,
                                        base.getValue(),
                                        elt,
                                        gen.getLoweredType(e->getType()));
    
    gen.emitRetainRValue(e, extract);
    return gen.emitManagedRValueWithCleanup(extract);
  }
}
  
static ManagedValue emitExtractFromClass(SILGenFunction &gen,
                                         Expr *e, ManagedValue base,
                                         VarDecl *field) {
  return ManagedValue(gen.B.createRefElementAddr(e,
                                             base.getValue(),
                                             field,
                                             gen.getLoweredType(e->getType())),
                      ManagedValue::LValue);
}

template<typename ANY_MEMBER_REF_EXPR>
ManagedValue emitAnyMemberRefExpr(SILGenFunction &gen,
                                  ANY_MEMBER_REF_EXPR *e,
                                  ArrayRef<Substitution> substitutions) {
  SILType ty = gen.getLoweredType(e->getBase()->getType()->getRValueType());
  
  // If this is a physical field, derive its address directly.
  if (VarDecl *var = dyn_cast<VarDecl>(e->getDecl())) {
    if (!var->isProperty()) {
      SILCompoundTypeInfo *cti = ty.getCompoundTypeInfo();
      // We can get to the element directly with element_addr or
      // ref_element_addr.
      ManagedValue base = gen.visit(e->getBase()).getAsSingleValue(gen);
      if (ty.hasReferenceSemantics())
        return emitExtractFromClass(gen, e, base, var);
      size_t index = cti->getIndexOfMemberDecl(var);
      return emitExtractLoadableElement(gen, e, base, index);
    }
  }

  // Otherwise, call the getter.
  Type baseTy = e->getBase()->getType();
  (void)baseTy;
  // We have to load the element indirectly through a property.
  assert((baseTy->is<LValueType>() || baseTy->hasReferenceSemantics()) &&
         "member ref of a non-lvalue?!");
  RValue base = gen.visit(e->getBase());
  // Get the getter function, which will have type This -> () -> T.
  // The type will be a polymorphic function if the This type is generic.
  SILConstant get(e->getDecl(), SILConstant::Kind::Getter);
  
  // Apply the getter.
  Materialize propTemp = gen.emitGetProperty(e, get, substitutions,
                                             std::move(base), RValue(),
                                             e->getType()->getRValueType());
  return ManagedValue(propTemp.address, ManagedValue::LValue);
}

} // end anonymous namespace

ManagedValue SILGenFunction::emitMethodRef(SILLocation loc,
                                           SILValue thisValue,
                                           SILConstant methodConstant,
                                           ArrayRef<Substitution> innerSubs) {
  // FIXME: Emit dynamic dispatch instruction (class_method, super_method, etc.)
  // if needed.
  
  SILType methodType = SGM.getConstantType(methodConstant);
  SILValue methodValue = B.createFunctionRef(loc,
                                             SGM.getFunction(methodConstant));
  
  /// If the 'this' type is a bound generic, specialize the method ref with
  /// its substitutions.
  ArrayRef<Substitution> outerSubs;
  
  Type innerMethodTy = methodType.castTo<AnyFunctionType>()->getResult();
  
  if (!innerSubs.empty()) {
    // Specialize the inner method type.
    // FIXME: This assumes that 'innerSubs' is an identity mapping, which is
    // true for generic allocating constructors calling initializers but not in
    // general.
    
    PolymorphicFunctionType *innerPFT
      = innerMethodTy->castTo<PolymorphicFunctionType>();
    innerMethodTy = FunctionType::get(innerPFT->getInput(),
                                      innerPFT->getResult(),
                                      F.getContext());
  }
  
  Type outerMethodTy = FunctionType::get(thisValue.getType().getSwiftType(),
                                         innerMethodTy,
                                         /*isAutoClosure*/ false,
                                         /*isBlock*/ false,
                                         /*isThin*/ true,
                                         F.getContext());

  if (BoundGenericType *bgt = thisValue.getType().getAs<BoundGenericType>())
    outerSubs = bgt->getSubstitutions();
  
  if (!innerSubs.empty() || !outerSubs.empty()) {
    // Specialize the generic method.
    ArrayRef<Substitution> allSubs;
    if (outerSubs.empty())
      allSubs = innerSubs;
    else if (innerSubs.empty())
      allSubs = outerSubs;
    else {
      Substitution *allSubsBuf
        = F.getContext().Allocate<Substitution>(outerSubs.size()
                                                  + innerSubs.size());
      std::memcpy(allSubsBuf,
                  outerSubs.data(), outerSubs.size() * sizeof(Substitution));
      std::memcpy(allSubsBuf + outerSubs.size(),
                  innerSubs.data(), innerSubs.size() * sizeof(Substitution));
      allSubs = {allSubsBuf, outerSubs.size()+innerSubs.size()};
    }
    
    SILType specType = getLoweredLoadableType(outerMethodTy,
                              methodType.getFunctionTypeInfo()->getAbstractCC(),
                              methodValue.getType().getUncurryLevel());
    
    methodValue = B.createSpecialize(loc, methodValue, allSubs, specType);
  }

  return ManagedValue(methodValue, ManagedValue::Unmanaged);
}

RValue SILGenFunction::visitMemberRefExpr(MemberRefExpr *E,
                                                SGFContext C) {
  return RValue(*this, emitAnyMemberRefExpr(*this, E, {}));
}

RValue SILGenFunction::visitGenericMemberRefExpr(GenericMemberRefExpr *E,
                                                 SGFContext C)
{
  if (E->getBase()->getType()->is<MetaTypeType>()) {
    assert(E->getType()->is<MetaTypeType>() &&
           "generic_member_ref of metatype should give metatype");
    // If the base and member are metatypes, emit an associated_metatype inst
    // to extract the associated type from the type metadata.
    SILValue baseMetatype = visit(E->getBase()).getUnmanagedSingleValue(*this);
    return RValue(*this,
                  ManagedValue(B.createAssociatedMetatype(E,
                             baseMetatype,
                             getLoweredLoadableType(E->getType())),
                        ManagedValue::Unmanaged));

  }
  return RValue(*this, emitAnyMemberRefExpr(*this, E, E->getSubstitutions()));
}

SILValue SILGenFunction::emitArchetypeMethod(ArchetypeMemberRefExpr *e,
                                             SILValue archetype) {
  if (isa<FuncDecl>(e->getDecl())) {
    // This is a method reference. Extract the method implementation from the
    // archetype and apply the "this" argument.
    Type methodType = FunctionType::get(archetype.getType().getSwiftType(),
                                        e->getType(),
                                        F.getContext());
    SILConstant c(e->getDecl());
    return B.createArchetypeMethod(e, archetype, c,
                                   getLoweredLoadableType(methodType,
                                                          SGM.getConstantCC(c),
                                                          c.uncurryLevel));
  } else {
    llvm_unreachable("archetype properties not yet implemented");
  }
}

RValue SILGenFunction::visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E,
                                                   SGFContext C) {
  SILValue archetype = visit(E->getBase()).getUnmanagedSingleValue(*this);
  assert((archetype.getType().isAddress() ||
          archetype.getType().is<MetaTypeType>()) &&
         "archetype must be an address or metatype");
  // FIXME: curried archetype
  // FIXME: archetype properties
  llvm_unreachable("unapplied archetype method not implemented");
}

SILValue SILGenFunction::emitProtocolMethod(ExistentialMemberRefExpr *e,
                                            SILValue existential) {
  if (isa<FuncDecl>(e->getDecl())) {
    // 'this' for instance methods is projected out of the existential container
    // as an OpaquePointer.
    // 'this' for existential metatypes is the metatype itself.
    Type thisTy = e->getDecl()->isInstanceMember()
      ? F.getContext().TheOpaquePointerType
      : e->getBase()->getType();
    
    // This is a method reference. Extract the method implementation from the
    // archetype and apply the "this" argument.
    Type methodType = FunctionType::get(thisTy,
                                        e->getType(),
                                        F.getContext());
    SILConstant c(e->getDecl());
    return B.createProtocolMethod(e, existential, c,
                                  getLoweredLoadableType(methodType,
                                                         SGM.getConstantCC(c),
                                                         c.uncurryLevel));
  } else {
    llvm_unreachable("existential properties not yet implemented");
  }
}

RValue SILGenFunction::visitExistentialMemberRefExpr(
                                                 ExistentialMemberRefExpr *E,
                                                 SGFContext C) {
  SILValue existential = visit(E->getBase()).getUnmanagedSingleValue(*this);
  assert((existential.getType().isAddress() ||
          existential.getType().is<MetaTypeType>()) &&
         "existential must be an address");
  //SILValue projection = B.createProjectExistential(E, existential);
  //SILValue method = emitProtocolMethod(E, existential);
  // FIXME: curried existential
  // FIXME: existential properties
  llvm_unreachable("unapplied protocol method not implemented");
}

RValue SILGenFunction::visitDotSyntaxBaseIgnoredExpr(
                                                  DotSyntaxBaseIgnoredExpr *E,
                                                  SGFContext C) {
  visit(E->getLHS());
  return visit(E->getRHS());
}

RValue SILGenFunction::visitModuleExpr(ModuleExpr *E, SGFContext C) {
  SILValue module = B.createModule(E, getLoweredLoadableType(E->getType()));
  return RValue(*this, ManagedValue(module, ManagedValue::Unmanaged));
}

namespace {

template<typename ANY_SUBSCRIPT_EXPR>
ManagedValue emitAnySubscriptExpr(SILGenFunction &gen,
                                  ANY_SUBSCRIPT_EXPR *e,
                                  ArrayRef<Substitution> substitutions) {
  SubscriptDecl *sd = e->getDecl();
  RValue base = gen.visit(e->getBase());
  
  // Get the getter function, which will have type This -> Index -> () -> T.
  SILConstant get(sd, SILConstant::Kind::Getter);

  // Evaluate the index.
  RValue index = gen.visit(e->getIndex());
  
  // Apply the getter.
  Materialize propTemp = gen.emitGetProperty(e, get, substitutions,
                                             std::move(base), std::move(index),
                                             e->getType()->getRValueType());
  return ManagedValue(propTemp.address, ManagedValue::LValue);
}

}

RValue SILGenFunction::visitSubscriptExpr(SubscriptExpr *E,
                                                SGFContext C) {
  return RValue(*this, emitAnySubscriptExpr(*this, E, {}));
}

RValue SILGenFunction::visitGenericSubscriptExpr(GenericSubscriptExpr *E,
                                                       SGFContext C)
{
  return RValue(*this, emitAnySubscriptExpr(*this, E, E->getSubstitutions()));
}

RValue SILGenFunction::visitTupleElementExpr(TupleElementExpr *E,
                                             SGFContext C) {
  if (E->getType()->is<LValueType>()) {
    SILValue baseAddr = visit(E->getBase()).getUnmanagedSingleValue(*this);
    SILValue eltAddr = B.createElementAddr(E, baseAddr, E->getFieldNumber(),
                                 getLoweredType(E->getType()).getAddressType());
    return RValue(*this, ManagedValue(eltAddr, ManagedValue::LValue));
  } else {
    return visit(E->getBase()).extractElement(E->getFieldNumber());
  }
}

RValue SILGenFunction::visitTupleShuffleExpr(TupleShuffleExpr *E,
                                             SGFContext C) {
  /* TODO:
  // If we're emitting into an initialization, we can try shuffling the
  // elements of the initialization.
  if (Initialization *I = C.getEmitInto()) {
    emitTupleShuffleExprInto(*this, E, I);
    return RValue();
  }
   */

  // Emit the sub-expression tuple and destructure it into elements.
  SmallVector<RValue, 4> elements;
  visit(E->getSubExpr()).extractElements(elements);
  
  // Prepare a new tuple to hold the shuffled result.
  RValue result(E->getType()->getCanonicalType());
  
  auto outerFields = E->getType()->castTo<TupleType>()->getFields();
  auto shuffleIndexIterator = E->getElementMapping().begin(),
    shuffleIndexEnd = E->getElementMapping().end();
  for (auto &field : outerFields) {
    assert(shuffleIndexIterator != shuffleIndexEnd &&
           "ran out of shuffle indexes before running out of fields?!");
    int shuffleIndex = *shuffleIndexIterator++;
    
    // If the shuffle index is DefaultInitialize, we're supposed to use the
    // default value.
    if (shuffleIndex == TupleShuffleExpr::DefaultInitialize) {
      assert(field.hasInit() && "no default initializer for field!");
      result.addElement(visit(field.getInit()->getExpr()));
      continue;
    }

    // If the shuffle index is FirstVariadic, it is the beginning of the list of
    // varargs inputs.  Save this case for last.
    if (shuffleIndex != TupleShuffleExpr::FirstVariadic) {
      // Map from a different tuple element.
      result.addElement(std::move(elements[shuffleIndex]));
      continue;
    }

    assert(field.isVararg() && "Cannot initialize nonvariadic element");
    
    // Okay, we have a varargs tuple element.  All the remaining elements feed
    // into the varargs portion of this, which is then constructed into a Slice
    // through an informal protocol captured by the InjectionFn in the
    // TupleShuffleExpr.
    assert(E->getVarargsInjectionFunction() &&
           "no injection function for varargs tuple?!");
    SmallVector<ManagedValue, 4> variadicValues;
    
    while (shuffleIndexIterator != shuffleIndexEnd) {
      unsigned sourceField = *shuffleIndexIterator++;
      variadicValues.push_back(
                     std::move(elements[sourceField]).getAsSingleValue(*this));
    }
    
    ManagedValue varargs = emitVarargs(*this, E, field.getVarargBaseTy(),
                                       variadicValues,
                                       E->getVarargsInjectionFunction());
    result.addElement(RValue(*this, varargs));
    break;
  }
  
  return result;
}

static void emitScalarToTupleExprInto(SILGenFunction &gen,
                                      ScalarToTupleExpr *E,
                                      Initialization *I) {
  auto outerFields = E->getType()->castTo<TupleType>()->getFields();
  bool isScalarFieldVariadic = outerFields[E->getScalarField()].isVararg();

  // Decompose the initialization.
  SmallVector<InitializationPtr, 4> subInitializationBuf;
  auto subInitializations = I->getSubInitializations(gen, subInitializationBuf);
  assert(subInitializations.size() == outerFields.size() &&
         "initialization size does not match tuple size?!");
  
  // If the scalar field isn't variadic, emit it into the destination field of
  // the tuple.
  Initialization *scalarInit = subInitializations[E->getScalarField()].get();
  if (!isScalarFieldVariadic)
    gen.emitExprInto(E->getSubExpr(), scalarInit);
  else {
    // Otherwise, create the vararg and store it to the vararg field.
    ManagedValue scalar = gen.visit(E->getSubExpr()).getAsSingleValue(gen);
    ManagedValue varargs = emitVarargs(gen, E, E->getSubExpr()->getType(),
                                       scalar, E->getVarargsInjectionFunction());
    varargs.forwardInto(gen, E, scalarInit->getAddress());
    scalarInit->finishInitialization(gen);
  }
  
  // Emit the non-scalar fields.
  for (unsigned i = 0, e = outerFields.size(); i != e; ++i) {
    if (i == E->getScalarField())
      continue;
    // Fill the vararg field with an empty array.
    if (outerFields[i].isVararg()) {
      assert(i == e - 1 && "vararg isn't last?!");
      ManagedValue varargs = emitVarargs(gen, E, outerFields[i].getVarargBaseTy(),
                                         {}, E->getVarargsInjectionFunction());
      varargs.forwardInto(gen, E, subInitializations[i]->getAddress());
      subInitializations[i]->finishInitialization(gen);
    }
    // Evaluate default initializers in-place.
    else {
      assert(outerFields[i].hasInit() &&
             "no default initializer in non-scalar field of scalar-to-tuple?!");
      gen.emitExprInto(outerFields[i].getInit()->getExpr(),
                       subInitializations[i].get());
    }
  }
  
  // Finish the aggregate initialization.
  I->finishInitialization(gen);
}

RValue SILGenFunction::visitScalarToTupleExpr(ScalarToTupleExpr *E,
                                              SGFContext C) {
  // If we're emitting into an Initialization, we can decompose the
  // initialization.
  if (Initialization *I = C.getEmitInto()) {
    emitScalarToTupleExprInto(*this, E, I);
    return RValue();
  }
  
  // Emit the scalar member.
  RValue scalar = visit(E->getSubExpr());

  // Prepare a tuple rvalue to house the result.
  RValue result(E->getType()->getCanonicalType());
  
  // Create a tuple around the scalar along with any
  // default values or varargs.
  auto outerFields = E->getType()->castTo<TupleType>()->getFields();
  for (unsigned i = 0, e = outerFields.size(); i != e; ++i) {
    // Handle the variadic argument. If we didn't emit the scalar field yet,
    // it goes into the variadic array; otherwise, the variadic array is empty.
    if (outerFields[i].isVararg()) {
      assert(i == e - 1 && "vararg isn't last?!");
      ManagedValue varargs;
      if (!scalar.isUsed())
        varargs = emitVarargs(*this, E, outerFields[i].getVarargBaseTy(),
                              std::move(scalar).getAsSingleValue(*this),
                              E->getVarargsInjectionFunction());
      else
        varargs = emitVarargs(*this, E, outerFields[i].getVarargBaseTy(),
                              {}, E->getVarargsInjectionFunction());
      result.addElement(RValue(*this, varargs));
      break;
    }

    // Add the scalar to the tuple in the right place.
    else if (i == E->getScalarField()) {
      result.addElement(std::move(scalar));
    }
    // Fill in the other fields with their default initializers.
    else {
      assert(outerFields[i].hasInit() &&
             "no default initializer in non-scalar field of scalar-to-tuple?!");
      result.addElement(visit(outerFields[i].getInit()->getExpr()));
    }
  }

  return result;
}

RValue SILGenFunction::visitNewArrayExpr(NewArrayExpr *E, SGFContext C) {
  SILValue NumElements = visit(E->getBounds()[0].Value)
    .getAsSingleValue(*this)
    .getValue();

  // Allocate the array.
  AllocArrayInst *AllocArray = B.createAllocArray(E,
                                            getLoweredType(E->getElementType()),
                                            NumElements);

  ManagedValue ObjectPtr
    = emitManagedRValueWithCleanup(SILValue(AllocArray, 0));
  SILValue BasePtr(AllocArray, 1);

  // FIXME: We need to initialize the elements of the array that are now
  // allocated.

  // Finally, build and return a Slice instance using the object
  // header/base/count.
  return RValue(*this, emitArrayInjectionCall(ObjectPtr, BasePtr, NumElements,
                                              E->getInjectionFunction()));
}

SILValue SILGenFunction::emitMetatypeOfValue(SILLocation loc, SILValue base) {
  // For class, archetype, and protocol types, look up the dynamic metatype.
  SILType metaTy = getLoweredLoadableType(
    MetaTypeType::get(base.getType().getSwiftRValueType(), F.getContext()));
  if (base.getType().getSwiftType()->getClassOrBoundGenericClass()) {
    return B.createClassMetatype(loc, metaTy, base);
  } else if (base.getType().getSwiftRValueType()->is<ArchetypeType>()) {
    return B.createArchetypeMetatype(loc, metaTy, base);
  } else if (base.getType().getSwiftRValueType()->isExistentialType()) {
    return B.createProtocolMetatype(loc, metaTy, base);
  }
  // Otherwise, ignore the base and return the static metatype.
  return B.createMetatype(loc, metaTy);
}

RValue SILGenFunction::visitMetatypeExpr(MetatypeExpr *E, SGFContext C) {
  // Evaluate the base if present.
  SILValue metatype;
  
  if (E->getBase()) {
    SILValue base = visit(E->getBase()).getAsSingleValue(*this).getValue();
    metatype = emitMetatypeOfValue(E, base);
  } else {
    metatype = B.createMetatype(E, getLoweredLoadableType(E->getType()));
  }
  
  return RValue(*this, ManagedValue(metatype, ManagedValue::Unmanaged));
}

ManagedValue SILGenFunction::emitClosureForCapturingExpr(SILLocation loc,
                                                         SILConstant constant,
                                                         CapturingExpr *body) {
  // FIXME: Stash the capture args somewhere and curry them on demand rather
  // than here.
  assert(((constant.uncurryLevel == 1 && !body->getCaptures().empty())
          || (constant.uncurryLevel == 0 && body->getCaptures().empty()))
         && "curried local functions not yet supported");

  auto captures = body->getCaptures();
  if (!captures.empty()) {
    
    llvm::SmallVector<SILValue, 4> capturedArgs;
    for (ValueDecl *capture : captures) {
      switch (getDeclCaptureKind(capture)) {
        case CaptureKind::Box: {
          // LValues are captured as both the box owning the value and the
          // address of the value.
          assert(VarLocs.count(capture) &&
                 "no location for captured var!");
          
          VarLoc const &vl = VarLocs[capture];
          assert(vl.box && "no box for captured var!");
          assert(vl.address && "no address for captured var!");
          B.createRetain(loc, vl.box);
          capturedArgs.push_back(vl.box);
          capturedArgs.push_back(vl.address);
          break;
        }
        case CaptureKind::Byref: {
          // Byrefs are captured by address only.
          assert(VarLocs.count(capture) &&
                 "no location for captured byref!");
          capturedArgs.push_back(VarLocs[capture].address);
          break;
        }
        case CaptureKind::Constant: {
          // SILValue is a constant such as a local func. Pass on the reference.
          ManagedValue v = emitReferenceToDecl(loc, capture);
          capturedArgs.push_back(v.forward(*this));
          break;
        }
        case CaptureKind::GetterSetter: {
          // Pass the setter and getter closure references on.
          ManagedValue v = emitFunctionRef(loc, SILConstant(capture,
                                                   SILConstant::Kind::Setter));
          capturedArgs.push_back(v.forward(*this));
          [[clang::fallthrough]];
        }
        case CaptureKind::Getter: {
          // Pass the getter closure reference on.
          ManagedValue v = emitFunctionRef(loc, SILConstant(capture,
                                                   SILConstant::Kind::Getter));
          capturedArgs.push_back(v.forward(*this));
          break;
        }
      }
    }
    
    SILValue functionRef = emitGlobalFunctionRef(loc, constant);
    Type closureSwiftTy
      = functionRef.getType().getAs<AnyFunctionType>()->getResult();
    SILType closureTy = getLoweredLoadableType(closureSwiftTy);
    return emitManagedRValueWithCleanup(
                    B.createPartialApply(loc, functionRef, capturedArgs,
                                         closureTy));
  } else {
    return ManagedValue(emitGlobalFunctionRef(loc, constant),
                        ManagedValue::Unmanaged);
  }
}

RValue SILGenFunction::visitFuncExpr(FuncExpr *e, SGFContext C) {
  // Generate the local function body.
  SGM.emitFunction(e, e);

  // Generate the closure (if any) for the function reference.
  return RValue(*this, emitClosureForCapturingExpr(e, SILConstant(e), e));
}

RValue SILGenFunction::visitClosureExpr(ClosureExpr *e, SGFContext C) {
  // Generate the closure body.
  SGM.emitClosure(e);
  
  // Generate the closure value (if any) for the closure expr's function
  // reference.
  return RValue(*this, emitClosureForCapturingExpr(e, SILConstant(e), e));
}

void SILGenFunction::emitFunction(FuncExpr *fe) {
  emitProlog(fe, fe->getBodyParamPatterns(), fe->getResultType(F.getContext()));
  visit(fe->getBody());
}

void SILGenFunction::emitClosure(ClosureExpr *ce) {
  emitProlog(ce, ce->getParamPatterns(),
             ce->getType()->castTo<FunctionType>()->getResult());

  // Closure expressions implicitly return the result of their body expression.
  emitReturnExpr(ce, ce->getBody());
  
  assert(!B.hasValidInsertionPoint() &&
         "returning closure body did not terminate closure?!");
}

bool SILGenFunction::emitEpilogBB(SILLocation loc) {
  assert(epilogBB && "no epilog bb to emit?!");
  
  // If the epilog was not branched to at all, just unwind like a "return"
  // and emit the epilog into the current BB.
  if (epilogBB->pred_empty()) {
    epilogBB->eraseFromParent();

    // If the current bb is terminated then the epilog is just unreachable.
    if (!B.hasValidInsertionPoint())
      return false;
    
    Cleanups.emitCleanupsForReturn(loc);
  } else {
    // If the body didn't explicitly return, we need to branch out of it as if
    // returning. emitReturnAndCleanups will do that.
    if (B.hasValidInsertionPoint())
      Cleanups.emitReturnAndCleanups(loc, SILValue());
    // Emit the epilog into the epilog bb.
    B.emitBlock(epilogBB);
  }
  return true;
}

void SILGenFunction::emitDestructor(ClassDecl *cd, DestructorDecl *dd) {
  SILValue thisValue = emitDestructorProlog(cd, dd);

  // Create a basic block to jump to for the implicit destruction behavior
  // of releasing the elements and calling the base class destructor.
  // We won't actually emit the block until we finish with the destructor body.
  epilogBB = new (SGM.M) SILBasicBlock(&F, "destructor");
  
  // Emit the destructor body, if any.
  if (dd)
    visit(dd->getBody());
  
  if (!emitEpilogBB(dd))
    return;
  
  // Release our members.
  // FIXME: generic params
  // FIXME: Can a destructor always consider its fields fragile like this?
  for (Decl *member : cd->getMembers()) {
    if (VarDecl *vd = dyn_cast<VarDecl>(member)) {
      if (vd->isProperty())
        continue;
      TypeLoweringInfo const &ti = getTypeLoweringInfo(vd->getType());
      if (!ti.isTrivial()) {
        SILValue addr = B.createRefElementAddr(dd, thisValue, vd,
                                          ti.getLoweredType().getAddressType());
        if (ti.isAddressOnly()) {
          B.createDestroyAddr(dd, addr);
        } else {
          SILValue field = B.createLoad(dd, addr);
          emitReleaseRValue(dd, field);
        }
      }
    }
  }
  
  // If we have a base class, invoke its destructor.
  SILType objectPtrTy = SILType::getObjectPointerType(F.getContext());
  if (Type baseTy = cd->getBaseClass()) {
    ClassDecl *baseClass = baseTy->getClassOrBoundGenericClass();
    
    // FIXME: We can't sensibly call up to ObjC dealloc methods right now
    // because they aren't really destroying destructors.
    if (baseClass->hasClangNode() && baseClass->isObjC()) {
      thisValue = B.createRefToObjectPointer(dd, thisValue, objectPtrTy);
      B.createReturn(dd, thisValue);
      return;
    }
    
    SILConstant dtorConstant =
      SILConstant(baseClass, SILConstant::Kind::Destroyer);
    SILType baseSILTy = getLoweredLoadableType(baseTy);
    SILValue baseThis = B.createUpcast(dd, thisValue, baseSILTy);
    ManagedValue dtorValue = emitMethodRef(dd, baseThis, dtorConstant,
                                           /*innerSubstitutions*/ {});
    thisValue = B.createApply(dd, dtorValue.forward(*this),
                              objectPtrTy,
                              baseThis);
  } else {
    thisValue = B.createRefToObjectPointer(dd, thisValue, objectPtrTy);
  }
  B.createReturn(dd, thisValue);
}

static void emitConstructorMetatypeArg(SILGenFunction &gen,
                                       ConstructorDecl *ctor) {
  // In addition to the declared arguments, the constructor implicitly takes
  // the metatype as its first argument, like a static function.
  Type metatype = ctor->getType()->castTo<AnyFunctionType>()->getInput();
  new (gen.F.getModule()) SILArgument(gen.getLoweredType(metatype),
                                      gen.F.begin());
}

static RValue emitImplicitValueConstructorArg(SILGenFunction &gen,
                                              SILLocation loc,
                                              Type ty) {
  SILType argTy = gen.getLoweredType(ty);
  
  // Restructure tuple arguments.
  if (TupleType *tupleTy = argTy.getAs<TupleType>()) {
    RValue tuple(tupleTy->getCanonicalType());
    for (auto &field : tupleTy->getFields())
      tuple.addElement(
                   emitImplicitValueConstructorArg(gen, loc, field.getType()));

    return tuple;
  } else {
    SILValue arg = new (gen.F.getModule()) SILArgument(gen.getLoweredType(ty),
                                                       gen.F.begin());
    return RValue(gen, ManagedValue(arg, ManagedValue::Unmanaged));
  }
}

namespace {
  class ImplicitValueInitialization : public SingleInitializationBase {
    SILValue slot;
  public:
    ImplicitValueInitialization(SILValue slot, Type type)
      : SingleInitializationBase(type), slot(slot)
    {}
    
    SILValue getAddressOrNull() override {
      return slot;
    };
  };
}

static void emitImplicitValueDefaultConstructor(SILGenFunction &gen,
                                                ConstructorDecl *ctor) {
  emitConstructorMetatypeArg(gen, ctor);

  SILType thisTy
    = gen.getLoweredType(ctor->getImplicitThisDecl()->getType());
  
  // FIXME: We should actually elementwise default-construct the elements.
  if (thisTy.isAddressOnly()) {
    SILValue resultSlot
      = new (gen.F.getModule()) SILArgument(thisTy, gen.F.begin());
    gen.B.createInitializeVar(ctor, resultSlot, /*canDefaultConstruct*/ false);
    gen.B.createReturn(ctor, gen.emitEmptyTuple(ctor));
  } else {
    SILValue addr = gen.B.createAllocVar(ctor, AllocKind::Stack, thisTy);
    gen.B.createInitializeVar(ctor, addr, /*canDefaultConstruct*/ false);
    SILValue result = gen.B.createLoad(ctor, addr);
    gen.B.createReturn(ctor, result);
  }
}

static void emitImplicitValueConstructor(SILGenFunction &gen,
                                         ConstructorDecl *ctor) {
  auto *TP = cast<TuplePattern>(ctor->getArguments());
  SILType thisTy
    = gen.getLoweredType(ctor->getImplicitThisDecl()->getType());

  if (TP->getFields().empty() &&
      !thisTy.getCompoundTypeInfo()->getElements().empty()) {
    // Emit a default constructor.
    return emitImplicitValueDefaultConstructor(gen, ctor);
  }

  emitConstructorMetatypeArg(gen, ctor);

  // Emit the elementwise arguments.
  SmallVector<RValue, 4> elements;
  for (size_t i = 0; i < TP->getFields().size(); ++i) {
    auto *P = cast<TypedPattern>(TP->getFields()[i].getPattern());
    
    elements.push_back(
                     emitImplicitValueConstructorArg(gen, ctor, P->getType()));
  }
  
  // If we have an indirect return slot, initialize it in-place in the implicit
  // return slot.
  if (thisTy.isAddressOnly()) {
    SILValue resultSlot
      = new (gen.F.getModule()) SILArgument(thisTy, gen.F.begin());
    
    for (size_t i = 0; i < elements.size(); ++i) {
      SILType argTy = gen.getLoweredType(elements[i].getType());

      // Store each argument in the corresponding element of 'this'.
      SILValue slot = gen.B.createElementAddr(ctor, resultSlot, i,
                                              argTy.getAddressType());
      InitializationPtr init(new ImplicitValueInitialization(slot,
                                                         elements[i].getType()));
      std::move(elements[i]).forwardInto(gen, init.get());
    }
    gen.B.createReturn(ctor, gen.emitEmptyTuple(ctor));
    return;
  }
  
  // Otherwise, build a struct value directly from the elements.
  SmallVector<SILValue, 4> eltValues;
  for (RValue &rv : elements) {
    eltValues.push_back(std::move(rv).forwardAsSingleValue(gen));
  }
  
  SILValue thisValue = gen.B.createStruct(ctor, thisTy, eltValues);
  gen.B.createReturn(ctor, thisValue);
  return;
}

void SILGenFunction::emitValueConstructor(ConstructorDecl *ctor) {
  // If there's no body, this is the implicit elementwise constructor.
  if (!ctor->getBody())
    return emitImplicitValueConstructor(*this, ctor);
  
  // Emit the prolog.
  emitConstructorMetatypeArg(*this, ctor);
  emitProlog(ctor->getArguments(), ctor->getImplicitThisDecl()->getType());
  
  // Get the 'this' decl and type.
  VarDecl *thisDecl = ctor->getImplicitThisDecl();
  SILType thisTy = getLoweredType(thisDecl->getType());
  assert(!thisTy.hasReferenceSemantics() && "can't emit a ref type ctor here");
  assert(!ctor->getAllocThisExpr() && "alloc_this expr for value type?!");

  // Emit a local variable for 'this'.
  // FIXME: The (potentially partially initialized) variable would need to be
  // cleaned up on an error unwind.
  
  // If we don't need to heap-allocate the local 'this' and we're returning
  // indirectly, we can emplace 'this' in the return slot.
  bool canConstructInPlace
    = thisDecl->hasFixedLifetime() && IndirectReturnAddress.isValid();
  if (canConstructInPlace)
    VarLocs[thisDecl] = {SILValue(), IndirectReturnAddress};
  else
    emitLocalVariable(thisDecl);

  SILValue thisLV = VarLocs[thisDecl].address;
  
  // Emit a default initialization of the this value.
  // Note that this initialization *cannot* be lowered to a
  // default constructor--we're already in a constructor!
  B.createInitializeVar(ctor, thisLV, /*CanDefaultConstruct*/ false);
  
  // Create a basic block to jump to for the implicit 'this' return.
  // We won't emit this until after we've emitted the body.
  epilogBB = new (SGM.M) SILBasicBlock(&F, "constructor");

  // Emit the constructor body.
  visit(ctor->getBody());
  
  // Return 'this' in the epilog.
  if (!emitEpilogBB(ctor))
    return;

  // If we constructed in-place, we're done.
  if (canConstructInPlace) {
    B.createReturn(ctor, emitEmptyTuple(ctor));
    return;
  }
  
  // If 'this' is address-only, copy 'this' into the indirect return slot.
  if (thisTy.isAddressOnly()) {
    assert(IndirectReturnAddress &&
           "no indirect return for address-only ctor?!");
    SILValue thisBox = VarLocs[thisDecl].box;
    assert(thisBox &&
           "address-only non-heap this should have been allocated in-place");
    // We have to do a non-take copy because someone else may be using the box.
    B.createCopyAddr(ctor, thisLV, IndirectReturnAddress,
                     /*isTake=*/ false,
                     /*isInit=*/ true);
    B.createRelease(ctor, thisBox);
    B.createReturn(ctor, emitEmptyTuple(ctor));
    return;
  }

  // Otherwise, load and return the final 'this' value.
  SILValue thisValue = B.createLoad(ctor, thisLV);
  if (SILValue thisBox = VarLocs[thisDecl].box) {
    // We have to do a retain because someone else may be using the box.
    emitRetainRValue(ctor, thisValue);
    B.createRelease(ctor, thisBox);
  } else {
    // We can just take ownership from the stack slot and consider it
    // deinitialized.
    B.createDeallocVar(ctor, AllocKind::Stack, thisLV);
  }
  B.createReturn(ctor, thisValue);
}

namespace {
  // Unlike the ArgumentInitVisitor, this visitor generates arguments but leaves
  // them destructured instead of storing them to lvalues so that the
  // argument set can be easily forwarded to another function.
  class ArgumentForwardVisitor
    : public PatternVisitor<ArgumentForwardVisitor>
  {
    SILGenFunction &gen;
    SmallVectorImpl<SILValue> &args;
  public:
    ArgumentForwardVisitor(SILGenFunction &gen,
                           SmallVectorImpl<SILValue> &args)
      : gen(gen), args(args) {}
    
    void makeArgument(Type ty) {
      assert(ty && "no type?!");
      // Destructure tuple arguments.
      if (TupleType *tupleTy = ty->getAs<TupleType>()) {
        for (auto &field : tupleTy->getFields())
          makeArgument(field.getType());
      } else {
        SILValue arg = new (gen.F.getModule()) SILArgument(gen.getLoweredType(ty),
                                                       gen.F.begin());
        args.push_back(arg);
      }
    }

    void visitParenPattern(ParenPattern *P) {
      visit(P->getSubPattern());
    }
    
    void visitTypedPattern(TypedPattern *P) {
      // FIXME: work around a bug in visiting the "this" argument of methods
      if (isa<NamedPattern>(P->getSubPattern()))
        makeArgument(P->getType());
      else
        visit(P->getSubPattern());
    }
    
    void visitTuplePattern(TuplePattern *P) {
      for (auto &elt : P->getFields())
        visit(elt.getPattern());
    }
    
    void visitAnyPattern(AnyPattern *P) {
      makeArgument(P->getType());
    }
    
    void visitNamedPattern(NamedPattern *P) {
      makeArgument(P->getType());
    }
  };
} // end anonymous namespace

void SILGenFunction::emitClassConstructorAllocator(ConstructorDecl *ctor) {
  // Emit the prolog. Since we're just going to forward our args directly
  // to the initializer, don't allocate local variables for them.
  emitConstructorMetatypeArg(*this, ctor);

  SmallVector<SILValue, 8> args;
  
  // Allocate the "this" value.
  VarDecl *thisDecl = ctor->getImplicitThisDecl();
  SILType thisTy = getLoweredType(thisDecl->getType());
  assert(thisTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");
  SILValue thisValue;
  if (ctor->getAllocThisExpr()) {
    FullExpr allocThisScope(Cleanups);
    // If the constructor has an alloc-this expr, emit it to get "this".
    thisValue = visit(ctor->getAllocThisExpr()).forwardAsSingleValue(*this);
    assert(thisValue.getType() == thisTy &&
           "alloc-this expr type did not match this type?!");
  } else {
    // Otherwise, just emit an alloc_ref instruction for the default allocation
    // path.
    // FIXME: should have a cleanup in case of exception
    thisValue = B.createAllocRef(ctor, AllocKind::Heap, thisTy);
  }
  args.push_back(thisValue);

  // Forward the constructor arguments.
  ArgumentForwardVisitor(*this, args).visit(ctor->getArguments());

  // Call the initializer.
  SILConstant initConstant = SILConstant(ctor, SILConstant::Kind::Initializer);
  ManagedValue initVal = emitMethodRef(ctor, thisValue, initConstant,
                                       ctor->getForwardingSubstitutions());
  
  SILValue initedThisValue
    = B.createApply(ctor, initVal.forward(*this), thisTy, args);
  
  // Return the initialized 'this'.
  B.createReturn(ctor, initedThisValue);
}

static void emitClassImplicitConstructorInitializer(SILGenFunction &gen,
                                                    ConstructorDecl *ctor) {
  // The default constructor is currently a no-op. Just return back 'this'.
  // FIXME: We should default-construct fields maybe?

  assert(cast<TuplePattern>(ctor->getArguments())->getNumFields() == 0
         && "implicit class ctor has arguments?!");

  VarDecl *thisDecl = ctor->getImplicitThisDecl();
  SILType thisTy = gen.getLoweredLoadableType(thisDecl->getType());
  SILValue thisArg = new (gen.SGM.M) SILArgument(thisTy, gen.F.begin());
  assert(thisTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");
  
  gen.B.createReturn(ctor, thisArg);
}

void SILGenFunction::emitClassConstructorInitializer(ConstructorDecl *ctor) {
  // If there's no body, this is the implicit constructor.
  if (!ctor->getBody())
    return emitClassImplicitConstructorInitializer(*this, ctor);
  
  // Emit the 'this' argument and make an lvalue for it.
  VarDecl *thisDecl = ctor->getImplicitThisDecl();
  SILType thisTy = getLoweredLoadableType(thisDecl->getType());
  SILValue thisArg = new (SGM.M) SILArgument(thisTy, F.begin());
  assert(thisTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");

  // FIXME: The (potentially partially initialized) value here would need to be
  // cleaned up on a constructor failure unwinding.
  emitLocalVariable(thisDecl);
  SILValue thisLV = VarLocs[thisDecl].address;
  emitStore(ctor, ManagedValue(thisArg, ManagedValue::Unmanaged), thisLV);
  
  // Emit the prolog for the non-this arguments.
  emitProlog(ctor->getArguments(), TupleType::getEmpty(F.getContext()));

  // Create a basic block to jump to for the implicit 'this' return.
  // We won't emit the block until after we've emitted the body.
  epilogBB = new (SGM.M) SILBasicBlock(&F, "constructor");
  
  // Emit the constructor body.
  visit(ctor->getBody());
  
  // Return 'this' in the epilog.
  if (!emitEpilogBB(ctor))
    return;

  // Load and return the final 'this'.
  SILValue thisValue = B.createLoad(ctor, thisLV);
  if (SILValue thisBox = VarLocs[thisDecl].box) {
    // We have to do a retain because someone else may be using the box.
    emitRetainRValue(ctor, thisValue);
    B.createRelease(ctor, thisBox);
  } else {
    // We can just take ownership from the stack slot and consider it
    // deinitialized.
    B.createDeallocVar(ctor, AllocKind::Stack, thisLV);
  }
  B.createReturn(ctor, thisValue);
}

static void forwardCaptureArgs(SILGenFunction &gen,
                               SmallVectorImpl<SILValue> &args,
                               ValueDecl *capture) {
  ASTContext &c = capture->getASTContext();
  
  auto addSILArgument = [&](SILType t) {
    args.push_back(new (gen.SGM.M) SILArgument(t, gen.F.begin()));
  };

  switch (getDeclCaptureKind(capture)) {
  case CaptureKind::Box: {
    SILType ty = gen.getLoweredType(capture->getTypeOfReference());
    // Forward the captured owning ObjectPointer.
    addSILArgument(SILType::getObjectPointerType(c));
    // Forward the captured value address.
    addSILArgument(ty);
    break;
  }
  case CaptureKind::Byref: {
    // Forward the captured address.
    SILType ty = gen.getLoweredType(capture->getTypeOfReference());
    addSILArgument(ty);
    break;
  }
  case CaptureKind::Constant: {
    // Forward the captured value.
    SILType ty = gen.getLoweredType(capture->getType());
    addSILArgument(ty);
    break;
  }
  case CaptureKind::GetterSetter: {
    // Forward the captured setter.
    Type setTy = gen.SGM.Types.getPropertyType(SILConstant::Kind::Setter,
                                               capture->getType());
    addSILArgument(gen.getLoweredType(setTy));
    [[clang::fallthrough]];
  }
  case CaptureKind::Getter: {
    // Forward the captured getter.
    Type setTy = gen.SGM.Types.getPropertyType(SILConstant::Kind::Getter,
                                               capture->getType());
    addSILArgument(gen.getLoweredType(setTy));
    break;
  }
  }
}

void SILGenFunction::emitCurryThunk(FuncExpr *fe,
                                    SILConstant from, SILConstant to) {
  SmallVector<SILValue, 8> curriedArgs;
  
  unsigned paramCount = from.uncurryLevel + 1;
  
  // If the function has implicit closure context arguments, forward them.
  if (!fe->getCaptures().empty()) {
    for (auto capture : fe->getCaptures())
      forwardCaptureArgs(*this, curriedArgs, capture);
    --paramCount;
  }

  // Forward the curried formal arguments.
  ArgumentForwardVisitor forwarder(*this, curriedArgs);
  for (auto *paramPattern : fe->getBodyParamPatterns().slice(0, paramCount))
    forwarder.visit(paramPattern);
  
  // FIXME: Forward archetypes and specialize if the function is generic.
  
  // Partially apply the next uncurry level and return the result closure.
  auto toFn = B.createFunctionRef(fe, SGM.getFunction(to));
  SILType resultTy
    = SGM.getConstantType(from).getFunctionTypeInfo()->getResultType();
  auto toClosure = B.createPartialApply(fe, toFn, curriedArgs, resultTy);
  B.createReturn(fe, toClosure);
}

RValue SILGenFunction::visitInterpolatedStringLiteralExpr(
                                              InterpolatedStringLiteralExpr *E,
                                              SGFContext C)
{
  return visit(E->getSemanticExpr());
}

RValue SILGenFunction::visitCollectionExpr(CollectionExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr());
}

RValue SILGenFunction::visitRebindThisInConstructorExpr(
                                RebindThisInConstructorExpr *E, SGFContext C) {
  // FIXME: Use a different instruction from 'downcast'. IRGen can make
  // "rebind this" into a no-op if the called constructor is a Swift one.
  ManagedValue newThis = visit(E->getSubExpr()).getAsSingleValue(*this);
  if (!newThis.getType().getSwiftRValueType()
        ->isEqual(E->getThis()->getType())) {
    assert(!newThis.getType().isAddress() &&
           newThis.getType().hasReferenceSemantics() &&
           "delegating ctor type mismatch for non-reference type?!");
    CleanupsDepth newThisCleanup = newThis.getCleanup();
    SILValue newThisValue = B.createDowncast(E, newThis.getValue(),
                              getLoweredLoadableType(E->getThis()->getType()));
    newThis = ManagedValue(newThisValue, newThisCleanup);
  }
  
  SILValue thisAddr = emitReferenceToDecl(E, E->getThis()).getUnmanagedValue();
  newThis.assignInto(*this, E, thisAddr);
  
  return emitEmptyTupleRValue(E);
}

RValue SILGenFunction::visitArchetypeSubscriptExpr(
                                     ArchetypeSubscriptExpr *E, SGFContext C) {
  llvm_unreachable("not implemented");
}

RValue SILGenFunction::visitExistentialSubscriptExpr(
                                   ExistentialSubscriptExpr *E, SGFContext C) {
  llvm_unreachable("not implemented");
}

RValue SILGenFunction::visitBridgeToBlockExpr(BridgeToBlockExpr *E,
                                              SGFContext C) {
  SILValue func = visit(E->getSubExpr()).forwardAsSingleValue(*this);
  // Thicken thin function value if necessary.
  // FIXME: This should go away when Swift typechecking learns how to handle
  // thin functions.
  func = emitGeneralizedValue(E, func);
  
  // Emit the bridge_to_block instruction.
  SILValue block = B.createBridgeToBlock(E, func,
                                         getLoweredLoadableType(E->getType()));
  return RValue(*this, emitManagedRValueWithCleanup(block));
}

RValue SILGenFunction::visitIfExpr(IfExpr *E, SGFContext C) {
  // FIXME: We could avoid imploding and reexploding tuples here.
  // FIXME: "emit into" optimization
  
  Condition cond = emitCondition(E, E->getCondExpr(),
                                 /*hasFalse*/ true,
                                 /*invertCondition*/ false,
                                 getLoweredType(E->getType()));
  
  cond.enterTrue(B);
  SILValue trueValue = visit(E->getThenExpr()).forwardAsSingleValue(*this);
  cond.exitTrue(B, trueValue);
  
  cond.enterFalse(B);
  SILValue falseValue = visit(E->getElseExpr()).forwardAsSingleValue(*this);
  cond.exitFalse(B, falseValue);
  
  SILBasicBlock *cont = cond.complete(B);
  assert(cont && "no continuation block for if expr?!");
  
  SILValue result = cont->bbarg_begin()[0];
  
  return RValue(*this, emitManagedRValueWithCleanup(result));
}

RValue SILGenFunction::visitZeroValueExpr(ZeroValueExpr *E, SGFContext C) {
  SILValue zero = B.createBuiltinZero(E,
                                      getLoweredLoadableType(E->getType()));
  return RValue(*this, ManagedValue(zero, ManagedValue::Unmanaged));
}

RValue SILGenFunction::visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

SILValue SILGenFunction::emitGeneralizedValue(SILLocation loc, SILValue v) {
  assert(v.getType().getUncurryLevel() == 0 &&
         "uncurried functions shouldn't be used as swift values");
  // Thicken thin functions.
  if (v.getType().is<AnyFunctionType>() &&
      v.getType().castTo<AnyFunctionType>()->isThin()) {
    // Thunk functions to the standard "freestanding" calling convention.
    if (v.getType().getFunctionTypeInfo()->getAbstractCC()
          != AbstractCC::Freestanding) {
      SILType freestandingType
        = getLoweredLoadableType(v.getType().getSwiftType(),
                                 AbstractCC::Freestanding,
                                 0);
      v = B.createConvertCC(loc, v, freestandingType);
    }
    
    Type thickTy = getThickFunctionType(v.getType().getSwiftType());
    
    v = B.createThinToThickFunction(loc, v,
                                    getLoweredLoadableType(thickTy));
  }
  
  return v;
}

void SILGenFunction::emitStore(SILLocation loc, ManagedValue src,
                               SILValue destAddr) {
  SILValue fwdSrc = src.forward(*this);
  // If we store a function value, we lose its thinness.
  // FIXME: This should go away when Swift typechecking learns how to handle
  // thin functions.
  fwdSrc = emitGeneralizedValue(loc, fwdSrc);  
  B.createStore(loc, fwdSrc, destAddr);
}

RValue SILGenFunction::emitEmptyTupleRValue(SILLocation loc) {
  return RValue(CanType(TupleType::getEmpty(F.getContext())));
}
