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
#include "Scope.h"
#include "swift/AST/AST.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/SourceManager.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"
#include "Initialization.h"
#include "OwnershipConventions.h"
#include "LValue.h"
#include "RValue.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace Lowering;

namespace {
  class CleanupRValue : public Cleanup {
    const TypeLowering &Lowering;
    SILValue Value;
  public:
    CleanupRValue(const TypeLowering &lowering, SILValue value)
      : Lowering(lowering), Value(value) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      Lowering.emitDestroyRValue(gen.B, l, Value);
    }
  };
  
  class CleanupMaterializedValue : public Cleanup {
    const TypeLowering &Lowering;
    SILValue Address;
  public:
    CleanupMaterializedValue(const TypeLowering &lowering, SILValue address)
      : Lowering(lowering), Address(address) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      Lowering.emitDestroyAddress(gen.B, l, Address);
    }
  };
  
  class CleanupMaterializedAddressOnlyValue : public Cleanup {
    SILValue address;
  public:
    CleanupMaterializedAddressOnlyValue(SILValue address) : address(address) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.createDestroyAddr(l, address);
    }
  };
} // end anonymous namespace

SILGenFunction::OpaqueValueRAII::~OpaqueValueRAII() {
  // Destroy the value, unless it was both uniquely referenced and consumed.
  auto entry = Self.OpaqueValues.find(OpaqueValue);
  if (!OpaqueValue->isUniquelyReferenced() || !entry->second.second) {
    SILValue &value = entry->second.first;
    auto &lowering = Self.getTypeLowering(value.getType().getSwiftRValueType());
    if (lowering.isTrivial()) {
      // Nothing to do.
    } else if (lowering.isAddressOnly()) {
      lowering.emitDestroyAddress(Self.B, OpaqueValue, value);
    } else {
      lowering.emitDestroyRValue(Self.B, OpaqueValue, value);
    }
  }

  // Remove the opaque value.
  Self.OpaqueValues.erase(entry);
}

ManagedValue SILGenFunction::emitManagedRetain(SILLocation loc,
                                               SILValue v) {
  auto &lowering = getTypeLowering(v.getType().getSwiftRValueType());
  return emitManagedRetain(loc, v, lowering);
}

ManagedValue SILGenFunction::emitManagedRetain(SILLocation loc,
                                               SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType() == v.getType());
  if (lowering.isTrivial())
    return ManagedValue(v, ManagedValue::Unmanaged);
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");

  lowering.emitRetain(B, loc, v);
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v) {
  auto &lowering = getTypeLowering(v.getType().getSwiftRValueType());
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType() == v.getType());
  if (lowering.isTrivial()) {
    return ManagedValue(v, ManagedValue::Unmanaged);
  } else if (lowering.isAddressOnly()) {
    Cleanups.pushCleanup<CleanupMaterializedAddressOnlyValue>(v);
    return ManagedValue(v, getCleanupsDepth());
  } else {
    Cleanups.pushCleanup<CleanupRValue>(lowering, v);
    return ManagedValue(v, getCleanupsDepth());
  }
}

void SILGenFunction::emitExprInto(Expr *E, Initialization *I) {
  // FIXME: actually emit into the initialization. The initialization should
  // be passed down in the context argument to visit, and it should be the
  // visit*Expr method's responsibility to store to it if possible.
  RValue result = emitRValue(E, SGFContext(I));
  if (result)
    std::move(result).forwardInto(*this, I, E);
}

namespace {
  class RValueEmitter
      : public Lowering::ExprVisitor<RValueEmitter, RValue, SGFContext> {
    SILGenFunction &SGF;
    typedef Lowering::ExprVisitor<RValueEmitter,RValue,SGFContext> super;
  public:
    RValueEmitter(SILGenFunction &SGF) : SGF(SGF) {}

    using super::visit;
    RValue visit(Expr *E) {
      return visit(E, SGFContext());
    }

    RValue visitErrorExpr(ErrorExpr *E, SGFContext C) {
      llvm_unreachable("shouldn't see an unreachable declaration!");
    }

    RValue visitApplyExpr(ApplyExpr *E, SGFContext C);

    RValue visitDeclRefExpr(DeclRefExpr *E, SGFContext C);
    RValue visitSuperRefExpr(SuperRefExpr *E, SGFContext C);
    RValue visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E,
                                            SGFContext C);
  
    RValue visitIntegerLiteralExpr(IntegerLiteralExpr *E, SGFContext C);
    RValue visitFloatLiteralExpr(FloatLiteralExpr *E, SGFContext C);
    RValue visitCharacterLiteralExpr(CharacterLiteralExpr *E, SGFContext C);
    RValue visitStringLiteralExpr(StringLiteralExpr *E, SGFContext C);
    RValue visitLoadExpr(LoadExpr *E, SGFContext C);
    RValue visitMaterializeExpr(MaterializeExpr *E, SGFContext C);
    RValue visitDerivedToBaseExpr(DerivedToBaseExpr *E, SGFContext C);
    RValue visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                       SGFContext C);
    RValue visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, SGFContext C);
    RValue visitRequalifyExpr(RequalifyExpr *E, SGFContext C);
    RValue visitFunctionConversionExpr(FunctionConversionExpr *E,
                                       SGFContext C);
    RValue visitErasureExpr(ErasureExpr *E, SGFContext C);
    RValue visitUnconditionalCheckedCastExpr(UnconditionalCheckedCastExpr *E,
                                             SGFContext C);
    RValue visitIsaExpr(IsaExpr *E, SGFContext C);
    RValue visitParenExpr(ParenExpr *E, SGFContext C);
    RValue visitTupleExpr(TupleExpr *E, SGFContext C);
    RValue visitScalarToTupleExpr(ScalarToTupleExpr *E, SGFContext C);
    RValue visitSpecializeExpr(SpecializeExpr *E, SGFContext C);
    RValue visitAddressOfExpr(AddressOfExpr *E, SGFContext C);
    RValue visitMemberRefExpr(MemberRefExpr *E, SGFContext C);
    RValue visitDynamicMemberRefExpr(DynamicMemberRefExpr *E, SGFContext C);
    RValue visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E,
                                       SGFContext C);
    RValue visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E,
                                         SGFContext C);
    RValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E,
                                         SGFContext C);
    RValue visitModuleExpr(ModuleExpr *E, SGFContext C);
    RValue visitTupleElementExpr(TupleElementExpr *E, SGFContext C);
    RValue visitSubscriptExpr(SubscriptExpr *E, SGFContext C);
    RValue visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E,
                                       SGFContext C);
    RValue visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E,
                                         SGFContext C);
    RValue visitTupleShuffleExpr(TupleShuffleExpr *E, SGFContext C);
    RValue visitNewArrayExpr(NewArrayExpr *E, SGFContext C);
    RValue visitMetatypeExpr(MetatypeExpr *E, SGFContext C);
    RValue visitPipeClosureExpr(PipeClosureExpr *E, SGFContext C);
    RValue visitClosureExpr(ClosureExpr *E, SGFContext C);
    RValue visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                              SGFContext C);
    RValue visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E,
                                           SGFContext C);
    RValue visitCollectionExpr(CollectionExpr *E, SGFContext C);
    RValue visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E,
                                            SGFContext C);
    RValue visitBridgeToBlockExpr(BridgeToBlockExpr *E, SGFContext C);
    RValue visitIfExpr(IfExpr *E, SGFContext C);
    RValue visitZeroValueExpr(ZeroValueExpr *E, SGFContext C);
    RValue visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C);
    RValue visitAssignExpr(AssignExpr *E, SGFContext C);

    RValue visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C);
  };
}

RValue RValueEmitter::visitApplyExpr(ApplyExpr *E, SGFContext C) {
  return SGF.emitApplyExpr(E, C);
}

SILValue SILGenFunction::emitEmptyTuple(SILLocation loc) {
  return B.createTuple(loc,
                       getLoweredType(TupleType::getEmpty(SGM.M.getASTContext())),
                       {});
}

SILValue SILGenFunction::emitGlobalFunctionRef(SILLocation loc,
                                               SILDeclRef constant) {
  assert(!LocalConstants.count(constant) &&
         "emitting ref to local constant without context?!");
  if (constant.hasDecl() &&
      isa<BuiltinModule>(constant.getDecl()->getDeclContext())) {
    return B.createBuiltinFunctionRef(loc, cast<FuncDecl>(constant.getDecl()),
                                      SGM.getConstantType(constant));
  }
  
  // If the constant is a curry thunk we haven't emitted yet, emit it.
  if (constant.isCurried && !SGM.hasFunction(constant)) {
    // Non-functions can't be referenced uncurried.
    FuncDecl *fd = cast<FuncDecl>(constant.getDecl());
    
    // Getters and setters can't be referenced uncurried.
    assert(!fd->isGetterOrSetter());
    
    // FIXME: Thunks for instance methods of generics.
    assert(!(fd->isInstanceMember() && isa<ProtocolDecl>(fd->getDeclContext()))
           && "currying generic method not yet supported");

    // FIXME: Curry thunks for generic methods don't work right yet, so skip
    // emitting thunks for them
    assert(!(fd->getType()->is<AnyFunctionType>() &&
             fd->getType()->castTo<AnyFunctionType>()->getResult()
               ->is<PolymorphicFunctionType>()));
    
    // Reference the next uncurrying level of the function.
    SILDeclRef next = SILDeclRef(fd, SILDeclRef::Kind::Func,
                                 constant.uncurryLevel + 1);
    // If the function is fully uncurried and natively ObjC, reference its ObjC
    // entry point.
    if (!next.isCurried && fd->hasClangNode())
      next = next.asObjC();
    
    SGM.emitCurryThunk(constant, next, fd);
  }
  
  return B.createFunctionRef(loc, SGM.getFunction(constant));
}

SILValue SILGenFunction::emitUnmanagedFunctionRef(SILLocation loc,
                                               SILDeclRef constant) {
  // If this is a reference to a local constant, grab it.
  if (LocalConstants.count(constant)) {
    return LocalConstants[constant];
  }
  
  // Otherwise, use a global FunctionRefInst.
  return emitGlobalFunctionRef(loc, constant);
}

ManagedValue SILGenFunction::emitFunctionRef(SILLocation loc,
                                             SILDeclRef constant) {
  // If this is a reference to a local constant, grab it.
  if (LocalConstants.count(constant)) {
    SILValue v = LocalConstants[constant];
    return emitManagedRetain(loc, v);
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
    assert((uncurryLevel == SILDeclRef::ConstructAtNaturalUncurryLevel
            || uncurryLevel == 0)
           && "uncurry level doesn't make sense for types");
    return ManagedValue(B.createMetatype(loc, getLoweredType(declType)),
                        ManagedValue::Unmanaged);
  }
  
  // If this is a reference to a var, produce an address.
  if (VarDecl *var = dyn_cast<VarDecl>(decl)) {
    assert((uncurryLevel == SILDeclRef::ConstructAtNaturalUncurryLevel
            || uncurryLevel == 0)
           && "uncurry level doesn't make sense for vars");

    assert(!var->isProperty() &&
           "property accessors should go through ");
    
    // For local decls, use the address we allocated.
    auto It = VarLocs.find(decl);
    if (It != VarLocs.end())
      return ManagedValue(It->second.address, ManagedValue::LValue);

    // If this is a global variable, invoke its accessor function to get its
    // address.
    return emitGlobalVariable(*this, loc, var);
  }
  
  // If the referenced decl isn't a VarDecl, it should be a constant of some
  // sort.
  assert(!decl->isReferencedAsLValue());

  // If the referenced decl is a local func with context, then the SILDeclRef
  // uncurry level is one deeper (for the context vars).
  if (auto *fd = dyn_cast<FuncDecl>(decl)) {
    if (fd->getCaptureInfo().hasLocalCaptures() &&
        uncurryLevel != SILDeclRef::ConstructAtNaturalUncurryLevel)
      ++uncurryLevel;
  }

  return emitFunctionRef(loc, SILDeclRef(decl, uncurryLevel));
}

RValue RValueEmitter::visitDeclRefExpr(DeclRefExpr *E, SGFContext C) {
  if (E->getType()->is<LValueType>())
    return SGF.emitLValueAsRValue(E);
  return RValue(SGF,
                SGF.emitReferenceToDecl(E, E->getDecl(), E->getType(), 0), E);
}

RValue RValueEmitter::visitSuperRefExpr(SuperRefExpr *E, SGFContext C) {
  if (E->getType()->is<LValueType>())
    return SGF.emitLValueAsRValue(E);
  return RValue(SGF,
                SGF.emitReferenceToDecl(E, E->getSelf(), E->getType(), 0), E);
}

RValue RValueEmitter::visitOtherConstructorDeclRefExpr(
                                OtherConstructorDeclRefExpr *E, SGFContext C) {
  // This should always be a child of an ApplyExpr and so will be emitted by
  // SILGenApply.
  llvm_unreachable("unapplied reference to constructor?!");
}

RValue RValueEmitter::visitIntegerLiteralExpr(IntegerLiteralExpr *E,
                                               SGFContext C) {
  return RValue(SGF, ManagedValue(SGF.B.createIntegerLiteral(E),
                                  ManagedValue::Unmanaged), E);
}
RValue RValueEmitter::visitFloatLiteralExpr(FloatLiteralExpr *E,
                                             SGFContext C) {
  return RValue(SGF, ManagedValue(SGF.B.createFloatLiteral(E),
                                  ManagedValue::Unmanaged), E);
}
RValue RValueEmitter::visitCharacterLiteralExpr(CharacterLiteralExpr *E,
                                                 SGFContext C)
{
  return RValue(SGF, ManagedValue(SGF.B.createIntegerLiteral(E),
                                  ManagedValue::Unmanaged), E);
}
RValue RValueEmitter::visitStringLiteralExpr(StringLiteralExpr *E,
                                             SGFContext C) {
  SILType ty = SGF.getLoweredLoadableType(E->getType());
  SILValue string = SGF.B.createStringLiteral(E, ty);
  return RValue(SGF, ManagedValue(string, ManagedValue::Unmanaged), E);
}

RValue RValueEmitter::visitLoadExpr(LoadExpr *E, SGFContext C) {
  LValue lv = SGF.emitLValue(E->getSubExpr());
  return RValue(SGF, SGF.emitLoadOfLValue(E, lv, C), E);
}

SILValue SILGenFunction::emitTemporaryAllocation(SILLocation loc,
                                                 SILType ty) {
  ty = ty.getObjectType();
  auto alloc = B.createAllocStack(loc, ty);
  enterDeallocStackCleanup(loc, alloc->getContainerResult());
  return alloc->getAddressResult();
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
  return emitTemporaryAllocation(loc, ty.getObjectType());
}

Materialize SILGenFunction::emitMaterialize(SILLocation loc, ManagedValue v) {
  assert(!v.isLValue() && "materializing an lvalue?!");
  // Address-only values are already materialized.
  if (v.getType().isAddress()) {
    assert(v.getType().isAddressOnly(SGM.M) && "can't materialize an l-value");
    return Materialize{v.getValue(), v.getCleanup()};
  }

  auto &lowering = getTypeLowering(v.getType().getSwiftType());
  
  // We don't use getBufferForExprResult here because the result of a
  // MaterializeExpr is *not* the value, but an lvalue reference to the value.
  SILValue tmpMem = emitTemporaryAllocation(loc, v.getType());
  v.forwardInto(*this, loc, tmpMem);
  
  CleanupsDepth valueCleanup = CleanupsDepth::invalid();
  if (!lowering.isTrivial()) {
    Cleanups.pushCleanup<CleanupMaterializedValue>(lowering, tmpMem);
    valueCleanup = getCleanupsDepth();
  }
  
  return Materialize{tmpMem, valueCleanup};
}

RValue RValueEmitter::visitMaterializeExpr(MaterializeExpr *E, SGFContext C) {
  // Always an lvalue.
  return SGF.emitLValueAsRValue(E);
}

RValue RValueEmitter::visitDerivedToBaseExpr(DerivedToBaseExpr *E,
                                             SGFContext C) {
  ManagedValue original = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  SILValue converted = SGF.B.createUpcast(E,
                                   original.getValue(),
                                   SGF.getLoweredType(E->getType()));
  return RValue(SGF, ManagedValue(converted, original.getCleanup()), E);
}

RValue RValueEmitter::visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                                   SGFContext C) {
  SILValue metaBase = visit(E->getSubExpr()).getUnmanagedSingleValue(SGF,
                                                              E->getSubExpr());
  return RValue(SGF,
                ManagedValue(SGF.B.createUpcast(E, metaBase,
                                    SGF.getLoweredLoadableType(E->getType())),
                             ManagedValue::Unmanaged), E);
}

RValue RValueEmitter::visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E,
                                                 SGFContext C) {
  ManagedValue archetype = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  // Replace the cleanup with a new one on the superclass value so we always use
  // concrete retain/release operations.
  SILValue base = SGF.B.createArchetypeRefToSuper(E,
                                    archetype.forward(SGF),
                                    SGF.getLoweredLoadableType(E->getType()));
  return RValue(SGF, SGF.emitManagedRValueWithCleanup(base), E);
}

RValue RValueEmitter::visitRequalifyExpr(RequalifyExpr *E, SGFContext C) {
  assert(E->getType()->is<LValueType>() && "non-lvalue requalify");
  // Ignore lvalue qualifiers.
  return visit(E->getSubExpr());
}

RValue RValueEmitter::visitFunctionConversionExpr(FunctionConversionExpr *e,
                                                  SGFContext C)
{
  ManagedValue original = visit(e->getSubExpr()).getAsSingleValue(SGF,
                                                              e->getSubExpr());
  
  // Retain the thinness of the original function type.
  Type destTy = e->getType();
  if (original.getType().castTo<FunctionType>()->isThin())
    destTy = getThinFunctionType(destTy);
  
  SILValue converted = SGF.B.createConvertFunction(e, original.getValue(),
                                                   SGF.getLoweredType(destTy));
  return RValue(SGF, ManagedValue(converted, original.getCleanup()), e);
}

namespace {
  /// An Initialization representing the concrete value buffer inside an
  /// existential container.
  class ExistentialValueInitialization : public SingleInitializationBase {
    SILValue valueAddr;
  public:
    ExistentialValueInitialization(SILValue valueAddr)
      : valueAddr(valueAddr)
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

static RValue emitClassBoundErasure(SILGenFunction &gen, ErasureExpr *E) {
  ManagedValue sub = gen.emitRValue(E->getSubExpr()).getAsSingleValue(gen,
                                                              E->getSubExpr());
  SILType resultTy = gen.getLoweredLoadableType(E->getType());
  
  SILValue v;
  
  if (E->getSubExpr()->getType()->isExistentialType())
    // If the source value is already of protocol type, we can use
    // upcast_existential_ref to steal the already-initialized witness tables
    // and concrete value.
    v = gen.B.createUpcastExistentialRef(E, sub.getValue(), resultTy);
  else
    // Otherwise, create a new existential container value around the class
    // instance.
    v = gen.B.createInitExistentialRef(E, resultTy, sub.getValue(),
                                       E->getConformances());

  return RValue(gen, ManagedValue(v, sub.getCleanup()), E);
}

static RValue emitAddressOnlyErasure(SILGenFunction &gen, ErasureExpr *E,
                                     SGFContext C) {
  // FIXME: Need to stage cleanups here. If code fails between
  // InitExistential and initializing the value, clean up using
  // DeinitExistential.
  
  // Allocate the existential.
  SILValue existential = gen.getBufferForExprResult(E,
                                              gen.getLoweredType(E->getType()),
                                              C);
  
  if (E->getSubExpr()->getType()->isExistentialType()) {
    // If the source value is already of a protocol type, we can use
    // upcast_existential to steal its already-initialized witness tables and
    // concrete value.
    ManagedValue subExistential
      = gen.emitRValue(E->getSubExpr()).getAsSingleValue(gen, E->getSubExpr());

    IsTake_t isTake = IsTake_t(subExistential.hasCleanup());

    gen.B.createUpcastExistential(E, subExistential.getValue(), existential,
                                  isTake);
  } else {
    // Otherwise, we need to initialize a new existential container from
    // scratch.
    
    // Allocate the concrete value inside the container.
    SILValue valueAddr = gen.B.createInitExistential(E, existential,
                                gen.getLoweredType(E->getSubExpr()->getType()),
                                E->getConformances());
    // Initialize the concrete value in-place.
    InitializationPtr init(new ExistentialValueInitialization(valueAddr));
    gen.emitExprInto(E->getSubExpr(), init.get());
  }
  
  return RValue(gen, gen.emitManagedRValueWithCleanup(existential), E);
}

RValue RValueEmitter::visitErasureExpr(ErasureExpr *E, SGFContext C) {
  if (E->getType()->isClassExistentialType())
    return emitClassBoundErasure(SGF, E);
  return emitAddressOnlyErasure(SGF, E, C);
}

namespace {
  class CleanupUsedExistentialContainer : public Cleanup {
    SILValue existential;
  public:
    CleanupUsedExistentialContainer(SILValue existential)
      : existential(existential) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.createDeinitExistential(l, existential);
    }
  };
}

/// \brief Emit the cast instruction appropriate to the kind of checked cast.
///
/// \param loc          The AST location associated with the operation.
/// \param originalMV   The value to cast.
/// \param origTy       The original AST-level type.
/// \param castTy       The destination type.
/// \param kind         The semantics of the cast.
/// \param mode         Whether to emit an unconditional or conditional cast.
/// \param useCastValue If true, the cleanup on the original value will be
///                     disabled, and the callee will be expected to take
///                     ownership of the returned value. If false, the original
///                     value's cleanup is left intact, and an unowned reference
///                     or address is returned.
SILValue SILGenFunction::emitCheckedCast(SILLocation loc,
                                         ManagedValue originalMV,
                                         Type origTy,
                                         Type castTy,
                                         CheckedCastKind kind,
                                         CheckedCastMode mode,
                                         bool useCastValue) {
  SILValue original = useCastValue
    ? originalMV.forward(*this)
    : originalMV.getValue();
  
  switch (kind) {
  case CheckedCastKind::Unresolved:
  case CheckedCastKind::InvalidCoercible:
    llvm_unreachable("invalid checked cast?!");
      
  case CheckedCastKind::Downcast:
    return B.createDowncast(loc, original,
                            getLoweredLoadableType(castTy), mode);
  case CheckedCastKind::SuperToArchetype:
    return B.createSuperToArchetypeRef(loc, original,
                                       getLoweredLoadableType(castTy), mode);
  case CheckedCastKind::ArchetypeToArchetype:
  case CheckedCastKind::ArchetypeToConcrete:
    if (origTy->castTo<ArchetypeType>()->requiresClass()) {
      return B.createDowncastArchetypeRef(loc, original,
                                getLoweredLoadableType(castTy), mode);
    } else {
      SILType loweredTy = getLoweredType(castTy);
      SILValue cast = B.createDowncastArchetypeAddr(loc, original,
                                             loweredTy.getAddressType(), mode);
      if (useCastValue && loweredTy.isLoadable(F.getModule()))
        cast = B.createLoad(loc, cast);
      return cast;
    }
  
  case CheckedCastKind::ExistentialToArchetype:
  case CheckedCastKind::ExistentialToConcrete:
    if (origTy->isClassExistentialType()) {
      return B.createDowncastExistentialRef(loc, original,
                                getLoweredLoadableType(castTy), mode);
    } else {
      // Project the concrete value address out of the container.
      SILType loweredTy = getLoweredType(castTy);
      SILValue cast = B.createProjectDowncastExistentialAddr(loc, original,
                                             loweredTy.getAddressType(), mode);
      if (useCastValue) {
        if (loweredTy.isLoadable(F.getModule()))
          cast = B.createLoad(loc, cast);
      
        // We'll pass on ownership of the contained value, but we still need to
        // deallocate the existential buffer when we're done.
        Cleanups.pushCleanup<CleanupUsedExistentialContainer>(original);
      }

      return cast;
    }
  }
}

RValue RValueEmitter::visitUnconditionalCheckedCastExpr(
                                               UnconditionalCheckedCastExpr *E,
                                               SGFContext C) {
  // Disable the original cleanup because the cast-to type is more specific and
  // should have a more efficient cleanup.
  ManagedValue original = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  SILValue cast = SGF.emitCheckedCast(E, original,
                                  E->getSubExpr()->getType(),
                                  E->getCastTypeLoc().getType(),
                                  E->getCastKind(),
                                  CheckedCastMode::Unconditional,
                                  /*useCastValue*/ true);
  return RValue(SGF, SGF.emitManagedRValueWithCleanup(cast), E);
}

RValue RValueEmitter::visitIsaExpr(IsaExpr *E, SGFContext C) {
  // Cast the value using a conditional cast.
  ManagedValue original = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  SILValue cast = SGF.emitCheckedCast(E, original,
                                  E->getSubExpr()->getType(),
                                  E->getCastTypeLoc().getType(),
                                  E->getCastKind(),
                                  CheckedCastMode::Conditional,
                                  /*useCastValue*/ false);
  // Check the result.
  SILValue is = SGF.B.createIsNonnull(E, cast);

  // Call the _getBool library intrinsic.
  return RValue(SGF, SGF.emitApplyOfLibraryIntrinsic(E,
                                  SGF.SGM.M.getASTContext().getGetBoolDecl(),
                                  ManagedValue(is, ManagedValue::Unmanaged),
                                  E->getType()->getCanonicalType(),
                                  C), E);
}

RValue RValueEmitter::visitParenExpr(ParenExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

static ManagedValue emitVarargs(SILGenFunction &gen,
                                SILLocation loc,
                                Type baseTy,
                                ArrayRef<ManagedValue> elements,
                                Expr *VarargsInjectionFn) {
  SILValue numEltsVal = gen.B.createIntegerLiteral(loc,
                      SILType::getBuiltinIntegerType(64, gen.F.getASTContext()),
                      elements.size());
  AllocArrayInst *allocArray = gen.B.createAllocArray(loc,
                                                  gen.getLoweredType(baseTy),
                                                  numEltsVal);
  // The first result is the owning ObjectPointer for the array.
  ManagedValue objectPtr
    = gen.emitManagedRValueWithCleanup(SILValue(allocArray, 0));
  // The second result is a RawPointer to the base address of the array.
  SILValue basePtr(allocArray, 1);

  for (size_t i = 0, size = elements.size(); i < size; ++i) {
    SILValue eltPtr = basePtr;
    if (i != 0) {
      SILValue index = gen.B.createIntegerLiteral(loc,
                  SILType::getBuiltinIntegerType(64, gen.F.getASTContext()), i);
      eltPtr = gen.B.createIndexAddr(loc, basePtr, index);
    }
    ManagedValue v = elements[i];
    v.forwardInto(gen, loc, eltPtr);
  }

  return gen.emitArrayInjectionCall(objectPtr, basePtr,
                                    numEltsVal, VarargsInjectionFn, loc);
}

RValue RValueEmitter::visitTupleExpr(TupleExpr *E, SGFContext C) {
  auto type = cast<TupleType>(E->getType()->getCanonicalType());

  // If we have an Initialization, emit the tuple elements into its elements.
  if (Initialization *I = C.getEmitInto()) {
    SmallVector<InitializationPtr, 4> subInitializationBuf;
    auto subInitializations =
      I->getSubInitializationsForTuple(SGF, type, subInitializationBuf,
                                       RegularLocation(E));
    assert(subInitializations.size() == E->getElements().size() &&
           "initialization for tuple has wrong number of elements");
    for (unsigned i = 0, size = subInitializations.size(); i < size; ++i) {
      SGF.emitExprInto(E->getElements()[i], subInitializations[i].get());
    }
    return RValue();
  }
  
  RValue result(type);
  for (Expr *elt : E->getElements()) {
    result.addElement(visit(elt));
  }
  return result;
}

RValue RValueEmitter::visitSpecializeExpr(SpecializeExpr *E,
                                          SGFContext C) {
  SILValue unspecialized
    = visit(E->getSubExpr()).getUnmanagedSingleValue(SGF, E->getSubExpr());
  SILType specializedType
    = SGF.getLoweredLoadableType(getThinFunctionType(E->getType()));
  SILValue spec = SGF.B.createSpecialize(E, unspecialized,
                                         E->getSubstitutions(),
                                         specializedType);
  return RValue(SGF, ManagedValue(spec, ManagedValue::Unmanaged), E);
}

RValue RValueEmitter::visitAddressOfExpr(AddressOfExpr *E,
                                          SGFContext C) {
  return SGF.emitLValueAsRValue(E);
}

ManagedValue SILGenFunction::emitMethodRef(SILLocation loc,
                                           SILValue selfValue,
                                           SILDeclRef methodConstant,
                                           ArrayRef<Substitution> innerSubs) {
  // FIXME: Emit dynamic dispatch instruction (class_method, super_method, etc.)
  // if needed.
  
  SILValue methodValue = B.createFunctionRef(loc,
                                             SGM.getFunction(methodConstant));
  SILType methodType = SGM.getConstantType(methodConstant.atUncurryLevel(0));
  
  /// If the 'self' type is a bound generic, specialize the method ref with
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
                                      F.getASTContext());
  }
  auto Info = FunctionType::ExtInfo()
                .withCallingConv(methodType.getAbstractCC())
                .withIsThin(true);
  Type outerMethodTy = FunctionType::get(selfValue.getType().getSwiftType(),
                                         innerMethodTy,
                                         Info,
                                         F.getASTContext());

  if (BoundGenericType *bgt = selfValue.getType().getAs<BoundGenericType>())
    outerSubs = bgt->getSubstitutions(F.getDeclContext()->getParentModule(),
                                      nullptr);
  
  if (!innerSubs.empty() || !outerSubs.empty()) {
    // Specialize the generic method.
    ArrayRef<Substitution> allSubs;
    if (outerSubs.empty())
      allSubs = innerSubs;
    else if (innerSubs.empty())
      allSubs = outerSubs;
    else {
      Substitution *allSubsBuf
        = F.getASTContext().Allocate<Substitution>(outerSubs.size()
                                                  + innerSubs.size());
      std::memcpy(allSubsBuf,
                  outerSubs.data(), outerSubs.size() * sizeof(Substitution));
      std::memcpy(allSubsBuf + outerSubs.size(),
                  innerSubs.data(), innerSubs.size() * sizeof(Substitution));
      allSubs = {allSubsBuf, outerSubs.size()+innerSubs.size()};
    }
    
    SILType specType = getLoweredLoadableType(outerMethodTy,
                                              methodConstant.uncurryLevel);
    
    methodValue = B.createSpecialize(loc, methodValue, allSubs, specType);
  }

  return ManagedValue(methodValue, ManagedValue::Unmanaged);
}

RValue RValueEmitter::visitMemberRefExpr(MemberRefExpr *E,
                                         SGFContext C) {
  if (E->getBase()->getType()->is<MetaTypeType>()) {
    // Emit the metatype for the associated type.
    assert(E->getType()->is<MetaTypeType>() &&
           "generic_member_ref of metatype should give metatype");
    visit(E->getBase());
    return RValue(SGF,
                  ManagedValue(SGF.B.createMetatype(E,
                                 SGF.getLoweredLoadableType(E->getType())),
                               ManagedValue::Unmanaged),
                  E);
    
  }

  return SGF.emitLValueAsRValue(E);
}

RValue RValueEmitter::visitDynamicMemberRefExpr(DynamicMemberRefExpr *E,
                                                SGFContext C) {
  return SGF.emitDynamicMemberRefExpr(E, C);
}

RValue RValueEmitter::visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E,
                                                  SGFContext C) {
  SILValue archetype = visit(E->getBase()).getUnmanagedSingleValue(SGF,
                                                                  E->getBase());
  assert((archetype.getType().isAddress() ||
          archetype.getType().is<MetaTypeType>()) &&
         "archetype must be an address or metatype");
  // FIXME: curried archetype
  // FIXME: archetype properties
  (void)archetype;
  llvm_unreachable("unapplied archetype method not implemented");
}

RValue RValueEmitter::visitExistentialMemberRefExpr(
                                                 ExistentialMemberRefExpr *E,
                                                 SGFContext C) {
  SILValue existential = visit(E->getBase()).getUnmanagedSingleValue(SGF,
                                                                  E->getBase());
  //SILValue projection = B.createProjectExistential(E, existential);
  //SILValue method = emitProtocolMethod(E, existential);
  // FIXME: curried existential
  // FIXME: existential properties
  (void)existential;
  llvm_unreachable("unapplied protocol method not implemented");
}

RValue RValueEmitter::visitDotSyntaxBaseIgnoredExpr(
                                                  DotSyntaxBaseIgnoredExpr *E,
                                                  SGFContext C) {
  visit(E->getLHS());
  return visit(E->getRHS());
}

RValue RValueEmitter::visitModuleExpr(ModuleExpr *E, SGFContext C) {
  SILValue module =
    SGF.B.createModule(E, SGF.getLoweredLoadableType(E->getType()));
  return RValue(SGF, ManagedValue(module, ManagedValue::Unmanaged), E);
}

RValue RValueEmitter::visitSubscriptExpr(SubscriptExpr *E,
                                         SGFContext C) {
  return SGF.emitLValueAsRValue(E);
}

RValue RValueEmitter::visitTupleElementExpr(TupleElementExpr *E,
                                            SGFContext C) {
  if (E->getType()->is<LValueType>()) {
    return SGF.emitLValueAsRValue(E);
  } else {
    return visit(E->getBase()).extractElement(E->getFieldNumber());
  }
}

RValue RValueEmitter::visitTupleShuffleExpr(TupleShuffleExpr *E,
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
  auto shuffleIndexIterator = E->getElementMapping().begin();
  auto shuffleIndexEnd = E->getElementMapping().end();
  unsigned callerDefaultArgIndex = 0;
  for (auto &field : outerFields) {
    assert(shuffleIndexIterator != shuffleIndexEnd &&
           "ran out of shuffle indexes before running out of fields?!");
    int shuffleIndex = *shuffleIndexIterator++;
    
    // If the shuffle index is DefaultInitialize, we're supposed to use the
    // default value.
    if (shuffleIndex == TupleShuffleExpr::DefaultInitialize) {
      unsigned destIndex
        = shuffleIndexIterator - E->getElementMapping().begin() - 1;
      SILDeclRef generator 
        = SILDeclRef::getDefaultArgGenerator(E->getDefaultArgsOwner(),
                                              destIndex);
      auto fnRef = SGF.emitFunctionRef(E, generator);
      auto generatorTy = SGF.SGM.getConstantType(generator);
      auto resultTy = generatorTy.getFunctionResultType();
      auto apply = SGF.emitApply(E, fnRef, { }, resultTy,
                             OwnershipConventions::getDefault(SGF,
                                                              generatorTy),
                                 generator.isTransparent());
      result.addElement(SGF, apply, E);
      continue;
    }

    // If the shuffle index is CallerDefaultInitialize, we're supposed to
    // use the caller-provided default value. This is used only in special
    // cases, e.g., __FILE__, __LINE__, and __COLUMN__.
    if (shuffleIndex == TupleShuffleExpr::CallerDefaultInitialize) {
      auto arg = E->getCallerDefaultArgs()[callerDefaultArgIndex++];
      result.addElement(visit(arg));
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
                     std::move(elements[sourceField]).getAsSingleValue(SGF, E));
    }
    
    ManagedValue varargs = emitVarargs(SGF, E, field.getVarargBaseTy(),
                                       variadicValues,
                                       E->getVarargsInjectionFunction());
    result.addElement(RValue(SGF, varargs, E));
    break;
  }
  
  return result;
}

static void emitScalarToTupleExprInto(SILGenFunction &gen,
                                      ScalarToTupleExpr *E,
                                      Initialization *I) {
  auto tupleType = cast<TupleType>(E->getType()->getCanonicalType());
  auto outerFields = tupleType->getFields();
  unsigned scalarField = E->getScalarField();
  bool isScalarFieldVariadic = outerFields[scalarField].isVararg();

  // Decompose the initialization.
  SmallVector<InitializationPtr, 4> subInitializationBuf;
  auto subInitializations = I->getSubInitializationsForTuple(gen, tupleType,
                                                          subInitializationBuf,
                                                          RegularLocation(E));
  assert(subInitializations.size() == outerFields.size() &&
         "initialization size does not match tuple size?!");
  
  // If the scalar field isn't variadic, emit it into the destination field of
  // the tuple.
  Initialization *scalarInit = subInitializations[E->getScalarField()].get();
  if (!isScalarFieldVariadic) {
    gen.emitExprInto(E->getSubExpr(), scalarInit);
  } else {
    // Otherwise, create the vararg and store it to the vararg field.
    ManagedValue scalar = gen.emitRValue(E->getSubExpr()).getAsSingleValue(gen,
                                                              E->getSubExpr());
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
      continue;
    }

    auto &element = E->getElements()[i];
    // If this element comes from a default argument generator, emit a call to
    // that generator in-place.
    assert(outerFields[i].hasInit() &&
           "no default initializer in non-scalar field of scalar-to-tuple?!");
    if (auto defaultArgOwner = element.dyn_cast<ValueDecl *>()) {
      SILDeclRef generator
      = SILDeclRef::getDefaultArgGenerator(defaultArgOwner, i);
      auto fnRef = gen.emitFunctionRef(E, generator);
      auto generatorTy = gen.SGM.getConstantType(generator);
      auto resultTy = generatorTy.getFunctionResultType();
      auto apply = gen.emitApply(E, fnRef, { }, resultTy,
                                 OwnershipConventions::getDefault(gen,
                                                                  generatorTy),
                                 generator.isTransparent());
      apply.forwardInto(gen, E,
                        subInitializations[i].get()->getAddressOrNull());
      subInitializations[i]->finishInitialization(gen);
      continue;
    }

    // We have an caller-side default argument. Emit it in-place.
    Expr *defArg = element.get<Expr *>();
    gen.emitExprInto(defArg, subInitializations[i].get());
  }
}

RValue RValueEmitter::visitScalarToTupleExpr(ScalarToTupleExpr *E,
                                             SGFContext C) {
  // If we're emitting into an Initialization, we can decompose the
  // initialization.
  if (Initialization *I = C.getEmitInto()) {
    emitScalarToTupleExprInto(SGF, E, I);
    return RValue();
  }

  // Emit the scalar member.
  RValue scalar = visit(E->getSubExpr());

  // Prepare a tuple rvalue to house the result.
  RValue result(E->getType()->getCanonicalType());
  
  // Create a tuple from the scalar along with any default values or varargs.
  auto outerFields = E->getType()->castTo<TupleType>()->getFields();
  for (unsigned i = 0, e = outerFields.size(); i != e; ++i) {
    // Handle the variadic argument. If we didn't emit the scalar field yet,
    // it goes into the variadic array; otherwise, the variadic array is empty.
    if (outerFields[i].isVararg()) {
      assert(i == e - 1 && "vararg isn't last?!");
      ManagedValue varargs;
      if (!scalar.isUsed())
        varargs = emitVarargs(SGF, E, outerFields[i].getVarargBaseTy(),
                              std::move(scalar).getAsSingleValue(SGF, E),
                              E->getVarargsInjectionFunction());
      else
        varargs = emitVarargs(SGF, E, outerFields[i].getVarargBaseTy(),
                              {}, E->getVarargsInjectionFunction());
      result.addElement(RValue(SGF, varargs, E));
      break;
    }

    auto &element = E->getElements()[i];

    // A null element indicates that this is the position of the scalar. Add
    // the scalar here.
    if (element.isNull()) {
      result.addElement(std::move(scalar));
      continue;
    }

    // If this element comes from a default argument generator, emit a call to
    // that generator.
    assert(outerFields[i].hasInit() &&
           "no default initializer in non-scalar field of scalar-to-tuple?!");
    if (auto defaultArgOwner = element.dyn_cast<ValueDecl *>()) {
      SILDeclRef generator
        = SILDeclRef::getDefaultArgGenerator(defaultArgOwner, i);
      auto fnRef = SGF.emitFunctionRef(E, generator);
      auto generatorTy = SGF.SGM.getConstantType(generator);
      auto resultTy = generatorTy.getFunctionResultType();
      auto apply = SGF.emitApply(E, fnRef, { }, resultTy,
                           OwnershipConventions::getDefault(SGF, generatorTy),
                                 generator.isTransparent());
      result.addElement(SGF, apply, E);
      continue;
    }

    // We have an caller-side default argument. Emit it.
    Expr *defArg = element.get<Expr *>();
    result.addElement(visit(defArg));
  }

  return result;
}

RValue RValueEmitter::visitNewArrayExpr(NewArrayExpr *E, SGFContext C) {
  SILValue NumElements = visit(E->getBounds()[0].Value)
    .getAsSingleValue(SGF, E->getBounds()[0].Value)
    .getValue();

  // Allocate the array.
  AllocArrayInst *AllocArray = SGF.B.createAllocArray(E,
                                            SGF.getLoweredType(E->getElementType()),
                                            NumElements);

  ManagedValue ObjectPtr
    = SGF.emitManagedRValueWithCleanup(SILValue(AllocArray, 0));
  SILValue BasePtr(AllocArray, 1);

  // FIXME: We need to initialize the elements of the array that are now
  // allocated.

  // Finally, build and return a Slice instance using the object
  // header/base/count.
  return RValue(SGF,
                SGF.emitArrayInjectionCall(ObjectPtr, BasePtr, NumElements,
                                           E->getInjectionFunction(), E),
                E);
}

SILValue SILGenFunction::emitMetatypeOfValue(SILLocation loc, SILValue base) {
  // For class, archetype, and protocol types, look up the dynamic metatype.
  SILType metaTy = getLoweredLoadableType(
    MetaTypeType::get(base.getType().getSwiftRValueType(), F.getASTContext()));
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

RValue RValueEmitter::visitMetatypeExpr(MetatypeExpr *E, SGFContext C) {
  // Evaluate the base if present.
  SILValue metatype;
  
  if (E->getBase()) {
    SILValue base = visit(E->getBase()).getAsSingleValue(SGF,
                                                      E->getBase()).getValue();
    metatype = SGF.emitMetatypeOfValue(E, base);
  } else {
    metatype = SGF.B.createMetatype(E, SGF.getLoweredLoadableType(E->getType()));
  }
  
  return RValue(SGF, ManagedValue(metatype, ManagedValue::Unmanaged), E);
}

ManagedValue SILGenFunction::emitClosureForCapturingExpr(SILLocation loc,
                                             SILDeclRef constant,
                                             ArrayRef<Substitution> forwardSubs,
                                             AnyFunctionRef TheClosure) {
  // FIXME: Stash the capture args somewhere and curry them on demand rather
  // than here.
  assert(((constant.uncurryLevel == 1 &&
           TheClosure.getCaptureInfo().hasLocalCaptures()) ||
          (constant.uncurryLevel == 0 &&
           !TheClosure.getCaptureInfo().hasLocalCaptures())) &&
         "curried local functions not yet supported");
  
  SILValue functionRef = emitGlobalFunctionRef(loc, constant);
  
  // Forward substitutions from the outer scope.
  
  // FIXME: ImplicitClosureExprs appear to always have null parent decl
  // contexts, so getFunctionTypeWithCaptures is unable to find contextual
  // generic parameters for them. The getAs null check here should be
  // unnecessary.
  auto pft = SGM.getConstantType(constant).getAs<PolymorphicFunctionType>();
  
  if (pft && !forwardSubs.empty()) {
    auto Info = FunctionType::ExtInfo()
                  .withCallingConv(pft->getAbstractCC())
                  .withIsThin(true);

    FunctionType *specialized = FunctionType::get(pft->getInput(),
                                                  pft->getResult(),
                                                  Info,
                                                  F.getASTContext());
    functionRef = B.createSpecialize(loc, functionRef, forwardSubs,
                                     getLoweredLoadableType(specialized));
  }

  if (!TheClosure.getCaptureInfo().hasLocalCaptures())
    return ManagedValue(functionRef, ManagedValue::Unmanaged);

  SmallVector<ValueDecl*, 4> captures;
  TheClosure.getCaptureInfo().getLocalCaptures(captures);
  SmallVector<SILValue, 4> capturedArgs;
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
        B.createStrongRetain(loc, vl.box);
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
        ManagedValue v = emitFunctionRef(loc, SILDeclRef(capture,
                                                 SILDeclRef::Kind::Setter));
        capturedArgs.push_back(v.forward(*this));
        SWIFT_FALLTHROUGH;
      }
      case CaptureKind::Getter: {
        // Pass the getter closure reference on.
        ManagedValue v = emitFunctionRef(loc, SILDeclRef(capture,
                                                 SILDeclRef::Kind::Getter));
        capturedArgs.push_back(v.forward(*this));
        break;
      }
    }
  }
  
  SILType closureTy = getLoweredLoadableType(TheClosure.getType());
  return emitManagedRValueWithCleanup(
                  B.createPartialApply(loc, functionRef, capturedArgs,
                                       closureTy));
}

RValue RValueEmitter::visitPipeClosureExpr(PipeClosureExpr *e, SGFContext C) {
  // Generate the closure function.
  SGF.SGM.emitClosure(e);

  // Generate the closure value (if any) for the closure expr's function
  // reference.
  return RValue(SGF, SGF.emitClosureForCapturingExpr(e, SILDeclRef(e),
                                             SGF.getForwardingSubstitutions(),
                                             e),
                e);
}

RValue RValueEmitter::visitClosureExpr(ClosureExpr *e, SGFContext C) {
  // Generate the closure body.
  SGF.SGM.emitClosure(e);
  
  // Generate the closure value (if any) for the closure expr's function
  // reference.
  return RValue(SGF, SGF.emitClosureForCapturingExpr(e, SILDeclRef(e),
                                          SGF.getForwardingSubstitutions(), e),
                e);
}

void SILGenFunction::emitFunction(FuncDecl *fd) {
  Type resultTy = fd->getResultType(F.getASTContext());
  emitProlog(fd, fd->getBodyParamPatterns(), resultTy);
  prepareEpilog(resultTy, CleanupLocation(fd));
  visit(fd->getBody());
  emitEpilog(fd);
}

void SILGenFunction::emitClosure(PipeClosureExpr *ce) {
  emitProlog(ce, ce->getParams(), ce->getResultType());
  prepareEpilog(ce->getResultType(), CleanupLocation(ce));
  visit(ce->getBody());
  emitEpilog(ce);
}

void SILGenFunction::emitClosure(ClosureExpr *ce) {
  Type resultTy = ce->getType()->castTo<FunctionType>()->getResult();
  emitProlog(ce, ce->getParamPatterns(), resultTy);
  prepareEpilog(resultTy, CleanupLocation(ce));

  // Closure expressions implicitly return the result of their body expression.
  emitReturnExpr(ImplicitReturnLocation(ce), ce->getBody());
  emitEpilog(ce);
}

std::pair<Optional<SILValue>, Optional<SILLocation>>
SILGenFunction::emitEpilogBB(SILLocation TopLevel) {

  assert(ReturnDest.getBlock() && "no epilog bb prepared?!");
  SILBasicBlock *epilogBB = ReturnDest.getBlock();
  SILLocation ImplicitReturnFromTopLevel =
    ImplicitReturnLocation::getImplicitReturnLoc(TopLevel);
  SILValue returnValue;
  Optional<SILLocation> returnLoc = Nothing;

  // If the current BB isn't terminated, and we require a return, then we
  // are not allowed to fall off the end of the function and can't reach here.
  if (NeedsReturn && B.hasValidInsertionPoint()) {
    B.createUnreachable(ImplicitReturnFromTopLevel);
  }
  
  if (epilogBB->pred_empty()) {
    bool hadArg = !epilogBB->bbarg_empty();
    
    // If the epilog was not branched to at all, kill the BB and
    // just emit the epilog into the current BB.
    epilogBB->eraseFromParent();

    // If the current bb is terminated then the epilog is just unreachable.
    if (!B.hasValidInsertionPoint())
      return std::pair<Optional<SILValue>, Optional<SILLocation>>(Nothing,
                                                                  Nothing);
    // We emit the epilog at the current insertion point.
    assert(!hadArg && "NeedsReturn is false but epilog had argument?!");
    (void)hadArg;
    returnLoc = ImplicitReturnFromTopLevel;

  } else if (std::next(epilogBB->pred_begin()) == epilogBB->pred_end()
             && !B.hasValidInsertionPoint()) {
    // If the epilog has a single predecessor and there's no current insertion
    // point to fall through from, then we can weld the epilog to that
    // predecessor BB.

    bool needsArg = false;
    if (!epilogBB->bbarg_empty()) {
      assert(epilogBB->bbarg_size() == 1 && "epilog should take 0 or 1 args");
      needsArg = true;
    }
    
    epilogBB->eraseFromParent();

    // Steal the branch argument as the return value if present.
    SILBasicBlock *pred = *epilogBB->pred_begin();
    BranchInst *predBranch = cast<BranchInst>(pred->getTerminator());
    assert(predBranch->getArgs().size() == (needsArg ? 1 : 0)
           && "epilog predecessor arguments does not match block params");
    if (needsArg)
      returnValue = predBranch->getArgs()[0];

    // If we are optimizing, we should use the return location from the single,
    // previously processed, return statement if any.
    if (predBranch->getLoc().is<ReturnLocation>()) {
      returnLoc = predBranch->getLoc();
    } else {
      returnLoc = ImplicitReturnFromTopLevel;
    }

    // Kill the branch to the now-dead epilog BB.
    pred->getInstList().erase(predBranch);

    // Emit the epilog into its former predecessor.
    B.setInsertionPoint(pred);
  } else {
    // Emit the epilog into the epilog bb. Its argument is the return value.
    if (!epilogBB->bbarg_empty()) {
      assert(epilogBB->bbarg_size() == 1 && "epilog should take 0 or 1 args");
      returnValue = epilogBB->bbarg_begin()[0];
    }

    // If we are falling through from the current block, the return is implicit.
    B.emitBlock(epilogBB, ImplicitReturnFromTopLevel);
  }
  
  // Emit top-level cleanups into the epilog block.
  assert(getCleanupsDepth() == ReturnDest.getDepth() &&
         "emitting epilog in wrong scope");
  // FIXME: Use proper cleanups location.
  Cleanups.emitCleanupsForReturn(CleanupLocation::getCleanupLocation(TopLevel));
  
  return std::pair<Optional<SILValue>, Optional<SILLocation>>(returnValue,
                                                              returnLoc);
}

void SILGenFunction::emitEpilog(SILLocation TopLevel, bool AutoGen) {
  Optional<SILValue> maybeReturnValue;
  Optional<SILLocation> optReturnLoc;

  // Construct the appropriate SIL Location for the return instruction.
  if (AutoGen)
    TopLevel.markAutoGenerated();

  llvm::tie(maybeReturnValue, optReturnLoc)  = emitEpilogBB(TopLevel);

  // If the epilog is unreachable, we're done.
  if (!maybeReturnValue)
    return;
  
  // Otherwise, return the return value, if any.
  SILValue returnValue = *maybeReturnValue;

  // If the return location is known to be that of an already processed return,
  // use it. (This will get triggered when epilog logic is simplified.)
  SILLocation returnLoc = optReturnLoc ? *optReturnLoc : TopLevel;

  // Return () if no return value was given.
  if (!returnValue)
    returnValue = emitEmptyTuple(TopLevel);

  B.createReturn(returnLoc, returnValue)->setDebugScope(F.getDebugScope());
}

void SILGenFunction::emitDestructor(ClassDecl *cd, DestructorDecl *dd) {
  // For implicit destructor, use class location.
  SILLocation Loc =
    dd ? (SILLocation)RegularLocation(dd) :
         (SILLocation)RegularLocation(cd);
  if (!dd)
    Loc.markAutoGenerated();

  SILValue selfValue = emitDestructorProlog(cd, dd);

  // Create a basic block to jump to for the implicit destruction behavior
  // of releasing the elements and calling the superclass destructor.
  // We won't actually emit the block until we finish with the destructor body.
  prepareEpilog(Type(), CleanupLocation::getCleanupLocation(Loc));
  
  // Emit the destructor body, if any.
  if (dd)
    visit(dd->getBody());

  if (!emitEpilogBB(Loc).first)
    return;
  
  // Release our members.
  // FIXME: generic params
  // FIXME: Can a destructor always consider its fields fragile like this?
  for (Decl *member : cd->getMembers()) {
    if (VarDecl *vd = dyn_cast<VarDecl>(member)) {
      if (vd->isProperty())
        continue;
      const TypeLowering &ti = getTypeLowering(vd->getType());
      if (!ti.isTrivial()) {
        SILValue addr = B.createRefElementAddr(Loc, selfValue, vd,
                                          ti.getLoweredType().getAddressType());
        B.createDestroyAddr(Loc, addr);
      }
    }
  }
  
  // If we have a superclass, invoke its destructor.
  SILType objectPtrTy = SILType::getObjectPointerType(F.getASTContext());
  if (Type superclassTy = cd->getSuperclass()) {
    ClassDecl *superclass = superclassTy->getClassOrBoundGenericClass();
    
    // FIXME: We can't sensibly call up to ObjC dealloc methods right now
    // because they aren't really destroying destructors.
    if (superclass->hasClangNode() && superclass->isObjC()) {
      selfValue = B.createRefToObjectPointer(Loc, selfValue, objectPtrTy);
      B.createReturn(Loc, selfValue);
      return;
    }
    
    SILDeclRef dtorConstant =
      SILDeclRef(superclass, SILDeclRef::Kind::Destroyer);
    SILType baseSILTy = getLoweredLoadableType(superclassTy);
    SILValue baseSelf = B.createUpcast(Loc, selfValue, baseSILTy);
    ManagedValue dtorValue = emitMethodRef(Loc, baseSelf, dtorConstant,
                                           /*innerSubstitutions*/ {});
    selfValue = B.createApply(Loc, dtorValue.forward(*this),
                              objectPtrTy,
                              baseSelf);
  } else {
    selfValue = B.createRefToObjectPointer(Loc, selfValue, objectPtrTy);
  }
  B.createReturn(Loc, selfValue);
}

static void emitConstructorMetatypeArg(SILGenFunction &gen,
                                       ValueDecl *ctor) {
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
    for (auto fieldType : tupleTy->getElementTypes())
      tuple.addElement(emitImplicitValueConstructorArg(gen, loc, fieldType));

    return tuple;
  } else {
    SILValue arg = new (gen.F.getModule()) SILArgument(gen.getLoweredType(ty),
                                                       gen.F.begin());
    return RValue(gen, ManagedValue(arg, ManagedValue::Unmanaged), loc);
  }
}

namespace {
  class ImplicitValueInitialization : public SingleInitializationBase {
    SILValue slot;
  public:
    ImplicitValueInitialization(SILValue slot) : slot(slot)
    {}
    
    SILValue getAddressOrNull() override {
      return slot;
    };
  };
}

static void emitImplicitValueDefaultConstructor(SILGenFunction &gen,
                                                ConstructorDecl *ctor,
                                                SILLocation Loc) {
  emitConstructorMetatypeArg(gen, ctor);

  SILType selfTy = gen.getLoweredType(ctor->getImplicitSelfDecl()->getType());
  
  // FIXME: We should actually elementwise default-construct the elements.
  if (selfTy.isAddressOnly(gen.SGM.M)) {
    SILValue resultSlot
      = new (gen.F.getModule()) SILArgument(selfTy, gen.F.begin());
    gen.B.createInitializeVar(Loc, resultSlot, /*canDefaultConstruct*/ false);
    gen.B.createReturn(Loc, gen.emitEmptyTuple(ctor));
  } else {
    auto alloc = gen.B.createAllocStack(Loc, selfTy);
    gen.B.createInitializeVar(Loc, alloc->getAddressResult(),
                              /*canDefaultConstruct*/ false);
    SILValue result = gen.B.createLoad(Loc, alloc->getAddressResult());
    gen.B.createDeallocStack(Loc, alloc->getContainerResult());
    gen.B.createReturn(Loc, result);
  }
}

static void emitImplicitValueConstructor(SILGenFunction &gen,
                                         ConstructorDecl *ctor) {
  RegularLocation Loc(ctor);
  Loc.markAutoGenerated();
  auto *TP = cast<TuplePattern>(ctor->getArguments());
  SILType selfTy = gen.getLoweredType(ctor->getImplicitSelfDecl()->getType());

  if (TP->getFields().empty()) {
    // Emit a default constructor.
    return emitImplicitValueDefaultConstructor(gen, ctor, Loc);
  }

  // Emit the indirect return argument, if any.
  SILValue resultSlot;
  if (selfTy.isAddressOnly(gen.SGM.M))
    resultSlot = new (gen.F.getModule()) SILArgument(selfTy, gen.F.begin());
  
  // Emit the elementwise arguments.
  SmallVector<RValue, 4> elements;
  for (size_t i = 0, size = TP->getFields().size(); i < size; ++i) {
    auto *P = cast<TypedPattern>(TP->getFields()[i].getPattern());
    
    elements.push_back(emitImplicitValueConstructorArg(gen, Loc, P->getType()));
  }

  emitConstructorMetatypeArg(gen, ctor);

  auto *decl = selfTy.getStructOrBoundGenericStruct();
  assert(decl && "not a struct?!");
  
  // If we have an indirect return slot, initialize it in-place.
  if (resultSlot) {
    
    auto elti = elements.begin(), eltEnd = elements.end();
    for (VarDecl *field : decl->getPhysicalFields()) {
      assert(elti != eltEnd && "number of args does not match number of fields");
      (void)eltEnd;
      auto fieldTy = selfTy.getSwiftRValueType()
        ->getTypeOfMember(decl->getModuleContext(), field, nullptr);
      auto &fieldTL = gen.getTypeLowering(fieldTy);
      SILValue slot = gen.B.createStructElementAddr(Loc, resultSlot, field,
                                    fieldTL.getLoweredType().getAddressType());
      InitializationPtr init(new ImplicitValueInitialization(slot));
      std::move(*elti).forwardInto(gen, init.get(), Loc);
      ++elti;
    }
    gen.B.createReturn(Loc, gen.emitEmptyTuple(Loc));
    return;
  }
  
  // Otherwise, build a struct value directly from the elements.
  SmallVector<SILValue, 4> eltValues;
  
  auto elti = elements.begin(), eltEnd = elements.end();
  for (VarDecl *field : decl->getPhysicalFields()) {
    assert(elti != eltEnd && "number of args does not match number of fields");
    (void)eltEnd;
    auto fieldTy = selfTy.getSwiftRValueType()
      ->getTypeOfMember(decl->getModuleContext(), field, nullptr);
    auto fieldSILTy = gen.getLoweredLoadableType(fieldTy);
    
    SILValue v
      = std::move(*elti).forwardAsSingleStorageValue(gen, fieldSILTy, Loc);
    
    eltValues.push_back(v);
    
    ++elti;
  }
  
  SILValue selfValue = gen.B.createStruct(Loc, selfTy, eltValues);
  gen.B.createReturn(Loc, selfValue);
  return;
}

void SILGenFunction::emitValueConstructor(ConstructorDecl *ctor) {
  // If there's no body, this is the implicit elementwise constructor.
  if (!ctor->getBody())
    return emitImplicitValueConstructor(*this, ctor);
  
  // Emit the prolog.
  emitProlog(ctor->getArguments(), ctor->getImplicitSelfDecl()->getType());
  emitConstructorMetatypeArg(*this, ctor);
  
  // Get the 'self' decl and type.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  auto &lowering = getTypeLowering(selfDecl->getType());
  SILType selfTy = lowering.getLoweredType();
  (void)selfTy;
  assert(!selfTy.hasReferenceSemantics() && "can't emit a ref type ctor here");
  assert(!ctor->getAllocSelfExpr() && "alloc_this expr for value type?!");

  // Emit a local variable for 'self'.
  // FIXME: The (potentially partially initialized) variable would need to be
  // cleaned up on an error unwind.
  emitLocalVariable(selfDecl);

  SILValue selfLV = VarLocs[selfDecl].address;
  
  // Emit a default initialization of the this value.
  // Note that this initialization *cannot* be lowered to a
  // default constructor--we're already in a constructor!
  B.createInitializeVar(ctor, selfLV, /*CanDefaultConstruct*/ false);
  
  // Create a basic block to jump to for the implicit 'self' return.
  // We won't emit this until after we've emitted the body.
  // The epilog takes a void return because the return of 'self' is implicit.
  prepareEpilog(Type(), CleanupLocation(ctor));

  // Emit the constructor body.
  visit(ctor->getBody());
  
  // Return 'self' in the epilog.
  if (!emitEpilogBB(ctor).first)
    return;

  // If 'self' is address-only, copy 'self' into the indirect return slot.
  if (lowering.isAddressOnly()) {
    assert(IndirectReturnAddress &&
           "no indirect return for address-only ctor?!");
    SILValue selfBox = VarLocs[selfDecl].box;
    assert(selfBox &&
           "address-only non-heap this should have been allocated in-place");
    // We have to do a non-take copy because someone else may be using the box.
    B.createCopyAddr(ctor, selfLV, IndirectReturnAddress,
                     IsNotTake, IsInitialization);
    B.createStrongRelease(ctor, selfBox);
    B.createReturn(ctor, emitEmptyTuple(ctor));
    return;
  }

  // Otherwise, load and return the final 'self' value.
  SILValue selfValue = B.createLoad(ctor, selfLV);
  SILValue selfBox = VarLocs[selfDecl].box;
  assert(selfBox);

  // We have to do a retain because someone else may be using the box.
  lowering.emitRetain(B, ctor, selfValue);

  // Release the box.
  B.createStrongRelease(ctor, selfBox);

  B.createReturn(ctor, selfValue);
}

static void emitAddressOnlyUnionConstructor(SILGenFunction &gen,
                                            SILType unionTy,
                                            UnionElementDecl *element) {
  RegularLocation Loc(element);
  Loc.markAutoGenerated();

  // Emit the indirect return slot.
  SILValue resultSlot
    = new (gen.F.getModule()) SILArgument(unionTy, gen.F.begin());
  
  // Emit the exploded constructor argument.
  ManagedValue argValue;
  if (element->hasArgumentType()) {
    RValue arg = emitImplicitValueConstructorArg(gen, Loc,
                                                 element->getArgumentType());
    argValue = std::move(arg).getAsSingleValue(gen, Loc);
  }
  emitConstructorMetatypeArg(gen, element);
  
  // Store the data, if any.
  if (element->hasArgumentType()) {
    SILValue resultData = gen.B.createUnionDataAddr(element, resultSlot,
      element, gen.getLoweredType(element->getArgumentType()).getAddressType());
    argValue.forwardInto(gen, element, resultData);
  }
  
  // Apply the tag.
  gen.B.createInjectUnionAddr(Loc, resultSlot, element);
  gen.B.createReturn(Loc, gen.emitEmptyTuple(element));
}

static void emitLoadableUnionConstructor(SILGenFunction &gen,
                                         SILType unionTy,
                                         UnionElementDecl *element) {
  RegularLocation Loc(element);
  Loc.markAutoGenerated();

  // Emit the exploded constructor argument.
  SILValue argValue;
  if (element->hasArgumentType()) {
    RValue arg = emitImplicitValueConstructorArg(gen, Loc,
                                                 element->getArgumentType());
    argValue = std::move(arg).forwardAsSingleValue(gen, Loc);
  }
  
  emitConstructorMetatypeArg(gen, element);
  
  // Create and return the union value.
  SILValue result = gen.B.createUnion(Loc, argValue, element, unionTy);
  gen.B.createReturn(Loc, result);
}

void SILGenFunction::emitUnionConstructor(UnionElementDecl *element) {
  Type unionTy = element->getType()->getAs<AnyFunctionType>()->getResult();
  if (element->hasArgumentType())
    unionTy = unionTy->getAs<AnyFunctionType>()->getResult();
  auto &unionTI = getTypeLowering(unionTy);
  
  if (unionTI.isAddressOnly()) {
    return emitAddressOnlyUnionConstructor(*this, unionTI.getLoweredType(),
                                           element);
  }
  return emitLoadableUnionConstructor(*this, unionTI.getLoweredType(),
                                      element);
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
        for (auto fieldType : tupleTy->getElementTypes())
          makeArgument(fieldType);
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
      // FIXME: work around a bug in visiting the "self" argument of methods
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
    
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) \
    void visit##Id##Pattern(Id##Pattern *) { \
      llvm_unreachable("pattern not valid in argument binding"); \
    }
#include "swift/AST/PatternNodes.def"

  };
} // end anonymous namespace

ArrayRef<Substitution>
SILGenFunction::buildForwardingSubstitutions(ArrayRef<ArchetypeType *> params) {
  if (params.empty())
    return {};
  
  ASTContext &C = F.getASTContext();
  
  size_t paramCount = params.size();
  Substitution *resultBuf = C.Allocate<Substitution>(paramCount);
  MutableArrayRef<Substitution> results{resultBuf, paramCount};
  
  for (size_t i = 0; i < paramCount; ++i) {
    // FIXME: better way to do this?
    ArchetypeType *archetype = params[i];
    // "Check conformance" on each declared protocol to build a
    // conformance map.
    SmallVector<ProtocolConformance*, 2> conformances;
    
    for (ProtocolDecl *conformsTo : archetype->getConformsTo()) {
      (void)conformsTo;
      conformances.push_back(nullptr);
    }
    
    // Build an identity mapping with the derived conformances.
    auto replacement = SubstitutedType::get(archetype, archetype, C);
    results[i] = {archetype, replacement,
                  C.AllocateCopy(conformances)};
  }
  
  return results;
}

void SILGenFunction::emitClassConstructorAllocator(ConstructorDecl *ctor) {
  // Emit the prolog. Since we're just going to forward our args directly
  // to the initializer, don't allocate local variables for them.
  RegularLocation Loc(ctor);
  Loc.markAutoGenerated();

  SmallVector<SILValue, 8> args;
  
  // Forward the constructor arguments.
  ArgumentForwardVisitor(*this, args).visit(ctor->getArguments());

  emitConstructorMetatypeArg(*this, ctor);

  // Allocate the "self" value.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  SILType selfTy = getLoweredType(selfDecl->getType());
  assert(selfTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");
  SILValue selfValue;
  if (ctor->getAllocSelfExpr()) {
    FullExpr allocSelfScope(Cleanups,CleanupLocation(ctor->getAllocSelfExpr()));
    // If the constructor has an alloc-this expr, emit it to get "self".
    auto e = ctor->getAllocSelfExpr();
    selfValue = emitRValue(e).forwardAsSingleValue(*this, e);
    assert(selfValue.getType() == selfTy &&
           "alloc-this expr type did not match this type?!");
  } else {
    // Otherwise, just emit an alloc_ref instruction for the default allocation
    // path.
    // FIXME: should have a cleanup in case of exception
    selfValue = B.createAllocRef(Loc, selfTy);
  }
  args.push_back(selfValue);

  // Call the initializer.
  SILDeclRef initConstant = SILDeclRef(ctor, SILDeclRef::Kind::Initializer);

  ArrayRef<ArchetypeType *> archetypes;
  if (auto genericParams = ctor->getGenericParams())
    archetypes = genericParams->getAllArchetypes();
  auto forwardingSubs = buildForwardingSubstitutions(archetypes);
  ManagedValue initVal = emitMethodRef(Loc, selfValue, initConstant,
                                       forwardingSubs);
  
  SILValue initedSelfValue
    = B.createApply(Loc, initVal.forward(*this), selfTy, args,
                    initConstant.isTransparent());
  
  // Return the initialized 'self'.
  B.createReturn(Loc, initedSelfValue);
}

static void emitClassImplicitConstructorInitializer(SILGenFunction &gen,
                                                    ConstructorDecl *ctor) {
  // The default constructor is currently a no-op. Just return back 'self'.
  // FIXME: We should default-construct fields maybe?

  assert(cast<TuplePattern>(ctor->getArguments())->getNumFields() == 0
         && "implicit class ctor has arguments?!");

  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  SILType selfTy = gen.getLoweredLoadableType(selfDecl->getType());
  SILValue selfArg = new (gen.SGM.M) SILArgument(selfTy, gen.F.begin());
  assert(selfTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");
  
  RegularLocation Loc(ctor);
  Loc.markAutoGenerated();
  gen.B.createReturn(Loc, selfArg);
}

void SILGenFunction::emitClassConstructorInitializer(ConstructorDecl *ctor) {
  // If there's no body, this is the implicit constructor.
  if (!ctor->getBody())
    return emitClassImplicitConstructorInitializer(*this, ctor);
  
  // Emit the prolog for the non-this arguments.
  emitProlog(ctor->getArguments(), TupleType::getEmpty(F.getASTContext()));
  
  // Emit the 'self' argument and make an lvalue for it.
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  SILType selfTy = getLoweredLoadableType(selfDecl->getType());
  SILValue selfArg = new (SGM.M) SILArgument(selfTy, F.begin());
  assert(selfTy.hasReferenceSemantics() &&
         "can't emit a value type ctor here");

  // FIXME: The (potentially partially initialized) value here would need to be
  // cleaned up on a constructor failure unwinding.
  emitLocalVariable(selfDecl);
  SILValue selfLV = VarLocs[selfDecl].address;
  B.createStore(ctor, selfArg, selfLV);
  
  // Create a basic block to jump to for the implicit 'self' return.
  // We won't emit the block until after we've emitted the body.
  prepareEpilog(Type(), CleanupLocation(ctor));
  
  // Emit the constructor body.
  visit(ctor->getBody());
  
  // Return 'self' in the epilog.
  if (!emitEpilogBB(ctor).first)
    return;

  // Load and return the final 'self'.
  SILValue selfValue = B.createLoad(ctor, selfLV);
  SILValue selfBox = VarLocs[selfDecl].box;
  assert(selfBox);

  // We have to do a retain because someone else may be using the box.
  B.emitRetainValue(ctor, selfValue);
  B.createStrongRelease(ctor, selfBox);

  B.createReturn(ctor, selfValue);
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
    SILType ty = gen.getLoweredType(capture->getType()).getAddressType();
    // Forward the captured owning ObjectPointer.
    addSILArgument(SILType::getObjectPointerType(c));
    // Forward the captured value address.
    addSILArgument(ty);
    break;
  }
  case CaptureKind::Byref: {
    // Forward the captured address.
    SILType ty = gen.getLoweredType(capture->getType()).getAddressType();
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
    Type setTy = gen.SGM.Types.getPropertyType(SILDeclRef::Kind::Setter,
                                               capture->getType());
    addSILArgument(gen.getLoweredType(setTy));
    SWIFT_FALLTHROUGH;
  }
  case CaptureKind::Getter: {
    // Forward the captured getter.
    Type getTy = gen.SGM.Types.getPropertyType(SILDeclRef::Kind::Getter,
                                               capture->getType());
    addSILArgument(gen.getLoweredType(getTy));
    break;
  }
  }
}

// Reduce a PolymorphicFunctionType to a concrete FunctionType as if in a
// context with forwarding substitutions. For example, given <T> (T) -> Int,
// returns the concrete function type (T) -> Int (in a context with T bound).
static SILType getFunctionTypeWithForwardedSubstitutions(SILGenFunction &gen,
                                                         SILType ty) {
  if (auto pft = ty.getAs<PolymorphicFunctionType>()) {
    auto ft = FunctionType::get(pft->getInput(),
                                pft->getResult(),
                                pft->getExtInfo(),
                                pft->getASTContext());
    return gen.getLoweredLoadableType(ft);
  }
  
  return ty;
}

static SILValue getNextUncurryLevelRef(SILGenFunction &gen,
                                       SILLocation loc,
                                       SILDeclRef next,
                                       ArrayRef<SILValue> curriedArgs) {
  // For the fully-uncurried reference to a class method, emit the dynamic
  // dispatch.
  // FIXME: We should always emit dynamic dispatch at uncurry level 1,
  // to support overriding a curried method with a non-curried,
  // function-returning method.
  if (!next.isCurried
      && next.kind == SILDeclRef::Kind::Func
      && next.hasDecl() && isa<ClassDecl>(next.getDecl()->getDeclContext())) {
    SILValue thisArg;
    thisArg = curriedArgs.back();
    
    return gen.B.createClassMethod(loc, thisArg, next,
                                   gen.SGM.getConstantType(next));
  }
  
  return gen.emitGlobalFunctionRef(loc, next);
}

void SILGenFunction::emitCurryThunk(FuncDecl *fd,
                                    SILDeclRef from, SILDeclRef to) {
  SmallVector<SILValue, 8> curriedArgs;
  
  unsigned paramCount = from.uncurryLevel + 1;
  
  // Forward implicit closure context arguments.
  bool hasCaptures = fd->getCaptureInfo().hasLocalCaptures();
  if (hasCaptures)
    --paramCount;

  // Forward the curried formal arguments.
  auto forwardedPatterns = fd->getBodyParamPatterns().slice(0, paramCount);
  ArgumentForwardVisitor forwarder(*this, curriedArgs);
  for (auto *paramPattern : reversed(forwardedPatterns))
    forwarder.visit(paramPattern);

  // Forward captures.
  if (hasCaptures) {
    SmallVector<ValueDecl*, 4> LocalCaptures;
    fd->getCaptureInfo().getLocalCaptures(LocalCaptures);
    for (auto capture : LocalCaptures)
      forwardCaptureArgs(*this, curriedArgs, capture);
  }

  SILValue toFn = getNextUncurryLevelRef(*this, fd, to, curriedArgs);
  SILType resultTy
    = SGM.getConstantType(from).getFunctionTypeInfo(SGM.M)->getResultType();

  // Forward archetypes and specialize if the function is generic.
  if (auto pft = toFn.getType().getAs<PolymorphicFunctionType>()) {
    auto subs = buildForwardingSubstitutions(pft->getAllArchetypes());
    toFn = B.createSpecialize(fd, toFn, subs,
              getFunctionTypeWithForwardedSubstitutions(*this, toFn.getType()));
  }
  
  // Partially apply the next uncurry level and return the result closure.
  auto toClosure = B.createPartialApply(fd, toFn, curriedArgs, resultTy);
  B.createReturn(fd, toClosure);
}

void SILGenFunction::emitGeneratorFunction(SILDeclRef function, Expr *value) {
  RegularLocation Loc(value);
  Loc.markAutoGenerated();

  emitProlog({ }, value->getType());
  prepareEpilog(value->getType(), CleanupLocation::getCleanupLocation(Loc));
  emitReturnExpr(Loc, value);
  emitEpilog(Loc);
}

RValue RValueEmitter::
visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                   SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

RValue RValueEmitter::
visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E, SGFContext C) {
  ASTContext &Ctx = SGF.SGM.M.getASTContext();
  SILType Ty = SGF.getLoweredLoadableType(E->getType());
  SourceLoc Loc;
  
  // If "overrideLocationForMagicIdentifiers" is set, then we use it as the
  // location point for these magic identifiers.
  if (SGF.overrideLocationForMagicIdentifiers.isValid())
    Loc = SGF.overrideLocationForMagicIdentifiers;
  else
    Loc = E->getStartLoc();
  
  switch (E->getKind()) {
  case MagicIdentifierLiteralExpr::File: {
    unsigned BufferID = Ctx.SourceMgr.findBufferContainingLoc(Loc);

    StringRef Value =
      Ctx.SourceMgr->getMemoryBuffer(BufferID)->getBufferIdentifier();
    
    return RValue(SGF, ManagedValue(SGF.B.createStringLiteral(E, Ty, Value),
                                    ManagedValue::Unmanaged),
                  E);
  }
  case MagicIdentifierLiteralExpr::Line: {
    unsigned Value = Ctx.SourceMgr.getLineAndColumn(Loc).first;
    return RValue(SGF, ManagedValue(SGF.B.createIntegerLiteral(E, Ty, Value),
                                    ManagedValue::Unmanaged),
                  E);
  }
  case MagicIdentifierLiteralExpr::Column: {
    unsigned Value = Ctx.SourceMgr.getLineAndColumn(Loc).second;
    return RValue(SGF, ManagedValue(SGF.B.createIntegerLiteral(E, Ty, Value),
                                    ManagedValue::Unmanaged),
                  E);
  }
  }
}

RValue RValueEmitter::visitCollectionExpr(CollectionExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr());
}

RValue RValueEmitter::visitRebindSelfInConstructorExpr(
                                RebindSelfInConstructorExpr *E, SGFContext C) {
  // FIXME: Use a different instruction from 'downcast'. IRGen can make
  // "rebind this" into a no-op if the called constructor is a Swift one.
  ManagedValue newSelf = visit(E->getSubExpr()).getAsSingleValue(SGF,
                                                              E->getSubExpr());
  if (!newSelf.getType().getSwiftRValueType()
        ->isEqual(E->getSelf()->getType())) {
    assert(newSelf.getType().isObject() &&
           newSelf.getType().hasReferenceSemantics() &&
           "delegating ctor type mismatch for non-reference type?!");
    CleanupsDepth newSelfCleanup = newSelf.getCleanup();
    SILValue newSelfValue = SGF.B.createDowncast(E, newSelf.getValue(),
                              SGF.getLoweredLoadableType(E->getSelf()->getType()),
                              CheckedCastMode::Unconditional);
    newSelf = ManagedValue(newSelfValue, newSelfCleanup);
  }
  
  SILValue selfAddr = SGF.emitReferenceToDecl(E, E->getSelf()).getUnmanagedValue();
  newSelf.assignInto(SGF, E, selfAddr);
  
  return SGF.emitEmptyTupleRValue(E);
}

RValue RValueEmitter::visitArchetypeSubscriptExpr(
                                     ArchetypeSubscriptExpr *E, SGFContext C) {
  llvm_unreachable("not implemented");
}

RValue RValueEmitter::visitExistentialSubscriptExpr(
                                   ExistentialSubscriptExpr *E, SGFContext C) {
  llvm_unreachable("not implemented");
}

RValue RValueEmitter::visitBridgeToBlockExpr(BridgeToBlockExpr *E,
                                              SGFContext C) {
  SILValue func = visit(E->getSubExpr()).forwardAsSingleValue(SGF,
                                                              E->getSubExpr());
  // Thicken thin function value if necessary.
  // FIXME: This should go away when Swift typechecking learns how to handle
  // thin functions.
  func = SGF.emitGeneralizedValue(E, func);
  
  // Emit the bridge_to_block instruction.
  SILValue block = SGF.B.createBridgeToBlock(E, func,
                                    SGF.getLoweredLoadableType(E->getType()));
  return RValue(SGF, SGF.emitManagedRValueWithCleanup(block), E);
}

RValue RValueEmitter::visitIfExpr(IfExpr *E, SGFContext C) {
  // FIXME: We could avoid imploding and reexploding tuples here.
  // FIXME: "emit into" optimization
  
  Condition cond = SGF.emitCondition(E->getCondExpr(),
                                     /*hasFalse*/ true,
                                     /*invertCondition*/ false,
                                     SGF.getLoweredType(E->getType()));
  
  cond.enterTrue(SGF.B);
  SILValue trueValue;
  {
    auto TE = E->getThenExpr();
    FullExpr trueScope(SGF.Cleanups, CleanupLocation(TE));
    trueValue = visit(TE).forwardAsSingleValue(SGF, TE);
  }
  cond.exitTrue(SGF.B, trueValue);
  
  cond.enterFalse(SGF.B);
  SILValue falseValue;
  {
    auto EE = E->getElseExpr();
    FullExpr falseScope(SGF.Cleanups, CleanupLocation(EE));
    falseValue = visit(EE).forwardAsSingleValue(SGF, EE);
  }
  cond.exitFalse(SGF.B, falseValue);
  
  SILBasicBlock *cont = cond.complete(SGF.B);
  assert(cont && "no continuation block for if expr?!");
  
  SILValue result = cont->bbarg_begin()[0];
  
  return RValue(SGF, SGF.emitManagedRValueWithCleanup(result), E);
}

RValue RValueEmitter::visitZeroValueExpr(ZeroValueExpr *E, SGFContext C) {
  SILValue zero = SGF.B.createBuiltinZero(E,
                                    SGF.getLoweredLoadableType(E->getType()));
  return RValue(SGF, ManagedValue(zero, ManagedValue::Unmanaged), E);
}

RValue RValueEmitter::visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

SILValue SILGenFunction::emitGeneralizedValue(SILLocation loc, SILValue v) {
  return B.emitGeneralizedValue(loc, v);
}

static ManagedValue emitBridgeStringToNSString(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue str) {
  // func convertStringToNSString([byref] String) -> NSString
  SILValue stringToNSStringFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getStringToNSStringFn());
  
  // Materialize the string so we can pass a reference.
  // Assume StringToNSString won't consume or modify the string, so leave the
  // cleanup on the original value intact.
  SILValue strTemp = gen.emitTemporaryAllocation(loc,
                                                 str.getType());
  gen.B.createStore(loc, str.getValue(), strTemp);
  
  SILValue nsstr = gen.B.createApply(loc, stringToNSStringFn,
                           gen.getLoweredType(gen.SGM.Types.getNSStringType()),
                           strTemp);
  return gen.emitManagedRValueWithCleanup(nsstr);
}

static ManagedValue emitBridgeNSStringToString(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue nsstr) {
  // func convertNSStringToString(NSString, [byref] String) -> ()
  SILValue nsstringToStringFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getNSStringToStringFn());
  
  // Allocate and initialize a temporary to receive the result String.
  SILValue strTemp = gen.emitTemporaryAllocation(loc,
                             gen.getLoweredType(gen.SGM.Types.getStringType()));
  gen.B.createInitializeVar(loc, strTemp, true);
  
  SILValue args[2] = {nsstr.forward(gen), strTemp};
  gen.B.createApply(loc, nsstringToStringFn,
                    gen.SGM.Types.getEmptyTupleType(),
                    args);
  
  // Load the result string, taking ownership of the value. There's no cleanup
  // on the value in the temporary allocation.
  SILValue str = gen.B.createLoad(loc, strTemp);
  return gen.emitManagedRValueWithCleanup(str);
}

static ManagedValue emitBridgeBoolToObjCBool(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue swiftBool) {
  // func convertBoolToObjCBool(Bool) -> ObjCBool
  SILValue boolToObjCBoolFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getBoolToObjCBoolFn());
  
  SILType resultTy =gen.getLoweredLoadableType(gen.SGM.Types.getObjCBoolType());
  
  SILValue result = gen.B.createApply(loc, boolToObjCBoolFn,
                                      resultTy, swiftBool.forward(gen));
  return gen.emitManagedRValueWithCleanup(result);
}

static ManagedValue emitBridgeObjCBoolToBool(SILGenFunction &gen,
                                             SILLocation loc,
                                             ManagedValue objcBool) {
  // func convertObjCBoolToBool(ObjCBool) -> Bool
  SILValue objcBoolToBoolFn
    = gen.emitGlobalFunctionRef(loc, gen.SGM.getObjCBoolToBoolFn());
  
  SILType resultTy = gen.getLoweredLoadableType(gen.SGM.Types.getBoolType());
  
  SILValue result = gen.B.createApply(loc, objcBoolToBoolFn,
                                      resultTy, objcBool.forward(gen));
  return gen.emitManagedRValueWithCleanup(result);
}

ManagedValue SILGenFunction::emitNativeToBridgedValue(SILLocation loc,
                                                      ManagedValue v,
                                                      AbstractCC destCC,
                                                      CanType bridgedTy) {
  // First, generalize the value representation.
  SILValue generalized = emitGeneralizedValue(loc, v.getValue());
  v = ManagedValue(generalized, v.getCleanup());

  switch (destCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // No additional bridging needed for native functions.
    return v;
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    // If the input is a native type with a bridged mapping, convert it.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType) \
    if (v.getType().getSwiftType() == SGM.Types.get##NativeType##Type() \
        && bridgedTy == SGM.Types.get##BridgedType##Type()) {           \
      return emitBridge##NativeType##To##BridgedType(*this, loc, v);    \
    }
#include "swift/SIL/BridgedTypes.def"
    return v;
  }
}

ManagedValue SILGenFunction::emitBridgedToNativeValue(SILLocation loc,
                                                      ManagedValue v,
                                                      AbstractCC srcCC,
                                                      CanType nativeTy) {
  switch (srcCC) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // No additional bridging needed for native functions.
    return v;

  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    // If the output is a bridged type, convert it back to a native type.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType)     \
    if (nativeTy == SGM.Types.get##NativeType##Type() &&                    \
        v.getType().getSwiftType() == SGM.Types.get##BridgedType##Type()) { \
      return emitBridge##BridgedType##To##NativeType(*this, loc, v);        \
    }
#include "swift/SIL/BridgedTypes.def"
    return v;
  }
}

RValue SILGenFunction::emitEmptyTupleRValue(SILLocation loc) {
  return RValue(CanType(TupleType::getEmpty(F.getASTContext())));
}

/// Destructure (potentially) recursive assignments into tuple expressions
/// down to their scalar stores.
static void emitAssignExprRecursive(AssignExpr *S, RValue &&Src, Expr *Dest,
                                    SILGenFunction &Gen) {
  // If the destination is a tuple, recursively destructure.
  if (TupleExpr *TE = dyn_cast<TupleExpr>(Dest)) {
    SmallVector<RValue, 4> elements;
    std::move(Src).extractElements(elements);
    unsigned EltNo = 0;
    for (Expr *DestElem : TE->getElements()) {
      emitAssignExprRecursive(S,
                              std::move(elements[EltNo++]),
                              DestElem, Gen);
    }
    return;
  }
  
  // Otherwise, emit the scalar assignment.
  LValue DstLV = Gen.emitLValue(Dest);
  Gen.emitAssignToLValue(S, std::move(Src), DstLV);
}


RValue RValueEmitter::visitAssignExpr(AssignExpr *E, SGFContext C) {
  FullExpr scope(SGF.Cleanups, CleanupLocation(E));

  // Handle tuple destinations by destructuring them if present.
  emitAssignExprRecursive(E, visit(E->getSrc()), E->getDest(), SGF);
  
  return SGF.emitEmptyTupleRValue(E);
}

RValue RValueEmitter::visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C) {
  assert(SGF.OpaqueValues.count(E) && "Didn't bind OpaqueValueExpr");

  auto &entry = SGF.OpaqueValues[E];

  // If the opaque value is uniquely referenced, we can just return the
  // value with a cleanup. There is no need to retain it separately.
  if (E->isUniquelyReferenced()) {
    assert(!entry.second &&"Uniquely-referenced opaque value already consumed");
    entry.second = true;
    return RValue(SGF, SGF.emitManagedRValueWithCleanup(entry.first), E);
  }

  // Retain the value.
  entry.second = true;
  return RValue(SGF, SGF.emitManagedRetain(E, entry.first), E);
}

RValue SILGenFunction::emitRValue(Expr *E, SGFContext C) {
  return RValueEmitter(*this).visit(E, C);
}
