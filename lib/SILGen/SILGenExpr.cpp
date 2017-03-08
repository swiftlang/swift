//===--- SILGenExpr.cpp - Implements Lowering of ASTs -> SIL for Exprs ----===//
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

#include "SILGen.h"
#include "Condition.h"
#include "Scope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/type_traits.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SIL/DynamicCasts.h"
#include "ExitableFullExpr.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "ArgumentSource.h"
#include "SILGenDynamicCast.h"
#include "Varargs.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"

#include "swift/AST/DiagnosticsSIL.h"

using namespace swift;
using namespace Lowering;

ManagedValue SILGenFunction::emitManagedRetain(SILLocation loc,
                                               SILValue v) {
  auto &lowering = F.getTypeLowering(v->getType());
  return emitManagedRetain(loc, v, lowering);
}

ManagedValue SILGenFunction::emitManagedRetain(SILLocation loc,
                                               SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType() == v->getType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);
  if (v->getType().isObject() &&
      v.getOwnershipKind() == ValueOwnershipKind::Trivial)
    return ManagedValue::forUnmanaged(v);
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");

  v = lowering.emitCopyValue(B, loc, v);
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedLoadCopy(SILLocation loc, SILValue v) {
  auto &lowering = F.getTypeLowering(v->getType());
  return emitManagedLoadCopy(loc, v, lowering);
}

ManagedValue SILGenFunction::emitManagedLoadCopy(SILLocation loc, SILValue v,
                                                 const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v->getType());
  v = lowering.emitLoadOfCopy(B, loc, v, IsNotTake);
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);
  if (v.getOwnershipKind() == ValueOwnershipKind::Trivial)
    return ManagedValue::forUnmanaged(v);
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedLoadBorrow(SILLocation loc,
                                                   SILValue v) {
  auto &lowering = F.getTypeLowering(v->getType());
  return emitManagedLoadBorrow(loc, v, lowering);
}

ManagedValue
SILGenFunction::emitManagedLoadBorrow(SILLocation loc, SILValue v,
                                      const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v->getType());
  if (lowering.isTrivial()) {
    v = lowering.emitLoadOfCopy(B, loc, v, IsNotTake);
    return ManagedValue::forUnmanaged(v);
  }

  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");
  auto *lbi = B.createLoadBorrow(loc, v);
  return emitManagedBorrowedRValueWithCleanup(v, lbi, lowering);
}

ManagedValue SILGenFunction::emitManagedStoreBorrow(SILLocation loc, SILValue v,
                                                    SILValue addr) {
  auto &lowering = F.getTypeLowering(v->getType());
  return emitManagedStoreBorrow(loc, v, addr, lowering);
}

ManagedValue SILGenFunction::emitManagedStoreBorrow(
    SILLocation loc, SILValue v, SILValue addr, const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() == v->getType());
  if (lowering.isTrivial() ||
      v.getOwnershipKind() == ValueOwnershipKind::Trivial) {
    lowering.emitStore(B, loc, v, addr, StoreOwnershipQualifier::Trivial);
    return ManagedValue::forUnmanaged(v);
  }
  assert(!lowering.isAddressOnly() && "cannot retain an unloadable type");
  auto *sbi = B.createStoreBorrow(loc, v, addr);
  return emitManagedBorrowedRValueWithCleanup(sbi->getSrc(), sbi, lowering);
}

ManagedValue SILGenFunction::emitManagedBeginBorrow(SILLocation loc,
                                                    SILValue v) {
  auto &lowering = F.getTypeLowering(v->getType());
  return emitManagedBeginBorrow(loc, v, lowering);
}

ManagedValue
SILGenFunction::emitManagedBeginBorrow(SILLocation loc, SILValue v,
                                       const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         v->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);

  if (v.getOwnershipKind() == ValueOwnershipKind::Trivial)
    return ManagedValue::forUnmanaged(v);

  if (v.getOwnershipKind() == ValueOwnershipKind::Guaranteed)
    return ManagedValue::forUnmanaged(v);

  auto *bbi = B.createBeginBorrow(loc, v);
  return emitManagedBorrowedRValueWithCleanup(v, bbi, lowering);
}

namespace {

struct EndBorrowCleanup : Cleanup {
  SILValue originalValue;
  SILValue borrowedValue;

  EndBorrowCleanup(SILValue originalValue, SILValue borrowedValue)
      : originalValue(originalValue), borrowedValue(borrowedValue) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.createEndBorrow(l, borrowedValue, originalValue);
  }

  void dump(SILGenFunction &gen) const override {
#ifndef NDEBUG
    llvm::errs() << "EndBorrowCleanup "
                 << "State:" << getState() << "\n"
                 << "original:" << originalValue << "borrowed:" << borrowedValue
                 << "\n";
#endif
  }
};

struct FormalEvaluationEndBorrowCleanup : Cleanup {
  FormalEvaluationContext::stable_iterator Depth;

  FormalEvaluationEndBorrowCleanup() : Depth() {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    getEvaluation(gen).finish(gen);
  }

  void dump(SILGenFunction &gen) const override {
#ifndef NDEBUG
    llvm::errs() << "FormalEvaluationEndBorrowCleanup "
                 << "State:" << getState() << "\n"
                 << "original:" << getOriginalValue(gen) << "\n"
                 << "borrowed:" << getBorrowedValue(gen) << "\n";
#endif
  }

  SharedBorrowFormalAccess &getEvaluation(SILGenFunction &gen) const {
    auto &evaluation = *gen.FormalEvalContext.find(Depth);
    assert(evaluation.getKind() == FormalAccess::Shared);
    return static_cast<SharedBorrowFormalAccess &>(evaluation);
  }

  SILValue getOriginalValue(SILGenFunction &gen) const {
    return getEvaluation(gen).getOriginalValue();
  }

  SILValue getBorrowedValue(SILGenFunction &gen) const {
    return getEvaluation(gen).getBorrowedValue();
  }
};

} // end anonymous namespace

ManagedValue
SILGenFunction::emitFormalEvaluationManagedBeginBorrow(SILLocation loc,
                                                       SILValue v) {
  if (v.getOwnershipKind() == ValueOwnershipKind::Guaranteed)
    return ManagedValue::forUnmanaged(v);
  auto &lowering = F.getTypeLowering(v->getType());
  return emitFormalEvaluationManagedBeginBorrow(loc, v, lowering);
}

ManagedValue SILGenFunction::emitFormalEvaluationManagedBeginBorrow(
    SILLocation loc, SILValue v, const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         v->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);
  if (v.getOwnershipKind() == ValueOwnershipKind::Guaranteed)
    return ManagedValue::forUnmanaged(v);
  auto *bbi = B.createBeginBorrow(loc, v);
  return emitFormalEvaluationManagedBorrowedRValueWithCleanup(loc, v, bbi,
                                                              lowering);
}

ManagedValue
SILGenFunction::emitFormalEvaluationManagedBorrowedRValueWithCleanup(
    SILLocation loc, SILValue original, SILValue borrowed) {
  auto &lowering = F.getTypeLowering(original->getType());
  return emitFormalEvaluationManagedBorrowedRValueWithCleanup(
      loc, original, borrowed, lowering);
}

ManagedValue
SILGenFunction::emitFormalEvaluationManagedBorrowedRValueWithCleanup(
    SILLocation loc, SILValue original, SILValue borrowed,
    const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         original->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(borrowed);

  if (!borrowed->getType().isObject()) {
    return ManagedValue(borrowed, CleanupHandle::invalid());
  }

  assert(InWritebackScope && "Must be in formal evaluation scope");
  auto &cleanup = Cleanups.pushCleanup<FormalEvaluationEndBorrowCleanup>();
  CleanupHandle handle = Cleanups.getTopCleanup();
  FormalEvalContext.push<SharedBorrowFormalAccess>(loc, handle, original,
                                                   borrowed);
  cleanup.Depth = FormalEvalContext.stable_begin();
  return ManagedValue(borrowed, CleanupHandle::invalid());
}

ManagedValue
SILGenFunction::emitManagedBorrowedRValueWithCleanup(SILValue original,
                                                     SILValue borrowed) {
  assert(original->getType().getObjectType() ==
         borrowed->getType().getObjectType());
  auto &lowering = F.getTypeLowering(original->getType());
  return emitManagedBorrowedRValueWithCleanup(original, borrowed, lowering);
}

ManagedValue SILGenFunction::emitManagedBorrowedRValueWithCleanup(
    SILValue original, SILValue borrowed, const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         original->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(borrowed);

  if (original->getType().isObject() &&
      original.getOwnershipKind() == ValueOwnershipKind::Trivial)
    return ManagedValue::forUnmanaged(borrowed);

  if (borrowed->getType().isObject()) {
    Cleanups.pushCleanup<EndBorrowCleanup>(original, borrowed);
  }

  return ManagedValue(borrowed, CleanupHandle::invalid());
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v) {
  auto &lowering = F.getTypeLowering(v->getType());
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         v->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);
  if (v->getType().isObject() &&
      v.getOwnershipKind() == ValueOwnershipKind::Trivial) {
    return ManagedValue::forUnmanaged(v);
  }
  return ManagedValue(v, enterDestroyCleanup(v));
}

ManagedValue SILGenFunction::emitManagedBufferWithCleanup(SILValue v) {
  auto &lowering = F.getTypeLowering(v->getType());
  return emitManagedBufferWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedBufferWithCleanup(SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v->getType() ||
         !silConv.useLoweredAddresses());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(v);

  return ManagedValue(v, enterDestroyCleanup(v));
}

void SILGenFunction::emitExprInto(Expr *E, Initialization *I) {
  // Handle the special case of copying an lvalue.
  if (auto load = dyn_cast<LoadExpr>(E)) {
    FormalEvaluationScope writeback(*this);
    auto lv = emitLValue(load->getSubExpr(), AccessKind::Read);
    emitCopyLValueInto(E, std::move(lv), I);
    return;
  }

  RValue result = emitRValue(E, SGFContext(I));
  if (result)
    std::move(result).forwardInto(*this, E, I);
}

namespace {
  class RValueEmitter
      : public Lowering::ExprVisitor<RValueEmitter, RValue, SGFContext>
  {
    typedef Lowering::ExprVisitor<RValueEmitter,RValue,SGFContext> super;
  public:
    SILGenFunction &SGF;
    
    RValueEmitter(SILGenFunction &SGF) : SGF(SGF) {}

    using super::visit;
    RValue visit(Expr *E) {
      assert(!E->getType()->is<LValueType>() &&
             !E->getType()->is<InOutType>() &&
             "RValueEmitter shouldn't be called on lvalues");
      return visit(E, SGFContext());
    }

    // These always produce lvalues.
    RValue visitInOutExpr(InOutExpr *E, SGFContext C) {
      LValue lv = SGF.emitLValue(E->getSubExpr(), AccessKind::ReadWrite);
      return RValue(SGF, E, SGF.emitAddressOfLValue(E->getSubExpr(),
                                                    std::move(lv),
                                                    AccessKind::ReadWrite));
    }
    
    RValue visitApplyExpr(ApplyExpr *E, SGFContext C);
    
    RValue visitDiscardAssignmentExpr(DiscardAssignmentExpr *E, SGFContext C) {
      llvm_unreachable("cannot appear in rvalue");
    }
    RValue visitDeclRefExpr(DeclRefExpr *E, SGFContext C);
    RValue visitTypeExpr(TypeExpr *E, SGFContext C);
    RValue visitSuperRefExpr(SuperRefExpr *E, SGFContext C);
    RValue visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E,
                                            SGFContext C);

    RValue visitForceTryExpr(ForceTryExpr *E, SGFContext C);
    RValue visitOptionalTryExpr(OptionalTryExpr *E, SGFContext C);

    RValue visitNilLiteralExpr(NilLiteralExpr *E, SGFContext C);
    RValue visitIntegerLiteralExpr(IntegerLiteralExpr *E, SGFContext C);
    RValue visitFloatLiteralExpr(FloatLiteralExpr *E, SGFContext C);
    RValue visitBooleanLiteralExpr(BooleanLiteralExpr *E, SGFContext C);

    RValue emitStringLiteral(Expr *E, StringRef Str, SGFContext C,
                             StringLiteralExpr::Encoding encoding);
        
    RValue visitStringLiteralExpr(StringLiteralExpr *E, SGFContext C);
    RValue visitLoadExpr(LoadExpr *E, SGFContext C);
    RValue visitDerivedToBaseExpr(DerivedToBaseExpr *E, SGFContext C);
    RValue visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                       SGFContext C);
    RValue visitCollectionUpcastConversionExpr(
             CollectionUpcastConversionExpr *E,
             SGFContext C);
    RValue visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, SGFContext C);
    RValue visitUnresolvedTypeConversionExpr(UnresolvedTypeConversionExpr *E,
                                             SGFContext C);
    RValue visitFunctionConversionExpr(FunctionConversionExpr *E,
                                       SGFContext C);
    RValue visitCovariantFunctionConversionExpr(
             CovariantFunctionConversionExpr *E,
             SGFContext C);
    RValue visitCovariantReturnConversionExpr(
             CovariantReturnConversionExpr *E,
             SGFContext C);
    RValue visitErasureExpr(ErasureExpr *E, SGFContext C);
    RValue visitAnyHashableErasureExpr(AnyHashableErasureExpr *E, SGFContext C);
    RValue visitForcedCheckedCastExpr(ForcedCheckedCastExpr *E,
                                      SGFContext C);
    RValue visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                           SGFContext C);
    RValue visitIsExpr(IsExpr *E, SGFContext C);
    RValue visitCoerceExpr(CoerceExpr *E, SGFContext C);
    RValue visitTupleExpr(TupleExpr *E, SGFContext C);
    RValue visitMemberRefExpr(MemberRefExpr *E, SGFContext C);
    RValue visitDynamicMemberRefExpr(DynamicMemberRefExpr *E, SGFContext C);
    RValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E,
                                         SGFContext C);
    RValue visitTupleElementExpr(TupleElementExpr *E, SGFContext C);
    RValue visitSubscriptExpr(SubscriptExpr *E, SGFContext C);
    RValue visitDynamicSubscriptExpr(DynamicSubscriptExpr *E,
                                     SGFContext C);
    RValue visitTupleShuffleExpr(TupleShuffleExpr *E, SGFContext C);
    RValue visitDynamicTypeExpr(DynamicTypeExpr *E, SGFContext C);
    RValue visitCaptureListExpr(CaptureListExpr *E, SGFContext C);
    RValue visitAbstractClosureExpr(AbstractClosureExpr *E, SGFContext C);
    RValue visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                              SGFContext C);
    RValue visitObjectLiteralExpr(ObjectLiteralExpr *E, SGFContext C);
    RValue visitEditorPlaceholderExpr(EditorPlaceholderExpr *E, SGFContext C);
    RValue visitObjCSelectorExpr(ObjCSelectorExpr *E, SGFContext C);
    RValue visitObjCKeyPathExpr(ObjCKeyPathExpr *E, SGFContext C);
    RValue visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E,
                                           SGFContext C);
    RValue visitCollectionExpr(CollectionExpr *E, SGFContext C);
    RValue visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E,
                                            SGFContext C);
    RValue visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E, SGFContext C);
    RValue visitLValueToPointerExpr(LValueToPointerExpr *E, SGFContext C);
    RValue visitClassMetatypeToObjectExpr(ClassMetatypeToObjectExpr *E,
                                          SGFContext C);
    RValue visitExistentialMetatypeToObjectExpr(ExistentialMetatypeToObjectExpr *E,
                                                SGFContext C);
    RValue visitProtocolMetatypeToObjectExpr(ProtocolMetatypeToObjectExpr *E,
                                             SGFContext C);
    RValue visitIfExpr(IfExpr *E, SGFContext C);
    
    RValue visitAssignExpr(AssignExpr *E, SGFContext C);
    RValue visitEnumIsCaseExpr(EnumIsCaseExpr *E, SGFContext C);

    RValue visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C);
    RValue visitOptionalEvaluationExpr(OptionalEvaluationExpr *E,
                                       SGFContext C);
    RValue visitForceValueExpr(ForceValueExpr *E, SGFContext C);
    RValue emitForceValue(SILLocation loc, Expr *E,
                          unsigned numOptionalEvaluations,
                          SGFContext C);
    RValue visitOpenExistentialExpr(OpenExistentialExpr *E, SGFContext C);
    RValue visitMakeTemporarilyEscapableExpr(
                                 MakeTemporarilyEscapableExpr *E, SGFContext C);

    RValue visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C);

    RValue visitInOutToPointerExpr(InOutToPointerExpr *E, SGFContext C);
    RValue visitArrayToPointerExpr(ArrayToPointerExpr *E, SGFContext C);
    RValue visitStringToPointerExpr(StringToPointerExpr *E, SGFContext C);
    RValue visitPointerToPointerExpr(PointerToPointerExpr *E, SGFContext C);
    RValue visitForeignObjectConversionExpr(ForeignObjectConversionExpr *E,
                                            SGFContext C);
    RValue visitUnevaluatedInstanceExpr(UnevaluatedInstanceExpr *E,
                                        SGFContext C);
  };
} // end anonymous namespace

RValue RValueEmitter::visitApplyExpr(ApplyExpr *E, SGFContext C) {
  return SGF.emitApplyExpr(E, C);
}

SILValue SILGenFunction::emitEmptyTuple(SILLocation loc) {
  return B.createTuple(loc,
               getLoweredType(TupleType::getEmpty(SGM.M.getASTContext())), {});
}

/// Emit the specified declaration as an address if possible,
/// otherwise return null.
ManagedValue SILGenFunction::emitLValueForDecl(SILLocation loc, VarDecl *var,
                                               CanType formalRValueType,
                                               AccessKind accessKind,
                                               AccessSemantics semantics) {
  // For local decls, use the address we allocated or the value if we have it.
  auto It = VarLocs.find(var);
  if (It != VarLocs.end()) {
    // If this has an address, return it.  By-value let's have no address.
    SILValue ptr = It->second.value;
    if (ptr->getType().isAddress())
      return ManagedValue::forLValue(ptr);
    
    // Otherwise, it is an RValue let.
    return ManagedValue();
  }

  switch (var->getAccessStrategy(semantics, accessKind)) {
  case AccessStrategy::Storage:
    // The only kind of stored variable that should make it to here is
    // a global variable.  Just invoke its accessor function to get its
    // address.
    return emitGlobalVariableRef(loc, var);

  case AccessStrategy::Addressor: {
    LValue lvalue =
      emitLValueForAddressedNonMemberVarDecl(loc, var, formalRValueType,
                                             accessKind, semantics);
    return emitAddressOfLValue(loc, std::move(lvalue), accessKind);
  }

  case AccessStrategy::DirectToAccessor:
  case AccessStrategy::DispatchToAccessor:
    return ManagedValue();
    
  case AccessStrategy::BehaviorStorage:
    // TODO: Behaviors aren't supported on non-instance properties yet.
    llvm_unreachable("not implemented");
  }
  llvm_unreachable("bad access strategy");
}

namespace {

/// Thie is a simple cleanup class that is only meant to help with delegating
/// initializers. Specifically, if the delegating initializer fails to consume
/// the loaded self, we want to write back self into the slot to ensure that
/// ownership is preserved.
struct DelegateInitSelfWritebackCleanup : Cleanup {

  /// We store our own loc so that we can ensure that DI ignores our writeback.
  SILLocation loc;

  SILValue lvalueAddress;
  SILValue value;

  DelegateInitSelfWritebackCleanup(SILLocation loc, SILValue lvalueAddress,
                                   SILValue value)
      : loc(loc), lvalueAddress(lvalueAddress), value(value) {}

  void emit(SILGenFunction &gen, CleanupLocation) override {
    gen.emitSemanticStore(loc, value, lvalueAddress,
                          gen.F.getTypeLowering(lvalueAddress->getType()),
                          IsInitialization);
  }

  void dump(SILGenFunction &gen) const override {
#ifndef NDEBUG
    llvm::errs() << "SimpleWritebackCleanup "
                 << "State:" << getState() << "\n"
                 << "lvalueAddress:" << lvalueAddress << "value:" << value
                 << "\n";
#endif
  }
};

} // end anonymous namespace

CleanupHandle SILGenFunction::enterDelegateInitSelfWritebackCleanup(
    SILLocation loc, SILValue address, SILValue newValue) {
  Cleanups.pushCleanup<DelegateInitSelfWritebackCleanup>(loc, address,
                                                         newValue);
  return Cleanups.getTopCleanup();
}

RValue SILGenFunction::emitRValueForSelfInDelegationInit(SILLocation loc,
                                                         CanType refType,
                                                         SILValue addr,
                                                         SGFContext C) {
  assert(SelfInitDelegationState != SILGenFunction::NormalSelf &&
         "This should never be called unless we are in a delegation sequence");
  assert(F.getTypeLowering(addr->getType()).isLoadable() &&
         "Make sure that we are not dealing with semantic rvalues");

  // If we are currently in the WillSharedBorrowSelf state, then we know that
  // old self is not the self to our delegating initializer. Self in this case
  // to the delegating initializer is a metatype. Thus, we perform a
  // load_borrow. And move from WillSharedBorrowSelf -> DidSharedBorrowSelf.
  if (SelfInitDelegationState == SILGenFunction::WillSharedBorrowSelf) {
    SelfInitDelegationState = SILGenFunction::DidSharedBorrowSelf;
    ManagedValue result =
        B.createFormalAccessLoadBorrow(loc, ManagedValue::forUnmanaged(addr));
    return RValue(*this, loc, refType, result);
  }

  // If we are already in the did shared borrow self state, just return the
  // shared borrow value.
  if (SelfInitDelegationState == SILGenFunction::DidSharedBorrowSelf) {
    ManagedValue result =
        B.createFormalAccessLoadBorrow(loc, ManagedValue::forUnmanaged(addr));
    return RValue(*this, loc, refType, result);
  }

  // If we are in WillExclusiveBorrowSelf, then we need to perform an exclusive
  // borrow (i.e. a load take) and then move to DidExclusiveBorrowSelf.
  if (SelfInitDelegationState == SILGenFunction::WillExclusiveBorrowSelf) {
    const auto &typeLowering = F.getTypeLowering(addr->getType());
    SelfInitDelegationState = SILGenFunction::DidExclusiveBorrowSelf;
    SILValue self =
        emitLoad(loc, addr, typeLowering, C, IsTake, false).forward(*this);
    // Forward our initial value for init delegation self and create a new
    // cleanup that performs a writeback at the end of lexical scope if our
    // value is not consumed.
    InitDelegationSelf = ManagedValue(
        self, enterDelegateInitSelfWritebackCleanup(*InitDelegationLoc, addr, self));
    InitDelegationSelfBox = addr;
    return RValue(*this, loc, refType, InitDelegationSelf);
  }

  // If we hit this point, we must have DidExclusiveBorrowSelf. Thus borrow
  // self.
  assert(SelfInitDelegationState == SILGenFunction::DidExclusiveBorrowSelf);

  // If we do not have a super init delegation self, just perform a formal
  // access borrow and return. This occurs with delegating initializers.
  if (!SuperInitDelegationSelf) {
    return RValue(*this, loc, refType,
                  InitDelegationSelf.formalAccessBorrow(*this, loc));
  }

  // Otherwise, we had an upcast of some sort due to a chaining
  // initializer. This means that we need to perform a borrow from
  // SuperInitDelegationSelf and then downcast that borrow.
  ManagedValue borrowedUpcast =
      SuperInitDelegationSelf.formalAccessBorrow(*this, loc);
  SILValue castedBorrowedType = B.createUncheckedRefCast(
      loc, borrowedUpcast.getValue(), InitDelegationSelf.getType());
  return RValue(*this, loc, refType,
                ManagedValue::forUnmanaged(castedBorrowedType));
}

RValue SILGenFunction::
emitRValueForDecl(SILLocation loc, ConcreteDeclRef declRef, Type ncRefType,
                  AccessSemantics semantics, SGFContext C) {
  assert(!ncRefType->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");

  // Any writebacks for this access are tightly scoped.
  FormalEvaluationScope scope(*this);

  // If this is a decl that we have an lvalue for, produce and return it.
  ValueDecl *decl = declRef.getDecl();
  
  if (!ncRefType) {
    ncRefType = decl->getInnermostDeclContext()->mapTypeIntoContext(
        decl->getInterfaceType());
  }
  CanType refType = ncRefType->getCanonicalType();

  auto getUnmanagedRValue = [&](SILValue value) -> RValue {
    return RValue(*this, loc, refType, ManagedValue::forUnmanaged(value));
  };

  // If this is a reference to a module, produce an undef value. The
  // module value should never actually be used.
  if (isa<ModuleDecl>(decl)) {
    return getUnmanagedRValue(
             SILUndef::get(getLoweredLoadableType(ncRefType), SGM.M));
  }

  // If this is a reference to a type, produce a metatype.
  if (isa<TypeDecl>(decl)) {
    assert(refType->is<MetatypeType>() &&
           "type declref does not have metatype type?!");
    return getUnmanagedRValue(B.createMetatype(loc, getLoweredType(refType)));
  }
  
  // If this is a reference to a var, produce an address or value.
  if (auto *var = dyn_cast<VarDecl>(decl)) {
    assert(!declRef.isSpecialized() &&
           "Cannot handle specialized variable references");

    // If this VarDecl is represented as an address, emit it as an lvalue, then
    // perform a load to get the rvalue.
    if (ManagedValue result =
            emitLValueForDecl(loc, var, refType, AccessKind::Read, semantics)) {
      bool guaranteedValid = false;
      IsTake_t shouldTake = IsNotTake;

      // We should only end up in this path for local and global variables,
      // i.e. ones whose lifetime is assured for the duration of the evaluation.
      // Therefore, if the variable is a constant, the value is guaranteed
      // valid as well.
      if (var->isLet())
        guaranteedValid = true;

      // If we have self, see if we are in an 'init' delegation sequence. If so,
      // call out to the special delegation init routine. Otherwise, use the
      // normal RValue emission logic.
      if (var->getName() == getASTContext().Id_self &&
          SelfInitDelegationState != NormalSelf) {
        return emitRValueForSelfInDelegationInit(loc, refType,
                                                 result.getLValueAddress(), C);
      }

      return RValue(*this, loc, refType,
                    emitLoad(loc, result.getLValueAddress(),
                             getTypeLowering(refType), C, shouldTake,
                             guaranteedValid));
    }

    // For local decls, use the address we allocated or the value if we have it.
    auto It = VarLocs.find(decl);
    if (It != VarLocs.end()) {
      // Mutable lvalue and address-only 'let's are LValues.
      assert(!It->second.value->getType().isAddress() &&
             "LValue cases should be handled above");

      SILValue Scalar = It->second.value;

      // For weak and unowned types, convert the reference to the right
      // pointer.
      if (Scalar->getType().is<ReferenceStorageType>()) {
        Scalar = emitConversionToSemanticRValue(loc, Scalar,
                                                getTypeLowering(refType));
        // emitConversionToSemanticRValue always produces a +1 strong result.
        return RValue(*this, loc,
                      refType, emitManagedRValueWithCleanup(Scalar));
      }

      // This is a let, so we can make guarantees, so begin the borrow scope.
      ManagedValue Result = emitManagedBeginBorrow(loc, Scalar);

      // If the client can't handle a +0 result, retain it to get a +1.
      // This is a 'let', so we can make guarantees.
      return RValue(*this, loc, refType,
                    C.isGuaranteedPlusZeroOk()
                      ? Result : Result.copyUnmanaged(*this, loc));
    }

    assert(var->hasAccessorFunctions() && "Unknown rvalue case");

    bool isDirectAccessorUse = (semantics == AccessSemantics::DirectToAccessor);
    SILDeclRef getter = getGetterDeclRef(var, isDirectAccessorUse);

    ArgumentSource selfSource;
    
    // Global properties have no base or subscript. Static properties
    // use the metatype as their base.
    // FIXME: This has to be dynamically looked up for classes, and
    // dynamically instantiated for generics.
    if (var->isStatic()) {
      auto baseTy = cast<NominalTypeDecl>(var->getDeclContext())
        ->getDeclaredInterfaceType();
      assert(!baseTy->is<BoundGenericType>() &&
             "generic static stored properties not implemented");
      assert((baseTy->getStructOrBoundGenericStruct() ||
              baseTy->getEnumOrBoundGenericEnum()) &&
             "static stored properties for classes/protocols not implemented");
      auto baseMeta = MetatypeType::get(baseTy)->getCanonicalType();

      auto metatype = B.createMetatype(loc,
                                       getLoweredLoadableType(baseMeta));
      auto metatypeMV = ManagedValue::forUnmanaged(metatype);
      auto metatypeRV = RValue(*this, loc, baseMeta, metatypeMV);
      selfSource = ArgumentSource(loc, std::move(metatypeRV));
    }
    return emitGetAccessor(loc, getter,
                           SGM.getNonMemberVarDeclSubstitutions(var),
                           std::move(selfSource),
                           /*isSuper=*/false, isDirectAccessorUse,
                           RValue(), C);
  }
  
  // If the referenced decl isn't a VarDecl, it should be a constant of some
  // sort.

  // If the referenced decl is a local func with context, then the SILDeclRef
  // uncurry level is one deeper (for the context vars).
  bool hasLocalCaptures = false;
  unsigned uncurryLevel = 0;
  if (auto *fd = dyn_cast<FuncDecl>(decl)) {
    hasLocalCaptures = SGM.M.Types.hasLoweredLocalCaptures(fd);
    if (hasLocalCaptures)
      ++uncurryLevel;
  }

  auto silDeclRef = SILDeclRef(decl, ResilienceExpansion::Minimal, uncurryLevel);

  ManagedValue result = emitClosureValue(loc, silDeclRef, refType,
                                         declRef.getSubstitutions());
  return RValue(*this, loc, refType, result);
}

static AbstractionPattern
getOrigFormalRValueType(SILGenFunction &gen, VarDecl *field) {
  auto origType = gen.SGM.Types.getAbstractionPattern(field);
  return origType.getReferenceStorageReferentType();
}

static SILDeclRef getRValueAccessorDeclRef(SILGenFunction &SGF,
                                           AbstractStorageDecl *storage,
                                           AccessStrategy strategy) {
  switch (strategy) {
  case AccessStrategy::BehaviorStorage:
    llvm_unreachable("shouldn't load an rvalue via behavior storage!");
  
  case AccessStrategy::Storage:
    llvm_unreachable("should already have been filtered out!");

  case AccessStrategy::DirectToAccessor:
    return SGF.getGetterDeclRef(storage, true);

  case AccessStrategy::DispatchToAccessor:
    return SGF.getGetterDeclRef(storage, false);

  case AccessStrategy::Addressor:
    return SGF.getAddressorDeclRef(storage, AccessKind::Read,
                                   /*always direct for now*/ true);
  }
  llvm_unreachable("should already have been filtered out!");
}

static RValue
emitRValueWithAccessor(SILGenFunction &SGF, SILLocation loc,
                       AbstractStorageDecl *storage,
                       SubstitutionList substitutions,
                       ArgumentSource &&baseRV, RValue &&subscriptRV,
                       bool isSuper, AccessStrategy strategy,
                       SILDeclRef accessor,
                       AbstractionPattern origFormalType,
                       CanType substFormalType,
                       SGFContext C) {
  bool isDirectUse = (strategy == AccessStrategy::DirectToAccessor);

  switch (strategy) {
  case AccessStrategy::BehaviorStorage:
    llvm_unreachable("shouldn't load an rvalue via behavior storage!");
  
  case AccessStrategy::Storage:
    llvm_unreachable("should already have been filtered out!");

  // The easy path here is if we don't need to use an addressor.
  case AccessStrategy::DirectToAccessor:
  case AccessStrategy::DispatchToAccessor: {
    return SGF.emitGetAccessor(loc, accessor, substitutions,
                               std::move(baseRV), isSuper, isDirectUse,
                               std::move(subscriptRV), C);
  }

  case AccessStrategy::Addressor:
    break;
  }

  auto &storageTL = SGF.getTypeLowering(origFormalType, substFormalType);
  SILType storageType = storageTL.getLoweredType().getAddressType();

  auto addressorResult =
    SGF.emitAddressorAccessor(loc, accessor, substitutions,
                              std::move(baseRV), isSuper, isDirectUse,
                              std::move(subscriptRV), storageType);

  SILValue address = addressorResult.first.getLValueAddress();

  SILType loweredSubstType =
    SGF.getLoweredType(substFormalType).getAddressType();
  bool hasAbstraction = (loweredSubstType != storageType);

  RValue result(SGF, loc, substFormalType,
    SGF.emitLoad(loc, address, storageTL,
                 (hasAbstraction ? SGFContext() : C), IsNotTake));
  if (hasAbstraction) {
    result = SGF.emitOrigToSubstValue(loc, std::move(result), origFormalType,
                                      substFormalType, C);
  }

  switch (cast<FuncDecl>(accessor.getDecl())->getAddressorKind()) {
  case AddressorKind::NotAddressor: llvm_unreachable("inconsistent");
  case AddressorKind::Unsafe:
    // Nothing to do.
    break;
  case AddressorKind::Owning:
  case AddressorKind::NativeOwning:
    // Emit the release immediately.
    SGF.B.emitDestroyValueOperation(loc, addressorResult.second.forward(SGF));
    break;
  case AddressorKind::NativePinning:
    // Emit the unpin immediately.
    SGF.B.createStrongUnpin(loc, addressorResult.second.forward(SGF),
                            SGF.B.getDefaultAtomicity());
    break;
  }
  
  return result;
}

/// Produce a singular RValue for a load from the specified property.  This
/// is designed to work with RValue ManagedValue bases that are either +0 or +1.
RValue SILGenFunction::emitRValueForPropertyLoad(
    SILLocation loc, ManagedValue base, CanType baseFormalType,
    bool isSuper, VarDecl *field, SubstitutionList substitutions,
    AccessSemantics semantics, Type propTy, SGFContext C,
    bool isGuaranteedValid) {
  AccessStrategy strategy =
    field->getAccessStrategy(semantics, AccessKind::Read);

  // If we should call an accessor of some kind, do so.
  if (strategy != AccessStrategy::Storage) {
    auto accessor = getRValueAccessorDeclRef(*this, field, strategy);
    ArgumentSource baseRV = prepareAccessorBaseArg(loc, base,
                                                   baseFormalType,
                                                   accessor);

    AbstractionPattern origFormalType =
      getOrigFormalRValueType(*this, field);
    auto substFormalType = propTy->getCanonicalType();

    return emitRValueWithAccessor(*this, loc, field, substitutions,
                                  std::move(baseRV), RValue(),
                                  isSuper, strategy, accessor,
                                  origFormalType, substFormalType, C);
  }

  assert(field->hasStorage() &&
         "Cannot directly access value without storage");

  // For static variables, emit a reference to the global variable backing
  // them.
  // FIXME: This has to be dynamically looked up for classes, and
  // dynamically instantiated for generics.
  if (field->isStatic()) {
    auto baseMeta = base.getType().castTo<MetatypeType>().getInstanceType();
    (void)baseMeta;
    assert(!baseMeta->is<BoundGenericType>() &&
           "generic static stored properties not implemented");
    if (field->getDeclContext()->getAsClassOrClassExtensionContext() &&
        field->hasStorage())
      // FIXME: don't need to check hasStorage, already done above
      assert(field->isFinal() && "non-final class stored properties not implemented");

    return emitRValueForDecl(loc, field, propTy, semantics, C);
  }


  // rvalue MemberRefExprs are produced in two cases: when accessing a 'let'
  // decl member, and when the base is a (non-lvalue) struct.
  assert(baseFormalType->getAnyNominal() &&
         base.getType().getSwiftRValueType()->getAnyNominal() &&
         "The base of an rvalue MemberRefExpr should be an rvalue value");

  // If the accessed field is stored, emit a StructExtract on the base.

  auto substFormalType = propTy->getCanonicalType();
  auto &lowering = getTypeLowering(substFormalType);

  // Check for an abstraction difference.
  AbstractionPattern origFormalType = getOrigFormalRValueType(*this, field);
  bool hasAbstractionChange = false;
  auto &abstractedTL = getTypeLowering(origFormalType, substFormalType);
  if (!origFormalType.isExactType(substFormalType)) {
    hasAbstractionChange =
        (abstractedTL.getLoweredType() != lowering.getLoweredType());
  }

  // If the base is a reference type, just handle this as loading the lvalue.
  if (baseFormalType->hasReferenceSemantics()) {
    LValue LV = emitPropertyLValue(loc, base, baseFormalType, field,
                                   AccessKind::Read,
                                   AccessSemantics::DirectToStorage);
    return emitLoadOfLValue(loc, std::move(LV), C, isGuaranteedValid);
  }

  ManagedValue result;
  if (!base.getType().isAddress()) {
    // For non-address-only structs, we emit a struct_extract sequence.
    result = B.createStructExtract(loc, base, field);

    if (result.getType().is<ReferenceStorageType>()) {
      // For weak and unowned types, convert the reference to the right
      // pointer, producing a +1.
      result = emitConversionToSemanticRValue(loc, result, lowering);

    } else if (hasAbstractionChange ||
               (!C.isImmediatePlusZeroOk() &&
                !(C.isGuaranteedPlusZeroOk() && isGuaranteedValid))) {
      // If we have an abstraction change or if we have to produce a result at
      // +1, then emit a RetainValue. If we know that our base will stay alive,
      // we can emit at +0 for a guaranteed consumer. Otherwise, since we do not
      // have enough information, we can only emit at +0 for immediate clients.
      result = result.copyUnmanaged(*this, loc);
    }
  } else {
    // For address-only sequences, the base is in memory.  Emit a
    // struct_element_addr to get to the field, and then load the element as an
    // rvalue.
    SILValue ElementPtr =
      B.createStructElementAddr(loc, base.getValue(), field);

    result = emitLoad(loc, ElementPtr, abstractedTL,
                      hasAbstractionChange ? SGFContext() : C, IsNotTake);
  }

  // If we're accessing this member with an abstraction change, perform that
  // now.
  if (hasAbstractionChange)
    result =
        emitOrigToSubstValue(loc, result, origFormalType, substFormalType, C);
  return RValue(*this, loc, substFormalType, result);
}


RValue RValueEmitter::visitDeclRefExpr(DeclRefExpr *E, SGFContext C) {
  return SGF.emitRValueForDecl(E, E->getDeclRef(), E->getType(),
                               E->getAccessSemantics(), C);
}

RValue RValueEmitter::visitTypeExpr(TypeExpr *E, SGFContext C) {
  assert(E->getType()->is<AnyMetatypeType>() &&
         "TypeExpr must have metatype type");
  auto Val = SGF.B.createMetatype(E, SGF.getLoweredType(E->getType()));
  return RValue(SGF, E, ManagedValue::forUnmanaged(Val));
}


RValue RValueEmitter::visitSuperRefExpr(SuperRefExpr *E, SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");

  // If we have a normal self call, then use the emitRValueForDecl call. This
  // will emit self at +0 since it is guaranteed.
  ManagedValue Self =
      SGF.emitRValueForDecl(E, E->getSelf(), E->getSelf()->getType(),
                            AccessSemantics::Ordinary)
          .getScalarValue();

  // Perform an upcast to convert self to the indicated super type.
  auto Result = SGF.B.createUpcast(E, Self.getValue(),
                                   SGF.getLoweredType(E->getType()));

  return RValue(SGF, E, ManagedValue(Result, Self.getCleanup()));
}

RValue RValueEmitter::
visitUnresolvedTypeConversionExpr(UnresolvedTypeConversionExpr *E,
                                  SGFContext C) {
  llvm_unreachable("invalid code made its way into SILGen");
}

RValue RValueEmitter::visitOtherConstructorDeclRefExpr(
                                OtherConstructorDeclRefExpr *E, SGFContext C) {
  // This should always be a child of an ApplyExpr and so will be emitted by
  // SILGenApply.
  llvm_unreachable("unapplied reference to constructor?!");
}

RValue RValueEmitter::visitNilLiteralExpr(NilLiteralExpr *E, SGFContext C) {
  llvm_unreachable("NilLiteralExpr not lowered?");
}

RValue RValueEmitter::visitIntegerLiteralExpr(IntegerLiteralExpr *E,
                                              SGFContext C) {
  return RValue(SGF, E,
                ManagedValue::forUnmanaged(SGF.B.createIntegerLiteral(E)));
}
RValue RValueEmitter::visitFloatLiteralExpr(FloatLiteralExpr *E,
                                            SGFContext C) {
  return RValue(SGF, E,
                ManagedValue::forUnmanaged(SGF.B.createFloatLiteral(E)));
}

RValue RValueEmitter::visitBooleanLiteralExpr(BooleanLiteralExpr *E, 
                                              SGFContext C) {
  auto i1Ty = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
  SILValue boolValue = SGF.B.createIntegerLiteral(E, i1Ty, E->getValue());
  return RValue(SGF, E, ManagedValue::forUnmanaged(boolValue));
}

RValue RValueEmitter::visitStringLiteralExpr(StringLiteralExpr *E,
                                             SGFContext C) {
  return SGF.emitLiteral(E, C);
}

RValue RValueEmitter::visitLoadExpr(LoadExpr *E, SGFContext C) {
  // Any writebacks here are tightly scoped.
  FormalEvaluationScope writeback(SGF);
  LValue lv = SGF.emitLValue(E->getSubExpr(), AccessKind::Read);
  return SGF.emitLoadOfLValue(E, std::move(lv), C);
}

SILValue SILGenFunction::emitTemporaryAllocation(SILLocation loc,
                                                 SILType ty) {
  ty = ty.getObjectType();
  auto alloc = B.createAllocStack(loc, ty);
  enterDeallocStackCleanup(alloc);
  return alloc;
}

SILValue SILGenFunction::
getBufferForExprResult(SILLocation loc, SILType ty, SGFContext C) {
  // If you change this, change manageBufferForExprResult below as well.

  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (SILValue address = C.getAddressForInPlaceInitialization())
    return address;
  
  // If we couldn't emit into the Initialization, emit into a temporary
  // allocation.
  return emitTemporaryAllocation(loc, ty.getObjectType());
}

ManagedValue SILGenFunction::
manageBufferForExprResult(SILValue buffer, const TypeLowering &bufferTL,
                          SGFContext C) {
  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (C.getAddressForInPlaceInitialization()) {
    C.getEmitInto()->finishInitialization(*this);
    return ManagedValue::forInContext();
  }
  
  // Add a cleanup for the temporary we allocated.
  if (bufferTL.isTrivial())
    return ManagedValue::forUnmanaged(buffer);

  return ManagedValue(buffer, enterDestroyCleanup(buffer));
}

RValue RValueEmitter::visitForceTryExpr(ForceTryExpr *E, SGFContext C) {
  // Set up a "catch" block for when an error occurs.
  SILBasicBlock *catchBB = SGF.createBasicBlock(FunctionSection::Postmatter);
  llvm::SaveAndRestore<JumpDest> throwDest{
      SGF.ThrowDest,
      JumpDest(catchBB, SGF.Cleanups.getCleanupsDepth(),
               CleanupLocation::get(E))};

  // Visit the sub-expression.
  RValue result = visit(E->getSubExpr(), C);

  // If there are no uses of the catch block, just drop it.
  if (catchBB->pred_empty()) {
    SGF.eraseBasicBlock(catchBB);
  } else {
    // Otherwise, we need to emit it.
    SavedInsertionPoint scope(SGF, catchBB, FunctionSection::Postmatter);

    ASTContext &ctx = SGF.getASTContext();
    auto error = catchBB->createPHIArgument(SILType::getExceptionType(ctx),
                                            ValueOwnershipKind::Owned);
    SGF.B.createBuiltin(E, ctx.getIdentifier("unexpectedError"),
                        SGF.SGM.Types.getEmptyTupleType(), {}, {error});
    SGF.B.createUnreachable(E);
  }

  return result;
}

RValue RValueEmitter::visitOptionalTryExpr(OptionalTryExpr *E, SGFContext C) {
  // FIXME: Much of this was copied from visitOptionalEvaluationExpr.

  auto &optTL = SGF.getTypeLowering(E->getType());

  Initialization *optInit = C.getEmitInto();
  bool usingProvidedContext = optInit && optInit->isSingleBuffer();

  // Form the optional using address operations if the type is address-only or
  // if we already have an address to use.
  bool isByAddress = usingProvidedContext || optTL.isAddressOnly();

  std::unique_ptr<TemporaryInitialization> optTemp;
  if (!usingProvidedContext && isByAddress) {
    // Allocate the temporary for the Optional<T> if we didn't get one from the
    // context.
    optTemp = SGF.emitTemporary(E, optTL);
    optInit = optTemp.get();
  } else if (!usingProvidedContext) {
    // If the caller produced a context for us, but we can't use it, then don't.
    optInit = nullptr;
  }

  FullExpr localCleanups(SGF.Cleanups, E);

  // Set up a "catch" block for when an error occurs.
  SILBasicBlock *catchBB = SGF.createBasicBlock(FunctionSection::Postmatter);
  llvm::SaveAndRestore<JumpDest> throwDest{
    SGF.ThrowDest,
    JumpDest(catchBB, SGF.Cleanups.getCleanupsDepth(), E)};

  SILValue branchArg;
  if (isByAddress) {
    assert(optInit);
    SILValue optAddr = optInit->getAddress();
    SGF.emitInjectOptionalValueInto(E, E->getSubExpr(), optAddr, optTL);
  } else {
    ManagedValue subExprValue = SGF.emitRValueAsSingleValue(E->getSubExpr());
    ManagedValue wrapped = SGF.getOptionalSomeValue(E, subExprValue, optTL);
    branchArg = wrapped.forward(SGF);
  }

  localCleanups.pop();

  // If it turns out there are no uses of the catch block, just drop it.
  if (catchBB->pred_empty()) {
    // Remove the dead failureBB.
    catchBB->eraseFromParent();

    // The value we provide is the one we've already got.
    if (!isByAddress)
      return RValue(SGF, E,
                    SGF.emitManagedRValueWithCleanup(branchArg, optTL));

    optInit->finishInitialization(SGF);

    // If we emitted into the provided context, we're done.
    if (usingProvidedContext)
      return RValue();

    return RValue(SGF, E, optTemp->getManagedAddress());
  }

  SILBasicBlock *contBB = SGF.createBasicBlock();

  // Branch to the continuation block.
  if (isByAddress)
    SGF.B.createBranch(E, contBB);
  else
    SGF.B.createBranch(E, contBB, branchArg);

  // If control branched to the failure block, inject .None into the
  // result type.
  SGF.B.emitBlock(catchBB);
  FullExpr catchCleanups(SGF.Cleanups, E);
  auto *errorArg =
      catchBB->createPHIArgument(SILType::getExceptionType(SGF.getASTContext()),
                                 ValueOwnershipKind::Owned);
  (void) SGF.emitManagedRValueWithCleanup(errorArg);
  catchCleanups.pop();

  if (isByAddress) {
    SGF.emitInjectOptionalNothingInto(E, optInit->getAddress(), optTL);
    SGF.B.createBranch(E, contBB);
  } else {
    auto branchArg = SGF.getOptionalNoneValue(E, optTL);
    SGF.B.createBranch(E, contBB, branchArg);
  }

  // Emit the continuation block.
  SGF.B.emitBlock(contBB);

  // If this was done in SSA registers, then the value is provided as an
  // argument to the block.
  if (!isByAddress) {
    auto arg = contBB->createPHIArgument(optTL.getLoweredType(),
                                         ValueOwnershipKind::Owned);
    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(arg, optTL));
  }

  optInit->finishInitialization(SGF);

  // If we emitted into the provided context, we're done.
  if (usingProvidedContext)
    return RValue();

  assert(optTemp);
  return RValue(SGF, E, optTemp->getManagedAddress());
}

RValue RValueEmitter::visitDerivedToBaseExpr(DerivedToBaseExpr *E,
                                             SGFContext C) {
  ManagedValue original = SGF.emitRValueAsSingleValue(E->getSubExpr());

  // Derived-to-base casts in the AST might not be reflected as such
  // in the SIL type system, for example, a cast from DynamicSelf
  // directly to its own Self type.
  auto loweredResultTy = SGF.getLoweredType(E->getType());
  if (original.getType() == loweredResultTy)
    return RValue(SGF, E, original);

  SILValue converted = SGF.B.createUpcast(E, original.getValue(), 
                                          loweredResultTy);
  return RValue(SGF, E, ManagedValue(converted, original.getCleanup()));
}

RValue RValueEmitter::visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                                  SGFContext C) {
  SILValue metaBase =
    SGF.emitRValueAsSingleValue(E->getSubExpr()).getUnmanagedValue();

  // Metatype conversion casts in the AST might not be reflected as
  // such in the SIL type system, for example, a cast from DynamicSelf.Type
  // directly to its own Self.Type.
  auto loweredResultTy = SGF.getLoweredLoadableType(E->getType());
  if (metaBase->getType() == loweredResultTy)
    return RValue(SGF, E, ManagedValue::forUnmanaged(metaBase));

  auto upcast = SGF.B.createUpcast(E, metaBase, loweredResultTy);
  return RValue(SGF, E, ManagedValue::forUnmanaged(upcast));
}

RValue SILGenFunction::emitCollectionConversion(SILLocation loc,
                                                FuncDecl *fn,
                                                CanType fromCollection,
                                                CanType toCollection,
                                                ManagedValue mv,
                                                SGFContext C) {
  auto *fromDecl = fromCollection->getAnyNominal();
  auto *toDecl = toCollection->getAnyNominal();

  auto fromSubMap = fromCollection->getContextSubstitutionMap(
    SGM.SwiftModule, fromDecl);
  auto toSubMap = toCollection->getContextSubstitutionMap(
    SGM.SwiftModule, toDecl);

  // Form type parameter substitutions.
  auto *genericSig = fn->getGenericSignature();
  unsigned fromParamCount = fromDecl->getGenericSignature()
    ->getGenericParams().size();

  auto subMap =
    SubstitutionMap::combineSubstitutionMaps(fromSubMap,
                                             toSubMap,
                                             CombineSubstitutionMaps::AtIndex,
                                             fromParamCount,
                                             0,
                                             genericSig);
  return emitApplyOfLibraryIntrinsic(loc, fn, subMap, {mv}, C);
}

RValue RValueEmitter::
visitCollectionUpcastConversionExpr(CollectionUpcastConversionExpr *E,
                                    SGFContext C) {
  
  SILLocation loc = RegularLocation(E);
  
  // Get the sub expression argument as a managed value
  auto mv = SGF.emitRValueAsSingleValue(E->getSubExpr());
  
  // Compute substitutions for the intrinsic call.
  auto fromCollection = E->getSubExpr()->getType()->getCanonicalType();
  auto toCollection = E->getType()->getCanonicalType();

  // Get the intrinsic function.
  auto &ctx = SGF.getASTContext();
  FuncDecl *fn = nullptr;
  if (fromCollection->getAnyNominal() == ctx.getArrayDecl()) {
    fn = SGF.SGM.getArrayForceCast(loc);
  } else if (fromCollection->getAnyNominal() == ctx.getDictionaryDecl()) {
    fn = SGF.SGM.getDictionaryUpCast(loc);
  } else if (fromCollection->getAnyNominal() == ctx.getSetDecl()) {
    fn = SGF.SGM.getSetUpCast(loc);
  } else {
    llvm_unreachable("unsupported collection upcast kind");
  }

  return SGF.emitCollectionConversion(loc, fn, fromCollection, toCollection,
                                      mv, C);
}

RValue RValueEmitter::visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E,
                                                SGFContext C) {
  ManagedValue archetype = SGF.emitRValueAsSingleValue(E->getSubExpr());
  // Replace the cleanup with a new one on the superclass value so we always use
  // concrete retain/release operations.
  SILValue base = SGF.B.createUpcast(E,
                                    archetype.forward(SGF),
                                    SGF.getLoweredLoadableType(E->getType()));
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(base));
}

static ManagedValue convertCFunctionSignature(SILGenFunction &SGF,
                                              FunctionConversionExpr *e,
                                              SILType loweredResultTy,
                                llvm::function_ref<ManagedValue ()> fnEmitter) {
  SILType loweredDestTy = SGF.getLoweredType(e->getType());
  ManagedValue result;

  // We're converting between C function pointer types. They better be
  // ABI-compatible, since we can't emit a thunk.
  switch (SGF.SGM.Types.checkForABIDifferences(loweredResultTy, loweredDestTy)){
  case TypeConverter::ABIDifference::Trivial:
    result = fnEmitter();
    assert(result.getType() == loweredResultTy);

    if (loweredResultTy != loweredDestTy) {
      result = ManagedValue::forUnmanaged(
          SGF.B.createConvertFunction(e, result.getUnmanagedValue(),
                                      loweredDestTy));
    }

    break;

  case TypeConverter::ABIDifference::NeedsThunk:
    // Note: in this case, we don't call the emitter at all -- doing so
    // just runs the risk of tripping up asserts in SILGenBridging.cpp
    SGF.SGM.diagnose(e, diag::unsupported_c_function_pointer_conversion,
                     e->getSubExpr()->getType(), e->getType());
    result = SGF.emitUndef(e, loweredDestTy);
    break;

  case TypeConverter::ABIDifference::ThinToThick:
    llvm_unreachable("Cannot have thin to thick conversion here");
  }

  return result;
}

static
ManagedValue emitCFunctionPointer(SILGenFunction &gen,
                                  FunctionConversionExpr *conversionExpr) {
  auto expr = conversionExpr->getSubExpr();
  
  // Look through base-ignored exprs to get to the function ref.
  auto semanticExpr = expr->getSemanticsProvidingExpr();
  while (auto ignoredBase = dyn_cast<DotSyntaxBaseIgnoredExpr>(semanticExpr)){
    gen.emitIgnoredExpr(ignoredBase->getLHS());
    semanticExpr = ignoredBase->getRHS()->getSemanticsProvidingExpr();
  }

  // Recover the decl reference.
  SILDeclRef::Loc loc;
  
  auto setLocFromConcreteDeclRef = [&](ConcreteDeclRef declRef) {
    // TODO: Handle generic instantiations, where we need to eagerly specialize
    // on the given generic parameters, and static methods, where we need to drop
    // in the metatype.
    assert(!declRef.getDecl()->getDeclContext()->isTypeContext()
           && "c pointers to static methods not implemented");
    assert(declRef.getSubstitutions().empty()
           && "c pointers to generics not implemented");
    loc = declRef.getDecl();
  };
  
  if (auto declRef = dyn_cast<DeclRefExpr>(semanticExpr)) {
    setLocFromConcreteDeclRef(declRef->getDeclRef());
  } else if (auto memberRef = dyn_cast<MemberRefExpr>(semanticExpr)) {
    setLocFromConcreteDeclRef(memberRef->getMember());
  } else if (auto closure = dyn_cast<AbstractClosureExpr>(semanticExpr)) {
    loc = closure;
    // Emit the closure body.
    gen.SGM.emitClosure(closure);
  } else {
    llvm_unreachable("c function pointer converted from a non-concrete decl ref");
  }

  // Produce a reference to the C-compatible entry point for the function.
  SILDeclRef constant(loc, ResilienceExpansion::Minimal,
                      /*uncurryLevel*/ 0,
                      /*foreign*/ true);
  SILConstantInfo constantInfo = gen.getConstantInfo(constant);

  return convertCFunctionSignature(
                    gen, conversionExpr,
                    constantInfo.getSILType(),
                    [&]() -> ManagedValue {
                      SILValue cRef = gen.emitGlobalFunctionRef(expr, constant);
                      return ManagedValue::forUnmanaged(cRef);
                    });
}

// Change the representation without changing the signature or
// abstraction level.
static ManagedValue convertFunctionRepresentation(SILGenFunction &SGF,
                                                  SILLocation loc,
                                                  ManagedValue result,
                                                  CanAnyFunctionType srcTy,
                                                  CanAnyFunctionType destTy) {
  auto resultFTy = result.getType().castTo<SILFunctionType>();

  // Note that conversions to and from block require a thunk
  switch (destTy->getRepresentation()) {

  // Convert thin, c, block => thick
  case AnyFunctionType::Representation::Swift: {
    switch (resultFTy->getRepresentation()) {
    case SILFunctionType::Representation::Thin: {
      auto v = SGF.B.createThinToThickFunction(loc, result.getValue(),
        SILType::getPrimitiveObjectType(
         adjustFunctionType(resultFTy, SILFunctionType::Representation::Thick)));
      result = ManagedValue(v, result.getCleanup());
      break;
    }
    case SILFunctionType::Representation::Thick:
      llvm_unreachable("should not try thick-to-thick repr change");
    case SILFunctionType::Representation::CFunctionPointer:
    case SILFunctionType::Representation::Block:
      result = SGF.emitBlockToFunc(loc, result,
                       SGF.getLoweredType(destTy).castTo<SILFunctionType>());
      break;
    case SILFunctionType::Representation::Method:
    case SILFunctionType::Representation::Closure:
    case SILFunctionType::Representation::ObjCMethod:
    case SILFunctionType::Representation::WitnessMethod:
      llvm_unreachable("should not do function conversion from method rep");
    }
    break;
  }

  // Convert thin, thick, c => block
  case AnyFunctionType::Representation::Block:
    switch (resultFTy->getRepresentation()) {
    case SILFunctionType::Representation::Thin: {
      // Make thick first.
      auto v = SGF.B.createThinToThickFunction(loc, result.getValue(),
        SILType::getPrimitiveObjectType(
         adjustFunctionType(resultFTy, SILFunctionType::Representation::Thick)));
      result = ManagedValue(v, result.getCleanup());
      LLVM_FALLTHROUGH;
    }
    case SILFunctionType::Representation::Thick:
    case SILFunctionType::Representation::CFunctionPointer:
      // Convert to a block.
      result = SGF.emitFuncToBlock(loc, result,
                     SGF.getLoweredType(destTy).castTo<SILFunctionType>());
      break;
    case SILFunctionType::Representation::Block:
      llvm_unreachable("should not try block-to-block repr change");
    case SILFunctionType::Representation::Method:
    case SILFunctionType::Representation::Closure:
    case SILFunctionType::Representation::ObjCMethod:
    case SILFunctionType::Representation::WitnessMethod:
      llvm_unreachable("should not do function conversion from method rep");
    }
    break;

  // Unsupported
  case AnyFunctionType::Representation::Thin:
    llvm_unreachable("should not do function conversion to thin");
  case AnyFunctionType::Representation::CFunctionPointer:
    llvm_unreachable("should not do C function pointer conversion here");
  }

  return result;
}

RValue RValueEmitter::visitFunctionConversionExpr(FunctionConversionExpr *e,
                                                  SGFContext C)
{
  CanAnyFunctionType srcRepTy =
      cast<FunctionType>(e->getSubExpr()->getType()->getCanonicalType());
  CanAnyFunctionType destRepTy =
      cast<FunctionType>(e->getType()->getCanonicalType());

  if (destRepTy->getRepresentation() ==
      FunctionTypeRepresentation::CFunctionPointer) {
    ManagedValue result;

    if (srcRepTy->getRepresentation() !=
        FunctionTypeRepresentation::CFunctionPointer) {
      // A "conversion" of a DeclRef a C function pointer is done by referencing
      // the thunk (or original C function) with the C calling convention.
      result = emitCFunctionPointer(SGF, e);
    } else {
      // Ok, we're converting a C function pointer value to another C function
      // pointer.

      // Emit the C function pointer
      result = SGF.emitRValueAsSingleValue(e->getSubExpr());

      // Possibly bitcast the C function pointer to account for ABI-compatible
      // parameter and result type conversions
      result = convertCFunctionSignature(SGF, e, result.getType(),
                                         [&]() -> ManagedValue {
                                           return result;
                                         });
    }
    return RValue(SGF, e, result);
  }

  // Break the conversion into three stages:
  // 1) changing the representation from foreign to native
  // 2) changing the signature within the representation
  // 3) changing the representation from native to foreign
  //
  // We only do one of 1) or 3), but we have to do them in the right order
  // with respect to 2).

  CanAnyFunctionType srcTy = srcRepTy;
  CanAnyFunctionType destTy = destRepTy;

  switch(srcRepTy->getRepresentation()) {
  case AnyFunctionType::Representation::Swift:
  case AnyFunctionType::Representation::Thin:
    // Source is native, so we can convert signature first.
    destTy = adjustFunctionType(destRepTy,
                                srcTy->getRepresentation());
    break;
  case AnyFunctionType::Representation::Block:
  case AnyFunctionType::Representation::CFunctionPointer:
    // Source is foreign, so do the representation change first.
    srcTy = adjustFunctionType(srcRepTy,
                               destRepTy->getRepresentation());
  }

  auto result = SGF.emitRValueAsSingleValue(e->getSubExpr());

  if (srcRepTy != srcTy)
    result = convertFunctionRepresentation(SGF, e, result, srcRepTy, srcTy);

  if (srcTy != destTy)
    result = SGF.emitTransformedValue(e, result, srcTy, destTy);

  if (destTy != destRepTy)
    result = convertFunctionRepresentation(SGF, e, result, destTy, destRepTy);

  return RValue(SGF, e, result);
}

RValue RValueEmitter::visitCovariantFunctionConversionExpr(
                        CovariantFunctionConversionExpr *e,
                        SGFContext C) {
  ManagedValue original = SGF.emitRValueAsSingleValue(e->getSubExpr());
  CanAnyFunctionType destTy
    = cast<AnyFunctionType>(e->getType()->getCanonicalType());
  SILType resultType = SGF.getLoweredType(destTy);
  SILValue result = SGF.B.createConvertFunction(e, 
                                                original.forward(SGF),
                                                resultType);
  return RValue(SGF, e, SGF.emitManagedRValueWithCleanup(result));
}

static ManagedValue createUnsafeDowncast(SILGenFunction &gen,
                                         SILLocation loc,
                                         ManagedValue input,
                                         SILType resultTy) {
  SILValue result = gen.B.createUncheckedRefCast(loc,
                                                 input.forward(gen),
                                                 resultTy);
  return gen.emitManagedRValueWithCleanup(result);
}

RValue RValueEmitter::visitCovariantReturnConversionExpr(
                        CovariantReturnConversionExpr *e,
                        SGFContext C) {
  SILType resultType = SGF.getLoweredType(e->getType());

  ManagedValue original = SGF.emitRValueAsSingleValue(e->getSubExpr());
  ManagedValue result;
  if (resultType.getSwiftRValueType().getAnyOptionalObjectType()) {
    result = SGF.emitOptionalToOptional(e, original, resultType,
                                        createUnsafeDowncast);
  } else {
    result = createUnsafeDowncast(SGF, e, original, resultType);
  }

  return RValue(SGF, e, result);
}

RValue RValueEmitter::visitErasureExpr(ErasureExpr *E, SGFContext C) {
  auto &existentialTL = SGF.getTypeLowering(E->getType());
  auto concreteFormalType = E->getSubExpr()->getType()->getCanonicalType();

  auto archetype = ArchetypeType::getAnyOpened(E->getType());
  AbstractionPattern abstractionPattern(archetype);
  auto &concreteTL = SGF.getTypeLowering(abstractionPattern,
                                         concreteFormalType);

  ManagedValue mv = SGF.emitExistentialErasure(E, concreteFormalType,
                                               concreteTL, existentialTL,
                                               E->getConformances(), C,
                               [&](SGFContext C) -> ManagedValue {
                                 return SGF.emitRValueAsOrig(E->getSubExpr(),
                                                             abstractionPattern,
                                                             concreteTL, C);
                               });

  return RValue(SGF, E, mv);
}

RValue SILGenFunction::emitAnyHashableErasure(SILLocation loc,
                                              ManagedValue value,
                                              Type type,
                                              ProtocolConformanceRef conformance,
                                              SGFContext C) {
  // Ensure that the intrinsic function exists.
  auto convertFn = SGM.getConvertToAnyHashable(loc);
  if (!convertFn)
    return emitUndefRValue(
        loc, getASTContext().getAnyHashableDecl()->getDeclaredType());

  // Construct the substitution for T: Hashable.
  auto subMap = SubstitutionMap::getProtocolSubstitutions(
      conformance.getRequirement(), type, conformance);

  return emitApplyOfLibraryIntrinsic(loc, convertFn, subMap, value, C);
}

RValue RValueEmitter::visitAnyHashableErasureExpr(AnyHashableErasureExpr *E,
                                                  SGFContext C) {
  // Emit the source value into a temporary.
  auto sourceOrigType = AbstractionPattern::getOpaque();
  auto source =
    SGF.emitMaterializedRValueAsOrig(E->getSubExpr(), sourceOrigType);

  return SGF.emitAnyHashableErasure(E, source,
                                    E->getSubExpr()->getType(),
                                    E->getConformance(), C);
}

/// Treating this as a successful operation, turn a CMV into a +1 MV.
ManagedValue SILGenFunction::getManagedValue(SILLocation loc,
                                             ConsumableManagedValue value) {
  // If the consumption rules say that this is already +1 given a
  // successful operation, just use the value.
  if (value.isOwned())
    return value.getFinalManagedValue();

  SILType valueTy = value.getType();
  auto &valueTL = getTypeLowering(valueTy);

  // If the type is trivial, it's always +1.
  if (valueTL.isTrivial())
    return ManagedValue::forUnmanaged(value.getValue());

  // If it's an object...
  if (valueTy.isObject()) {
    // See if we have more accurate information from the ownership kind. This
    // detects trivial cases of enums.
    if (value.getOwnershipKind() == ValueOwnershipKind::Trivial)
      return ManagedValue::forUnmanaged(value.getValue());

    // Otherwise, retain and enter a release cleanup.
    valueTL.emitCopyValue(B, loc, value.getValue());
    return emitManagedRValueWithCleanup(value.getValue(), valueTL);
  }

  // Otherwise, produce a temporary and copy into that.
  auto temporary = emitTemporary(loc, valueTL);
  valueTL.emitCopyInto(B, loc, value.getValue(), temporary->getAddress(),
                       IsNotTake, IsInitialization);
  temporary->finishInitialization(*this);
  return temporary->getManagedAddress();
}

RValue RValueEmitter::visitForcedCheckedCastExpr(ForcedCheckedCastExpr *E,
                                                 SGFContext C) {
  return emitUnconditionalCheckedCast(SGF, E, E->getSubExpr(), E->getType(),
                                      E->getCastKind(), C);
}


RValue RValueEmitter::
visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                SGFContext C) {
  ManagedValue operand = SGF.emitRValueAsSingleValue(E->getSubExpr());
  return emitConditionalCheckedCast(SGF, E, operand, E->getSubExpr()->getType(),
                                    E->getType(), E->getCastKind(), C);
}

RValue RValueEmitter::visitIsExpr(IsExpr *E, SGFContext C) {
  SILValue isa = emitIsa(SGF, E, E->getSubExpr(),
                         E->getCastTypeLoc().getType(), E->getCastKind());

  // Call the _getBool library intrinsic.
  ASTContext &ctx = SGF.getASTContext();
  auto result =
    SGF.emitApplyOfLibraryIntrinsic(E, ctx.getGetBoolDecl(nullptr),
                                    SubstitutionMap(),
                                    ManagedValue::forUnmanaged(isa),
                                    C);
  return result;
}

RValue RValueEmitter::visitEnumIsCaseExpr(EnumIsCaseExpr *E,
                                          SGFContext C) {
  ASTContext &ctx = SGF.getASTContext();
  // Get the enum value.
  auto subExpr = SGF.emitRValueAsSingleValue(E->getSubExpr(),
                                SGFContext(SGFContext::AllowImmediatePlusZero));
  // Test its case.
  auto i1Ty = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
  auto t = SGF.B.createIntegerLiteral(E, i1Ty, 1);
  auto f = SGF.B.createIntegerLiteral(E, i1Ty, 0);
  
  SILValue selected;
  if (subExpr.getType().isAddress()) {
    selected = SGF.B.createSelectEnumAddr(E, subExpr.getValue(), i1Ty, f,
                                          {{E->getEnumElement(), t}});
  } else {
    selected = SGF.B.createSelectEnum(E, subExpr.getValue(), i1Ty, f,
                                      {{E->getEnumElement(), t}});
  }
  
  // Call the _getBool library intrinsic.
  auto result =
    SGF.emitApplyOfLibraryIntrinsic(E, ctx.getGetBoolDecl(nullptr),
                                    SubstitutionMap(),
                                    ManagedValue::forUnmanaged(selected),
                                    C);
  return result;
}

RValue RValueEmitter::visitCoerceExpr(CoerceExpr *E, SGFContext C) {
  return visit(E->getSubExpr(), C);
}

VarargsInfo Lowering::emitBeginVarargs(SILGenFunction &gen, SILLocation loc,
                                       CanType baseTy, CanType arrayTy,
                                       unsigned numElements) {
  // Reabstract the base type against the array element type.
  auto baseAbstraction = AbstractionPattern::getOpaque();

  // Allocate the array.
  SILValue numEltsVal = gen.B.createIntegerLiteral(loc,
                             SILType::getBuiltinWordType(gen.getASTContext()),
                             numElements);
  // The first result is the array value.
  ManagedValue array;
  // The second result is a RawPointer to the base address of the array.
  SILValue basePtr;
  std::tie(array, basePtr)
    = gen.emitUninitializedArrayAllocation(arrayTy, numEltsVal, loc);

  // Temporarily deactivate the main array cleanup.
  if (array.hasCleanup())
    gen.Cleanups.setCleanupState(array.getCleanup(), CleanupState::Dormant);

  // Push a new cleanup to deallocate the array.
  auto abortCleanup =
    gen.enterDeallocateUninitializedArrayCleanup(array.getValue());

  auto &baseTL = gen.getTypeLowering(baseAbstraction, baseTy);

  // Turn the pointer into an address.
  basePtr = gen.B.createPointerToAddress(
    loc, basePtr, baseTL.getLoweredType().getAddressType(), /*isStrict*/ true);

  return VarargsInfo(array, abortCleanup, basePtr, baseTL, baseAbstraction);
}

ManagedValue Lowering::emitEndVarargs(SILGenFunction &gen, SILLocation loc,
                                      VarargsInfo &&varargs) {
  // Kill the abort cleanup.
  gen.Cleanups.setCleanupState(varargs.getAbortCleanup(), CleanupState::Dead);

  // Reactivate the result cleanup.
  auto result = varargs.getArray();
  if (result.hasCleanup())
    gen.Cleanups.setCleanupState(result.getCleanup(), CleanupState::Active);
  return result;
}

static ManagedValue emitVarargs(SILGenFunction &gen,
                                SILLocation loc,
                                Type _baseTy,
                                ArrayRef<ManagedValue> elements,
                                Type _arrayTy) {
  auto baseTy = _baseTy->getCanonicalType();
  auto arrayTy = _arrayTy->getCanonicalType();

  auto varargs = emitBeginVarargs(gen, loc, baseTy, arrayTy, elements.size());
  AbstractionPattern baseAbstraction = varargs.getBaseAbstractionPattern();
  SILValue basePtr = varargs.getBaseAddress();
  
  // Initialize the members.
  // TODO: If we need to cleanly unwind at this point, we would need to arrange
  // for the partially-initialized array to be cleaned up somehow, maybe by
  // poking its count to the actually-initialized size at the point of failure.
  
  for (size_t i = 0, size = elements.size(); i < size; ++i) {
    SILValue eltPtr = basePtr;
    if (i != 0) {
      SILValue index = gen.B.createIntegerLiteral(loc,
                  SILType::getBuiltinWordType(gen.F.getASTContext()), i);
      eltPtr = gen.B.createIndexAddr(loc, basePtr, index);
    }
    ManagedValue v = elements[i];
    v = gen.emitSubstToOrigValue(loc, v, baseAbstraction, baseTy);
    v.forwardInto(gen, loc, eltPtr);
  }

  return emitEndVarargs(gen, loc, std::move(varargs));
}

RValue RValueEmitter::visitTupleExpr(TupleExpr *E, SGFContext C) {
  auto type = cast<TupleType>(E->getType()->getCanonicalType());

  // If we have an Initialization, emit the tuple elements into its elements.
  if (Initialization *I = C.getEmitInto()) {

    bool implodeTuple = false;

    if (auto Address = I->getAddressOrNull()) {
      if (isa<GlobalAddrInst>(Address) &&
          SGF.getTypeLowering(type).getLoweredType().isTrivial(SGF.SGM.M)) {
        // Implode tuples in initialization of globals if they are
        // of trivial types.
        implodeTuple = true;
      }
    }

    if (!implodeTuple && I->canSplitIntoTupleElements()) {
      SmallVector<InitializationPtr, 4> subInitializationBuf;
      auto subInitializations =
        I->splitIntoTupleElements(SGF, RegularLocation(E), type,
                                  subInitializationBuf);
      assert(subInitializations.size() == E->getElements().size() &&
             "initialization for tuple has wrong number of elements");
      for (unsigned i = 0, size = subInitializations.size(); i < size; ++i)
        SGF.emitExprInto(E->getElement(i), subInitializations[i].get());
      I->finishInitialization(SGF);
      return RValue();
    }
  }
    
  RValue result(type);
  for (Expr *elt : E->getElements())
    result.addElement(SGF.emitRValue(elt));
  return result;
}

namespace {

/// A helper function with context that tries to emit member refs of nominal
/// types avoiding the conservative lvalue logic.
class NominalTypeMemberRefRValueEmitter {
  using SelfTy = NominalTypeMemberRefRValueEmitter;

  /// The member ref expression we are emitting.
  MemberRefExpr *Expr;

  /// The passed in SGFContext.
  SGFContext Context;

  /// The typedecl of the base expression of the member ref expression.
  NominalTypeDecl *Base;

  /// The field of the member.
  VarDecl *Field;

public:

  NominalTypeMemberRefRValueEmitter(MemberRefExpr *Expr, SGFContext Context,
                                    NominalTypeDecl *Base)
    : Expr(Expr), Context(Context), Base(Base),
      Field(cast<VarDecl>(Expr->getMember().getDecl())) {}

  /// Emit the RValue.
  Optional<RValue> emit(SILGenFunction &SGF) {
    // If we don't have a class or a struct, bail.
    if (!isa<ClassDecl>(Base) && !isa<StructDecl>(Base))
      return None;

    // Check that we have a stored access strategy. If we don't bail.
    AccessStrategy strategy =
      Field->getAccessStrategy(Expr->getAccessSemantics(), AccessKind::Read);
    if (strategy != AccessStrategy::Storage)
      return None;

    if (isa<StructDecl>(Base))
      return emitStructDecl(SGF);
    assert(isa<ClassDecl>(Base) && "Expected class");
    return emitClassDecl(SGF);
  }

  NominalTypeMemberRefRValueEmitter(const SelfTy &) = delete;
  NominalTypeMemberRefRValueEmitter(SelfTy &&) = delete;
  ~NominalTypeMemberRefRValueEmitter() = default;

private:
  RValue emitStructDecl(SILGenFunction &SGF) {
    ManagedValue base =
      SGF.emitRValueAsSingleValue(Expr->getBase(),
                                  SGFContext::AllowImmediatePlusZero);
    CanType baseFormalType =
      Expr->getBase()->getType()->getCanonicalType();
    assert(baseFormalType->isMaterializable());

    RValue result =
      SGF.emitRValueForPropertyLoad(Expr, base, baseFormalType,
                                    Expr->isSuper(),
                                    Field,
                                    Expr->getMember().getSubstitutions(),
                                    Expr->getAccessSemantics(),
                                    Expr->getType(), Context);
    return result;
  }

  Optional<RValue> emitClassDecl(SILGenFunction &SGF) {
    // If guaranteed plus zero is not ok, we bail.
    if (!Context.isGuaranteedPlusZeroOk())
      return None;

    // If the field is not a let, bail. We need to use the lvalue logic.
    if (!Field->isLet())
      return None;

    // Ok, now we know that we are able to emit our base at guaranteed plus zero
    // emit base.
    ManagedValue base =
      SGF.emitRValueAsSingleValue(Expr->getBase(), Context);
    
    CanType baseFormalType =
      Expr->getBase()->getType()->getCanonicalType();
    assert(baseFormalType->isMaterializable());

    // And then emit our property using whether or not base is at +0 to
    // discriminate whether or not the base was guaranteed.
    RValue result =
        SGF.emitRValueForPropertyLoad(Expr, base, baseFormalType,
                                      Expr->isSuper(),
                                      Field,
                                      Expr->getMember().getSubstitutions(),
                                      Expr->getAccessSemantics(),
                                      Expr->getType(), Context,
                                      base.isPlusZeroRValueOrTrivial());
    return std::move(result);
  }
};

} // end anonymous namespace

RValue RValueEmitter::visitMemberRefExpr(MemberRefExpr *E, SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");

  if (isa<TypeDecl>(E->getMember().getDecl())) {
    // Emit the metatype for the associated type.
    visit(E->getBase());
    SILValue MT =
      SGF.B.createMetatype(E, SGF.getLoweredLoadableType(E->getType()));
    return RValue(SGF, E, ManagedValue::forUnmanaged(MT));
  }

  // If we have a nominal type decl as our base, try to emit the base rvalue's
  // member using special logic that will let us avoid extra retains
  // and releases.
  if (auto *N = E->getBase()->getType()->getNominalOrBoundGenericNominal())
    if (auto RV = NominalTypeMemberRefRValueEmitter(E, C, N).emit(SGF))
      return RValue(std::move(RV.getValue()));

  // Everything else should use the l-value logic.

  // Any writebacks for this access are tightly scoped.
  FormalEvaluationScope scope(SGF);

  LValue lv = SGF.emitLValue(E, AccessKind::Read);
  return SGF.emitLoadOfLValue(E, std::move(lv), C);
}

RValue RValueEmitter::visitDynamicMemberRefExpr(DynamicMemberRefExpr *E,
                                                SGFContext C) {
  return SGF.emitDynamicMemberRefExpr(E, C);
}

RValue RValueEmitter::
visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E, SGFContext C) {
  visit(E->getLHS());
  return visit(E->getRHS());
}

RValue RValueEmitter::visitSubscriptExpr(SubscriptExpr *E, SGFContext C) {
  // Any writebacks for this access are tightly scoped.
  FormalEvaluationScope scope(SGF);

  LValue lv = SGF.emitLValue(E, AccessKind::Read);
  return SGF.emitLoadOfLValue(E, std::move(lv), C);
}

RValue RValueEmitter::visitDynamicSubscriptExpr(
                                      DynamicSubscriptExpr *E, SGFContext C) {
  return SGF.emitDynamicSubscriptExpr(E, C);
}


RValue RValueEmitter::visitTupleElementExpr(TupleElementExpr *E,
                                            SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");
  
  // If our client is ok with a +0 result, then we can compute our base as +0
  // and return its element that way.  It would not be ok to reuse the Context's
  // address buffer though, since our base value will a different type than the
  // element.
  SGFContext SubContext = C.withFollowingProjection();
  
  return visit(E->getBase(), SubContext).extractElement(E->getFieldNumber());
}

RValue
SILGenFunction::emitApplyOfDefaultArgGenerator(SILLocation loc,
                                               ConcreteDeclRef defaultArgsOwner,
                                               unsigned destIndex,
                                               CanType resultType,
                                             AbstractionPattern origResultType,
                                               SGFContext C) {
  SILDeclRef generator 
    = SILDeclRef::getDefaultArgGenerator(defaultArgsOwner.getDecl(),
                                         destIndex);

  // TODO: Should apply the default arg generator's captures, but Sema doesn't
  // track them.
  
  auto fnRef = ManagedValue::forUnmanaged(emitGlobalFunctionRef(loc,generator));
  auto fnType = fnRef.getType().castTo<SILFunctionType>();
  auto substFnType = fnType->substGenericArgs(SGM.M,
                                          defaultArgsOwner.getSubstitutions());
  return emitApply(loc, fnRef, defaultArgsOwner.getSubstitutions(),
                   {}, substFnType,
                   origResultType, resultType,
                   ApplyOptions::None, None, None, C);
}

RValue SILGenFunction::emitApplyOfStoredPropertyInitializer(
    SILLocation loc,
    const PatternBindingEntry &entry,
    SubstitutionList subs,
    CanType resultType,
    AbstractionPattern origResultType,
    SGFContext C) {

  VarDecl *var = entry.getAnchoringVarDecl();
  SILDeclRef constant(var, SILDeclRef::Kind::StoredPropertyInitializer);
  auto fnRef = ManagedValue::forUnmanaged(emitGlobalFunctionRef(loc, constant));
  auto fnType = fnRef.getType().castTo<SILFunctionType>();

  auto substFnType = fnType->substGenericArgs(SGM.M, subs);

  return emitApply(loc, fnRef, subs, {},
                   substFnType,
                   origResultType,
                   resultType,
                   ApplyOptions::None, None, None, C);
}

static void emitTupleShuffleExprInto(RValueEmitter &emitter,
                                     TupleShuffleExpr *E,
                                     Initialization *outerTupleInit) {
  CanTupleType outerTuple = cast<TupleType>(E->getType()->getCanonicalType());
  auto outerFields = outerTuple->getElements();
  (void) outerFields;

  // Decompose the initialization.
  SmallVector<InitializationPtr, 4> outerInitsBuffer;
  auto outerInits =
    outerTupleInit->splitIntoTupleElements(emitter.SGF, RegularLocation(E),
                                           outerTuple, outerInitsBuffer);
  assert(outerInits.size() == outerFields.size() &&
         "initialization size does not match tuple size?!");

  // Map outer initializations into a tuple of inner initializations:
  //   - fill out the initialization elements with null
  TupleInitialization innerTupleInit;
  if (E->isSourceScalar()) {
    innerTupleInit.SubInitializations.push_back(nullptr);
  } else {
    CanTupleType innerTuple =
      cast<TupleType>(E->getSubExpr()->getType()->getCanonicalType());
    innerTupleInit.SubInitializations.resize(innerTuple->getNumElements());
  }

  // Map all the outer initializations to their appropriate targets.
  for (unsigned outerIndex = 0; outerIndex != outerInits.size(); outerIndex++) {
    auto innerMapping = E->getElementMapping()[outerIndex];
    assert(innerMapping >= 0 &&
           "non-argument tuple shuffle with default arguments or variadics?");
    innerTupleInit.SubInitializations[innerMapping] =
      std::move(outerInits[outerIndex]);
  }

#ifndef NDEBUG
  for (auto &innerInit : innerTupleInit.SubInitializations) {
    assert(innerInit != nullptr && "didn't map all inner elements");
  }
#endif

  // Emit the sub-expression into the tuple initialization we just built.
  if (E->isSourceScalar()) {
    emitter.SGF.emitExprInto(E->getSubExpr(),
                             innerTupleInit.SubInitializations[0].get());
  } else {
    emitter.SGF.emitExprInto(E->getSubExpr(), &innerTupleInit);
  }

  outerTupleInit->finishInitialization(emitter.SGF);
}

RValue RValueEmitter::visitTupleShuffleExpr(TupleShuffleExpr *E,
                                            SGFContext C) {
  // If we're emitting into an initialization, we can try shuffling the
  // elements of the initialization.
  if (Initialization *I = C.getEmitInto()) {
    // In Swift 3 mode, we might be stripping off labels from a
    // one-element tuple; the destination type is a ParenType in
    // that case.
    //
    // FIXME: Remove this eventually.
    if (I->canSplitIntoTupleElements() &&
        !(isa<ParenType>(E->getType().getPointer()) &&
          SGF.getASTContext().isSwiftVersion3())) {
      emitTupleShuffleExprInto(*this, E, I);
      return RValue();
    }
  }

  // Emit the sub-expression tuple and destructure it into elements.
  SmallVector<RValue, 4> elements;
  if (E->isSourceScalar()) {
    elements.push_back(visit(E->getSubExpr()));
  } else {
    visit(E->getSubExpr()).extractElements(elements);
  }
  
  // Prepare a new tuple to hold the shuffled result.
  RValue result(E->getType()->getCanonicalType());

  // In Swift 3 mode, we might be stripping off labels from a
  // one-element tuple; the destination type is a ParenType in
  // that case.
  //
  // FIXME: Remove this eventually.
  if (isa<ParenType>(E->getType().getPointer()) &&
      SGF.getASTContext().isSwiftVersion3()) {
    assert(E->getElementMapping().size() == 1);
    auto shuffleIndex = E->getElementMapping()[0];
    assert(shuffleIndex != TupleShuffleExpr::DefaultInitialize &&
           shuffleIndex != TupleShuffleExpr::CallerDefaultInitialize &&
           shuffleIndex != TupleShuffleExpr::Variadic &&
           "Only argument tuples can have default initializers & varargs");

    result.addElement(std::move(elements[shuffleIndex]));
    return result;
  }

  auto outerFields = E->getType()->castTo<TupleType>()->getElements();
  auto shuffleIndexIterator = E->getElementMapping().begin();
  auto shuffleIndexEnd = E->getElementMapping().end();
  (void)shuffleIndexEnd;
  for (auto &field : outerFields) {
    assert(shuffleIndexIterator != shuffleIndexEnd &&
           "ran out of shuffle indexes before running out of fields?!");
    int shuffleIndex = *shuffleIndexIterator++;
    
    assert(shuffleIndex != TupleShuffleExpr::DefaultInitialize &&
           shuffleIndex != TupleShuffleExpr::CallerDefaultInitialize &&
           "Only argument tuples can have default initializers & varargs");

    // If the shuffle index is Variadic, the argument sources are stored
    // separately.
    if (shuffleIndex != TupleShuffleExpr::Variadic) {
      // Map from a different tuple element.
      result.addElement(std::move(elements[shuffleIndex]));
      continue;
    }

    assert(field.isVararg() && "Cannot initialize nonvariadic element");
    
    // Okay, we have a varargs tuple element.  The separately-stored variadic
    // elements feed into the varargs portion of this, which is then
    // constructed into an Array through an informal protocol captured by the
    // InjectionFn in the TupleShuffleExpr.
    assert(E->getVarargsArrayTypeOrNull() &&
           "no injection type for varargs tuple?!");
    SmallVector<ManagedValue, 4> variadicValues;

    for (unsigned sourceField : E->getVariadicArgs()) {
      variadicValues.push_back(
                     std::move(elements[sourceField]).getAsSingleValue(SGF, E));
    }
    
    ManagedValue varargs = emitVarargs(SGF, E, field.getVarargBaseTy(),
                                       variadicValues,
                                       E->getVarargsArrayType());
    result.addElement(RValue(SGF, E, field.getType()->getCanonicalType(),
                             varargs));
    break;
  }
  
  return result;
}

SILValue SILGenFunction::emitMetatypeOfValue(SILLocation loc, Expr *baseExpr) {
  Type formalBaseType = baseExpr->getType()->getLValueOrInOutObjectType();
  CanType baseTy = formalBaseType->getCanonicalType();

  // For class, archetype, and protocol types, look up the dynamic metatype.
  if (baseTy.isAnyExistentialType()) {
    SILType metaTy = getLoweredLoadableType(
                                      CanExistentialMetatypeType::get(baseTy));
    auto base = emitRValueAsSingleValue(baseExpr,
                                  SGFContext::AllowImmediatePlusZero).getValue();
    return B.createExistentialMetatype(loc, metaTy, base);
  }

  SILType metaTy = getLoweredLoadableType(CanMetatypeType::get(baseTy));
  // If the lowered metatype has a thick representation, we need to derive it
  // dynamically from the instance.
  if (metaTy.castTo<MetatypeType>()->getRepresentation()
          != MetatypeRepresentation::Thin) {
    auto base = emitRValueAsSingleValue(baseExpr,
                               SGFContext::AllowImmediatePlusZero).getValue();
    return B.createValueMetatype(loc, metaTy, base);
  }
  
  // Otherwise, ignore the base and return the static thin metatype.
  emitIgnoredExpr(baseExpr);
  return B.createMetatype(loc, metaTy);
}

RValue RValueEmitter::visitDynamicTypeExpr(DynamicTypeExpr *E, SGFContext C) {
  auto metatype = SGF.emitMetatypeOfValue(E, E->getBase());
  return RValue(SGF, E, ManagedValue::forUnmanaged(metatype));
}

RValue RValueEmitter::visitCaptureListExpr(CaptureListExpr *E, SGFContext C) {
  // Ensure that weak captures are in a separate scope.
  DebugScope scope(SGF, CleanupLocation(E));
  // ClosureExpr's evaluate their bound variables.
  for (auto capture : E->getCaptureList()) {
    SGF.visit(capture.Var);
    SGF.visit(capture.Init);
  }

  // Then they evaluate to their body.
  return visit(E->getClosureBody(), C);
}


RValue RValueEmitter::visitAbstractClosureExpr(AbstractClosureExpr *e,
                                               SGFContext C) {
  // Emit the closure body.
  SGF.SGM.emitClosure(e);

  SubstitutionList subs;
  if (e->getCaptureInfo().hasGenericParamCaptures())
    subs = SGF.getForwardingSubstitutions();

  // Generate the closure value (if any) for the closure expr's function
  // reference.
  auto refType = e->getType()->getCanonicalType();
  ManagedValue result = SGF.emitClosureValue(e, SILDeclRef(e),
                                             refType, subs);
  return RValue(SGF, e, refType, result);
}

RValue RValueEmitter::
visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                   SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

RValue RValueEmitter::
visitObjectLiteralExpr(ObjectLiteralExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

RValue RValueEmitter::
visitEditorPlaceholderExpr(EditorPlaceholderExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

RValue RValueEmitter::visitObjCSelectorExpr(ObjCSelectorExpr *e, SGFContext C) {
  SILType loweredSelectorTy = SGF.getLoweredType(e->getType());

  // Dig out the declaration of the Selector type.
  auto selectorDecl = e->getType()->getAs<StructType>()->getDecl();

  // Dig out the type of its pointer.
  Type selectorMemberTy;
  for (auto member : selectorDecl->getMembers()) {
    if (auto var = dyn_cast<VarDecl>(member)) {
      if (!var->isStatic() && var->hasStorage()) {
        selectorMemberTy = var->getInterfaceType()->getRValueType();
        break;
      }
    }
  }
  if (!selectorMemberTy) {
    SGF.SGM.diagnose(e, diag::objc_selector_malformed);
    return RValue(SGF, e, SGF.emitUndef(e, loweredSelectorTy));
  }

  // Form the selector string.
  llvm::SmallString<64> selectorScratch;
  auto selectorString =
    e->getMethod()->getObjCSelector().getString(selectorScratch);

  // Create an Objective-C selector string literal.
  auto selectorLiteral =
    SGF.B.createStringLiteral(e, selectorString,
                              StringLiteralInst::Encoding::ObjCSelector);

  // Create the pointer struct from the raw pointer.
  SILType loweredPtrTy = SGF.getLoweredType(selectorMemberTy);
  auto ptrValue = SGF.B.createStruct(e, loweredPtrTy, { selectorLiteral });

  // Wrap that up in a Selector and return it.
  auto selectorValue = SGF.B.createStruct(e, loweredSelectorTy, { ptrValue });
  return RValue(SGF, e, ManagedValue::forUnmanaged(selectorValue));
}

RValue RValueEmitter::visitObjCKeyPathExpr(ObjCKeyPathExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

RValue RValueEmitter::
visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E, SGFContext C) {
  ASTContext &Ctx = SGF.getASTContext();
  SILType Ty = SGF.getLoweredLoadableType(E->getType());
  SourceLoc Loc;
  
  // If "overrideLocationForMagicIdentifiers" is set, then we use it as the
  // location point for these magic identifiers.
  if (SGF.overrideLocationForMagicIdentifiers)
    Loc = SGF.overrideLocationForMagicIdentifiers.getValue();
  else
    Loc = E->getStartLoc();
  
  switch (E->getKind()) {
  case MagicIdentifierLiteralExpr::File:
  case MagicIdentifierLiteralExpr::Function:
    return SGF.emitLiteral(E, C);
  case MagicIdentifierLiteralExpr::Line: {
    unsigned Value = 0;
    if (Loc.isValid())
      Value = Ctx.SourceMgr.getLineAndColumn(Loc).first;

    SILValue V = SGF.B.createIntegerLiteral(E, Ty, Value);
    return RValue(SGF, E, ManagedValue::forUnmanaged(V));
  }
  case MagicIdentifierLiteralExpr::Column: {
    unsigned Value = 0;
    if (Loc.isValid())
      Value = Ctx.SourceMgr.getLineAndColumn(Loc).second;

    SILValue V = SGF.B.createIntegerLiteral(E, Ty, Value);
    return RValue(SGF, E, ManagedValue::forUnmanaged(V));
  }

  case MagicIdentifierLiteralExpr::DSOHandle: {
    auto SILLoc = SILLocation(E);
    auto UnsafeRawPointer = SGF.getASTContext().getUnsafeRawPointerDecl();
    auto UnsafeRawPtrTy =
      SGF.getLoweredType(UnsafeRawPointer->getDeclaredInterfaceType());
    SILType BuiltinRawPtrTy = SILType::getRawPointerType(SGF.getASTContext());


    auto DSOGlobal = SGF.SGM.M.lookUpGlobalVariable("__dso_handle");
    if (!DSOGlobal)
      DSOGlobal = SILGlobalVariable::create(SGF.SGM.M,
                                            SILLinkage::HiddenExternal,
                                            IsNotFragile, "__dso_handle",
                                            BuiltinRawPtrTy);
    auto DSOAddr = SGF.B.createGlobalAddr(SILLoc, DSOGlobal);

    auto DSOPointer = SGF.B.createAddressToPointer(SILLoc, DSOAddr,
                                                   BuiltinRawPtrTy);

    auto UnsafeRawPtrStruct = SGF.B.createStruct(SILLoc, UnsafeRawPtrTy,
                                                 { DSOPointer });
    return RValue(SGF, E, ManagedValue::forUnmanaged(UnsafeRawPtrStruct));
  }
  }

  llvm_unreachable("Unhandled MagicIdentifierLiteralExpr in switch.");
}

RValue RValueEmitter::visitCollectionExpr(CollectionExpr *E, SGFContext C) {
  return visit(E->getSemanticExpr(), C);
}

/// Flattens one level of optional from a nested optional value.
static ManagedValue flattenOptional(SILGenFunction &SGF, SILLocation loc,
                                    ManagedValue optVal) {
  // FIXME: Largely copied from SILGenFunction::emitOptionalToOptional.
  auto contBB = SGF.createBasicBlock();
  auto isNotPresentBB = SGF.createBasicBlock();
  auto isPresentBB = SGF.createBasicBlock();

  SILType resultTy = optVal.getType().getAnyOptionalObjectType();
  auto &resultTL = SGF.getTypeLowering(resultTy);
  assert(resultTy.getSwiftRValueType().getAnyOptionalObjectType() &&
         "input was not a nested optional value");

  // If the result is address-only, we need to return something in memory,
  // otherwise the result is the BBArgument in the merge point.
  SILValue result;
  if (resultTL.isAddressOnly())
    result = SGF.emitTemporaryAllocation(loc, resultTy);
  else
    result = contBB->createPHIArgument(resultTy, ValueOwnershipKind::Owned);

  // Branch on whether the input is optional, this doesn't consume the value.
  auto isPresent = SGF.emitDoesOptionalHaveValue(loc, optVal.getValue());
  SGF.B.createCondBranch(loc, isPresent, isPresentBB, isNotPresentBB);

  // If it's present, apply the recursive transformation to the value.
  SGF.B.emitBlock(isPresentBB);
  SILValue branchArg;
  {
    // Don't allow cleanups to escape the conditional block.
    FullExpr presentScope(SGF.Cleanups, CleanupLocation::get(loc));

    // Pull the value out.  This will load if the value is not address-only.
    auto &inputTL = SGF.getTypeLowering(optVal.getType());
    auto resultValue = SGF.emitUncheckedGetOptionalValueFrom(loc, optVal,
                                                             inputTL);

    // Inject that into the result type if the result is address-only.
    if (resultTL.isAddressOnly())
      resultValue.forwardInto(SGF, loc, result);
    else
      branchArg = resultValue.forward(SGF);
  }
  if (branchArg)
    SGF.B.createBranch(loc, contBB, branchArg);
  else
    SGF.B.createBranch(loc, contBB);

  // If it's not present, inject 'nothing' into the result.
  SGF.B.emitBlock(isNotPresentBB);
  if (resultTL.isAddressOnly()) {
    SGF.emitInjectOptionalNothingInto(loc, result, resultTL);
    SGF.B.createBranch(loc, contBB);
  } else {
    branchArg = SGF.getOptionalNoneValue(loc, resultTL);
    SGF.B.createBranch(loc, contBB, branchArg);
  }

  // Continue.
  SGF.B.emitBlock(contBB);
  if (resultTL.isAddressOnly())
    return SGF.emitManagedBufferWithCleanup(result, resultTL);

  return SGF.emitManagedRValueWithCleanup(result, resultTL);
}

static ManagedValue
computeNewSelfForRebindSelfInConstructorExpr(SILGenFunction &SGF,
                                             RebindSelfInConstructorExpr *E) {
  // Get newSelf, forward the cleanup for newSelf and clean everything else
  // up.
  FormalEvaluationScope Scope(SGF);
  ManagedValue newSelfWithCleanup =
      SGF.emitRValueAsSingleValue(E->getSubExpr());

  SGF.InitDelegationSelf = ManagedValue();
  SGF.SuperInitDelegationSelf = ManagedValue();
  SGF.InitDelegationLoc.reset();
  return newSelfWithCleanup;
}

RValue RValueEmitter::visitRebindSelfInConstructorExpr(
                                RebindSelfInConstructorExpr *E, SGFContext C) {
  auto selfDecl = E->getSelf();
  auto ctorDecl = cast<ConstructorDecl>(selfDecl->getDeclContext());
  auto selfTy = selfDecl->getType()->getInOutObjectType();
  
  auto newSelfTy = E->getSubExpr()->getType();
  OptionalTypeKind failability;
  if (auto objTy = newSelfTy->getAnyOptionalObjectType(failability))
    newSelfTy = objTy;

  // "try? self.init()" can give us two levels of optional if the initializer
  // we delegate to is failable.
  OptionalTypeKind extraFailability;
  if (auto objTy = newSelfTy->getAnyOptionalObjectType(extraFailability))
    newSelfTy = objTy;

  bool requiresDowncast = !newSelfTy->isEqual(selfTy);

  // The subexpression consumes the current 'self' binding.
  assert(SGF.SelfInitDelegationState == SILGenFunction::NormalSelf
         && "already doing something funky with self?!");
  SGF.SelfInitDelegationState = SILGenFunction::WillSharedBorrowSelf;
  SGF.InitDelegationLoc.emplace(E);

  // Emit the subexpression, computing new self. New self is always returned at
  // +1.
  ManagedValue newSelf = computeNewSelfForRebindSelfInConstructorExpr(SGF, E);

  // We know that self is a box, so get its address.
  SILValue selfAddr =
    SGF.emitLValueForDecl(E, selfDecl, selfTy->getCanonicalType(),
                          AccessKind::Write).getLValueAddress();

  // Handle a nested optional case (see above).
  if (extraFailability != OTK_None)
    newSelf = flattenOptional(SGF, E, newSelf);

  // If both the delegated-to initializer and our enclosing initializer can
  // fail, deal with the failure.
  if (failability != OTK_None && ctorDecl->getFailability() != OTK_None) {
    SILBasicBlock *someBB = SGF.createBasicBlock();

    auto hasValue = SGF.emitDoesOptionalHaveValue(E, newSelf.getValue());

    assert(SGF.FailDest.isValid() && "too big to fail");

    // On the failure case, we don't need to clean up the 'self' returned
    // by the call to the other constructor, since we know it is nil and
    // therefore dynamically trivial.
    if (newSelf.getCleanup().isValid())
      SGF.Cleanups.setCleanupState(newSelf.getCleanup(),
                                   CleanupState::Dormant);
    auto noneBB = SGF.Cleanups.emitBlockForCleanups(SGF.FailDest, E);
    if (newSelf.getCleanup().isValid())
      SGF.Cleanups.setCleanupState(newSelf.getCleanup(),
                                   CleanupState::Active);

    SGF.B.createCondBranch(E, hasValue, someBB, noneBB);

    // Otherwise, project out the value and carry on.
    SGF.B.emitBlock(someBB);

    // If the current constructor is not failable, force out the value.
    newSelf = SGF.emitUncheckedGetOptionalValueFrom(E, newSelf,
                                    SGF.getTypeLowering(newSelf.getType()),
                                                    SGFContext());
  }
  
  // If we called a constructor that requires a downcast, perform the downcast.
  if (requiresDowncast) {
    assert(newSelf.getType().isObject() &&
           newSelf.getType().hasReferenceSemantics() &&
           "ctor type mismatch for non-reference type?!");
    CleanupHandle newSelfCleanup = newSelf.getCleanup();

    SILValue newSelfValue;
    auto destTy = SGF.getLoweredLoadableType(
                    E->getSelf()->getType()->getInOutObjectType());

    // Assume that the returned 'self' is the appropriate subclass
    // type (or a derived class thereof). Only Objective-C classes can
    // violate this assumption.
    newSelfValue = SGF.B.createUncheckedRefCast(E, newSelf.getValue(),
                                                destTy);
    newSelf = ManagedValue(newSelfValue, newSelfCleanup);
  }

  // Forward or assign into the box depending on whether we actually consumed
  // 'self'.
  switch (SGF.SelfInitDelegationState) {
  case SILGenFunction::NormalSelf:
    llvm_unreachable("self isn't normal in a constructor delegation");

  case SILGenFunction::WillSharedBorrowSelf:
    // We did not perform any borrow of self, exclusive or shared. This means
    // that old self is still located in the relevant box. This will ensure that
    // old self is destroyed.
    newSelf.assignInto(SGF, E, selfAddr);
    break;

  case SILGenFunction::DidSharedBorrowSelf:
    // We performed a shared borrow of self. This means that old self is still
    // located in the self box. Perform an assign to destroy old self.
    newSelf.assignInto(SGF, E, selfAddr);
    break;

  case SILGenFunction::WillExclusiveBorrowSelf:
    llvm_unreachable("Should never have newSelf without finishing an exclusive "
                     "borrow scope");

  case SILGenFunction::DidExclusiveBorrowSelf:
    // We performed an exclusive borrow of self and have a new value to
    // writeback. Writeback the self value into the now empty box.
    newSelf.forwardInto(SGF, E, selfAddr);
    break;
  }

  SGF.SelfInitDelegationState = SILGenFunction::NormalSelf;
  SGF.InitDelegationSelf = ManagedValue();

  // If we are using Objective-C allocation, the caller can return
  // nil. When this happens with an explicitly-written super.init or
  // self.init invocation, return early if we did get nil.
  //
  // TODO: Remove this when failable initializers are fully implemented.
  auto classDecl = selfTy->getClassOrBoundGenericClass();
  if (classDecl && !E->getSubExpr()->isImplicit() &&
      usesObjCAllocator(classDecl)) {
    // Check whether the new self is null.
    SILValue isNonnullSelf = SGF.B.createIsNonnull(E, newSelf.getValue());
    Condition cond = SGF.emitCondition(isNonnullSelf, E, 
                                       /*hasFalseCode=*/false,
                                       /*invertValue=*/true,
                                       { });

    // If self is null, branch to the epilog.
    cond.enterTrue(SGF);
    SGF.Cleanups.emitBranchAndCleanups(SGF.ReturnDest, E, { });
    cond.exitTrue(SGF);

    cond.complete(SGF);
  }

  return SGF.emitEmptyTupleRValue(E, C);
}

static bool isVerbatimNullableTypeInC(SILModule &M, Type ty) {
  ty = ty->getLValueOrInOutObjectType()->getReferenceStorageReferent();

  // Class instances, and @objc existentials are all nullable.
  if (ty->hasReferenceSemantics()) {
    // So are blocks, but we usually bridge them to Swift closures before we get
    // a chance to check for optional promotion, so we're already screwed if
    // an API lies about nullability.
    if (auto fnTy = ty->getAs<AnyFunctionType>()) {
      switch (fnTy->getRepresentation()) {
      // Carried verbatim from C.
      case FunctionTypeRepresentation::Block:
      case FunctionTypeRepresentation::CFunctionPointer:
        return true;
      // Was already bridged.
      case FunctionTypeRepresentation::Swift:
      case FunctionTypeRepresentation::Thin:
        return false;
      }
    }
    return true;
  }

  // Other types like UnsafePointer can also be nullable.
  const DeclContext *DC = M.getAssociatedContext();
  if (!DC)
    DC = M.getSwiftModule();
  ty = OptionalType::get(ty);
  return ty->isTriviallyRepresentableIn(ForeignLanguage::C, DC);
}

/// Determine whether the given declaration returns a non-optional object that
/// might actually be nil.
///
/// This is an awful hack that makes it possible to work around several kinds
/// of problems:
///   - initializers currently cannot fail, so they always return non-optional.
///   - an Objective-C method might have been annotated to state (incorrectly)
///     that it returns a non-optional object
///   - an Objective-C property might be annotated to state (incorrectly) that
///     it is non-optional
static bool mayLieAboutNonOptionalReturn(SILModule &M,
                                         ValueDecl *decl) {
  // Any Objective-C initializer, because failure propagates from any
  // initializer written in Objective-C (and there's no way to tell).
  if (auto constructor = dyn_cast<ConstructorDecl>(decl)) {
    return constructor->isObjC();
  }

  // Functions that return non-optional reference type and were imported from
  // Objective-C.
  if (auto func = dyn_cast<FuncDecl>(decl)) {
    assert((func->getResultInterfaceType()->hasTypeParameter()
            || isVerbatimNullableTypeInC(M, func->getResultInterfaceType()))
           && "func's result type is not nullable?!");
    return func->hasClangNode();
  }

  // Computed properties of non-optional reference type that were imported from
  // Objective-C.
  if (auto var = dyn_cast<VarDecl>(decl)) {
#ifndef NDEBUG
    auto type = var->getInterfaceType();
    assert((type->hasTypeParameter()
            || isVerbatimNullableTypeInC(M, type->getReferenceStorageReferent()))
           && "property's result type is not nullable?!");
#endif
    return var->hasClangNode();
  }

  // Subscripts of non-optional reference type that were imported from
  // Objective-C.
  if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
    assert((subscript->getElementInterfaceType()->hasTypeParameter()
            || isVerbatimNullableTypeInC(M, subscript->getElementInterfaceType()))
           && "subscript's result type is not nullable?!");
    return subscript->hasClangNode();
  }
  return false;
}

/// Determine whether the given expression returns a non-optional object that
/// might actually be nil.
///
/// This is an awful hack that makes it possible to work around several kinds
/// of problems:
///   - an Objective-C method might have been annotated to state (incorrectly)
///     that it returns a non-optional object
///   - an Objective-C property might be annotated to state (incorrectly) that
///     it is non-optional
static bool mayLieAboutNonOptionalReturn(SILModule &M, Expr *expr) {
  expr = expr->getSemanticsProvidingExpr();

  // An application that produces a reference type, which we look through to
  // get the function we're calling.
  if (auto apply = dyn_cast<ApplyExpr>(expr)) {
    // The result has to be a nullable type.
    if (!isVerbatimNullableTypeInC(M, apply->getType()))
      return false;
    
    auto getFuncDeclFromDynamicMemberLookup = [&](Expr *expr) -> FuncDecl * {
      if (auto open = dyn_cast<OpenExistentialExpr>(expr))
        expr = open->getSubExpr();
      
      if (auto memberRef = dyn_cast<DynamicMemberRefExpr>(expr))
        return dyn_cast<FuncDecl>(memberRef->getMember().getDecl());
      return nullptr;
    };
    
    // The function should come from C, being either an ObjC function or method
    // or having a C-derived convention.
    ValueDecl *method = nullptr;
    if (auto selfApply = dyn_cast<ApplyExpr>(apply->getFn())) {
      if (auto methodRef = dyn_cast<DeclRefExpr>(selfApply->getFn())) {
        method = methodRef->getDecl();
      }
    } else if (auto force = dyn_cast<ForceValueExpr>(apply->getFn())) {
      method = getFuncDeclFromDynamicMemberLookup(force->getSubExpr());
    } else if (auto bind = dyn_cast<BindOptionalExpr>(apply->getFn())) {
      method = getFuncDeclFromDynamicMemberLookup(bind->getSubExpr());
    } else if (auto fnRef = dyn_cast<DeclRefExpr>(apply->getFn())) {
      // Only consider a full application of a method. Partial applications
      // never lie.
      if (auto func = dyn_cast<AbstractFunctionDecl>(fnRef->getDecl()))
        if (func->getParameterLists().size() == 1)
          method = fnRef->getDecl();
    }
    if (method && mayLieAboutNonOptionalReturn(M, method))
      return true;
    
    auto convention = apply->getFn()->getType()->castTo<AnyFunctionType>()
      ->getRepresentation();
    
    switch (convention) {
    case FunctionTypeRepresentation::Block:
    case FunctionTypeRepresentation::CFunctionPointer:
      return true;
    case FunctionTypeRepresentation::Swift:
    case FunctionTypeRepresentation::Thin:
      return false;
    }
  }

  // A load.
  if (auto load = dyn_cast<LoadExpr>(expr)) {
    return mayLieAboutNonOptionalReturn(M, load->getSubExpr());
  }

  // A reference to a member property.
  if (auto member = dyn_cast<MemberRefExpr>(expr)) {
    return isVerbatimNullableTypeInC(M, member->getType()) &&
      mayLieAboutNonOptionalReturn(M, member->getMember().getDecl());
  }

  // A reference to a subscript.
  if (auto subscript = dyn_cast<SubscriptExpr>(expr)) {
    return isVerbatimNullableTypeInC(M, subscript->getType()) &&
      mayLieAboutNonOptionalReturn(M, subscript->getDecl().getDecl());
  }

  // A reference to a member property found via dynamic lookup.
  if (auto member = dyn_cast<DynamicMemberRefExpr>(expr)) {
    return isVerbatimNullableTypeInC(M, member->getType()) &&
      mayLieAboutNonOptionalReturn(M, member->getMember().getDecl());
  }

  // A reference to a subscript found via dynamic lookup.
  if (auto subscript = dyn_cast<DynamicSubscriptExpr>(expr)) {
    return isVerbatimNullableTypeInC(M, subscript->getType()) &&
      mayLieAboutNonOptionalReturn(M, subscript->getMember().getDecl());
  }

  return false;
}

RValue RValueEmitter::visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E,
                                                  SGFContext C) {
  // This is an awful hack. When the source expression might produce a
  // non-optional reference that could legitimated be nil, such as with an
  // initializer, allow this workaround to capture that nil:
  //
  //   let x: NSFoo? = NSFoo(potentiallyFailingInit: x)
  //
  // However, our optimizer is smart enough now to recognize that an initializer
  // can "never" produce nil, and will optimize away any attempts to check the
  // resulting optional for nil. As a special case, when we're injecting the
  // result of an ObjC constructor into an optional, do it using an unchecked
  // bitcast, which is opaque to the optimizer.
  if (mayLieAboutNonOptionalReturn(SGF.SGM.M, E->getSubExpr())) {
    auto result = SGF.emitRValueAsSingleValue(E->getSubExpr());
    auto optType = SGF.getLoweredLoadableType(E->getType());
    SILValue bitcast = SGF.B.createUncheckedBitCast(E, result.getValue(),
                                                    optType);
    ManagedValue bitcastMV = ManagedValue(bitcast, result.getCleanup());
    return RValue(SGF, E, bitcastMV);
  }

  OptionalTypeKind OTK;
  E->getType()->getAnyOptionalObjectType(OTK);
  assert(OTK != OTK_None);

  auto someDecl = SGF.getASTContext().getOptionalSomeDecl(OTK);

  ManagedValue result = SGF.emitInjectEnum(E, ArgumentSource(E->getSubExpr()),
                                           SGF.getLoweredType(E->getType()),
                                           someDecl, C);
  if (result.isInContext())
    return RValue();
  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitLValueToPointerExpr(LValueToPointerExpr *E,
                                               SGFContext C) {
  LValue lv = SGF.emitLValue(E->getSubExpr(), AccessKind::ReadWrite);
  SILValue address = SGF.emitAddressOfLValue(E->getSubExpr(),
                                             std::move(lv),
                                             AccessKind::ReadWrite)
    .getUnmanagedValue();
  // TODO: Reabstract the lvalue to match the abstraction level expected by
  // the inout address conversion's InOutType. For now, just report cases where
  // we would need a reabstraction as unsupported.
  SILType abstractedTy
    = SGF.getLoweredType(AbstractionPattern(E->getAbstractionPatternType()),
                         E->getSubExpr()->getType()->getLValueOrInOutObjectType());
  if (address->getType().getObjectType() != abstractedTy)
    SGF.SGM.diagnose(E, diag::not_implemented,
                     "abstraction difference in inout conversion");
  
  SILValue ptr = SGF.B.createAddressToPointer(E, address,
                              SILType::getRawPointerType(SGF.getASTContext()));
  return RValue(SGF, E, ManagedValue::forUnmanaged(ptr));
}

RValue RValueEmitter::visitClassMetatypeToObjectExpr(
                                                   ClassMetatypeToObjectExpr *E,
                                                   SGFContext C) {
  ManagedValue v = SGF.emitRValueAsSingleValue(E->getSubExpr());
  SILType resultTy = SGF.getLoweredLoadableType(E->getType());
  return RValue(SGF, E, SGF.emitClassMetatypeToObject(E, v, resultTy));
}

RValue RValueEmitter::visitExistentialMetatypeToObjectExpr(
                                             ExistentialMetatypeToObjectExpr *E,
                                             SGFContext C) {
  ManagedValue v = SGF.emitRValueAsSingleValue(E->getSubExpr());
  SILType resultTy = SGF.getLoweredLoadableType(E->getType());
  return RValue(SGF, E, SGF.emitExistentialMetatypeToObject(E, v, resultTy));
}

RValue RValueEmitter::visitProtocolMetatypeToObjectExpr(
                                                ProtocolMetatypeToObjectExpr *E,
                                                SGFContext C) {
  SGF.emitIgnoredExpr(E->getSubExpr());
  CanType inputTy = E->getSubExpr()->getType()->getCanonicalType();
  SILType resultTy = SGF.getLoweredLoadableType(E->getType());

  ManagedValue v = SGF.emitProtocolMetatypeToObject(E, inputTy, resultTy);
  return RValue(SGF, E, v);
}

RValue RValueEmitter::visitIfExpr(IfExpr *E, SGFContext C) {
  auto &lowering = SGF.getTypeLowering(E->getType());
  
  if (lowering.isLoadable()) {
    // If the result is loadable, emit each branch and forward its result
    // into the destination block argument.
    
    // FIXME: We could avoid imploding and reexploding tuples here.
    Condition cond = SGF.emitCondition(E->getCondExpr(),
                                       /*hasFalse*/ true,
                                       /*invertCondition*/ false,
                                       SGF.getLoweredType(E->getType()));
    
    cond.enterTrue(SGF);
    SGF.emitProfilerIncrement(E->getThenExpr());
    SILValue trueValue;
    {
      auto TE = E->getThenExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(TE));
      trueValue = visit(TE).forwardAsSingleValue(SGF, TE);
    }
    cond.exitTrue(SGF, trueValue);
    
    cond.enterFalse(SGF);
    SILValue falseValue;
    {
      auto EE = E->getElseExpr();
      FullExpr falseScope(SGF.Cleanups, CleanupLocation(EE));
      falseValue = visit(EE).forwardAsSingleValue(SGF, EE);
    }
    cond.exitFalse(SGF, falseValue);
    
    SILBasicBlock *cont = cond.complete(SGF);
    assert(cont && "no continuation block for if expr?!");

    SILValue result = cont->args_begin()[0];

    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(result));
  } else {
    // If the result is address-only, emit the result into a common stack buffer
    // that dominates both branches.
    SILValue resultAddr = SGF.getBufferForExprResult(
                                               E, lowering.getLoweredType(), C);
    
    Condition cond = SGF.emitCondition(E->getCondExpr(),
                                       /*hasFalse*/ true,
                                       /*invertCondition*/ false);
    cond.enterTrue(SGF);
    SGF.emitProfilerIncrement(E->getThenExpr());
    {
      auto TE = E->getThenExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(TE));
      KnownAddressInitialization init(resultAddr);
      SGF.emitExprInto(TE, &init);
    }
    cond.exitTrue(SGF);
    
    cond.enterFalse(SGF);
    {
      auto EE = E->getElseExpr();
      FullExpr trueScope(SGF.Cleanups, CleanupLocation(EE));
      KnownAddressInitialization init(resultAddr);
      SGF.emitExprInto(EE, &init);
    }
    cond.exitFalse(SGF);
    
    cond.complete(SGF);

    return RValue(SGF, E,
                  SGF.manageBufferForExprResult(resultAddr, lowering, C));
  }
}

RValue SILGenFunction::emitEmptyTupleRValue(SILLocation loc,
                                            SGFContext C) {
  return RValue(CanType(TupleType::getEmpty(F.getASTContext())));
}

namespace {
  /// A visitor for creating a flattened list of LValues from a
  /// tuple-of-lvalues expression.
  ///
  /// Note that we can have tuples down to arbitrary depths in the
  /// type, but every branch should lead to an l-value otherwise.
  class TupleLValueEmitter
      : public Lowering::ExprVisitor<TupleLValueEmitter> {
    SILGenFunction &SGF;

    AccessKind TheAccessKind;

    /// A flattened list of l-values.
    SmallVectorImpl<Optional<LValue>> &Results;
  public:
    TupleLValueEmitter(SILGenFunction &SGF, AccessKind accessKind,
                       SmallVectorImpl<Optional<LValue>> &results)
      : SGF(SGF), TheAccessKind(accessKind), Results(results) {}

    // If the destination is a tuple, recursively destructure.
    void visitTupleExpr(TupleExpr *E) {
      assert(E->getType()->is<TupleType>());
      assert(!E->getType()->isMaterializable() || E->getType()->isVoid());
      for (auto &elt : E->getElements()) {
        visit(elt);
      }
    }

    // If the destination is '_', queue up a discard.
    void visitDiscardAssignmentExpr(DiscardAssignmentExpr *E) {
      Results.push_back(None);
    }

    // Otherwise, queue up a scalar assignment to an lvalue.
    void visitExpr(Expr *E) {
      assert(E->getType()->is<LValueType>());
      Results.push_back(SGF.emitLValue(E, TheAccessKind));
    }
  };

  /// A visitor for consuming tuples of l-values.
  class TupleLValueAssigner
      : public CanTypeVisitor<TupleLValueAssigner, void, RValue &&> {
    SILGenFunction &SGF;
    SILLocation AssignLoc;
    MutableArrayRef<Optional<LValue>> DestLVQueue;

    Optional<LValue> &&getNextDest() {
      assert(!DestLVQueue.empty());
      Optional<LValue> &next = DestLVQueue.front();
      DestLVQueue = DestLVQueue.slice(1);
      return std::move(next);
    }

  public:
    TupleLValueAssigner(SILGenFunction &SGF, SILLocation assignLoc,
                        SmallVectorImpl<Optional<LValue>> &destLVs)
      : SGF(SGF), AssignLoc(assignLoc), DestLVQueue(destLVs) {}

    /// Top-level entrypoint.
    void emit(CanType destType, RValue &&src) {
      visitTupleType(cast<TupleType>(destType), std::move(src));
      assert(DestLVQueue.empty() && "didn't consume all l-values!");
    }

    // If the destination is a tuple, recursively destructure.
    void visitTupleType(CanTupleType destTupleType, RValue &&srcTuple) {
      // Break up the source r-value.
      SmallVector<RValue, 4> srcElts;
      std::move(srcTuple).extractElements(srcElts);

      // Consume source elements off the queue.
      unsigned eltIndex = 0;
      for (CanType destEltType : destTupleType.getElementTypes()) {
        visit(destEltType, std::move(srcElts[eltIndex++]));
      }
    }

    // Okay, otherwise we pull one destination off the queue.
    void visitType(CanType destType, RValue &&src) {
      assert(isa<LValueType>(destType));

      Optional<LValue> &&next = getNextDest();

      // If the destination is a discard, do nothing.
      if (!next.hasValue())
        return;

      // Otherwise, emit the scalar assignment.
      SGF.emitAssignToLValue(AssignLoc, std::move(src),
                             std::move(next.getValue()));
    }
  };
} // end anonymous namespace

/// Emit a simple assignment, i.e.
///
///   dest = src
///
/// The destination operand can be an arbitrarily-structured tuple of
/// l-values.
static void emitSimpleAssignment(SILGenFunction &SGF, SILLocation loc,
                                 Expr *dest, Expr *src) {
  // Handle lvalue-to-lvalue assignments with a high-level copy_addr
  // instruction if possible.
  if (auto *srcLoad = dyn_cast<LoadExpr>(src)) {
    // Check that the two l-value expressions have the same type.
    // Compound l-values like (a,b) have tuple type, so this check
    // also prevents us from getting into that case.
    if (dest->getType()->isEqual(srcLoad->getSubExpr()->getType())) {
      assert(!dest->getType()->is<TupleType>());
      FormalEvaluationScope writeback(SGF);
      auto destLV = SGF.emitLValue(dest, AccessKind::Write);
      auto srcLV = SGF.emitLValue(srcLoad->getSubExpr(), AccessKind::Read);
      SGF.emitAssignLValueToLValue(loc, std::move(srcLV), std::move(destLV));
      return;
    }
  }

  // Handle tuple destinations by destructuring them if present.
  CanType destType = dest->getType()->getCanonicalType();
  assert(!destType->isMaterializable() || destType->isVoid());

  // But avoid this in the common case.
  if (!isa<TupleType>(destType)) {
    // If we're assigning to a discard, just emit the operand as ignored.
    dest = dest->getSemanticsProvidingExpr();
    if (isa<DiscardAssignmentExpr>(dest)) {
      SGF.emitIgnoredExpr(src);
      return;
    }

    FormalEvaluationScope writeback(SGF);
    LValue destLV = SGF.emitLValue(dest, AccessKind::Write);
    RValue srcRV = SGF.emitRValue(src);
    SGF.emitAssignToLValue(loc, std::move(srcRV), std::move(destLV));
    return;
  }

  FormalEvaluationScope writeback(SGF);

  // Produce a flattened queue of LValues.
  SmallVector<Optional<LValue>, 4> destLVs;
  TupleLValueEmitter(SGF, AccessKind::Write, destLVs).visit(dest);

  // Emit the r-value.
  RValue srcRV = SGF.emitRValue(src);

  // Recurse on the type of the destination, pulling LValues as
  // needed from the queue we built up before.
  TupleLValueAssigner(SGF, loc, destLVs).emit(destType, std::move(srcRV));
}

RValue RValueEmitter::visitAssignExpr(AssignExpr *E, SGFContext C) {
  FullExpr scope(SGF.Cleanups, CleanupLocation(E));
  emitSimpleAssignment(SGF, E, E->getDest(), E->getSrc());
  return SGF.emitEmptyTupleRValue(E, C);
}

void SILGenFunction::emitBindOptional(SILLocation loc,
                                      ManagedValue optionalAddrOrValue,
                                      unsigned depth) {
  assert(depth < BindOptionalFailureDests.size());
  auto failureDest = BindOptionalFailureDests[BindOptionalFailureDests.size()
                                                - depth - 1];

  // Check whether the optional has a value.
  SILBasicBlock *hasValueBB = createBasicBlock();
  auto hasValue = emitDoesOptionalHaveValue(loc,optionalAddrOrValue.getValue());

  // If there is a cleanup for the optional value being tested, we can disable
  // it on the failure path.  We don't need to destroy it because we know that
  // on that path it is nil.
  if (optionalAddrOrValue.hasCleanup())
    Cleanups.setCleanupState(optionalAddrOrValue.getCleanup(),
                             CleanupState::Dormant);
    
  // If not, thread out through a bunch of cleanups.
  SILBasicBlock *hasNoValueBB = Cleanups.emitBlockForCleanups(failureDest, loc);
  B.createCondBranch(loc, hasValue, hasValueBB, hasNoValueBB);
  
  // If so, continue.
  B.emitBlock(hasValueBB);

  // Reenable the cleanup for the optional on the normal path.
  if (optionalAddrOrValue.hasCleanup())
    Cleanups.setCleanupState(optionalAddrOrValue.getCleanup(),
                             CleanupState::Active);
}

RValue RValueEmitter::visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C) {
  // Create a temporary of type Optional<T> if it is address-only.
  auto &optTL = SGF.getTypeLowering(E->getSubExpr()->getType());
  
  ManagedValue optValue;
  if (optTL.isLoadable()) {
    optValue = SGF.emitRValueAsSingleValue(E->getSubExpr());
  } else {
    auto temp = SGF.emitTemporary(E, optTL);
    optValue = temp->getManagedAddress();

    // Emit the operand into the temporary.
    SGF.emitExprInto(E->getSubExpr(), temp.get());

  }

  // Check to see whether the optional is present, if not, jump to the current
  // nil handler block.
  SGF.emitBindOptional(E, optValue, E->getDepth());

  // If we continued, get the value out as the result of the expression.
  auto resultValue = SGF.emitUncheckedGetOptionalValueFrom(E, optValue,
                                                           optTL, C);
  return RValue(SGF, E, resultValue);
}

namespace {
  /// A RAII object to save and restore BindOptionalFailureDest.
  class RestoreOptionalFailureDest {
    SILGenFunction &SGF;
#ifndef NDEBUG
    unsigned Depth;
#endif
  public:
    RestoreOptionalFailureDest(SILGenFunction &SGF, JumpDest &&dest)
      : SGF(SGF)
#ifndef NDEBUG
      , Depth(SGF.BindOptionalFailureDests.size())
#endif
    {
      SGF.BindOptionalFailureDests.push_back(std::move(dest));
    }
    ~RestoreOptionalFailureDest() {
      assert(SGF.BindOptionalFailureDests.size() == Depth + 1);
      SGF.BindOptionalFailureDests.pop_back();
    }
  };
} // end anonymous namespace

// Return an initialization address we can emit directly into.
static SILValue getAddressForInPlaceInitialization(const Initialization *I) {
  return I ? I->getAddressForInPlaceInitialization() : SILValue();
}

/// emitOptimizedOptionalEvaluation - Look for cases where we can short-circuit
/// evaluation of an OptionalEvaluationExpr by pattern matching the AST.
///
static bool emitOptimizedOptionalEvaluation(OptionalEvaluationExpr *E,
                                            SILValue &LoadableResult,
                                            Initialization *optInit,
                                            RValueEmitter &RVE) {
  auto &SGF = RVE.SGF;
  // It is a common occurrence to get conversions back and forth from T! to T?.
  // Peephole these by looking for a subexpression that is a BindOptionalExpr.
  // If we see one, we can produce a single instruction, which doesn't require
  // a CFG diamond.
  //
  // Check for:
  // (optional_evaluation_expr type='T?'
  //   (inject_into_optional type='T?'
  //     (bind_optional_expr type='T'
  //       (whatever type='T?' ...)
  auto *IIO = dyn_cast<InjectIntoOptionalExpr>(E->getSubExpr()
                                               ->getSemanticsProvidingExpr());
  if (!IIO) return false;
  
  // Make sure the bind is to the OptionalEvaluationExpr we're emitting.
  auto *BO = dyn_cast<BindOptionalExpr>(IIO->getSubExpr()
                                        ->getSemanticsProvidingExpr());
  if (!BO || BO->getDepth() != 0) return false;
  
  // If the subexpression type is exactly the same, then just peephole the
  // whole thing away.
  if (BO->getSubExpr()->getType()->isEqual(E->getType())) {
    if (optInit)
      SGF.emitExprInto(BO->getSubExpr(), optInit);
    else
      LoadableResult=SGF.emitRValueAsSingleValue(BO->getSubExpr()).forward(SGF);
    return true;
  }
  
  OptionalTypeKind Kind = OTK_None; (void)Kind;
  assert(BO->getSubExpr()->getType()->getAnyOptionalObjectType(Kind)
         ->isEqual(E->getType()->getAnyOptionalObjectType(Kind)));
  
  // If we're not emitting into memory (which happens both because the type is
  // address only or because we have a contextual memory location to
  // initialize).
  if (optInit == nullptr) {
    LoadableResult = SGF.emitRValueAsSingleValue(BO->getSubExpr()).forward(SGF);
    return true;
  }
  
  // If this is an address-only case, get the address of the buffer we want the
  // result in, then cast the address of it to the right type, then emit into
  // it.
  SILValue optAddr = getAddressForInPlaceInitialization(optInit);
  assert(optAddr && "Caller should have provided a buffer");
  
  KnownAddressInitialization subInit(optAddr);
  SGF.emitExprInto(BO->getSubExpr(), &subInit);
  optInit->finishInitialization(SGF);
  return true;
}

RValue RValueEmitter::visitOptionalEvaluationExpr(OptionalEvaluationExpr *E,
                                                  SGFContext C) {
  auto &optTL = SGF.getTypeLowering(E->getType());

  Initialization *optInit = C.getEmitInto();
  bool usingProvidedContext = optInit && optInit->isSingleBuffer();

  // Form the optional using address operations if the type is address-only or
  // if we already have an address to use.
  bool isByAddress = usingProvidedContext || optTL.isAddressOnly();
  
  std::unique_ptr<TemporaryInitialization> optTemp;
  if (!usingProvidedContext && isByAddress) {
    // Allocate the temporary for the Optional<T> if we didn't get one from the
    // context.  This needs to happen outside of the cleanups scope we're about
    // to push.
    optTemp = SGF.emitTemporary(E, optTL);
    optInit = optTemp.get();
  } else if (!usingProvidedContext) {
    // If the caller produced a context for us, but we can't use it, then don't.
    optInit = nullptr;
  }

  // Enter a cleanups scope.
  FullExpr scope(SGF.Cleanups, E);

  // Install a new optional-failure destination just outside of the
  // cleanups scope.
  SILBasicBlock *failureBB = SGF.createBasicBlock();
  RestoreOptionalFailureDest restoreFailureDest(SGF,
                    JumpDest(failureBB, SGF.Cleanups.getCleanupsDepth(), E));

  SILValue NormalArgument;
  if (emitOptimizedOptionalEvaluation(E, NormalArgument, optInit, *this)) {
    // Already emitted code for this.
  } else if (isByAddress) {
    // Emit the operand into the temporary.
    SGF.emitExprInto(E->getSubExpr(), optInit);
  } else {
    NormalArgument = SGF.emitRValueAsSingleValue(E->getSubExpr()).forward(SGF);
  }

  // We fell out of the normal result, which generated a T? as either
  // a scalar in NormalArgument or directly into optInit.

  // This concludes the conditional scope.
  scope.pop();

  
  // In the usual case, the code will have emitted one or more branches to the
  // failure block.  However, if the body is simple enough, we can end up with
  // no branches to the failureBB.  Detect this and simplify the generated code
  // if so.
  if (failureBB->pred_empty()) {
    // Remove the dead failureBB.
    failureBB->eraseFromParent();
    
    // The value we provide is the one we've already got.
    if (!isByAddress)
      return RValue(SGF, E,
                    SGF.emitManagedRValueWithCleanup(NormalArgument, optTL));

    // If we emitted into the provided context, we're done.
    if (usingProvidedContext)
      return RValue();

    return RValue(SGF, E, optTemp->getManagedAddress());
  }
  
  
  SILBasicBlock *contBB = SGF.createBasicBlock();

  // Branch to the continuation block.
  if (NormalArgument)
    SGF.B.createBranch(E, contBB, NormalArgument);
  else
    SGF.B.createBranch(E, contBB);

  // If control branched to the failure block, inject .None into the
  // result type.
  SGF.B.emitBlock(failureBB);

  if (isByAddress) {
    SGF.emitInjectOptionalNothingInto(E, optInit->getAddress(), optTL);
    SGF.B.createBranch(E, contBB);
  } else {
    auto branchArg = SGF.getOptionalNoneValue(E, optTL);
    SGF.B.createBranch(E, contBB, branchArg);
  }

  // Emit the continuation block.
  SGF.B.emitBlock(contBB);

  // If this was done in SSA registers, then the value is provided as an
  // argument to the block.
  if (!isByAddress) {
    auto arg = contBB->createPHIArgument(optTL.getLoweredType(),
                                         ValueOwnershipKind::Owned);
    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(arg, optTL));
  }
  
  // If we emitted into the provided context, we're done.
  if (usingProvidedContext)
    return RValue();
  
  assert(optTemp);
  return RValue(SGF, E, optTemp->getManagedAddress());
}

RValue RValueEmitter::visitForceValueExpr(ForceValueExpr *E, SGFContext C) {
  return emitForceValue(E, E->getSubExpr(), 0, C);
}

/// Emit an expression in a forced context.
///
/// \param loc - the location that is causing the force
/// \param E - the forced expression
/// \param numOptionalEvaluations - the number of enclosing
///   OptionalEvaluationExprs that we've opened.
RValue RValueEmitter::emitForceValue(SILLocation loc, Expr *E,
                                     unsigned numOptionalEvaluations,
                                     SGFContext C) {
  auto valueType = E->getType()->getAnyOptionalObjectType();
  assert(valueType);
  E = E->getSemanticsProvidingExpr();

  // If the subexpression is a conditional checked cast, emit an unconditional
  // cast, which drastically simplifies the generated SIL for something like:
  //
  //   (x as? Foo)!
  if (auto checkedCast = dyn_cast<ConditionalCheckedCastExpr>(E)) {
    return emitUnconditionalCheckedCast(SGF, loc, checkedCast->getSubExpr(),
                                        valueType, checkedCast->getCastKind(),
                                        C);
  }

  // If the subexpression is a monadic optional operation, peephole
  // the emission of the operation.
  if (auto eval = dyn_cast<OptionalEvaluationExpr>(E)) {
    CleanupLocation cleanupLoc = CleanupLocation::get(loc);
    SILBasicBlock *failureBB;
    JumpDest failureDest(cleanupLoc);

    // Set up an optional-failure scope (which cannot actually return).
    // We can just borrow the enclosing one if we're in a nested context.
    if (numOptionalEvaluations) {
      failureBB = nullptr; // remember that we did this
      failureDest = SGF.BindOptionalFailureDests.back();
    } else {
      failureBB = SGF.createBasicBlock(FunctionSection::Postmatter);
      failureDest = JumpDest(failureBB, SGF.Cleanups.getCleanupsDepth(),
                             cleanupLoc);
    }
    RestoreOptionalFailureDest restoreFailureDest(SGF, std::move(failureDest));
    RValue result = emitForceValue(loc, eval->getSubExpr(),
                                   numOptionalEvaluations + 1, C);

    // Emit the failure destination, but only if actually used.
    if (failureBB) {
      if (failureBB->pred_empty()) {
        SGF.eraseBasicBlock(failureBB);
      } else {
        SILGenBuilder failureBuilder(SGF, failureBB);
        failureBuilder.setTrackingList(SGF.getBuilder().getTrackingList());
        auto boolTy = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
        auto trueV = failureBuilder.createIntegerLiteral(loc, boolTy, 1);
        failureBuilder.createCondFail(loc, trueV);
        failureBuilder.createUnreachable(loc);
      }
    }

    return result;
  }

  // Handle injections.
  if (auto injection = dyn_cast<InjectIntoOptionalExpr>(E)) {
    auto subexpr = injection->getSubExpr()->getSemanticsProvidingExpr();

    // An injection of a bind is the idiom for a conversion between
    // optional types (e.g. ImplicitlyUnwrappedOptional<T> -> Optional<T>).
    // Handle it specially to avoid unnecessary control flow.
    if (auto bindOptional = dyn_cast<BindOptionalExpr>(subexpr)) {
      if (bindOptional->getDepth() < numOptionalEvaluations) {
        return emitForceValue(loc, bindOptional->getSubExpr(),
                              numOptionalEvaluations, C);
      }
    }

    // Otherwise, just emit the injected value directly into the result.
    return SGF.emitRValue(injection->getSubExpr(), C);
  }

  // Otherwise, emit the optional and force its value out.
  const TypeLowering &optTL = SGF.getTypeLowering(E->getType());
  ManagedValue opt = SGF.emitRValueAsSingleValue(E);
  ManagedValue V =
    SGF.emitCheckedGetOptionalValueFrom(loc, opt, optTL, C);
  return RValue(SGF, loc, valueType->getCanonicalType(), V);
}

void SILGenFunction::emitOpenExistentialExprImpl(
       OpenExistentialExpr *E,
       llvm::function_ref<void(Expr *)> emitSubExpr) {
  Optional<FormalEvaluationScope> writebackScope;

  // Emit the existential value.
  ManagedValue existentialValue;
  AccessKind accessKind;
  if (E->getExistentialValue()->getType()->is<LValueType>()) {
    // Create a writeback scope for the access to the existential lvalue.
    writebackScope.emplace(*this);

    accessKind = E->getExistentialValue()->getLValueAccessKind();
    existentialValue = emitAddressOfLValue(
                         E->getExistentialValue(),
                         emitLValue(E->getExistentialValue(), accessKind),
                         accessKind);
  } else {
    accessKind = AccessKind::Read;
    existentialValue = emitRValueAsSingleValue(
                         E->getExistentialValue(),
                         SGFContext::AllowGuaranteedPlusZero);
  }
  
  Type opaqueValueType = E->getOpaqueValue()->getType()->getRValueType();
  SILGenFunction::OpaqueValueState state = emitOpenExistential(
      E, existentialValue, CanArchetypeType(E->getOpenedArchetype()),
      getLoweredType(opaqueValueType), accessKind);

  // Register the opaque value for the projected existential.
  SILGenFunction::OpaqueValueRAII opaqueValueRAII(
                                    *this, E->getOpaqueValue(), state);

  emitSubExpr(E->getSubExpr());
}

RValue RValueEmitter::visitOpenExistentialExpr(OpenExistentialExpr *E,
                                               SGFContext C) {
  return SGF.emitOpenExistentialExpr<RValue>(E,
                                             [&](Expr *subExpr) -> RValue {
                                               return visit(subExpr, C);
                                             });
}

RValue RValueEmitter::visitMakeTemporarilyEscapableExpr(
    MakeTemporarilyEscapableExpr *E,
    SGFContext C) {
  // TODO: Some day we want to specialize the representation of nonescaping
  // closures to be POD and allow an arbitrary payload in their context word.
  // At that point, this operation would need to wrap the nonescaping closure
  // in an escaping stub, which we could dynamically check at the end of the
  // expression to verify it did not in fact escape at runtime. For now, to
  // get the syntax for withoutActuallyEscaping in place, this is a no-op.
  
  // Emit the closure and bind it to an opaque value for use in the
  // subexpression.
  auto closure = visit(E->getNonescapingClosureValue());
  SILGenFunction::OpaqueValueState opaqueValue{
    std::move(closure).getAsSingleValue(SGF, E),
    /*consumable*/ true,
    /*hasBeenConsumed*/ false,
  };
  
  SILGenFunction::OpaqueValueRAII pushOpaqueValue(SGF, E->getOpaqueValue(),
                                                  opaqueValue);
  return visit(E->getSubExpr(), C);
}

RValue RValueEmitter::visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C) {
  assert(SGF.OpaqueValues.count(E) && "Didn't bind OpaqueValueExpr");
  auto &entry = SGF.OpaqueValues[E];
  return RValue(SGF, E, SGF.manageOpaqueValue(entry, E, C));
}

ProtocolDecl *SILGenFunction::getPointerProtocol() {
  if (SGM.PointerProtocol)
    return *SGM.PointerProtocol;
  
  SmallVector<ValueDecl*, 1> lookup;
  getASTContext().lookupInSwiftModule("_Pointer", lookup);
  // FIXME: Should check for protocol in Sema
  assert(lookup.size() == 1 && "no _Pointer protocol");
  assert(isa<ProtocolDecl>(lookup[0]) && "_Pointer is not a protocol");
  SGM.PointerProtocol = cast<ProtocolDecl>(lookup[0]);
  return cast<ProtocolDecl>(lookup[0]);
}

namespace {
class AutoreleasingWritebackComponent : public LogicalPathComponent {
public:
  AutoreleasingWritebackComponent(LValueTypeData typeData)
    : LogicalPathComponent(typeData, AutoreleasingWritebackKind)
  {}
  
  std::unique_ptr<LogicalPathComponent>
  clone(SILGenFunction &gen, SILLocation l) const override {
    return std::unique_ptr<LogicalPathComponent>(
      new AutoreleasingWritebackComponent(getTypeData()));
  }

  AccessKind getBaseAccessKind(SILGenFunction &gen,
                               AccessKind kind) const override {
    return kind;
  }
  
  void set(SILGenFunction &gen, SILLocation loc,
           RValue &&value, ManagedValue base) && override {
    // Convert the value back to a +1 strong reference.
    auto unowned = std::move(value).getAsSingleValue(gen, loc).getUnmanagedValue();
    auto strongType = SILType::getPrimitiveObjectType(
              unowned->getType().castTo<UnmanagedStorageType>().getReferentType());
    auto owned = gen.B.createUnmanagedToRef(loc, unowned, strongType);
    auto ownedMV = gen.emitManagedRetain(loc, owned);
    
    // Reassign the +1 storage with it.
    ownedMV.assignInto(gen, loc, base.getUnmanagedValue());
  }
  
  RValue get(SILGenFunction &gen, SILLocation loc,
             ManagedValue base, SGFContext c) && override {
    FullExpr TightBorrowScope(gen.Cleanups, CleanupLocation::get(loc));

    // Load the value at +0.
    ManagedValue loadedBase = gen.B.createLoadBorrow(loc, base);

    // Convert it to unowned.
    auto refType = loadedBase.getType().getSwiftRValueType();
    auto unownedType = SILType::getPrimitiveObjectType(
                                        CanUnmanagedStorageType::get(refType));
    SILValue unowned = gen.B.createRefToUnmanaged(
        loc, loadedBase.getUnmanagedValue(), unownedType);

    // A reference type should never be exploded.
    return RValue::withPreExplodedElements(ManagedValue::forUnmanaged(unowned),
                                           refType);
  }

  /// Compare 'this' lvalue and the 'rhs' lvalue (which is guaranteed to have
  /// the same dynamic PathComponent type as the receiver) to see if they are
  /// identical.  If so, there is a conflicting writeback happening, so emit a
  /// diagnostic.
  void diagnoseWritebackConflict(LogicalPathComponent *RHS,
                                 SILLocation loc1, SILLocation loc2,
                                 SILGenFunction &gen) override {
    //      auto &rhs = (GetterSetterComponent&)*RHS;
  }

  void print(raw_ostream &OS) const override {
    OS << "AutoreleasingWritebackComponent()\n";
  }
};
} // end anonymous namespace

RValue RValueEmitter::visitInOutToPointerExpr(InOutToPointerExpr *E,
                                              SGFContext C) {
  // If we're converting on the behalf of an
  // AutoreleasingUnsafeMutablePointer, convert the lvalue to
  // unowned(unsafe), so we can point at +0 storage.
  PointerTypeKind pointerKind;
  Type elt = E->getType()->getAnyPointerElementType(pointerKind);
  assert(elt && "not a pointer");
  (void)elt;

  AccessKind accessKind =
    ((pointerKind == PTK_UnsafePointer || pointerKind == PTK_UnsafeRawPointer)
       ? AccessKind::Read : AccessKind::ReadWrite);

  // Get the original lvalue.
  LValue lv = SGF.emitLValue(cast<InOutExpr>(E->getSubExpr())->getSubExpr(),
                             accessKind);

  auto ptr = SGF.emitLValueToPointer(E, std::move(lv),
                                     E->getType()->getCanonicalType(),
                                     pointerKind, accessKind);
  return RValue(SGF, E, ptr);
}

/// Convert an l-value to a pointer type: unsafe, unsafe-mutable, or
/// autoreleasing-unsafe-mutable.
ManagedValue SILGenFunction::emitLValueToPointer(SILLocation loc,
                                                 LValue &&lv,
                                                 CanType pointerType,
                                                 PointerTypeKind pointerKind,
                                                 AccessKind accessKind) {
  // The incoming lvalue should be at the abstraction level of T in
  // Unsafe*Pointer<T>. Reabstract it if necessary.
  auto opaqueTy = AbstractionPattern::getOpaque();
  auto loweredTy = getLoweredType(opaqueTy, lv.getSubstFormalType());
  if (lv.getTypeOfRValue().getSwiftRValueType()
        != loweredTy.getSwiftRValueType()) {
    lv.addSubstToOrigComponent(opaqueTy, loweredTy);
  }
  switch (pointerKind) {
  case PTK_UnsafeMutablePointer:
  case PTK_UnsafePointer:
  case PTK_UnsafeMutableRawPointer:
  case PTK_UnsafeRawPointer:
    // +1 is fine.
    break;

  case PTK_AutoreleasingUnsafeMutablePointer: {
    // Set up a writeback through a +0 buffer.
    LValueTypeData typeData = lv.getTypeData();
    SILType rvalueType = SILType::getPrimitiveObjectType(
      CanUnmanagedStorageType::get(typeData.TypeOfRValue.getSwiftRValueType()));

    LValueTypeData unownedTypeData(
      AbstractionPattern(
        typeData.OrigFormalType.getGenericSignature(),
        CanUnmanagedStorageType::get(typeData.OrigFormalType.getType())),
      CanUnmanagedStorageType::get(typeData.SubstFormalType),
      rvalueType);
    lv.add<AutoreleasingWritebackComponent>(unownedTypeData);
    break;
  }
  }

  // Get the lvalue address as a raw pointer.
  SILValue address =
    emitAddressOfLValue(loc, std::move(lv), accessKind).getUnmanagedValue();
  address = B.createAddressToPointer(loc, address,
                               SILType::getRawPointerType(getASTContext()));
  
  // Disable nested writeback scopes for any calls evaluated during the
  // conversion intrinsic.
  InOutConversionScope scope(*this);
  
  // Invoke the conversion intrinsic.
  FuncDecl *converter =
    getASTContext().getConvertInOutToPointerArgument(nullptr);

  auto subMap = pointerType->getContextSubstitutionMap(SGM.M.getSwiftModule(),
                                                       getPointerProtocol());
  return emitApplyOfLibraryIntrinsic(loc, converter, subMap,
                                     ManagedValue::forUnmanaged(address),
                                     SGFContext())
           .getAsSingleValue(*this, loc);
}
RValue RValueEmitter::visitArrayToPointerExpr(ArrayToPointerExpr *E,
                                              SGFContext C) {
  FormalEvaluationScope writeback(SGF);

  auto &Ctx = SGF.getASTContext();
  FuncDecl *converter;
  ManagedValue orig;

  // Convert the array mutably if it's being passed inout.
  auto subExpr = E->getSubExpr();
  if (subExpr->getType()->is<InOutType>()) {
    converter = Ctx.getConvertMutableArrayToPointerArgument(nullptr);
    orig = SGF.emitAddressOfLValue(subExpr,
                               SGF.emitLValue(subExpr, AccessKind::ReadWrite),
                                   AccessKind::ReadWrite);
  } else {
    converter = Ctx.getConvertConstArrayToPointerArgument(nullptr);
    orig = SGF.emitRValueAsSingleValue(subExpr);
  }

  // Invoke the conversion intrinsic, which will produce an owner-pointer pair.
  auto *M = SGF.SGM.M.getSwiftModule();
  auto firstSubMap = subExpr->getType()->getInOutObjectType()
    ->getContextSubstitutionMap(
      M, Ctx.getArrayDecl());
  auto secondSubMap = E->getType()
    ->getContextSubstitutionMap(
      M, SGF.getPointerProtocol());

  auto *genericSig = converter->getGenericSignature();
  auto subMap =
    SubstitutionMap::combineSubstitutionMaps(firstSubMap,
                                             secondSubMap,
                                             CombineSubstitutionMaps::AtIndex,
                                             1, 0,
                                             genericSig);

  SmallVector<ManagedValue, 2> resultScalars;
  SGF.emitApplyOfLibraryIntrinsic(E, converter, subMap, orig, SGFContext())
    .getAll(resultScalars);
  assert(resultScalars.size() == 2);
  
  // The owner's already in its own cleanup.  Return the pointer.
  return RValue(SGF, E, resultScalars[1]);
}
RValue RValueEmitter::visitStringToPointerExpr(StringToPointerExpr *E,
                                               SGFContext C) {
  auto &Ctx = SGF.getASTContext();
  FuncDecl *converter = Ctx.getConvertConstStringToUTF8PointerArgument(nullptr);

  // Get the original value.
  ManagedValue orig = SGF.emitRValueAsSingleValue(E->getSubExpr());
  
  // Invoke the conversion intrinsic, which will produce an owner-pointer pair.
  auto subMap = E->getType()->getCanonicalType()
    ->getContextSubstitutionMap(SGF.SGM.M.getSwiftModule(),
                                SGF.getPointerProtocol());
  SmallVector<ManagedValue, 2> results;
  SGF.emitApplyOfLibraryIntrinsic(E, converter, subMap, orig, C).getAll(results);
  assert(results.size() == 2);
  
  // Implicitly leave the owner managed and return the pointer.
  // FIXME: should this be using mark_dependence?
  auto pointer = results[1];
  return RValue(SGF, E, pointer);
}
RValue RValueEmitter::visitPointerToPointerExpr(PointerToPointerExpr *E,
                                                SGFContext C) {
  auto &Ctx = SGF.getASTContext();
  auto converter = Ctx.getConvertPointerToPointerArgument(nullptr);

  // Get the original pointer value, abstracted to the converter function's
  // expected level.
  AbstractionPattern origTy(converter->getInterfaceType());
  origTy = origTy.getFunctionInputType();

  CanType inputTy = E->getSubExpr()->getType()->getCanonicalType();
  auto &origTL = SGF.getTypeLowering(origTy, inputTy);
  ManagedValue orig = SGF.emitRValueAsOrig(E->getSubExpr(), origTy, origTL);

  CanType outputTy = E->getType()->getCanonicalType();
  return SGF.emitPointerToPointer(E, orig, inputTy, outputTy, C);
}

RValue RValueEmitter::visitForeignObjectConversionExpr(
         ForeignObjectConversionExpr *E,
         SGFContext C) {
  // Get the original value.
  ManagedValue orig = SGF.emitRValueAsSingleValue(E->getSubExpr());
  ManagedValue result(SGF.B.createUncheckedRefCast(
                        E, orig.getValue(),
                        SGF.getLoweredType(E->getType())),
                      orig.getCleanup());
  return RValue(SGF, E, E->getType()->getCanonicalType(), result);
}

RValue RValueEmitter::visitUnevaluatedInstanceExpr(UnevaluatedInstanceExpr *E,
                                                   SGFContext C) {
  llvm_unreachable("unevaluated_instance expression can never be evaluated");
}

RValue SILGenFunction::emitRValue(Expr *E, SGFContext C) {
  assert(E->getType()->isMaterializable() &&
         "l-values must be emitted with emitLValue");
  return RValueEmitter(*this).visit(E, C);
}

// Evaluate the expression as an lvalue or rvalue, discarding the result.
void SILGenFunction::emitIgnoredExpr(Expr *E) {
  // If this is a tuple expression, recursively ignore its elements.
  // This may let us recursively avoid work.
  if (auto *TE = dyn_cast<TupleExpr>(E)) {
    for (auto *elt : TE->getElements())
      emitIgnoredExpr(elt);
    return;
  }
  
  // TODO: Could look through arbitrary implicit conversions that don't have
  // side effects, or through tuple shuffles, by emitting ignored default
  // arguments.
  
  FullExpr scope(Cleanups, CleanupLocation(E));
  if (!E->getType()->isMaterializable()) {
    // Emit the l-value, but don't perform an access.
    FormalEvaluationScope scope(*this);
    emitLValue(E, AccessKind::Read);
    return;
  }

  // If this is a load expression, we try hard not to actually do the load
  // (which could materialize a potentially expensive value with cleanups).
  if (auto *LE = dyn_cast<LoadExpr>(E)) {
    FormalEvaluationScope scope(*this);
    LValue lv = emitLValue(LE->getSubExpr(), AccessKind::Read);
    // If the lvalue is purely physical, then it won't have any side effects,
    // and we don't need to drill into it.
    if (lv.isPhysical())
      return;

    // If the last component is physical, then we just need to drill through
    // side effects in the lvalue, but don't need to perform the final load.
    if (lv.isLastComponentPhysical()) {
      emitAddressOfLValue(E, std::move(lv), AccessKind::Read);
      return;
    }

    // Otherwise, we must call the ultimate getter to get its potential side
    // effect.
    emitLoadOfLValue(E, std::move(lv), SGFContext::AllowImmediatePlusZero);
    return;
  }

  // Otherwise, emit the result (to get any side effects), but produce it at +0
  // if that allows simplification.
  emitRValue(E, SGFContext::AllowImmediatePlusZero);
}

/// Emit the given expression as an r-value, then (if it is a tuple), combine
/// it together into a single ManagedValue.
ManagedValue SILGenFunction::emitRValueAsSingleValue(Expr *E, SGFContext C) {
  RValue &&rv = emitRValue(E, C);
  if (rv.isUsed()) return ManagedValue::forInContext();
  return std::move(rv).getAsSingleValue(*this, E);
}

RValue SILGenFunction::emitUndefRValue(SILLocation loc, Type type) {
  return RValue(*this, loc, type->getCanonicalType(),
                emitUndef(loc, getLoweredType(type)));
}

ManagedValue SILGenFunction::emitUndef(SILLocation loc, Type type) {
  return emitUndef(loc, getLoweredType(type));
}

ManagedValue SILGenFunction::emitUndef(SILLocation loc, SILType type) {
  SILValue undef = SILUndef::get(type, SGM.M);
  return ManagedValue::forUnmanaged(undef);
}
