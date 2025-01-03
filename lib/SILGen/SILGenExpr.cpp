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

#include "ArgumentScope.h"
#include "ArgumentSource.h"
#include "Callee.h"
#include "Condition.h"
#include "Conversion.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "ResultPlan.h"
#include "SGFContext.h"
#include "SILGen.h"
#include "SILGenDynamicCast.h"
#include "SILGenFunctionBuilder.h"
#include "Scope.h"
#include "SwitchEnumBuilder.h"
#include "Varargs.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/DistributedDecl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/type_traits.h"
#include "swift/SIL/Consumption.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

#include "swift/AST/DiagnosticsSIL.h"

using namespace swift;
using namespace Lowering;

ManagedValue SILGenFunction::emitManagedCopy(SILLocation loc, SILValue v) {
  auto &lowering = getTypeLowering(v->getType());
  return emitManagedCopy(loc, v, lowering);
}

ManagedValue SILGenFunction::emitManagedCopy(SILLocation loc, SILValue v,
                                             const TypeLowering &lowering) {
  assert(lowering.getLoweredType() == v->getType());
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(v);
  if (v->getType().isObject() && v->getOwnershipKind() == OwnershipKind::None)
    return ManagedValue::forObjectRValueWithoutOwnership(v);
  assert((!lowering.isAddressOnly() || !silConv.useLoweredAddresses()) &&
         "cannot retain an unloadable type");

  v = lowering.emitCopyValue(B, loc, v);
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedFormalEvaluationCopy(SILLocation loc,
                                                             SILValue v) {
  auto &lowering = getTypeLowering(v->getType());
  return emitManagedFormalEvaluationCopy(loc, v, lowering);
}

ManagedValue
SILGenFunction::emitManagedFormalEvaluationCopy(SILLocation loc, SILValue v,
                                                const TypeLowering &lowering) {
  assert(lowering.getLoweredType() == v->getType());
  assert(isInFormalEvaluationScope() && "Must be in formal evaluation scope");
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(v);
  if (v->getType().isObject() && v->getOwnershipKind() == OwnershipKind::None)
    return ManagedValue::forObjectRValueWithoutOwnership(v);
  assert((!lowering.isAddressOnly() || !silConv.useLoweredAddresses()) &&
         "cannot retain an unloadable type");

  v = lowering.emitCopyValue(B, loc, v);
  return emitFormalAccessManagedRValueWithCleanup(loc, v);
}

ManagedValue SILGenFunction::emitManagedLoadCopy(SILLocation loc, SILValue v) {
  auto &lowering = getTypeLowering(v->getType());
  return emitManagedLoadCopy(loc, v, lowering);
}

ManagedValue SILGenFunction::emitManagedLoadCopy(SILLocation loc, SILValue v,
                                                 const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v->getType());
  v = lowering.emitLoadOfCopy(B, loc, v, IsNotTake);
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(v);
  if (v->getOwnershipKind() == OwnershipKind::None)
    return ManagedValue::forObjectRValueWithoutOwnership(v);
  assert((!lowering.isAddressOnly() || !silConv.useLoweredAddresses()) &&
         "cannot retain an unloadable type");
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedLoadBorrow(SILLocation loc,
                                                   SILValue v) {
  auto &lowering = getTypeLowering(v->getType());
  return emitManagedLoadBorrow(loc, v, lowering);
}

ManagedValue
SILGenFunction::emitManagedLoadBorrow(SILLocation loc, SILValue v,
                                      const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v->getType());
  if (lowering.isTrivial()) {
    v = lowering.emitLoadOfCopy(B, loc, v, IsNotTake);
    return ManagedValue::forObjectRValueWithoutOwnership(v);
  }

  assert((!lowering.isAddressOnly() || !silConv.useLoweredAddresses()) &&
         "cannot retain an unloadable type");
  auto *lbi = B.createLoadBorrow(loc, v);
  return emitManagedBorrowedRValueWithCleanup(v, lbi, lowering);
}

ManagedValue SILGenFunction::emitManagedStoreBorrow(SILLocation loc, SILValue v,
                                                    SILValue addr) {
  auto &lowering = getTypeLowering(v->getType());
  return emitManagedStoreBorrow(loc, v, addr, lowering);
}

ManagedValue SILGenFunction::emitManagedStoreBorrow(
    SILLocation loc, SILValue v, SILValue addr, const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() == v->getType());
  if (lowering.isTrivial() || v->getOwnershipKind() == OwnershipKind::None) {
    lowering.emitStore(B, loc, v, addr, StoreOwnershipQualifier::Trivial);
    return ManagedValue::forTrivialAddressRValue(addr);
  }
  assert((!lowering.isAddressOnly() || !silConv.useLoweredAddresses()) &&
         "cannot retain an unloadable type");
  auto *sbi = B.createStoreBorrow(loc, v, addr);
  Cleanups.pushCleanup<EndBorrowCleanup>(sbi);
  return ManagedValue::forBorrowedAddressRValue(sbi);
}

ManagedValue SILGenFunction::emitManagedBeginBorrow(SILLocation loc,
                                                    SILValue v) {
  auto &lowering = getTypeLowering(v->getType());
  return emitManagedBeginBorrow(loc, v, lowering);
}

ManagedValue
SILGenFunction::emitManagedBeginBorrow(SILLocation loc, SILValue v,
                                       const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         v->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(v);

  if (v->getType().isAddress())
    return ManagedValue::forBorrowedAddressRValue(v);

  if (v->getOwnershipKind() == OwnershipKind::None)
    return ManagedValue::forRValueWithoutOwnership(v);

  if (v->getOwnershipKind() == OwnershipKind::Guaranteed)
    return ManagedValue::forBorrowedObjectRValue(v);

  auto *bbi = B.createBeginBorrow(loc, v);
  return emitManagedBorrowedRValueWithCleanup(v, bbi, lowering);
}

EndBorrowCleanup::EndBorrowCleanup(SILValue borrowedValue)
    : borrowedValue(borrowedValue) {
  assert(!SILArgument::isTerminatorResult(borrowedValue) &&
         "Transforming terminators do not have end_borrow");
  assert(!isa<SILFunctionArgument>(borrowedValue) &&
         "SILFunctionArguments cannot have an end_borrow");
}

void EndBorrowCleanup::emit(SILGenFunction &SGF, CleanupLocation l,
                            ForUnwind_t forUnwind) {
  SGF.B.createEndBorrow(l, borrowedValue);
}

void EndBorrowCleanup::dump(SILGenFunction &) const {
#ifndef NDEBUG
  llvm::errs() << "EndBorrowCleanup "
               << "State:" << getState() << "\n"
               << "borrowed:" << borrowedValue << "\n";
#endif
}

namespace {

struct FormalEvaluationEndBorrowCleanup : Cleanup {
  FormalEvaluationContext::stable_iterator Depth;

  FormalEvaluationEndBorrowCleanup() : Depth() {}

  void emit(SILGenFunction &SGF, CleanupLocation l, ForUnwind_t forUnwind) override {
    getEvaluation(SGF).finish(SGF);
  }

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "FormalEvaluationEndBorrowCleanup "
                 << "State:" << getState() << "\n"
                 << "original:" << getOriginalValue(SGF) << "\n"
                 << "borrowed:" << getBorrowedValue(SGF) << "\n";
#endif
  }

  SharedBorrowFormalAccess &getEvaluation(SILGenFunction &SGF) const {
    auto &evaluation = *SGF.FormalEvalContext.find(Depth);
    assert(evaluation.getKind() == FormalAccess::Shared);
    return static_cast<SharedBorrowFormalAccess &>(evaluation);
  }

  SILValue getOriginalValue(SILGenFunction &SGF) const {
    return getEvaluation(SGF).getOriginalValue();
  }

  SILValue getBorrowedValue(SILGenFunction &SGF) const {
    return getEvaluation(SGF).getBorrowedValue();
  }
};

} // end anonymous namespace

ManagedValue
SILGenFunction::emitFormalEvaluationManagedBeginBorrow(SILLocation loc,
                                                       SILValue v) {
  if (v->getOwnershipKind() == OwnershipKind::Guaranteed)
    return ManagedValue::forBorrowedObjectRValue(v);
  auto &lowering = getTypeLowering(v->getType());
  return emitFormalEvaluationManagedBeginBorrow(loc, v, lowering);
}

ManagedValue SILGenFunction::emitFormalEvaluationManagedBeginBorrow(
    SILLocation loc, SILValue v, const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         v->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(v);
  if (v->getOwnershipKind() == OwnershipKind::Guaranteed)
    return ManagedValue::forBorrowedRValue(v);
  auto *bbi = B.createBeginBorrow(loc, v);
  return emitFormalEvaluationManagedBorrowedRValueWithCleanup(loc, v, bbi,
                                                              lowering);
}

ManagedValue SILGenFunction::emitFormalEvaluationManagedStoreBorrow(
    SILLocation loc, SILValue v, SILValue addr) {
  auto &lowering = getTypeLowering(v->getType());
  if (lowering.isTrivial() || v->getOwnershipKind() == OwnershipKind::None) {
    lowering.emitStore(B, loc, v, addr, StoreOwnershipQualifier::Trivial);
    return ManagedValue::forTrivialAddressRValue(addr);
  }
  auto *sbi = B.createStoreBorrow(loc, v, addr);
  return emitFormalEvaluationManagedBorrowedRValueWithCleanup(loc, v, sbi,
                                                              lowering);
}

ManagedValue
SILGenFunction::emitFormalEvaluationManagedBorrowedRValueWithCleanup(
    SILLocation loc, SILValue original, SILValue borrowed) {
  auto &lowering = getTypeLowering(original->getType());
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
    return ManagedValue::forRValueWithoutOwnership(borrowed);

  assert(isInFormalEvaluationScope() && "Must be in formal evaluation scope");
  auto &cleanup = Cleanups.pushCleanup<FormalEvaluationEndBorrowCleanup>();
  CleanupHandle handle = Cleanups.getTopCleanup();
  FormalEvalContext.push<SharedBorrowFormalAccess>(loc, handle, original,
                                                   borrowed);
  cleanup.Depth = FormalEvalContext.stable_begin();
  return ManagedValue::forBorrowedRValue(borrowed);
}

ManagedValue
SILGenFunction::emitManagedBorrowedArgumentWithCleanup(SILPhiArgument *arg) {
  if (arg->getOwnershipKind() == OwnershipKind::None ||
      arg->getType().isTrivial(F)) {
    return ManagedValue::forRValueWithoutOwnership(arg);
  }

  assert(arg->getOwnershipKind() == OwnershipKind::Guaranteed);
  Cleanups.pushCleanup<EndBorrowCleanup>(arg);
  return ManagedValue::forBorrowedObjectRValue(arg);
}

ManagedValue
SILGenFunction::emitManagedBorrowedRValueWithCleanup(SILValue original,
                                                     SILValue borrowed) {
  assert(original->getType().getObjectType() ==
         borrowed->getType().getObjectType());
  auto &lowering = getTypeLowering(original->getType());
  return emitManagedBorrowedRValueWithCleanup(original, borrowed, lowering);
}

ManagedValue
SILGenFunction::emitManagedBorrowedRValueWithCleanup(SILValue borrowed) {
  auto &lowering = getTypeLowering(borrowed->getType());
  return emitManagedBorrowedRValueWithCleanup(borrowed, lowering);
}

ManagedValue SILGenFunction::emitManagedBorrowedRValueWithCleanup(
    SILValue borrowed, const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         borrowed->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(borrowed);

  if (borrowed->getType().isObject() &&
      borrowed->getOwnershipKind() == OwnershipKind::None)
    return ManagedValue::forObjectRValueWithoutOwnership(borrowed);

  if (borrowed->getType().isObject()) {
    Cleanups.pushCleanup<EndBorrowCleanup>(borrowed);
  }

  return ManagedValue::forBorrowedRValue(borrowed);
}

ManagedValue SILGenFunction::emitManagedBorrowedRValueWithCleanup(
    SILValue original, SILValue borrowed, const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         original->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(borrowed);

  if (original->getType().isObject() &&
      original->getOwnershipKind() == OwnershipKind::None)
    return ManagedValue::forObjectRValueWithoutOwnership(borrowed);

  Cleanups.pushCleanup<EndBorrowCleanup>(borrowed);
  return ManagedValue::forBorrowedRValue(borrowed);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v) {
  auto &lowering = getTypeLowering(v->getType());
  return emitManagedRValueWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedRValueWithCleanup(SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getObjectType() ==
         v->getType().getObjectType());
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(v);
  if (v->getType().isObject() && v->getOwnershipKind() == OwnershipKind::None) {
    return ManagedValue::forRValueWithoutOwnership(v);
  }
  return ManagedValue::forOwnedRValue(v, enterDestroyCleanup(v));
}

ManagedValue SILGenFunction::emitManagedBufferWithCleanup(SILValue v) {
  auto &lowering = getTypeLowering(v->getType());
  return emitManagedBufferWithCleanup(v, lowering);
}

ManagedValue SILGenFunction::emitManagedBufferWithCleanup(SILValue v,
                                               const TypeLowering &lowering) {
  assert(lowering.getLoweredType().getAddressType() == v->getType() ||
         !silConv.useLoweredAddresses());
  if (lowering.isTrivial())
    return ManagedValue::forTrivialAddressRValue(v);

  return ManagedValue::forOwnedAddressRValue(v, enterDestroyCleanup(v));
}

void SILGenFunction::emitExprInto(Expr *E, Initialization *I,
                                  std::optional<SILLocation> L) {
  // Handle the special case of copying an lvalue.
  if (auto load = dyn_cast<LoadExpr>(E)) {
    FormalEvaluationScope writeback(*this);
    auto lv = emitLValue(load->getSubExpr(),
                         SGFAccessKind::BorrowedAddressRead);
    emitCopyLValueInto(L ? *L : E, std::move(lv), I);
    return;
  }

  RValue result = emitRValue(E, SGFContext(I));
  if (result.isInContext())
    return;
  std::move(result).ensurePlusOne(*this, E).forwardInto(*this, L ? *L : E, I);
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
      LValue lv = SGF.emitLValue(E->getSubExpr(), SGFAccessKind::ReadWrite);
      return RValue(SGF, E, SGF.emitAddressOfLValue(E->getSubExpr(),
                                                    std::move(lv)));
    }

    RValue visitLazyInitializerExpr(LazyInitializerExpr *E, SGFContext C);

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

    RValue visitStringLiteralExpr(StringLiteralExpr *E, SGFContext C);
    RValue visitLoadExpr(LoadExpr *E, SGFContext C);
    RValue visitDerivedToBaseExpr(DerivedToBaseExpr *E, SGFContext C);
    RValue visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                       SGFContext C);
    RValue visitCollectionUpcastConversionExpr(
             CollectionUpcastConversionExpr *E,
             SGFContext C);
    RValue visitBridgeToObjCExpr(BridgeToObjCExpr *E, SGFContext C);
    RValue visitPackExpansionExpr(PackExpansionExpr *E, SGFContext C);
    RValue visitPackElementExpr(PackElementExpr *E, SGFContext C);
    RValue visitMaterializePackExpr(MaterializePackExpr *E, SGFContext C);
    RValue visitBridgeFromObjCExpr(BridgeFromObjCExpr *E, SGFContext C);
    RValue visitConditionalBridgeFromObjCExpr(ConditionalBridgeFromObjCExpr *E,
                                              SGFContext C);
    RValue visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, SGFContext C);
    RValue visitUnresolvedTypeConversionExpr(UnresolvedTypeConversionExpr *E,
                                             SGFContext C);
    RValue visitABISafeConversionExpr(ABISafeConversionExpr *E, SGFContext C) {
      llvm_unreachable("cannot appear in rvalue");
    }
    RValue visitFunctionConversionExpr(FunctionConversionExpr *E,
                                       SGFContext C);
    RValue visitActorIsolationErasureExpr(ActorIsolationErasureExpr *E,
                                          SGFContext C);
    RValue visitExtractFunctionIsolationExpr(ExtractFunctionIsolationExpr *E,
                                             SGFContext C);
    RValue visitCovariantFunctionConversionExpr(
             CovariantFunctionConversionExpr *E,
             SGFContext C);
    RValue visitCovariantReturnConversionExpr(
             CovariantReturnConversionExpr *E,
             SGFContext C);
    RValue visitUnsafeCastExpr(UnsafeCastExpr *E, SGFContext C);
    RValue visitErasureExpr(ErasureExpr *E, SGFContext C);
    RValue visitAnyHashableErasureExpr(AnyHashableErasureExpr *E, SGFContext C);
    RValue visitForcedCheckedCastExpr(ForcedCheckedCastExpr *E,
                                      SGFContext C);
    RValue visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E,
                                           SGFContext C);
    RValue visitIsExpr(IsExpr *E, SGFContext C);
    RValue visitCoerceExpr(CoerceExpr *E, SGFContext C);
    RValue visitUnderlyingToOpaqueExpr(UnderlyingToOpaqueExpr *E, SGFContext C);
    RValue visitUnreachableExpr(UnreachableExpr *E, SGFContext C);
    RValue visitTupleExpr(TupleExpr *E, SGFContext C);
    RValue visitMemberRefExpr(MemberRefExpr *E, SGFContext C);
    RValue visitDynamicMemberRefExpr(DynamicMemberRefExpr *E, SGFContext C);
    RValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E,
                                         SGFContext C);
    RValue visitTupleElementExpr(TupleElementExpr *E, SGFContext C);
    RValue visitSubscriptExpr(SubscriptExpr *E, SGFContext C);
    RValue visitKeyPathApplicationExpr(KeyPathApplicationExpr *E, SGFContext C);
    RValue visitDynamicSubscriptExpr(DynamicSubscriptExpr *E,
                                     SGFContext C);
    RValue visitDestructureTupleExpr(DestructureTupleExpr *E, SGFContext C);
    RValue visitDynamicTypeExpr(DynamicTypeExpr *E, SGFContext C);
    RValue visitCaptureListExpr(CaptureListExpr *E, SGFContext C);
    RValue visitAbstractClosureExpr(AbstractClosureExpr *E, SGFContext C);
    ManagedValue tryEmitConvertedClosure(AbstractClosureExpr *e,
                                         const Conversion &conv);
    ManagedValue emitClosureReference(AbstractClosureExpr *e,
                                      const FunctionTypeInfo &contextInfo);
    RValue visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                              SGFContext C);
    RValue visitRegexLiteralExpr(RegexLiteralExpr *E, SGFContext C);
    RValue visitObjectLiteralExpr(ObjectLiteralExpr *E, SGFContext C);
    RValue visitEditorPlaceholderExpr(EditorPlaceholderExpr *E, SGFContext C);
    RValue visitObjCSelectorExpr(ObjCSelectorExpr *E, SGFContext C);
    RValue visitKeyPathExpr(KeyPathExpr *E, SGFContext C);
    RValue visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E,
                                           SGFContext C);
    RValue visitCollectionExpr(CollectionExpr *E, SGFContext C);
    RValue visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E,
                                            SGFContext C);
    RValue visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E, SGFContext C);
    RValue visitClassMetatypeToObjectExpr(ClassMetatypeToObjectExpr *E,
                                          SGFContext C);
    RValue visitExistentialMetatypeToObjectExpr(ExistentialMetatypeToObjectExpr *E,
                                                SGFContext C);
    RValue visitProtocolMetatypeToObjectExpr(ProtocolMetatypeToObjectExpr *E,
                                             SGFContext C);
    RValue visitTernaryExpr(TernaryExpr *E, SGFContext C);

    RValue visitAssignExpr(AssignExpr *E, SGFContext C);
    RValue visitEnumIsCaseExpr(EnumIsCaseExpr *E, SGFContext C);

    RValue visitSingleValueStmtExpr(SingleValueStmtExpr *E, SGFContext C);

    RValue visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C);
    RValue visitOptionalEvaluationExpr(OptionalEvaluationExpr *E,
                                       SGFContext C);
    RValue visitForceValueExpr(ForceValueExpr *E, SGFContext C);
    RValue emitForceValue(ForceValueExpr *loc, Expr *E,
                          unsigned numOptionalEvaluations,
                          SGFContext C);
    RValue visitOpenExistentialExpr(OpenExistentialExpr *E, SGFContext C);
    RValue visitMakeTemporarilyEscapableExpr(
                                 MakeTemporarilyEscapableExpr *E, SGFContext C);

    RValue visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C);
    RValue visitPropertyWrapperValuePlaceholderExpr(
        PropertyWrapperValuePlaceholderExpr *E, SGFContext C);
    RValue visitAppliedPropertyWrapperExpr(
        AppliedPropertyWrapperExpr *E, SGFContext C);

    RValue visitInOutToPointerExpr(InOutToPointerExpr *E, SGFContext C);
    RValue visitArrayToPointerExpr(ArrayToPointerExpr *E, SGFContext C);
    RValue visitStringToPointerExpr(StringToPointerExpr *E, SGFContext C);
    RValue visitPointerToPointerExpr(PointerToPointerExpr *E, SGFContext C);
    RValue visitForeignObjectConversionExpr(ForeignObjectConversionExpr *E,
                                            SGFContext C);
    RValue visitUnevaluatedInstanceExpr(UnevaluatedInstanceExpr *E,
                                        SGFContext C);
    RValue visitTapExpr(TapExpr *E, SGFContext C);
    RValue visitDefaultArgumentExpr(DefaultArgumentExpr *E, SGFContext C);
    RValue visitErrorExpr(ErrorExpr *E, SGFContext C);

    RValue visitDifferentiableFunctionExpr(DifferentiableFunctionExpr *E,
                                           SGFContext C);
    RValue visitLinearFunctionExpr(LinearFunctionExpr *E, SGFContext C);
    RValue visitDifferentiableFunctionExtractOriginalExpr(
        DifferentiableFunctionExtractOriginalExpr *E, SGFContext C);
    RValue visitLinearFunctionExtractOriginalExpr(
        LinearFunctionExtractOriginalExpr *E, SGFContext C);
    RValue visitLinearToDifferentiableFunctionExpr(
        LinearToDifferentiableFunctionExpr *E, SGFContext C);
    RValue visitConsumeExpr(ConsumeExpr *E, SGFContext C);
    RValue visitCopyExpr(CopyExpr *E, SGFContext C);
    RValue visitMacroExpansionExpr(MacroExpansionExpr *E, SGFContext C);
    RValue visitCurrentContextIsolationExpr(CurrentContextIsolationExpr *E, SGFContext C);
    RValue visitTypeValueExpr(TypeValueExpr *E, SGFContext C);
  };
} // end anonymous namespace

namespace {
  struct BridgingConversion {
    Expr *SubExpr;
    std::optional<Conversion::KindTy> Kind;
    unsigned MaxOptionalDepth;

    BridgingConversion() : SubExpr(nullptr) {}
    BridgingConversion(Expr *sub, std::optional<Conversion::KindTy> kind,
                       unsigned depth)
        : SubExpr(sub), Kind(kind), MaxOptionalDepth(depth) {
      assert(!kind || Conversion::isBridgingKind(*kind));
    }

    explicit operator bool() const { return SubExpr != nullptr; }
  };
}

static BridgingConversion getBridgingConversion(Expr *E) {
  E = E->getSemanticsProvidingExpr();

  // Detect bridging conversions.
  if (auto bridge = dyn_cast<BridgeToObjCExpr>(E)) {
    return { bridge->getSubExpr(), Conversion::BridgeToObjC, 0 };
  }
  if (auto bridge = dyn_cast<BridgeFromObjCExpr>(E)) {
    return { bridge->getSubExpr(), Conversion::BridgeFromObjC, 0 };
  }

  // We can handle optional injections.
  if (auto inject = dyn_cast<InjectIntoOptionalExpr>(E)) {
    return getBridgingConversion(inject->getSubExpr());
  }

  // Look through optional-to-optional conversions.
  if (auto optEval = dyn_cast<OptionalEvaluationExpr>(E)) {
    auto sub = optEval->getSubExpr()->getSemanticsProvidingExpr();
    if (auto subResult = getBridgingConversion(sub)) {
      sub = subResult.SubExpr->getSemanticsProvidingExpr();
      if (auto bind = dyn_cast<BindOptionalExpr>(sub)) {
        if (bind->getDepth() == subResult.MaxOptionalDepth) {
          return { bind->getSubExpr(),
                   subResult.Kind,
                   subResult.MaxOptionalDepth + 1 };
        }
      }
    }
  }

  // Open-existentials can be part of bridging conversions in very
  // specific patterns.
  auto open = dyn_cast<OpenExistentialExpr>(E);
  if (open) E = open->getSubExpr();

  // Existential erasure.
  if (auto erasure = dyn_cast<ErasureExpr>(E)) {
    Conversion::KindTy kind;

    // Converting to Any is sometimes part of bridging and definitely
    // needs special peepholing behavior.
    if (erasure->getType()->isAny()) {
      kind = Conversion::AnyErasure;

    // Otherwise, nope.
    } else {
      return {};
    }

    // Tentatively look through the erasure.
    E = erasure->getSubExpr();

    // If we have an opening, we can only peephole if the value being
    // used is exactly the original value.
    if (open) {
      if (E == open->getOpaqueValue()) {
        return { open->getExistentialValue(), kind, 0 };
      }
      return {};
    }

    // Otherwise we can always peephole.
    return { E, kind, 0 };
  }

  // If we peeked through an opening, and we didn't recognize a specific
  // pattern above involving the opaque value, make sure we use the opening
  // as the final expression instead of accidentally looking through it.
  if (open)
    return {open, std::nullopt, 0};

  return {E, std::nullopt, 0};
}

/// If the given expression represents a bridging conversion, emit it with
/// the special reabstracting context.
static std::optional<ManagedValue>
tryEmitAsBridgingConversion(SILGenFunction &SGF, Expr *E, bool isExplicit,
                            SGFContext C) {
  // Try to pattern-match a conversion.  This can find bridging
  // conversions, but it can also find simple optional conversions:
  // injections and opt-to-opt conversions.
  auto result = getBridgingConversion(E);

  // If we didn't find a conversion at all, there's nothing special to do.
  if (!result ||
      result.SubExpr == E ||
      result.SubExpr->getType()->isEqual(E->getType()))
    return std::nullopt;

  // Even if the conversion doesn't involve bridging, we might still
  // expose more peephole opportunities by combining it with a contextual
  // conversion.
  if (!result.Kind) {
    // Only do this if the conversion is implicit.
    if (isExplicit)
      return std::nullopt;

    // Look for a contextual conversion.
    auto conversion = C.getAsConversion();
    if (!conversion)
      return std::nullopt;

    // Adjust the contextual conversion.
    auto sub = result.SubExpr;
    auto sourceType = sub->getType()->getCanonicalType();
    if (auto adjusted = conversion->getConversion()
                         .adjustForInitialOptionalConversions(sourceType)) {
      // Emit into the applied conversion.
      return conversion->emitWithAdjustedConversion(SGF, E, *adjusted,
                [sub](SILGenFunction &SGF, SILLocation loc, SGFContext C) {
        return SGF.emitRValueAsSingleValue(sub, C);
      });
    }

    // If that didn't work, there's nothing special to do.
    return std::nullopt;
  }

  auto kind = *result.Kind;
  auto subExpr = result.SubExpr;

  CanType resultType = E->getType()->getCanonicalType();
  Conversion conversion =
    Conversion::getBridging(kind, subExpr->getType()->getCanonicalType(),
                            resultType, SGF.getLoweredType(resultType),
                            isExplicit);

  // Only use this special pattern for AnyErasure conversions when we're
  // emitting into a peephole.
  if (kind == Conversion::AnyErasure) {
    auto outerConversion = C.getAsConversion();
    if (!outerConversion ||
        !canPeepholeConversions(SGF, outerConversion->getConversion(),
                                conversion)) {
      return std::nullopt;
    }
  }

  return SGF.emitConvertedRValue(subExpr, conversion, C);
}

RValue RValueEmitter::visitLazyInitializerExpr(LazyInitializerExpr *E,
                                               SGFContext C) {
  // We need to emit a profiler count increment specifically for the lazy
  // initialization, as we don't want to record an increment for every call to
  // the getter.
  SGF.emitProfilerIncrement(E);
  return visit(E->getSubExpr(), C);
}

RValue RValueEmitter::visitApplyExpr(ApplyExpr *E, SGFContext C) {
  return SGF.emitApplyExpr(E, C);
}

SILValue SILGenFunction::emitEmptyTuple(SILLocation loc) {
  return B.createTuple(
      loc, getLoweredType(TupleType::getEmpty(SGM.M.getASTContext())),
      ArrayRef<SILValue>());
}

namespace {

/// This is a simple cleanup class that at the end of a lexical scope consumes
/// an owned value by writing it back to memory. The user can forward this
/// cleanup to take ownership of the value and thus prevent it form being
/// written back.
struct OwnedValueWritebackCleanup final : Cleanup {
  using Flags = Cleanup::Flags;

  /// We store our own loc so that we can ensure that DI ignores our writeback.
  SILLocation loc;

  SILValue lvalueAddress;
  SILValue value;

  OwnedValueWritebackCleanup(SILLocation loc, SILValue lvalueAddress,
                             SILValue value)
      : loc(loc), lvalueAddress(lvalueAddress), value(value) {}

  bool getWritebackBuffer(function_ref<void(SILValue)> func) override {
    func(lvalueAddress);
    return true;
  }

  void emit(SILGenFunction &SGF, CleanupLocation l, ForUnwind_t forUnwind) override {
    SILValue valueToStore = value;
    SILType lvalueObjTy = lvalueAddress->getType().getObjectType();

    // If we calling a super.init and thus upcasted self, when we store self
    // back into the self slot, we need to perform a downcast from the upcasted
    // store value to the derived type of our lvalueAddress.
    if (valueToStore->getType() != lvalueObjTy) {
      if (!valueToStore->getType().isExactSuperclassOf(lvalueObjTy)) {
        llvm_unreachable("Invalid usage of delegate init self writeback");
      }

      valueToStore = SGF.B.createUncheckedRefCast(loc, valueToStore,
                                                  lvalueObjTy);
    }

    SGF.B.emitStoreValueOperation(loc, valueToStore, lvalueAddress,
                                  StoreOwnershipQualifier::Init);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "OwnedValueWritebackCleanup "
                 << "State:" << getState() << "\n"
                 << "lvalueAddress:" << lvalueAddress << "value:" << value
                 << "\n";
#endif
  }
};

} // end anonymous namespace

CleanupHandle SILGenFunction::enterOwnedValueWritebackCleanup(
    SILLocation loc, SILValue address, SILValue newValue) {
  Cleanups.pushCleanup<OwnedValueWritebackCleanup>(loc, address, newValue);
  return Cleanups.getTopCleanup();
}

RValue SILGenFunction::emitRValueForSelfInDelegationInit(SILLocation loc,
                                                         CanType refType,
                                                         SILValue addr,
                                                         SGFContext C) {
  assert(SelfInitDelegationState != SILGenFunction::NormalSelf &&
         "This should never be called unless we are in a delegation sequence");
  assert(getTypeLowering(addr->getType()).isLoadable() &&
         "Make sure that we are not dealing with semantic rvalues");

  // If we are currently in the WillSharedBorrowSelf state, then we know that
  // old self is not the self to our delegating initializer. Self in this case
  // to the delegating initializer is a metatype. Thus, we perform a
  // load_borrow. And move from WillSharedBorrowSelf -> DidSharedBorrowSelf.
  if (SelfInitDelegationState == SILGenFunction::WillSharedBorrowSelf) {
    assert(C.isGuaranteedPlusZeroOk() &&
           "This should only be called if guaranteed plus zero is ok");
    SelfInitDelegationState = SILGenFunction::DidSharedBorrowSelf;
    ManagedValue result =
        B.createLoadBorrow(loc, ManagedValue::forBorrowedAddressRValue(addr));
    return RValue(*this, loc, refType, result);
  }

  // If we are already in the did shared borrow self state, just return the
  // shared borrow value.
  if (SelfInitDelegationState == SILGenFunction::DidSharedBorrowSelf) {
    assert(C.isGuaranteedPlusZeroOk() &&
           "This should only be called if guaranteed plus zero is ok");
    ManagedValue result =
        B.createLoadBorrow(loc, ManagedValue::forBorrowedAddressRValue(addr));
    return RValue(*this, loc, refType, result);
  }

  // If we are in WillExclusiveBorrowSelf, then we need to perform an exclusive
  // borrow (i.e. a load take) and then move to DidExclusiveBorrowSelf.
  if (SelfInitDelegationState == SILGenFunction::WillExclusiveBorrowSelf) {
    const auto &typeLowering = getTypeLowering(addr->getType());
    SelfInitDelegationState = SILGenFunction::DidExclusiveBorrowSelf;
    SILValue self =
        emitLoad(loc, addr, typeLowering, C, IsTake, false).forward(*this);
    // Forward our initial value for init delegation self and create a new
    // cleanup that performs a writeback at the end of lexical scope if our
    // value is not consumed.
    InitDelegationSelf = ManagedValue::forExclusivelyBorrowedOwnedObjectRValue(
        self, enterOwnedValueWritebackCleanup(*InitDelegationLoc, addr, self));
    InitDelegationSelfBox = addr;
    return RValue(*this, loc, refType, InitDelegationSelf);
  }

  // If we hit this point, we must have DidExclusiveBorrowSelf. We should have
  // gone through the formal evaluation variant but did not. The only way that
  // this can happen is if during argument evaluation, we are accessing self in
  // a way that is illegal before we call super. Return a copy of self in this
  // case so that DI will flag on this issue. We do not care where the destroy
  // occurs, so we can use a normal scoped copy.
  ManagedValue Result;
  if (!SuperInitDelegationSelf) {
    Result = InitDelegationSelf.copy(*this, loc);
  } else {
    Result =
        B.createUncheckedRefCast(loc, SuperInitDelegationSelf.copy(*this, loc),
                                 InitDelegationSelf.getType());
  }

  return RValue(*this, loc, refType, Result);
}

RValue SILGenFunction::emitFormalEvaluationRValueForSelfInDelegationInit(
    SILLocation loc, CanType refType, SILValue addr, SGFContext C) {
  assert(SelfInitDelegationState != SILGenFunction::NormalSelf &&
         "This should never be called unless we are in a delegation sequence");
  assert(getTypeLowering(addr->getType()).isLoadable() &&
         "Make sure that we are not dealing with semantic rvalues");

  // If we are currently in the WillSharedBorrowSelf state, then we know that
  // old self is not the self to our delegating initializer. Self in this case
  // to the delegating initializer is a metatype. Thus, we perform a
  // load_borrow. And move from WillSharedBorrowSelf -> DidSharedBorrowSelf.
  if (SelfInitDelegationState == SILGenFunction::WillSharedBorrowSelf) {
    assert(C.isGuaranteedPlusZeroOk() &&
           "This should only be called if guaranteed plus zero is ok");
    SelfInitDelegationState = SILGenFunction::DidSharedBorrowSelf;
    ManagedValue result = B.createFormalAccessLoadBorrow(
        loc, ManagedValue::forBorrowedAddressRValue(addr));
    return RValue(*this, loc, refType, result);
  }

  // If we are already in the did shared borrow self state, just return the
  // shared borrow value.
  if (SelfInitDelegationState == SILGenFunction::DidSharedBorrowSelf) {
    assert(C.isGuaranteedPlusZeroOk() &&
           "This should only be called if guaranteed plus zero is ok");
    ManagedValue result = B.createFormalAccessLoadBorrow(
        loc, ManagedValue::forBorrowedAddressRValue(addr));
    return RValue(*this, loc, refType, result);
  }

  // If we hit this point, we must have DidExclusiveBorrowSelf. Thus borrow
  // self.
  //
  // *NOTE* This routine should /never/ begin an exclusive borrow of self. It is
  // only called when emitting self as a base in lvalue emission.
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
  ManagedValue castedBorrowedType = B.createUncheckedRefCast(
      loc, borrowedUpcast, InitDelegationSelf.getType());
  return RValue(*this, loc, refType, castedBorrowedType);
}

RValue SILGenFunction::
emitRValueForDecl(SILLocation loc, ConcreteDeclRef declRef, Type ncRefType,
                  AccessSemantics semantics, SGFContext C) {
  assert(!ncRefType->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");

  // If this is a decl that we have an lvalue for, produce and return it.
  ValueDecl *decl = declRef.getDecl();

  CanType refType = ncRefType->getCanonicalType();

  // If this is a reference to a module, produce an undef value. The
  // module value should never actually be used.
  if (isa<ModuleDecl>(decl)) {
    return emitUndefRValue(loc, refType);
  }

  // If this is a reference to a var, emit it as an l-value and then load.
  if (auto *var = dyn_cast<VarDecl>(decl))
    return emitRValueForNonMemberVarDecl(loc, declRef, refType, semantics, C);

  assert(!isa<TypeDecl>(decl));

  // If the referenced decl isn't a VarDecl, it should be a constant of some
  // sort.
  SILDeclRef silDeclRef(decl);
  assert(silDeclRef.getParameterListCount() == 1);
  auto substType = cast<AnyFunctionType>(refType);
  auto typeContext = getFunctionTypeInfo(substType);
  ManagedValue result = emitClosureValue(loc, silDeclRef, typeContext,
                                         declRef.getSubstitutions());
  return RValue(*this, loc, refType, result);
}

RValue RValueEmitter::visitDeclRefExpr(DeclRefExpr *E, SGFContext C) {
  return SGF.emitRValueForDecl(E, E->getDeclRef(), E->getType(),
                               E->getAccessSemantics(), C);
}

RValue RValueEmitter::visitTypeExpr(TypeExpr *E, SGFContext C) {
  assert(E->getType()->is<AnyMetatypeType>() &&
         "TypeExpr must have metatype type");
  auto Val = SGF.B.createMetatype(E, SGF.getLoweredType(E->getType()));
  return RValue(SGF, E, ManagedValue::forObjectRValueWithoutOwnership(Val));
}


RValue RValueEmitter::visitSuperRefExpr(SuperRefExpr *E, SGFContext C) {
  assert(!E->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");

  // If we have a normal self call, then use the emitRValueForDecl call. This
  // will emit self at +0 since it is guaranteed.
  ManagedValue Self =
      SGF.emitRValueForDecl(E, E->getSelf(), E->getSelf()->getTypeInContext(),
                            AccessSemantics::Ordinary)
          .getScalarValue();

  // Perform an upcast to convert self to the indicated super type.
  auto result = SGF.B.createUpcast(E, Self, SGF.getLoweredType(E->getType()));

  return RValue(SGF, E, result);
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
  // Peephole away the call to Optional<T>(nilLiteral: ()).
  if (E->getType()->getOptionalObjectType()) {
    auto *noneDecl = SGF.getASTContext().getOptionalNoneDecl();
    auto enumTy = SGF.getLoweredType(E->getType());

    ManagedValue noneValue;
    if (enumTy.isLoadable(SGF.F) || !SGF.silConv.useLoweredAddresses()) {
      auto *e = SGF.B.createEnum(E, SILValue(), noneDecl, enumTy);
      noneValue = SGF.emitManagedRValueWithCleanup(e);
    } else {
      noneValue =
          SGF.B.bufferForExpr(E, enumTy, SGF.getTypeLowering(enumTy), C,
                              [&](SILValue newAddr) {
                                SGF.B.createInjectEnumAddr(E, newAddr, noneDecl);
                              });
    }
    return RValue(SGF, E, noneValue);
  }

  return SGF.emitLiteral(E, C);
}

RValue RValueEmitter::visitIntegerLiteralExpr(IntegerLiteralExpr *E,
                                              SGFContext C) {
  if (E->getType()->is<AnyBuiltinIntegerType>())
    return RValue(SGF, E,
                  ManagedValue::forObjectRValueWithoutOwnership(
                      SGF.B.createIntegerLiteral(E)));
  return SGF.emitLiteral(E, C);
}
RValue RValueEmitter::visitFloatLiteralExpr(FloatLiteralExpr *E,
                                            SGFContext C) {
  if (E->getType()->is<BuiltinFloatType>())
    return RValue(SGF, E,
                  ManagedValue::forObjectRValueWithoutOwnership(
                      SGF.B.createFloatLiteral(E)));

  return SGF.emitLiteral(E, C);
}

RValue RValueEmitter::visitBooleanLiteralExpr(BooleanLiteralExpr *E, 
                                              SGFContext C) {
  return SGF.emitLiteral(E, C);
}

RValue RValueEmitter::visitStringLiteralExpr(StringLiteralExpr *E,
                                             SGFContext C) {
  return SGF.emitLiteral(E, C);
}

RValue RValueEmitter::visitLoadExpr(LoadExpr *E, SGFContext C) {
  // Any writebacks here are tightly scoped.
  FormalEvaluationScope writeback(SGF);
  LValue lv = SGF.emitLValue(E->getSubExpr(), SGFAccessKind::OwnedObjectRead);
  // We can't load at immediate +0 from the lvalue without deeper analysis,
  // since the access will be immediately ended and might invalidate the value
  // we loaded.
  return SGF.emitLoadOfLValue(E, std::move(lv), C.withFollowingSideEffects());
}

SILValue SILGenFunction::emitTemporaryAllocation(SILLocation loc, SILType ty,
                                                 HasDynamicLifetime_t dynamic,
                                                 IsLexical_t isLexical,
                                                 IsFromVarDecl_t isFromVarDecl,
                                                 bool generateDebugInfo) {
  ty = ty.getObjectType();
  std::optional<SILDebugVariable> DbgVar;
  if (generateDebugInfo)
    if (auto *VD = loc.getAsASTNode<VarDecl>())
      DbgVar = SILDebugVariable(VD->isLet(), 0);
  auto *alloc =
      B.createAllocStack(loc, ty, DbgVar, dynamic, isLexical, isFromVarDecl,
                         DoesNotUseMoveableValueDebugInfo
#ifndef NDEBUG
                         ,
                         !generateDebugInfo
#endif
      );
  enterDeallocStackCleanup(alloc);
  return alloc;
}

SILValue
SILGenFunction::emitTemporaryPackAllocation(SILLocation loc, SILType ty) {
  assert(ty.is<SILPackType>());
  ty = ty.getObjectType();
  auto *alloc = B.createAllocPack(loc, ty);
  enterDeallocPackCleanup(alloc);
  return alloc;
}

SILValue SILGenFunction::
getBufferForExprResult(SILLocation loc, SILType ty, SGFContext C) {
  // If you change this, change manageBufferForExprResult below as well.

  // If we have a single-buffer "emit into" initialization, use that for the
  // result.
  if (SILValue address = C.getAddressForInPlaceInitialization(*this, loc))
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
  if (C.finishInPlaceInitialization(*this))
    return ManagedValue::forInContext();
  
  // Add a cleanup for the temporary we allocated.
  if (bufferTL.isTrivial())
    return ManagedValue::forTrivialAddressRValue(buffer);

  return ManagedValue::forOwnedAddressRValue(buffer,
                                             enterDestroyCleanup(buffer));
}

SILGenFunction::ForceTryEmission::ForceTryEmission(SILGenFunction &SGF,
                                                   ForceTryExpr *loc)
    : SGF(SGF), Loc(loc), OldThrowDest(SGF.ThrowDest) {
  assert(loc && "cannot pass a null location");

  // Set up a "catch" block for when an error occurs.
  SILBasicBlock *catchBB = SGF.createBasicBlock(FunctionSection::Postmatter);

  SILValue indirectError;
  auto &errorTL = SGF.getTypeLowering(loc->getThrownError());
  if (!errorTL.isAddressOnly()) {
    (void) catchBB->createPhiArgument(errorTL.getLoweredType(),
                                      OwnershipKind::Owned);
  } else {
    indirectError = SGF.B.createAllocStack(loc, errorTL.getLoweredType());
    SGF.enterDeallocStackCleanup(indirectError);

  }

  SGF.ThrowDest = JumpDest(catchBB, SGF.Cleanups.getCleanupsDepth(),
                           CleanupLocation(loc),
                           ThrownErrorInfo(indirectError, /*discard=*/true));
}

void SILGenFunction::ForceTryEmission::finish() {
  assert(Loc && "emission already finished");

  auto catchBB = SGF.ThrowDest.getBlock();
  auto indirectError = SGF.ThrowDest.getThrownError().IndirectErrorResult;
  SGF.ThrowDest = OldThrowDest;

  // If there are no uses of the catch block, just drop it.
  if (catchBB->pred_empty()) {
    SGF.eraseBasicBlock(catchBB);
  } else {
    // Otherwise, we need to emit it.
    SILGenSavedInsertionPoint scope(SGF, catchBB, FunctionSection::Postmatter);

    ASTContext &ctx = SGF.getASTContext();

    // Consume the thrown error.
    ManagedValue error;
    if (catchBB->getNumArguments() == 1) {
      error = ManagedValue::forForwardedRValue(SGF, catchBB->getArgument(0));
    } else {
      error = ManagedValue::forForwardedRValue(SGF, indirectError);
    }

    // If we have 'any Error', use the older entrypoint that takes an
    // existential error directly. Otherwise, use the newer generic entrypoint.
    auto diagnoseError = error.getType().getASTType()->isErrorExistentialType()
      ? ctx.getDiagnoseUnexpectedError()
      : ctx.getDiagnoseUnexpectedErrorTyped();
    if (diagnoseError) {
      SILValue tmpBuffer;
      auto args = SGF.emitSourceLocationArgs(Loc->getExclaimLoc(), Loc);

      SubstitutionMap subMap;
      if (auto genericSig = diagnoseError->getGenericSignature()) {
        // FIXME: The conformance of the thrown error type to the Error
        // protocol should be provided to us by the type checker.
        subMap = SubstitutionMap::get(
            genericSig, [&](SubstitutableType *dependentType) {
              return error.getType().getObjectType().getASTType();
            }, LookUpConformanceInModule());

        // Generic errors are passed indirectly.
        if (!error.getType().isAddress()) {
          auto *tmp = SGF.B.createAllocStack(
              Loc, error.getType().getObjectType(), std::nullopt);
          error.forwardInto(SGF, Loc, tmp);
          error = ManagedValue::forForwardedRValue(SGF, tmp);

          tmpBuffer = tmp;
        }
      }

      SGF.emitApplyOfLibraryIntrinsic(
              Loc,
              diagnoseError,
              subMap,
              {
                error,
                args.filenameStartPointer,
                args.filenameLength,
                args.filenameIsAscii,
                args.line
              },
              SGFContext());

      if (tmpBuffer)
        SGF.B.createDeallocStack(Loc, tmpBuffer);
    }

    SGF.B.createUnreachable(Loc);
  }

  // Prevent double-finishing and make the destructor a no-op.
  Loc = nullptr;
}

RValue RValueEmitter::visitForceTryExpr(ForceTryExpr *E, SGFContext C) {
  SILGenFunction::ForceTryEmission emission(SGF, E);

  // Visit the sub-expression.
  return visit(E->getSubExpr(), C);
}

RValue RValueEmitter::visitOptionalTryExpr(OptionalTryExpr *E, SGFContext C) {
  // FIXME: Much of this was copied from visitOptionalEvaluationExpr.

  // Prior to Swift 5, an optional try's subexpression is always wrapped in an additional optional
  bool shouldWrapInOptional = !(SGF.getASTContext().LangOpts.isSwiftVersionAtLeast(5));
  
  auto &optTL = SGF.getTypeLowering(E->getType());

  Initialization *optInit = C.getEmitInto();
  bool usingProvidedContext =
    optInit && optInit->canPerformInPlaceInitialization();

  // Form the optional using address operations if the type is address-only or
  // if we already have an address to use.
  bool isByAddress = ((usingProvidedContext || optTL.isAddressOnly()) &&
      SGF.silConv.useLoweredAddresses());

  std::unique_ptr<TemporaryInitialization> optTemp;
  if (!isByAddress) {
    // If the caller produced a context for us, but we're not going
    // to use it, make sure we don't.
    optInit = nullptr;
  } else if (!usingProvidedContext) {
    // Allocate the temporary for the Optional<T> if we didn't get one from the
    // context.  This needs to happen outside of the cleanups scope we're about
    // to push.
    optTemp = SGF.emitTemporary(E, optTL);
    optInit = optTemp.get();
  }
  assert(isByAddress == (optInit != nullptr));

  // Acquire the address to emit into outside of the cleanups scope.
  SILValue optAddr;
  if (isByAddress)
    optAddr = optInit->getAddressForInPlaceInitialization(SGF, E);

  // Set up a "catch" block for when an error occurs.
  SILBasicBlock *catchBB = SGF.createBasicBlock(FunctionSection::Postmatter);

  // FIXME: opaque values
  auto &errorTL = SGF.getTypeLowering(E->getThrownError());
  if (!errorTL.isAddressOnly()) {
    (void) catchBB->createPhiArgument(errorTL.getLoweredType(),
                                      OwnershipKind::Owned);
  }

  FullExpr localCleanups(SGF.Cleanups, E);

  llvm::SaveAndRestore<JumpDest> throwDest{
    SGF.ThrowDest,
    JumpDest(catchBB, SGF.Cleanups.getCleanupsDepth(), E,
             ThrownErrorInfo::forDiscard())};

  SILValue branchArg;
  if (shouldWrapInOptional) {
    if (isByAddress) {
      assert(optAddr);
      SGF.emitInjectOptionalValueInto(E, E->getSubExpr(), optAddr, optTL);
    } else {
      ManagedValue subExprValue = SGF.emitRValueAsSingleValue(E->getSubExpr());
      ManagedValue wrapped = SGF.getOptionalSomeValue(E, subExprValue, optTL);
      branchArg = wrapped.forward(SGF);
    }
  }
  else {
    if (isByAddress) {
      assert(optAddr);
      // We've already computed the address where we want the result.
      KnownAddressInitialization normalInit(optAddr);
      SGF.emitExprInto(E->getSubExpr(), &normalInit);
      normalInit.finishInitialization(SGF);
    } else {
      ManagedValue subExprValue = SGF.emitRValueAsSingleValue(E->getSubExpr());
      branchArg = subExprValue.forward(SGF);
    }
  }
  
  localCleanups.pop();

  // If it turns out there are no uses of the catch block, just drop it.
  if (catchBB->pred_empty()) {
    // Remove the dead failureBB.
    SGF.eraseBasicBlock(catchBB);

    // The value we provide is the one we've already got.
    if (!isByAddress)
      return RValue(SGF, E,
                    SGF.emitManagedRValueWithCleanup(branchArg, optTL));

    optInit->finishInitialization(SGF);

    // If we emitted into the provided context, we're done.
    if (usingProvidedContext)
      return RValue::forInContext();

    return RValue(SGF, E, optTemp->getManagedAddress());
  }

  SILBasicBlock *contBB = SGF.createBasicBlock();

  // Branch to the continuation block.
  if (isByAddress)
    SGF.B.createBranch(E, contBB);
  else
    SGF.B.createBranch(E, contBB, branchArg);

  // If control branched to the failure block, inject .none into the
  // result type.
  SGF.B.emitBlock(catchBB);
  FullExpr catchCleanups(SGF.Cleanups, E);

  // Consume the thrown error.
  if (!errorTL.isAddressOnly())
    (void) SGF.emitManagedRValueWithCleanup(catchBB->getArgument(0));
  catchCleanups.pop();

  if (isByAddress) {
    SGF.emitInjectOptionalNothingInto(E, optAddr, optTL);
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
    auto arg =
        contBB->createPhiArgument(optTL.getLoweredType(), OwnershipKind::Owned);
    return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(arg, optTL));
  }

  optInit->finishInitialization(SGF);
  
  // If we emitted into the provided context, we're done.
  if (usingProvidedContext)
    return RValue::forInContext();

  assert(optTemp);
  return RValue(SGF, E, optTemp->getManagedAddress());
}

static bool inExclusiveBorrowSelfSection(
    SILGenFunction::SelfInitDelegationStates delegationState) {
  return delegationState == SILGenFunction::WillExclusiveBorrowSelf ||
         delegationState == SILGenFunction::DidExclusiveBorrowSelf;
}

static RValue visitDerivedToBaseExprOfSelf(SILGenFunction &SGF,
                                           DeclRefExpr *dre,
                                           DerivedToBaseExpr *E, SGFContext C) {
  SGFContext ctx;
  auto *vd = cast<ParamDecl>(dre->getDecl());
  SILType derivedType = SGF.getLoweredType(E->getType());
  ManagedValue selfValue;

  // If we have not exclusively borrowed self, we need to do so now.
  if (SGF.SelfInitDelegationState == SILGenFunction::WillExclusiveBorrowSelf) {
    // We need to use a full scope here to ensure that any underlying
    // "normal cleanup" borrows are cleaned up.
    Scope S(SGF, E);
    selfValue = S.popPreservingValue(SGF.emitRValueAsSingleValue(dre));
  } else {
    // If we already exclusively borrowed self, then we need to emit self
    // using formal evaluation primitives.

    assert(SGF.SelfInitDelegationState ==
           SILGenFunction::DidExclusiveBorrowSelf);
    // This needs to be inlined since there is a Formal Evaluation Scope
    // in emitRValueForDecl that causing any borrow for this LValue to be
    // popped too soon.
    selfValue =
      SGF.emitAddressOfLocalVarDecl(dre, vd, dre->getType()->getCanonicalType(),
                                    SGFAccessKind::OwnedObjectRead);
    selfValue = SGF.emitFormalEvaluationRValueForSelfInDelegationInit(
                       E, dre->getType()->getCanonicalType(),
                       selfValue.getLValueAddress(), ctx)
                    .getAsSingleValue(SGF, E);
  }
  assert(selfValue);

  // Check if we need to perform a conversion here.
  if (derivedType && selfValue.getType() != derivedType)
    selfValue = SGF.B.createUpcast(E, selfValue, derivedType);
  return RValue(SGF, dre, selfValue);
}

RValue RValueEmitter::visitDerivedToBaseExpr(DerivedToBaseExpr *E,
                                             SGFContext C) {
  // If we are going through a decl ref expr and have self and we are in the
  // exclusive borrow section of delegating init emission, use a special case.
  if (inExclusiveBorrowSelfSection(SGF.SelfInitDelegationState)) {
    if (auto *dre = dyn_cast<DeclRefExpr>(E->getSubExpr())) {
      if (isa<ParamDecl>(dre->getDecl()) &&
          dre->getDecl()->getName() == SGF.getASTContext().Id_self &&
          dre->getDecl()->isImplicit()) {
        return visitDerivedToBaseExprOfSelf(SGF, dre, E, C);
      }
    }
  }

  // We can pass down the SGFContext as a following projection. We have never
  // actually implemented emit into here, so we are not changing behavior.
  ManagedValue original =
      SGF.emitRValueAsSingleValue(E->getSubExpr(), C.withFollowingProjection());

  // Derived-to-base casts in the AST might not be reflected as such
  // in the SIL type system, for example, a cast from DynamicSelf
  // directly to its own Self type.
  auto loweredResultTy = SGF.getLoweredType(E->getType());
  if (original.getType() == loweredResultTy)
    return RValue(SGF, E, original);

  ManagedValue converted = SGF.B.createUpcast(E, original, loweredResultTy);
  return RValue(SGF, E, converted);
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
    return RValue(SGF, E,
                  ManagedValue::forObjectRValueWithoutOwnership(metaBase));

  auto upcast = SGF.B.createUpcast(E, metaBase, loweredResultTy);
  return RValue(SGF, E, ManagedValue::forObjectRValueWithoutOwnership(upcast));
}

RValue SILGenFunction::emitCollectionConversion(SILLocation loc,
                                                FuncDecl *fn,
                                                CanType fromCollection,
                                                CanType toCollection,
                                                ManagedValue mv,
                                                SGFContext C) {
  SmallVector<Type, 4> replacementTypes;

  auto fromArgs = cast<BoundGenericType>(fromCollection)->getGenericArgs();
  auto toArgs = cast<BoundGenericType>(toCollection)->getGenericArgs();
  replacementTypes.insert(replacementTypes.end(),
                          fromArgs.begin(), fromArgs.end());
  replacementTypes.insert(replacementTypes.end(),
                          toArgs.begin(), toArgs.end());

  // Form type parameter substitutions.
  auto genericSig = fn->getGenericSignature();
  auto subMap =
    SubstitutionMap::get(genericSig, replacementTypes,
                         LookUpConformanceInModule());
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
  FuncDecl *fn = nullptr;
  if (fromCollection->isArray()) {
    fn = SGF.SGM.getArrayForceCast(loc);
  } else if (fromCollection->isDictionary()) {
    fn = SGF.SGM.getDictionaryUpCast(loc);
  } else if (fromCollection->isSet()) {
    fn = SGF.SGM.getSetUpCast(loc);
  } else {
    llvm_unreachable("unsupported collection upcast kind");
  }

  return SGF.emitCollectionConversion(loc, fn, fromCollection, toCollection,
                                      mv, C);
}

RValue
RValueEmitter::visitConditionalBridgeFromObjCExpr(
                              ConditionalBridgeFromObjCExpr *E, SGFContext C) {
  // Get the sub expression argument as a managed value
  auto mv = SGF.emitRValueAsSingleValue(E->getSubExpr());

  auto conversionRef = E->getConversion();
  auto conversion = cast<FuncDecl>(conversionRef.getDecl());
  auto subs = conversionRef.getSubstitutions();

  auto nativeType = Type(GenericTypeParamType::getType(/*depth*/ 0, /*index*/ 0,
                                                       SGF.getASTContext()))
                        .subst(subs);

  auto metatypeType = SGF.getLoweredType(MetatypeType::get(nativeType));
  auto metatype = ManagedValue::forObjectRValueWithoutOwnership(
      SGF.B.createMetatype(E, metatypeType));

  return SGF.emitApplyOfLibraryIntrinsic(E, conversion, subs,
                                         { mv, metatype }, C);
}

/// Given an implicit bridging conversion, check whether the context
/// can be peepholed.
static bool
tryPeepholeBridgingConversion(SILGenFunction &SGF, Conversion::KindTy kind,
                              ImplicitConversionExpr *E, SGFContext C) {
  assert(isa<BridgeFromObjCExpr>(E) || isa<BridgeToObjCExpr>(E));
  if (auto outerConversion = C.getAsConversion()) {
    auto subExpr = E->getSubExpr();
    CanType sourceType = subExpr->getType()->getCanonicalType();
    CanType resultType = E->getType()->getCanonicalType();
    SILType loweredResultTy = SGF.getLoweredType(resultType);
    auto conversion = Conversion::getBridging(kind, sourceType, resultType,
                                              loweredResultTy);
    if (outerConversion->tryPeephole(SGF, E->getSubExpr(), conversion)) {
      outerConversion->finishInitialization(SGF);
      return true;
    }
  }

  return false;
}

RValue
RValueEmitter::visitBridgeFromObjCExpr(BridgeFromObjCExpr *E, SGFContext C) {
  if (tryPeepholeBridgingConversion(SGF, Conversion::BridgeFromObjC, E, C))
    return RValue::forInContext();

  // Emit the sub-expression.
  auto mv = SGF.emitRValueAsSingleValue(E->getSubExpr());

  CanType origType = E->getSubExpr()->getType()->getCanonicalType();
  CanType resultType = E->getType()->getCanonicalType();
  SILType loweredResultTy = SGF.getLoweredType(resultType);
  auto result = SGF.emitBridgedToNativeValue(E, mv, origType, resultType,
                                             loweredResultTy, C);
  return RValue(SGF, E, result);
}

RValue
RValueEmitter::visitBridgeToObjCExpr(BridgeToObjCExpr *E, SGFContext C) {
  if (tryPeepholeBridgingConversion(SGF, Conversion::BridgeToObjC, E, C))
    return RValue::forInContext();

  // Emit the sub-expression.
  auto mv = SGF.emitRValueAsSingleValue(E->getSubExpr());

  CanType origType = E->getSubExpr()->getType()->getCanonicalType();
  CanType resultType = E->getType()->getCanonicalType();
  SILType loweredResultTy = SGF.getLoweredType(resultType);
  auto result = SGF.emitNativeToBridgedValue(E, mv, origType, resultType,
                                             loweredResultTy, C);
  return RValue(SGF, E, result);
}

RValue
RValueEmitter::visitPackExpansionExpr(PackExpansionExpr *E,
                                      SGFContext C) {
  // The contexts where PackExpansionExpr can occur are expected to
  // set up for pack-expansion emission by either recognizing them
  // and treating them specially or setting up an appropriate context
  // to emit into.
  auto init = C.getEmitInto();
  assert(init && init->canPerformPackExpansionInitialization() &&
         "cannot emit a PackExpansionExpr without an appropriate context");

  auto type = E->getType()->getCanonicalType();
  assert(isa<PackExpansionType>(type));
  auto formalPackType = CanPackType::get(SGF.getASTContext(), {type});

  SGF.emitDynamicPackLoop(E, formalPackType, /*component index*/ 0,
                          E->getGenericEnvironment(),
                          [&](SILValue indexWithinComponent,
                              SILValue packExpansionIndex,
                              SILValue packIndex) {
    init->performPackExpansionInitialization(SGF, E, indexWithinComponent,
                                             [&](Initialization *eltInit) {
      SGF.emitExprInto(E->getPatternExpr(), eltInit);
    });
  });

  init->finishInitialization(SGF);

  return RValue::forInContext();
}

RValue
RValueEmitter::visitPackElementExpr(PackElementExpr *E, SGFContext C) {
  // If this is a captured pack element reference, just emit the parameter value
  // that was passed to the closure.
  auto found = SGF.OpaqueValues.find(E);
  if (found != SGF.OpaqueValues.end())
    return RValue(SGF, E, SGF.manageOpaqueValue(found->second, E, C));

  // Otherwise, we're going to project the address of an element from the pack
  // itself.
  FormalEvaluationScope scope(SGF);

  LValue lv = SGF.emitLValue(E, SGFAccessKind::OwnedObjectRead);

  // Otherwise, we can't load at +0 without further analysis, since the formal
  // access into the lvalue will end immediately.
  return SGF.emitLoadOfLValue(E, std::move(lv),
                              C.withFollowingSideEffects());
}

RValue
RValueEmitter::visitMaterializePackExpr(MaterializePackExpr *E, SGFContext C) {
  // Always emitted through `visitPackElementExpr`.
  llvm_unreachable("materialized pack outside of PackElementExpr");
}

RValue RValueEmitter::visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E,
                                                SGFContext C) {
  ManagedValue archetype = SGF.emitRValueAsSingleValue(E->getSubExpr());
  auto loweredTy = SGF.getLoweredLoadableType(E->getType());
  if (loweredTy == archetype.getType())
    return RValue(SGF, E, archetype);

  // Replace the cleanup with a new one on the superclass value so we always use
  // concrete retain/release operations.
  auto base = SGF.B.createUpcast(E, archetype, loweredTy);
  return RValue(SGF, E, base);
}

static bool isAnyClosureExpr(Expr *e) {
  return isa<AbstractClosureExpr>(e) || isa<CaptureListExpr>(e);
}

static ManagedValue emitCaptureListExpr(SILGenFunction &SGF,
                                        CaptureListExpr *e,
            llvm::function_ref<ManagedValue(AbstractClosureExpr *closure)> fn);

static ManagedValue emitAnyClosureExpr(SILGenFunction &SGF, Expr *e,
            llvm::function_ref<ManagedValue(AbstractClosureExpr *closure)> fn) {
  if (auto closure = dyn_cast<AbstractClosureExpr>(e)) {
    return fn(closure);
  } else if (auto captures = dyn_cast<CaptureListExpr>(e)) {
    return emitCaptureListExpr(SGF, captures, fn);
  } else {
    llvm_unreachable("not a closure expression!");
  }
}

static ManagedValue convertCFunctionSignature(SILGenFunction &SGF,
                                              FunctionConversionExpr *e,
                                              SILType loweredResultTy,
                                llvm::function_ref<ManagedValue ()> fnEmitter) {
  SILType loweredDestTy = SGF.getLoweredType(e->getType());
  ManagedValue result;

  // We're converting between C function pointer types. They better be
  // ABI-compatible, since we can't emit a thunk.
  switch (SGF.SGM.Types.checkForABIDifferences(SGF.SGM.M,
                                               loweredResultTy, loweredDestTy)){
  case TypeConverter::ABIDifference::CompatibleRepresentation:
  case TypeConverter::ABIDifference::CompatibleCallingConvention:
    result = fnEmitter();
    assert(result.getType() == loweredResultTy);

    if (loweredResultTy != loweredDestTy) {
      assert(!result.hasCleanup());
      result = SGF.B.createConvertFunction(e, result, loweredDestTy);
    }

    break;

  case TypeConverter::ABIDifference::NeedsThunk:
    // Note: in this case, we don't call the emitter at all -- doing so
    // just runs the risk of tripping up asserts in SILGenBridging.cpp
    SGF.SGM.diagnose(e, diag::unsupported_c_function_pointer_conversion,
                     e->getSubExpr()->getType(), e->getType());
    result = SGF.emitUndef(loweredDestTy);
    break;

  case TypeConverter::ABIDifference::CompatibleCallingConvention_ThinToThick:
  case TypeConverter::ABIDifference::CompatibleRepresentation_ThinToThick:
    llvm_unreachable("Cannot have thin to thick conversion here");
  }

  return result;
}

static
ManagedValue emitCFunctionPointer(SILGenFunction &SGF,
                                  FunctionConversionExpr *conversionExpr) {
  auto expr = conversionExpr->getSubExpr();
  
  // Look through base-ignored exprs to get to the function ref.
  auto semanticExpr = expr->getSemanticsProvidingExpr();
  while (auto ignoredBase = dyn_cast<DotSyntaxBaseIgnoredExpr>(semanticExpr)){
    SGF.emitIgnoredExpr(ignoredBase->getLHS());
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
    loc = declRef.getDecl();
  };
  
  if (auto conv = dyn_cast<FunctionConversionExpr>(semanticExpr)) {
    // There might be an intermediate conversion adding or removing @Sendable.
#ifndef NDEBUG
    {
      auto ty1 = conv->getType()->castTo<AnyFunctionType>();
      auto ty2 = conv->getSubExpr()->getType()->castTo<AnyFunctionType>();
      assert(ty1->withExtInfo(ty1->getExtInfo().withSendable(false))
             ->isEqual(ty2->withExtInfo(ty2->getExtInfo().withSendable(false))));
    }
#endif
    semanticExpr = conv->getSubExpr()->getSemanticsProvidingExpr();
  }
  
  if (auto declRef = dyn_cast<DeclRefExpr>(semanticExpr)) {
    setLocFromConcreteDeclRef(declRef->getDeclRef());
  } else if (auto memberRef = dyn_cast<MemberRefExpr>(semanticExpr)) {
    setLocFromConcreteDeclRef(memberRef->getMember());
  } else if (isAnyClosureExpr(semanticExpr)) {
    (void) emitAnyClosureExpr(SGF, semanticExpr,
                              [&](AbstractClosureExpr *closure) {
      // Emit the closure body.
      SGF.SGM.emitClosure(closure, SGF.getClosureTypeInfo(closure));

      loc = closure;
      return ManagedValue();
    });
  } else {
    llvm_unreachable("c function pointer converted from a non-concrete decl ref");
  }

  // Produce a reference to the C-compatible entry point for the function.
  SILDeclRef constant(loc, /*foreign*/ true);
  SILConstantInfo constantInfo =
      SGF.getConstantInfo(SGF.getTypeExpansionContext(), constant);

  // C function pointers cannot capture anything from their context.
  auto captures = SGF.SGM.Types.getLoweredLocalCaptures(constant);

  // Catch cases like:
  //   func g(_ : @convention(c) () -> ()) {}
  //   func q() { let z = 0; func r() { print(z) }; g(r); } // error
  // (See also: [NOTE: diagnose-swift-to-c-convention-change])
  if (!captures.getCaptures().empty() ||
      captures.hasGenericParamCaptures() ||
      captures.hasDynamicSelfCapture() ||
      captures.hasOpaqueValueCapture()) {
    unsigned kind = 0;
    if (captures.hasGenericParamCaptures())
      kind = 1;
    else if (captures.hasDynamicSelfCapture())
      kind = 2;
    SGF.SGM.diagnose(expr->getLoc(),
                     diag::c_function_pointer_from_function_with_context,
                     /*closure*/ constant.hasClosureExpr(),
                     kind);

    auto loweredTy = SGF.getLoweredType(conversionExpr->getType());
    return SGF.emitUndef(loweredTy);
  }

  return convertCFunctionSignature(
                    SGF, conversionExpr,
                    constantInfo.getSILType(),
                    [&]() -> ManagedValue {
                      SILValue cRef = SGF.emitGlobalFunctionRef(expr, constant);
                      return ManagedValue::forObjectRValueWithoutOwnership(
                          cRef);
                    });
}

// Change the representation without changing the signature or
// abstraction level.
static ManagedValue convertFunctionRepresentation(SILGenFunction &SGF,
                                                  SILLocation loc,
                                                  ManagedValue source,
                                            CanAnyFunctionType sourceFormalTy,
                                            CanAnyFunctionType resultFormalTy) {
  auto sourceTy = source.getType().castTo<SILFunctionType>();
  CanSILFunctionType resultTy =
    SGF.getLoweredType(resultFormalTy).castTo<SILFunctionType>();

  // Note that conversions to and from block require a thunk
  switch (resultFormalTy->getRepresentation()) {

  // Convert thin, c, block => thick
  case AnyFunctionType::Representation::Swift: {
    switch (sourceTy->getRepresentation()) {
    case SILFunctionType::Representation::Thin: {
      auto v = SGF.B.createThinToThickFunction(
          loc, source.getValue(),
          SILType::getPrimitiveObjectType(
            sourceTy->getWithRepresentation(
              SILFunctionTypeRepresentation::Thick)));
      // FIXME: what if other reabstraction is required?
      return ManagedValue::forOwnedRValue(v, source.getCleanup());
    }
    case SILFunctionType::Representation::Thick:
      llvm_unreachable("should not try thick-to-thick repr change");
    case SILFunctionType::Representation::CFunctionPointer:
    case SILFunctionType::Representation::Block:
      return SGF.emitBlockToFunc(loc, source, sourceFormalTy, resultFormalTy,
                                 resultTy);
    case SILFunctionType::Representation::Method:
    case SILFunctionType::Representation::Closure:
    case SILFunctionType::Representation::ObjCMethod:
    case SILFunctionType::Representation::WitnessMethod:
    case SILFunctionType::Representation::CXXMethod:
    case SILFunctionType::Representation::KeyPathAccessorGetter:
    case SILFunctionType::Representation::KeyPathAccessorSetter:
    case SILFunctionType::Representation::KeyPathAccessorEquals:
    case SILFunctionType::Representation::KeyPathAccessorHash:
      llvm_unreachable("should not do function conversion from method rep");
    }
    llvm_unreachable("bad representation");
  }

  // Convert thin, thick, c => block
  case AnyFunctionType::Representation::Block:
    switch (sourceTy->getRepresentation()) {
    case SILFunctionType::Representation::Thin: {
      // Make thick first.
      auto v = SGF.B.createThinToThickFunction(
          loc, source.getValue(),
          SILType::getPrimitiveObjectType(
            sourceTy->getWithRepresentation(
              SILFunctionTypeRepresentation::Thick)));
      source = ManagedValue::forOwnedRValue(v, source.getCleanup());
      LLVM_FALLTHROUGH;
    }
    case SILFunctionType::Representation::Thick:
    case SILFunctionType::Representation::CFunctionPointer:
      // Convert to a block.
      return SGF.emitFuncToBlock(loc, source, sourceFormalTy, resultFormalTy,
                                 resultTy);
    case SILFunctionType::Representation::Block:
      llvm_unreachable("should not try block-to-block repr change");
    case SILFunctionType::Representation::Method:
    case SILFunctionType::Representation::Closure:
    case SILFunctionType::Representation::ObjCMethod:
    case SILFunctionType::Representation::WitnessMethod:
    case SILFunctionType::Representation::CXXMethod:
    case SILFunctionType::Representation::KeyPathAccessorGetter:
    case SILFunctionType::Representation::KeyPathAccessorSetter:
    case SILFunctionType::Representation::KeyPathAccessorEquals:
    case SILFunctionType::Representation::KeyPathAccessorHash:
      llvm_unreachable("should not do function conversion from method rep");
    }
    llvm_unreachable("bad representation");

  // Unsupported
  case AnyFunctionType::Representation::Thin:
    llvm_unreachable("should not do function conversion to thin");
  case AnyFunctionType::Representation::CFunctionPointer:
    llvm_unreachable("should not do C function pointer conversion here");
  }
  llvm_unreachable("bad representation");
}

RValue RValueEmitter::visitFunctionConversionExpr(FunctionConversionExpr *e,
                                                  SGFContext C)
{
  CanAnyFunctionType srcType =
      cast<FunctionType>(e->getSubExpr()->getType()->getCanonicalType());
  CanAnyFunctionType destType =
      cast<FunctionType>(e->getType()->getCanonicalType());

  if (destType->getRepresentation() ==
      FunctionTypeRepresentation::CFunctionPointer) {
    ManagedValue result;

    if (srcType->getRepresentation() !=
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
  
  // If the function being converted is a closure literal, then the only use
  // of the closure should be as the destination type of the conversion. Rather
  // than emit the closure as is and convert it, see if we can emit the closure
  // directly as the desired type.
  //
  // TODO: Move this up when we can emit closures directly with C calling
  // convention.
  auto subExpr = e->getSubExpr()->getSemanticsProvidingExpr();
  // Look through `as` type ascriptions that don't induce bridging too.
  while (auto subCoerce = dyn_cast<CoerceExpr>(subExpr)) {
    // Coercions that introduce bridging aren't simple type ascriptions.
    // (Maybe we could still peephole through them eventually, though, by
    // performing the bridging in the closure prolog/epilog and/or emitting
    // the closure with the correct contextual block/closure/C function pointer
    // representation.)
    if (!subCoerce->getSubExpr()->getType()->isEqual(subCoerce->getType())) {
      break;
    }
    subExpr = subCoerce->getSubExpr()->getSemanticsProvidingExpr();
  }

  assert(subExpr->getType()->getCanonicalType() == srcType &&
         "looked through a type change?");

  // If the subexpression is a closure, emit it in a converting context.
  // The emission of the closure expression will decide whether we can
  // actually apply the conversion directly when emitting the closure.
  //
  // We don't allow representation changes on this path, although we
  // probably could.
  if (isAnyClosureExpr(subExpr) &&
      destType->getRepresentation() == srcType->getRepresentation()) {
    auto loweredDestTy = SGF.getLoweredType(destType);
    auto conversion = Conversion::getSubtype(srcType, destType, loweredDestTy);
    auto closure = SGF.emitConvertedRValue(subExpr, conversion, C);
    return RValue(SGF, e, closure);
  }
  
  // Handle a reference to a "thin" native Swift function that only changes
  // representation and refers to an inherently thin function reference.
  // FIXME: this definitely should not be completely replacing the ExtInfo.
  if (destType->getRepresentation() == FunctionTypeRepresentation::Thin) {
    if (srcType->getRepresentation() == FunctionTypeRepresentation::Swift
        && srcType->withExtInfo(destType->getExtInfo())->isEqual(destType)) {
      auto value = SGF.emitRValueAsSingleValue(e->getSubExpr());
      auto expectedTy = SGF.getLoweredType(destType);
      if (auto thinToThick =
            dyn_cast<ThinToThickFunctionInst>(value.getValue())) {
        value = ManagedValue::forObjectRValueWithoutOwnership(
            thinToThick->getOperand());
      } else {
        SGF.SGM.diagnose(e->getLoc(), diag::not_implemented,
                         "nontrivial thin function reference");
        value = SGF.emitUndef(expectedTy);
      }
      
      if (value.getType() != expectedTy) {
        SGF.SGM.diagnose(e->getLoc(), diag::not_implemented,
                         "nontrivial thin function reference");
        value = SGF.emitUndef(expectedTy);
      }
      return RValue(SGF, e, value);
    }
  }

  // Break the conversion into three stages:
  // 1) changing the representation from foreign to native
  // 2) changing the signature within the representation
  // 3) changing the representation from native to foreign
  //
  // We only do one of 1) or 3), but we have to do them in the right order
  // with respect to 2).

  CanAnyFunctionType stage1Type = srcType;
  CanAnyFunctionType stage2Type = destType;

  switch(srcType->getRepresentation()) {
  case AnyFunctionType::Representation::Swift:
  case AnyFunctionType::Representation::Thin:
    // Source is native, so we can convert signature first.
    stage2Type = adjustFunctionType(destType, srcType->getRepresentation(),
                                    srcType->getClangTypeInfo());
    break;
  case AnyFunctionType::Representation::Block:
  case AnyFunctionType::Representation::CFunctionPointer:
    // Source is foreign, so do the representation change first.
    stage1Type = adjustFunctionType(srcType, destType->getRepresentation(),
                                    destType->getClangTypeInfo());
  }

  auto result = SGF.emitRValueAsSingleValue(e->getSubExpr());

  if (srcType != stage1Type)
    result = convertFunctionRepresentation(SGF, e, result, srcType, stage1Type);

  if (stage1Type != stage2Type) {
    result = SGF.emitTransformedValue(e, result, stage1Type, stage2Type,
                                      SGFContext());
  }

  if (stage2Type != destType)
    result = convertFunctionRepresentation(SGF, e, result, stage2Type, destType);

  return RValue(SGF, e, result);
}

RValue RValueEmitter::visitCovariantFunctionConversionExpr(
                        CovariantFunctionConversionExpr *e,
                        SGFContext C) {
  ManagedValue original = SGF.emitRValueAsSingleValue(e->getSubExpr());
  CanAnyFunctionType destTy
    = cast<AnyFunctionType>(e->getType()->getCanonicalType());
  SILType resultType = SGF.getLoweredType(destTy);
  SILValue result =
      SGF.B.createConvertFunction(e, original.forward(SGF), resultType,
                                  /*Withoutactuallyescaping=*/false);
  return RValue(SGF, e, SGF.emitManagedRValueWithCleanup(result));
}

RValue RValueEmitter::visitCovariantReturnConversionExpr(
                        CovariantReturnConversionExpr *e,
                        SGFContext C) {
  ManagedValue original = SGF.emitRValueAsSingleValue(e->getSubExpr());
  SILType resultType = SGF.getLoweredType(e->getType());

  // DynamicSelfType lowers as its self type, so no SIL-level conversion
  // is required in this case.
  if (resultType == original.getType())
    return RValue(SGF, e, original);

  ManagedValue result = SGF.B.createUncheckedRefCast(e, original, resultType);

  return RValue(SGF, e, result);
}

RValue RValueEmitter::visitActorIsolationErasureExpr(ActorIsolationErasureExpr *E,
                                                     SGFContext C) {
  auto loc = SILLocation(E).asAutoGenerated();
  auto funcRef = SGF.emitRValueAsSingleValue(E->getSubExpr());

  auto isolatedType =
      cast<AnyFunctionType>(E->getSubExpr()->getType()->getCanonicalType());
  auto nonIsolatedType =
      cast<AnyFunctionType>(E->getType()->getCanonicalType());

  return RValue(SGF, E,
                SGF.emitActorIsolationErasureThunk(loc, funcRef, isolatedType,
                                                   nonIsolatedType));
}

RValue RValueEmitter::visitExtractFunctionIsolationExpr(
    ExtractFunctionIsolationExpr *E, SGFContext C) {
  auto arg = SGF.emitRValue(E->getFunctionExpr());
  auto result = SGF.emitExtractFunctionIsolation(
      E, ArgumentSource(E, std::move(arg)), C);
  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitUnsafeCastExpr(UnsafeCastExpr *E, SGFContext C) {
  ManagedValue original = SGF.emitRValueAsSingleValue(E->getSubExpr());
  SILType resultType = SGF.getLoweredType(E->getType());

  if (resultType == original.getType())
    return RValue(SGF, E, original);

  ManagedValue result;
  if (original.getType().isAddress()) {
    ASSERT(resultType.isAddress());
    result = SGF.B.createUncheckedAddrCast(E, original, resultType);
  } else {
    result = SGF.B.createUncheckedForwardingCast(E, original, resultType);
  }

  return RValue(SGF, E, result);
}

RValue RValueEmitter::visitErasureExpr(ErasureExpr *E, SGFContext C) {
  if (auto result = tryEmitAsBridgingConversion(SGF, E, false, C)) {
    return RValue(SGF, E, *result);
  }

  auto &existentialTL = SGF.getTypeLowering(E->getType());
  auto concreteFormalType = E->getSubExpr()->getType()->getCanonicalType();

  auto archetype = OpenedArchetypeType::getAny(E->getType()->getCanonicalType());
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
    return emitUndefRValue(loc, getASTContext().getAnyHashableType());

  // Construct the substitution for T: Hashable.
  auto subMap = SubstitutionMap::getProtocolSubstitutions(
      conformance.getRequirement(), type, conformance);

  return emitApplyOfLibraryIntrinsic(loc, convertFn, subMap, value, C);
}

RValue RValueEmitter::visitAnyHashableErasureExpr(AnyHashableErasureExpr *E,
                                                  SGFContext C) {
  // Emit the source value into a temporary.
  auto sourceOrigType = AbstractionPattern::getOpaque();
  auto subExpr = E->getSubExpr();
  auto &sourceOrigTL = SGF.getTypeLowering(sourceOrigType, subExpr->getType());
  auto source = SGF.silConv.useLoweredAddresses()
                    ? SGF.emitMaterializedRValueAsOrig(subExpr, sourceOrigType)
                    : SGF.emitRValueAsOrig(subExpr, sourceOrigType,
                                           sourceOrigTL, SGFContext());

  return SGF.emitAnyHashableErasure(E, source, subExpr->getType(),
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
    return ManagedValue::forRValueWithoutOwnership(value.getValue());

  // If it's an object...
  if (valueTy.isObject()) {
    // See if we have more accurate information from the ownership kind. This
    // detects trivial cases of enums.
    if (value.getOwnershipKind() == OwnershipKind::None)
      return ManagedValue::forObjectRValueWithoutOwnership(value.getValue());

    // Otherwise, copy the value and return.
    return value.getFinalManagedValue().copy(*this, loc);
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
  ProfileCounter trueCount = ProfileCounter();
  ProfileCounter falseCount = ProfileCounter();
  auto parent = SGF.getPGOParent(E);
  if (parent) {
    auto &Node = parent.value();
    auto *NodeS = Node.get<Stmt *>();
    if (auto *IS = dyn_cast<IfStmt>(NodeS)) {
      trueCount = SGF.loadProfilerCount(IS->getThenStmt());
      if (auto *ElseStmt = IS->getElseStmt()) {
        falseCount = SGF.loadProfilerCount(ElseStmt);
      }
    }
  }
  ManagedValue operand = SGF.emitRValueAsSingleValue(E->getSubExpr());
  return emitConditionalCheckedCast(SGF, E, operand, E->getSubExpr()->getType(),
                                    E->getType(), E->getCastKind(), C,
                                    trueCount, falseCount);
}

static RValue emitBoolLiteral(SILGenFunction &SGF, SILLocation loc,
                              SILValue builtinBool,
                              SGFContext C) {
  // Call the Bool(_builtinBooleanLiteral:) initializer
  ASTContext &ctx = SGF.getASTContext();
  auto init = ctx.getBoolBuiltinInitDecl();
  auto builtinArgType = CanType(BuiltinIntegerType::get(1, ctx));
  RValue builtinArg(SGF,
                    ManagedValue::forObjectRValueWithoutOwnership(builtinBool),
                    builtinArgType);

  PreparedArguments builtinArgs((AnyFunctionType::Param(builtinArgType)));
  builtinArgs.add(loc, std::move(builtinArg));

  auto result =
    SGF.emitApplyAllocatingInitializer(loc, ConcreteDeclRef(init),
                                       std::move(builtinArgs), Type(),
                                       C);
  return result;
}
RValue RValueEmitter::visitIsExpr(IsExpr *E, SGFContext C) {
  SILValue isa = emitIsa(SGF, E, E->getSubExpr(),
                         E->getCastType(), E->getCastKind());
  return emitBoolLiteral(SGF, E, isa, C);
}

RValue RValueEmitter::visitEnumIsCaseExpr(EnumIsCaseExpr *E,
                                          SGFContext C) {
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
  
  return emitBoolLiteral(SGF, E, selected, C);
}

RValue RValueEmitter::visitSingleValueStmtExpr(SingleValueStmtExpr *E,
                                               SGFContext C) {
  auto emitStmt = [&]() {
    SGF.emitStmt(E->getStmt());

    // A switch of an uninhabited value gets emitted as an unreachable. In that
    // case we need to emit a block to emit the rest of the expression code
    // into. This block will be unreachable, so will be eliminated by the
    // SILOptimizer. This is easier than handling unreachability throughout
    // expression emission, as eventually SingleValueStmtExprs ought to be able
    // to appear in arbitrary expression position. The rest of the emission
    // will reference the uninitialized temporary variable, but that's fine
    // because it'll be eliminated.
    if (!SGF.B.hasValidInsertionPoint())
      SGF.B.emitBlock(SGF.createBasicBlock());
  };

  // A void SingleValueStmtExpr either only has Void expression branches, or
  // we've decided that it should have purely statement semantics. In either
  // case, we can just emit the statement as-is, and produce the void rvalue.
  if (E->getType()->isVoid()) {
    emitStmt();
    return SGF.emitEmptyTupleRValue(E, C);
  }
  auto &lowering = SGF.getTypeLowering(E->getType());
  auto resultAddr = SGF.emitTemporaryAllocation(E, lowering.getLoweredType());

  // Collect the target exprs that will be used for initialization.
  SmallVector<Expr *, 4> scratch;
  SILGenFunction::SingleValueStmtInitialization initInfo(resultAddr);
  for (auto *E : E->getResultExprs(scratch))
    initInfo.Exprs.insert(E);

  // Push the initialization for branches of the statement to initialize into.
  SGF.SingleValueStmtInitStack.push_back(std::move(initInfo));
  SWIFT_DEFER { SGF.SingleValueStmtInitStack.pop_back(); };
  emitStmt();
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(resultAddr));
}

RValue RValueEmitter::visitCoerceExpr(CoerceExpr *E, SGFContext C) {
  if (auto result = tryEmitAsBridgingConversion(SGF, E->getSubExpr(), true, C))
    return RValue(SGF, E, *result);

  return visit(E->getSubExpr(), C);
}

RValue RValueEmitter::visitUnderlyingToOpaqueExpr(UnderlyingToOpaqueExpr *E,
                                                  SGFContext C) {
  // The opaque type has the layout of the underlying type, abstracted as
  // a type parameter.
  auto &opaqueTL = SGF.getTypeLowering(E->getType());
  auto &underlyingTL = SGF.getTypeLowering(AbstractionPattern::getOpaque(),
                                           E->getSubExpr()->getType());
  
  auto &underlyingSubstTL = SGF.getTypeLowering(E->getSubExpr()->getType());

  if (underlyingSubstTL.getLoweredType() == opaqueTL.getLoweredType()) {
    return SGF.emitRValue(E->getSubExpr(), C);
  }

  // If the opaque type is address only, initialize in place.
  if (opaqueTL.getLoweredType().isAddress()) {
    auto opaqueAddr = SGF.getBufferForExprResult(
                                               E, opaqueTL.getLoweredType(), C);
    // Initialize the buffer as the underlying type.
    auto underlyingAddr = SGF.B.createUncheckedAddrCast(E,
                                opaqueAddr,
                                underlyingTL.getLoweredType().getAddressType());
    
    auto underlyingInit = SGF.useBufferAsTemporary(underlyingAddr, underlyingTL);

    // Try to emit directly into the buffer if no reabstraction is necessary.
    ManagedValue underlying;
    if (underlyingSubstTL.getLoweredType() == underlyingTL.getLoweredType()) {
      underlying = SGF.emitRValueAsSingleValue(E->getSubExpr(),
                                              SGFContext(underlyingInit.get()));
    } else {
      // Otherwise, emit the underlying value then bring it to the right
      // abstraction level.
      underlying = SGF.emitRValueAsSingleValue(E->getSubExpr());
      underlying = SGF.emitSubstToOrigValue(E, underlying,
                                AbstractionPattern::getOpaque(),
                                E->getSubExpr()->getType()->getCanonicalType());
    }
    if (!underlying.isInContext()) {
      underlyingInit->copyOrInitValueInto(SGF, E, underlying, /*init*/ true);
      underlyingInit->finishInitialization(SGF);
    }
    // Kill the cleanup on the underlying value, and hand off the opaque buffer
    // as the result.
    underlyingInit->getManagedAddress().forward(SGF);
    
    auto opaque = SGF.manageBufferForExprResult(opaqueAddr, opaqueTL, C);
    return RValue(SGF, E, opaque);
  }
  
  // If the opaque type is loadable, emit the subexpression and bitcast it.
  auto value = SGF.emitRValueAsSingleValue(E->getSubExpr());
  if (underlyingSubstTL.getLoweredType() != underlyingTL.getLoweredType()) {
    value = SGF.emitSubstToOrigValue(E, value, AbstractionPattern::getOpaque(),
                                E->getSubExpr()->getType()->getCanonicalType());
  }

  if (value.getType() == opaqueTL.getLoweredType())
    return RValue(SGF, E, value);

  auto cast = SGF.B.createUncheckedBitCast(E, value,
                                           opaqueTL.getLoweredType());
  return RValue(SGF, E, cast);
}

RValue RValueEmitter::visitUnreachableExpr(UnreachableExpr *E, SGFContext C) {
  // Emit the expression, followed by an unreachable. To produce a value of
  // arbitrary type, we emit a temporary allocation, with the use of the
  // allocation in the unreachable block. The SILOptimizer will eliminate both
  // the unreachable block and unused allocation.
  SGF.emitIgnoredExpr(E->getSubExpr());

  auto &lowering = SGF.getTypeLowering(E->getType());
  auto resultAddr = SGF.emitTemporaryAllocation(E, lowering.getLoweredType());

  SGF.B.createUnreachable(E);
  SGF.B.emitBlock(SGF.createBasicBlock());

  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(resultAddr));
}

VarargsInfo Lowering::emitBeginVarargs(SILGenFunction &SGF, SILLocation loc,
                                       CanType baseTy, CanType arrayTy,
                                       unsigned numElements) {
  // Reabstract the base type against the array element type.
  auto baseAbstraction = AbstractionPattern::getOpaque();
  auto &baseTL = SGF.getTypeLowering(baseAbstraction, baseTy);

  // Allocate the array.
  SILValue numEltsVal = SGF.B.createIntegerLiteral(loc,
                             SILType::getBuiltinWordType(SGF.getASTContext()),
                             numElements);
  // The first result is the array value.
  ManagedValue array;
  // The second result is a RawPointer to the base address of the array.
  SILValue basePtr;
  std::tie(array, basePtr)
    = SGF.emitUninitializedArrayAllocation(arrayTy, numEltsVal, loc);

  // Temporarily deactivate the main array cleanup.
  if (array.hasCleanup())
    SGF.Cleanups.setCleanupState(array.getCleanup(), CleanupState::Dormant);

  // Push a new cleanup to deallocate the array.
  auto abortCleanup =
    SGF.enterDeallocateUninitializedArrayCleanup(array.getValue());

  // Turn the pointer into an address.
  basePtr = SGF.B.createPointerToAddress(
    loc, basePtr, baseTL.getLoweredType().getAddressType(),
    /*isStrict*/ true,
    /*isInvariant*/ false);

  return VarargsInfo(array, abortCleanup, basePtr, baseTL, baseAbstraction);
}

ManagedValue Lowering::emitEndVarargs(SILGenFunction &SGF, SILLocation loc,
                                      VarargsInfo &&varargs,
                                      unsigned numElements) {
  // Kill the abort cleanup.
  SGF.Cleanups.setCleanupState(varargs.getAbortCleanup(), CleanupState::Dead);

  // Reactivate the result cleanup.
  auto array = varargs.getArray();
  if (array.hasCleanup())
    SGF.Cleanups.setCleanupState(array.getCleanup(), CleanupState::Active);

  // Array literals only need to be finalized, if the array is really allocated.
  // In case of zero elements, no allocation is done, but the empty-array
  // singleton is used. "Finalization" means to emit an end_cow_mutation
  // instruction on the array. As the empty-array singleton is a read-only and
  // shared object, it's not legal to do a end_cow_mutation on it.
  if (numElements == 0)
    return array;
    
  return SGF.emitUninitializedArrayFinalization(loc, std::move(array));
}

RValue RValueEmitter::visitTupleExpr(TupleExpr *E, SGFContext C) {
  auto type = cast<TupleType>(E->getType()->getCanonicalType());

  // If we have an Initialization, emit the tuple elements into its elements.
  if (Initialization *I = C.getEmitInto()) {

    bool implodeTuple = false;

    if (I->canPerformInPlaceInitialization() &&
        I->isInPlaceInitializationOfGlobal() &&
        SGF.getLoweredType(type).isTrivial(SGF.F)) {
      // Implode tuples in initialization of globals if they are
      // of trivial types.
      implodeTuple = true;
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
      return RValue::forInContext();
    }
  }

  // If the tuple has a pack expansion in it, initialize an object in
  // memory (and recurse; this pattern should reliably enter the above,
  // though).
  if (type.containsPackExpansionType()) {
    auto &tupleTL = SGF.getTypeLowering(type);
    auto initialization = SGF.emitTemporary(E, tupleTL);
    {
      RValue result = visitTupleExpr(E, SGFContext(initialization.get()));
      assert(result.isInContext()); (void) result;
    }
    return RValue(SGF, E, type, initialization->getManagedAddress());
  }

  llvm::SmallVector<RValue, 8> tupleElts;
  bool hasAtleastOnePlusOneValue = false;
  for (Expr *elt : E->getElements()) {
    RValue RV = SGF.emitRValue(elt);
    hasAtleastOnePlusOneValue |= RV.isPlusOne(SGF);
    tupleElts.emplace_back(std::move(RV));
  }

  // Once we have found if we have any plus one arguments, add each element of
  // tuple elts into result, making sure each value is at plus 1.
  RValue result(type);
  if (hasAtleastOnePlusOneValue) {
    for (unsigned i : indices(tupleElts)) {
      result.addElement(std::move(tupleElts[i]).ensurePlusOne(SGF, E));
    }
  } else {
    for (unsigned i : indices(tupleElts)) {
      result.addElement(std::move(tupleElts[i]));
    }
  }

  return result;
}

RValue RValueEmitter::visitMemberRefExpr(MemberRefExpr *e,
                                         SGFContext resultCtx) {
  assert(!e->getType()->is<LValueType>() &&
         "RValueEmitter shouldn't be called on lvalues");
  assert(isa<VarDecl>(e->getMember().getDecl()));

  // Everything else should use the l-value logic.

  // Any writebacks for this access are tightly scoped.
  FormalEvaluationScope scope(SGF);

  LValue lv = SGF.emitLValue(e, SGFAccessKind::OwnedObjectRead);

  // Otherwise, we can't load at +0 without further analysis, since the formal
  // access into the lvalue will end immediately.
  return SGF.emitLoadOfLValue(e, std::move(lv),
                              resultCtx.withFollowingSideEffects());
}

RValue RValueEmitter::visitDynamicMemberRefExpr(DynamicMemberRefExpr *E,
                                                SGFContext C) {
  assert(!E->isImplicitlyAsync() && "an actor-isolated @objc member?");
  assert(!E->isImplicitlyThrows() && "an distributed-actor-isolated @objc member?");

  // Emit the operand (the base).
  SILValue Operand = SGF.emitRValueAsSingleValue(E->getBase()).getValue();

  // Emit the member reference.
  return SGF.emitDynamicMemberRef(E, Operand, E->getMember(),
                                  E->getType()->getCanonicalType(), C);
}

RValue RValueEmitter::
visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E, SGFContext C) {
  visit(E->getLHS());
  return visit(E->getRHS());
}

RValue RValueEmitter::visitSubscriptExpr(SubscriptExpr *E, SGFContext C) {
  // Any writebacks for this access are tightly scoped.
  FormalEvaluationScope scope(SGF);

  LValue lv = SGF.emitLValue(E, SGFAccessKind::OwnedObjectRead);
  // We can't load at +0 without further analysis, since the formal access into
  // the lvalue will end immediately.
  return SGF.emitLoadOfLValue(E, std::move(lv), C.withFollowingSideEffects());
}

RValue RValueEmitter::visitDynamicSubscriptExpr(
                                      DynamicSubscriptExpr *E, SGFContext C) {
  assert(!E->isImplicitlyAsync() && "an actor-isolated @objc member?");
  assert(!E->isImplicitlyThrows() && "an distributed-actor-isolated @objc member?");

  // Emit the base operand.
  SILValue Operand = SGF.emitRValueAsSingleValue(E->getBase()).getValue();

  // Emit the indices.
  //
  // FIXME: This is apparently not true for Swift @objc subscripts.
  // Objective-C subscripts only ever have a single parameter.
  Expr *IndexExpr = E->getArgs()->getUnaryExpr();
  assert(IndexExpr);

  PreparedArguments IndexArgs(
      FunctionType::Param(IndexExpr->getType()->getCanonicalType()));
  IndexArgs.add(E, SGF.emitRValue(IndexExpr));

  return SGF.emitDynamicSubscriptGetterApply(
      E, Operand, E->getMember(), std::move(IndexArgs),
      E->getType()->getCanonicalType(), C);
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
                                               bool implicitlyAsync,
                                               SGFContext C) {
  SILDeclRef generator 
    = SILDeclRef::getDefaultArgGenerator(defaultArgsOwner.getDecl(),
                                         destIndex);

  auto fnRef = ManagedValue::forObjectRValueWithoutOwnership(
      emitGlobalFunctionRef(loc, generator));
  auto fnType = fnRef.getType().castTo<SILFunctionType>();

  SubstitutionMap subs;
  if (fnType->isPolymorphic())
    subs = defaultArgsOwner.getSubstitutions();

  auto constantInfo = SGM.Types.getConstantInfo(
      TypeExpansionContext::minimal(), generator);
  AbstractionPattern origResultType =
      constantInfo.FormalPattern.getFunctionResultType();

  auto substFnType =
      fnType->substGenericArgs(SGM.M, subs, getTypeExpansionContext());

  CalleeTypeInfo calleeTypeInfo(substFnType, origResultType, resultType);
  ResultPlanPtr resultPtr =
      ResultPlanBuilder::computeResultPlan(*this, calleeTypeInfo, loc, C);
  ArgumentScope argScope(*this, loc);

  SmallVector<ManagedValue, 4> captures;
  emitCaptures(loc, generator, CaptureEmission::ImmediateApplication,
               captures);

  return emitApply(std::move(resultPtr), std::move(argScope), loc, fnRef, subs,
                   captures, calleeTypeInfo, ApplyOptions(), C, std::nullopt);
}

RValue SILGenFunction::emitApplyOfStoredPropertyInitializer(
    SILLocation loc,
    VarDecl *var,
    SubstitutionMap subs,
    CanType resultType,
    AbstractionPattern origResultType,
    SGFContext C) {

  SILDeclRef constant(var, SILDeclRef::Kind::StoredPropertyInitializer);
  auto fnRef = ManagedValue::forObjectRValueWithoutOwnership(
      emitGlobalFunctionRef(loc, constant));
  auto fnType = fnRef.getType().castTo<SILFunctionType>();

  auto substFnType =
      fnType->substGenericArgs(SGM.M, subs, getTypeExpansionContext());

  CalleeTypeInfo calleeTypeInfo(substFnType, origResultType, resultType);
  ResultPlanPtr resultPlan =
      ResultPlanBuilder::computeResultPlan(*this, calleeTypeInfo, loc, C);
  ArgumentScope argScope(*this, loc);
  return emitApply(std::move(resultPlan), std::move(argScope), loc, fnRef, subs,
                   {}, calleeTypeInfo, ApplyOptions(), C, std::nullopt);
}

RValue RValueEmitter::visitDestructureTupleExpr(DestructureTupleExpr *E,
                                                SGFContext C) {
  // Emit the sub-expression tuple and destructure it into elements.
  SmallVector<RValue, 4> elements;
  visit(E->getSubExpr()).extractElements(elements);

  // Bind each element of the input tuple to its corresponding
  // opaque value.
  for (unsigned i = 0, e = E->getDestructuredElements().size();
       i != e; ++i) {
    auto *opaqueElt = E->getDestructuredElements()[i];
    assert(!SGF.OpaqueValues.count(opaqueElt));

    auto opaqueMV = std::move(elements[i]).getAsSingleValue(SGF, E);
    SGF.OpaqueValues[opaqueElt] = opaqueMV;
  }

  // Emit the result expression written in terms of the above
  // opaque values.
  auto result = visit(E->getResultExpr(), C);

  // Clean up.
  for (unsigned i = 0, e = E->getDestructuredElements().size();
       i != e; ++i) {
    auto *opaqueElt = E->getDestructuredElements()[i];
    SGF.OpaqueValues.erase(opaqueElt);
  }

  return result;
}

static SILValue emitMetatypeOfDelegatingInitExclusivelyBorrowedSelf(
    SILGenFunction &SGF, SILLocation loc, DeclRefExpr *dre, SILType metaTy) {
  SGFContext ctx;
  auto *vd = cast<ParamDecl>(dre->getDecl());
  ManagedValue selfValue;

  Scope S(SGF, loc);
  std::optional<FormalEvaluationScope> FES;

  // If we have not exclusively borrowed self, we need to do so now.
  if (SGF.SelfInitDelegationState == SILGenFunction::WillExclusiveBorrowSelf) {
    // We need to use a full scope here to ensure that any underlying
    // "normal cleanup" borrows are cleaned up.
    selfValue = SGF.emitRValueAsSingleValue(dre);
  } else {
    // If we already exclusively borrowed self, then we need to emit self
    // using formal evaluation primitives.

    assert(SGF.SelfInitDelegationState ==
           SILGenFunction::DidExclusiveBorrowSelf);
    // This needs to be inlined since there is a Formal Evaluation Scope
    // in emitRValueForDecl that causing any borrow for this LValue to be
    // popped too soon.
    FES.emplace(SGF);
    CanType formalRValueType = dre->getType()->getCanonicalType();
    selfValue = SGF.emitAddressOfLocalVarDecl(dre, vd, formalRValueType,
                                              SGFAccessKind::OwnedObjectRead);
    selfValue = SGF.emitFormalEvaluationRValueForSelfInDelegationInit(
                       loc, formalRValueType,
                       selfValue.getLValueAddress(), ctx)
                    .getAsSingleValue(SGF, loc);
  }

  return SGF.B.createValueMetatype(loc, metaTy, selfValue.getValue());
}

SILValue SILGenFunction::emitMetatypeOfValue(SILLocation loc, Expr *baseExpr) {
  Type formalBaseType = baseExpr->getType()->getWithoutSpecifierType();
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
    if (inExclusiveBorrowSelfSection(SelfInitDelegationState)) {
      if (auto *dre = dyn_cast<DeclRefExpr>(baseExpr)) {
        if (isa<ParamDecl>(dre->getDecl()) &&
            dre->getDecl()->getName() == getASTContext().Id_self &&
            dre->getDecl()->isImplicit()) {
          return emitMetatypeOfDelegatingInitExclusivelyBorrowedSelf(
              *this, loc, dre, metaTy);
        }
      }
    }

    Scope S(*this, loc);
    auto base = emitRValueAsSingleValue(baseExpr, SGFContext::AllowImmediatePlusZero);
    return S.popPreservingValue(B.createValueMetatype(loc, metaTy, base))
        .getValue();
  }
  // Otherwise, ignore the base and return the static thin metatype.
  emitIgnoredExpr(baseExpr);
  return B.createMetatype(loc, metaTy);
}

RValue RValueEmitter::visitDynamicTypeExpr(DynamicTypeExpr *E, SGFContext C) {
  auto metatype = SGF.emitMetatypeOfValue(E, E->getBase());
  return RValue(SGF, E,
                ManagedValue::forObjectRValueWithoutOwnership(metatype));
}

RValue RValueEmitter::visitCaptureListExpr(CaptureListExpr *E, SGFContext C) {
  return RValue(SGF, E, emitCaptureListExpr(SGF, E, [&](AbstractClosureExpr *body) {
    return visitAbstractClosureExpr(body, C).getScalarValue();
  }));
}

static ManagedValue emitCaptureListExpr(SILGenFunction &SGF,
                                        CaptureListExpr *E,
    llvm::function_ref<ManagedValue(AbstractClosureExpr *)> operation) {
  // Ensure that weak captures are in a separate scope.
  DebugScope scope(SGF, CleanupLocation(E));
  // CaptureListExprs evaluate their bound variables, but they don't introduce
  // new ones that should be described in the debug info.
  bool generateDebugInfo = false;
  for (auto capture : E->getCaptureList())
    SGF.visitPatternBindingDecl(capture.PBD, generateDebugInfo);

  // Then they evaluate to their "body" (the underlying closure expression).
  return operation(E->getClosureBody());
}

/// Returns the wrapped value placeholder that is meant to be substituted
/// in for the given autoclosure. This autoclosure placeholder is created
/// when \c init(wrappedValue:) takes an autoclosure for the \c wrappedValue
/// parameter.
static PropertyWrapperValuePlaceholderExpr *
wrappedValueAutoclosurePlaceholder(const AbstractClosureExpr *e) {
  if (auto ace = dyn_cast<AutoClosureExpr>(e)) {
    if (auto ce = dyn_cast<CallExpr>(ace->getSingleExpressionBody())) {
      return dyn_cast<PropertyWrapperValuePlaceholderExpr>(ce->getFn());
    }
  }
  return nullptr;
}

/// Try to turn a contextual conversion into type information for a
/// specialized closure function emission.
static std::optional<FunctionTypeInfo>
tryGetSpecializedClosureTypeFromContext(CanAnyFunctionType closureType,
                                        const Conversion &conv) {
  // Note that the kinds of conversion we work on here have to be kinds
  // that we can call withSourceType on later.
  if (conv.getKind() == Conversion::Reabstract ||
      conv.getKind() == Conversion::Subtype) {
    // We don't care about the input type here; we'll be emitting that
    // based on the closure.
    auto destType = cast<AnyFunctionType>(conv.getResultType());
    auto origType =
      conv.getKind() == Conversion::Reabstract
        ? conv.getReabstractionOutputOrigType()
        : AbstractionPattern(destType);
    auto expectedTy = conv.getLoweredResultType().castTo<SILFunctionType>();
    return FunctionTypeInfo{origType, destType, expectedTy};
  }

  // No other kinds of conversion.
  return std::nullopt;
}

/// Whether the given abstraction pattern as an opaque thrown error.
static bool hasOpaqueThrownError(const AbstractionPattern &pattern) {
  if (auto thrownPattern = pattern.getFunctionThrownErrorType())
    return thrownPattern->isTypeParameterOrOpaqueArchetype();

  return false;
}

/// Given that a subtype conversion is possibly being applied to the
/// type of a closure, can we emit the closure function under this
/// conversion?
static bool canEmitClosureFunctionUnderConversion(
    CanAnyFunctionType literalFnType, CanAnyFunctionType convertedFnType) {

  // Is it an identity conversion?
  if (literalFnType == convertedFnType) {
    return true;
  }

  // Are the types equivalent aside from effects (throws) or coeffects
  // (escaping)? Then we should emit the literal as having the destination type
  // (co)effects, even if it doesn't exercise them.
  //
  // TODO: We could also in principle let `async` through here, but that
  // interferes with the implementation of `reasync`.
  auto literalWithoutEffects = literalFnType->getExtInfo().intoBuilder()
    .withNoEscape(false)
    .withSendable(false)
    .withThrows(false, Type());

  auto convertedWithoutEffects = convertedFnType->getExtInfo().intoBuilder()
    .withNoEscape(false)
    .withSendable(false)
    .withThrows(false, Type());

  // If the converted type has erased isolation, remove the isolation from
  // both types.
  if (convertedWithoutEffects.getIsolationKind() ==
        FunctionTypeIsolation::Kind::Erased) {
    auto nonIsolation = FunctionTypeIsolation::forNonIsolated();
    literalWithoutEffects = literalWithoutEffects.withIsolation(nonIsolation);
    convertedWithoutEffects = convertedWithoutEffects.withIsolation(nonIsolation);
  }

  if (literalFnType->withExtInfo(literalWithoutEffects.build())
        ->isEqual(convertedFnType->withExtInfo(convertedWithoutEffects.build()))) {
    return true;
  }

  return false;
}

/// Can we emit a closure with the given specialized type info?
///
/// TODO: ideally, our prolog/epilog emission would be able to handle
/// all possible subtype and reabstraction conversions.
static bool canEmitSpecializedClosureFunction(CanAnyFunctionType closureType,
                                        const FunctionTypeInfo &contextInfo) {
  auto destType = contextInfo.FormalType;

  // Require the closure's formal type to be closely related to the formal
  // type we're trying to convert it to.
  if (!canEmitClosureFunctionUnderConversion(closureType, destType))
    return false;

  // If the abstraction pattern has an abstract thrown error, we are
  // currently unable to emit the literal with a difference in the thrown
  // error type.
  if (hasOpaqueThrownError(contextInfo.OrigType) &&
      (closureType->isThrowing() != destType->isThrowing() ||
       closureType.getThrownError() != destType.getThrownError()))
    return false;

  return true;
}

/// Try to emit the given closure under the given conversion.
/// Returns an invalid ManagedValue if this fails.
ManagedValue RValueEmitter::tryEmitConvertedClosure(AbstractClosureExpr *e,
                                                    const Conversion &conv) {
  auto closureType = cast<AnyFunctionType>(e->getType()->getCanonicalType());

  // Bail out if we don't have specialized type information from context.
  auto info = tryGetSpecializedClosureTypeFromContext(closureType, conv);
  if (!info) return ManagedValue();

  // If we can emit the closure with all of the specialized type information,
  // that's great.
  if (canEmitSpecializedClosureFunction(closureType, *info)) {
    return emitClosureReference(e, *info);
  }

  // If we're converting to an `@isolated(any)` type, at least force the
  // closure to be emitted using the erased-isolation pattern so that
  // we don't lose that information.
  if (info->ExpectedLoweredType->hasErasedIsolation()) {
    // This assertion is why this isn't an infinite recursion.
    assert(!closureType->getIsolation().isErased() &&
           "closure cannot directly have erased isolation");

    // Construct a conversion that just erases isolation and doesn't make
    // any other changes to the closure type.
    auto erasedExtInfo = closureType->getExtInfo()
      .withIsolation(FunctionTypeIsolation::forErased());
    auto erasedClosureType = closureType.withExtInfo(erasedExtInfo);
    auto erasureInfo = SGF.getFunctionTypeInfo(erasedClosureType);

    // Emit the closure under that conversion.  This should always succeed.
    assert(canEmitSpecializedClosureFunction(closureType, erasureInfo));
    auto erasedResult = emitClosureReference(e, erasureInfo);

    // Narrow the original conversion to start from the erased closure type.
    auto convAfterErasure = conv.withSourceType(SGF, erasedClosureType);

    // Apply the narrowed conversion.
    return convAfterErasure.emit(SGF, e, erasedResult, SGFContext());
  }

  // Otherwise, give up.
  return ManagedValue();
}

RValue RValueEmitter::visitAbstractClosureExpr(AbstractClosureExpr *e,
                                               SGFContext C) {
  // Look through autoclosures that are just calls to a placeholder
  // expression.  TODO: this is just eta reduction; try to recognize
  // more situations for it.
  if (auto *placeholder = wrappedValueAutoclosurePlaceholder(e))
    return visitPropertyWrapperValuePlaceholderExpr(placeholder, C);

  // If we're emitting into a converting context, try to combine the
  // conversion into the emission of the closure function.
  if (auto *convertingInit = C.getAsConversion()) {
    ManagedValue closure =
        tryEmitConvertedClosure(e, convertingInit->getConversion());
    if (closure.isValid()) {
      convertingInit->initWithConvertedValue(SGF, e, closure);
      convertingInit->finishInitialization(SGF);
      return RValue::forInContext();
    }
  }

  // Otherwise, emit the expression using the simple type of the expression.
  auto info = SGF.getClosureTypeInfo(e);
  auto closure = emitClosureReference(e, info);

  return RValue(SGF, e, e->getType()->getCanonicalType(), closure);
}

ManagedValue
RValueEmitter::emitClosureReference(AbstractClosureExpr *e,
                                    const FunctionTypeInfo &contextInfo) {
  // Emit the closure body.
  SGF.SGM.emitClosure(e, contextInfo);

  // Generate the closure value (if any) for the closure expr's function
  // reference.
  SILLocation loc = e;
  return SGF.emitClosureValue(loc, SILDeclRef(e), contextInfo,
                              SubstitutionMap());
}

RValue RValueEmitter::
visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E,
                                   SGFContext C) {
  RValue interpolation;
  {
    TapExpr *ETap = E->getAppendingExpr();
    // Inlined from TapExpr:
    // TODO: This is only necessary because constant evaluation requires that
    // the box for the var gets defined before the initializer happens.
    auto Var = ETap->getVar();
    auto VarType = ETap->getType()->getCanonicalType();

    Scope outerScope(SGF, CleanupLocation(ETap));

    // Initialize the var with our SubExpr.
    auto VarInit =
        SGF.emitInitializationForVarDecl(Var, /*forceImmutable=*/false);
    {
      // Modified from TapExpr to evaluate the SubExpr directly rather than
      // indirectly through the OpaqueValue system.
      PreparedArguments builderInitArgs;
      RValue literalCapacity = visit(E->getLiteralCapacityExpr(), SGFContext());
      RValue interpolationCount =
          visit(E->getInterpolationCountExpr(), SGFContext());
      builderInitArgs.emplace(
          {AnyFunctionType::Param(literalCapacity.getType()),
           AnyFunctionType::Param(interpolationCount.getType())});
      builderInitArgs.add(E, std::move(literalCapacity));
      builderInitArgs.add(E, std::move(interpolationCount));
      RValue subexpr_result = SGF.emitApplyAllocatingInitializer(
          E, E->getBuilderInit(), std::move(builderInitArgs), Type(),
          SGFContext(VarInit.get()));
      if (!subexpr_result.isInContext()) {
        ArgumentSource(
            SILLocation(E),
            std::move(subexpr_result).ensurePlusOne(SGF, SILLocation(E)))
            .forwardInto(SGF, VarInit.get());
      }
    }

    // Emit the body and let it mutate the var if it chooses.
    SGF.emitStmt(ETap->getBody());

    // Retrieve and return the var, making it +1 so it survives the scope.
    auto result = SGF.emitRValueForDecl(SILLocation(ETap), Var, VarType,
                                        AccessSemantics::Ordinary, SGFContext());
    result = std::move(result).ensurePlusOne(SGF, SILLocation(ETap));
    interpolation = outerScope.popPreservingValue(std::move(result));
  }

  PreparedArguments resultInitArgs;
  resultInitArgs.emplace(AnyFunctionType::Param(interpolation.getType()));
  resultInitArgs.add(E, std::move(interpolation));

  return SGF.emitApplyAllocatingInitializer(
      E, E->getInitializer(), std::move(resultInitArgs), Type(), C);
}

RValue RValueEmitter::visitRegexLiteralExpr(RegexLiteralExpr *E, SGFContext C) {
  return SGF.emitLiteral(E, C);
}

RValue RValueEmitter::
visitObjectLiteralExpr(ObjectLiteralExpr *E, SGFContext C) {
  ConcreteDeclRef init = E->getInitializer();
  auto *decl = cast<ConstructorDecl>(init.getDecl());
  AnyFunctionType *fnTy = decl->getMethodInterfaceType()
                              .subst(init.getSubstitutions())
                              ->getAs<AnyFunctionType>();
  PreparedArguments args(fnTy->getParams(), E->getArgs());
  return SGF.emitApplyAllocatingInitializer(SILLocation(E), init,
                                            std::move(args), E->getType(), C);
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
        selectorMemberTy = var->getInterfaceType();
        break;
      }
    }
  }
  if (!selectorMemberTy) {
    SGF.SGM.diagnose(e, diag::objc_selector_malformed);
    return RValue(SGF, e, SGF.emitUndef(loweredSelectorTy));
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
  return RValue(SGF, e,
                ManagedValue::forObjectRValueWithoutOwnership(selectorValue));
}

static ManagedValue
emitKeyPathRValueBase(SILGenFunction &subSGF,
                      AbstractStorageDecl *storage,
                      SILLocation loc,
                      SILValue paramArg,
                      CanType &baseType,
                      SubstitutionMap subs) {
  // If the storage is at global scope, then the base value () is a formality.
  // There no real argument to pass to the underlying accessors.
  if (!storage->getDeclContext()->isTypeContext())
    return ManagedValue();

  auto paramOrigValue = paramArg->getType().isTrivial(subSGF.F)
                            ? ManagedValue::forRValueWithoutOwnership(paramArg)
                            : ManagedValue::forBorrowedRValue(paramArg);
  paramOrigValue = paramOrigValue.copy(subSGF, loc);
  auto paramSubstValue = subSGF.emitOrigToSubstValue(loc, paramOrigValue,
                                             AbstractionPattern::getOpaque(),
                                             baseType);

  // If base is a metatype, it cannot be opened as an existential or upcasted
  // from a class.
  if (baseType->is<MetatypeType>())
    return paramSubstValue;
  
  // Pop open an existential container base.
  if (baseType->isAnyExistentialType()) {
    // Use the opened archetype from the AST for a protocol member, or make a
    // new one (which we'll upcast immediately below) for a class member.
    ArchetypeType *opened;
    if (storage->getDeclContext()->getSelfClassDecl()) {
      opened = OpenedArchetypeType::get(baseType);
    } else {
      opened = subs.getReplacementTypes()[0]->castTo<ArchetypeType>();
    }
    assert(opened->isOpenedExistential());

    FormalEvaluationScope scope(subSGF);
    
    baseType = opened->getCanonicalType();
    auto openedOpaqueValue = subSGF.emitOpenExistential(loc, paramSubstValue,
                                                        subSGF.getLoweredType(baseType),
                                                        AccessKind::Read);
    // Maybe we could peephole this if we know the property load can borrow the
    // base value…
    paramSubstValue = openedOpaqueValue.ensurePlusOne(subSGF, loc);
  }
  
  // Upcast a class instance to the property's declared type if necessary.
  if (auto propertyClass = storage->getDeclContext()->getSelfClassDecl()) {
    if (baseType->getClassOrBoundGenericClass() != propertyClass) {
      baseType = baseType->getSuperclassForDecl(propertyClass)
        ->getCanonicalType();
      paramSubstValue = subSGF.B.createUpcast(loc, paramSubstValue,
                                     SILType::getPrimitiveObjectType(baseType));
    }
  }
  // …or pop open an existential container.
  return paramSubstValue;
}

using IndexTypePair = std::pair<CanType, SILType>;

/// Helper function to load the captured indexes out of a key path component
/// in order to invoke the accessors on that key path. A component with captured
/// indexes passes down a pointer to those captures to the accessor thunks,
/// which we can copy out of to produce values we can pass to the real
/// accessor functions.
static PreparedArguments
loadIndexValuesForKeyPathComponent(SILGenFunction &SGF, SILLocation loc,
                                   AbstractStorageDecl *storage,
                                   ArrayRef<IndexTypePair> indexes,
                                   SILValue pointer) {
  // If not a subscript, do nothing.
  if (!isa<SubscriptDecl>(storage))
    return PreparedArguments();

  SmallVector<AnyFunctionType::Param, 8> indexParams;
  for (auto &elt : indexes) {
    // FIXME: Varargs?
    indexParams.emplace_back(SGF.F.mapTypeIntoContext(elt.first));
  }
  
  PreparedArguments indexValues(indexParams);
  if (indexes.empty()) {
    assert(indexValues.isValid());
    return indexValues;
  }

  for (unsigned i : indices(indexes)) {
    SILValue eltAddr = pointer;
    if (indexes.size() > 1) {
      eltAddr = SGF.B.createTupleElementAddr(loc, eltAddr, i);
    }
    auto ty = SGF.F.mapTypeIntoContext(indexes[i].second);
    auto value = SGF.emitLoad(loc, eltAddr,
                              SGF.getTypeLowering(ty),
                              SGFContext(), IsNotTake);
    auto substType =
      SGF.F.mapTypeIntoContext(indexes[i].first)->getCanonicalType();
    indexValues.add(loc, RValue(SGF, loc, substType, value));
  }

  assert(indexValues.isValid());
  return indexValues;
}

static AccessorDecl *
getRepresentativeAccessorForKeyPath(AbstractStorageDecl *storage) {
  if (storage->requiresOpaqueGetter())
    return storage->getOpaqueAccessor(AccessorKind::Get);
  assert(storage->requiresOpaqueReadCoroutine());
  return storage->getOpaqueAccessor(AccessorKind::Read);
}

static CanType buildKeyPathIndicesTuple(ASTContext &C,
                                        ArrayRef<KeyPathPatternComponent::Index> indexes) {

  if (indexes.size() == 1) {
    return indexes[0].FormalType;
  }

  SmallVector<TupleTypeElt, 8> indicesElements;
  for (auto &elt : indexes) {
    indicesElements.emplace_back(elt.FormalType);
  }
  return TupleType::get(indicesElements, C)->getCanonicalType();
}

static SILFunction *getOrCreateKeyPathGetter(SILGenModule &SGM,
                         AbstractStorageDecl *property,
                         SubstitutionMap subs,
                         GenericEnvironment *genericEnv,
                         ResilienceExpansion expansion,
                         ArrayRef<IndexTypePair> indexes,
                         CanType baseType,
                         CanType propertyType) {
  // If the storage declaration is from a protocol, chase the override chain
  // back to the declaration whose getter introduced the witness table
  // entry.
  if (isa<ProtocolDecl>(property->getDeclContext())) {
    auto accessor = getRepresentativeAccessorForKeyPath(property);
    if (!accessor->requiresNewWitnessTableEntry()) {
      // Find the getter that does have a witness table entry.
      auto wtableAccessor =
        cast<AccessorDecl>(SILDeclRef::getOverriddenWitnessTableEntry(accessor));

      // Substitute the 'Self' type of the base protocol.
      subs = SILGenModule::mapSubstitutionsForWitnessOverride(
              accessor, wtableAccessor, subs);
      property = wtableAccessor->getStorage();
    }
  }

  auto genericSig =
      genericEnv ? genericEnv->getGenericSignature().getCanonicalSignature()
                 : nullptr;
  if (genericSig && genericSig->areAllParamsConcrete()) {
    genericSig = nullptr;
    genericEnv = nullptr;
  }

  // Build the signature of the thunk as expected by the keypath runtime.
  auto signature = [&]() {
    CanType loweredBaseTy, loweredPropTy;
    AbstractionPattern opaque = AbstractionPattern::getOpaque();

    loweredBaseTy = SGM.Types.getLoweredRValueType(
        TypeExpansionContext::minimal(), opaque, baseType);
    loweredPropTy = SGM.Types.getLoweredRValueType(
        TypeExpansionContext::minimal(), opaque, propertyType);
    
    auto paramConvention = ParameterConvention::Indirect_In_Guaranteed;

    SmallVector<SILParameterInfo, 2> params;
    params.push_back({loweredBaseTy, paramConvention});
    auto &C = SGM.getASTContext();

    if (!indexes.empty()) {
      if (indexes.size() > 1) {
        SmallVector<TupleTypeElt, 8> indicesElements;
        for (auto &elt : indexes) {
          indicesElements.emplace_back(elt.first);
        }
        auto indicesTupleTy = TupleType::get(indicesElements, C)->getCanonicalType();
        params.push_back({indicesTupleTy, paramConvention});
      } else {
        params.push_back({indexes[0].first, paramConvention});
      }
    }

    SILResultInfo result(loweredPropTy, ResultConvention::Indirect);

    return SILFunctionType::get(
        genericSig,
        SILFunctionType::ExtInfo().withRepresentation(
            SILFunctionType::Representation::KeyPathAccessorGetter),
        SILCoroutineKind::None, ParameterConvention::Direct_Unowned, params, {},
        result, std::nullopt, SubstitutionMap(), SubstitutionMap(),
        SGM.getASTContext());
  }();
  
  // Find the function and see if we already created it.
  auto name = Mangle::ASTMangler(SGM.getASTContext(), property)
    .mangleKeyPathGetterThunkHelper(property, genericSig, baseType,
                                    subs, expansion);
  auto loc = RegularLocation::getAutoGeneratedLocation();

  SILGenFunctionBuilder builder(SGM);
  auto thunk = builder.getOrCreateSharedFunction(
      loc, name, signature, IsBare, IsNotTransparent,
      (expansion == ResilienceExpansion::Minimal
       ? IsSerialized
       : IsNotSerialized),
      ProfileCounter(), IsThunk, IsNotDynamic, IsNotDistributed,
      IsNotRuntimeAccessible);
  if (!thunk->empty())
    return thunk;
  
  // Emit the thunk, which accesses the underlying property normally with
  // reabstraction where necessary.
  if (genericEnv) {
    baseType = genericEnv->mapTypeIntoContext(baseType)->getCanonicalType();
    propertyType = genericEnv->mapTypeIntoContext(propertyType)
      ->getCanonicalType();
    thunk->setGenericEnvironment(genericEnv);
  }
  
  SILGenFunction subSGF(SGM, *thunk, SGM.SwiftModule);

  signature = subSGF.F.getLoweredFunctionTypeInContext(
    subSGF.F.getTypeExpansionContext());

  auto entry = thunk->begin();
  auto resultArgTy =
      subSGF.silConv.getSILType(signature->getSingleResult(), signature,
                                subSGF.F.getTypeExpansionContext());
  auto baseArgTy =
      subSGF.silConv.getSILType(signature->getParameters()[0], signature,
                                subSGF.F.getTypeExpansionContext());
  if (genericEnv) {
    resultArgTy = genericEnv->mapTypeIntoContext(SGM.M, resultArgTy);
    baseArgTy = genericEnv->mapTypeIntoContext(SGM.M, baseArgTy);
  }
  SILFunctionArgument *resultArg = nullptr;
  if (SGM.M.useLoweredAddresses()) {
    resultArg = entry->createFunctionArgument(resultArgTy);
  }
  auto baseArg = entry->createFunctionArgument(baseArgTy);
  SILValue indexPtrArg;
  if (!indexes.empty()) {
    auto indexArgTy =
        subSGF.silConv.getSILType(signature->getParameters()[1], signature,
                                  subSGF.F.getTypeExpansionContext());
    if (genericEnv)
      indexArgTy = genericEnv->mapTypeIntoContext(SGM.M, indexArgTy);
    indexPtrArg = entry->createFunctionArgument(indexArgTy);
  }
  
  ArgumentScope scope(subSGF, loc);
  
  auto baseSubstValue = emitKeyPathRValueBase(subSGF, property,
                                               loc, baseArg,
                                               baseType, subs);
  auto subscriptIndices =
    loadIndexValuesForKeyPathComponent(subSGF, loc, property,
                                       indexes, indexPtrArg);

  ManagedValue resultSubst;
  {
    RValue resultRValue;

    // Emit a dynamic method branch if the storage decl is an @objc optional
    // requirement, or just a load otherwise.
    if (property->getAttrs().hasAttribute<OptionalAttr>()) {
      const auto declRef = ConcreteDeclRef(property, subs);

      if (isa<VarDecl>(property)) {
        resultRValue =
            subSGF.emitDynamicMemberRef(loc, baseSubstValue.getValue(), declRef,
                                        propertyType, SGFContext());
      } else {
        assert(isa<SubscriptDecl>(property));

        resultRValue = subSGF.emitDynamicSubscriptGetterApply(
            loc, baseSubstValue.getValue(), declRef,
            std::move(subscriptIndices), propertyType, SGFContext());
      }
    } else {
      resultRValue = subSGF.emitRValueForStorageLoad(
          loc, baseSubstValue, baseType, /*super*/ false, property,
          std::move(subscriptIndices), subs, AccessSemantics::Ordinary,
          propertyType, SGFContext());
    }
    resultSubst = std::move(resultRValue).getAsSingleValue(subSGF, loc);
  }

  if (resultSubst.getType().getAddressType() != resultArgTy)
    resultSubst = subSGF.emitSubstToOrigValue(loc, resultSubst,
                                         AbstractionPattern::getOpaque(),
                                         propertyType);

  if (SGM.M.useLoweredAddresses()) {
    resultSubst.forwardInto(subSGF, loc, resultArg);
    scope.pop();

    subSGF.B.createReturn(loc, subSGF.emitEmptyTuple(loc));
  } else {
    auto result = resultSubst.forward(subSGF);
    scope.pop();
    subSGF.B.createReturn(loc, result);
  }

  SGM.emitLazyConformancesForFunction(thunk);
  return thunk;
}

static SILFunction *getOrCreateKeyPathSetter(SILGenModule &SGM,
                          AbstractStorageDecl *property,
                          SubstitutionMap subs,
                          GenericEnvironment *genericEnv,
                          ResilienceExpansion expansion,
                          ArrayRef<IndexTypePair> indexes,
                          CanType baseType,
                          CanType propertyType) {
  // If the storage declaration is from a protocol, chase the override chain
  // back to the declaration whose setter introduced the witness table
  // entry.
  if (isa<ProtocolDecl>(property->getDeclContext())) {
    auto setter = property->getOpaqueAccessor(AccessorKind::Set);
    if (!setter->requiresNewWitnessTableEntry()) {
      // Find the setter that does have a witness table entry.
      auto wtableSetter =
        cast<AccessorDecl>(SILDeclRef::getOverriddenWitnessTableEntry(setter));

      // Substitute the 'Self' type of the base protocol.
      subs = SILGenModule::mapSubstitutionsForWitnessOverride(
              setter, wtableSetter, subs);
      property = wtableSetter->getStorage();
    }
  }

  auto genericSig =
      genericEnv ? genericEnv->getGenericSignature().getCanonicalSignature()
                 : nullptr;
  if (genericSig && genericSig->areAllParamsConcrete()) {
    genericSig = nullptr;
    genericEnv = nullptr;
  }

  // Build the signature of the thunk as expected by the keypath runtime.
  auto signature = [&]() {
    CanType loweredBaseTy, loweredPropTy;
    {
      AbstractionPattern opaque = AbstractionPattern::getOpaque();

      loweredBaseTy = SGM.Types.getLoweredRValueType(
          TypeExpansionContext::minimal(), opaque, baseType);
      loweredPropTy = SGM.Types.getLoweredRValueType(
          TypeExpansionContext::minimal(), opaque, propertyType);
    }
    
    auto &C = SGM.getASTContext();
    
    auto paramConvention = ParameterConvention::Indirect_In_Guaranteed;

    SmallVector<SILParameterInfo, 3> params;
    // property value
    params.push_back({loweredPropTy, paramConvention});
    // base
    params.push_back({loweredBaseTy,
                      property->isSetterMutating()
                        ? ParameterConvention::Indirect_Inout
                        : paramConvention});
    // indexes
    if (!indexes.empty()) {
      if (indexes.size() > 1) {
        SmallVector<TupleTypeElt, 8> indicesElements;
        for (auto &elt : indexes) {
          indicesElements.emplace_back(elt.first);
        }
        auto indicesTupleTy = TupleType::get(indicesElements, C)->getCanonicalType();
        params.push_back({indicesTupleTy, paramConvention});
      } else {
        params.push_back({indexes[0].first, paramConvention});
      }
    }

    return SILFunctionType::get(
        genericSig,
        SILFunctionType::ExtInfo().withRepresentation(
            SILFunctionType::Representation::KeyPathAccessorSetter),
        SILCoroutineKind::None, ParameterConvention::Direct_Unowned, params, {},
        {}, std::nullopt, SubstitutionMap(), SubstitutionMap(),
        SGM.getASTContext());
  }();
  
  // Mangle the name of the thunk to see if we already created it.
  auto name = Mangle::ASTMangler(SGM.getASTContext(), property)
    .mangleKeyPathSetterThunkHelper(property, genericSig, baseType,
                                    subs, expansion);
  auto loc = RegularLocation::getAutoGeneratedLocation();

  SILGenFunctionBuilder builder(SGM);
  auto thunk = builder.getOrCreateSharedFunction(
      loc, name, signature, IsBare, IsNotTransparent,
      (expansion == ResilienceExpansion::Minimal
       ? IsSerialized
       : IsNotSerialized),
      ProfileCounter(), IsThunk, IsNotDynamic, IsNotDistributed,
      IsNotRuntimeAccessible);
  if (!thunk->empty())
    return thunk;
  
  // Emit the thunk, which accesses the underlying property normally with
  // reabstraction where necessary.
  if (genericEnv) {
    baseType = genericEnv->mapTypeIntoContext(baseType)->getCanonicalType();
    propertyType = genericEnv->mapTypeIntoContext(propertyType)
      ->getCanonicalType();
    thunk->setGenericEnvironment(genericEnv);
  }
  
  SILGenFunction subSGF(SGM, *thunk, SGM.SwiftModule);

  signature = subSGF.F.getLoweredFunctionTypeInContext(
    subSGF.F.getTypeExpansionContext());

  auto entry = thunk->begin();
  auto valueArgTy =
      subSGF.silConv.getSILType(signature->getParameters()[0], signature,
                                subSGF.getTypeExpansionContext());
  auto baseArgTy =
      subSGF.silConv.getSILType(signature->getParameters()[1], signature,
                                subSGF.getTypeExpansionContext());
  if (genericEnv) {
    valueArgTy = genericEnv->mapTypeIntoContext(SGM.M, valueArgTy);
    baseArgTy = genericEnv->mapTypeIntoContext(SGM.M, baseArgTy);
  }
  auto valueArg = entry->createFunctionArgument(valueArgTy);
  auto baseArg = entry->createFunctionArgument(baseArgTy);
  SILValue indicesTupleArg;
  
  if (!indexes.empty()) {
    auto indexArgTy =
        subSGF.silConv.getSILType(signature->getParameters()[2], signature,
                                  subSGF.getTypeExpansionContext());
    if (genericEnv)
      indexArgTy = genericEnv->mapTypeIntoContext(SGM.M, indexArgTy);
    indicesTupleArg = entry->createFunctionArgument(indexArgTy);
  }

  Scope scope(subSGF, loc);

  auto subscriptIndices =
    loadIndexValuesForKeyPathComponent(subSGF, loc, property,
                                       indexes, indicesTupleArg);

  auto valueOrig = valueArgTy.isTrivial(subSGF.F)
                       ? ManagedValue::forRValueWithoutOwnership(valueArg)
                       : ManagedValue::forBorrowedRValue(valueArg);
  valueOrig = valueOrig.copy(subSGF, loc);
  auto valueSubst = subSGF.emitOrigToSubstValue(loc, valueOrig,
                                                AbstractionPattern::getOpaque(),
                                                propertyType);
  
  LValue lv;

  if (!property->isSetterMutating()) {
    auto baseSubst = emitKeyPathRValueBase(subSGF, property,
                                           loc, baseArg,
                                           baseType, subs);

    lv = LValue::forValue(SGFAccessKind::BorrowedObjectRead,
                          baseSubst, baseType);
  } else {
    auto baseOrig = ManagedValue::forLValue(baseArg);
    lv = LValue::forAddress(SGFAccessKind::ReadWrite, baseOrig, std::nullopt,
                            AbstractionPattern::getOpaque(), baseType);

    // Open an existential lvalue, if necessary.
    if (baseType->isAnyExistentialType()) {
      auto opened = subs.getReplacementTypes()[0]->castTo<ArchetypeType>();
      assert(opened->isOpenedExistential());
      baseType = opened->getCanonicalType();
      lv = subSGF.emitOpenExistentialLValue(loc, std::move(lv),
                                            CanArchetypeType(opened),
                                            baseType,
                                            SGFAccessKind::ReadWrite);
    }
  }

  auto semantics = AccessSemantics::Ordinary;
  auto strategy = property->getAccessStrategy(semantics, AccessKind::Write,
                                              SGM.M.getSwiftModule(), expansion,
                                              /*useOldABI=*/false);

  LValueOptions lvOptions;
  lv.addMemberComponent(subSGF, loc, property, subs, lvOptions,
                        /*super*/ false, SGFAccessKind::Write,
                        strategy, propertyType,
                        std::move(subscriptIndices),
                        /*index for diags*/ nullptr);

  // If the assigned value will need to be reabstracted, add a reabstraction
  // component.
  const auto loweredSubstType = subSGF.getLoweredType(lv.getSubstFormalType());
  if (lv.getTypeOfRValue() != loweredSubstType.getObjectType()) {
    // Logical components always re-abstract back to the substituted type.
    assert(lv.isLastComponentPhysical());
    lv.addOrigToSubstComponent(loweredSubstType);
  }

  subSGF.emitAssignToLValue(loc,
    RValue(subSGF, loc, propertyType, valueSubst),
    std::move(lv));
  scope.pop();
  
  subSGF.B.createReturn(loc, subSGF.emitEmptyTuple(loc));
  
  SGM.emitLazyConformancesForFunction(thunk);
  return thunk;
}

static void
getOrCreateKeyPathEqualsAndHash(SILGenModule &SGM,
                                SILLocation loc,
                                GenericEnvironment *genericEnv,
                                ResilienceExpansion expansion,
                                ArrayRef<KeyPathPatternComponent::Index> indexes,
                                SILFunction *&equals,
                                SILFunction *&hash) {
  if (indexes.empty()) {
    equals = nullptr;
    hash = nullptr;
    return;
  }

  auto genericSig =
      genericEnv ? genericEnv->getGenericSignature().getCanonicalSignature()
                 : nullptr;

  if (genericSig && genericSig->areAllParamsConcrete()) {
    genericSig = nullptr;
    genericEnv = nullptr;
  }

  auto &C = SGM.getASTContext();
  auto boolTy = C.getBoolType()->getCanonicalType();
  auto intTy = C.getIntType()->getCanonicalType();
  auto indicesTupleTy = buildKeyPathIndicesTuple(C, indexes);

  auto hashableProto = C.getProtocol(KnownProtocolKind::Hashable);

  SmallVector<CanType, 4> indexTypes;
  indexTypes.reserve(indexes.size());
  for (auto &index : indexes)
    indexTypes.push_back(index.FormalType);

  CanType indexTupleTy;
  if (indexes.size() == 1) {
    indexTupleTy = GenericEnvironment::mapTypeIntoContext(
        genericEnv, indexes[0].FormalType)->getCanonicalType();
  } else {
    SmallVector<TupleTypeElt, 2> indexElts;
    for (auto &elt : indexes) {
      indexElts.push_back(GenericEnvironment::mapTypeIntoContext(
          genericEnv, elt.FormalType));
    }

    indexTupleTy = TupleType::get(indexElts, SGM.getASTContext())
                          ->getCanonicalType();
  }

  RValue indexValue(indexTupleTy);

  // Get or create the equals witness
  [boolTy, indicesTupleTy, genericSig, &C, &indexTypes, &equals, loc, &SGM,
   genericEnv, expansion, indexes] {
    // (lhs: (X, Y, ...), rhs: (X, Y, ...)) -> Bool
    SmallVector<SILParameterInfo, 2> params;
    params.push_back(
        {indicesTupleTy, ParameterConvention::Indirect_In_Guaranteed});
    params.push_back(
        {indicesTupleTy, ParameterConvention::Indirect_In_Guaranteed});

    SmallVector<SILResultInfo, 1> results;
    results.push_back({boolTy, ResultConvention::Unowned});

    auto signature = SILFunctionType::get(
        genericSig,
        SILFunctionType::ExtInfo().withRepresentation(
            SILFunctionType::Representation::KeyPathAccessorEquals),
        SILCoroutineKind::None, ParameterConvention::Direct_Unowned, params,
        /*yields*/ {}, results, std::nullopt, SubstitutionMap(),
        SubstitutionMap(), C);

    // Mangle the name of the thunk to see if we already created it.
    auto name = Mangle::ASTMangler(SGM.getASTContext())
      .mangleKeyPathEqualsHelper(indexTypes, genericSig, expansion);
    SILGenFunctionBuilder builder(SGM);
    equals = builder.getOrCreateSharedFunction(
        loc, name, signature, IsBare, IsNotTransparent,
        (expansion == ResilienceExpansion::Minimal
         ? IsSerialized
         : IsNotSerialized),
        ProfileCounter(), IsThunk, IsNotDynamic, IsNotDistributed,
        IsNotRuntimeAccessible);
    if (!equals->empty()) {
      return;
    }
    
    SILGenFunction subSGF(SGM, *equals, SGM.SwiftModule);
    equals->setGenericEnvironment(genericEnv);
    auto entry = equals->begin();
    auto lhsArgTy = subSGF.silConv.getSILType(
        params[0], signature, subSGF.getTypeExpansionContext());
    auto rhsArgTy = subSGF.silConv.getSILType(
        params[1], signature, subSGF.getTypeExpansionContext());
    if (genericEnv) {
      lhsArgTy = genericEnv->mapTypeIntoContext(SGM.M, lhsArgTy);
      rhsArgTy = genericEnv->mapTypeIntoContext(SGM.M, rhsArgTy);
    }
    auto lhsAddr = entry->createFunctionArgument(lhsArgTy);
    auto rhsAddr = entry->createFunctionArgument(rhsArgTy);

    Scope scope(subSGF, loc);

    // Compare each pair of index values using the == witness from the
    // conformance.
    auto equatableProtocol = C.getProtocol(KnownProtocolKind::Equatable);
    auto equalsMethod = equatableProtocol->getSingleRequirement(
      C.Id_EqualsOperator);
    auto equalsRef = SILDeclRef(equalsMethod);
    auto equalsTy = subSGF.SGM.Types.getConstantType(
        TypeExpansionContext(subSGF.F), equalsRef);

    auto isFalseBB = subSGF.createBasicBlock();
    auto i1Ty = SILType::getBuiltinIntegerType(1, C);
    for (unsigned i : indices(indexes)) {
      auto &index = indexes[i];
      
      Type formalTy = index.FormalType;
      ProtocolConformanceRef hashable = index.Hashable;
      std::tie(formalTy, hashable)
        = GenericEnvironment::mapConformanceRefIntoContext(genericEnv,
                                                           formalTy,
                                                           hashable);
      auto formalCanTy = formalTy->getReducedType(genericSig);
      
      // Get the Equatable conformance from the Hashable conformance.
      auto equatable = hashable.getAssociatedConformance(
          formalTy,
          GenericTypeParamType::getType(/*depth*/ 0, /*index*/ 0, C),
          equatableProtocol);

      assert(equatable.isAbstract() == hashable.isAbstract());
      if (equatable.isConcrete())
        assert(equatable.getConcrete()->getType()->isEqual(
                  hashable.getConcrete()->getType()));
    
      auto equalsWitness = subSGF.B.createWitnessMethod(loc,
        formalCanTy, equatable,
        equalsRef, equalsTy);
      
      auto equatableSub
        = SubstitutionMap::getProtocolSubstitutions(equatableProtocol,
                                                    formalCanTy,
                                                    equatable);
      auto equalsSubstTy = equalsTy.castTo<SILFunctionType>()->substGenericArgs(
          SGM.M, equatableSub, TypeExpansionContext(subSGF.F));
      auto equalsInfo =
          CalleeTypeInfo(equalsSubstTy, AbstractionPattern(boolTy), boolTy,
                         std::nullopt, std::nullopt, ImportAsMemberStatus());

      Scope branchScope(subSGF, loc);

      SILValue lhsEltAddr = lhsAddr;
      SILValue rhsEltAddr = rhsAddr;
      if (indexes.size() > 1) {
        lhsEltAddr = subSGF.B.createTupleElementAddr(loc, lhsAddr, i);
        rhsEltAddr = subSGF.B.createTupleElementAddr(loc, rhsAddr, i);
      }
      auto lhsArg = subSGF.emitLoad(loc, lhsEltAddr,
             subSGF.getTypeLowering(AbstractionPattern::getOpaque(), formalTy),
             SGFContext(), IsNotTake);
      auto rhsArg = subSGF.emitLoad(loc, rhsEltAddr,
             subSGF.getTypeLowering(AbstractionPattern::getOpaque(), formalTy),
             SGFContext(), IsNotTake);
      
      if (!lhsArg.getType().isAddress()) {
        auto lhsBuf = subSGF.emitTemporaryAllocation(loc, lhsArg.getType());
        lhsArg.forwardInto(subSGF, loc, lhsBuf);
        lhsArg = subSGF.emitManagedBufferWithCleanup(lhsBuf);

        auto rhsBuf = subSGF.emitTemporaryAllocation(loc, rhsArg.getType());
        rhsArg.forwardInto(subSGF, loc, rhsBuf);
        rhsArg = subSGF.emitManagedBufferWithCleanup(rhsBuf);
      }

      auto metaty = CanMetatypeType::get(formalCanTy,
                                         MetatypeRepresentation::Thick);
      auto metatyValue =
          ManagedValue::forObjectRValueWithoutOwnership(subSGF.B.createMetatype(
              loc, SILType::getPrimitiveObjectType(metaty)));
      SILValue isEqual;
      {
        auto equalsResultPlan = ResultPlanBuilder::computeResultPlan(subSGF,
          equalsInfo, loc, SGFContext());
        ArgumentScope argScope(subSGF, loc);
        isEqual = subSGF
                      .emitApply(std::move(equalsResultPlan),
                                 std::move(argScope), loc,
                                 ManagedValue::forObjectRValueWithoutOwnership(
                                     equalsWitness),
                                 equatableSub, {lhsArg, rhsArg, metatyValue},
                                 equalsInfo, ApplyOptions(), SGFContext(),
                                 std::nullopt)
                      .getUnmanagedSingleValue(subSGF, loc);
      }
      
      branchScope.pop();
      
      auto isEqualI1 = subSGF.B.createStructExtract(loc, isEqual,
        C.getBoolDecl()->getStoredProperties()[0], i1Ty);
      
      auto isTrueBB = subSGF.createBasicBlock();
      // Each false condition needs its own block to avoid critical edges.
      auto falseEdgeBB = subSGF.createBasicBlockAndBranch(loc, isFalseBB);

      subSGF.B.createCondBranch(loc, isEqualI1, isTrueBB, falseEdgeBB);

      subSGF.B.emitBlock(isTrueBB);
    }
    
    auto returnBB = subSGF.createBasicBlock(FunctionSection::Postmatter);
    
    SILValue trueValue = subSGF.B.createIntegerLiteral(loc, i1Ty, 1);
    subSGF.B.createBranch(loc, returnBB, trueValue);
    
    subSGF.B.emitBlock(isFalseBB);
    SILValue falseValue = subSGF.B.createIntegerLiteral(loc, i1Ty, 0);
    subSGF.B.createBranch(loc, returnBB, falseValue);
    
    subSGF.B.emitBlock(returnBB);
    scope.pop();
    SILValue returnVal = returnBB->createPhiArgument(i1Ty, OwnershipKind::None);
    auto returnBoolVal = subSGF.B.createStruct(loc,
      SILType::getPrimitiveObjectType(boolTy), returnVal);
    subSGF.B.createReturn(loc, returnBoolVal);

    SGM.emitLazyConformancesForFunction(equals);
  }();

  // Get or create the hash witness
  [intTy, indicesTupleTy, genericSig, &C, indexTypes, &hash, &loc, &SGM,
   genericEnv, expansion, hashableProto, indexes] {
    // (indices: (X, Y, ...)) -> Int
    SmallVector<SILParameterInfo, 1> params;
    params.push_back({indicesTupleTy,
                      ParameterConvention::Indirect_In_Guaranteed});

    SmallVector<SILResultInfo, 1> results;
    results.push_back({intTy, ResultConvention::Unowned});

    auto signature = SILFunctionType::get(
        genericSig,
        SILFunctionType::ExtInfo().withRepresentation(
            SILFunctionType::Representation::KeyPathAccessorHash),
        SILCoroutineKind::None, ParameterConvention::Direct_Unowned, params,
        /*yields*/ {}, results, std::nullopt, SubstitutionMap(),
        SubstitutionMap(), C);

    // Mangle the name of the thunk to see if we already created it.
    SmallString<64> nameBuf;
    
    auto name = Mangle::ASTMangler(SGM.getASTContext())
      .mangleKeyPathHashHelper(indexTypes, genericSig, expansion);
    SILGenFunctionBuilder builder(SGM);
    hash = builder.getOrCreateSharedFunction(
        loc, name, signature, IsBare, IsNotTransparent,
        (expansion == ResilienceExpansion::Minimal
         ? IsSerialized
         : IsNotSerialized),
        ProfileCounter(), IsThunk, IsNotDynamic, IsNotDistributed,
        IsNotRuntimeAccessible);
    if (!hash->empty()) {
      return;
    }
    
    SILGenFunction subSGF(SGM, *hash, SGM.SwiftModule);
    hash->setGenericEnvironment(genericEnv);
    auto entry = hash->begin();
    auto indexArgTy = subSGF.silConv.getSILType(
        params[0], signature, subSGF.getTypeExpansionContext());
    if (genericEnv)
      indexArgTy = genericEnv->mapTypeIntoContext(SGM.M, indexArgTy);
    auto indexPtr = entry->createFunctionArgument(indexArgTy);

    SILValue hashCode;

    // For now, just use the hash value of the first index.
    // TODO: Combine hashes of the indexes using an inout Hasher
    {
      ArgumentScope scope(subSGF, loc);

      auto &index = indexes[0];
      
      // Extract the index value.
      SILValue indexAddr = indexPtr;
      if (indexes.size() > 1) {
        indexAddr = subSGF.B.createTupleElementAddr(loc, indexPtr, 0);
      }

      VarDecl *hashValueVar =
        cast<VarDecl>(hashableProto->getSingleRequirement(C.Id_hashValue));

      auto formalTy = index.FormalType;
      auto hashable = index.Hashable;
      if (genericEnv) {
        formalTy = genericEnv->mapTypeIntoContext(formalTy)->getCanonicalType();
        hashable = hashable.subst(index.FormalType,
          [&](Type t) -> Type { return genericEnv->mapTypeIntoContext(t); },
          LookUpConformanceInModule());
      }

      // Set up a substitution of Self => IndexType.
      auto hashGenericSig =
        hashValueVar->getDeclContext()->getGenericSignatureOfContext();
      assert(hashGenericSig);
      SubstitutionMap hashableSubsMap = SubstitutionMap::get(
          hashGenericSig,
          [&](SubstitutableType *type) -> Type { return formalTy; },
          [&](CanType dependentType, Type replacementType, ProtocolDecl *proto)
              -> ProtocolConformanceRef { return hashable; });

      // Read the storage.
      ManagedValue base = ManagedValue::forBorrowedAddressRValue(indexAddr);
      hashCode =
        subSGF.emitRValueForStorageLoad(loc, base, formalTy, /*super*/ false,
                                        hashValueVar, PreparedArguments(),
                                        hashableSubsMap,
                                        AccessSemantics::Ordinary,
                                        intTy, SGFContext())
              .getUnmanagedSingleValue(subSGF, loc);

      scope.pop();
    }

    subSGF.B.createReturn(loc, hashCode);
    SGM.emitLazyConformancesForFunction(hash);
  }();
  
  return;
}

static KeyPathPatternComponent::ComputedPropertyId
getIdForKeyPathComponentComputedProperty(SILGenModule &SGM,
                                         AbstractStorageDecl *storage,
                                         ResilienceExpansion expansion,
                                         AccessStrategy strategy) {
  auto getAccessorFunction = [&SGM](AbstractStorageDecl *storage,
                                    bool isForeign) -> SILFunction * {
    // Identify the property using its (unthunked) getter. For a
    // computed property, this should be stable ABI; for a resilient public
    // property, this should also be stable ABI across modules.
    auto representativeDecl = getRepresentativeAccessorForKeyPath(storage);
    // If the property came from an import-as-member function defined in C,
    // use the original C function as the key.
    auto ref =
        SILDeclRef(representativeDecl, SILDeclRef::Kind::Func, isForeign);
    // TODO: If the getter has shared linkage (say it's synthesized for a
    // Clang-imported thing), we'll need some other sort of
    // stable identifier.
    return SGM.getFunction(ref, NotForDefinition);
  };

  switch (strategy.getKind()) {
  case AccessStrategy::Storage:
    if (auto decl = cast<VarDecl>(storage); decl->isStatic()) {
      // For metatype keypaths, identify property via accessors.
      return getAccessorFunction(storage, /*isForeign=*/false);
    }
    // Otherwise, identify reabstracted stored properties by the property
    // itself.
    return cast<VarDecl>(storage);
  case AccessStrategy::MaterializeToTemporary:
    // Use the read strategy.  But try to avoid turning e.g. an
    // observed property into a stored property.
    strategy = strategy.getReadStrategy();
    if (strategy.getKind() != AccessStrategy::Storage ||
        !getRepresentativeAccessorForKeyPath(storage)) {
      return getIdForKeyPathComponentComputedProperty(SGM, storage, expansion,
                                                      strategy);
    }
    LLVM_FALLTHROUGH;
  case AccessStrategy::DirectToAccessor: {
    return getAccessorFunction(
        storage,
        getRepresentativeAccessorForKeyPath(storage)->isImportAsMember());
  }
  case AccessStrategy::DispatchToAccessor: {
    // Identify the property by its vtable or wtable slot.
    return SGM.getAccessorDeclRef(getRepresentativeAccessorForKeyPath(storage),
                                  expansion);
  }

  case AccessStrategy::DispatchToDistributedThunk: {
    auto thunkRef = SILDeclRef(cast<VarDecl>(storage)->getDistributedThunk(),
                               SILDeclRef::Kind::Func,
                               /*isForeign=*/false,
                               /*isDistributed=*/true);
    return SGM.getFunction(thunkRef, NotForDefinition);
  }
  }
  llvm_unreachable("unhandled access strategy");
}

static void
lowerKeyPathSubscriptIndexTypes(
                 SILGenModule &SGM,
                 SmallVectorImpl<IndexTypePair> &indexPatterns,
                 SubscriptDecl *subscript,
                 SubstitutionMap subscriptSubs,
                 ResilienceExpansion expansion,
                 bool &needsGenericContext) {
  // Capturing an index value dependent on the generic context means we
  // need the generic context captured in the key path.
  auto subscriptSubstTy = subscript->getInterfaceType();
  SubstitutionMap subMap;
  auto sig = subscript->getGenericSignature();
  if (sig) {
    subscriptSubstTy = subscriptSubstTy.subst(subscriptSubs);
  }
  needsGenericContext |= subscriptSubstTy->hasArchetype();

  for (auto *index : *subscript->getIndices()) {
    auto indexTy = index->getInterfaceType();
    if (sig) {
      indexTy = indexTy.subst(subscriptSubs);
    }

    auto indexLoweredTy = SGM.Types.getLoweredType(
        AbstractionPattern::getOpaque(), indexTy,
        TypeExpansionContext::noOpaqueTypeArchetypesSubstitution(expansion));
    indexLoweredTy = indexLoweredTy.mapTypeOutOfContext();
    indexPatterns.push_back({indexTy->mapTypeOutOfContext()
                                    ->getCanonicalType(),
                             indexLoweredTy});
  }
}

static void
lowerKeyPathSubscriptIndexPatterns(
                 SmallVectorImpl<KeyPathPatternComponent::Index> &indexPatterns,
                 ArrayRef<IndexTypePair> indexTypes,
                 ArrayRef<ProtocolConformanceRef> indexHashables,
                 unsigned &baseOperand) {
  for (unsigned i : indices(indexTypes)) {
    CanType formalTy;
    SILType loweredTy;
    std::tie(formalTy, loweredTy) = indexTypes[i];
    auto hashable = indexHashables[i].mapConformanceOutOfContext();
    assert(hashable.isAbstract() ||
           hashable.getConcrete()->getType()->isEqual(formalTy));

    indexPatterns.push_back({baseOperand++, formalTy, loweredTy, hashable});
  }
}

KeyPathPatternComponent
SILGenModule::emitKeyPathComponentForDecl(SILLocation loc,
                                GenericEnvironment *genericEnv,
                                ResilienceExpansion expansion,
                                unsigned &baseOperand,
                                bool &needsGenericContext,
                                SubstitutionMap subs,
                                AbstractStorageDecl *storage,
                                ArrayRef<ProtocolConformanceRef> indexHashables,
                                CanType baseTy,
                                DeclContext *useDC,
                                bool forPropertyDescriptor) {
  auto baseDecl = storage;

  // ABI-compatible overrides do not have property descriptors, so we need
  // to reference the overridden declaration instead.
  if (isa<ClassDecl>(baseDecl->getDeclContext())) {
    while (!baseDecl->isValidKeyPathComponent())
      baseDecl = baseDecl->getOverriddenDecl();
  }

  /// Returns true if a key path component for the given property or
  /// subscript should be externally referenced.
  auto shouldUseExternalKeyPathComponent = [&]() -> bool {
    // The property descriptor has the canonical key path component information
    // so doesn't have to refer to another external descriptor.
    if (forPropertyDescriptor) {
      return false;
    }
    
    // Don't need to use the external component if we're inside the resilience
    // domain of its defining module.
    if (baseDecl->getModuleContext() == SwiftModule
        && !baseDecl->isResilient(SwiftModule, expansion)) {
      return false;
    }

    // Protocol requirements don't have nor need property descriptors.
    if (isa<ProtocolDecl>(baseDecl->getDeclContext())) {
      return false;
    }
    
    // Always-emit-into-client properties can't reliably refer to a property
    // descriptor that may not exist in older versions of their home dylib.
    // Their definition is also always entirely visible to clients so it isn't
    // needed.
    if (baseDecl->getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>()) {
      return false;
    }

    // Back deployed properties have the same restrictions as
    // always-emit-into-client properties.
    if (requiresBackDeploymentThunk(baseDecl, expansion)) {
      return false;
    }

    // Properties that only dispatch via ObjC lookup do not have nor
    // need property descriptors, since the selector identifies the
    // storage.
    // Properties that are not public don't need property descriptors
    // either.
    if (baseDecl->requiresOpaqueAccessors()) {
      auto representative = getAccessorDeclRef(
          getRepresentativeAccessorForKeyPath(baseDecl), expansion);
      if (representative.isForeign)
        return false;

      switch (representative.getLinkage(ForDefinition)) {
      case SILLinkage::Public:
      case SILLinkage::PublicNonABI:
      case SILLinkage::Package:
      case SILLinkage::PackageNonABI:
        break;
      case SILLinkage::Hidden:
      case SILLinkage::Shared:
      case SILLinkage::Private:
      case SILLinkage::PublicExternal:
      case SILLinkage::PackageExternal:
      case SILLinkage::HiddenExternal:
        return false;
      }
    }
    
    return true;
  };

  auto strategy = storage->getAccessStrategy(
      AccessSemantics::Ordinary,
      storage->supportsMutation() ? AccessKind::ReadWrite : AccessKind::Read,
      M.getSwiftModule(), expansion,
      /*useOldABI=*/false);

  AbstractStorageDecl *externalDecl = nullptr;
  SubstitutionMap externalSubs;
  
  if (shouldUseExternalKeyPathComponent()) {
    externalDecl = storage;
    // Map the substitutions out of context.
    if (!subs.empty()) {
      externalSubs = subs;
      // If any of the substitutions involve primary archetypes, then the
      // key path pattern needs to capture the generic context, and we need
      // to map the pattern substitutions out of this context.
      if (externalSubs.getRecursiveProperties().hasArchetype()) {
        needsGenericContext = true;
        // FIXME: This doesn't do anything for local archetypes!
        externalSubs = externalSubs.mapReplacementTypesOutOfContext();
      }
    }

    // ABI-compatible overrides do not have property descriptors, so we need
    // to reference the overridden declaration instead.
    if (baseDecl != externalDecl) {
      externalSubs = SubstitutionMap::getOverrideSubstitutions(baseDecl,
                                                               externalDecl)
          .subst(externalSubs);
      externalDecl = baseDecl;
    }
  }
  
  auto isSettableInComponent = [&]() -> bool {
    // For storage we reference by a property descriptor, the descriptor will
    // supply the settability if needed. We only reference it here if the
    // setter is public.
    if (shouldUseExternalKeyPathComponent())
      return storage->isSettableInSwift(useDC) &&
             storage->isSetterAccessibleFrom(useDC);
    return storage->isSettableInSwift(storage->getDeclContext());
  };

  if (auto var = dyn_cast<VarDecl>(storage)) {
    CanType componentTy;
    if (!var->getDeclContext()->isTypeContext()) {
      componentTy = var->getInterfaceType()->getCanonicalType();
    } else if (var->getDeclContext()->getSelfProtocolDecl() &&
               baseTy->isExistentialType()) {
      componentTy = var->getValueInterfaceType()->getCanonicalType();
      ASSERT(!componentTy->hasTypeParameter());
    } else {
      // The mapTypeIntoContext() / mapTypeOutOfContext() dance is there
      // to handle the case where baseTy being a type parameter subject
      // to a superclass requirement.
      componentTy = var->getValueInterfaceType().subst(
        GenericEnvironment::mapTypeIntoContext(genericEnv, baseTy)
          ->getContextSubstitutionMap(var->getDeclContext()))
          ->mapTypeOutOfContext()
          ->getCanonicalType();
    }

    // The component type for an @objc optional requirement needs to be
    // wrapped in an optional.
    if (var->getAttrs().hasAttribute<OptionalAttr>()) {
      componentTy = OptionalType::get(componentTy)->getCanonicalType();
    }
  
    if (canStorageUseStoredKeyPathComponent(var, expansion)) {
      return KeyPathPatternComponent::forStoredProperty(var, componentTy);
    }

    // We need thunks to bring the getter and setter to the right signature
    // expected by the key path runtime.
    auto id = getIdForKeyPathComponentComputedProperty(*this, var, expansion,
                                                       strategy);
    auto getter = getOrCreateKeyPathGetter(*this,
             var, subs,
             needsGenericContext ? genericEnv : nullptr,
             expansion, {}, baseTy, componentTy);
    
    if (isSettableInComponent()) {
      auto setter = getOrCreateKeyPathSetter(*this,
             var, subs,
             needsGenericContext ? genericEnv : nullptr,
             expansion, {}, baseTy, componentTy);
      return KeyPathPatternComponent::forComputedSettableProperty(id,
          getter, setter, {}, nullptr, nullptr,
          externalDecl, externalSubs, componentTy);
    } else {
      return KeyPathPatternComponent::forComputedGettableProperty(id,
          getter, {}, nullptr, nullptr,
          externalDecl, externalSubs, componentTy);
    }
  }
  
  if (auto decl = dyn_cast<SubscriptDecl>(storage)) {
    auto baseSubscriptTy =
      decl->getInterfaceType()->castTo<AnyFunctionType>();
    if (auto genSubscriptTy = baseSubscriptTy->getAs<GenericFunctionType>())
      baseSubscriptTy = genSubscriptTy->substGenericArgs(subs);
    auto baseSubscriptInterfaceTy = cast<AnyFunctionType>(
      baseSubscriptTy->mapTypeOutOfContext()->getCanonicalType());

    auto componentTy = baseSubscriptInterfaceTy.getResult();
    if (decl->getAttrs().hasAttribute<OptionalAttr>()) {
      // The component type for an @objc optional requirement needs to be
      // wrapped in an optional
      componentTy = OptionalType::get(componentTy)->getCanonicalType();
    }

    SmallVector<IndexTypePair, 4> indexTypes;
    lowerKeyPathSubscriptIndexTypes(*this, indexTypes,
                                    decl, subs,
                                    expansion,
                                    needsGenericContext);
    
    SmallVector<KeyPathPatternComponent::Index, 4> indexPatterns;
    SILFunction *indexEquals = nullptr, *indexHash = nullptr;
    // Property descriptors get their index information from the client.
    if (!forPropertyDescriptor) {
      lowerKeyPathSubscriptIndexPatterns(indexPatterns,
                                         indexTypes, indexHashables,
                                         baseOperand);
      
      getOrCreateKeyPathEqualsAndHash(*this, loc,
               needsGenericContext ? genericEnv : nullptr,
               expansion,
               indexPatterns,
               indexEquals, indexHash);
    }

    auto id = getIdForKeyPathComponentComputedProperty(*this, decl, expansion,
                                                       strategy);
    auto getter = getOrCreateKeyPathGetter(*this,
             decl, subs,
             needsGenericContext ? genericEnv : nullptr,
             expansion,
             indexTypes,
             baseTy, componentTy);
  
    auto indexPatternsCopy = getASTContext().AllocateCopy(indexPatterns);
    if (isSettableInComponent()) {
      auto setter = getOrCreateKeyPathSetter(*this,
             decl, subs,
             needsGenericContext ? genericEnv : nullptr,
             expansion,
             indexTypes,
             baseTy, componentTy);
      return KeyPathPatternComponent::forComputedSettableProperty(id,
                                                           getter, setter,
                                                           indexPatternsCopy,
                                                           indexEquals,
                                                           indexHash,
                                                           externalDecl,
                                                           externalSubs,
                                                           componentTy);
    } else {
      return KeyPathPatternComponent::forComputedGettableProperty(id,
                                                           getter,
                                                           indexPatternsCopy,
                                                           indexEquals,
                                                           indexHash,
                                                           externalDecl,
                                                           externalSubs,
                                                           componentTy);
    }
  }
  
  llvm_unreachable("unknown kind of storage");
}

RValue RValueEmitter::visitKeyPathExpr(KeyPathExpr *E, SGFContext C) {
  if (E->isObjC()) {
    return visit(E->getObjCStringLiteralExpr(), C);
  }

  // Figure out the key path pattern, abstracting out generic arguments and
  // subscript indexes.
  SmallVector<KeyPathPatternComponent, 4> loweredComponents;
  auto loweredTy = SGF.getLoweredType(E->getType());

  CanType rootTy = E->getRootType()->getCanonicalType();

  bool needsGenericContext = false;
  if (rootTy->hasArchetype()) {
    needsGenericContext = true;
    rootTy = rootTy->mapTypeOutOfContext()->getCanonicalType();
  }
  
  auto baseTy = rootTy;
  SmallVector<SILValue, 4> operands;

  for (auto &component : E->getComponents()) {
    switch (auto kind = component.getKind()) {
    case KeyPathExpr::Component::Kind::Property:
    case KeyPathExpr::Component::Kind::Subscript: {
      auto decl = cast<AbstractStorageDecl>(component.getDeclRef().getDecl());

      unsigned numOperands = operands.size();
      loweredComponents.push_back(
        SGF.SGM.emitKeyPathComponentForDecl(SILLocation(E),
                            SGF.F.getGenericEnvironment(),
                            SGF.F.getResilienceExpansion(),
                            numOperands,
                            needsGenericContext,
                            component.getDeclRef().getSubstitutions(),
                            decl,
                            component.getSubscriptIndexHashableConformances(),
                            baseTy,
                            SGF.FunctionDC,
                            /*for descriptor*/ false));
      baseTy = loweredComponents.back().getComponentType();
      if (kind == KeyPathExpr::Component::Kind::Property)
        break;

      auto subscript = cast<SubscriptDecl>(decl);
      auto loweredArgs = SGF.emitKeyPathSubscriptOperands(
          E, subscript,
          component.getDeclRef().getSubstitutions(),
          component.getSubscriptArgs());

      for (auto &arg : loweredArgs) {
        operands.push_back(arg.forward(SGF));
      }

      break;
    }

    case KeyPathExpr::Component::Kind::TupleElement: {
      assert(baseTy->is<TupleType>() && "baseTy is expected to be a TupleType");

      auto tupleIndex = component.getTupleIndex();
      auto elementTy = baseTy->getAs<TupleType>()
        ->getElementType(tupleIndex)
        ->getCanonicalType();

      loweredComponents.push_back(
        KeyPathPatternComponent::forTupleElement(tupleIndex, elementTy));

      baseTy = loweredComponents.back().getComponentType();

      break;
    }

    case KeyPathExpr::Component::Kind::OptionalChain:
    case KeyPathExpr::Component::Kind::OptionalForce:
    case KeyPathExpr::Component::Kind::OptionalWrap: {
      KeyPathPatternComponent::Kind loweredKind;
      switch (kind) {
      case KeyPathExpr::Component::Kind::OptionalChain:
        loweredKind = KeyPathPatternComponent::Kind::OptionalChain;
        baseTy = baseTy->getOptionalObjectType()->getCanonicalType();
        break;
      case KeyPathExpr::Component::Kind::OptionalForce:
        loweredKind = KeyPathPatternComponent::Kind::OptionalForce;
        baseTy = baseTy->getOptionalObjectType()->getCanonicalType();
        break;
      case KeyPathExpr::Component::Kind::OptionalWrap:
        loweredKind = KeyPathPatternComponent::Kind::OptionalWrap;
        baseTy = OptionalType::get(baseTy)->getCanonicalType();
        break;
      default:
        llvm_unreachable("out of sync");
      }
      loweredComponents.push_back(
                    KeyPathPatternComponent::forOptional(loweredKind, baseTy));
      break;
    }
    
    case KeyPathExpr::Component::Kind::Identity:
      continue;
        
    case KeyPathExpr::Component::Kind::Invalid:
    case KeyPathExpr::Component::Kind::UnresolvedProperty:
    case KeyPathExpr::Component::Kind::UnresolvedSubscript:
    case KeyPathExpr::Component::Kind::CodeCompletion:
      llvm_unreachable("not resolved");
      break;

    case KeyPathExpr::Component::Kind::DictionaryKey:
      llvm_unreachable("DictionaryKey only valid in #keyPath");
      break;
    }
  }
  
  StringRef objcString;
  if (auto objcExpr = dyn_cast_or_null<StringLiteralExpr>
                                                (E->getObjCStringLiteralExpr()))
    objcString = objcExpr->getValue();
  
  auto pattern = KeyPathPattern::get(SGF.SGM.M,
                                     needsGenericContext
                                       ? SGF.F.getLoweredFunctionType()
                                             ->getInvocationGenericSignature()
                                       : nullptr,
                                     rootTy, baseTy,
                                     loweredComponents,
                                     objcString);
  auto keyPath = SGF.B.createKeyPath(SILLocation(E), pattern,
                                     needsGenericContext
                                       ? SGF.F.getForwardingSubstitutionMap()
                                       : SubstitutionMap(),
                                     operands,
                                     loweredTy);
  auto value = SGF.emitManagedRValueWithCleanup(keyPath);
  return RValue(SGF, E, value);
}

RValue RValueEmitter::
visitKeyPathApplicationExpr(KeyPathApplicationExpr *E, SGFContext C) {
  FormalEvaluationScope scope(SGF);

  auto lv = SGF.emitLValue(E, SGFAccessKind::OwnedObjectRead);
  return SGF.emitLoadOfLValue(E, std::move(lv), C);
}

RValue RValueEmitter::
visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E, SGFContext C) {
  switch (E->getKind()) {
#define MAGIC_POINTER_IDENTIFIER(NAME, STRING, SYNTAX_KIND)
#define MAGIC_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
  case MagicIdentifierLiteralExpr::NAME:
#include "swift/AST/MagicIdentifierKinds.def"
    return SGF.emitLiteral(E, C);
    
  case MagicIdentifierLiteralExpr::DSOHandle: {
    auto SILLoc = SILLocation(E);
    auto UnsafeRawPointer = SGF.getASTContext().getUnsafeRawPointerDecl();
    auto UnsafeRawPtrTy =
      SGF.getLoweredType(UnsafeRawPointer->getDeclaredInterfaceType());
    SILType BuiltinRawPtrTy = SILType::getRawPointerType(SGF.getASTContext());

    SILModule &M = SGF.SGM.M;
    SILBuilder &B = SGF.B;

    GlobalAddrInst *ModuleBase = nullptr;

    if (M.getASTContext().LangOpts.Target.isOSWindows()) {
      auto ImageBase = M.lookUpGlobalVariable("__ImageBase");
      if (!ImageBase)
        ImageBase =
            SILGlobalVariable::create(M, SILLinkage::DefaultForDeclaration,
                                      IsNotSerialized, "__ImageBase",
                                      BuiltinRawPtrTy);
      ModuleBase = B.createGlobalAddr(SILLoc, ImageBase, /*dependencyToken=*/ SILValue());
    } else {
      auto DSOHandle = M.lookUpGlobalVariable("__dso_handle");
      if (!DSOHandle)
        DSOHandle = SILGlobalVariable::create(M, SILLinkage::PublicExternal,
                                              IsNotSerialized, "__dso_handle",
                                              BuiltinRawPtrTy);
      ModuleBase = B.createGlobalAddr(SILLoc, DSOHandle, /*dependencyToken=*/ SILValue());
    }

    auto ModuleBasePointer =
        B.createAddressToPointer(SILLoc, ModuleBase, BuiltinRawPtrTy,
                                 /*needsStackProtection=*/ false);
    StructInst *S =
        B.createStruct(SILLoc, UnsafeRawPtrTy, { ModuleBasePointer });
    return RValue(SGF, E, ManagedValue::forObjectRValueWithoutOwnership(S));
  }
  }

  llvm_unreachable("Unhandled MagicIdentifierLiteralExpr in switch.");
}

RValue RValueEmitter::visitCollectionExpr(CollectionExpr *E, SGFContext C) {
  auto loc = SILLocation(E);
  ArgumentScope scope(SGF, loc);

  // CSApply builds ArrayExprs without an initializer for the trivial case
  // of emitting varargs.
  CanType arrayType, elementType;
  if (E->getInitializer()) {
    if (auto *arrayExpr = dyn_cast<ArrayExpr>(E)) {
      elementType = arrayExpr->getElementType()->getCanonicalType();
    } else {
      auto *dictionaryExpr = cast<DictionaryExpr>(E);
      elementType = dictionaryExpr->getElementType()->getCanonicalType();
    }
    arrayType = ArraySliceType::get(elementType)->getCanonicalType();
  } else {
    arrayType = E->getType()->getCanonicalType();
    auto genericType = cast<BoundGenericStructType>(arrayType);
    assert(genericType->isArray());
    elementType = genericType.getGenericArgs()[0];
  }

  VarargsInfo varargsInfo =
      emitBeginVarargs(SGF, loc, elementType, arrayType,
                       E->getNumElements());

  // Cleanups for any elements that have been initialized so far.
  SmallVector<CleanupHandle, 8> cleanups;

  for (unsigned index : range(E->getNumElements())) {
    auto destAddr = varargsInfo.getBaseAddress();
    if (index != 0) {
      SILValue indexValue = SGF.B.createIntegerLiteral(
          loc, SILType::getBuiltinWordType(SGF.getASTContext()), index);
      destAddr = SGF.B.createIndexAddr(loc, destAddr, indexValue,
              /*needsStackProtection=*/ false);
    }
    auto &destTL = varargsInfo.getBaseTypeLowering();
    // Create a dormant cleanup for the value in case we exit before the
    // full array has been constructed.

    CleanupHandle destCleanup = CleanupHandle::invalid();
    if (!destTL.isTrivial()) {
      destCleanup = SGF.enterDestroyCleanup(destAddr);
      SGF.Cleanups.setCleanupState(destCleanup, CleanupState::Dormant);
      cleanups.push_back(destCleanup);
    }

    TemporaryInitialization init(destAddr, destCleanup);

    ArgumentSource(E->getElements()[index])
        .forwardInto(SGF, varargsInfo.getBaseAbstractionPattern(), &init,
                     destTL);
  }

  // Kill the per-element cleanups. The array will take ownership of them.
  for (auto destCleanup : cleanups)
    SGF.Cleanups.setCleanupState(destCleanup, CleanupState::Dead);

  RValue array(SGF, loc, arrayType,
        emitEndVarargs(SGF, loc, std::move(varargsInfo), E->getNumElements()));

  array = scope.popPreservingValue(std::move(array));

  // If we're building an array, we don't have to call the initializer;
  // we've already built one.
  if (arrayType->isEqual(E->getType()))
    return array;

  // Call the builtin initializer.
  PreparedArguments args(AnyFunctionType::Param(E->getType()));
  args.add(E, std::move(array));

  return SGF.emitApplyAllocatingInitializer(
      loc, E->getInitializer(), std::move(args), E->getType(), C);
}

/// Flattens one level of optional from a nested optional value.
static ManagedValue flattenOptional(SILGenFunction &SGF, SILLocation loc,
                                    ManagedValue optVal) {
  // This code assumes that we have a +1 value.
  assert(optVal.isPlusOne(SGF));

  // FIXME: Largely copied from SILGenFunction::emitOptionalToOptional.
  auto contBB = SGF.createBasicBlock();
  auto isNotPresentBB = SGF.createBasicBlock();
  auto isPresentBB = SGF.createBasicBlock();

  SILType resultTy = optVal.getType().getOptionalObjectType();
  auto &resultTL = SGF.getTypeLowering(resultTy);
  assert(resultTy.getASTType().getOptionalObjectType() &&
         "input was not a nested optional value");

  SILValue contBBArg;
  TemporaryInitializationPtr addrOnlyResultBuf;
  if (resultTL.isAddressOnly()) {
    addrOnlyResultBuf = SGF.emitTemporary(loc, resultTL);
  } else {
    contBBArg = contBB->createPhiArgument(resultTy, OwnershipKind::Owned);
  }

  SwitchEnumBuilder SEB(SGF.B, loc, optVal);

  SEB.addOptionalSomeCase(
      isPresentBB, contBB, [&](ManagedValue input, SwitchCaseFullExpr &&scope) {
        if (resultTL.isAddressOnly()) {
          SILValue addr =
              addrOnlyResultBuf->getAddressForInPlaceInitialization(SGF, loc);
          auto *someDecl = SGF.getASTContext().getOptionalSomeDecl();
          input = SGF.B.createUncheckedTakeEnumDataAddr(
              loc, input, someDecl, input.getType().getOptionalObjectType());
          SGF.B.createCopyAddr(loc, input.getValue(), addr, IsNotTake,
                               IsInitialization);
          scope.exitAndBranch(loc);
          return;
        }
        scope.exitAndBranch(loc, input.forward(SGF));
      });
  SEB.addOptionalNoneCase(
      isNotPresentBB, contBB,
      [&](ManagedValue input, SwitchCaseFullExpr &&scope) {
        if (resultTL.isAddressOnly()) {
          SILValue addr =
              addrOnlyResultBuf->getAddressForInPlaceInitialization(SGF, loc);
          SGF.emitInjectOptionalNothingInto(loc, addr, resultTL);
          scope.exitAndBranch(loc);
          return;
        }

        auto mv = SGF.B.createManagedOptionalNone(loc, resultTy).forward(SGF);
        scope.exitAndBranch(loc, mv);
      });
  std::move(SEB).emit();

  // Continue.
  SGF.B.emitBlock(contBB);
  if (resultTL.isAddressOnly()) {
    addrOnlyResultBuf->finishInitialization(SGF);
    return addrOnlyResultBuf->getManagedAddress();
  }
  return SGF.emitManagedRValueWithCleanup(contBBArg, resultTL);
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
  auto selfIfaceTy = ctorDecl->getDeclContext()->getSelfInterfaceType();
  auto selfTy = ctorDecl->mapTypeIntoContext(selfIfaceTy);

  bool isChaining; // Ignored
  auto *otherCtor = E->getCalledConstructor(isChaining)->getDecl();
  assert(otherCtor);

  // The optionality depth of the 'new self' value. This can be '2' if the ctor
  // we are delegating/chaining to is both throwing and failable, or more if
  // 'self' is optional.
  auto srcOptionalityDepth = E->getSubExpr()->getType()->getOptionalityDepth();

  // The optionality depth of the result type of the enclosing initializer in
  // this context.
  const auto destOptionalityDepth =
      ctorDecl->mapTypeIntoContext(ctorDecl->getResultInterfaceType())
          ->getOptionalityDepth();

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
    SGF.emitAddressOfLocalVarDecl(E, selfDecl, selfTy->getCanonicalType(),
                                  SGFAccessKind::Write).getLValueAddress();

  // Flatten a nested optional if 'new self' is a deeper optional than we
  // can return.
  if (srcOptionalityDepth > destOptionalityDepth) {
    assert(destOptionalityDepth > 0);
    assert(otherCtor->isFailable() && otherCtor->hasThrows());

    --srcOptionalityDepth;
    newSelf = flattenOptional(SGF, E, newSelf);

    assert(srcOptionalityDepth == destOptionalityDepth &&
           "Flattening a single level was not enough?");
  }

  // If the enclosing ctor is failable and the optionality depths match, switch
  // on 'new self' to either return 'nil' or continue with the projected value.
  if (srcOptionalityDepth == destOptionalityDepth && ctorDecl->isFailable()) {
    assert(destOptionalityDepth > 0);
    assert(otherCtor->isFailable() || otherCtor->hasThrows());

    SILBasicBlock *someBB = SGF.createBasicBlock();

    auto hasValue = SGF.emitDoesOptionalHaveValue(E, newSelf.getValue());

    assert(SGF.FailDest.isValid() && "too big to fail");

    auto noneBB = SGF.Cleanups.emitBlockForCleanups(SGF.FailDest, E);

    SGF.B.createCondBranch(E, hasValue, someBB, noneBB);

    // Otherwise, project out the value and carry on.
    SGF.B.emitBlock(someBB);

    // If the current constructor is not failable, force out the value.
    newSelf = SGF.emitUncheckedGetOptionalValueFrom(E, newSelf,
                                    SGF.getTypeLowering(newSelf.getType()),
                                                    SGFContext());
  }

  // If we called a constructor that requires a downcast, perform the downcast.
  auto destTy = SGF.getLoweredType(selfTy);
  if (newSelf.getType() != destTy) {
    assert(newSelf.getType().isObject() && destTy.isObject());

    // Assume that the returned 'self' is the appropriate subclass
    // type (or a derived class thereof). Only Objective-C classes can
    // violate this assumption.
    newSelf = SGF.B.createUncheckedRefCast(E, newSelf, destTy);
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

  return SGF.emitEmptyTupleRValue(E, C);
}

static bool isVerbatimNullableTypeInC(SILModule &M, Type ty) {
  ty = ty->getWithoutSpecifierType()->getReferenceStorageReferent();

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
        if (!func->hasImplicitSelfDecl())
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

  // A reference to a potentially dynamic member/subscript property.
  if (auto member = dyn_cast<LookupExpr>(expr)) {
    return isVerbatimNullableTypeInC(M, member->getType()) &&
      mayLieAboutNonOptionalReturn(M, member->getMember().getDecl());
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
    ManagedValue bitcast = SGF.B.createUncheckedBitCast(E, result, optType);
    return RValue(SGF, E, bitcast);
  }

  // Try the bridging peephole.
  if (auto result = tryEmitAsBridgingConversion(SGF, E, false, C)) {
    return RValue(SGF, E, *result);
  }

  auto helper = [E](SILGenFunction &SGF, SILLocation loc, SGFContext C) {
    return SGF.emitRValueAsSingleValue(E->getSubExpr(), C);
  };

  auto result =
    SGF.emitOptionalSome(E, SGF.getLoweredType(E->getType()), helper, C);
  return RValue(SGF, E, result);
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

RValue RValueEmitter::visitTernaryExpr(TernaryExpr *E, SGFContext C) {
  auto &lowering = SGF.getTypeLowering(E->getType());

  auto NumTrueTaken = SGF.loadProfilerCount(E->getThenExpr());
  auto NumFalseTaken = SGF.loadProfilerCount(E->getElseExpr());

  if (lowering.isLoadable() || !SGF.silConv.useLoweredAddresses()) {
    // If the result is loadable, emit each branch and forward its result
    // into the destination block argument.
    
    // FIXME: We could avoid imploding and reexploding tuples here.
    Condition cond = SGF.emitCondition(E->getCondExpr(),
                                       /*invertCondition*/ false,
                                       SGF.getLoweredType(E->getType()),
                                       NumTrueTaken, NumFalseTaken);
    
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
                                       /*invertCondition*/ false,
                                       /*contArgs*/ {},
                                       NumTrueTaken, NumFalseTaken);
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

    SGFAccessKind TheAccessKind;

    /// A flattened list of l-values.
    SmallVectorImpl<std::optional<LValue>> &Results;

  public:
    TupleLValueEmitter(SILGenFunction &SGF, SGFAccessKind accessKind,
                       SmallVectorImpl<std::optional<LValue>> &results)
        : SGF(SGF), TheAccessKind(accessKind), Results(results) {}

    // If the destination is a tuple, recursively destructure.
    void visitTupleExpr(TupleExpr *E) {
      for (auto &elt : E->getElements()) {
        visit(elt);
      }
    }

    // If the destination is '_', queue up a discard.
    void visitDiscardAssignmentExpr(DiscardAssignmentExpr *E) {
      Results.push_back(std::nullopt);
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
    MutableArrayRef<std::optional<LValue>> DestLVQueue;

    std::optional<LValue> &&getNextDest() {
      assert(!DestLVQueue.empty());
      std::optional<LValue> &next = DestLVQueue.front();
      DestLVQueue = DestLVQueue.slice(1);
      return std::move(next);
    }

  public:
    TupleLValueAssigner(SILGenFunction &SGF, SILLocation assignLoc,
                        SmallVectorImpl<std::optional<LValue>> &destLVs)
        : SGF(SGF), AssignLoc(assignLoc), DestLVQueue(destLVs) {}

    /// Top-level entrypoint.
    void emit(CanType destType, RValue &&src) {
      visit(destType, std::move(src));
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

      std::optional<LValue> &&next = getNextDest();

      // If the destination is a discard, do nothing.
      if (!next.has_value())
        return;

      // Otherwise, emit the scalar assignment.
      SGF.emitAssignToLValue(AssignLoc, std::move(src),
                             std::move(next.value()));
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

      dest = dest->getSemanticsProvidingExpr();
      if (isa<DiscardAssignmentExpr>(dest)) {
        // The logical thing to do here would be emitIgnoredExpr, but that
        // changed some test results in a way I wanted to avoid, so instead
        // we're doing this.
        FormalEvaluationScope writeback(SGF);
        auto srcLV = SGF.emitLValue(srcLoad->getSubExpr(),
                                    SGFAccessKind::IgnoredRead);
        RValue rv = SGF.emitLoadOfLValue(loc, std::move(srcLV), SGFContext());
        // If we have a move only type, we need to implode and perform a move to
        // ensure we consume our argument as part of the assignment. Otherwise,
        // we don't do anything.
        if (rv.getLoweredType(SGF).isMoveOnly()) {
          ManagedValue value = std::move(rv).getAsSingleValue(SGF, loc);
          // If we have an address, then ensure plus one will create a temporary
          // copy which will act as a consume of the address value. If we have
          // an object, we need to insert our own move though.
          value = value.ensurePlusOne(SGF, loc);
          if (value.getType().isObject())
            value = SGF.B.createMoveValue(loc, value);
        }
        return;
      }

      FormalEvaluationScope writeback(SGF);
      auto destLV = SGF.emitLValue(dest, SGFAccessKind::Write);
      auto srcLV = SGF.emitLValue(srcLoad->getSubExpr(),
                                  SGFAccessKind::BorrowedAddressRead);
      SGF.emitAssignLValueToLValue(loc, std::move(srcLV), std::move(destLV));
      return;
    }
  }

  // Handle tuple destinations by destructuring them if present.
  CanType destType = dest->getType()->getCanonicalType();

  // But avoid this in the common case.
  if (!isa<TupleType>(destType)) {
    // If we're assigning to a discard, just emit the operand as ignored.
    dest = dest->getSemanticsProvidingExpr();
    if (isa<DiscardAssignmentExpr>(dest)) {
      SGF.emitIgnoredExpr(src);
      return;
    }
    
    FormalEvaluationScope writeback(SGF);
    LValue destLV = SGF.emitLValue(dest, SGFAccessKind::Write);
    SGF.emitAssignToLValue(loc, src, std::move(destLV));
    return;
  }

  FormalEvaluationScope writeback(SGF);

  // Produce a flattened queue of LValues.
  SmallVector<std::optional<LValue>, 4> destLVs;
  TupleLValueEmitter(SGF, SGFAccessKind::Write, destLVs).visit(dest);

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

namespace {
  /// A visitor for creating a flattened list of LValues from a
  /// pattern.
  class PatternLValueEmitter
      : public PatternVisitor<PatternLValueEmitter, Type> {

    SILGenFunction &SGF;

    SGFAccessKind TheAccessKind;

    /// A flattened list of l-values.
    SmallVectorImpl<std::optional<LValue>> &Results;

  public:
    PatternLValueEmitter(SILGenFunction &SGF, SGFAccessKind accessKind,
                         SmallVectorImpl<std::optional<LValue>> &results)
        : SGF(SGF), TheAccessKind(accessKind), Results(results) {}

#define USE_SUBPATTERN(Kind) \
    Type visit##Kind##Pattern(Kind##Pattern *pattern) { \
      return visit(pattern->getSubPattern());            \
    }

    USE_SUBPATTERN(Paren)
    USE_SUBPATTERN(Typed)
    USE_SUBPATTERN(Binding)
#undef USE_SUBPATTERN

#define PATTERN(Kind, Parent)
#define REFUTABLE_PATTERN(Kind, Parent) \
    Type visit##Kind##Pattern(Kind##Pattern *pattern) { \
      llvm_unreachable("No refutable patterns here");    \
    }
#include "swift/AST/PatternNodes.def"

    Type visitTuplePattern(TuplePattern *pattern) {
      SmallVector<TupleTypeElt, 4> tupleElts;
      for (auto &element : pattern->getElements()) {
        Type elementType = visit(element.getPattern());
        tupleElts.push_back(
            TupleTypeElt(elementType, element.getLabel()));
      }

      return TupleType::get(tupleElts, SGF.getASTContext());
    }

    Type visitNamedPattern(NamedPattern *pattern) {
      Type type = LValueType::get(pattern->getDecl()->getTypeInContext());
      auto declRef = new (SGF.getASTContext()) DeclRefExpr(
          pattern->getDecl(), DeclNameLoc(), /*Implicit=*/true,
          AccessSemantics::Ordinary, type);

      Results.push_back(SGF.emitLValue(declRef, TheAccessKind));

      return type;
    }

    Type visitAnyPattern(AnyPattern *pattern) {
      // Discard the value at this position.
      Results.push_back(std::nullopt);

      return LValueType::get(pattern->getType());
    }
  };
}

void SILGenFunction::emitAssignToPatternVars(
    SILLocation loc, Pattern *destPattern, RValue &&src) {
  FormalEvaluationScope writeback(*this);

  // Produce a flattened queue of LValues.
  SmallVector<std::optional<LValue>, 4> destLVs;
  CanType destType = PatternLValueEmitter(
      *this, SGFAccessKind::Write, destLVs).visit(destPattern)
    ->getCanonicalType();

  // Recurse on the type of the destination, pulling LValues as
  // needed from the queue we built up before.
  TupleLValueAssigner(*this, loc, destLVs).emit(destType, std::move(src));
}

ManagedValue SILGenFunction::emitBindOptional(SILLocation loc,
                                              ManagedValue optValue,
                                              unsigned depth) {
  assert(optValue.isPlusOne(*this) && "Can only bind plus one values");
  assert(depth < BindOptionalFailureDests.size());
  auto failureDest = BindOptionalFailureDests[BindOptionalFailureDests.size()
                                                - depth - 1];

  SILBasicBlock *hasValueBB = createBasicBlock();
  SILBasicBlock *hasNoValueBB = createBasicBlock();

  // For move checking purposes, binding always consumes the value whole.
  if (optValue.getType().isMoveOnly() && optValue.getType().isAddress()) {
    optValue = B.createOpaqueConsumeBeginAccess(loc, optValue);
  }

  SILType optValueTy = optValue.getType();
  SwitchEnumBuilder SEB(B, loc, optValue);
  SEB.addOptionalSomeCase(hasValueBB, nullptr,
                          [&](ManagedValue mv, SwitchCaseFullExpr &&expr) {
                            // If mv is not an address, forward it. We will
                            // recreate the cleanup outside when we return the
                            // argument.
                            if (mv.getType().isObject()) {
                              mv.forward(*this);
                            }
                            expr.exit();
                          });
  // If not, thread out through a bunch of cleanups.
  SEB.addOptionalNoneCase(hasNoValueBB, failureDest,
                          [&](ManagedValue mv, SwitchCaseFullExpr &&expr) {
                            expr.exitAndBranch(loc);
                          });
  std::move(SEB).emit();

  // Reset the insertion point at the end of hasValueBB so we can
  // continue to emit code there.
  B.setInsertionPoint(hasValueBB);

  // If optValue was loadable, we emitted a switch_enum. In such a case, return
  // the argument from hasValueBB.
  if (optValue.getType().isLoadable(F) || !silConv.useLoweredAddresses()) {
    return emitManagedRValueWithCleanup(hasValueBB->getArgument(0));
  }

  // Otherwise, if we had an address only value, we emitted the value at +0. In
  // such a case, since we want to model this as a consuming operation. Use
  // ensure_plus_one and extract out the value from there.
  auto *someDecl = getASTContext().getOptionalSomeDecl();
  auto eltTy =
      optValueTy.getObjectType().getOptionalObjectType().getAddressType();
  assert(eltTy);
  SILValue address = optValue.forward(*this);
  return emitManagedBufferWithCleanup(
      B.createUncheckedTakeEnumDataAddr(loc, address, someDecl, eltTy));
}

RValue RValueEmitter::visitBindOptionalExpr(BindOptionalExpr *E, SGFContext C) {
  // Create a temporary of type std::optional<T> if it is address-only.
  auto &optTL = SGF.getTypeLowering(E->getSubExpr()->getType());
  
  ManagedValue optValue;
  if (!SGF.silConv.useLoweredAddresses() || optTL.isLoadable()
      || E->getType()->hasOpenedExistential()) {
    optValue = SGF.emitRValueAsSingleValue(E->getSubExpr());
  } else {
    auto temp = SGF.emitTemporary(E, optTL);

    // Emit the operand into the temporary.
    SGF.emitExprInto(E->getSubExpr(), temp.get());

    // And then grab the managed address.
    optValue = temp->getManagedAddress();
  }

  // Check to see whether the optional is present, if not, jump to the current
  // nil handler block. Otherwise, return the value as the result of the
  // expression.
  optValue = SGF.emitBindOptional(E, optValue, E->getDepth());
  return RValue(SGF, E, optValue);
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

/// emitOptimizedOptionalEvaluation - Look for cases where we can short-circuit
/// evaluation of an OptionalEvaluationExpr by pattern matching the AST.
///
static bool emitOptimizedOptionalEvaluation(SILGenFunction &SGF,
                                            OptionalEvaluationExpr *E,
                                            ManagedValue &result,
                                            SGFContext ctx) {
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

  // SIL defines away abstraction differences between T? and T!,
  // so we can just emit the sub-initialization normally.
  result = SGF.emitRValueAsSingleValue(BO->getSubExpr(), ctx);
  return true;
}

RValue RValueEmitter::visitOptionalEvaluationExpr(OptionalEvaluationExpr *E,
                                                  SGFContext C) {
  if (auto result = tryEmitAsBridgingConversion(SGF, E, false, C)) {
    return RValue(SGF, E, *result);
  }

  SmallVector<ManagedValue, 1> results;
  SGF.emitOptionalEvaluation(E, E->getType(), results, C,
    [&](SmallVectorImpl<ManagedValue> &results, SGFContext primaryC) {
      ManagedValue result;
      if (!emitOptimizedOptionalEvaluation(SGF, E, result, primaryC)) {
        result = SGF.emitRValueAsSingleValue(E->getSubExpr(), primaryC);
      }

      assert(results.empty());
      results.push_back(result);
    });

  assert(results.size() == 1);
  if (results[0].isInContext()) {
    return RValue::forInContext();
  } else {
    return RValue(SGF, E, results[0]);
  }
}

void SILGenFunction::emitOptionalEvaluation(SILLocation loc, Type optType,
                                       SmallVectorImpl<ManagedValue> &results,
                                            SGFContext C,
                        llvm::function_ref<void(SmallVectorImpl<ManagedValue> &,
                                                SGFContext primaryC)>
                                              generateNormalResults) {
  assert(results.empty());

  auto &optTL = getTypeLowering(optType);

  Initialization *optInit = C.getEmitInto();
  bool usingProvidedContext =
    optInit && optInit->canPerformInPlaceInitialization();

  // Form the optional using address operations if the type is address-only or
  // if we already have an address to use.
  bool isByAddress = ((usingProvidedContext || optTL.isAddressOnly()) &&
                      silConv.useLoweredAddresses());

  std::unique_ptr<TemporaryInitialization> optTemp;
  if (!isByAddress) {
    // If the caller produced a context for us, but we're not going
    // to use it, make sure we don't.
    optInit = nullptr;
  } else if (!usingProvidedContext) {
    // Allocate the temporary for the Optional<T> if we didn't get one from the
    // context.  This needs to happen outside of the cleanups scope we're about
    // to push.
    optTemp = emitTemporary(loc, optTL);
    optInit = optTemp.get();
  }
  assert(isByAddress == (optInit != nullptr));

  // Acquire the address to emit into outside of the cleanups scope.
  SILValue optAddr;
  if (isByAddress)
    optAddr = optInit->getAddressForInPlaceInitialization(*this, loc);

  // Enter a cleanups scope.
  FullExpr scope(Cleanups, CleanupLocation(loc));

  // Inside of the cleanups scope, create a new initialization to
  // emit into optAddr.
  std::unique_ptr<TemporaryInitialization> normalInit;
  if (isByAddress) {
    normalInit = useBufferAsTemporary(optAddr, optTL);
  }

  // Install a new optional-failure destination just outside of the
  // cleanups scope.
  SILBasicBlock *failureBB = createBasicBlock();
  RestoreOptionalFailureDest
    restoreFailureDest(*this, JumpDest(failureBB, Cleanups.getCleanupsDepth(),
                                       CleanupLocation(loc)));

  generateNormalResults(results, SGFContext(normalInit.get()));
  assert(results.size() >= 1 && "didn't include a normal result");
  assert(results[0].isInContext() ||
         results[0].getType().getObjectType()
           == optTL.getLoweredType().getObjectType());

  // If we're emitting into the context, make sure the normal value is there.
  if (normalInit && !results[0].isInContext()) {
    normalInit->copyOrInitValueInto(*this, loc, results[0], /*init*/ true);
    normalInit->finishInitialization(*this);
    results[0] = ManagedValue::forInContext();
  }

  // We fell out of the normal result, which generated a T? as either
  // a scalar in normalArgument or directly into normalInit.

  // If we're using by-address initialization, we must've emitted into
  // normalInit.  Forward its cleanup before popping the scope.
  if (isByAddress) {
    normalInit->getManagedAddress().forward(*this);
    normalInit.reset(); // Make sure we don't use this anymore.
  } else {
    assert(!results[0].isInContext());
    results[0].forward(*this);
  }

  // For all the secondary results, forward their cleanups and make sure
  // they're of optional type so that we can inject nil into them in
  // the failure path.
  // (Should this be controllable by the client?)
  for (auto &result : MutableArrayRef<ManagedValue>(results).slice(1)) {
    assert(!result.isInContext() && "secondary result was in context");
    auto resultTy = result.getType();
    assert(resultTy.isObject() && "secondary result wasn't an object");

    // Forward the cleanup.
    SILValue value = result.forward(*this);

    // If it's not already an optional type, make it optional.
    if (!resultTy.getOptionalObjectType()) {
      resultTy = SILType::getOptionalType(resultTy);
      value = B.createOptionalSome(loc, value, resultTy);
      // This is really unprincipled.
      result = ManagedValue::forUnmanagedOwnedValue(value);
    }
  }

  // This concludes the conditional scope.
  scope.pop();
  
  // In the usual case, the code will have emitted one or more branches to the
  // failure block.  However, if the body is simple enough, we can end up with
  // no branches to the failureBB.  Detect this and simplify the generated code
  // if so.
  if (failureBB->pred_empty()) {
    // Remove the dead failureBB.
    failureBB->eraseFromParent();

    // Just re-manage all the secondary results.
    for (auto &result : MutableArrayRef<ManagedValue>(results).slice(1)) {
      result = emitManagedRValueWithCleanup(result.getValue());
    }

    // Just re-manage the main result if we're not using address-based IRGen.
    if (!isByAddress) {
      results[0] = emitManagedRValueWithCleanup(results[0].getValue(), optTL);
      return;
    }

    // Otherwise, we must have emitted into normalInit, which means that,
    // now that we're out of the cleanups scope, we need to finish optInit.
    assert(results[0].isInContext());
    optInit->finishInitialization(*this);

    // If optInit came from the SGFContext, then we've successfully emitted
    // into that.
    if (usingProvidedContext) return;

    // Otherwise, we must have emitted into optTemp.
    assert(optTemp);
    results[0] = optTemp->getManagedAddress();
    return;
  }
  
  // Okay, we do have uses of the failure block, so we'll need to merge
  // control paths.

  SILBasicBlock *contBB = createBasicBlock();

  // Branch to the continuation block.
  SmallVector<SILValue, 4> bbArgs;
  if (!isByAddress)
    bbArgs.push_back(results[0].getValue());
  for (const auto &result : llvm::ArrayRef(results).slice(1))
    bbArgs.push_back(result.getValue());

  // Branch to the continuation block.
  B.createBranch(loc, contBB, bbArgs);

  // In the failure block, inject nil into the result.
  B.emitBlock(failureBB);

  // Note that none of the code here introduces any cleanups.
  // If it did, we'd need to push a scope.
  bbArgs.clear();
  if (isByAddress) {
    emitInjectOptionalNothingInto(loc, optAddr, optTL);
  } else {
    bbArgs.push_back(getOptionalNoneValue(loc, optTL));
  }
  for (const auto &result : llvm::ArrayRef(results).slice(1)) {
    auto resultTy = result.getType();
    bbArgs.push_back(getOptionalNoneValue(loc, getTypeLowering(resultTy)));
  }
  B.createBranch(loc, contBB, bbArgs);

  // Emit the continuation block.
  B.emitBlock(contBB);

  // Create a PHI for the optional result if desired.
  if (isByAddress) {
    assert(results[0].isInContext());
  } else {
    auto arg =
        contBB->createPhiArgument(optTL.getLoweredType(), OwnershipKind::Owned);
    results[0] = emitManagedRValueWithCleanup(arg, optTL);
  }

  // Create PHIs for all the secondary results and manage them.
  for (auto &result : MutableArrayRef<ManagedValue>(results).slice(1)) {
    auto arg =
        contBB->createPhiArgument(result.getType(), OwnershipKind::Owned);
    result = emitManagedRValueWithCleanup(arg);
  }

  // We may need to manage the value in optInit.
  if (!isByAddress) return;

  assert(results[0].isInContext());
  optInit->finishInitialization(*this);

  // If we didn't emit into the provided context, the primary result
  // is really a temporary.
  if (usingProvidedContext) return;

  assert(optTemp);
  results[0] = optTemp->getManagedAddress();
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
RValue RValueEmitter::emitForceValue(ForceValueExpr *loc, Expr *E,
                                     unsigned numOptionalEvaluations,
                                     SGFContext C) {
  auto valueType = E->getType()->getOptionalObjectType();
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
    CleanupLocation cleanupLoc = CleanupLocation(loc);
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
        failureBuilder.createCondFail(loc, trueV, "force unwrapped a nil value");
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

  // If this is an implicit force of an ImplicitlyUnwrappedOptional,
  // and we're emitting into an unbridging conversion, try adjusting the
  // context.
  bool isImplicitUnwrap = loc->isImplicit() &&
    loc->isForceOfImplicitlyUnwrappedOptional();
  if (isImplicitUnwrap) {
    if (auto conv = C.getAsConversion()) {
      if (auto adjusted = conv->getConversion().adjustForInitialForceValue()) {
        auto value =
          conv->emitWithAdjustedConversion(SGF, loc, *adjusted,
                       [E](SILGenFunction &SGF, SILLocation loc, SGFContext C) {
          return SGF.emitRValueAsSingleValue(E, C);
        });
        return RValue(SGF, loc, value);
      }
    }
  }

  // Otherwise, emit the optional and force its value out.
  const TypeLowering &optTL = SGF.getTypeLowering(E->getType());
  ManagedValue opt = SGF.emitRValueAsSingleValue(E);
  ManagedValue V =
    SGF.emitCheckedGetOptionalValueFrom(loc, opt, isImplicitUnwrap, optTL, C);
  return RValue(SGF, loc, valueType->getCanonicalType(), V);
}

void SILGenFunction::emitOpenExistentialExprImpl(
       OpenExistentialExpr *E,
       llvm::function_ref<void(Expr *)> emitSubExpr) {
  assert(isInFormalEvaluationScope());

  // Emit the existential value.
  if (E->getExistentialValue()->getType()->is<LValueType>()) {
    bool inserted = OpaqueValueExprs.insert({E->getOpaqueValue(), E}).second;
    (void)inserted;
    assert(inserted && "already have this opened existential?");

    emitSubExpr(E->getSubExpr());
    return;
  }

  auto existentialValue = emitRValueAsSingleValue(
      E->getExistentialValue(),
      SGFContext::AllowGuaranteedPlusZero);

  Type opaqueValueType = E->getOpaqueValue()->getType()->getRValueType();
  auto payload = emitOpenExistential(
      E, existentialValue,
      getLoweredType(opaqueValueType),
      AccessKind::Read);

  // Register the opaque value for the projected existential.
  SILGenFunction::OpaqueValueRAII opaqueValueRAII(
                                    *this, E->getOpaqueValue(), payload);

  emitSubExpr(E->getSubExpr());
}

RValue RValueEmitter::visitOpenExistentialExpr(OpenExistentialExpr *E,
                                               SGFContext C) {
  if (auto result = tryEmitAsBridgingConversion(SGF, E, false, C)) {
    return RValue(SGF, E, *result);
  }

  FormalEvaluationScope writebackScope(SGF);
  return SGF.emitOpenExistentialExpr<RValue>(E,
                                             [&](Expr *subExpr) -> RValue {
                                               return visit(subExpr, C);
                                             });
}

RValue RValueEmitter::visitMakeTemporarilyEscapableExpr(
    MakeTemporarilyEscapableExpr *E, SGFContext C) {
  // Emit the non-escaping function value.
  auto functionValue =
    visit(E->getNonescapingClosureValue()).getAsSingleValue(SGF, E);

  auto escapingFnTy = SGF.getLoweredType(E->getOpaqueValue()->getType());
  auto silFnTy = escapingFnTy.castTo<SILFunctionType>();

  auto visitSubExpr = [&](ManagedValue escapingClosure,
                          bool isClosureConsumable) -> RValue {
    // Bind the opaque value to the escaping function.
    assert(isClosureConsumable == escapingClosure.hasCleanup());
    SILGenFunction::OpaqueValueRAII pushOpaqueValue(SGF, E->getOpaqueValue(),
                                                    escapingClosure);

    // Emit the guarded expression.
    return visit(E->getSubExpr(), C);
  };

  // Handle @convention(block) an @convention(c). No withoutActuallyEscaping
  // verification yet.
  auto closureRepresentation = silFnTy->getExtInfo().getRepresentation();
  if (closureRepresentation != SILFunctionTypeRepresentation::Thick) {
    auto escapingClosure =
        SGF.B.createConvertFunction(E, functionValue, escapingFnTy,
                                    /*WithoutActuallyEscaping=*/true);
    bool isBlockConvention =
        closureRepresentation == SILFunctionTypeRepresentation::Block;
    return visitSubExpr(escapingClosure,
                        isBlockConvention /*isClosureConsumable*/);
  }

  // Convert it to an escaping function value.
  auto escapingClosure =
      SGF.createWithoutActuallyEscapingClosure(E, functionValue, escapingFnTy);
  auto loc = SILLocation(E);
  auto borrowedClosure = escapingClosure.borrow(SGF, loc);
  RValue rvalue = visitSubExpr(borrowedClosure, false /* isClosureConsumable */);

  // Now create the verification of the withoutActuallyEscaping operand.
  // Either we fail the uniqueness check (which means the closure has escaped)
  // and abort or we continue and destroy the ultimate reference.
  auto isEscaping = SGF.B.createIsEscapingClosure(
      loc, borrowedClosure.getValue(),
      IsEscapingClosureInst::WithoutActuallyEscaping);
  SGF.B.createCondFail(loc, isEscaping, "non-escaping closure has escaped");
  return rvalue;
}

RValue RValueEmitter::visitOpaqueValueExpr(OpaqueValueExpr *E, SGFContext C) {
  auto found = SGF.OpaqueValues.find(E);
  assert(found != SGF.OpaqueValues.end());
  return RValue(SGF, E, SGF.manageOpaqueValue(found->second, E, C));
}

RValue RValueEmitter::visitPropertyWrapperValuePlaceholderExpr(
    PropertyWrapperValuePlaceholderExpr *E, SGFContext C) {
  return visitOpaqueValueExpr(E->getOpaqueValuePlaceholder(), C);
}

RValue RValueEmitter::visitAppliedPropertyWrapperExpr(
    AppliedPropertyWrapperExpr *E, SGFContext C) {
  auto *param = const_cast<ParamDecl *>(E->getParamDecl());
  auto argument = visit(E->getValue());
  SILDeclRef::Kind initKind;
  switch (E->getValueKind()) {
  case swift::AppliedPropertyWrapperExpr::ValueKind::WrappedValue:
    initKind = SILDeclRef::Kind::PropertyWrapperBackingInitializer;
    break;
  case swift::AppliedPropertyWrapperExpr::ValueKind::ProjectedValue:
    initKind = SILDeclRef::Kind::PropertyWrapperInitFromProjectedValue;
    break;
  }

  // The property wrapper generator function needs the same substitutions as the
  // enclosing function or closure. If the parameter is declared in a function, take
  // the substitutions from the concrete callee. Otherwise, forward the archetypes
  // from the closure.
  SubstitutionMap subs;
  if (param->getDeclContext()->getAsDecl()) {
    subs = E->getCallee().getSubstitutions();
  } else {
    subs = SGF.getForwardingSubstitutionMap();
  }

  return SGF.emitApplyOfPropertyWrapperBackingInitializer(
      SILLocation(E), param, subs, std::move(argument), initKind);
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
  clone(SILGenFunction &SGF, SILLocation l) const override {
    return std::unique_ptr<LogicalPathComponent>(
      new AutoreleasingWritebackComponent(getTypeData()));
  }

  virtual bool isLoadingPure() const override { return true; }
  
  void set(SILGenFunction &SGF, SILLocation loc,
           ArgumentSource &&value, ManagedValue base) && override {
    // Convert the value back to a +1 strong reference.
    auto unowned = std::move(value).getAsSingleValue(SGF).getUnmanagedValue();
    auto strongType = SILType::getPrimitiveObjectType(
              unowned->getType().castTo<UnmanagedStorageType>().getReferentType());
    auto owned = SGF.B.createUnmanagedToRef(loc, unowned, strongType);
    auto ownedMV = SGF.emitManagedCopy(loc, owned);

    // Then create a mark dependence in between the base and the ownedMV. This
    // is important to ensure that the destroy of the assign is not hoisted
    // above the retain. We are doing unmanaged things here so we need to be
    // extra careful.
    ownedMV = SGF.B.createMarkDependence(loc, ownedMV, base,
                                         MarkDependenceKind::Escaping);

    // Then reassign the mark dependence into the +1 storage.
    ownedMV.assignInto(SGF, loc, base.getUnmanagedValue());
  }
  
  RValue get(SILGenFunction &SGF, SILLocation loc,
             ManagedValue base, SGFContext c) && override {
    FullExpr TightBorrowScope(SGF.Cleanups, CleanupLocation(loc));

    // Load the value at +0.
    ManagedValue loadedBase = SGF.B.createLoadBorrow(loc, base);

    // Convert it to unowned.
    auto refType = loadedBase.getType().getASTType();
    auto unownedType = SILType::getPrimitiveObjectType(
                                        CanUnmanagedStorageType::get(refType));
    SILValue unowned = SGF.B.createRefToUnmanaged(
        loc, loadedBase.getUnmanagedValue(), unownedType);

    // A reference type should never be exploded.
    return RValue(SGF, ManagedValue::forUnownedObjectValue(unowned), refType);
  }

  std::optional<AccessStorage> getAccessStorage() const override {
    return std::nullopt;
  }

  void dump(raw_ostream &OS, unsigned indent) const override {
    OS.indent(indent) << "AutoreleasingWritebackComponent()\n";
  }
};
} // end anonymous namespace

SILGenFunction::PointerAccessInfo
SILGenFunction::getPointerAccessInfo(Type type) {
  PointerTypeKind pointerKind;
  Type elt = type->getAnyPointerElementType(pointerKind);
  assert(elt && "not a pointer");
  (void)elt;

  SGFAccessKind accessKind =
    ((pointerKind == PTK_UnsafePointer || pointerKind == PTK_UnsafeRawPointer)
       ? SGFAccessKind::BorrowedAddressRead : SGFAccessKind::ReadWrite);

  return { type->getCanonicalType(), pointerKind, accessKind };
}

RValue RValueEmitter::visitInOutToPointerExpr(InOutToPointerExpr *E,
                                              SGFContext C) {
  // If we're converting on the behalf of an
  // AutoreleasingUnsafeMutablePointer, convert the lvalue to
  // unowned(unsafe), so we can point at +0 storage.
  auto accessInfo = SGF.getPointerAccessInfo(E->getType());

  // Get the original lvalue.
  LValue lv = SGF.emitLValue(E->getSubExpr(), accessInfo.AccessKind);

  auto ptr = SGF.emitLValueToPointer(E, std::move(lv), accessInfo);
  return RValue(SGF, E, ptr);
}

/// Implicit conversion from a nontrivial inout type to a raw pointer are
/// dangerous. For example:
///
///   func bar(_ p: UnsafeRawPointer) { ... }
///   func foo(object: inout AnyObject) {
///       bar(&object)
///   }
///
/// These conversions should be done explicitly.
///
static void diagnoseImplicitRawConversion(Type sourceTy, Type pointerTy,
                                          SILLocation loc,
                                          SILGenFunction &SGF) {
  // Array conversion does not always go down the ArrayConverter
  // path. Recognize the Array source type here both for ArrayToPointer and
  // InoutToPointer cases and diagnose on the element type.
  Type eltTy = sourceTy->isArrayType();
  if (!eltTy)
    eltTy = sourceTy;

  if (SGF.getLoweredType(eltTy).isTrivial(SGF.F))
    return;

  auto &ctx = SGF.getASTContext();
  if (auto *bitwiseCopyableDecl = ctx.getProtocol(
        KnownProtocolKind::BitwiseCopyable)) {
    if (checkConformance(eltTy, bitwiseCopyableDecl))
      return;
  }
  if (auto *fixedWidthIntegerDecl = ctx.getProtocol(
          KnownProtocolKind::FixedWidthInteger)) {
    if (checkConformance(eltTy, fixedWidthIntegerDecl))
      return;
  }

  PointerTypeKind kindOfPtr;
  auto pointerElt = pointerTy->getAnyPointerElementType(kindOfPtr);
  assert(!pointerElt.isNull() && "expected an unsafe pointer type");

  // The element type may contain a reference. Disallow conversion to a "raw"
  // pointer type. Consider Int8/UInt8 to be raw pointers. Trivial element types
  // are filtered out above, so Int8/UInt8 pointers can't match the source
  // type. But the type checker may have allowed these for direct C calls, in
  // which Int8/UInt8 are equivalent to raw pointers..
  if (!(pointerElt->isVoid() || pointerElt->isInt8() || pointerElt->isUInt8()))
    return;

  if (sourceTy->isString()) {
    SGF.SGM.diagnose(loc, diag::nontrivial_string_to_rawpointer_conversion,
                     pointerTy);
  } else {
    SGF.SGM.diagnose(loc, diag::nontrivial_to_rawpointer_conversion, sourceTy,
                     pointerTy, eltTy);
  }
}

namespace {
/// Cleanup to insert fix_lifetime on an LValue address.
class FixLifetimeLValueCleanup : public Cleanup {
  friend LValueToPointerFormalAccess;

  FormalEvaluationContext::stable_iterator depth;

public:
  FixLifetimeLValueCleanup() : depth() {}

  LValueToPointerFormalAccess &getFormalAccess(SILGenFunction &SGF) const {
    auto &access = *SGF.FormalEvalContext.find(depth);
    return static_cast<LValueToPointerFormalAccess &>(access);
  }

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    getFormalAccess(SGF).finish(SGF);
  }

  SILValue getAddress(SILGenFunction &SGF) const {
    return getFormalAccess(SGF).address;
  }

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "FixLifetimeLValueCleanup "
                 << "State:" << getState() << " "
                 << "Address: " << getAddress(SGF) << "\n";
#endif
  }
};
} // end anonymous namespace

SILValue LValueToPointerFormalAccess::enter(SILGenFunction &SGF,
                                            SILLocation loc,
                                            SILValue address) {
  auto &lowering = SGF.getTypeLowering(address->getType().getObjectType());
  SILValue pointer = SGF.B.createAddressToPointer(
    loc, address, SILType::getRawPointerType(SGF.getASTContext()),
    /*needsStackProtection=*/ true);
  if (!lowering.isTrivial()) {
    assert(SGF.isInFormalEvaluationScope() &&
           "Must be in formal evaluation scope");
    auto &cleanup = SGF.Cleanups.pushCleanup<FixLifetimeLValueCleanup>();
    CleanupHandle handle = SGF.Cleanups.getTopCleanup();
    SGF.FormalEvalContext.push<LValueToPointerFormalAccess>(loc, address,
                                                            handle);
    cleanup.depth = SGF.FormalEvalContext.stable_begin();
  }
  return pointer;
}

// Address-to-pointer conversion always requires either a fix_lifetime or
// mark_dependence. Emitting a fix_lifetime immediately after the call as
// opposed to a mark_dependence allows the lvalue's lifetime to be optimized
// outside of this narrow scope.
void LValueToPointerFormalAccess::finishImpl(SILGenFunction &SGF) {
  SGF.B.emitFixLifetime(loc, address);
}

/// Convert an l-value to a pointer type: unsafe, unsafe-mutable, or
/// autoreleasing-unsafe-mutable.
ManagedValue SILGenFunction::emitLValueToPointer(SILLocation loc, LValue &&lv,
                                                PointerAccessInfo pointerInfo) {
  assert(pointerInfo.AccessKind == lv.getAccessKind());

  diagnoseImplicitRawConversion(lv.getSubstFormalType(),
                                pointerInfo.PointerType, loc, *this);

  // The incoming lvalue should be at the abstraction level of T in
  // Unsafe*Pointer<T>. Reabstract it if necessary.
  auto opaqueTy = AbstractionPattern::getOpaque();
  auto loweredTy = getLoweredType(opaqueTy, lv.getSubstFormalType());
  if (lv.getTypeOfRValue().getASTType() != loweredTy.getASTType()) {
    lv.addSubstToOrigComponent(opaqueTy, loweredTy);
  }

  switch (pointerInfo.PointerKind) {
  case PTK_UnsafeMutablePointer:
  case PTK_UnsafePointer:
  case PTK_UnsafeMutableRawPointer:
  case PTK_UnsafeRawPointer:
    // +1 is fine.
    break;

  case PTK_AutoreleasingUnsafeMutablePointer: {
    // Set up a writeback through a +0 buffer.
    LValueTypeData typeData = lv.getTypeData();
    auto rvalueType = CanUnmanagedStorageType::get(typeData.TypeOfRValue);

    LValueTypeData unownedTypeData(
      lv.getAccessKind(),
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
    emitAddressOfLValue(loc, std::move(lv)).getUnmanagedValue();

  SILValue pointer = LValueToPointerFormalAccess::enter(*this, loc, address);
  
  // Disable nested writeback scopes for any calls evaluated during the
  // conversion intrinsic.
  InOutConversionScope scope(*this);
  
  // Invoke the conversion intrinsic.
  FuncDecl *converter =
    getASTContext().getConvertInOutToPointerArgument();

  auto pointerType = pointerInfo.PointerType;
  auto subMap = pointerType->getContextSubstitutionMap(getPointerProtocol());
  return emitApplyOfLibraryIntrinsic(
             loc, converter, subMap,
             ManagedValue::forObjectRValueWithoutOwnership(pointer),
             SGFContext())
      .getAsSingleValue(*this, loc);
}

RValue RValueEmitter::visitArrayToPointerExpr(ArrayToPointerExpr *E,
                                              SGFContext C) {
  FormalEvaluationScope writeback(SGF);

  auto subExpr = E->getSubExpr();
  auto accessInfo = SGF.getArrayAccessInfo(E->getType(),
                                     subExpr->getType()->getInOutObjectType());

  // Convert the array mutably if it's being passed inout.
  ManagedValue array;
  if (accessInfo.AccessKind == SGFAccessKind::ReadWrite) {
    array = SGF.emitAddressOfLValue(subExpr,
                  SGF.emitLValue(subExpr, SGFAccessKind::ReadWrite));
  } else {
    assert(isReadAccess(accessInfo.AccessKind));
    array = SGF.emitRValueAsSingleValue(subExpr);
  }

  auto pointer = SGF.emitArrayToPointer(E, array, accessInfo).first;
  return RValue(SGF, E, pointer);
}

SILGenFunction::ArrayAccessInfo
SILGenFunction::getArrayAccessInfo(Type pointerType, Type arrayType) {
  auto pointerAccessInfo = getPointerAccessInfo(pointerType);
  return { pointerType, arrayType, pointerAccessInfo.AccessKind };
}

std::pair<ManagedValue, ManagedValue>
SILGenFunction::emitArrayToPointer(SILLocation loc, LValue &&lv,
                                   ArrayAccessInfo accessInfo) {
  auto array = emitAddressOfLValue(loc, std::move(lv));
  return emitArrayToPointer(loc, array, accessInfo);
}

std::pair<ManagedValue, ManagedValue>
SILGenFunction::emitArrayToPointer(SILLocation loc, ManagedValue array,
                                   ArrayAccessInfo accessInfo) {
  auto &ctx = getASTContext();

  FuncDecl *converter;
  if (accessInfo.AccessKind != SGFAccessKind::ReadWrite) {
    assert(isReadAccess(accessInfo.AccessKind));
    converter = ctx.getConvertConstArrayToPointerArgument();
    if (array.isLValue())
      array = B.createLoadCopy(loc, array);

  } else {
    converter = ctx.getConvertMutableArrayToPointerArgument();
    assert(array.isLValue());
  }

  diagnoseImplicitRawConversion(accessInfo.ArrayType, accessInfo.PointerType,
                                loc, *this);

  // Invoke the conversion intrinsic, which will produce an owner-pointer pair.
  SmallVector<Type, 2> replacementTypes;
  replacementTypes.push_back(
      accessInfo.ArrayType->castTo<BoundGenericStructType>()->getGenericArgs()[0]);
  replacementTypes.push_back(accessInfo.PointerType);

  auto genericSig = converter->getGenericSignature();
  auto subMap = SubstitutionMap::get(genericSig, replacementTypes,
                                     LookUpConformanceInModule());

  SmallVector<ManagedValue, 2> resultScalars;
  emitApplyOfLibraryIntrinsic(loc, converter, subMap, array, SGFContext())
      .getAll(resultScalars);
  assert(resultScalars.size() == 2);

  // Mark the dependence of the pointer on the owner value.
  auto owner = resultScalars[0];
  auto pointer = resultScalars[1].forward(*this);
  pointer = B.createMarkDependence(loc, pointer, owner.getValue(),
                                   MarkDependenceKind::Escaping);

  // The owner's already in its own cleanup.  Return the pointer.
  return {ManagedValue::forObjectRValueWithoutOwnership(pointer), owner};
}

RValue RValueEmitter::visitStringToPointerExpr(StringToPointerExpr *E,
                                               SGFContext C) {
  // Get the original value.
  ManagedValue orig = SGF.emitRValueAsSingleValue(E->getSubExpr());

  // Perform the conversion.
  auto results = SGF.emitStringToPointer(E, orig, E->getType());

  // Implicitly leave the owner managed and return the pointer.
  return RValue(SGF, E, results.first);
}

std::pair<ManagedValue, ManagedValue>
SILGenFunction::emitStringToPointer(SILLocation loc, ManagedValue stringValue,
                                    Type pointerType) {
  auto &Ctx = getASTContext();
  FuncDecl *converter = Ctx.getConvertConstStringToUTF8PointerArgument();
  
  // Invoke the conversion intrinsic, which will produce an owner-pointer pair.
  auto subMap = pointerType->getContextSubstitutionMap(getPointerProtocol());
  SmallVector<ManagedValue, 2> results;
  emitApplyOfLibraryIntrinsic(loc, converter, subMap, stringValue, SGFContext())
    .getAll(results);
  assert(results.size() == 2);

  // Mark the dependence of the pointer on the owner value.
  auto owner = results[0];
  auto pointer = results[1].forward(*this);
  pointer = B.createMarkDependence(loc, pointer, owner.getValue(),
                                   MarkDependenceKind::Escaping);

  return {ManagedValue::forObjectRValueWithoutOwnership(pointer), owner};
}

RValue RValueEmitter::visitPointerToPointerExpr(PointerToPointerExpr *E,
                                                SGFContext C) {
  auto &Ctx = SGF.getASTContext();
  auto converter = Ctx.getConvertPointerToPointerArgument();

  // Get the original pointer value, abstracted to the converter function's
  // expected level.
  AbstractionPattern origTy(converter->getInterfaceType());
  origTy = origTy.getFunctionParamType(0);

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
  ManagedValue result = SGF.B.createUncheckedRefCast(
                        E, orig, SGF.getLoweredType(E->getType()));
  return RValue(SGF, E, E->getType()->getCanonicalType(), result);
}

RValue RValueEmitter::visitUnevaluatedInstanceExpr(UnevaluatedInstanceExpr *E,
                                                   SGFContext C) {
  llvm_unreachable("unevaluated_instance expression can never be evaluated");
}

RValue RValueEmitter::visitDifferentiableFunctionExpr(
    DifferentiableFunctionExpr *E, SGFContext C) {
  auto origFunc = SGF.emitRValueAsSingleValue(E->getSubExpr());
  auto destTy = SGF.getLoweredType(E->getType()).castTo<SILFunctionType>();
  auto *diffFunc = SGF.B.createDifferentiableFunction(
      E, destTy->getDifferentiabilityParameterIndices(),
      destTy->getDifferentiabilityResultIndices(), origFunc.forward(SGF));
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(diffFunc));
}

RValue RValueEmitter::visitLinearFunctionExpr(
    LinearFunctionExpr *E, SGFContext C) {
  auto origFunc = SGF.emitRValueAsSingleValue(E->getSubExpr());
  auto destTy = SGF.getLoweredType(E->getType()).castTo<SILFunctionType>();
  auto *diffFunc = SGF.B.createLinearFunction(
      E, destTy->getDifferentiabilityParameterIndices(), origFunc.forward(SGF));
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(diffFunc));
}

RValue RValueEmitter::visitDifferentiableFunctionExtractOriginalExpr(
    DifferentiableFunctionExtractOriginalExpr *E, SGFContext C) {
  auto diffFunc = SGF.emitRValueAsSingleValue(E->getSubExpr());
  auto borrowedDiffFunc = diffFunc.borrow(SGF, E);
  auto *borrowedOrigFunc = SGF.B.createDifferentiableFunctionExtractOriginal(
      E, borrowedDiffFunc.getValue());
  auto ownedOrigFunc = SGF.B.emitCopyValueOperation(E, borrowedOrigFunc);
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(ownedOrigFunc));
}

RValue RValueEmitter::visitLinearFunctionExtractOriginalExpr(
    LinearFunctionExtractOriginalExpr *E, SGFContext C) {
  auto diffFunc = SGF.emitRValueAsSingleValue(E->getSubExpr());
  auto borrowedDiffFunc = diffFunc.borrow(SGF, E);
  auto *borrowedOrigFunc = SGF.B.createLinearFunctionExtract(
      E, LinearDifferentiableFunctionTypeComponent::Original,
      borrowedDiffFunc.getValue());
  auto ownedOrigFunc = SGF.B.emitCopyValueOperation(E, borrowedOrigFunc);
  return RValue(SGF, E, SGF.emitManagedRValueWithCleanup(ownedOrigFunc));
}

RValue RValueEmitter::visitLinearToDifferentiableFunctionExpr(
    LinearToDifferentiableFunctionExpr *E, SGFContext C) {
  // TODO: Implement this.
  llvm_unreachable("Unsupported!");
}

RValue RValueEmitter::visitTapExpr(TapExpr *E, SGFContext C) {
  // This implementation is not very robust; if TapExpr were to ever become
  // user-accessible (as some sort of "with" statement), it should probably
  // permit a full pattern binding, saving the unused parts and "re-structuring"
  // them to return the modified value.

  auto Var = E->getVar();
  auto VarType = E->getType()->getCanonicalType();

  Scope outerScope(SGF, CleanupLocation(E));

  // Initialize the var with our SubExpr.
  auto VarInit =
    SGF.emitInitializationForVarDecl(Var, /*forceImmutable=*/false);
  SGF.emitExprInto(E->getSubExpr(), VarInit.get(), SILLocation(E));

  // Emit the body and let it mutate the var if it chooses.
  SGF.emitStmt(E->getBody());

  // Retrieve and return the var, making it +1 so it survives the scope.
  auto result = SGF.emitRValueForDecl(SILLocation(E), Var, 
                                      VarType, AccessSemantics::Ordinary, C);
  result = std::move(result).ensurePlusOne(SGF, SILLocation(E));
  return outerScope.popPreservingValue(std::move(result));
}

RValue RValueEmitter::visitDefaultArgumentExpr(DefaultArgumentExpr *E,
                                               SGFContext C) {
  // We should only be emitting this as an rvalue for caller-side default
  // arguments such as magic literals. Other default arguments get handled
  // specially.
  return SGF.emitRValue(E->getCallerSideDefaultExpr());
}

RValue RValueEmitter::visitErrorExpr(ErrorExpr *E, SGFContext C) {
  // Running into an ErrorExpr here means we've failed to lazily typecheck
  // something. Just emit an undef of the appropriate type and carry on.
  if (SGF.getASTContext().Diags.hadAnyError())
    return SGF.emitUndefRValue(E, E->getType());

  // Use report_fatal_error to ensure we trap in release builds instead of
  // miscompiling.
  llvm::report_fatal_error("Found an ErrorExpr but didn't emit an error?");
}

RValue RValueEmitter::visitConsumeExpr(ConsumeExpr *E, SGFContext C) {
  auto *subExpr = E->getSubExpr();
  auto subASTType = subExpr->getType()->getCanonicalType();
  auto subType = SGF.getLoweredType(subASTType);

  if (auto *li = dyn_cast<LoadExpr>(subExpr)) {
    FormalEvaluationScope writeback(SGF);
    LValue lv =
      SGF.emitLValue(li->getSubExpr(), SGFAccessKind::ReadWrite);
    auto address = SGF.emitAddressOfLValue(subExpr, std::move(lv));
    auto optTemp = SGF.emitTemporary(E, SGF.getTypeLowering(subType));
    SILValue toAddr = optTemp->getAddressForInPlaceInitialization(SGF, E);
    SILValue fromAddr = address.getLValueAddress();
    bool isMoveOnly = fromAddr->getType().isMoveOnly() ||
                      isa<MoveOnlyWrapperToCopyableAddrInst>(fromAddr);
    if (toAddr->getType().isMoveOnlyWrapped())
      toAddr = SGF.B.createMoveOnlyWrapperToCopyableAddr(subExpr, toAddr);
    if (isMoveOnly) {
      if (fromAddr->getType().isMoveOnlyWrapped())
        fromAddr = SGF.B.createMoveOnlyWrapperToCopyableAddr(subExpr, fromAddr);
      SGF.B.createCopyAddr(subExpr, fromAddr, toAddr, IsNotTake,
                           IsInitialization);
    } else {
      SGF.B.createMarkUnresolvedMoveAddr(subExpr, fromAddr, toAddr);
    }
    optTemp->finishInitialization(SGF);

    if (subType.isLoadable(SGF.F) || !SGF.useLoweredAddresses()) {
      ManagedValue value = SGF.B.createLoadTake(E, optTemp->getManagedAddress());
      if (value.getType().isTrivial(SGF.F))
        return RValue(SGF, {value}, subType.getASTType());
      return RValue(SGF, {value}, subType.getASTType());
    }

    return RValue(SGF, {optTemp->getManagedAddress()}, subType.getASTType());
  }

  if (subType.isLoadable(SGF.F) || !SGF.useLoweredAddresses()) {
    ManagedValue mv = SGF.emitRValue(subExpr).getAsSingleValue(SGF, subExpr);
    if (mv.getType().isTrivial(SGF.F))
      return RValue(SGF, {mv}, subType.getASTType());
    mv = SGF.B.createMoveValue(E, mv);
    // Set the flag so we check this.
    cast<MoveValueInst>(mv.getValue())->setAllowsDiagnostics(true);
    if (subType.isMoveOnly()) {
      // We need to move-only-check the moved value.
      mv = SGF.B.createMarkUnresolvedNonCopyableValueInst(
          E, mv,
          MarkUnresolvedNonCopyableValueInst::CheckKind::
              ConsumableAndAssignable);
    }
    return RValue(SGF, {mv}, subType.getASTType());
  }

  // If we aren't loadable, then create a temporary initialization and
  // explicit_copy_addr into that.
  std::unique_ptr<TemporaryInitialization> optTemp;
  optTemp = SGF.emitTemporary(E, SGF.getTypeLowering(subType));
  SILValue toAddr = optTemp->getAddressForInPlaceInitialization(SGF, E);
  assert(!isa<LValueType>(E->getType()->getCanonicalType()) &&
         "Shouldn't see an lvalue type here");

  ManagedValue mv =
      SGF.emitRValue(subExpr, SGFContext(SGFContext::AllowImmediatePlusZero))
          .getAsSingleValue(SGF, subExpr);
  assert(mv.getType().isAddress());
  bool isMoveOnly = mv.getType().isMoveOnly() ||
                    isa<MoveOnlyWrapperToCopyableAddrInst>(mv.getValue());
  SILValue fromAddr = mv.getValue();
  if (toAddr->getType().isMoveOnlyWrapped())
    toAddr = SGF.B.createMoveOnlyWrapperToCopyableAddr(subExpr, toAddr);
  if (isMoveOnly) {
    if (fromAddr->getType().isMoveOnlyWrapped())
      fromAddr = SGF.B.createMoveOnlyWrapperToCopyableAddr(subExpr, fromAddr);
    SGF.B.createCopyAddr(subExpr, fromAddr, toAddr, IsNotTake,
                         IsInitialization);
  } else {
    SGF.B.createMarkUnresolvedMoveAddr(subExpr, mv.getValue(), toAddr);
  }
  optTemp->finishInitialization(SGF);
  return RValue(SGF, {optTemp->getManagedAddress()}, subType.getASTType());
}

RValue RValueEmitter::visitCopyExpr(CopyExpr *E, SGFContext C) {
  auto *subExpr = E->getSubExpr();
  auto subASTType = subExpr->getType()->getCanonicalType();
  auto subType = SGF.getLoweredType(subASTType);

  if (auto *li = dyn_cast<LoadExpr>(subExpr)) {
    FormalEvaluationScope writeback(SGF);
    LValue lv =
        SGF.emitLValue(li->getSubExpr(), SGFAccessKind::BorrowedAddressRead);
    auto address = SGF.emitAddressOfLValue(subExpr, std::move(lv));

    if (subType.isLoadable(SGF.F)) {
      // Use a formal access load borrow so this closes in the writeback scope
      // above.
      ManagedValue value = SGF.B.createFormalAccessLoadBorrow(E, address);

      if (value.getType().isMoveOnlyWrapped()) {
        value = SGF.B.createGuaranteedMoveOnlyWrapperToCopyableValue(E, value);
        // If we have a trivial value after unwrapping, just return that.
        if (value.getType().isTrivial(SGF.F))
          return RValue(SGF, {value}, subType.getASTType());
      }

      // We purposely, use a lexical cleanup here so that the cleanup lasts
      // through the formal evaluation scope.
      ManagedValue copy = SGF.B.createExplicitCopyValue(E, value);

      return RValue(SGF, {copy}, subType.getASTType());
    }

    auto optTemp = SGF.emitTemporary(E, SGF.getTypeLowering(subType));
    SILValue toAddr = optTemp->getAddressForInPlaceInitialization(SGF, E);
    if (toAddr->getType().isMoveOnlyWrapped())
      toAddr = SGF.B.createMoveOnlyWrapperToCopyableAddr(E, toAddr);
    SILValue fromAddr = address.getLValueAddress();
    if (fromAddr->getType().isMoveOnlyWrapped())
      fromAddr = SGF.B.createMoveOnlyWrapperToCopyableAddr(E, fromAddr);
    SGF.B.createExplicitCopyAddr(subExpr, fromAddr, toAddr, IsNotTake,
                                 IsInitialization);
    optTemp->finishInitialization(SGF);
    return RValue(SGF, {optTemp->getManagedAddress()}, subType.getASTType());
  }

  if (subType.isLoadable(SGF.F)) {
    ManagedValue mv = SGF.emitRValue(subExpr).getAsSingleValue(SGF, subExpr);
    if (mv.getType().isTrivial(SGF.F))
      return RValue(SGF, {mv}, subType.getASTType());
    {
      // We use a formal evaluation scope so we tightly scope the formal access
      // borrow below.
      FormalEvaluationScope scope(SGF);
      if (mv.getType().isMoveOnlyWrapped()) {
        if (mv.getOwnershipKind() != OwnershipKind::Guaranteed)
          mv = mv.formalAccessBorrow(SGF, E);
        mv = SGF.B.createGuaranteedMoveOnlyWrapperToCopyableValue(E, mv);
      }

      // Only perform the actual explicit_copy_value if we do not have a trivial
      // type.
      //
      // DISCUSSION: We can only get a trivial type if we have a moveonlywrapped
      // type of a trivial type.
      if (!mv.getType().isTrivial(SGF.F))
        mv = SGF.B.createExplicitCopyValue(E, mv);
    }
    return RValue(SGF, {mv}, subType.getASTType());
  }

  // If we aren't loadable, then create a temporary initialization and
  // explicit_copy_addr into that.
  std::unique_ptr<TemporaryInitialization> optTemp;
  optTemp = SGF.emitTemporary(E, SGF.getTypeLowering(subType));
  SILValue toAddr = optTemp->getAddressForInPlaceInitialization(SGF, E);
  assert(!isa<LValueType>(E->getType()->getCanonicalType()) &&
         "Shouldn't see an lvalue type here");

  ManagedValue mv =
      SGF.emitRValue(subExpr, SGFContext(SGFContext::AllowImmediatePlusZero))
          .getAsSingleValue(SGF, subExpr);
  assert(mv.getType().isAddress());
  if (toAddr->getType().isMoveOnlyWrapped())
    toAddr = SGF.B.createMoveOnlyWrapperToCopyableAddr(E, toAddr);
  SILValue fromAddr = mv.getValue();
  if (fromAddr->getType().isMoveOnlyWrapped())
    fromAddr = SGF.B.createMoveOnlyWrapperToCopyableAddr(E, fromAddr);
  SGF.B.createExplicitCopyAddr(subExpr, fromAddr, toAddr, IsNotTake,
                               IsInitialization);
  optTemp->finishInitialization(SGF);
  return RValue(SGF, {optTemp->getManagedAddress()}, subType.getASTType());
}

RValue RValueEmitter::visitMacroExpansionExpr(MacroExpansionExpr *E,
                                              SGFContext C) {
  if (auto *rewritten = E->getRewritten()) {
    return visit(rewritten, C);
  }
  else if (auto *MED = E->getSubstituteDecl()) {
    Mangle::ASTMangler mangler(MED->getASTContext());
    MED->forEachExpandedNode([&](ASTNode node) {
      if (auto *expr = node.dyn_cast<Expr *>())
        visit(expr, C);
      else if (auto *stmt = node.dyn_cast<Stmt *>())
        SGF.emitStmt(stmt);
      else
        SGF.visit(node.get<Decl *>());
    });
    return RValue();
  }
  return RValue();
}

RValue RValueEmitter::visitCurrentContextIsolationExpr(
    CurrentContextIsolationExpr *E, SGFContext C) {
  // If we are in an actor initializer that is isolated to self, the
  // current isolation is flow-sensitive; use that instead of the
  // synthesized expression.
  if (auto ctor = dyn_cast_or_null<ConstructorDecl>(
          SGF.F.getDeclRef().getDecl())) {
    auto isolation = getActorIsolation(ctor);
    if (ctor->isDesignatedInit() &&
        isolation == ActorIsolation::ActorInstance &&
        isolation.getActorInstance() == ctor->getImplicitSelfDecl()) {
      auto isolationValue =
        SGF.emitFlowSensitiveSelfIsolation(E, isolation);
      return RValue(SGF, E, isolationValue);
    }
  }

  return visit(E->getActor(), C);
}

RValue RValueEmitter::visitTypeValueExpr(TypeValueExpr *E, SGFContext C) {
  auto paramType = E->getParamType()->getCanonicalType();

  return RValue(SGF, E, ManagedValue::forObjectRValueWithoutOwnership(
    SGF.B.createTypeValue(E, SGF.getLoweredType(E->getType()), paramType)));
}

ManagedValue
SILGenFunction::emitExtractFunctionIsolation(SILLocation loc,
                                             ArgumentSource &&fnSource,
                                             SGFContext C) {
  std::optional<Scope> scope;

  // Emit the function value in its own scope unless we're going
  // to return it at +0.
  if (!C.isGuaranteedPlusZeroOk())
    scope.emplace(Cleanups, CleanupLocation(loc));

  // Emit a borrow of the function value.  Isolation extraction is a kind
  // of projection, so we can emit the function with the same context as
  // we got.
  auto fnLoc = fnSource.getLocation();
  auto fn = std::move(fnSource).getAsSingleValue(*this,
                                                 C.withFollowingProjection());
  fn = fn.borrow(*this, fnLoc);

  // Extract the isolation value.
  SILValue isolation = B.createFunctionExtractIsolation(loc, fn.getValue());

  // If we can return the isolation at +0, do so.
  if (C.isGuaranteedPlusZeroOk())
    return ManagedValue::forBorrowedObjectRValue(isolation);

  // Otherwise, copy it.
  isolation = B.createCopyValue(loc, isolation);

  // Manage the copy and exit the scope we entered earlier.
  auto isolationMV = emitManagedRValueWithCleanup(isolation);
  isolationMV = scope->popPreservingValue(isolationMV);
  return isolationMV;
}

RValue SILGenFunction::emitRValue(Expr *E, SGFContext C) {
  assert(!E->getType()->hasLValueType() &&
         "l-values must be emitted with emitLValue");
  return RValueEmitter(*this).visit(E, C);
}

RValue SILGenFunction::emitPlusOneRValue(Expr *E, SGFContext C) {
  Scope S(*this, SILLocation(E));
  assert(!E->getType()->hasLValueType() &&
         "l-values must be emitted with emitLValue");
  return S.popPreservingValue(
      RValueEmitter(*this).visit(E, C.withSubExprSideEffects()));
}

RValue SILGenFunction::emitPlusZeroRValue(Expr *E) {
  // Check if E is a case that we know how to emit at plus zero. If so, handle
  // it here.
  //
  // TODO: Fill this in.

  // Otherwise, we go through the +1 path and borrow the result.
  return emitPlusOneRValue(E).borrow(*this, SILLocation(E));
}

static void emitIgnoredPackExpansion(SILGenFunction &SGF,
                                     PackExpansionExpr *E) {
  auto expansionType =
    cast<PackExpansionType>(E->getType()->getCanonicalType());
  auto formalPackType = CanPackType::get(SGF.getASTContext(), expansionType);
  auto openedElementEnv = E->getGenericEnvironment();
  SGF.emitDynamicPackLoop(E, formalPackType, /*component index*/ 0,
                          openedElementEnv,
                          [&](SILValue indexWithinComponent,
                              SILValue packExpansionIndex,
                              SILValue packIndex) {
    SGF.emitIgnoredExpr(E->getPatternExpr());
  });
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

  // Pack expansions can come up in tuples, and potentially elsewhere
  // if we ever emit e.g. ignored call arguments with a builtin.
  if (auto *expansion = dyn_cast<PackExpansionExpr>(E)) {
    return emitIgnoredPackExpansion(*this, expansion);
  }
  
  // TODO: Could look through arbitrary implicit conversions that don't have
  // side effects, or through tuple shuffles, by emitting ignored default
  // arguments.
  
  FullExpr scope(Cleanups, CleanupLocation(E));
  if (E->getType()->hasLValueType()) {
    // Emit the l-value, but don't perform an access.
    FormalEvaluationScope scope(*this);
    emitLValue(E, SGFAccessKind::IgnoredRead);
    return;
  }

  // If this is a load expression, we try hard not to actually do the load
  // (which could materialize a potentially expensive value with cleanups).
  if (auto *LE = dyn_cast<LoadExpr>(E)) {
    FormalEvaluationScope scope(*this);
    LValue lv = emitLValue(LE->getSubExpr(), SGFAccessKind::IgnoredRead);

    // If loading from the lvalue is guaranteed to have no side effects, we
    // don't need to drill into it.
    if (lv.isLoadingPure())
      return;

    // If the last component is physical, then we just need to drill through
    // side effects in the lvalue, but don't need to perform the final load.
    if (lv.isLastComponentPhysical()) {
      emitAddressOfLValue(E, std::move(lv));
      return;
    }

    // Otherwise, we must call the ultimate getter to get its potential side
    // effect.
    emitLoadOfLValue(E, std::move(lv), SGFContext::AllowImmediatePlusZero);
    return;
  }

  auto findLoadThroughForceValueExprs = [](Expr *E,
                                           SmallVectorImpl<ForceValueExpr *>
                                             &forceValueExprs) -> LoadExpr * {
    while (auto FVE = dyn_cast<ForceValueExpr>(E)) {
      forceValueExprs.push_back(FVE);
      E = FVE->getSubExpr();
    }
    return dyn_cast<LoadExpr>(E);
  };

  // Look through force unwrap(s) of an lvalue. If possible, we want to just to
  // emit the precondition(s) without having to load the value.
  SmallVector<ForceValueExpr *, 4> forceValueExprs;
  if (auto *LE = findLoadThroughForceValueExprs(E, forceValueExprs)) {
    FormalEvaluationScope scope(*this);
    LValue lv = emitLValue(LE->getSubExpr(), SGFAccessKind::IgnoredRead);

    ManagedValue value;
    if (lv.isLastComponentPhysical()) {
      value = emitAddressOfLValue(LE, std::move(lv));
    } else {
      value = emitLoadOfLValue(LE, std::move(lv),
          SGFContext::AllowImmediatePlusZero).getAsSingleValue(*this, LE);
    }

    for (auto &FVE : llvm::reverse(forceValueExprs)) {
      const TypeLowering &optTL = getTypeLowering(FVE->getSubExpr()->getType());
      bool isImplicitUnwrap = FVE->isImplicit() &&
          FVE->isForceOfImplicitlyUnwrappedOptional();
      value = emitCheckedGetOptionalValueFrom(
          FVE, value, isImplicitUnwrap, optTL, SGFContext::AllowImmediatePlusZero);
    }
    return;
  }

  // Otherwise, emit the result (to get any side effects), but produce it at +0
  // if that allows simplification.
  emitRValue(E, SGFContext::AllowImmediatePlusZero);
}

/// Emit the given expression as an r-value, then (if it is a tuple), combine
/// it together into a single ManagedValue.
ManagedValue SILGenFunction::emitRValueAsSingleValue(Expr *E, SGFContext C) {
  return emitRValue(E, C).getAsSingleValue(*this, E);
}

RValue SILGenFunction::emitUndefRValue(SILLocation loc, Type type) {
  return RValue(*this, loc, type->getCanonicalType(),
                emitUndef(getLoweredType(type)));
}

ManagedValue SILGenFunction::emitUndef(Type type) {
  return emitUndef(getLoweredType(type));
}

ManagedValue SILGenFunction::emitUndef(SILType type) {
  SILValue undef = SILUndef::get(F, type);
  return ManagedValue::forRValueWithoutOwnership(undef);
}
