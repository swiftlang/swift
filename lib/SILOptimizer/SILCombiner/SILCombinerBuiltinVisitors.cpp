//===--- SILCombinerBuiltinVisitors.cpp -----------------------------------===//
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

#define DEBUG_TYPE "sil-combine"
#include "SILCombiner.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/CFG.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"

using namespace swift;
using namespace swift::PatternMatch;

SILInstruction *SILCombiner::optimizeBuiltinCompareEq(BuiltinInst *BI,
                                                      bool NegateResult) {
  // Canonicalize boolean comparisons.
  if (auto OpTy = BI->getArguments()[0]->getType().getAs<BuiltinIntegerType>())
    if (OpTy->isFixedWidth(1))
      // cmp_eq %X, -1 -> xor (cmp_eq %X, 0), -1
      if (!NegateResult) {
        if (auto *ILOp = dyn_cast<IntegerLiteralInst>(BI->getArguments()[1]))
          if (ILOp->getValue().isAllOnesValue()) {
            auto X = BI->getArguments()[0];
            SILValue One(ILOp);
            SILValue Zero(
                Builder.createIntegerLiteral(BI->getLoc(), BI->getType(), 0));
            SILValue Inverted(Builder.createBuiltin(
                BI->getLoc(), BI->getName(), BI->getType(), {}, {X, Zero}));
            auto *Xor = Builder.createBuiltinBinaryFunction(
                BI->getLoc(), "xor", BI->getType(), BI->getType(),
                {Inverted, One});
            replaceInstUsesWith(*BI, Xor);
            return eraseInstFromFunction(*BI);
          }
      }
  IsZeroKind LHS = isZeroValue(BI->getArguments()[0]);
  IsZeroKind RHS = isZeroValue(BI->getArguments()[1]);

  // Can't handle unknown values.
  if (LHS == IsZeroKind::Unknown || RHS == IsZeroKind::Unknown)
    return nullptr;

  // Can't handle non-zero ptr values.
  if (LHS == IsZeroKind::NotZero && RHS == IsZeroKind::NotZero)
    return nullptr;

  // Set to true if both sides are zero. Set to false if only one side is zero.
  bool Val = (LHS == RHS) ^ NegateResult;

  return Builder.createIntegerLiteral(BI->getLoc(), BI->getType(),
                                      APInt(1, Val));
}

SILInstruction *SILCombiner::optimizeBuiltinCanBeObjCClass(BuiltinInst *BI) {
  assert(BI->hasSubstitutions() && "Expected substitutions for canBeClass");

  auto const &Subs = BI->getSubstitutions();
  assert((Subs.size() == 1) &&
         "Expected one substitution in call to canBeClass");

  auto Ty = Subs[0].getReplacement()->getCanonicalType();
  switch (Ty->canBeClass()) {
  case TypeTraitResult::IsNot:
    return Builder.createIntegerLiteral(BI->getLoc(), BI->getType(),
                                        APInt(8, 0));
  case TypeTraitResult::Is:
    return Builder.createIntegerLiteral(BI->getLoc(), BI->getType(),
                                        APInt(8, 1));
  case TypeTraitResult::CanBe:
    return nullptr;
  }

  llvm_unreachable("Unhandled TypeTraitResult in switch.");
}

static unsigned getTypeWidth(SILType Ty) {
  if (auto BuiltinIntTy =
          dyn_cast<BuiltinIntegerType>(Ty.getSwiftRValueType())) {
    if (BuiltinIntTy->isFixedWidth()) {
      return BuiltinIntTy->getFixedWidth();
    }
  }
  return 0;
}

SILInstruction *SILCombiner::optimizeBuiltinTruncOrBitCast(BuiltinInst *I) {
  assert(I->getBuiltinInfo().ID == BuiltinValueKind::TruncOrBitCast &&
         "BuiltinInst is not Trunc");
  SILValue Op = I->getArguments()[0];
  SILValue Source;
  if (match(Op, m_ZExtOrBitCast(m_SILValue(Source)))) {
    SILType ResultType = I->getType();
    SILType SourceType = Source->getType();
    SILType SourceTargetType = Op->getType();
    unsigned ResultTypeWidth = getTypeWidth(ResultType);
    unsigned SourceTypeWidth = getTypeWidth(SourceType);
    unsigned SourceTargetTypeWidth = getTypeWidth(SourceTargetType);
    if (ResultTypeWidth == 0 || SourceTypeWidth == 0 ||
        SourceTargetTypeWidth == 0) {
      // Not all types involved have fixed width
      return nullptr;
    }
    if (SourceTargetTypeWidth <= SourceTypeWidth) {
      return nullptr;
    }
    if (ResultTypeWidth < SourceTypeWidth) {
      return nullptr;
    }
    // Reach here if and only if:
    // SourceTargetTypeWidth > SourceTypeWidth and ResultTypeWidth >=
    // SourceTypeWidth
    auto *NI = Builder.createBuiltinBinaryFunctionWithTwoOpTypes(
        I->getLoc(), "zextOrBitCast", Source->getType(), ResultType, ResultType,
        Source);
    replaceInstUsesWith(*I, NI);
    return eraseInstFromFunction(*I);
  }
  return nullptr;
}

SILInstruction *SILCombiner::optimizeBuiltinZextOrBitCast(BuiltinInst *I) {
  assert(I->getBuiltinInfo().ID == BuiltinValueKind::ZExtOrBitCast &&
         "BuiltinInst is not ZExt");
  SILValue Op = I->getArguments()[0];
  SILValue Source;
  if (match(Op, m_ZExtOrBitCast(m_SILValue(Source)))) {
    SILType ResultType = I->getType();
    // We can't extend to a size *smaller* than the source.
    // As such, this transformation will always maintain correctness
    auto *NI = Builder.createBuiltinBinaryFunctionWithTwoOpTypes(
        I->getLoc(), "zextOrBitCast", Source->getType(), ResultType, ResultType,
        Source);
    replaceInstUsesWith(*I, NI);
    return eraseInstFromFunction(*I);
  }
  return nullptr;
}

/// Optimize builtins which receive the same value in their first and second
/// operand.
static SILInstruction *optimizeBuiltinWithSameOperands(SILBuilder &Builder,
                                                       BuiltinInst *I,
                                                       SILCombiner *C) {
  // Handle all builtins which can be optimized.
  // We have to take special care about floating point operations because of
  // potential NaN values. E.g. ordered equal FCMP_OEQ(Nan, Nan) is not true.
  switch (I->getBuiltinInfo().ID) {
      
  // Replace the uses with one of the (identical) operands.
  case BuiltinValueKind::And:
  case BuiltinValueKind::Or: {
    // We cannot just _return_ the operand because it is not necessarily an
    // instruction. It can be an argument.
    SILValue Op = I->getOperand(0);
    C->replaceInstUsesWith(*I, Op);
    break;
  }

  // Return 0 or false.
  case BuiltinValueKind::Sub:
  case BuiltinValueKind::SRem:
  case BuiltinValueKind::URem:
  case BuiltinValueKind::Xor:
  case BuiltinValueKind::ICMP_NE:
  case BuiltinValueKind::ICMP_SLT:
  case BuiltinValueKind::ICMP_SGT:
  case BuiltinValueKind::ICMP_ULT:
  case BuiltinValueKind::ICMP_UGT:
  case BuiltinValueKind::FCMP_ONE:
    if (auto Ty = I->getType().getAs<BuiltinIntegerType>()) {
      // FIXME: Should also use SILBuilderWithScope?
      return Builder.createIntegerLiteral(I->getLoc(), I->getType(),
                                          APInt(Ty->getGreatestWidth(), 0));
    }
    break;
      
  // Return 1 or true.
  case BuiltinValueKind::ICMP_EQ:
  case BuiltinValueKind::ICMP_SLE:
  case BuiltinValueKind::ICMP_SGE:
  case BuiltinValueKind::ICMP_ULE:
  case BuiltinValueKind::ICMP_UGE:
  case BuiltinValueKind::FCMP_UEQ:
  case BuiltinValueKind::FCMP_UGE:
  case BuiltinValueKind::FCMP_ULE:
  case BuiltinValueKind::SDiv:
  case BuiltinValueKind::UDiv:
    if (auto Ty = I->getType().getAs<BuiltinIntegerType>()) {
      // FIXME: Should also use SILBuilderWithScope?
      return Builder.createIntegerLiteral(I->getLoc(), I->getType(),
                                          APInt(Ty->getGreatestWidth(), 1));
    }
    break;

  // Return 0 in a tuple.
  case BuiltinValueKind::SSubOver:
  case BuiltinValueKind::USubOver: {
    SILType Ty = I->getType();
    SILType IntTy = Ty.getTupleElementType(0);
    SILType BoolTy = Ty.getTupleElementType(1);
    SILBuilderWithScope B(I);
    SILValue Elements[] = {
      B.createIntegerLiteral(I->getLoc(), IntTy, /* Result */ 0),
      B.createIntegerLiteral(I->getLoc(), BoolTy, /* Overflow */ 0)
    };
    return B.createTuple(I->getLoc(), Ty, Elements);
  }
      
  default:
    break;
  }
  return nullptr;
}

/// Match an index pointer that is fed by a sizeof(T)*Distance offset.
static IndexRawPointerInst *
matchSizeOfMultiplication(SILValue I, MetatypeInst *RequiredType,
                          BuiltinInst *&TruncOrBitCast, SILValue &Ptr,
                          SILValue &Distance) {
  IndexRawPointerInst *Res = dyn_cast<IndexRawPointerInst>(I);
  if (!Res)
    return nullptr;

  SILValue Dist;
  MetatypeInst *StrideType;
  if (match(
          Res->getOperand(1),
          m_ApplyInst(
              BuiltinValueKind::TruncOrBitCast,
              m_TupleExtractInst(
                  m_ApplyInst(
                      BuiltinValueKind::SMulOver, m_SILValue(Dist),
                      m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                                  m_ApplyInst(BuiltinValueKind::Strideof,
                                              m_MetatypeInst(StrideType)))),
                  0))) ||
      match(
          Res->getOperand(1),
          m_ApplyInst(
              BuiltinValueKind::TruncOrBitCast,
              m_TupleExtractInst(
                  m_ApplyInst(
                      BuiltinValueKind::SMulOver,
                      m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                                  m_ApplyInst(BuiltinValueKind::Strideof,
                                              m_MetatypeInst(StrideType))),
                      m_SILValue(Dist)),
                  0)))) {
    if (StrideType != RequiredType)
      return nullptr;
    TruncOrBitCast = cast<BuiltinInst>(Res->getOperand(1));
    Distance = Dist;
    Ptr = Res->getOperand(0);
    return Res;
  }
  return nullptr;
}

/// Given an index_raw_pointer Ptr, size_of(Metatype) * Distance create an
/// address_to_pointer (index_addr ptr, Distance : $*Metatype) : $RawPointer
/// instruction.
static SILInstruction *createIndexAddrFrom(IndexRawPointerInst *I,
                                           MetatypeInst *Metatype,
                                           BuiltinInst *TruncOrBitCast,
                                           SILValue Ptr, SILValue Distance,
                                           SILType RawPointerTy,
                                           SILBuilder &Builder) {
  Builder.setCurrentDebugScope(I->getDebugScope());
  SILType InstanceType =
    Metatype->getType().getMetatypeInstanceType(I->getModule());

  // index_raw_pointer's address type is currently always strict.
  auto *NewPTAI = Builder.createPointerToAddress(
    I->getLoc(), Ptr, InstanceType.getAddressType(),
    /*isStrict*/ true, /*isInvariant*/ false);

  auto *DistanceAsWord =
      Builder.createBuiltin(I->getLoc(), TruncOrBitCast->getName(),
                             TruncOrBitCast->getType(), {}, Distance);

  auto *NewIAI = Builder.createIndexAddr(I->getLoc(), NewPTAI, DistanceAsWord);
  auto *NewATPI =
    Builder.createAddressToPointer(I->getLoc(), NewIAI, RawPointerTy);
  return NewATPI;
}

/// Optimize an array operation that has (index_raw_pointer b, sizeof(T) * Dist)
/// operands into one that use index_addr as operands.
SILInstruction *optimizeBuiltinArrayOperation(BuiltinInst *I,
                                              SILBuilder &Builder) {
  if (I->getNumOperands() < 3)
    return nullptr;

  // Take something like this:
  //   %stride = Builtin.strideof(T) * %distance
  //   %ptr' = index_raw_pointer %ptr, %stride
  //     = builtin "takeArrayFrontToBack"<Int>(%metatype, %ptr', ...

  // And convert it to this:
  //   %addr = pointer_to_address %ptr, strict $*T
  //   %result = index_addr %addr, %distance
  //   %ptr' = address_to_pointer result : $RawPointer
  //     = builtin "takeArrayFrontToBack"<Int>(%metatype, %ptr', ...

  auto *Metatype = dyn_cast<MetatypeInst>(I->getOperand(0));
  if (!Metatype)
    return nullptr;

  SILValue Ptr;
  SILValue Distance;
  BuiltinInst *TruncOrBitCast;
  SILValue NewOp1 = I->getOperand(1), NewOp2 = I->getOperand(2);

  // Try to replace the first pointer operand.
  auto *IdxRawPtr1 = matchSizeOfMultiplication(I->getOperand(1), Metatype,
                                               TruncOrBitCast, Ptr, Distance);
  if (IdxRawPtr1)
    NewOp1 = createIndexAddrFrom(IdxRawPtr1, Metatype, TruncOrBitCast, Ptr,
                                 Distance, NewOp1->getType(), Builder);

  // Try to replace the second pointer operand.
  auto *IdxRawPtr2 = matchSizeOfMultiplication(I->getOperand(2), Metatype,
                                               TruncOrBitCast, Ptr, Distance);
  if (IdxRawPtr2)
    NewOp2 = createIndexAddrFrom(IdxRawPtr2, Metatype, TruncOrBitCast, Ptr,
                                 Distance, NewOp2->getType(), Builder);

  if (NewOp1 != I->getOperand(1) || NewOp2 != I->getOperand(2)) {
    SmallVector<SILValue, 5> NewOpds;
    for (auto OldOpd : I->getArguments())
      NewOpds.push_back(OldOpd);
    NewOpds[1] = NewOp1;
    NewOpds[2] = NewOp2;
    return Builder.createBuiltin(I->getLoc(), I->getName(), I->getType(),
                                 I->getSubstitutions(), NewOpds);
  }
  return nullptr;
}

/// Get operands of a binary bitop builtin where one operand is an integer
/// literal.
static bool getBitOpArgs(BuiltinInst *BI, SILValue &op, APInt &bits) {
  OperandValueArrayRef Args = BI->getArguments();
  if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[0])) {
    op = Args[1];
    bits = IL->getValue();
    return true;
  }
  if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[1])) {
    op = Args[0];
    bits = IL->getValue();
    return true;
  }
  return false;
}

/// Optimizes binary bit operations. Optimizations for "and":
///   x & 0 -> 0
///   x & ~0 -> x
///   (x & c1) & c2 -> x & (c1 & c2)
/// The same optimizations are done for "or" and "xor".
template <typename CombineFunc, typename NeutralFunc, typename ZeroFunc>
SILInstruction *optimizeBitOp(BuiltinInst *BI,
                              CombineFunc combine,
                              NeutralFunc isNeutral,
                              ZeroFunc isZero,
                              SILBuilder &Builder,
                              SILCombiner *C) {
  SILValue firstOp;
  APInt bits;
  if (!getBitOpArgs(BI, firstOp, bits))
    return nullptr;

  // Combine all bits of consecutive bit operations, e.g. ((op & c1) & c2) & c3
  SILValue op = firstOp;
  BuiltinInst *Prev;
  APInt prevBits;
  while ((Prev = dyn_cast<BuiltinInst>(op)) &&
         Prev->getBuiltinInfo().ID == BI->getBuiltinInfo().ID &&
         getBitOpArgs(Prev, op, prevBits)) {
    combine(bits, prevBits);
  }
  if (isNeutral(bits))
    // The bit operation has no effect, e.g. x | 0 -> x
    return C->replaceInstUsesWith(*BI, op);

  if (isZero(bits))
    // The bit operation yields to a constant, e.g. x & 0 -> 0
    return Builder.createIntegerLiteral(BI->getLoc(), BI->getType(), bits);
  
  if (op != firstOp) {
    // We combined multiple bit operations to a single one,
    // e.g. (x & c1) & c2 -> x & (c1 & c2)
    auto *newLI = Builder.createIntegerLiteral(BI->getLoc(), BI->getType(),
                                                bits);
    return Builder.createBuiltin(BI->getLoc(), BI->getName(), BI->getType(),
                                 BI->getSubstitutions(),
                                 { op, newLI });
  }
  return nullptr;
}

SILInstruction *SILCombiner::visitBuiltinInst(BuiltinInst *I) {
  if (I->getBuiltinInfo().ID == BuiltinValueKind::CanBeObjCClass)
    return optimizeBuiltinCanBeObjCClass(I);
  if (I->getBuiltinInfo().ID == BuiltinValueKind::TakeArrayFrontToBack ||
      I->getBuiltinInfo().ID == BuiltinValueKind::TakeArrayBackToFront ||
      I->getBuiltinInfo().ID == BuiltinValueKind::CopyArray)
    return optimizeBuiltinArrayOperation(I, Builder);

  if (I->getBuiltinInfo().ID == BuiltinValueKind::TruncOrBitCast) {
    return optimizeBuiltinTruncOrBitCast(I);
  }
  if (I->getBuiltinInfo().ID == BuiltinValueKind::ZExtOrBitCast) {
    return optimizeBuiltinZextOrBitCast(I);
  }

  if (I->getNumOperands() >= 2 && I->getOperand(0) == I->getOperand(1)) {
    // It's a builtin which has the same value in its first and second operand.
    auto *Replacement = optimizeBuiltinWithSameOperands(Builder, I, this);
    if (Replacement)
      return Replacement;
  }

  // Optimize this case for unsigned and equality comparisons:
  //   cmp_*_T . (zext U->T x, zext U->T y)
  //      => cmp_*_T (x, y)
  switch (I->getBuiltinInfo().ID) {
  case BuiltinValueKind::ICMP_ULT: {
    if (auto *ILOp = dyn_cast<IntegerLiteralInst>(I->getArguments()[0])) {
      if (ILOp->getValue().isMaxValue()) {
        auto *Zero = Builder.createIntegerLiteral(I->getLoc(), I->getType(), 0);
        replaceInstUsesWith(*I, Zero);
        return eraseInstFromFunction(*I);
      }
    }
  }
    LLVM_FALLTHROUGH;
  case BuiltinValueKind::ICMP_ULE:
  case BuiltinValueKind::ICMP_UGE:
  case BuiltinValueKind::ICMP_UGT:
  case BuiltinValueKind::ICMP_EQ:
  case BuiltinValueKind::ICMP_NE: {
    SILValue LCast, RCast;
    if (match(I->getArguments()[0],
              m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                          m_SILValue(LCast))) &&
        match(I->getArguments()[1],
              m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                          m_SILValue(RCast))) &&
        LCast->getType() == RCast->getType()) {

      auto *NewCmp = Builder.createBuiltinBinaryFunction(
          I->getLoc(), getBuiltinName(I->getBuiltinInfo().ID),
          LCast->getType(), I->getType(), {LCast, RCast});

      I->replaceAllUsesWith(NewCmp);
      replaceInstUsesWith(*I, NewCmp);
      return eraseInstFromFunction(*I);
    }
    break;
  }
  case BuiltinValueKind::And:
    return optimizeBitOp(I,
      [](APInt &left, const APInt &right) { left &= right; }    /* combine */,
      [](const APInt &i) -> bool { return i.isAllOnesValue(); } /* isNeutral */,
      [](const APInt &i) -> bool { return i.isMinValue(); }     /* isZero */,
      Builder, this);
  case BuiltinValueKind::Or:
    return optimizeBitOp(I,
      [](APInt &left, const APInt &right) { left |= right; }    /* combine */,
      [](const APInt &i) -> bool { return i.isMinValue(); }     /* isNeutral */,
      [](const APInt &i) -> bool { return i.isAllOnesValue(); } /* isZero */,
      Builder, this);
  case BuiltinValueKind::Xor:
    return optimizeBitOp(I,
      [](APInt &left, const APInt &right) { left ^= right; } /* combine */,
      [](const APInt &i) -> bool { return i.isMinValue(); }  /* isNeutral */,
      [](const APInt &i) -> bool { return false; }           /* isZero */,
      Builder, this);
  case BuiltinValueKind::DestroyArray: {
    SubstitutionList Substs = I->getSubstitutions();
    // Check if the element type is a trivial type.
    if (Substs.size() == 1) {
      Substitution Subst = Substs[0];
      Type ElemType = Subst.getReplacement();
      auto &SILElemTy = I->getModule().Types.getTypeLowering(ElemType);
      // Destroying an array of trivial types is a no-op.
      if (SILElemTy.isTrivial())
        return eraseInstFromFunction(*I);
    }
    break;
  }
  default:
    break;
  }
  
  if (I->getBuiltinInfo().ID == BuiltinValueKind::ICMP_EQ)
    return optimizeBuiltinCompareEq(I, /*Negate Eq result*/ false);

  if (I->getBuiltinInfo().ID == BuiltinValueKind::ICMP_NE)
    return optimizeBuiltinCompareEq(I, /*Negate Eq result*/ true);

  // Optimize sub(ptrtoint(index_raw_pointer(v, x)), ptrtoint(v)) -> x.
  BuiltinInst *Bytes2;
  IndexRawPointerInst *Indexraw;
  if (I->getNumOperands() == 2 &&
      match(I, m_BuiltinInst(BuiltinValueKind::Sub,
                             m_BuiltinInst(BuiltinValueKind::PtrToInt,
                                           m_IndexRawPointerInst(Indexraw)),
                             m_BuiltinInst(Bytes2)))) {
    if (match(Bytes2,
              m_BuiltinInst(BuiltinValueKind::PtrToInt, m_ValueBase()))) {
      if (Indexraw->getOperand(0) == Bytes2->getOperand(0) &&
          Indexraw->getOperand(1)->getType() == I->getType()) {
        replaceInstUsesWith(*I, Indexraw->getOperand(1));
        return eraseInstFromFunction(*I);
      }
    }
  }

  // Canonicalize multiplication by a stride to be such that the stride is
  // always the second argument.
  if (I->getNumOperands() != 3)
    return nullptr;

  if (match(I, m_ApplyInst(BuiltinValueKind::SMulOver,
                            m_ApplyInst(BuiltinValueKind::Strideof),
                            m_ValueBase(), m_IntegerLiteralInst()))) {
    I->swapOperands(0, 1);
    return I;
  }

  return nullptr;
}
