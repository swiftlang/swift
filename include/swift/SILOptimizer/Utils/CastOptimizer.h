//===--- CastOptimizer.h --------------------------------------------------===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_CASTOPTIMIZER_H
#define SWIFT_SILOPTIMIZER_UTILS_CASTOPTIMIZER_H

#include "swift/Basic/ArrayRefView.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Allocator.h"
#include <functional>
#include <utility>

namespace swift {

class SILOptFunctionBuilder;

/// \brief This is a helper class used to optimize casts.
class CastOptimizer {
  SILOptFunctionBuilder &FunctionBuilder;

  // Callback to be called when uses of an instruction should be replaced.
  std::function<void(SingleValueInstruction *I, ValueBase *V)>
      ReplaceInstUsesAction;

  // Callback to call when an instruction needs to be erased.
  std::function<void(SILInstruction *)> EraseInstAction;

  // Callback to call after an optimization was performed based on the fact
  // that a cast will succeed.
  std::function<void()> WillSucceedAction;

  // Callback to call after an optimization was performed based on the fact
  // that a cast will fail.
  std::function<void()> WillFailAction;

  /// Optimize a cast from a bridged ObjC type into
  /// a corresponding Swift type implementing _ObjectiveCBridgeable.
  SILInstruction *optimizeBridgedObjCToSwiftCast(
      SILInstruction *Inst, bool isConditional, SILValue Src, SILValue Dest,
      CanType Source, CanType Target, Type BridgedSourceTy,
      Type BridgedTargetTy, SILBasicBlock *SuccessBB, SILBasicBlock *FailureBB);

  /// Optimize a cast from a Swift type implementing _ObjectiveCBridgeable
  /// into a bridged ObjC type.
  SILInstruction *optimizeBridgedSwiftToObjCCast(
      SILInstruction *Inst, CastConsumptionKind ConsumptionKind,
      bool isConditional, SILValue Src, SILValue Dest, CanType Source,
      CanType Target, Type BridgedSourceTy, Type BridgedTargetTy,
      SILBasicBlock *SuccessBB, SILBasicBlock *FailureBB);

  void deleteInstructionsAfterUnreachable(SILInstruction *UnreachableInst,
                                          SILInstruction *TrapInst);

public:
  CastOptimizer(SILOptFunctionBuilder &FunctionBuilder,
                std::function<void(SingleValueInstruction *I, ValueBase *V)>
                    ReplaceInstUsesAction,
                std::function<void(SILInstruction *)> EraseAction,
                std::function<void()> WillSucceedAction,
                std::function<void()> WillFailAction = []() {})
      : FunctionBuilder(FunctionBuilder), ReplaceInstUsesAction(ReplaceInstUsesAction),
        EraseInstAction(EraseAction), WillSucceedAction(WillSucceedAction),
        WillFailAction(WillFailAction) {}

  // This constructor is used in
  // 'SILOptimizer/Mandatory/ConstantPropagation.cpp'. MSVC2015 compiler
  // couldn't use the single constructor version which has three default
  // arguments. It seems the number of the default argument with lambda is
  // limited.
  CastOptimizer(SILOptFunctionBuilder &FunctionBuilder,
                std::function<void(SingleValueInstruction *I, ValueBase *V)>
                    ReplaceInstUsesAction,
                std::function<void(SILInstruction *)> EraseAction =
                    [](SILInstruction *) {})
      : CastOptimizer(FunctionBuilder, ReplaceInstUsesAction, EraseAction, []() {}, []() {}) {}

  /// Simplify checked_cast_br. It may change the control flow.
  SILInstruction *simplifyCheckedCastBranchInst(CheckedCastBranchInst *Inst);

  /// Simplify checked_cast_value_br. It may change the control flow.
  SILInstruction *
  simplifyCheckedCastValueBranchInst(CheckedCastValueBranchInst *Inst);

  /// Simplify checked_cast_addr_br. It may change the control flow.
  SILInstruction *
  simplifyCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *Inst);

  /// Optimize checked_cast_br. This cannot change the control flow.
  SILInstruction *optimizeCheckedCastBranchInst(CheckedCastBranchInst *Inst);

  /// Optimize checked_cast_value_br. This cannot change the control flow.
  SILInstruction *
  optimizeCheckedCastValueBranchInst(CheckedCastValueBranchInst *Inst);

  /// Optimize checked_cast_addr_br. This cannot change the control flow.
  SILInstruction *
  optimizeCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *Inst);

  /// Optimize unconditional_checked_cast. This cannot change the control flow.
  ValueBase *
  optimizeUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *Inst);

  /// Optimize unconditional_checked_cast_addr. This cannot change the control
  /// flow.
  SILInstruction *optimizeUnconditionalCheckedCastAddrInst(
      UnconditionalCheckedCastAddrInst *Inst);

  /// Check if it is a bridged cast and optimize it.
  /// May change the control flow.
  SILInstruction *optimizeBridgedCasts(SILInstruction *Inst,
                                       CastConsumptionKind ConsumptionKind,
                                       bool isConditional, SILValue Src,
                                       SILValue Dest, CanType Source,
                                       CanType Target, SILBasicBlock *SuccessBB,
                                       SILBasicBlock *FailureBB);
};

} // namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_CASTOPTIMIZER_H
