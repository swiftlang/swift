//===--- PhiStorageOptimizer.h - Phi storage optimizer --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This file defines PhiStorageOptimizer, a utility for use with the
/// mandatory AddressLowering pass.
///
//===----------------------------------------------------------------------===//

#include "AddressLowering.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class DominanceInfo;

class CoalescedPhi {
  friend class PhiStorageOptimizer;

  SmallVector<Operand *, 4> coalescedOperands;

  CoalescedPhi(const CoalescedPhi &) = delete;
  CoalescedPhi &operator=(const CoalescedPhi &) = delete;

public:
  CoalescedPhi() = default;
  CoalescedPhi(CoalescedPhi &&) = default;
  CoalescedPhi &operator=(CoalescedPhi &&) = default;

  void coalesce(PhiValue phi, const ValueStorageMap &valueStorageMap,
                DominanceInfo *domInfo);

  bool empty() const { return coalescedOperands.empty(); }

  ArrayRef<Operand *> getCoalescedOperands() const { return coalescedOperands; }

  SILInstruction::OperandValueRange getCoalescedValues() const {
    return SILInstruction::getOperandValues(getCoalescedOperands());
  }
};

} // namespace swift
