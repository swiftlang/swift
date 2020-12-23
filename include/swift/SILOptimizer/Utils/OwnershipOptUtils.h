//===--- OwnershipOptUtils.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Ownership Utilities that rely on SILOptimizer functionality.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_OWNERSHIPOPTUTILS_H
#define SWIFT_SILOPTIMIZER_UTILS_OWNERSHIPOPTUTILS_H

#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILModule.h"

namespace swift {

// Defined in BasicBlockUtils.h
struct JointPostDominanceSetComputer;

struct OwnershipFixupContext {
  std::function<void(SILInstruction *)> eraseNotify;
  std::function<void(SILInstruction *)> newInstNotify;
  DeadEndBlocks &deBlocks;
  JointPostDominanceSetComputer &jointPostDomSetComputer;

  SILBasicBlock::iterator
  replaceAllUsesAndEraseFixingOwnership(SingleValueInstruction *oldValue,
                                        SILValue newValue);

  /// We can not RAUW all old values with new values.
  ///
  /// Namely, we do not support RAUWing values with ValueOwnershipKind::None
  /// that have uses that do not require ValueOwnershipKind::None or
  /// ValueOwnershipKind::Any.
  static bool canFixUpOwnershipForRAUW(const SingleValueInstruction *oldValue,
                                       SILValue newValue);
};

} // namespace swift

#endif
