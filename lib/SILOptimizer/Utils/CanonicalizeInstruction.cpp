//===--- CanonicalizeInstruction.cpp - canonical SIL peepholes ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// SSA-peephole transformations that yield a more canonical SIL representation.
///
/// A superset of simplifyInstruction.
///
//===----------------------------------------------------------------------===//

// CanonicalizeInstruction defines a default DEBUG_TYPE: "sil-canonicalize"

#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// STATISTIC uses the default DEBUG_TYPE.
#define DEBUG_TYPE CanonicalizeInstruction::defaultDebugType
STATISTIC(NumSimplified, "Number of instructions simplified");

// Tracing within the implementation can also be activiated by the pass.
#undef DEBUG_TYPE
#define DEBUG_TYPE pass.debugType

// Vtable anchor.
CanonicalizeInstruction::~CanonicalizeInstruction() {}

// If simplification is successful, return a valid iterator to the next
// intruction that wasn't erased.
static Optional<SILBasicBlock::iterator>
simplifyAndReplace(SILInstruction *inst, CanonicalizeInstruction &pass) {
  // FIXME: temporarily bypass simplification untill all simplifications
  // preserve ownership SIL.
  if (inst->getFunction()->hasOwnership())
    return None;

  SILValue result = simplifyInstruction(inst);
  if (!result)
    return None;

  ++NumSimplified;

  LLVM_DEBUG(llvm::dbgs() << "Simplify Old = " << *inst
                          << "    New = " << *result << '\n');

  // Erase the simplified instruction and any instructions that end its
  // scope. Nothing needs to be added to the worklist except for Result,
  // because the instruction and all non-replaced users will be deleted.
  auto nextII = replaceAllSimplifiedUsesAndErase(
      inst, result,
      [&pass](SILInstruction *deleted) { pass.killInstruction(deleted); });

  // Push the new instruction and any users onto the worklist.
  pass.newUsers(result);
  return nextII;
}

SILBasicBlock::iterator
CanonicalizeInstruction::canonicalize(SILInstruction *inst) {
  if (auto nextII = simplifyAndReplace(inst, *this))
    return nextII.getValue();

  // Skip ahead.
  return std::next(inst->getIterator());
}
