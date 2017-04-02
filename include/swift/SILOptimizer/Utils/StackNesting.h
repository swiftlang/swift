//===--- StackNesting.h - Utility for stack nesting -------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_STACKNESTING_H
#define SWIFT_SILOPTIMIZER_UTILS_STACKNESTING_H

#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/SmallBitVector.h"

#include <vector>

namespace swift {

/// A utility to correct the nesting of stack allocating/deallocating
/// instructions.
///
/// This utility is useful for optimizations which create stack allocations
/// without caring about correct nesting with existing allocations. This may
/// result in code like
/// \code
///   %1 = alloc_stack
///     ...
///   %2 = alloc_stack
///     ...
///   dealloc_stack %1
///     ...
///   dealloc_stack %2
/// \endcode
///
/// The StackNesting utility is able to correct the code:
/// \code
///   %1 = alloc_stack
///     ...
///   %2 = alloc_stack
///     ...
///   dealloc_stack %2
///   dealloc_stack %1
/// \endcode
///
class StackNesting {

  typedef llvm::SmallBitVector BitVector;

  /// Data stored for each block (actually for each block which is not dead).
  struct BlockInfo {

    /// Back-link to the block.
    SILBasicBlock *Block;

    /// The cached list of successors.
    llvm::SmallVector<BlockInfo *, 8> Successors;

    /// The list of stack allocating/deallocating instructions in the block.
    llvm::SmallVector<SILInstruction *, 8> StackInsts;

    /// The bit-set of alive stack locations at the block entry.
    BitVector AliveStackLocsAtEntry;

    /// True if there is a path from this block to a function-exit.
    ///
    /// In other words: this block does not end in an unreachable-instruction.
    /// This flag is only used for verifying that the lifetime of a stack
    /// location does not end at the end of a block.
    bool ExitReachable = false;

    BlockInfo(SILBasicBlock *Block) : Block(Block) { }
  };

  /// Data stored for each stack location (= allocation).
  ///
  /// Each stack location is allocated by a single allocation instruction.
  struct StackLoc {
    StackLoc(SILInstruction *Alloc) : Alloc(Alloc) { }

    /// Back-link to the allocation instruction.
    SILInstruction *Alloc;

    /// Bit-set which represents all alive locations at this allocation.
    /// It obviously includes this location itself. And it includes all "outer"
    /// locations which surround this location.
    BitVector AliveLocs;
  };

  /// Mapping from stack allocations (= locations) to bit numbers.
  llvm::DenseMap<SILInstruction *, unsigned> StackLoc2BitNumbers;

  /// The list of stack locations. The index into this array is also the bit
  /// number in the bit-sets.
  llvm::SmallVector<StackLoc, 8> StackLocs;

  /// Block data for all (non-dead) blocks.
  std::vector<BlockInfo> BlockInfos;

public:

  /// The possible return values of correctStackNesting().
  enum class Changes {
    /// No changes are made.
    None,

    /// Only instructions were inserted or deleted.
    Instructions,

    /// Instructions were inserted or deleted and new blocks were inserted.
    CFG
  };

  StackNesting() { }

  /// Performs correction of stack nesting by moving stack-deallocation
  /// instructions down the control flow.
  ///
  /// Returns the status of what changes were made.
  Changes correctStackNesting(SILFunction *F);
  
  /// For debug dumping.
  void dump() const;

  static void dumpBits(const BitVector &Bits);

private:
  /// Initializes the data structures.
  void setup(SILFunction *F);

  /// Solves the dataflow problem.
  ///
  /// Returns true if there is a nesting of locations in any way, which can
  /// potentially in the wrong order.
  bool solve();

  /// Insert deallocation instructions for all locations which are alive before
  /// the InsertionPoint (AliveBefore) but not alive after the InsertionPoint
  /// (AliveAfter).
  ///
  /// Returns true if any deallocations were inserted.
  bool insertDeallocs(const BitVector &AliveBefore, const BitVector &AliveAfter,
                      SILInstruction *InsertionPoint);

  /// Modifies the SIL to end up with a correct stack nesting.
  ///
  /// Returns the status of what changes were made.
  Changes adaptDeallocs();
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_STACKNESTING_H
