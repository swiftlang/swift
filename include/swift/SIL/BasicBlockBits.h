//===--- BasicBlockBits.h - SILBasicBlock bit utilities ---------*- C++ -*-===//
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
//
// This file defines utilities for BasicBlock bit fields and sets.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBITFIELD_H
#define SWIFT_SIL_SILBITFIELD_H

#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

/// Utility to add a custom bitfield to a function's basic blocks.
///
/// This can be used by transforms to store temporary flags or tiny values per
/// basic block.
/// The memory managed is a 32 bit field within each basic block (\see
/// BasicBlock::customBits) and thus is very efficient: no memory allocation is
/// needed, no hash set or map is needed for lookup and there is no
/// initialization cost (in contrast to BasicBlockData which needs to iterate
/// over all blocks at initialization).
///
/// Invariants:
/// * BasicBlockBitfield instances must be allocated and deallocated
///   following a strict stack discipline, because bit-positions in
///   BasicBlock::customBits are "allocated" and "freed" with a stack-allocation
///   algorithm. This means, it's fine to use a BasicBlockBitfield as (or in)
///   local variables, e.g. in transformations. But it's not possible to store
///   a BasicBlockBitfield in an Analysis.
/// * The total number of bits which are alive at the same time must not exceed
///   32 (the size of BasicBlock::customBits).
class BasicBlockBitfield {
  /// The bitfield is "added" to the blocks of this function.
  SILFunction *function;

  /// A single linked list of currently alive BasicBlockBitfields (oldest is
  /// last, newest is first).
  /// The head of the list is function->lastAllocatedBitfield.
  BasicBlockBitfield *parent;
  
  /// Initialized with the monotonically increasing currentBitfieldID of the
  /// function.
  /// Used to check if the bitfield in a block is initialized.
  /// If a block's lastInitializedBitfieldID is less than this ID, it means
  /// that the bits of that block are not initialized yet.
  /// See also: SILBasicBlock::lastInitializedBitfieldID,
  ///           SILFunction::currentBitfieldID
  unsigned bitfieldID;

  short startBit;
  short endBit;
  uint32_t mask;

public:
  BasicBlockBitfield(SILFunction *function, int size) :
      function(function),
      parent(function->newestAliveBitfield),
      bitfieldID(function->currentBitfieldID),
      startBit(parent ? parent->endBit : 0),
      endBit(startBit + size),
      mask(0xffffffffu >> (32 - size) << startBit) {
    assert(size > 0 && "bit field size must be > 0");
    assert(endBit <= 32 && "too many/large bit fields allocated in function");
    assert((!parent || bitfieldID > parent->bitfieldID) &&
           "BasicBlockBitfield indices are not in order");
    function->newestAliveBitfield = this;
    ++function->currentBitfieldID;
    assert(function->currentBitfieldID != 0 && "currentBitfieldID overflow");
  }

  ~BasicBlockBitfield() {
    assert(function->newestAliveBitfield == this &&
           "BasicBlockBitfield destructed too early");
    function->newestAliveBitfield = parent;
  }

  BasicBlockBitfield(const BasicBlockBitfield &) = delete;
  BasicBlockBitfield(BasicBlockBitfield &&) = delete;
  BasicBlockBitfield &operator=(const BasicBlockBitfield &) = delete;
  BasicBlockBitfield &operator=(BasicBlockBitfield &&) = delete;

  SILFunction *getFunction() const { return function; }

  unsigned get(SILBasicBlock *block) const {
    assert(block->getParent() == function);
    if (bitfieldID > block->lastInitializedBitfieldID) {
      // The bitfield is not initialized yet in this block.
      return 0;
    }
    return (block->customBits & mask) >> startBit;
  }

  void set(SILBasicBlock *block, unsigned value) {
    assert(block->getParent() == function);
    assert(((value << startBit) & ~mask) == 0 &&
           "value too large for BasicBlockBitfield");
    unsigned clearMask = mask;
    if (bitfieldID > block->lastInitializedBitfieldID) {

      // The bitfield is not initialized yet in this block.
      // Initialize the bitfield, and also initialize all parent bitfields,
      // which are not initialized, yet. Example:
      //
      //  This field   Last initialized field
      //    |            |
      //    V            V
      //   EE DDD C BB AAA
      //
      // block->lastInitializedBitfieldID == AAA.bitfieldID
      // -> we have to initialize the fields: BB, C, DDD and EE
      //
      BasicBlockBitfield *bf = parent;
      while (bf && bf->bitfieldID > block->lastInitializedBitfieldID) {
        clearMask |= bf->mask;
        bf = bf->parent;
      }
      block->lastInitializedBitfieldID = bitfieldID;
    }
    block->customBits = (block->customBits & ~clearMask) | (value << startBit);
  }
};

/// A BasicBlockBitfield containing a single bit - a flag.
class BasicBlockFlag {
  BasicBlockBitfield bit;

public:
  BasicBlockFlag(SILFunction *function) : bit(function, 1) {}

  SILFunction *getFunction() const { return bit.getFunction(); }
  
  bool get(SILBasicBlock *block) const { return (bool)bit.get(block); }

  void set(SILBasicBlock *block, bool value = true) {
    bit.set(block, (unsigned)value);
  }
  void reset(SILBasicBlock *block) { bit.set(block, 0); }

  /// Sets the flag and returns the old value.
  bool testAndSet(SILBasicBlock *block) {
    bool oldValue = get(block);
    set(block);
    return oldValue;
  }
};

/// A BasicBlockFlag with a set-like API.
class BasicBlockSet {
  BasicBlockFlag flag;

public:
  BasicBlockSet(SILFunction *function) : flag(function) {}

  SILFunction *getFunction() const { return flag.getFunction(); }

  bool contains(SILBasicBlock *block) const { return flag.get(block); }

  /// Returns true if \p block was not contained in the set before inserting.
  bool insert(SILBasicBlock *block) { return !flag.testAndSet(block); }

  void erase(SILBasicBlock *block) { flag.reset(block); }
};

/// An implementation of `llvm::SetVector<SILBasicBlock *,
///                                       SmallVector<SILBasicBlock *, N>,
///                                       BasicBlockSet>`.
///
/// Unfortunately it's not possible to use `llvm::SetVector` directly because
/// the BasicBlockSet constructor needs a `SILFunction` argument.
///
/// Note: This class does not provide a `remove` method intentinally, because
/// it would have a O(n) complexity.
template <unsigned N> class BasicBlockSetVector {
  using Vector = llvm::SmallVector<SILBasicBlock *, N>;

  Vector vector;
  BasicBlockSet set;
  
public:
  using iterator = typename Vector::const_iterator;

  BasicBlockSetVector(SILFunction *function) : set(function) {}

  iterator begin() const { return vector.begin(); }
  iterator end() const { return vector.end(); }

  unsigned size() const { return vector.size(); }
  bool empty() const { return vector.empty(); }

  bool contains(SILBasicBlock *block) const { return set.contains(block); }

  /// Returns true if \p block was not contained in the set before inserting.
  bool insert(SILBasicBlock *block) {
    if (set.insert(block)) {
      vector.push_back(block);
      return true;
    }
    return false;
  }
};

/// A utility for processing basic blocks in a worklist.
///
/// It is basically a combination of a block vector and a block set. It can be
/// used for typical worklist-processing algorithms.
template <unsigned N> class BasicBlockWorklist {
  llvm::SmallVector<SILBasicBlock *, N> worklist;
  BasicBlockSet visited;
  
public:
  /// Construct an empty worklist.
  BasicBlockWorklist(SILFunction *function) : visited(function) {}

  /// Initialize the worklist with \p initialBlock.
  BasicBlockWorklist(SILBasicBlock *initialBlock)
      : visited(initialBlock->getParent()) {
    push(initialBlock);
  }

  /// Pops the last added element from the worklist or returns null, if the
  /// worklist is empty.
  SILBasicBlock *pop() {
    if (worklist.empty())
      return nullptr;
    return worklist.pop_back_val();
  }

  /// Pushes \p block onto the worklist if \p block has never been push before.
  bool pushIfNotVisited(SILBasicBlock *block) {
    if (visited.insert(block)) {
      worklist.push_back(block);
      return true;
    }
    return false;
  }

  /// Like `pushIfNotVisited`, but requires that \p block has never been on the
  /// worklist before.
  void push(SILBasicBlock *block) {
    assert(!visited.contains(block));
    visited.insert(block);
    worklist.push_back(block);
  }

  /// Like `pop`, but marks the returned block as "unvisited". This means, that
  /// the block can be pushed onto the worklist again.
  SILBasicBlock *popAndForget() {
    if (worklist.empty())
      return nullptr;
    SILBasicBlock *block = worklist.pop_back_val();
    visited.erase(block);
    return block;
  }

  /// Returns true if \p block was visited, i.e. has been added to the worklist.
  bool isVisited(SILBasicBlock *block) const { return visited.contains(block); }
};

} // namespace swift

#endif
