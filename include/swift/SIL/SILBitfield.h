//===--- SILBitfield.h ------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the base template class SILBitfield.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILBITFIELD_H
#define SWIFT_SIL_SILBITFIELD_H

#include "swift/SIL/SILFunction.h"

namespace swift {

/// The base template class for BasicBlockBitfield and NodeBitfield.
template <class Impl, class T> class SILBitfield {
  /// Initialized with the monotonically increasing currentBitfieldID of the
  /// function.
  /// Used to check if the bitfield in a block is initialized.
  /// If a block's lastInitializedBitfieldID is less than this ID, it means
  /// that the bits of that block are not initialized yet.
  /// See also: SILBasicBlock::lastInitializedBitfieldID,
  ///           SILFunction::currentBitfieldID
  uint64_t bitfieldID;

  short startBit;
  short endBit;
  uint32_t mask;

protected:
  /// A single linked list of currently alive bitfields (oldest is
  /// last, newest is first).
  /// The head of the list is SILFunction::newestAliveBlockBitfield and
  /// SILFunction::newestAliveNodeBitfield, respectively.
  Impl *parent;
  
  /// The bitfield is "added" to the blocks/nodes of this function.
  SILFunction *function;

public:
  SILBitfield(SILFunction *function, int size, Impl *parent) :
      bitfieldID(function->currentBitfieldID),
      startBit(parent ? parent->endBit : 0),
      endBit(startBit + size),
      mask(0xffffffffu >> (32 - size) << startBit),
      parent(parent),
      function(function) {
    assert(size > 0 && "bit field size must be > 0");
    assert(endBit <= T::numCustomBits && "too many/large bit fields allocated in function");
    assert((!parent || bitfieldID > parent->bitfieldID) &&
           "BasicBlockBitfield indices are not in order");
    ++function->currentBitfieldID;
    assert(function->currentBitfieldID != 0 && "currentBitfieldID overflow");
  }

  SILBitfield(const SILBitfield &) = delete;
  SILBitfield(SILBitfield &&) = delete;
  SILBitfield &operator=(const SILBitfield &) = delete;
  SILBitfield &operator=(SILBitfield &&) = delete;

  SILFunction *getFunction() const { return function; }

  unsigned get(T *entity) const {
    assert(entity->getFunction() == function);
    if (bitfieldID > entity->lastInitializedBitfieldID) {
      // The bitfield is not initialized yet in this block.
      return 0;
    }
    return (entity->getCustomBits() & mask) >> startBit;
  }

  void set(T *entity, unsigned value) {
    assert(entity->getFunction() == function);
    assert(((value << startBit) & ~mask) == 0 &&
           "value too large for BasicBlockBitfield");
    unsigned clearMask = mask;
    if (bitfieldID > entity->lastInitializedBitfieldID) {

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
      Impl *bf = parent;
      while (bf && bf->bitfieldID > entity->lastInitializedBitfieldID) {
        clearMask |= bf->mask;
        bf = bf->parent;
      }
      entity->lastInitializedBitfieldID = bitfieldID;
    }
    entity->setCustomBits((entity->getCustomBits() & ~clearMask) | (value << startBit));
  }
};


} // namespace swift

#endif
