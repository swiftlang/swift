//===--- OperandBits.h ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_OPERANDBITS_H
#define SWIFT_SIL_OPERANDBITS_H

#include "swift/SIL/SILBitfield.h"
#include "swift/SIL/SILValue.h"

namespace swift {

class OperandBitfield : public SILBitfield<OperandBitfield, Operand> {
  template <class, class>
  friend class SILBitfield;

  OperandBitfield *insertInto(SILFunction *function) {
    assert(function && "OperandBitField constructed with a null function");
    OperandBitfield *oldParent = function->newestAliveOperandBitfield;
    function->newestAliveOperandBitfield = this;
    return oldParent;
  }

public:
  OperandBitfield(SILFunction *function, int size)
      : SILBitfield(function, size, insertInto(function)) {}

  ~OperandBitfield() {
    assert(function->newestAliveOperandBitfield == this &&
           "BasicBlockBitfield destructed too early");
    function->newestAliveOperandBitfield = parent;
  }
};

/// A set of Operands.
///
/// For details see OperandBitfield.
class OperandSet {
  OperandBitfield bit;

public:
  OperandSet(SILFunction *function) : bit(function, 1) {}

  SILFunction *getFunction() const { return bit.getFunction(); }

  bool contains(Operand *node) const { return (bool)bit.get(node); }

  /// Returns true if \p node was not contained in the set before inserting.
  bool insert(Operand *node) {
    bool wasContained = contains(node);
    if (!wasContained) {
      bit.set(node, 1);
    }
    return !wasContained;
  }

  void erase(Operand *node) { bit.set(node, 0); }
};

using OperandSetWithSize = KnownSizeSet<OperandSet>;

} // namespace swift

#endif
