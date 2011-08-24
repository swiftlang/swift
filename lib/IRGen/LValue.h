//===--- LValue.h - LValue Representation -----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A storage structure for holding l-values in Swift IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LVALUE_H
#define SWIFT_IRGEN_LVALUE_H

#include "IRGen.h"

namespace llvm {
  class Value;
}

namespace swift {
namespace irgen {

/// An l-value represents a reference to storage holding a value
/// of a type, as opposed to an r-value, which is an actual value
/// of the type.
class LValue {
public:
  LValue() : Address(nullptr) {}

  bool isValid() const { return Address != nullptr; }

  llvm::Value *getAddress() const {
    assert(isValid());
    return Address;
  }

  Alignment getAlignment() const {
    assert(isValid());
    return Align;
  }

  static LValue forAddress(llvm::Value *Address, Alignment Align) {
    assert(Address != nullptr);

    LValue LV;
    LV.Address = Address;
    LV.Align = Align;
    return LV;
  }

private:
  llvm::Value *Address;
  Alignment Align;
};

} // end namespace irgen
} // end namespace swift

#endif
