//===--- RValue.h - RValue Representation -----------------------*- C++ -*-===//
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
// A storage structure for holding r-values in Swift IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_RVALUE_H
#define SWIFT_IRGEN_RVALUE_H

#include "llvm/ADT/ArrayRef.h"

namespace llvm {
  class Value;
}

namespace swift {
namespace irgen {

/// An r-value represents a value of a type, as opposed to an l-value,
/// which is a reference to storage holding such a value.
///
/// An r-value can either be *scalar*, in which case it is some number
/// (from 0 to MaxScalarValues) of independent scalar values, or
/// *aggregate*, in which case it is the address of an (unaliased)
/// location in memory holding the value.
class RValue {
public:
  enum { MaxScalarValues = 3 };
private:
  enum { IsAggregate = MaxScalarValues + 1,
         IsInvalid = MaxScalarValues + 2 };

public:
  RValue() : NumScalarValues(IsInvalid) {}

  bool isValid() const { return NumScalarValues != IsInvalid; }
  bool isScalar() const {
    assert(isValid());
    return NumScalarValues != IsAggregate;
  }
  bool isAggregate() const { return !isScalar(); }

  ArrayRef<llvm::Value*> getScalars() const {
    assert(isScalar());
    return makeArrayRef(Values, Values + NumScalarValues);
  }

  llvm::Value *getAggregateAddr() const {
    assert(isAggregate());
    return Values[0];
  }

  static RValue forScalars(ArrayRef<llvm::Value*> Scalars) {
    assert(Scalars.size() <= MaxScalarValues);

    RValue RV;
    RV.NumScalarValues = Scalars.size();
    for (unsigned I = 0, E = Scalars.size(); I != E; ++I)
      RV.Values[I] = Scalars[I];
    return RV;
  }

  static RValue forAggregate(llvm::Value *Addr) {
    RValue RV;
    RV.NumScalarValues = IsAggregate;
    RV.Values[0] = Addr;
    return RV;
  }

private:
  llvm::Value *Values[MaxScalarValues];
  unsigned char NumScalarValues;
};

} // end namespace irgen
} // end namespace swift

#endif
