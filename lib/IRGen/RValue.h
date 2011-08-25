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
// Types relating to the LLVM emission of r-values, including the
// RValue structure for holding emitted r-values.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_RVALUE_H
#define SWIFT_IRGEN_RVALUE_H

#include "llvm/ADT/ArrayRef.h"
#include "IRGen.h"

namespace llvm {
  class Type;
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
  enum { MaxScalars = 3 };
private:
  enum { IsAggregate = MaxScalars + 1,
         IsInvalid = MaxScalars + 2 };

public:
  RValue() : NumScalars(IsInvalid) {}

  bool isValid() const { return NumScalars != IsInvalid; }
  bool isScalar() const {
    assert(isValid());
    return NumScalars != IsAggregate;
  }
  bool isAggregate() const { return !isScalar(); }

  /// Answers whether this RValue holds the given number of scalars.
  bool isScalar(unsigned Count) const {
    assert(Count <= MaxScalars);
    return NumScalars == Count;
  }

  ArrayRef<llvm::Value*> getScalars() const {
    assert(isScalar());
    return makeArrayRef(ScalarValues, ScalarValues + NumScalars);
  }

  llvm::Value *getAggregateAddress() const {
    assert(isAggregate());
    return AggregateAddress;
  }

  static RValue forScalars(ArrayRef<llvm::Value*> Scalars) {
    assert(Scalars.size() <= MaxScalars);

    RValue RV;
    RV.NumScalars = Scalars.size();
    for (unsigned I = 0, E = Scalars.size(); I != E; ++I)
      RV.ScalarValues[I] = Scalars[I];
    return RV;
  }

  static RValue forScalars() {
    return forScalars(ArrayRef<llvm::Value*>());
  }

  static RValue forScalars(llvm::Value *V) {
    llvm::Value *Scalars[] = { V };
    return forScalars(Scalars);
  }

  static RValue forScalars(llvm::Value *V1, llvm::Value *V2) {
    llvm::Value *Scalars[] = { V1, V2 };
    return forScalars(Scalars);
  }

  static RValue forScalars(llvm::Value *V1, llvm::Value *V2, llvm::Value *V3) {
    llvm::Value *Scalars[] = { V1, V2, V3 };
    return forScalars(Scalars);
  }

  static RValue forAggregate(llvm::Value *Addr) {
    RValue RV;
    RV.NumScalars = IsAggregate;
    RV.AggregateAddress = Addr;
    return RV;
  }

private:
  union {
    llvm::Value *ScalarValues[MaxScalars];
    llvm::Value *AggregateAddress;
  };
  unsigned char NumScalars;
};

/// An r-value schema is a sort of "type" for emitted r-values.  It
/// indicates whether an conformant r-value will be emitted as
/// independent scalars, and if so, which LLVM IR types they have, or
/// if it will be emitted as an aggregate in memory, and if so, the
/// required type and alignment for that memory.
class RValueSchema {
public:
  enum { MaxScalars = RValue::MaxScalars };
private:
  enum { IsAggregate = MaxScalars + 1,
         IsInvalid = MaxScalars + 2 };

public:
  RValueSchema() : NumScalars(IsInvalid) {}

  bool isValid() const { return NumScalars != IsInvalid; }
  bool isScalar() const {
    assert(isValid());
    return NumScalars != IsAggregate;
  }
  bool isAggregate() const { return !isScalar(); }

  /// Answers whether this RValue holds the given number of scalars.
  bool isScalar(unsigned Count) const {
    assert(Count <= MaxScalars);
    return NumScalars == Count;
  }

  ArrayRef<llvm::Type*> getScalarTypes() const {
    assert(isScalar());
    return makeArrayRef(ScalarTypes, ScalarTypes + NumScalars);
  }

  /// Returns the allocation type of an aggregate value.
  llvm::StructType *getAggregateType() const {
    assert(isAggregate());
    return Aggregate.Type;
  }

  Alignment getAggregateAlignment() const {
    assert(isAggregate());
    return Alignment(Aggregate.Alignment);
  }

  static RValueSchema forScalars(ArrayRef<llvm::Type*> Scalars) {
    assert(Scalars.size() <= MaxScalars);

    RValueSchema RV;
    RV.NumScalars = Scalars.size();
    for (unsigned I = 0, E = Scalars.size(); I != E; ++I)
      RV.ScalarTypes[I] = Scalars[I];
    return RV;
  }

  static RValueSchema forScalars() {
    return forScalars(ArrayRef<llvm::Type*>());
  }

  static RValueSchema forScalars(llvm::Type *Ty) {
    llvm::Type *Scalars[] = { Ty };
    return forScalars(Scalars);
  }

  static RValueSchema forScalars(llvm::Type *Ty1, llvm::Type *Ty2) {
    llvm::Type *Scalars[] = { Ty1, Ty2 };
    return forScalars(Scalars);
  }

  static RValueSchema forScalars(llvm::Type *Ty1, llvm::Type *Ty2,
                                 llvm::Type *Ty3) {
    llvm::Type *Scalars[] = { Ty1, Ty2, Ty3 };
    return forScalars(Scalars);
  }

  static RValueSchema forAggregate(llvm::StructType *Type, Alignment Align) {
    RValueSchema RV;
    RV.NumScalars = IsAggregate;
    RV.Aggregate.Type = Type;
    RV.Aggregate.Alignment = Align.getValue();
    return RV;
  }

private:
  union {
    llvm::Type *ScalarTypes[MaxScalars];
    struct {
      llvm::StructType *Type;
      Alignment::int_type Alignment;
    } Aggregate;
  };
  unsigned char NumScalars;
};

} // end namespace irgen
} // end namespace swift

#endif
