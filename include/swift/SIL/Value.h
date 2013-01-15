//===--- Value.h - Value base class for SIL ---------------------*- C++ -*-===//
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
// This file defines the Value class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_VALUE_H
#define SWIFT_SIL_VALUE_H

#include "swift/SIL/SILModule.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
  class SILTypeList;

  enum class ValueKind {
#define VALUE(Id, Parent) Id,
#define VALUE_RANGE(Id, FirstId, LastId) \
    First_##Id = FirstId, Last_##Id = LastId,
#include "swift/SIL/SILNodes.def"
  };

  /// ValueBase - This is the base class of the SIL value hierarchy, which
  /// represents a runtime computed value.  Things like Instruction derive from
  /// this.
  class ValueBase : public SILAllocated<ValueBase> {
    PointerUnion<SILTypeList*, SILType> Types;
    const ValueKind Kind;
  protected:
    ValueBase(ValueKind Kind, SILTypeList *TypeList = 0)
      : Types(TypeList), Kind(Kind) {}
    ValueBase(ValueKind Kind, SILType Ty)
      : Types(Ty), Kind(Kind) {}
  public:

    ValueKind getKind() const { return Kind; }

    ArrayRef<SILType> getTypes() const;

    SILType getType(unsigned i) const { return getTypes()[i]; }

    /// Pretty-print the Instruction.
    void dump() const;
    void print(raw_ostream &OS) const;
  };

  /// Value - A Value is a use of a specific result of an ValueBase.  As such,
  /// it is a pair of the ValueBase and the result number being referenced.
  class Value {
    llvm::PointerIntPair<ValueBase*, 1> ValueAndResultNumber;
  public:
    Value(const ValueBase *V = 0, unsigned ResultNumber = 0)
      : ValueAndResultNumber((ValueBase*)V, ResultNumber) {
      assert(ResultNumber == getResultNumber() && "Overflow");
    }

    ValueBase *getDef() const {
      return ValueAndResultNumber.getPointer();
    }
    ValueBase *operator->() const { return getDef(); }
    unsigned getResultNumber() const { return ValueAndResultNumber.getInt(); }

    SILType getType() const {
      return getDef()->getType(getResultNumber());
    }

    // Comparison.
    bool operator==(Value RHS) const {
      return ValueAndResultNumber == RHS.ValueAndResultNumber;
    }
    bool operator!=(Value RHS) const { return !(*this == RHS); }
    // Ordering (for std::map).
    bool operator<(Value RHS) const {
      return ValueAndResultNumber.getOpaqueValue() <
      RHS.ValueAndResultNumber.getOpaqueValue();
    }

    // Check validity.
    bool isValid() const { return getDef() != nullptr; }
    explicit operator bool() const { return getDef() != nullptr; }
    
    // Use as a pointer-like type.
    void *getOpaqueValue() const {
      return ValueAndResultNumber.getOpaqueValue();
    }
  };
} // end namespace swift


namespace llvm {
  // A Value casts like a ValueBase*.
  template<> struct simplify_type<const ::swift::Value> {
    typedef ::swift::ValueBase *SimpleType;
    static SimpleType getSimplifiedValue(::swift::Value Val) {
      return Val.getDef();
    }
  };
  template<> struct simplify_type< ::swift::Value>
    : public simplify_type<const ::swift::Value> {};

  // Value's hash just like pointers.
  template<> struct DenseMapInfo<swift::Value> {
    static swift::Value getEmptyKey() {
      return llvm::DenseMapInfo<swift::ValueBase*>::getEmptyKey();
    }
    static swift::Value getTombstoneKey() {
      return llvm::DenseMapInfo<swift::ValueBase*>::getTombstoneKey();
    }
    static unsigned getHashValue(swift::Value V) {
      return DenseMapInfo<swift::ValueBase*>::getHashValue(V.getDef());
    }
    static bool isEqual(swift::Value LHS, swift::Value RHS) {
      return LHS == RHS;
    }
  };
}  // end namespace llvm

#endif
