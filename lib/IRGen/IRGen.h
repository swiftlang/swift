//===--- IRGen.h - Common Declarations for IR Generation --------*- C++ -*-===//
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
// This file defines some types that are generically useful in IR
// Generation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGEN_H
#define SWIFT_IRGEN_IRGEN_H

#include "llvm/Support/DataTypes.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/SIL/AbstractionPattern.h"
#include <cassert>

namespace llvm {
  class Value;
}

namespace swift {
  class CanType;
  class ClusteredBitVector;
  enum ForDefinition_t : bool;
  
namespace irgen {
  using Lowering::AbstractionPattern;

/// In IRGen, we use Swift's ClusteredBitVector data structure to
/// store vectors of spare bits.
using SpareBitVector = ClusteredBitVector;

class Size;

enum IsPOD_t : bool { IsNotPOD, IsPOD };
inline IsPOD_t operator&(IsPOD_t l, IsPOD_t r) {
  return IsPOD_t(unsigned(l) & unsigned(r));
}
inline IsPOD_t &operator&=(IsPOD_t &l, IsPOD_t r) {
  return (l = (l & r));
}

enum IsFixedSize_t : bool { IsNotFixedSize, IsFixedSize };
inline IsFixedSize_t operator&(IsFixedSize_t l, IsFixedSize_t r) {
  return IsFixedSize_t(unsigned(l) & unsigned(r));
}
inline IsFixedSize_t &operator&=(IsFixedSize_t &l, IsFixedSize_t r) {
  return (l = (l & r));
}

enum IsLoadable_t : bool { IsNotLoadable, IsLoadable };
inline IsLoadable_t operator&(IsLoadable_t l, IsLoadable_t r) {
  return IsLoadable_t(unsigned(l) & unsigned(r));
}
inline IsLoadable_t &operator&=(IsLoadable_t &l, IsLoadable_t r) {
  return (l = (l & r));
}

enum IsBitwiseTakable_t : bool { IsNotBitwiseTakable, IsBitwiseTakable };
inline IsBitwiseTakable_t operator&(IsBitwiseTakable_t l, IsBitwiseTakable_t r) {
  return IsBitwiseTakable_t(unsigned(l) & unsigned(r));
}
inline IsBitwiseTakable_t &operator&=(IsBitwiseTakable_t &l, IsBitwiseTakable_t r) {
  return (l = (l & r));
}

/// Whether or not an object should be emitted on the heap.
enum OnHeap_t : unsigned char {
  NotOnHeap,
  OnHeap
};

/// Whether a function requires extra data.
enum class ExtraData : unsigned char {
  /// The function requires no extra data.
  None,

  /// The function requires a retainable object pointer of extra data.
  Retainable,
  
  /// The function takes its block object as extra data.
  Block,
  
  Last_ExtraData = Block
};

/// ResilienceScope - The compiler is often able to pursue
/// optimizations based on its knowledge of the implementation of some
/// language structure.  However, optimizations which affect
/// cross-component interfaces are not necessarily sound in the face
/// of differing compiler versions and API changes that make types
/// fragile.  The "resilience scope" is the breadth of the code
/// affected by the answer to a question being asked.
///
/// TODO: maybe deployment versions should factor in here.  If a
/// question is being asked vis-a-vis the implementation of a subject
/// structure that is unavailable in any revision for which the object
/// structure is resilient, is there any reason not to answer as if
/// the subject structure were universally fragile?
enum class ResilienceScope {
  /// Local scope means the decision doesn't have to be consistent
  /// with anything.
  Local,

  /// Component scope means the decision has to be consistent within
  /// the current component.  In the current theory, this is equivalent
  /// to Local because the entire component is recompiled as one.
  Component,

  /// Program scope means the decision has to be consistent across all
  /// components.
  Program,

  /// Universal scope means that the decision has to be consistent
  /// across all possible clients who could see this declaration.
  Universal
};

/// Whether an object is fixed in size or not.  This answer is always
/// relative to some resilience scope.
enum class ObjectSize : uint8_t {
  /// The object's size is fixed in the resilience scope.
  Fixed,

  /// The object's size is unknown in the resilience domain, but it is
  /// not dependent.
  Resilient,

  /// The object's size is dependent on a generic parameter.
  Dependent
};

/// Destructor variants.
enum class DestructorKind : uint8_t {
  /// A deallocating destructor destroys the object and deallocates
  /// the memory associated with it.
  Deallocating,

  /// A destroying destructor destroys the object but does not
  /// deallocate the memory associated with it.
  Destroying
};

/// Constructor variants.
enum class ConstructorKind : uint8_t {
  /// An allocating constructor allocates an object and initializes it.
  Allocating,

  /// An initializing constructor just initializes an existing object.
  Initializing
};

/// An alignment value, in eight-bit units.
class Alignment {
public:
  typedef uint32_t int_type;

  Alignment() : Value(0) {}
  explicit Alignment(int_type Value) : Value(Value) {}

  int_type getValue() const { return Value; }
  int_type getMaskValue() const { return Value - 1; }

  bool isOne() const { return Value == 1; }
  bool isZero() const { return Value == 0; }

  Alignment alignmentAtOffset(Size S) const;
  Size asSize() const;

  unsigned log2() const {
    return llvm::Log2_64(Value);
  }

  explicit operator bool() const { return Value != 0; }

  friend bool operator< (Alignment L, Alignment R){ return L.Value <  R.Value; }
  friend bool operator<=(Alignment L, Alignment R){ return L.Value <= R.Value; }
  friend bool operator> (Alignment L, Alignment R){ return L.Value >  R.Value; }
  friend bool operator>=(Alignment L, Alignment R){ return L.Value >= R.Value; }
  friend bool operator==(Alignment L, Alignment R){ return L.Value == R.Value; }
  friend bool operator!=(Alignment L, Alignment R){ return L.Value != R.Value; }

private:
  int_type Value;
};

/// A size value, in eight-bit units.
class Size {
public:
  typedef uint64_t int_type;

  constexpr Size() : Value(0) {}
  explicit constexpr Size(int_type Value) : Value(Value) {}

  /// An "invalid" size, equal to the maximum possible size.
  static constexpr Size invalid() { return Size(~int_type(0)); }

  /// Is this the "invalid" size value?
  bool isInvalid() const { return *this == Size::invalid(); }

  int_type getValue() const { return Value; }
  
  int_type getValueInBits() const { return Value * 8; }

  bool isZero() const { return Value == 0; }

  friend Size operator+(Size L, Size R) {
    return Size(L.Value + R.Value);
  }
  friend Size &operator+=(Size &L, Size R) {
    L.Value += R.Value;
    return L;
  }

  friend Size operator-(Size L, Size R) {
    return Size(L.Value - R.Value);
  }
  friend Size &operator-=(Size &L, Size R) {
    L.Value -= R.Value;
    return L;
  }

  friend Size operator*(Size L, int_type R) {
    return Size(L.Value * R);
  }
  friend Size operator*(int_type L, Size R) {
    return Size(L * R.Value);
  }
  friend Size &operator*=(Size &L, int_type R) {
    L.Value *= R;
    return L;
  }

  friend int_type operator/(Size L, Size R) {
    return L.Value / R.Value;
  }

  explicit operator bool() const { return Value != 0; }

  Size roundUpToAlignment(Alignment align) const {
    int_type value = getValue() + align.getValue() - 1;
    return Size(value & ~int_type(align.getValue() - 1));
  }

  bool isPowerOf2() const {
    auto value = getValue();
    return ((value & -value) == value);
  }

  bool isMultipleOf(Size other) const {
    return (Value % other.Value) == 0;
  }

  unsigned log2() const {
    return llvm::Log2_64(Value);
  }

  friend bool operator< (Size L, Size R) { return L.Value <  R.Value; }
  friend bool operator<=(Size L, Size R) { return L.Value <= R.Value; }
  friend bool operator> (Size L, Size R) { return L.Value >  R.Value; }
  friend bool operator>=(Size L, Size R) { return L.Value >= R.Value; }
  friend bool operator==(Size L, Size R) { return L.Value == R.Value; }
  friend bool operator!=(Size L, Size R) { return L.Value != R.Value; }

  friend Size operator%(Size L, Alignment R) {
    return Size(L.Value % R.getValue());
  }

private:
  int_type Value;
};

/// Compute the alignment of a pointer which points S bytes after a
/// pointer with this alignment.
inline Alignment Alignment::alignmentAtOffset(Size S) const {
  assert(getValue() && "called on object with zero alignment");

  // If the offset is zero, use the original alignment.
  Size::int_type V = S.getValue();
  if (!V) return *this;

  // Find the offset's largest power-of-two factor.
  V = V & -V;

  // The alignment at the offset is then the min of the two values.
  if (V < getValue())
    return Alignment(static_cast<Alignment::int_type>(V));
  return *this;
}

/// Get this alignment asx a Size value.
inline Size Alignment::asSize() const {
  return Size(getValue());
}

} // end namespace irgen
} // end namespace swift

#endif
