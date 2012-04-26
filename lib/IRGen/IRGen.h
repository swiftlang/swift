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
#include <cassert>

namespace llvm {
  class Value;
}

namespace swift {
namespace irgen {

class Size;

/// Whether or not an object should be emitted on the heap.
enum OnHeap_t : unsigned char {
  NotOnHeap,
  OnHeap
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
  Program
};

/// ExplosionKind - A policy for choosing what types should be
/// exploded, as informed by the resilience model.
enum class ExplosionKind : unsigned {
  /// A minimal explosion does not explode types that do not have a
  /// universally fragile representation.  This provides a baseline
  /// for what all components can possibly support.
  ///   - All exported functions must be compiled to at least provide
  ///     a minimally-exploded entrypoint, or else it will be
  ///     impossible for components that do not have that type
  ///     to call the function.
  ///   - Similarly, any sort of opaque function call must be through
  ///     a minimally-exploded entrypoint.
  Minimal,

  /// A maximal explosion explodes all types with fragile
  /// representation, even when they're not universally fragile.  This
  /// is useful when internally manipulating objects or when working
  /// with specialized entry points for a function.
  Maximal
};

/// An alignment value, in eight-bit units.
class Alignment {
public:
  typedef uint32_t int_type;

  Alignment() : Value(0) {}
  explicit Alignment(int_type Value) : Value(Value) {}

  int_type getValue() const { return Value; }

  bool isOne() const { return Value == 1; }
  bool isZero() const { return Value == 0; }

  Alignment alignmentAtOffset(Size S) const;

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

  Size() : Value(0) {}
  explicit Size(int_type Value) : Value(Value) {}

  int_type getValue() const { return Value; }

  bool isZero() const { return Value == 0; }

  friend Size operator+(Size L, Size R) {
    return Size(L.Value + R.Value);
  }

  friend Size &operator+=(Size &L, Size R) {
    L.Value += R.Value;
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

  explicit operator bool() const { return Value != 0; }

  Size roundUpToAlignment(Alignment align) const {
    int_type value = getValue() + align.getValue() - 1;
    return Size(value & ~int_type(align.getValue() - 1));
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

} // end namespace irgen
} // end namespace swift

#endif
