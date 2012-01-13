//===--- Explosion.h - Exploded R-Value Representation ----------*- C++ -*-===//
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
// A storage structure for holding an exploded r-value.  An exploded
// r-value has been separated into individual components.  Only types
// with non-resilient structure may be exploded.
//
// The standard use for explosions is for argument-passing.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_EXPLOSION_H
#define SWIFT_IRGEN_EXPLOSION_H

#include "llvm/ADT/SmallVector.h"

namespace llvm {
  class Value;
}

namespace swift {
namespace irgen {

/// Different places in the model require specific explosion
/// semantics.
enum class ExplosionKind : unsigned {
  /// A minimal explosion does not explode types that do not have a
  /// universally fragile representation.
  ///
  /// This is the explosion used for calls through function pointers,
  /// because we cannot assume that all the types are fragile in the
  /// component providing the function.
  Minimal,

  /// A maximal explosion explodes all types with fragile
  /// representation, even when they're not universally fragile.
  Maximal
};

/// The representation for an explosion is just a list of raw LLVM
/// values.  The meaning of these values is imposed externally by the
/// type infos, except that it is expected that they will be passed
/// as arguments in exactly this way.
class Explosion {
  unsigned NextValue;
  ExplosionKind Kind;
  SmallVector<llvm::Value*, 8> Values;

public:
  Explosion(ExplosionKind kind) : NextValue(0), Kind(kind) {}

  /// Return the type of explosion this represents.
  ExplosionKind getKind() const { return Kind; }

  bool empty() const {
    return NextValue == Values.size();
  }

  size_t size() const {
    return Values.size() - NextValue;
  }

  typedef llvm::Value **iterator;
  iterator begin() { return Values.begin() + NextValue; }
  iterator end() { return Values.end(); }

  typedef llvm::Value * const *const_iterator;
  const_iterator begin() const { return Values.begin() + NextValue; }
  const_iterator end() const { return Values.end(); }

  /// Add a value to this end of this exploded r-value.
  void add(llvm::Value *value) {
    assert(NextValue == 0 && "adding to partially-claimed explosion?");
    Values.push_back(value);
  }

  void add(llvm::ArrayRef<llvm::Value *> values) {
    assert(NextValue == 0 && "adding to partially-claimed explosion?");
    Values.append(values.begin(), values.end());
  }

  /// Return an array containing the given range of values.  The values
  /// are not claimed.
  llvm::ArrayRef<llvm::Value *> getRange(unsigned from, unsigned to) const {
    assert(from <= to);
    assert(to <= Values.size());
    return llvm::makeArrayRef(begin() + from, to - from);
  }

  /// Return an array containing all of the remaining values.  The values
  /// are not claimed.
  llvm::ArrayRef<llvm::Value*> getAll() {
    return llvm::makeArrayRef(begin(), Values.size() - NextValue);
  }

  /// Claim and return the next value in this explosion.
  llvm::Value *claimNext() {
    assert(NextValue < Values.size());
    return Values[NextValue++];
  }

  /// Claim and return the next N values in this explosion.
  llvm::ArrayRef<llvm::Value*> claim(unsigned n) {
    assert(NextValue + n <= Values.size());
    auto array = llvm::makeArrayRef(begin(), n);
    NextValue += n;
    return array;
  }

  /// Claim and return all the remaining values in this explosion.
  llvm::ArrayRef<llvm::Value*> claimAll() {
    auto array = llvm::makeArrayRef(begin(), Values.size() - NextValue);
    NextValue = Values.size();
    return array;
  }

  /// Reset this explosion.
  void reset() {
    NextValue = 0;
    Values.clear();
  }
};

/// An explosion schema is essentially the type of an Explosion.
class ExplosionSchema {
public:
  class Element {
    llvm::Type *Type;
    Alignment::int_type Align;
    Element() = default;
  public:
    static Element forScalar(llvm::Type *type) {
      Element e;
      e.Type = type;
      e.Align = 0;
      return e;
    }

    static Element forAggregate(llvm::Type *type, Alignment align) {
      assert(align.getValue() != 0 && "alignment with zero value!");
      Element e;
      e.Type = type;
      e.Align = align.getValue();
      return e;
    }

    bool isScalar() const { return Align == 0; }
    llvm::Type *getScalarType() const { assert(isScalar()); return Type; }

    bool isAggregate() const { return !isScalar(); }
    llvm::Type *getAggregateType() const {
      assert(isAggregate());
      return Type;
    }
    Alignment getAggregateAlignment() const {
      assert(isAggregate());
      return Alignment(Align);
    }
  };
  
private:
  llvm::SmallVector<Element, 8> Elements;
  ExplosionKind Kind;

public:
  ExplosionSchema(ExplosionKind kind) : Kind(kind) {}

  ExplosionKind getKind() const { return Kind; }

  /// Return the number of elements in this schema.
  unsigned size() const { return Elements.size(); }
  bool empty() const { return Elements.empty(); }

  typedef llvm::SmallVectorImpl<Element>::iterator iterator;
  typedef llvm::SmallVectorImpl<Element>::const_iterator const_iterator;
  
  iterator begin() { return Elements.begin(); }
  iterator end() { return Elements.end(); }
  const_iterator begin() const { return Elements.begin(); }
  const_iterator end() const { return Elements.end(); }

  void add(Element e) { Elements.push_back(e); }
};

} // end namespace irgen
} // end namespace swift

#endif
