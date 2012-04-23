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

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "IRGen.h"
#include "IRGenFunction.h"

namespace swift {
namespace irgen {

/// A managed value is a pair of an llvm::Value* and an optional
/// cleanup.
class ManagedValue {
  llvm::Value *Value;
  IRGenFunction::CleanupsDepth Cleanup;

public:
  ManagedValue() = default;
  explicit ManagedValue(llvm::Value *value)
    : Value(value), Cleanup(IRGenFunction::CleanupsDepth::invalid()) {}
  ManagedValue(llvm::Value *value, IRGenFunction::CleanupsDepth cleanup)
    : Value(value), Cleanup(cleanup) {}

  llvm::Value *getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  llvm::Value *getValue() const { return Value; }

  bool hasCleanup() const { return Cleanup.isValid(); }
  IRGenFunction::CleanupsDepth getCleanup() const { return Cleanup; }

  /// Forward this value, deactivating the cleanup and returning the
  /// underlying value.
  llvm::Value *forward(IRGenFunction &IGF) {
    if (hasCleanup())
      IGF.setCleanupState(getCleanup(), CleanupState::Dead);
    return getValue();
  }
};

/// The representation for an explosion is just a list of raw LLVM
/// values.  The meaning of these values is imposed externally by the
/// type infos, except that it is expected that they will be passed
/// as arguments in exactly this way.
class Explosion {
  unsigned NextValue;
  ExplosionKind Kind;
  SmallVector<ManagedValue, 8> Values;

public:
  Explosion(ExplosionKind kind) : NextValue(0), Kind(kind) {}

  // We want to be a move-only type.
  Explosion(const Explosion &) = delete;
  Explosion &operator=(const Explosion &) = delete;
  Explosion &operator=(Explosion &&) = delete;
  Explosion(Explosion &&other) : NextValue(0), Kind(other.Kind) {
    // Do an uninitialized copy of the non-consumed elements.
    Values.reserve(other.size());
    Values.set_size(other.size());
    std::uninitialized_copy(other.begin(), other.end(), Values.begin());

    // Remove everything from the other explosion.
    other.reset();
  }

  ~Explosion() {
    assert(empty() && "explosion had values remaining when destroyed!");
  }

  /// Return the type of explosion this represents.
  ExplosionKind getKind() const { return Kind; }

  bool empty() const {
    return NextValue == Values.size();
  }

  size_t size() const {
    return Values.size() - NextValue;
  }

  typedef ManagedValue *iterator;
  iterator begin() { return Values.begin() + NextValue; }
  iterator end() { return Values.end(); }

  typedef const ManagedValue *const_iterator;
  const_iterator begin() const { return Values.begin() + NextValue; }
  const_iterator end() const { return Values.end(); }

  /// Add a value to the end of this exploded r-value.
  void add(ManagedValue value) {
    assert(NextValue == 0 && "adding to partially-claimed explosion?");
    Values.push_back(value);
  }

  /// Add an unmanaged value to the end of this exploded r-value.
  void addUnmanaged(llvm::Value *value) {
    add(ManagedValue(value));
  }

  void add(llvm::ArrayRef<ManagedValue> values) {
    assert(NextValue == 0 && "adding to partially-claimed explosion?");
    Values.append(values.begin(), values.end());
  }

  /// Return an array containing the given range of values.  The values
  /// are not claimed.
  llvm::ArrayRef<ManagedValue> getRange(unsigned from, unsigned to) const {
    assert(from <= to);
    assert(to <= Values.size());
    return llvm::makeArrayRef(begin() + from, to - from);
  }

  /// Return an array containing all of the remaining values.  The values
  /// are not claimed.
  llvm::ArrayRef<ManagedValue> getAll() {
    return llvm::makeArrayRef(begin(), Values.size() - NextValue);
  }

  /// Transfer ownership of the next N values to the given explosion.
  void transferInto(Explosion &other, unsigned n) {
    other.add(claim(n));
  }

  /// The next N values are being ignored; ensure they are destroyed.
  void ignoreAndDestroy(IRGenFunction &IGF, unsigned n) {
    // For now, just leave their cleanups active.
    markClaimed(n);
  }

  /// The next N values have been claimed in some indirect way (e.g.
  /// using getRange() and the like); just give up on them.
  void markClaimed(unsigned n) {
    assert(NextValue + n <= Values.size());
    NextValue += n;
  }

  /// Claim a value which is known to have no management.
  llvm::Value *claimUnmanagedNext() {
    assert(NextValue < Values.size());
    assert(!Values[NextValue].hasCleanup());
    return Values[NextValue++].getValue();
  }

  /// Claim a series of values which are known to have no management.
  void claimUnmanaged(unsigned n, llvm::SmallVectorImpl<llvm::Value *> &out) {
    assert(NextValue + n <= Values.size());

    out.reserve(out.size() + n);
    for (auto i = begin(), e = i + n; i != e; ++i)
      out.push_back(i->getUnmanagedValue());
    NextValue += n;
  }

  /// Claim and return the next value in this explosion.
  /// The caller becomes responsible for managing the cleanup.
  ManagedValue claimNext() {
    assert(NextValue < Values.size());
    return Values[NextValue++];
  }

  /// Claim and return the next N values in this explosion.
  /// The caller becomes responsible for managing the cleanups.
  llvm::ArrayRef<ManagedValue> claim(unsigned n) {
    assert(NextValue + n <= Values.size());
    auto array = llvm::makeArrayRef(begin(), n);
    NextValue += n;
    return array;
  }

  /// Claim and return all the values in this explosion.
  /// The caller becomes responsible for managing the cleanups.
  llvm::ArrayRef<ManagedValue> claimAll() {
    return claim(size());
  }

  /// Forward the next value in this explosion, deactivating its
  /// cleanup if present.
  llvm::Value *forwardNext(IRGenFunction &IGF) {
    assert(NextValue < Values.size());
    return Values[NextValue++].forward(IGF);
  }

  /// Forward a series of values out of this explosion.
  void forward(IRGenFunction &IGF, unsigned n,
               llvm::SmallVectorImpl<llvm::Value *> &out) {
    assert(NextValue + n <= Values.size());

    out.reserve(out.size() + n);
    for (auto i = begin(), e = i + n; i != e; ++i)
      out.push_back(i->forward(IGF));

    NextValue += n;
  }

  /// Claim and remove the last item in the array.
  /// Unlike the normal 'claim' methods, the item is gone forever.
  ManagedValue takeLast() {
    assert(!empty());
    auto result = Values.back();
    Values.pop_back();
    return result;
  }

  /// Reset this explosion.
  void reset() {
    NextValue = 0;
    Values.clear();
  }

  void reset(ExplosionKind level) {
    Kind = level;
    reset();
  }
};

/// An explosion schema is essentially the type of an Explosion.
class ExplosionSchema {
public:
  /// The maximum number of scalars that we allow to be returned
  /// directly.
  enum { MaxScalarsForDirectResult = 3 };

  /// The schema for one atom of the explosion.
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
  bool ContainsAggregate;

public:
  ExplosionSchema(ExplosionKind kind)
    : Kind(kind), ContainsAggregate(false) {}

  ExplosionKind getKind() const { return Kind; }

  /// Return the number of elements in this schema.
  unsigned size() const { return Elements.size(); }
  bool empty() const { return Elements.empty(); }

  /// Does this explosion contain an aggregate?
  bool containsAggregate() const { return ContainsAggregate; }

  bool requiresIndirectResult() const {
    return containsAggregate() || size() > MaxScalarsForDirectResult;
  }

  typedef llvm::SmallVectorImpl<Element>::iterator iterator;
  typedef llvm::SmallVectorImpl<Element>::const_iterator const_iterator;
  
  iterator begin() { return Elements.begin(); }
  iterator end() { return Elements.end(); }
  const_iterator begin() const { return Elements.begin(); }
  const_iterator end() const { return Elements.end(); }

  void add(Element e) {
    Elements.push_back(e);
    ContainsAggregate |= e.isAggregate();
  }
};

} // end namespace irgen
} // end namespace swift

#endif
