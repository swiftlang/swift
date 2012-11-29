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

#ifndef SWIFT_LOWERING_EXPLOSION_H
#define SWIFT_LOWERING_EXPLOSION_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "SILGen.h"

namespace swift {
namespace Lowering {

/// A managed value is a pair of a SIL value and an optional cleanup.
class ManagedValue {
  Value value;
  CleanupsDepth cleanup;

public:
  ManagedValue() = default;
  explicit ManagedValue(Value value)
    : value(value), cleanup(CleanupsDepth::invalid()) {}
  ManagedValue(Value value, CleanupsDepth cleanup)
    : value(value), cleanup(cleanup) {}

  Value getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  Value getValue() const { return value; }

  bool hasCleanup() const { return cleanup.isValid(); }
  CleanupsDepth getCleanup() const { return cleanup; }

  /// Forward this value, deactivating the cleanup and returning the
  /// underlying value.
  Value forward(SILGen &gen) {
    if (hasCleanup())
      gen.Cleanups.setCleanupState(getCleanup(), CleanupState::Dead);
    return getValue();
  }

  /// Split this value into its underlying value and, if present, its cleanup.
  Value split(llvm::SmallVectorImpl<CleanupsDepth> &cleanups) {
    if (hasCleanup()) cleanups.push_back(getCleanup());
    return getValue();
  }
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
  void addUnmanaged(Value value) {
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
  void ignoreAndDestroy(SILGen &gen, unsigned n) {
    // For now, just leave their cleanups active.
    markClaimed(n);
  }

  /// The next N values are being ignored.  They are all unmanaged.
  void ignoreUnmanaged(unsigned n) {
#ifndef NDEBUG
    assert(NextValue + n <= Values.size());
    for (auto i = NextValue, e = NextValue + n; i != e; ++i)
      assert(!Values[i].hasCleanup());
#endif
    markClaimed(n);
  }

  /// The next N values have been claimed in some indirect way (e.g.
  /// using getRange() and the like); just give up on them.
  void markClaimed(unsigned n) {
    assert(NextValue + n <= Values.size());
    NextValue += n;
  }

  /// Claim a value which is known to have no management.
  Value claimUnmanagedNext() {
    assert(NextValue < Values.size());
    assert(!Values[NextValue].hasCleanup());
    return Values[NextValue++].getValue();
  }

  /// Claim a series of values which are known to have no management.
  void claimUnmanaged(unsigned n, llvm::SmallVectorImpl<Value> &out) {
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
  Value forwardNext(SILGen &gen) {
    assert(NextValue < Values.size());
    return Values[NextValue++].forward(gen);
  }

  /// Forward a series of values out of this explosion.
  void forward(SILGen &gen, unsigned n,
               llvm::SmallVectorImpl<Value> &out) {
    assert(NextValue + n <= Values.size());

    out.reserve(out.size() + n);
    for (auto i = begin(), e = i + n; i != e; ++i)
      out.push_back(i->forward(gen));

    NextValue += n;
  }

  // These are all kindof questionable.

  /// Without changing any state, take the last claimed value,
  /// if there is one.
  Value getLastClaimed() {
    assert(NextValue > 0);
    return Values[NextValue-1].getUnmanagedValue();
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
  
// TODO explosion schema

} // end namespace Lowering
} // end namespace swift

#endif
