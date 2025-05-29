//===--- EnumMap.h - A map optimized for having enum keys -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
///  This file defines the EnumMap class template, which is a map data
///  structure optimized for working with enumerated keys.  It is built on
///  top of SmallMap, but it replaces the default large map with a flat
///  heap-allocated array of indexes into the elements, which is reasonable
///  for small-ish enums.
///
///  Currently the map requires the key type to be an enum type.
///  The expectation is that the enum has a small number of enumerators
///  which are all in the range 0..<NumValues.  NumValues must be provided
///  by specializing the EnumTraits class.
///
///  The elements of the map remain insertion-ordered for the lifetime of
///  the map.  There are currently no operations to remove elements.
///  Iterators are invalidated by insertion.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ENUMMAP_H
#define SWIFT_BASIC_ENUMMAP_H

#include "swift/Basic/EnumTraits.h"
#include "swift/Basic/SmallMap.h"
#include "llvm/ADT/SmallVector.h"
#include <type_traits>

namespace swift {

/// The maximum number of elements that the map can have before
/// it flips from brute-force searching the keys to using a sparse
/// array.
static constexpr size_t DefaultEnumMapDirectSearchLimit =
  DefaultSmallMapDirectSearchLimit;

/// The primary customization point for an EnumMap.
///
/// template <>
/// struct EnumMapTraits<MyKey> {
///   using IndexType = <some integer type>;
///   struct LargeMapStorage {
///     std::optional<IndexType> find(IndexType) const;
///     std::pair<IndexType, bool> insert(IndexType key, IndexType value);
///   };
/// };
template <class Key, class KeyTraits = EnumTraits<Key>>
struct EnumMapTraits;

template <class Key, class Value,
          size_t DirectSearchLimit = DefaultEnumMapDirectSearchLimit,
          class MapTraits = EnumMapTraits<Key>,
          class ElementStorage = llvm::SmallVector<Value>>
class EnumMap {
  using IndexType = typename MapTraits::IndexType;

  // EnumMapTraits is currently designed to be usable directly as a
  // SmallMapTraits.
  using MapType =
    SmallMap<IndexType, Value, DirectSearchLimit, MapTraits, ElementStorage>;
  MapType map;

public:
  bool empty() const { return map.empty(); }
  size_t size() const { return map.size(); }

  using iterator = typename MapType::iterator;
  iterator begin() { return map.begin(); }
  iterator end() { return map.end(); }

  using const_iterator = typename MapType::const_iterator;
  const_iterator begin() const { return map.begin(); }
  const_iterator end() const { return map.end(); }

  /// Look up a key in the map.  Returns end() if the entry is not found.
  const_iterator find(Key key) const {
    return map.find(IndexType(key));
  }

  /// Try to insert the given key/value pair.  If there's already an element
  /// with this key, return false and an iterator for the existing element.
  /// Otherwise, return true and an iterator for the new element.
  ///
  /// The value in the set will be constructed by emplacing it with the
  /// given arguments.
  template <class... Args>
  std::pair<iterator, bool> insert(Key key, Args &&...valueArgs) {
    return map.insert(IndexType(key), std::forward<Args>(valueArgs)...);
  }
};

namespace EnumMapImpl {

template <size_t N,
          bool SmallEnoughForUInt8 = (N < (1U << 8)),
          bool SmallEnoughForUInt16 = (N < (1U << 16))>
struct SufficientIntFor;

template <size_t N>
struct SufficientIntFor<N, true, true> {
  using type = uint8_t;
};

template <size_t N>
struct SufficientIntFor<N, false, true> {
  using type = uint16_t;
};

template <size_t N>
struct SufficientIntFor<N, false, false> {
  static_assert(N < (1ULL << 32), "just how large is this \"enum\" exactly");
  using type = uint32_t;
};

/// A map from integers in 0..<N to integers in 0..<N, implemented as a
/// flat array of integers in 0...N, with zero meaning a missing entry.
///
/// This is a great implementation for N <= 255, where the
/// entire flat array is <= 256 bytes.  It gets increasingly marginal
/// for N up to ~1K or so (unless we really expect to have entries
/// for a large proportion of the enum).  Past that, we should probably
/// be falling back on something like a hashtable, because needing tens
/// of kilobytes to hold as few as 17 entries is objectively unreasonable.
template <size_t N>
class FlatMap {
public:
  using IndexType = typename SufficientIntFor<N>::type;
  using StoredIndexType = typename SufficientIntFor<N + 1>::type;

private:
  StoredIndexType *ptr;

public:
  FlatMap() : ptr(new StoredIndexType[N]) {
    memset(ptr, 0, N * sizeof(StoredIndexType));
  }
  FlatMap(FlatMap &&other)
      : ptr(other.ptr) {
    other.ptr = nullptr;
  }
  FlatMap &operator=(FlatMap &&other) {
    delete ptr;
    ptr = other.ptr;
    other.ptr = nullptr;
  }
  FlatMap(const FlatMap &other)
      : ptr(new StoredIndexType[N]) {
    memcpy(ptr, other.ptr, N * sizeof(StoredIndexType));
  }
  FlatMap &operator=(const FlatMap &other) {
    memcpy(ptr, other.ptr, N * sizeof(StoredIndexType));
  }

  ~FlatMap() {
    delete ptr;
  }

  std::pair<IndexType, bool> insert(IndexType key, IndexType value) {
    assert(key < N);
    StoredIndexType &entry = ptr[key];
    if (entry == 0) {
      entry = StoredIndexType(value) + 1;
      return std::make_pair(value, true);
    } else {
      return std::make_pair(IndexType(entry - 1), false);
    }
  }

  std::optional<IndexType> find(IndexType key) const {
    assert(key < N);
    StoredIndexType entry = ptr[key];
    if (entry == 0) {
      return std::nullopt;
    } else {
      return IndexType(entry - 1);
    }
  };
};

} // end namespace EnumMapImpl

/// The default implementation of EnumMapTraits.
template <class Key_, class KeyTraits_>
struct EnumMapTraits {
  using Key = Key_;
  using KeyTraits = KeyTraits_;

  using LargeMapStorage = EnumMapImpl::FlatMap<KeyTraits::NumValues>;
  using IndexType = typename LargeMapStorage::IndexType;
};

} // end namespace swift

#endif
