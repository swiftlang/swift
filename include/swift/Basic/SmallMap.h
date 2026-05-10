//===--- SmallMap.h - A map optimized for having few entries ----*- C++ -*-===//
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
///  This file defines the SmallMap data structure, which is optimized for
///  a small number of keys.  The values of the map are stored in a dynamic
///  array (such a SmallVector).  Iterating the map iterates these values
///  in insertion order.  If the number of entries is small (not more than
///  the "direct search limit"), the keys are stored in an inline array
///  that is parallel to the elements array, and lookups brute-force search
///  this array for the key and then use the element with the same index.  If
///  the number of entries grows beyond that limit, the map fall back to a
///  "large" map of keys to indexes, which defaults to a DenseMap<Key, size_t>.
///
///  There are currently no operations to remove elements.
///  Iterators are invalidated by insertion.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_SMALLMAP_H
#define SWIFT_BASIC_SMALLMAP_H

#include "swift/Basic/Range.h"
#include "swift/Basic/UninitializedArray.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include <optional>
#include <type_traits>
#include <utility>

namespace swift {

/// The maximum number of elements that the map can have before
/// it flips from brute-force searching the keys to using the
/// large map structure.
static constexpr size_t DefaultSmallMapDirectSearchLimit = 16;

/// The primary customization point for a SmallMap.
///
/// template <>
/// struct SmallMapTraits<MyKey> {
///   using IndexType = <some integer type>;
///   struct LargeMapStorage {
///     std::optional<IndexType> find(const MyKey &key) const;
///     std::pair<IndexType, bool> insert(MyKey &&key, IndexType value);
///   };
/// };
template <class Key>
struct SmallMapTraits;

template <class Key, class Value,
          size_t DirectSearchLimit = DefaultSmallMapDirectSearchLimit,
          class MapTraits = SmallMapTraits<Key>,
          class ElementStorage = llvm::SmallVector<Value>>
class SmallMap {
  using IndexType = typename MapTraits::IndexType;
  using LargeMapStorage = typename MapTraits::LargeMapStorage;
  using SmallMapStorage = UninitializedArray<Key, DirectSearchLimit>;

  static_assert(std::is_integral_v<IndexType>,
                "index type must be an integer type");

  ElementStorage elements;

  union {
    LargeMapStorage largeMap;
    SmallMapStorage smallMap;
  };

  bool isLarge() const {
    // This only works because there are no operations to remove entries.
    return elements.size() > DirectSearchLimit;
  }

  template <class... Args>
  void initializeLargeMap(Args &&...args) {
    ::new ((void*) &largeMap) LargeMapStorage(std::forward<Args>(args)...);
  }
  void destroyLargeMap() {
    largeMap.~LargeMapStorage();
  }

  void initializeSmallMap() {
    ::new ((void*) &smallMap) SmallMapStorage();
  }
  void destroySmallMap(size_t numElements) {
    smallMap.destroy(numElements);
    smallMap.~SmallMapStorage();
  }

public:
  SmallMap() {
    initializeSmallMap();
  }

  SmallMap(SmallMap &&other)
      : elements(std::move(other.elements)) {

    // Make sure that the other object has an element count of zero.
    other.elements.clear();
    assert(!other.isLarge());

    auto newSize = size();
    bool newIsLarge = isLarge();

    // Destructively move the other object's map storage to this object.
    // Postcondition: the other object's map storage is in the small
    // map state with zero initialized objects.
    if (newIsLarge) {
      initializeLargeMap(std::move(other.largeMap));
      other.destroyLargeMap();
      other.initializeSmallMap();
    } else {
      initializeSmallMap();
      smallMap.destructiveMoveInitialize(std::move(other.smallMap), newSize);
    }
  }

  SmallMap(const SmallMap &other)
      : elements(other.elements) {
    auto newSize = size();
    bool newIsLarge = isLarge();
    if (newIsLarge) {
      initializeLargeMap(other.largeMap);
    } else {
      initializeSmallMap();
      smallMap.copyInitialize(other.smallMap, newSize);
    }
  }

  SmallMap &operator=(SmallMap &&other) {
    size_t oldSize = size();
    bool oldIsLarge = isLarge();
    elements = std::move(other.elements);
    size_t newSize = size();
    bool newIsLarge = isLarge();

    // Make sure that the other object has an element count of zero.
    other.elements.clear();
    assert(!other.isLarge());

    // Move the other object's map storage to this object.
    // Postcondition: the other object's map storage is in the small
    // map state with zero initialized objects.

    // large -> large
    if (oldIsLarge && newIsLarge) {
      largeMap = std::move(other.largeMap);
      other.destroyLargeMap();
      other.initializeSmallMap();

    // large -> small
    } else if (oldIsLarge) {
      destroyLargeMap();
      initializeSmallMap();
      smallMap.destructiveMoveInitialize(std::move(other.smallMap), newSize);

    // small -> large
    } else if (newIsLarge) {
      destroySmallMap(oldSize);
      initializeLargeMap(std::move(other.largeMap));
      other.destroyLargeMap();
      other.initializeSmallMap();

    // small -> small
    } else {
      smallMap.destructiveMoveAssign(std::move(other.smallMap), oldSize, newSize);
    }

    return *this;
  }

  SmallMap &operator=(const SmallMap &other) {
    size_t oldSize = size();
    bool oldIsLarge = isLarge();

    // Copy the other object's elements to this object.
    elements = other.elements;

    size_t newSize = size();
    bool newIsLarge = isLarge();

    // Copy the other object's map storage to this object:

    // large -> large
    if (oldIsLarge && newIsLarge) {
      largeMap = other.largeMap;

    // large -> small
    } else if (oldIsLarge) {
      destroyLargeMap();
      initializeSmallMap();
      smallMap.copyInitialize(other.smallMap, newSize);

    // small -> large
    } else if (newIsLarge) {
      destroySmallMap(oldSize);
      initializeLargeMap(other.largeMap);

    // small -> small
    } else {
      smallMap.copyAssign(other.smallMap, oldSize, newSize);
    }

    return *this;
  }

  ~SmallMap() {
    if (isLarge()) {
      destroyLargeMap();
    } else {
      destroySmallMap(size());
    }
  }

  bool empty() const { return elements.empty(); }
  size_t size() const { return elements.size(); }

  using iterator = typename ElementStorage::iterator;
  iterator begin() { return elements.begin(); }
  iterator end() { return elements.end(); }

  using const_iterator = typename ElementStorage::const_iterator;
  const_iterator begin() const { return elements.begin(); }
  const_iterator end() const { return elements.end(); }

  /// Look up a key in the map.  Returns end() if the entry is not found.
  const_iterator find(const Key &key) const {
    if (isLarge()) {
      std::optional<IndexType> result = largeMap.find(key);
      if (result)
        return elements.begin() + *result;
      return elements.end();
    }

    size_t n = elements.size();
    for (size_t i : range(n))
      if (smallMap[i] == key)
        return elements.begin() + i;

    return elements.end();
  }

  /// Try to insert the given key/value pair.  If there's already an element
  /// with this key, return false and an iterator for the existing element.
  /// Otherwise, return true and an iterator for the new element.
  ///
  /// The value in the set will be constructed by emplacing it with the
  /// given arguments.
  template <class KeyT, class... Args>
  std::pair<iterator, bool> insert(KeyT &&key, Args &&...valueArgs) {
    // The current number of elements, and therefore also the index of
    // the new element if we create one.
    auto n = elements.size();

    if (isLarge()) {
      // Try to insert a map entry pointing to the potential new element.
      auto result = largeMap.insert(std::forward<KeyT>(key), n);

      // If we successfully inserted, emplace the new element.
      if (result.second) {
        assert(result.first == n);
        elements.emplace_back(std::forward<Args>(valueArgs)...);
        return {elements.begin() + n, true};
      }

      // Otherwise, return the existing value.
      return {elements.begin() + result.first, false};
    }

    // Search the small map for the key.
    for (size_t i : range(n))
      if (smallMap[i] == key)
        return {elements.begin() + i, false};

    // If that didn't match, we have to insert.  Emplace the new element.
    elements.emplace_back(std::forward<Args>(valueArgs)...);

    // If we aren't crossing the large-map threshold, just emplace the
    // new key.
    if (n < DirectSearchLimit) {
      smallMap.emplace(n, std::forward<KeyT>(key));
      return {elements.begin() + n, true};
    }

    // Otherwise, we need to transition the map from small to large.

    // Move the small map aside.
    assert(isLarge());
    SmallMapStorage smallMapCopy;
    smallMapCopy.destructiveMoveInitialize(std::move(smallMap), n);
    destroySmallMap(0); // formally end lifetime

    // Initialize the large map with the existing mappings taken from
    // the moved-aside small map.
    initializeLargeMap();
    for (size_t i : range(n)) {
      auto result = largeMap.insert(std::move(smallMapCopy[i]), i);
      assert(result.second && result.first == i); (void) result;
    }

    // Add the new mapping.
    auto result = largeMap.insert(std::forward<KeyT>(key), n);
    assert(result.second && result.first == n); (void) result;

    // Destroy the elements of the copied small map, which we moved
    // into the large map but didn't *destructively* move.
    smallMapCopy.destroy(n);

    return {elements.begin() + n, true};
  }
};

namespace SmallMapImpl {

template <class Key>
struct DefaultSmallMapTraits {
  using IndexType = size_t;

  struct LargeMapStorage {
    llvm::DenseMap<Key, IndexType> map;

    std::optional<IndexType> find(const Key &key) const {
      auto it = map.find(key);
      if (it == map.end()) return std::nullopt;
      return it->second;
    }

    template <class KeyArg>
    std::pair<IndexType, bool> insert(KeyArg &&key, IndexType value) {
      auto result = map.insert(std::make_pair(std::forward<KeyArg>(key), value));
      return std::make_pair(result.first->second, result.second);
    }
  };
};

} // end namespace SmallMapImpl

template <class Key>
struct SmallMapTraits : SmallMapImpl::DefaultSmallMapTraits<Key> {};

} // end namespace swift

#endif
