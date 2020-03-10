//===--- FrozenMultiMap.h ----------------------------------*- C++ --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// A 2 stage multi-map. Initially the multimap is mutable and can only be
/// initialized. Once complete, the map is frozen and can be only used for map
/// operations. It is guaranteed that all values are still in insertion order.
///
/// DISCUSSION: These restrictions flow from the internal implementation of the
/// multi-map being a pair of keys, values. We form the map property by
/// performing a stable_sort of the (key, value) in the process of freezing the
/// map.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_FROZENMULTIMAP_H
#define SWIFT_BASIC_FROZENMULTIMAP_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include <vector>

namespace swift {

template <typename Key, typename Value,
          typename VectorStorage = std::vector<std::pair<Key, Value>>>
class FrozenMultiMap {
  VectorStorage storage;
  bool frozen = false;

private:
  struct PairToSecondElt;

public:
  using PairToSecondEltRange =
      TransformRange<ArrayRef<std::pair<Key, Value>>, PairToSecondElt>;

  FrozenMultiMap() = default;

  void insert(const Key &key, const Value &value) {
    assert(!isFrozen() && "Can not insert new keys once map is frozen");
    storage.emplace_back(key, value);
  }

  Optional<PairToSecondEltRange> find(const Key &key) const {
    assert(isFrozen() &&
           "Can not perform a find operation until the map is frozen");
    // Since our array is sorted, we need to first find the first pair with our
    // inst as the first element.
    auto start = std::lower_bound(
        storage.begin(), storage.end(), std::make_pair(key, Value()),
        [&](const std::pair<Key, Value> &p1, const std::pair<Key, Value> &p2) {
          return p1.first < p2.first;
        });
    if (start == storage.end() || start->first != key) {
      return None;
    }

    // Ok, we found our first element. Now scan forward until we find a pair
    // whose instruction is not our own instruction.
    auto end = find_if_not(
        start, storage.end(),
        [&](const std::pair<Key, Value> &pair) { return pair.first == key; });
    unsigned count = std::distance(start, end);
    ArrayRef<std::pair<Key, Value>> slice(&*start, count);
    return PairToSecondEltRange(slice, PairToSecondElt());
  }

  bool isFrozen() const { return frozen; }

  /// Set this map into its frozen state when we
  void setFrozen() {
    std::stable_sort(storage.begin(), storage.end(),
                     [&](const std::pair<Key, Value> &lhs,
                         const std::pair<Key, Value> &rhs) {
                       // Only compare the first entry so that we preserve
                       // insertion order.
                       return lhs.first < rhs.first;
                     });
    frozen = true;
  }

  unsigned size() const { return storage.size(); }
  bool empty() const { return storage.empty(); }

  struct iterator : std::iterator<std::forward_iterator_tag,
                                  std::pair<Key, ArrayRef<Value>>> {
    using base_iterator = typename decltype(storage)::iterator;

    FrozenMultiMap &map;
    base_iterator baseIter;
    Optional<std::pair<Key, PairToSecondEltRange>> currentValue;

    iterator(FrozenMultiMap &map, base_iterator iter)
        : map(map), baseIter(iter), currentValue() {
      updateCurrentValue();
    }

    void updateCurrentValue() {
      base_iterator end = map.storage.end();

      // If we are end, set currentValue to be None.
      if (baseIter == end) {
        currentValue = None;
        return;
      }

      // Otherwise, determine the next range that we are visiting.
      auto rangeEnd = std::find_if_not(std::next(baseIter), end,
                                       [&](const std::pair<Key, Value> &elt) {
                                         return elt.first == baseIter->first;
                                       });
      unsigned count = std::distance(baseIter, rangeEnd);
      ArrayRef<std::pair<Key, Value>> slice(&*baseIter, count);
      currentValue = {baseIter->first,
                      PairToSecondEltRange(slice, PairToSecondElt())};
    }

    iterator &operator++() {
      baseIter = std::find_if_not(std::next(baseIter), map.storage.end(),
                                  [&](const std::pair<Key, Value> &elt) {
                                    return elt.first == baseIter->first;
                                  });
      updateCurrentValue();
      return *this;
    }

    iterator operator++(int) {
      auto tmp = *this;
      baseIter = std::find_if_not(std::next(baseIter), map.storage.end(),
                                  [&](const std::pair<Key, Value> &elt) {
                                    return elt.first == baseIter->first;
                                  });
      updateCurrentValue();
      return tmp;
    }

    std::pair<Key, PairToSecondEltRange> operator*() const {
      return *currentValue;
    }

    bool operator==(const iterator &RHS) const {
      return baseIter == RHS.baseIter;
    }

    bool operator!=(const iterator &RHS) const {
      return baseIter != RHS.baseIter;
    }
  };

  /// Return a range of (key, ArrayRef<Value>) pairs. The keys are guaranteed to
  /// be in key sorted order and the ArrayRef<Value> are in insertion order.
  llvm::iterator_range<iterator> getRange() const {
    assert(isFrozen() &&
           "Can not create range until data structure is frozen?!");
    auto *self = const_cast<FrozenMultiMap *>(this);
    iterator iter1 = iterator(*self, self->storage.begin());
    iterator iter2 = iterator(*self, self->storage.end());
    return llvm::make_range(iter1, iter2);
  }
};

template <typename Key, typename Value, typename Storage>
struct FrozenMultiMap<Key, Value, Storage>::PairToSecondElt {
  PairToSecondElt() {}

  Value operator()(const std::pair<Key, Value> &pair) const {
    return pair.second;
  }
};

template <typename Key, typename Value, unsigned SmallSize>
using SmallFrozenMultiMap =
    FrozenMultiMap<Key, Value, SmallVector<std::pair<Key, Value>, SmallSize>>;

} // namespace swift

#endif
