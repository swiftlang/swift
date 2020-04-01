//===--- MultiMapCache.h --------------------------------------------------===//
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

#ifndef SWIFT_BASIC_MULTIMAPCACHE_H
#define SWIFT_BASIC_MULTIMAPCACHE_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

/// A CRTP write-once multi-map cache that can be small. It uses a DenseMap
/// internally, so it can be used as a cache without needing to be frozen like
/// FrozenMultiMap (which uses only a vector internally). The Impl class
/// implements the method constructValuesForKey that is used to compute the
/// actual cache value.
///
/// NOTE: constructValuesForKeys is assumed to take a KeyTy and a
/// SmallVectorImpl<ValueTy>. It must append all results to that accumulator and
/// not read any contents of the accumulator.
///
/// NOTE: We store the (size, length) of each ArrayRef<ValueTy> instead of
/// storing the ArrayRef to avoid data invalidation issues caused by SmallVector
/// switching from small to large representations.
///
/// For an example of a subclass implementation see:
/// unittests/Basic/MultiMapCacheTest.cpp.
template <typename ImplType, typename KeyTy, typename ValueTy,
          typename MapTy =
              llvm::DenseMap<KeyTy, Optional<std::tuple<unsigned, unsigned>>>,
          typename VectorTy = std::vector<ValueTy>,
          typename VectorTyImpl = VectorTy>
class MultiMapCache {
  MapTy valueToDataOffsetIndexMap;
  VectorTy data;

  constexpr static unsigned ArrayStartOffset = 0;
  constexpr static unsigned ArrayLengthOffset = 1;

  constexpr ImplType &asImpl() const {
    auto *self = const_cast<MultiMapCache *>(this);
    return reinterpret_cast<ImplType &>(*self);
  }

public:
  void clear() {
    valueToDataOffsetIndexMap.clear();
    data.clear();
  }

  bool empty() const { return valueToDataOffsetIndexMap.empty(); }
  unsigned size() const { return valueToDataOffsetIndexMap.size(); }

  Optional<ArrayRef<ValueTy>> get(const KeyTy &key) {
    auto iter = valueToDataOffsetIndexMap.try_emplace(key, None);

    // If we already have a cached value, just return the cached value.
    if (!iter.second) {
      return iter.first->second.map(
          [&](std::tuple<unsigned, unsigned> startLengthRange) {
            return llvm::makeArrayRef(data).slice(
                std::get<ArrayStartOffset>(startLengthRange),
                std::get<ArrayLengthOffset>(startLengthRange));
          });
    }

    // Otherwise, try to compute the value. If we failed conservatively, return
    // None. The iter value already had None by default set to it, this just
    // makes it so that we do not need to have a memory dependency and can just
    // exit.
    unsigned initialOffset = data.size();

    // We assume that constructValuesForKey /only/ inserts to the end of data
    // and does not inspect any other values in the data array.
    if (!asImpl().constructValuesForKey(key, data)) {
      return None;
    }

    // Otherwise, compute our our length, compute our initial ArrayRef<ValueTy>,
    // update the map with the start, length, and return the resulting ArrayRef.
    unsigned length = data.size() - initialOffset;
    iter.first->second = std::make_tuple(initialOffset, length);
    auto result = llvm::makeArrayRef(data).slice(initialOffset, length);
    return result;
  }
};

template <typename ImplType, typename KeyTy, typename ValueTy>
using SmallMultiMapCache = MultiMapCache<
    ImplType, KeyTy, ValueTy,
    llvm::SmallDenseMap<KeyTy, Optional<std::tuple<unsigned, unsigned>>, 8>,
    SmallVector<ValueTy, 32>, SmallVectorImpl<ValueTy>>;

} // namespace swift

#endif
