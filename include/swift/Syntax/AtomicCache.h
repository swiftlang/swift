//===------------- AtomicCache.h - Lazy Atomic Cache ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_ATOMICCACHE_H
#define SWIFT_SYNTAX_ATOMICCACHE_H

#include <functional>
#include "swift/Syntax/References.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

/// AtomicCache is an atomic cache for a reference-counted value. It maintains
/// a reference-counted pointer with a facility for atomically getting or
/// creating it with a lambda.
template <typename T>
class AtomicCache {
  // Whatever type is created through this cache must be pointer-sized,
  // othwerise, we can't pretend it's a uintptr_t and use its
  // compare_exchange_strong.
  static_assert(sizeof(uintptr_t) == sizeof(RC<T>),
                "RC<T> must be pointer sized!");
private:
  /// This must only be mutated in one place: AtomicCache::getOrCreate.
  mutable RC<T> Storage = nullptr;

public:
  /// The empty constructor initializes the storage to nullptr.
  AtomicCache() {}

  /// Gets the value inside the cache, or creates it atomically using the
  /// provided lambda if it doesn't already exist.
  RC<T> getOrCreate(llvm::function_ref<RC<T>()> Create) const {
    auto &Ptr = *reinterpret_cast<std::atomic<uintptr_t> *>(&Storage);

    // If an atomic load gets an initialized value, then return Storage.
    if (Ptr) {
      return Storage;
    }

    // We expect the uncached value to wrap a nullptr. If another thread
    // beats us to caching the child, it'll be non-null, so we would
    // leave it alone.
    uintptr_t Expected = 0;

    // Make a RC<T> at RefCount == 1, which we'll try to
    // atomically swap in.
    auto Data = Create();

    // Try to swap in raw pointer value.
    // If we won, then leave the RefCount == 1.
    if (Ptr.compare_exchange_strong(Expected,
          reinterpret_cast<uintptr_t>(Data.get()))) {
      Data.resetWithoutRelease();
    }

    // Otherwise, the Data we just made is unfortunately useless.
    // Let it die on this scope exit after its terminal release.

    return Storage;
  }
};

} // end namespace swift

#endif /* SWIFT_SYNTAX_ATOMICCACHE_H */
