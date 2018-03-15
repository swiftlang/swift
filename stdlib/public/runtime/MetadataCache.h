//===--- MetadataCache.h - Implements the metadata cache --------*- C++ -*-===//
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
#ifndef SWIFT_RUNTIME_METADATACACHE_H
#define SWIFT_RUNTIME_METADATACACHE_H

#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include <condition_variable>
#include <thread>

#ifndef SWIFT_DEBUG_RUNTIME
#define SWIFT_DEBUG_RUNTIME 0
#endif

namespace swift {

class MetadataAllocator : public llvm::AllocatorBase<MetadataAllocator> {
public:
  void Reset() {}

  LLVM_ATTRIBUTE_RETURNS_NONNULL void *Allocate(size_t size, size_t alignment);
  using AllocatorBase<MetadataAllocator>::Allocate;

  void Deallocate(const void *Ptr, size_t size);
  using AllocatorBase<MetadataAllocator>::Deallocate;

  void PrintStats() const {}
};

/// A typedef for simple global caches.
template <class EntryTy>
using SimpleGlobalCache =
  ConcurrentMap<EntryTy, /*destructor*/ false, MetadataAllocator>;

/// A map for which there is a phase of initialization that is guaranteed
/// to be performed exclusively.
///
/// In addition to the requirements of ConcurrentMap, the entry type must
/// provide the following members:
///
///   /// A type which provides a contextual operator bool; this is used
///   /// by the data structure when calling checkStatus to test whether
///   /// initialization has completed.
///   using StatusType = ...;
///
///   /// Check the current status of the entry.  If 'locked' is true,
///   /// there are no competing operations to modify the entry.
///   StatusType checkStatus(bool locked, ArgTys...)
template <class EntryType, bool ProvideDestructor = true>
class LockingConcurrentMap {
  ConcurrentMap<EntryType, /*destructor*/ false, MetadataAllocator> Map;

  struct ConcurrencyControl {
    Mutex Lock;
    ConditionVariable Queue;
  };

  using StatusType = typename EntryType::StatusType;

  ConcurrencyControl *Concurrency;
public:
  LockingConcurrentMap() : Concurrency(new ConcurrencyControl()) {}

  MetadataAllocator &getAllocator() { return Map.getAllocator(); }

  template <class KeyType, class... ExtraArgTys>
  std::pair<EntryType*, StatusType>
  getOrInsert(KeyType key, ExtraArgTys ...extraArgs) {
    auto result = Map.getOrInsert(key, extraArgs...);
    auto entry = result.first;
    auto concurrency = Concurrency;

    // If we are not inserting the entry, we need to check whether the entry
    // currently satisfies our conditions.
    if (!result.second) {
      // Check status.
      StatusType status = entry->checkStatus(false, extraArgs...);
      if (status) {
        return { entry, std::move(status) };
      }

      concurrency->Lock.withLockOrWait(concurrency->Queue, [&] {
        status = entry->checkStatus(true, extraArgs...);

        // If the status is ever satisfied, we can stop waiting.
        if (status) {
          return true;
        }

        return false;
      });

      return { entry, std::move(status) };
    }

    // Initialize the entry.
    StatusType status = entry->initialize(extraArgs...);

    // Notify anyone who might be waiting.
    concurrency->Lock.withLockThenNotifyAll(concurrency->Queue, [&] {
      entry->flagInitializedStatus(status);
    });

    return { entry, std::move(status) };
  }
};

// A wrapper around a pointer to a metadata cache entry that provides
// DenseMap semantics that compare values in the key vector for the metadata
// instance.
//
// This is stored as a pointer to the arguments buffer, so that we can save
// an offset while looking for the matching argument given a key.
class KeyDataRef {
  const void * const *Args;
  unsigned Length;

  KeyDataRef(const void * const *args, unsigned length)
    : Args(args), Length(length) {}

public:
  static KeyDataRef forArguments(const void * const *args,
                                 unsigned numArguments) {
    return KeyDataRef(args, numArguments);
  }

  bool operator==(KeyDataRef rhs) const {
    // Compare the sizes.
    unsigned asize = size(), bsize = rhs.size();
    if (asize != bsize) return false;

    // Compare the content.
    auto abegin = begin(), bbegin = rhs.begin();
    for (unsigned i = 0; i < asize; ++i)
      if (abegin[i] != bbegin[i]) return false;
    return true;
  }

  int compare(KeyDataRef rhs) const {
    // Compare the sizes.
    unsigned asize = size(), bsize = rhs.size();
    if (asize != bsize) {
      return (asize < bsize ? -1 : 1);
    }

    // Compare the content.
    auto abegin = begin(), bbegin = rhs.begin();
    for (unsigned i = 0; i < asize; ++i) {
      if (abegin[i] != bbegin[i])
        return (uintptr_t(abegin[i]) < uintptr_t(bbegin[i]) ? -1 : 1);
    }

    return 0;
  }

  size_t hash() {
    size_t H = 0x56ba80d1 * Length ;
    for (unsigned i = 0; i < Length; i++) {
      H = (H >> 10) | (H << ((sizeof(size_t) * 8) - 10));
      H ^= ((size_t)Args[i]) ^ ((size_t)Args[i] >> 19);
    }
    H *= 0x27d4eb2d;
    return (H >> 10) | (H << ((sizeof(size_t) * 8) - 10));
  }

  const void * const *begin() const { return Args; }
  const void * const *end() const { return Args + Length; }
  unsigned size() const { return Length; }
};

/// A key value as provided to the concurrent map.
struct MetadataCacheKey {
  size_t Hash;
  KeyDataRef KeyData;

  MetadataCacheKey(KeyDataRef data) : Hash(data.hash()), KeyData(data) {}
  MetadataCacheKey(const void *const *data, size_t size)
    : MetadataCacheKey(KeyDataRef::forArguments(data, size)) {}
};

/// A helper class for ConcurrentMap entry types which allows trailing objects
/// objects and automatically implements the getExtraAllocationSize methods
/// in terms of numTrailingObjects calls.
///
/// For each trailing object type T, the subclass must provide:
///   size_t numTrailingObjects(OverloadToken<T>) const;
///   static size_t numTrailingObjects(OverloadToken<T>, ...) const;
/// where the arguments to the latter are the arguments to getOrInsert,
/// including the key.
template <class Impl, class... Objects>
struct ConcurrentMapTrailingObjectsEntry
    : swift::ABI::TrailingObjects<Impl, Objects...> {
protected:
  using TrailingObjects =
      swift::ABI::TrailingObjects<Impl, Objects...>;

  const Impl &asImpl() const { return static_cast<const Impl &>(*this); }

  template<typename T>
  using OverloadToken = typename TrailingObjects::template OverloadToken<T>;

public:
  template <class... Args>
  static size_t getExtraAllocationSize(const MetadataCacheKey &key,
                                       Args &&...args) {
    return TrailingObjects::template additionalSizeToAlloc<Objects...>(
        Impl::numTrailingObjects(OverloadToken<Objects>(), key, args...)...);
  }
  size_t getExtraAllocationSize() const {
    return TrailingObjects::template additionalSizeToAlloc<Objects...>(
        asImpl().numTrailingObjects(OverloadToken<Objects>())...);
  }
};

/// A base class offerring a reasonable default implementation for entries
/// in a generic metadata cache.  Supports variably-sized keys.
///
/// The value type may be an arbitrary type, but it must be contextually
/// convertible to bool, and it must be default-constructible in a false
/// state.
///
/// Concrete implementations should provide:
///   /// A name describing the map; used in debugging diagnostics.
///   static const char *getName();
///
///   /// A constructor which should set up an entry.  Note that this phase
///   /// of initialization may race with other threads attempting to set up
///   /// the same entry; do not do anything during it which might block or
///   /// need to be reverted.
///   /// The extra arguments are those provided to getOrInsert.
///   Entry(MetadataCacheKey key, ExtraArgTys...);
///
///   /// As described in LockingConcurrentMap.
///   static ValueType initialize(ExtraArgTys...);
template <class Impl, class ValueType_, class... Objects>
class MetadataCacheEntryBase
    : public ConcurrentMapTrailingObjectsEntry<Impl, const void *, Objects...> {
  using super =
             ConcurrentMapTrailingObjectsEntry<Impl, const void *, Objects...>;
  friend super;
  using TrailingObjects = typename super::TrailingObjects;
  friend TrailingObjects;

public:
  using ValueType = ValueType_;
  using StatusType = ValueType;

protected:
  template<typename T>
  using OverloadToken = typename TrailingObjects::template OverloadToken<T>;

  size_t numTrailingObjects(OverloadToken<const void *>) const {
    return KeyLength;
  }

  template <class... Args>
  static size_t numTrailingObjects(OverloadToken<const void *>,
                                   const MetadataCacheKey &key,
                                   Args &&...extraArgs) {
    return key.KeyData.size();
  }

  using super::asImpl;

private:
  size_t Hash;
  unsigned KeyLength;

  /// Does this entry have a value, or is it currently undergoing
  /// initialization?
  ///
  /// This (and the following field) is ever modified under the lock,
  /// but it can be read from any thread, including while the lock
  /// is held.
  std::atomic<bool> HasValue;
  union {
    ValueType Value;
    std::thread::id InitializingThread;
  };

public:
  MetadataCacheEntryBase(const MetadataCacheKey &key)
      : Hash(key.Hash), KeyLength(key.KeyData.size()), HasValue(false) {
    InitializingThread = std::this_thread::get_id();
    memcpy(this->template getTrailingObjects<const void*>(),
           key.KeyData.begin(),
           KeyLength * sizeof(void*));
  }

  bool isBeingInitializedByCurrentThread() const {
    return InitializingThread == std::this_thread::get_id();
  }

  KeyDataRef getKeyData() const {
    return KeyDataRef::forArguments(
                              this->template getTrailingObjects<const void*>(),
                                    KeyLength);
  }

  intptr_t getKeyIntValueForDump() const {
    return Hash;
  }

  int compareWithKey(const MetadataCacheKey &key) const {
    // Order by hash first, then by the actual key data.
    if (auto comparison = compareIntegers(key.Hash, Hash)) {
      return comparison;
    } else {
      return key.KeyData.compare(getKeyData());
    }
  }

  template <class... Args>
  ValueType checkStatus(bool locked, Args &&...extraArgs) const {
    if (HasValue.load(std::memory_order_acquire)) {
      return Value;
    }

    // As a QoI safe-guard against the simplest form of cyclic
    // dependency, check whether this thread is the one responsible
    // for initializing the metadata.
    if (locked && isBeingInitializedByCurrentThread()) {
      fprintf(stderr,
              "%s(%p): cyclic metadata dependency detected, aborting\n",
              Impl::getName(), static_cast<const void*>(this));
      abort();
    }

    return ValueType();
  }

  void flagInitializedStatus(ValueType value) {
    Value = value;
    HasValue.store(true, std::memory_order_release);
  }
};

} // namespace swift

#endif // SWIFT_RUNTIME_METADATACACHE_H
