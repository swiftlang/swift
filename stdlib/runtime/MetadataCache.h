//===--- MetadataCache.h - Implements the metadata cache  -----------------===//
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
#ifndef SWIFT_RUNTIME_METADATACACHE_H
#define SWIFT_RUNTIME_METADATACACHE_H

#include "llvm/ADT/DenseMap.h"
#include "Locks.h"
#include <mutex>

static void *permanentAlloc(size_t size) { return malloc(size); }

// A wrapper around a pointer to a metadata cache entry that provides
// DenseMap semantics that compare values in the key vector for the metadata
// instance.
//
// This is stored as a pointer to the arguments buffer, so that we can save
// an offset while looking for the matching argument given a key.
template<class Entry>
class EntryRef {
  const void * const *args;
  unsigned length;

  EntryRef(const void * const *args, unsigned length)
    : args(args), length(length)
  {}

  friend struct llvm::DenseMapInfo<EntryRef>;
public:
  static EntryRef forEntry(const Entry *e, unsigned numArguments) {
    return EntryRef(e->getArgumentsBuffer(), numArguments);
  }

  static EntryRef forArguments(const void * const *args,
                               unsigned numArguments) {
    return EntryRef(args, numArguments);
  }

  const Entry *getEntry() const {
    return Entry::fromArgumentsBuffer(args, length);
  }

  const void * const *begin() const { return args; }
  const void * const *end() const { return args + length; }
  unsigned size() const { return length; }
};


template <class Impl>
struct CacheEntryHeader {
  /// LLDB walks this list.
  const Impl *Next;
};

/// A CRTP class for defining entries in a metadata cache.
template <class Impl, class Header = CacheEntryHeader<Impl> >
class alignas(void*) CacheEntry : public Header {

  CacheEntry(const CacheEntry &other) = delete;
  void operator=(const CacheEntry &other) = delete;

  Impl *asImpl() { return static_cast<Impl*>(this); }
  const Impl *asImpl() const { return static_cast<const Impl*>(this); }

protected:
  CacheEntry() = default;

public:
  static Impl *allocate(const void * const *arguments,
                        size_t numArguments, size_t payloadSize) {
    void *buffer = permanentAlloc(sizeof(Impl)  +
                                  numArguments * sizeof(void*) +
                                  payloadSize);
    void *resultPtr = (char*)buffer + numArguments * sizeof(void*);
    auto result = new (resultPtr) Impl(numArguments);

    // Copy the arguments into the right place for the key.
    memcpy(buffer, arguments,
           numArguments * sizeof(void*));

    return result;
  }

  void **getArgumentsBuffer() {
    return reinterpret_cast<void**>(this) - asImpl()->getNumArguments();
  }
  void * const *getArgumentsBuffer() const {
    return reinterpret_cast<void * const *>(this)
      - asImpl()->getNumArguments();
  }

  template <class T> T *getData() {
    return reinterpret_cast<T *>(asImpl() + 1);
  }
  template <class T> const T *getData() const {
    return const_cast<CacheEntry*>(this)->getData<T>();
  }

  static const Impl *fromArgumentsBuffer(const void * const *argsBuffer,
                                         unsigned numArguments) {
    return reinterpret_cast<const Impl *>(argsBuffer + numArguments);
  }
};


/// The implementation of a metadata cache.  Note that all-zero must
/// be a valid state for the cache.
template <class Entry> class MetadataCache {
  /// Synchronization.
  ///
  /// Readers: acquire EntriesLock for reading.
  ///
  /// Writers and waiters: Acquire InsertionLock first.
  /// Use InsertionWaiters to wait for another thread to complete an entry.
  /// Acquire EntriesLock for writing second when modifying the Entries table.
  ///
  /// We need a mutex in addition to the rwlock to make the
  /// condition variable work.
  struct MetadataCacheLock {
    RWMutex EntriesLock;
    std::mutex InsertionLock;
    std::condition_variable InsertionWaiters;
  };
  MetadataCacheLock *Lock;

  /// The head of a linked list connecting all the metadata cache entries.
  /// TODO: Remove this when LLDB is able to understand the final data
  /// structure for the metadata cache.
  const Entry *Head;

  enum class EntryState : uint8_t { Complete, Building, BuildingWithWaiters };

  /// The lookup table for cached entries.
  ///
  /// The EntryRef may be to temporary memory lacking a full backing
  /// Entry unless the value is Complete.  However, if there is an
  /// entry with a non-Complete value, there will eventually be a
  /// notification to the lock's wait list.
  ///
  /// TODO: Consider a more tuned hashtable implementation.
  typedef llvm::DenseMap<EntryRef<Entry>, EntryState> EntriesMapType;
  typedef typename EntriesMapType::iterator EntriesIteratorType;
  EntriesMapType Entries;

public:
  MetadataCache() : Lock(new MetadataCacheLock()) {
  }
  ~MetadataCache() { delete Lock; }

  /// Caches are not copyable.
  MetadataCache(const MetadataCache &other) = delete;
  MetadataCache &operator=(const MetadataCache &other) = delete;

  /// Look up a cached metadata entry.
  /// If a cache match exists, return it.
  /// Otherwise, call entryBuilder() and add that to the cache.
  const Entry *findOrAdd(const void * const *arguments,
                         size_t numArguments,
                         std::function<Entry *()> entryBuilder) {

#if SWIFT_DEBUG_RUNTIME
    printf("%s(%p): looking for entry with %zu arguments:\n",
           Entry::getName(), this, numArguments);
    for (size_t i = 0; i < numArguments; i++) {
      printf("%s(%p):     %p\n", Entry::getName(), this, arguments[i]);
    }
#endif

    auto key = EntryRef<Entry>::forArguments(arguments, numArguments);

    // Look for an existing entry.
    {
      ScopedReader readGuard(Lock->EntriesLock);
      auto found = Entries.find(key);
      if (found != Entries.end()  &&  found->second == EntryState::Complete) {
#if SWIFT_DEBUG_RUNTIME
        printf("%s(%p): found %p already in cache\n",
               Entry::getName(), this, found->first.getEntry());
#endif
        return found->first.getEntry();
      }
    }

    // No complete entry found. Insert a new entry or wait for the
    // existing one to complete.

    {
      std::unique_lock<std::mutex> insertionGuard(Lock->InsertionLock);

      // Try to insert a placeholder so other threads know a new entry
      // is under construction.
      std::pair<typename decltype(Entries)::iterator, bool> found;
      {
        ScopedWriter writeGuard(Lock->EntriesLock);
        found = Entries.insert({key, EntryState::Building});
      }
      // We no longer hold EntriesLock but `found` remains valid
      // while we still hold InsertionLock.

      auto it = found.first;
      bool inserted = found.second;

      if (it->second == EntryState::Complete) {
        // Some other thread built the entry already. Return it.
#if SWIFT_DEBUG_RUNTIME
        printf("%s(%p): found %p already in cache after losing writer race\n",
               Entry::getName(), this, it->first.getEntry());
#endif
        return it->first.getEntry();
      }

      if (!inserted) {
        // Some other thread is currently building the entry.
        // Wait for it to complete, then return it.

        // Tell the builder that we're here.
        it->second = EntryState::BuildingWithWaiters;

        // Wait until the entry's state goes to Complete.
        while (it->second != EntryState::Complete) {
          Lock->InsertionWaiters.wait(insertionGuard);

          // We dropped InsertionLock so don't trust the existing iterator.
          it = Entries.find(key);
          assert(it != Entries.end());
        }

#if SWIFT_DEBUG_RUNTIME
        printf("%s(%p): found %p already in cache after waiting\n",
               Entry::getName(), this, it->first.getEntry());
#endif
        return it->first.getEntry();
      }
    }

    // Placeholder insertion successful.

    // Build the new cache entry.
    // For some cache types this call may re-entrantly perform additional
    // cache lookups.
    Entry *entry = entryBuilder();
    assert(entry);

    // Insert the new cache entry.
    bool shouldNotify;
    const Entry *result;
    {
      std::unique_lock<std::mutex> insertionGuard(Lock->InsertionLock);

      // Update the linked list.
      entry->Next = Head;
      Head = entry;

      // Find our placeholder.
      // We hold InsertionLock so nobody can modify the table underneath us.
      auto it = Entries.find(key);
      assert(it != Entries.end());

      // Replace the placeholder entry with the real data.
      {
        // Acquire EntriesLock to keep readers out while we update.
        ScopedWriter writeGuard(Lock->EntriesLock);

        // The existing key is a reference to the (probably stack-based)
        // arguments array, so overwrite it.  Maps don't normally allow
        // their keys to be overwritten, and doing so isn't officially
        // allowed, but supposedly it is unofficially guaranteed to
        // work, at least with the standard containers.
        key = EntryRef<Entry>::forEntry(entry, entry->getNumArguments());
        assert(it == Entries.find(key));
        const_cast<EntryRef<Entry>&>(it->first) = key;

        // Mark the entry as Complete.
        assert(it->second != EntryState::Complete);
        shouldNotify = (it->second == EntryState::BuildingWithWaiters);
        it->second = EntryState::Complete;
        result = it->first.getEntry();
      }
    }

    // Notify any threads that might be waiting for the entry we just built.
    if (shouldNotify) {
      Lock->InsertionWaiters.notify_all();
    }

#if SWIFT_DEBUG_RUNTIME
    printf("%s(%p): created %p %p\n",
           Entry::getName(), this, entry, result);
#endif
    return result;
  }
};

#endif // SWIFT_RUNTIME_METADATACACHE_H
