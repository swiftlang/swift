//===--- MetadataCache.h - Implements the metadata cache -------*- C++ -*--===//
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

#include "llvm/ADT/Hashing.h"
#include "swift/Runtime/Concurrent.h"
#include <mutex>
#include <condition_variable>

#ifndef SWIFT_DEBUG_RUNTIME
#define SWIFT_DEBUG_RUNTIME 0
#endif

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

  EntryRef(const void * const *args, unsigned length) :
    args(args), length(length) {}

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

  bool operator==(EntryRef<Entry>& rhs) const {
    // Compare the sizes.
    unsigned asize = size(), bsize = rhs.size();
    if (asize != bsize) return false;

    // Compare the content.
    auto abegin = begin(), bbegin = rhs.begin();
    for (unsigned i = 0; i < asize; ++i)
      if (abegin[i] != bbegin[i]) return false;
    return true;
  }

  size_t hash() {
    size_t H = 0x56ba80d1 ^ length ;
    for (unsigned i = 0; i < length; i++) {
      H = (H >> 10) | (H << ((sizeof(size_t) * 8) - 10));
      H ^= ((size_t)args[i]) ^ ((size_t)args[i] >> 19);
    }
    return H * 0x27d4eb2d;
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

  /// This pair ties an EntryRef Key and an Entry Value.
  struct EntryPair {
    EntryPair(EntryRef<Entry> K, Entry* V) : Key(K), Value(V) {}
    EntryRef<Entry> Key;
    Entry* Value;
  };

  /// This collection maps hash codes to a list of entry pairs.
  typedef ConcurrentMap<size_t, EntryPair> MDMapTy;

  /// This map hash codes of entry refs to a list of entry pairs.
  MDMapTy *Map;

  /// Synchronization of metadata creation.
  std::mutex *Lock;

  /// The head of a linked list connecting all the metadata cache entries.
  /// TODO: Remove this when LLDB is able to understand the final data
  /// structure for the metadata cache.
  const Entry *Head;

public:
  MetadataCache() : Map(new MDMapTy()), Lock(new std::mutex()) {}
  ~MetadataCache() { delete Map; delete Lock; }

  /// Caches are not copyable.
  MetadataCache(const MetadataCache &other) = delete;
  MetadataCache &operator=(const MetadataCache &other) = delete;

  /// Call entryBuilder() and add the generated metadata to the cache.
  /// \p key is the key used by the cache and \p Bucket is the cache
  /// entry to place the new metadata entry.
  /// This method is marked as 'noinline' because it is infrequently executed
  /// and marking it as such generates better code that is easier to analyze
  /// and profile.
  __attribute__ ((noinline))
  const Entry *addMetadataEntry(EntryRef<Entry> key,
                                ConcurrentList<EntryPair> &Bucket,
                                llvm::function_ref<Entry *()> entryBuilder) {
    // Hold a lock to prevent the modification of the cache by multiple threads.
    std::unique_lock<std::mutex> ConstructionGuard(*Lock);

    // Some other thread may have setup the value we are about to construct
    // while we were asleep so do a search before constructing a new value.
    for (auto &A : Bucket) {
      if (A.Key == key) return A.Value;
    }

    // Build the new cache entry.
    // For some cache types this call may re-entrantly perform additional
    // cache lookups.
    // Notice that the entry is completly constructed before it is inserted
    // into the map, and that only one entry can be constructed at once
    // because of the lock above.
    Entry *entry = entryBuilder();
    assert(entry);

    // Update the linked list.
    entry->Next = Head;
    Head = entry;

    key = EntryRef<Entry>::forEntry(entry, entry->getNumArguments());
    Bucket.push_front(EntryPair(key, entry));

#if SWIFT_DEBUG_RUNTIME
    printf("%s(%p): created %p\n",
           Entry::getName(), this, entry);
#endif
    return key.getEntry();
  }

  /// Look up a cached metadata entry. If a cache match exists, return it.
  /// Otherwise, call entryBuilder() and add that to the cache.
  const Entry *findOrAdd(const void * const *arguments, size_t numArguments,
                         llvm::function_ref<Entry *()> entryBuilder) {

#if SWIFT_DEBUG_RUNTIME
    printf("%s(%p): looking for entry with %zu arguments:\n",
           Entry::getName(), this, numArguments);
    for (size_t i = 0; i < numArguments; i++) {
      printf("%s(%p):     %p\n", Entry::getName(), this, arguments[i]);
    }
#endif

    EntryRef<Entry> key = EntryRef<Entry>::forArguments(arguments,numArguments);
    size_t hash = key.hash();

#if SWIFT_DEBUG_RUNTIME
    printf("%s(%p): generated hash %llx\n",
           Entry::getName(), this, hash);
#endif

    // Look for an existing entry.
    // Find the bucket for the metadata entry.
    ConcurrentList<EntryPair> &Bucket = Map->findOrAllocateNode(hash);
    // Scan the bucket and return the value if we found the key.
    for (auto &A : Bucket) {
      if (A.Key == key) return A.Value;
    }

    // We did not find a key so we will need to create one and store it.
    return addMetadataEntry(key, Bucket, entryBuilder);
  }
};

#endif // SWIFT_RUNTIME_METADATACACHE_H
