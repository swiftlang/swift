///===--- ObjectCache.h -------------------------------------*- C++ -*--===///
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2026 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See https:///swift.org/LICENSE.txt for license information
/// See https:///swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===------------------------------------------------------------------===///
///
/// Implements an object cache; the idea here is that you can ask for an
/// instance from the cache, and you'll be handed an already-allocated
/// instance if there is one, otherwise the cache will allocate for you.
///
///===------------------------------------------------------------------===///

#ifndef SWIFT_OBJECT_CACHE_H
#define SWIFT_OBJECT_CACHE_H

#include "swift/Runtime/Heap.h"
#include "swift/Threading/Mutex.h"

#include <atomic>
#include <new>

namespace swift {

/// An object cache that lets us avoid allocation overhead for objects
/// that might be frequently created and destroyed.
template <class T, unsigned maxCount=1024>
class ObjectCache {
  Mutex lock;

  struct cache_elt {
    cache_elt *next;
    T         object;
  };

  cache_elt *head;
  unsigned   count;

public:
  ObjectCache() : lock(), head(nullptr), count(0) {}

  /// Allocate a new object of type T, returning a pointer.
  ///
  /// This method will return an object from the cache, if one exists,
  /// otherwise it will allocate a new object.
  template <typename... Args>
  T *allocate(Args... args) {
    cache_elt *elt;

    lock.lock();
    elt = head;
    if (elt) {
      head = elt->next;
      --count;
    }
    lock.unlock();

    if (!elt)
      elt = (cache_elt *)swift_slowAlloc(sizeof(cache_elt), 0);
    
    ::new (&elt->object) T(args...);

    return &elt->object;
  }

  /// Hand back a previously allocated object of type T.
  ///
  /// This method may release the object if the cache is already full.
  void put_back(T *object) {
    object->~T();

    struct cache_elt *elt = (struct cache_elt *)((char *)object
       - offsetof(cache_elt, object));

    lock.lock();
    if (count > maxCount) {
      lock.unlock();
      swift_slowDealloc(elt, sizeof(cache_elt), 0);
    } else {
      ++count;
      elt->next = head;
      head = elt;
      lock.unlock();
    }
  }
};

/// A sharded object cache is split into multiple shards to try to
/// decrease contention.
template <class T, unsigned maxCount=1024, unsigned numShards=16>
class ShardedObjectCache {
  std::atomic<unsigned> nextShard;

  unsigned shard() {
    return nextShard.fetch_add(1, std::memory_order_relaxed) % numShards;
  }

  ObjectCache<T, maxCount> cache[numShards];

public:
  /// Allocate a new object of type T, returning a pointer.
  ///
  /// This method will return an object from the cache, if one exists,
  /// otherwise it will allocate a new object.
  template <typename... Args>
  T *allocate(Args... args) {
    return cache[shard()].allocate(args...);
  }

  /// Hand back a previously allocated object of type T.
  ///
  /// This method may release the object if the cache is already full.
  void put_back(T *object) {
    cache[shard()].put_back(object);
  }
};

}

#endif // SWIFT_OBJECT_CACHE_H
