//===--- Concurrent.h - Concurrent Data Structures  -------------*- C++ -*-===//
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
#ifndef SWIFT_RUNTIME_CONCURRENTUTILS_H
#define SWIFT_RUNTIME_CONCURRENTUTILS_H
#include "Atomic.h"
#include "Debug.h"
#include "swift/Threading/Mutex.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/Allocator.h"
#include <algorithm>
#include <atomic>
#include <functional>
#include <iterator>
#include <stdint.h>
#include <vector>

#if defined(__FreeBSD__) || defined(__CYGWIN__) || defined(__HAIKU__)
#include <stdio.h>
#endif

#if defined(__APPLE__) && defined(__MACH__)
#include <malloc/malloc.h>
#endif

namespace swift {

/// A simple linked list representing pointers that need to be freed. This is
/// not a concurrent data structure, just a bit of support used in the types
/// below.
struct ConcurrentFreeListNode {
  ConcurrentFreeListNode *Next;
  void *Ptr;

  static void add(ConcurrentFreeListNode **head, void *ptr) {
    auto *newNode = reinterpret_cast<ConcurrentFreeListNode *>(
        malloc(sizeof(ConcurrentFreeListNode)));
    newNode->Next = *head;
    newNode->Ptr = ptr;
    *head = newNode;
  }

  /// Free all nodes in the free list, resetting `head` to `NULL`. Calls
  /// `FreeFn` on the Ptr field of every node.
  template <typename FreeFn>
  static void freeAll(ConcurrentFreeListNode **head, const FreeFn &freeFn) {
    auto *node = *head;
    while (node) {
      auto *next = node->Next;
      freeFn(node->Ptr);
      free(node);
      node = next;
    }
    *head = nullptr;
  }
};

/// An append-only array that can be read without taking locks. Writes
/// are still locked and serialized, but only with respect to other
/// writes.
template <class ElemTy> struct ConcurrentReadableArray {
private:
  /// The struct used for the array's storage. The `Elem` member is
  /// considered to be the first element of a variable-length array,
  /// whose size is determined by the allocation. The `Capacity` member
  /// from `ConcurrentReadableArray` indicates how large it can be.
  struct Storage {
    std::atomic<size_t> Count;
    typename std::aligned_storage<sizeof(ElemTy), alignof(ElemTy)>::type Elem;

    static Storage *allocate(size_t capacity) {
      auto size = sizeof(Storage) + (capacity - 1) * sizeof(Storage().Elem);
      auto *ptr = reinterpret_cast<Storage *>(malloc(size));
      if (!ptr) swift::crash("Could not allocate memory.");
      ptr->Count.store(0, std::memory_order_relaxed);
      return ptr;
    }

    void deallocate() {
      for (size_t i = 0; i < Count; ++i) {
        data()[i].~ElemTy();
      }
      free(this);
    }

    ElemTy *data() {
      return reinterpret_cast<ElemTy *>(&Elem);
    }
  };
  
  size_t Capacity;
  std::atomic<size_t> ReaderCount;
  std::atomic<Storage *> Elements;
  Mutex WriterLock;
  ConcurrentFreeListNode *FreeList{nullptr};

  void incrementReaders() {
    ReaderCount.fetch_add(1, std::memory_order_acquire);
  }
  
  void decrementReaders() {
    ReaderCount.fetch_sub(1, std::memory_order_release);
  }
  
  void deallocateFreeList() {
    ConcurrentFreeListNode::freeAll(&FreeList, [](void *ptr) {
      reinterpret_cast<Storage *>(ptr)->deallocate();
    });
  }
  
public:
  struct Snapshot {
    ConcurrentReadableArray *Array;
    const ElemTy *Start;
    size_t Count;
    
    Snapshot(ConcurrentReadableArray *array, const ElemTy *start, size_t count)
      : Array(array), Start(start), Count(count) {}
    
    Snapshot(const Snapshot &other)
      : Array(other.Array), Start(other.Start), Count(other.Count) {
      Array->incrementReaders();
    }
    
    ~Snapshot() {
      Array->decrementReaders();
    }

    // These are marked as ref-qualified (the &) to make sure they can't be
    // called on temporaries, since the temporary would be destroyed before the
    // return value can be used, making it invalid.
    const ElemTy *begin() const& { return Start; }
    const ElemTy *end() const& { return Start + Count; }
    const ElemTy& operator [](size_t index) const& {
      assert(index < count() && "out-of-bounds access to snapshot element");
      return Start[index];
    }

    size_t count() const { return Count; }
  };

  // This type cannot be safely copied or moved.
  ConcurrentReadableArray(const ConcurrentReadableArray &) = delete;
  ConcurrentReadableArray(ConcurrentReadableArray &&) = delete;
  ConcurrentReadableArray &operator=(const ConcurrentReadableArray &) = delete;
  
  ConcurrentReadableArray() : Capacity(0), ReaderCount(0), Elements(nullptr) {}
  
  ~ConcurrentReadableArray() {
    assert(ReaderCount.load(std::memory_order_acquire) == 0 &&
           "deallocating ConcurrentReadableArray with outstanding snapshots");
    deallocateFreeList();
  }
  
  void push_back(const ElemTy &elem) {
    Mutex::ScopedLock guard(WriterLock);

    auto *storage = Elements.load(std::memory_order_relaxed);
    auto count = storage ? storage->Count.load(std::memory_order_relaxed) : 0;
    if (count >= Capacity) {
      auto newCapacity = std::max((size_t)16, count * 2);
      auto *newStorage = Storage::allocate(newCapacity);
      if (storage) {
        std::uninitialized_copy_n(storage->data(), count, newStorage->data());
        newStorage->Count.store(count, std::memory_order_release);
        ConcurrentFreeListNode::add(&FreeList, storage);
      }
      
      storage = newStorage;
      Capacity = newCapacity;

      Elements.store(storage, std::memory_order_release);
    }
    
    new(&storage->data()[count]) ElemTy(elem);
    storage->Count.store(count + 1, std::memory_order_release);
    
    // The standard says that std::memory_order_seq_cst only applies to
    // read-modify-write operations, so we need an explicit fence:
    std::atomic_thread_fence(std::memory_order_seq_cst);
    if (ReaderCount.load(std::memory_order_relaxed) == 0)
      deallocateFreeList();
  }

  Snapshot snapshot() {
    incrementReaders();
    auto *storage = Elements.load(SWIFT_MEMORY_ORDER_CONSUME);
    if (storage == nullptr) {
      return Snapshot(this, nullptr, 0);
    }
    
    auto count = storage->Count.load(std::memory_order_acquire);
    const auto *ptr = storage->data();
    return Snapshot(this, ptr, count);
  }
};

using llvm::hash_value;

/// A hash table that can be queried without taking any locks. Writes are still
/// locked and serialized, but only with respect to other locks. Writers can add
/// elements and clear the table, but they cannot remove individual elements.
/// Readers work by taking a snapshot of the table and then querying that
/// snapshot.
///
/// The basic structure of the table consists of two arrays. Elements are stored
/// in a contiguous array, with new elements appended to the end. The second
/// array is the actual hash table, and it contains indices into the elements
/// array. This scheme cuts down on wasted space when the elements are larger
/// than a few bytes: instead of wasting `(1 - loadFactor) * sizeof(element)`
/// bytes on unused space in the hash table, we only waste `(1 - loadFactor) *
/// sizeof(index)`. This scheme also avoids readers seeing partially constructed
/// elements.
///
/// Reader/writer synchronization for new elements is handled by keeping an
/// element count which is only incremented when the element has been fully
/// constructed. A reader which sees an index beyond its view of the current
/// count will ignore it and treat that as if there was no entry.
///
/// Reader/writer synchronization for resizing the arrays is handled by tracking
/// the current number of active readers. When resizing, the new array is
/// allocated, the data copied, and then the old array is placed in a free list.
/// The free list is only deallocated if there are no readers, otherwise freeing
/// is deferred.
///
/// Reader/writer synchronization for clearing the table is a combination of the
/// above. By keeping the old arrays around until all readers are finished, we
/// ensure that readers which started before the clear see valid (pre-clear)
/// data. Readers which see any array as empty will produce no results, thus
/// providing valid post-clear data.
///
/// This is intended to be used for tables that exist for the life of the
/// process. It has no destructor, to avoid generating useless global destructor
/// calls. The memory it allocates can be freed by calling clear() with no
/// outstanding readers, but this won't destroy the static mutex it uses.
template <class ElemTy, class MutexTy = LazyMutex>
struct ConcurrentReadableHashMap {
  // We don't call destructors. Make sure the elements will put up with this.
  static_assert(std::is_trivially_destructible<ElemTy>::value,
                "Elements must not have destructors (they won't be called).");

private:
  /// The reciprocal of the load factor at which we expand the table. A value of
  /// 4 means that we resize at 1/4 = 75% load factor.
  static const size_t ResizeProportion = 4;

  /// Get the "good size" for a given allocation size. When available, this
  /// rounds up to the next allocation quantum by calling `malloc_good_size`.
  /// Otherwise, just return the passed-in size, which is always valid even if
  /// not necessarily optimal.
  static size_t goodSize(size_t size) {
#if defined(__APPLE__) && defined(__MACH__) && SWIFT_STDLIB_HAS_DARWIN_LIBMALLOC
    return malloc_good_size(size);
#else
    return size;
#endif
  }

  /// A private class representing the storage of the indices. In order to
  /// ensure that readers can get a consistent view of the indices with a single
  /// atomic read, we store the size of the indices array inline, as the first
  /// element in the array.
  ///
  /// We want the number of indices to be a power of two so that we can use a
  /// bitwise AND to convert a hash code to an index. We want the entire array
  /// to be a power of two in size to be friendly to the allocator, but the size
  /// is stored inline. We work around this contradiction by considering the
  /// first index to always be occupied with a value that never matches any key.
  struct IndexStorage {
    using RawType = uintptr_t;

    RawType Value;

    static constexpr uintptr_t log2(uintptr_t x) {
      return x <= 1 ? 0 : log2(x >> 1) + 1;
    }

    // A crude way to detect trivial use-after-free bugs given that a lot of
    // data structure have a strong bias toward bits that are zero.
#ifndef NDEBUG
    static constexpr uint8_t InlineCapacityDebugBits = 0xC0;
#else
    static constexpr uint8_t InlineCapacityDebugBits = 0;
#endif
    static constexpr uintptr_t InlineIndexBits = 4;
    static constexpr uintptr_t InlineIndexMask = 0xF;
    static constexpr uintptr_t InlineCapacity =
        sizeof(RawType) * CHAR_BIT / InlineIndexBits;
    static constexpr uintptr_t InlineCapacityLog2 = log2(InlineCapacity);

    // Indices can be stored in different ways, depending on how big they need
    // to be. The index mode is stored in the bottom two bits of Value. The
    // meaning of the rest of Value depends on the mode.
    enum class IndexMode {
      // Value is treated as an array of four-bit integers, storing the indices.
      // The first element overlaps with the mode, and is never used.
      Inline,

      // The rest of Value holds a pointer to storage. The first byte of this
      // storage holds the log2 of the storage capacity. The storage is treated
      // as an array of 8, 16, or 32-bit integers. The first element overlaps
      // with the capacity, and is never used.
      Array8,
      Array16,
      Array32,
    };

    IndexStorage() : Value(0) {}
    IndexStorage(RawType value) : Value(value) {}
    IndexStorage(void *ptr, unsigned indexSize, uint8_t capacityLog2) {
      assert(capacityLog2 > InlineCapacityLog2);
      IndexMode mode;
      switch (indexSize) {
      case sizeof(uint8_t):
        mode = IndexMode::Array8;
        break;
      case sizeof(uint16_t):
        mode = IndexMode::Array16;
        break;
      case sizeof(uint32_t):
        mode = IndexMode::Array32;
        break;
      default:
        swift_unreachable("unknown index size");
      }
      Value = reinterpret_cast<uintptr_t>(ptr) | static_cast<uintptr_t>(mode);
      *reinterpret_cast<uint8_t *>(ptr) = capacityLog2 | InlineCapacityDebugBits;
    }

    bool valueIsPointer() { return Value & 3; }

    void *pointer() {
      if (valueIsPointer())
        return (void *)(Value & (RawType)~3);
      return nullptr;
    }

    IndexMode indexMode() { return IndexMode(Value & 3); }

    // Index size is variable based on capacity, either 8, 16, or 32 bits.
    //
    // This is somewhat conservative. We could have, for example, a capacity of
    // 512 but a maximum index of only 200, which would still allow for 8-bit
    // indices. However, taking advantage of this would require reallocating
    // the index storage when the element count crossed a threshold, which is
    // more complex, and the advantages are minimal. This keeps it simple.

    // Get the size, in bytes, of the index needed for the given capacity.
    static unsigned indexSize(uint8_t capacityLog2) {
      if (capacityLog2 <= sizeof(uint8_t) * CHAR_BIT)
        return sizeof(uint8_t);
      if (capacityLog2 <= sizeof(uint16_t) * CHAR_BIT)
        return sizeof(uint16_t);
      return sizeof(uint32_t);
    }

    uint8_t getCapacityLog2() {
      if (auto *ptr = pointer()) {
        auto result = *reinterpret_cast<uint8_t *>(ptr);
        assert((result & InlineCapacityDebugBits) == InlineCapacityDebugBits);
        return result & ~InlineCapacityDebugBits;
      }
      return InlineCapacityLog2;
    }

    static IndexStorage allocate(size_t capacityLog2) {
      assert(capacityLog2 > 0);
      size_t capacity = 1UL << capacityLog2;
      unsigned size = indexSize(capacityLog2);
      auto *ptr = calloc(capacity, size);
      if (!ptr)
        swift::crash("Could not allocate memory.");
      return IndexStorage(ptr, size, capacityLog2);
    }

    unsigned loadIndexAt(size_t i, std::memory_order order) {
      assert(i > 0 && "index zero is off-limits, used to store capacity");
      assert(i < (1 << getCapacityLog2()) &&
             "index is off the end of the indices");

      switch (indexMode()) {
      case IndexMode::Inline:
        return (Value >> (i * InlineIndexBits)) & InlineIndexMask;
      case IndexMode::Array8:
        return ((std::atomic<uint8_t> *)pointer())[i].load(order);
      case IndexMode::Array16:
        return ((std::atomic<uint16_t> *)pointer())[i].load(order);
      case IndexMode::Array32:
        return ((std::atomic<uint32_t> *)pointer())[i].load(order);
      }
    }

    void storeIndexAt(std::atomic<RawType> *inlineStorage, unsigned value,
                      size_t i, std::memory_order order) {
      assert(i > 0 && "index zero is off-limits, used to store capacity");
      assert(i < (1 << getCapacityLog2()) &&
             "index is off the end of the indices");

      switch (indexMode()) {
      case IndexMode::Inline: {
        assert(value == (value & InlineIndexMask) && "value is too big to fit");
        auto shift = i * InlineIndexBits;
        assert((Value & (InlineIndexMask << shift)) == 0 &&
               "can't overwrite an existing index");
        assert(Value == inlineStorage->load(std::memory_order_relaxed) &&
               "writing with a stale IndexStorage");
        auto newStorage = Value | ((RawType)value << shift);
        inlineStorage->store(newStorage, order);
        break;
      }
      case IndexMode::Array8:
        ((std::atomic<uint8_t> *)pointer())[i].store(value, order);
        break;
      case IndexMode::Array16:
        ((std::atomic<uint16_t> *)pointer())[i].store(value, order);
        break;
      case IndexMode::Array32:
        ((std::atomic<uint32_t> *)pointer())[i].store(value, order);
        break;
      }
    }
  };

  /// The struct used for element storage. The `Elem` member is considered to be
  /// the first element of a variable-length array, whose size is determined by
  /// the allocation.
  struct ElementStorage {
    uintptr_t Capacity : 32;
    ElemTy Elem;

    static ElementStorage *allocate(size_t capacity) {
      auto headerSize = offsetof(ElementStorage, Elem);
      auto size = goodSize(headerSize + capacity * sizeof(ElemTy));

      auto *ptr = reinterpret_cast<ElementStorage *>(malloc(size));
      if (!ptr)
        swift::crash("Could not allocate memory.");

      ptr->Capacity = (size - headerSize) / sizeof(ElemTy);

      return ptr;
    }

    ElemTy *data() { return &Elem; }
  };

  /// The number of readers currently active, equal to the number of snapshot
  /// objects currently alive.
  std::atomic<uint32_t> ReaderCount{0};

  /// The number of elements in the elements array.
  std::atomic<uint32_t> ElementCount{0};

  /// The array of elements.
  std::atomic<ElementStorage *> Elements{nullptr};

  /// The array of indices.
  ///
  /// This has to be stored as a IndexStorage::RawType instead of a IndexStorage
  /// because some of our targets don't support interesting structs as atomic
  /// types. See also MetadataCache::TrackingInfo which uses the same technique.
  std::atomic<typename IndexStorage::RawType> Indices{0};

  /// The writer lock, which must be taken before any mutation of the table.
  MutexTy WriterLock;

  /// The list of pointers to be freed once no readers are active.
  ConcurrentFreeListNode *FreeList{nullptr};

  void incrementReaders() {
    ReaderCount.fetch_add(1, std::memory_order_acquire);
  }

  void decrementReaders() {
    ReaderCount.fetch_sub(1, std::memory_order_release);
  }

  /// Free all the arrays in the free lists if there are no active readers. If
  /// there are active readers, do nothing.
  void deallocateFreeListIfSafe() {
    // The standard says that std::memory_order_seq_cst only applies to
    // read-modify-write operations, so we need an explicit fence:
    std::atomic_thread_fence(std::memory_order_seq_cst);
    if (ReaderCount.load(std::memory_order_relaxed) == 0)
      ConcurrentFreeListNode::freeAll(&FreeList, free);
  }

  /// Grow the elements array, adding the old array to the free list and
  /// returning the new array with all existing elements copied into it.
  ElementStorage *resize(ElementStorage *elements, size_t elementCount) {
    // Grow capacity by 25%, making sure we grow by at least 1.
    size_t newCapacity =
        std::max(elementCount + (elementCount >> 2), elementCount + 1);
    auto *newElements = ElementStorage::allocate(newCapacity);

    if (elements) {
      if (std::is_trivially_copyable<ElemTy>::value) {
        memcpy(newElements->data(), elements->data(),
               elementCount * sizeof(ElemTy));
      } else {
        std::uninitialized_copy_n(elements->data(), elementCount,
                                  newElements->data());
      }
      ConcurrentFreeListNode::add(&FreeList, elements);
    }

    // Use seq_cst here to ensure that the subsequent load of ReaderCount is
    // ordered after this store. If ReaderCount is loaded first, then a new
    // reader could come in between that load and this store, and then we
    // could end up freeing the old elements pointer while it's still in use.
    Elements.store(newElements, std::memory_order_seq_cst);
    return newElements;
  }

  /// Grow the indices array, adding the old array to the free list and
  /// returning the new array with all existing indices copied into it. This
  /// operation performs a rehash, so that the indices are in the correct
  /// location in the new array.
  IndexStorage resize(IndexStorage indices, uint8_t indicesCapacityLog2,
                      ElemTy *elements) {
    // Double the size.
    size_t newCapacityLog2 = indicesCapacityLog2 + 1;
    size_t newMask = (1UL << newCapacityLog2) - 1;

    IndexStorage newIndices = IndexStorage::allocate(newCapacityLog2);

    size_t indicesCount = 1UL << indicesCapacityLog2;
    for (size_t i = 1; i < indicesCount; i++) {
      unsigned index = indices.loadIndexAt(i, std::memory_order_relaxed);
      if (index == 0)
        continue;

      auto *element = &elements[index - 1];
      auto hash = hash_value(*element);

      size_t newI = hash & newMask;
      // Index 0 is unusable (occupied by the capacity), so always skip it.
      while (newI == 0 ||
             newIndices.loadIndexAt(newI, std::memory_order_relaxed) != 0) {
        newI = (newI + 1) & newMask;
      }
      newIndices.storeIndexAt(nullptr, index, newI, std::memory_order_relaxed);
    }

    // Use seq_cst here to ensure that the subsequent load of ReaderCount is
    // ordered after this store. If ReaderCount is loaded first, then a new
    // reader could come in between that load and this store, and then we
    // could end up freeing the old indices pointer while it's still in use.
    Indices.store(newIndices.Value, std::memory_order_seq_cst);

    if (auto *ptr = indices.pointer())
      ConcurrentFreeListNode::add(&FreeList, ptr);

    return newIndices;
  }

  /// Search for the given key within the given indices and elements arrays. If
  /// an entry already exists for that key, return a pointer to the element. If
  /// no entry exists, return the location in the indices array where the index
  /// of the new element would be stored.
  template <class KeyTy>
  static std::pair<ElemTy *, unsigned>
  find(const KeyTy &key, IndexStorage indices, size_t elementCount,
       ElemTy *elements) {
    auto hash = hash_value(key);
    auto indicesMask = (1UL << indices.getCapacityLog2()) - 1;

    auto i = hash & indicesMask;
    while (true) {
      // Index 0 is used for the mask and is not actually an index.
      if (i == 0)
        i++;

      auto index = indices.loadIndexAt(i, std::memory_order_acquire);
      // Element indices are 1-based, 0 means no entry.
      if (index == 0)
        return {nullptr, i};
      if (index - 1 < elementCount) {
        auto *candidate = &elements[index - 1];
        if (candidate->matchesKey(key))
          return {candidate, 0};
      }

      i = (i + 1) & indicesMask;
    }
  }

public:
  // Implicitly trivial constructor/destructor.
  ConcurrentReadableHashMap() = default;
  ~ConcurrentReadableHashMap() = default;

  // This type cannot be safely copied or moved.
  ConcurrentReadableHashMap(const ConcurrentReadableHashMap &) = delete;
  ConcurrentReadableHashMap(ConcurrentReadableHashMap &&) = delete;
  ConcurrentReadableHashMap &
  operator=(const ConcurrentReadableHashMap &) = delete;

  /// Returns whether there are outstanding readers. For testing purposes only.
  bool hasActiveReaders() {
    return ReaderCount.load(std::memory_order_relaxed) > 0;
  }

  /// Readers take a snapshot of the hash map, then work with the snapshot.
  class Snapshot {
    ConcurrentReadableHashMap *Map;
    IndexStorage Indices;
    ElemTy *Elements;
    size_t ElementCount;

  public:
    Snapshot(ConcurrentReadableHashMap *map, IndexStorage indices,
             ElemTy *elements, size_t elementCount)
        : Map(map), Indices(indices), Elements(elements),
          ElementCount(elementCount) {}

    Snapshot(const Snapshot &other)
        : Map(other.Map), Indices(other.Indices), Elements(other.Elements),
          ElementCount(other.ElementCount) {
      Map->incrementReaders();
    }

    ~Snapshot() { Map->decrementReaders(); }

    /// Search for an element matching the given key. Returns a pointer to the
    /// found element, or nullptr if no matching element exists.
    //
    // This is marked as ref-qualified (the &) to make sure it can't be called
    // on temporaries, since the temporary would be destroyed before the return
    // value can be used, making it invalid.
    template <class KeyTy> const ElemTy *find(const KeyTy &key) & {
      if (!Indices.Value || !ElementCount || !Elements)
        return nullptr;
      return ConcurrentReadableHashMap::find(key, Indices, ElementCount,
                                             Elements)
          .first;
    }
  };

  /// Take a snapshot of the current state of the hash map.
  Snapshot snapshot() {
    incrementReaders();

    // Carefully loading the indices, element count, and elements pointer in
    // order ensures a consistent view of the table with respect to concurrent
    // inserts. However, this is not sufficient to avoid an inconsistent view
    // with respect to concurrent clears. The danger scenario is:
    //
    // 1. Read indices and elementCount from a table with N entries.
    // 2. Another thread clears the table.
    // 3. Another thread inserts M entries, where M < N.
    // 4. The reader thread reads elements.
    // 5. The reader thread performs a find. The key's hash leads us to an index
    //    I, where > M.
    // 6. The reader thread reads from element I, which is off the end of the
    //    elements array.
    //
    // To avoid this, read the elements pointer twice, at the beginning and end.
    // If the values are not the same then there may have been a clear in the
    // middle, so we retry. This will have false positives: a new element
    // pointer can just mean a concurrent insert that triggered a resize of the
    // elements array. This is harmless aside from a small performance hit, and
    // should not happen often.
    IndexStorage indices;
    size_t elementCount;
    ElementStorage *elements;
    ElementStorage *elements2;
    do {
      elements = Elements.load(std::memory_order_acquire);
      indices = Indices.load(std::memory_order_acquire);
      elementCount = ElementCount.load(std::memory_order_acquire);
      elements2 = Elements.load(std::memory_order_acquire);
    } while (elements != elements2);

    ElemTy *elementsPtr = elements ? elements->data() : nullptr;
    return Snapshot(this, indices, elementsPtr, elementCount);
  }

  /// Get an element by key, or insert a new element for that key if one is not
  /// already present. Invoke `call` with the pointer to the element. BEWARE:
  /// `call` is invoked with the internal writer lock held, keep work to a
  /// minimum.
  ///
  /// `call` is passed the following parameters:
  ///   - `element`: the pointer to the element corresponding to `key`
  ///   - `created`: true if the element is newly created, false if it already
  ///                exists
  /// `call` returns a `bool`. When `created` is `true`, the return values mean:
  ///   - `true` the new entry is to be kept
  ///   - `false` indicates that the new entry is discarded
  /// If the new entry is kept, then the new element MUST be initialized, and
  /// have a hash value that matches the hash value of `key`.
  ///
  /// The return value is ignored when `created` is `false`.
  template <class KeyTy, typename Call>
  void getOrInsert(KeyTy key, const Call &call) {
    typename MutexTy::ScopedLock guard(WriterLock);

    auto indices = IndexStorage{Indices.load(std::memory_order_relaxed)};
    auto indicesCapacityLog2 = indices.getCapacityLog2();
    auto elementCount = ElementCount.load(std::memory_order_relaxed);
    auto *elements = Elements.load(std::memory_order_relaxed);
    auto *elementsPtr = elements ? elements->data() : nullptr;

    auto found = this->find(key, indices, elementCount, elementsPtr);
    if (found.first) {
      call(found.first, false);
      deallocateFreeListIfSafe();
      return;
    }

    auto indicesCapacity = 1UL << indicesCapacityLog2;

    // The number of slots in use is elementCount + 1, since the capacity also
    // takes a slot.
    auto emptyCount = indicesCapacity - (elementCount + 1);
    auto proportion = indicesCapacity / emptyCount;
    if (proportion >= ResizeProportion) {
      indices = resize(indices, indicesCapacityLog2, elementsPtr);
      found = find(key, indices, elementCount, elementsPtr);
      assert(!found.first && "Shouldn't suddenly find the key after rehashing");
    }

    if (!elements || elementCount >= elements->Capacity) {
      elements = resize(elements, elementCount);
    }
    auto *element = &elements->data()[elementCount];

    // Order matters: fill out the element, then update the count,
    // then update the index.
    bool keep = call(element, true);
    if (keep) {
      assert(hash_value(key) == hash_value(*element) &&
             "Element must have the same hash code as its key.");
      ElementCount.store(elementCount + 1, std::memory_order_release);
      indices.storeIndexAt(&Indices, elementCount + 1, found.second,
                           std::memory_order_release);
    }

    deallocateFreeListIfSafe();
  }

  /// Clear the hash table, freeing (when safe) all memory currently used for
  /// indices and elements.
  ///
  /// - addFreedNodes: if nonempty, this function will be called with the head
  /// of the free list. The function may add additional nodes to the free
  /// list, which will be freed when it is safe to do so.
  void clear(std::function<void(ConcurrentFreeListNode *&)> addFreedNodes
                 = nullptr) {
    typename MutexTy::ScopedLock guard(WriterLock);

    IndexStorage indices = Indices.load(std::memory_order_relaxed);
    auto *elements = Elements.load(std::memory_order_relaxed);

    // Order doesn't matter here, snapshots will gracefully handle any field
    // being NULL/0 while the others are not.
    Indices.store(0, std::memory_order_relaxed);
    ElementCount.store(0, std::memory_order_relaxed);
    Elements.store(nullptr, std::memory_order_relaxed);

    if (auto *ptr = indices.pointer())
      ConcurrentFreeListNode::add(&FreeList, ptr);
    ConcurrentFreeListNode::add(&FreeList, elements);

    if (addFreedNodes)
      addFreedNodes(FreeList);

    deallocateFreeListIfSafe();
  }
};

/// A wrapper type for indirect hash map elements. Stores a pointer to the real
/// element and forwards key matching and hashing.
template <class ElemTy> struct HashMapElementWrapper {
  ElemTy *Ptr;

  template <class KeyTy> bool matchesKey(const KeyTy &key) {
    return Ptr->matchesKey(key);
  }

  friend llvm::hash_code hash_value(const HashMapElementWrapper &wrapper) {
    return hash_value(*wrapper.Ptr);
  }
};

/// A ConcurrentReadableHashMap that provides stable addresses for the elements
/// by allocating them separately and storing pointers to them. The elements of
/// the hash table are instances of HashMapElementWrapper. A new getOrInsert
/// method is provided that directly returns the stable element pointer.
template <class ElemTy, class Allocator, class MutexTy = LazyMutex>
struct StableAddressConcurrentReadableHashMap
    : public ConcurrentReadableHashMap<HashMapElementWrapper<ElemTy>, MutexTy> {
  // Implicitly trivial destructor.
  ~StableAddressConcurrentReadableHashMap() = default;

  std::atomic<ElemTy *> LastFound{nullptr};

  /// Get or insert an element for the given key and arguments. Returns the
  /// pointer to the existing or new element, and a bool indicating whether the
  /// element was created. When false, the element already existed before the
  /// call.
  template <class KeyTy, class... ArgTys>
  std::pair<ElemTy *, bool> getOrInsert(KeyTy key, ArgTys &&...args) {
    // See if this is a repeat of the previous search.
    if (auto lastFound = LastFound.load(std::memory_order_acquire))
      if (lastFound->matchesKey(key))
        return {lastFound, false};

    // Optimize for the case where the value already exists.
    {
      // Tightly scope the snapshot so it's gone before we call getOrInsert
      // below, otherwise that call will always see an outstanding snapshot and
      // never be able to collect garbage.
      auto snapshot = this->snapshot();
      if (auto wrapper = snapshot.find(key)) {
        LastFound.store(wrapper->Ptr, std::memory_order_relaxed);
        return {wrapper->Ptr, false};
      }
    }

    // No such element. Insert if needed. Note: another thread may have inserted
    // it in the meantime, so we still have to handle both cases!
    ElemTy *ptr = nullptr;
    bool outerCreated = false;
    ConcurrentReadableHashMap<HashMapElementWrapper<ElemTy>, MutexTy>::
        getOrInsert(key, [&](HashMapElementWrapper<ElemTy> *wrapper,
                             bool created) {
          if (created) {
            // Created the indirect entry. Allocate the actual storage.
            size_t allocSize =
                sizeof(ElemTy) + ElemTy::getExtraAllocationSize(key, args...);
            void *memory = Allocator().Allocate(allocSize, alignof(ElemTy));
            new (memory) ElemTy(key, std::forward<ArgTys>(args)...);
            wrapper->Ptr = reinterpret_cast<ElemTy *>(memory);
          }
          ptr = wrapper->Ptr;
          outerCreated = created;
          return true; // Keep the new entry.
        });
    LastFound.store(ptr, std::memory_order_relaxed);
    return {ptr, outerCreated};
  }

  template <class KeyTy> ElemTy *find(const KeyTy &key) {
    auto snapshot = this->snapshot();
    auto result = snapshot.find(key);
    if (!result)
      return nullptr;
    return result->Ptr;
  }

private:
  // Clearing would require deallocating elements, which we don't support.
  void clear() = delete;
};

} // end namespace swift

#endif // SWIFT_RUNTIME_CONCURRENTUTILS_H
