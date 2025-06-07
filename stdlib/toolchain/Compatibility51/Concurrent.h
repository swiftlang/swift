//===--- Concurrent.h - Concurrent Data Structures backport -----*- C++ -*-===//
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

// This is a snapshot of the `ConcurrentMap` and `ConcurrentReadableArray`
// structures from the Swift runtime, adapted to be independent of runtime
// dependencies on the C++ runtime and LLVM support libraries to make it
// suitable for use in back-deployment compatibility libraries.

#ifndef SWIFT_OVERRIDE_CONCURRENTUTILS_H
#define SWIFT_OVERRIDE_CONCURRENTUTILS_H
#include <iterator>
#include <algorithm>
#include <atomic>
#include <cassert>
#include <functional>
#include <pthread.h>
#include <stdint.h>
#include "swift/Basic/Defer.h"
#include "swift/Runtime/Atomic.h"

namespace swift {

namespace overrides {

/// A utility function for ordering two pointers, which is useful
/// for implementing compareWithKey.
template <class T>
static inline int comparePointers(const T *left, const T *right) {
  return (left == right ? 0 : std::less<const T *>()(left, right) ? -1 : 1);
}

template <class EntryTy, bool ProvideDestructor>
class ConcurrentMapBase;

/// The partial specialization of ConcurrentMapBase whose destructor is
/// trivial.  The other implementation inherits from this, so this is a
/// base for all ConcurrentMaps.
template <class EntryTy>
class ConcurrentMapBase<EntryTy, false> {
protected:
  struct Node {
    std::atomic<Node*> Left;
    std::atomic<Node*> Right;
    EntryTy Payload;

    template <class... Args>
    Node(Args &&... args)
      : Left(nullptr), Right(nullptr), Payload(std::forward<Args>(args)...) {}

    Node(const Node &) = delete;
    Node &operator=(const Node &) = delete;

  #ifndef NDEBUG
    void dump() const {
      auto L = Left.load(std::memory_order_acquire);
      auto R = Right.load(std::memory_order_acquire);
      printf("\"%p\" [ label = \" {<f0> %08lx | {<f1> | <f2>}}\" "
             "style=\"rounded\" shape=\"record\"];\n",
             this, (long) Payload.getKeyValueForDump());

      if (L) {
        L->dump();
        printf("\"%p\":f1 -> \"%p\":f0;\n", this, L);
      }
      if (R) {
        R->dump();
        printf("\"%p\":f2 -> \"%p\":f0;\n", this, R);
      }
    }
  #endif
  };

  std::atomic<Node*> Root;

  constexpr ConcurrentMapBase() : Root(nullptr) {}

  // Implicitly trivial destructor.
  ~ConcurrentMapBase() = default;

  void destroyNode(Node *node) {
    assert(node && "destroying null node");
    // Destroy the node's payload.
    node->~Node();
    // Deallocate the node.
    free(node);
  }
};

/// The partial specialization of ConcurrentMapBase which provides a
/// non-trivial destructor.
template <class EntryTy>
class ConcurrentMapBase<EntryTy, true>
    : protected ConcurrentMapBase<EntryTy, false> {
protected:
  using super = ConcurrentMapBase<EntryTy, false>;
  using Node = typename super::Node;

  constexpr ConcurrentMapBase() {}

  ~ConcurrentMapBase() {
    destroyTree(this->Root);
  }

private:
  void destroyTree(const std::atomic<Node*> &edge) {
    // This can be a relaxed load because destruction is not allowed to race
    // with other operations.
    auto node = edge.load(std::memory_order_relaxed);
    if (!node) return;

    // Destroy the node's children.
    destroyTree(node->Left);
    destroyTree(node->Right);

    // Destroy the node itself.
    this->destroyNode(node);
  }
};

/// A concurrent map that is implemented using a binary tree. It supports
/// concurrent insertions but does not support removals or rebalancing of
/// the tree.
///
/// The entry type must provide the following operations:
///
///   /// For debugging purposes only. Summarize this key as an integer value.
///   intptr_t getKeyIntValueForDump() const;
///
///   /// A ternary comparison.  KeyTy is the type of the key provided
///   /// to find or getOrInsert.
///   int compareWithKey(KeyTy key) const;
///
///   /// Return the amount of extra trailing space required by an entry,
///   /// where KeyTy is the type of the first argument to getOrInsert and
///   /// ArgTys is the type of the remaining arguments.
///   static size_t getExtraAllocationSize(KeyTy key, ArgTys...)
///
///   /// Return the amount of extra trailing space that was requested for
///   /// this entry.  This method is only used to compute the size of the
///   /// object during node deallocation; it does not need to return a
///   /// correct value so long as the allocator's Deallocate implementation
///   /// ignores this argument.
///   size_t getExtraAllocationSize() const;
///
/// If ProvideDestructor is false, the destructor will be trivial.  This
/// can be appropriate when the object is declared at global scope.
template <class EntryTy, bool ProvideDestructor = true>
class ConcurrentMap
      : private ConcurrentMapBase<EntryTy, ProvideDestructor> {
  using super = ConcurrentMapBase<EntryTy, ProvideDestructor>;

  using Node = typename super::Node;

  /// Inherited from base class:
  ///   std::atomic<Node*> Root;
  using super::Root;

  /// This member stores the address of the last node that was found by the
  /// search procedure. We cache the last search to accelerate code that
  /// searches the same value in a loop.
  std::atomic<Node*> LastSearch;

public:
  constexpr ConcurrentMap() : LastSearch(nullptr) {}

  ConcurrentMap(const ConcurrentMap &) = delete;
  ConcurrentMap &operator=(const ConcurrentMap &) = delete;

  // ConcurrentMap<T, false> must have a trivial destructor.
  ~ConcurrentMap() = default;

public:

#ifndef NDEBUG
  void dump() const {
    auto R = Root.load(std::memory_order_acquire);
    printf("digraph g {\n"
           "graph [ rankdir = \"TB\"];\n"
           "node  [ fontsize = \"16\" ];\n"
           "edge  [ ];\n");
    if (R) {
      R->dump();
    }
    printf("\n}\n");
  }
#endif

  /// Search for a value by key \p Key.
  /// \returns a pointer to the value or null if the value is not in the map.
  template <class KeyTy>
  EntryTy *find(const KeyTy &key) {
    // Check if we are looking for the same key that we looked for in the last
    // time we called this function.
    if (Node *last = LastSearch.load(std::memory_order_acquire)) {
      if (last->Payload.compareWithKey(key) == 0)
        return &last->Payload;
    }

    // Search the tree, starting from the root.
    Node *node = Root.load(std::memory_order_acquire);
    while (node) {
      int comparisonResult = node->Payload.compareWithKey(key);
      if (comparisonResult == 0) {
        LastSearch.store(node, std::memory_order_release);
        return &node->Payload;
      } else if (comparisonResult < 0) {
        node = node->Left.load(std::memory_order_acquire);
      } else {
        node = node->Right.load(std::memory_order_acquire);
      }
    }

    return nullptr;
  }

  /// Get or create an entry in the map.
  ///
  /// \returns the entry in the map and whether a new node was added (true)
  ///   or already existed (false)
  template <class KeyTy, class... ArgTys>
  std::pair<EntryTy*, bool> getOrInsert(KeyTy key, ArgTys &&... args) {
    // Check if we are looking for the same key that we looked for the
    // last time we called this function.
    if (Node *last = LastSearch.load(std::memory_order_acquire)) {
      if (last && last->Payload.compareWithKey(key) == 0)
        return { &last->Payload, false };
    }

    // The node we allocated.
    Node *newNode = nullptr;

    // Start from the root.
    auto edge = &Root;

    while (true) {
      // Load the edge.
      Node *node = edge->load(std::memory_order_acquire);

      // If there's a node there, it's either a match or we're going to
      // one of its children.
      if (node) {
      searchFromNode:

        // Compare our key against the node's key.
        int comparisonResult = node->Payload.compareWithKey(key);

        // If it's equal, we can use this node.
        if (comparisonResult == 0) {
          // Destroy the node we allocated before if we're carrying one around.
          if (newNode) this->destroyNode(newNode);

          // Cache and report that we found an existing node.
          LastSearch.store(node, std::memory_order_release);
          return { &node->Payload, false };
        }

        // Otherwise, select the appropriate child edge and descend.
        edge = (comparisonResult < 0 ? &node->Left : &node->Right);
        continue;
      }

      // Create a new node.
      if (!newNode) {
        size_t allocSize =
          sizeof(Node) + EntryTy::getExtraAllocationSize(key, args...);
        void *memory;
        if (posix_memalign(&memory, alignof(Node), allocSize))
          abort();
        newNode = ::new (memory) Node(key, std::forward<ArgTys>(args)...);
      }

      // Try to set the edge to the new node.
      if (std::atomic_compare_exchange_strong_explicit(edge, &node, newNode,
                                                  std::memory_order_acq_rel,
                                                  std::memory_order_acquire)) {
        // If that succeeded, cache and report that we created a new node.
        LastSearch.store(newNode, std::memory_order_release);
        return { &newNode->Payload, true };
      }

      // Otherwise, we lost the race because some other thread initialized
      // the edge before us.  node will be set to the current value;
      // repeat the search from there.
      assert(node && "spurious failure from compare_exchange_strong?");
      goto searchFromNode;
    }
  }
};

/// A minimal implementation of a growable array with no runtime dependencies.
template<class Element>
class MiniVector {
  Element *first;
  size_t size, capacity;
public:
  MiniVector() : first(nullptr), size(0), capacity(0) {
    static_assert(std::is_trivial<Element>::value,
                  "only implemented for trivial types");
  }
  ~MiniVector() { free(first); }
  
  MiniVector(const MiniVector &) = delete;
  
  Element *begin() { return first; }
  Element *end() { return first + size; }
  
  void push_back(const Element &e) {
    if (size >= capacity) {
      capacity = capacity ? capacity*2 : 8;
      first = (Element*)realloc(first, capacity * sizeof(Element));
      if (!first)
        abort();
    }
    first[size++] = e;
  }
  
  void clear_and_shrink_to_fit() {
    free(first);
    first = nullptr;
    size = 0;
    capacity = 0;
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
      if (!ptr) abort();
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
  pthread_mutex_t WriterMutex;
  MiniVector<Storage *> FreeList;
  
  void incrementReaders() {
    ReaderCount.fetch_add(1, std::memory_order_acquire);
  }
  
  void decrementReaders() {
    ReaderCount.fetch_sub(1, std::memory_order_release);
  }
  
  void deallocateFreeList() {
    for (Storage *storage : FreeList)
      storage->deallocate();
    FreeList.clear_and_shrink_to_fit();
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
    
    const ElemTy *begin() { return Start; }
    const ElemTy *end() { return Start + Count; }
    size_t count() { return Count; }
  };
  
  // This type cannot be safely copied, moved, or deleted.
  ConcurrentReadableArray(const ConcurrentReadableArray &) = delete;
  ConcurrentReadableArray(ConcurrentReadableArray &&) = delete;
  ConcurrentReadableArray &operator=(const ConcurrentReadableArray &) = delete;
  
  ConcurrentReadableArray()
    : Capacity(0), ReaderCount(0), Elements(nullptr) {
    pthread_mutex_init(&WriterMutex, nullptr);
  }
  
  ~ConcurrentReadableArray() {
    assert(ReaderCount.load(std::memory_order_acquire) == 0 &&
           "deallocating ConcurrentReadableArray with outstanding snapshots");
    deallocateFreeList();
    pthread_mutex_destroy(&WriterMutex);
  }
  
  void push_back(const ElemTy &elem) {
    pthread_mutex_lock(&WriterMutex);
    SWIFT_DEFER { pthread_mutex_unlock(&WriterMutex); };
    
    auto *storage = Elements.load(std::memory_order_relaxed);
    auto count = storage ? storage->Count.load(std::memory_order_relaxed) : 0;
    if (count >= Capacity) {
      auto newCapacity = std::max((size_t)16, count * 2);
      auto *newStorage = Storage::allocate(newCapacity);
      if (storage) {
        std::copy_n(storage->data(), count, newStorage->data());
        newStorage->Count.store(count, std::memory_order_relaxed);
        FreeList.push_back(storage);
      }
      
      storage = newStorage;
      Capacity = newCapacity;
      Elements.store(storage, std::memory_order_release);
    }
    
    new(&storage->data()[count]) ElemTy(elem);
    storage->Count.store(count + 1, std::memory_order_release);
    
    if (ReaderCount.load(std::memory_order_acquire) == 0)
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

}} // end namespace swift::overrides

#endif // SWIFT_RUNTIME_CONCURRENTUTILS_H
