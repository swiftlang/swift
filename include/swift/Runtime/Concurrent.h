//===--- Concurrent.h - Concurrent Data Structures  -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_RUNTIME_CONCURRENTUTILS_H
#define SWIFT_RUNTIME_CONCURRENTUTILS_H
#include <iterator>
#include <atomic>
#include <stdint.h>

/// This is a node in a concurrent linked list.
template <class ElemTy> struct ConcurrentListNode {
  ConcurrentListNode(ElemTy Elem) : Payload(Elem), Next(nullptr) {}
  ConcurrentListNode(const ConcurrentListNode &) = delete;
  ConcurrentListNode &operator=(const ConcurrentListNode &) = delete;

  /// The element.
  ElemTy Payload;
  /// Points to the next link in the chain.
  ConcurrentListNode<ElemTy> *Next;
};

/// This is a concurrent linked list. It supports insertion at the beginning
/// of the list and traversal using iterators.
/// This is a very simple implementation of a concurrent linked list
/// using atomic operations. The 'push_front' method allocates a new link
/// and attempts to compare and swap the old head pointer with pointer to
/// the new link. This operation may fail many times if there are other
/// contending threads, but eventually the head pointer is set to the new
/// link that already points to the old head value. Notice that the more
/// difficult feature of removing links is not supported.
/// See 'push_front' for more details.
template <class ElemTy> struct ConcurrentList {
  ConcurrentList() : First(nullptr) {}
  ~ConcurrentList() {
    clear();
  }

  /// Remove all of the links in the chain. This method leaves
  /// the list at a usable state and new links can be added.
  /// Notice that this operation is non-concurrent because
  /// we have no way of ensuring that no one is currently
  /// traversing the list.
  void clear() {
    // Iterate over the list and delete all the nodes.
    auto Ptr = First.load(std::memory_order_acquire);
    First.store(nullptr, std:: memory_order_release);

    while (Ptr) {
      auto N = Ptr->Next;
      delete Ptr;
      Ptr = N;
    }
  }

  ConcurrentList(const ConcurrentList &) = delete;
  ConcurrentList &operator=(const ConcurrentList &) = delete;

  /// A list iterator.
  struct ConcurrentListIterator :
      public std::iterator<std::forward_iterator_tag, ElemTy> {

    /// Points to the current link.
    ConcurrentListNode<ElemTy> *Ptr;
    /// C'tor.
    ConcurrentListIterator(ConcurrentListNode<ElemTy> *P) : Ptr(P) {}
    /// Move to the next element.
    ConcurrentListIterator &operator++() {
      Ptr = Ptr->Next;
      return *this;
    }
    /// Access the element.
    ElemTy &operator*() { return Ptr->Payload; }
    /// Same?
    bool operator==(const ConcurrentListIterator &o) const {
      return o.Ptr == Ptr;
    }
    /// Not the same?
    bool operator!=(const ConcurrentListIterator &o) const {
      return o.Ptr != Ptr;
    }
  };

  /// Iterator entry point.
  typedef ConcurrentListIterator iterator;
  /// Marks the beginning of the list.
  iterator begin() const {
    return ConcurrentListIterator(First.load(std::memory_order_acquire));
  }
  /// Marks the end of the list.
  iterator end() const { return ConcurrentListIterator(nullptr); }

  /// Add a new item to the list.
  void push_front(ElemTy Elem) {
    /// Allocate a new node.
    ConcurrentListNode<ElemTy> *N = new ConcurrentListNode<ElemTy>(Elem);
    // Point to the first element in the list.
    N->Next = First.load(std::memory_order_acquire);
    auto OldFirst = N->Next;
    // Try to replace the current First with the new node.
    while (!std::atomic_compare_exchange_weak_explicit(&First, &OldFirst, N,
                                               std::memory_order_release,
                                               std::memory_order_relaxed)) {
      // If we fail, update the new node to point to the new head and try to
      // insert before the new
      // first element.
      N->Next = OldFirst;
    }
  }

  /// Points to the first link in the list.
  std::atomic<ConcurrentListNode<ElemTy> *> First;
};

template <class KeyTy, class ValueTy> struct ConcurrentMapNode {
  ConcurrentMapNode(KeyTy H, const ValueTy &Val)
      : Left(nullptr), Right(nullptr), Key(H), Payload(Val) {}

  ~ConcurrentMapNode() {
    delete Left.load(std::memory_order_acquire);
    delete Right.load(std::memory_order_acquire);
  }

#ifndef NDEBUG
  void dump() {
    auto L = Left.load(std::memory_order_acquire);
    auto R = Right.load(std::memory_order_acquire);
    printf("\"%p\" [ label = \" {<f0> %08lx | {<f1> | <f2>}}\""
           "style=\"rounded\" shape = \"record\"];\n", this, Key);

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

  ConcurrentMapNode(const ConcurrentMapNode &) = delete;
  ConcurrentMapNode &operator=(const ConcurrentMapNode &) = delete;

  typedef std::atomic<ConcurrentMapNode *> EdgeTy;
  EdgeTy Left;
  EdgeTy Right;
  KeyTy Key;
  ValueTy Payload;
};

/// A concurrent map that is implemented using a binary tree. It supports
/// concurrent insertions but does not support removals or rebalancing of
/// the tree. Much like the concurrent linked list this data structure
/// does not support the removal of nodes. In order to reduce the memory usage
/// of the map we only store a hash of the key and the value. It is the
/// responsibility of the caller to handle hash collisions.
template <class KeyTy, class ValueTy> class ConcurrentMap {
public:
  ConcurrentMap() : Root(), LastSearch(nullptr) {}

  ConcurrentMap(const ConcurrentMap &) = delete;
  ConcurrentMap &operator=(const ConcurrentMap &) = delete;

  typedef ConcurrentMapNode<KeyTy, ValueTy> NodeTy;

  /// The root of the tree.
  std::atomic<NodeTy *> Root;

  /// This member stores the address of the last node that was found by the
  /// search procedure. We cache the last search to accelerate code that
  /// searches the same value in a loop.
  std::atomic<NodeTy *> LastSearch;

#ifndef NDEBUG
  void dump() {
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
  ValueTy *findValueByKey(KeyTy Key) {
    // Check if we are looking for the same key that we looked for in the last
    // time we called this function.
    NodeTy *Last = LastSearch.load(std::memory_order_acquire);
    if (Last && Last->Key == Key)
      return &Last->Payload;

    // Check if there are any entries in the tree.
    NodeTy *Head = Root.load(std::memory_order_acquire);
    if (!Head) {
      return nullptr;
    }

    // Search the tree.
    NodeTy *Found = findNodeByKey_rec(Head, Key);

    // Save the pointer to the last search result and return the address of the
    // value.
    if (Found) {
      LastSearch.store(Found, std:: memory_order_release);
      return &Found->Payload;
    }

    return nullptr;
  }

  /// Try to construct a new entry (\p Key, \p Value pair) in the map.
  /// \returns True if a new node was added, or false if an entry with the same
  /// key is already in the tree.
  bool tryToAllocateNewNode(KeyTy Key, const ValueTy &Val) {
    // Check if the tree is initialized.
    auto Head = Root.load(std::memory_order_acquire);
    if (!Head) {
      // Allocate a new head node.
      NodeTy *NewEntry = new NodeTy(Key, Val);

      // Try to set the new node at the top of the tree.
      if (std::atomic_compare_exchange_weak_explicit(&Root, &Head, NewEntry,
                                                  std::memory_order_release,
                                                  std::memory_order_relaxed)) {
        // We've allocated and registered a new node. Report Success.
        return true;
      }

      // We've lost the race. Some other thread initialized the root of the
      // tree before us. Delete the allocated node and try allocating a new
      // node again.
      delete NewEntry;
      return tryToAllocateNewNode(Key, Val);
    }

    return tryToAllocateNewNode_rec(Root, Key, Val);
  }

private:
  static NodeTy *findNodeByKey_rec(NodeTy *P, KeyTy Key) {
    // Found the node we were looking for.
    if (P->Key == Key)
      return P;

    // The current edge value.
    NodeTy *CurrentVal;

    // Select the edge to follow.
    if (P->Key > Key) {
      CurrentVal = P->Left.load(std::memory_order_acquire);
      if (CurrentVal) return findNodeByKey_rec(CurrentVal, Key);
    } else {
      CurrentVal = P->Right.load(std::memory_order_acquire);
      if (CurrentVal) return findNodeByKey_rec(CurrentVal, Key);
    }

    return nullptr;
  }

  static bool tryToAllocateNewNode_rec(NodeTy *P, KeyTy Key,
                                       const ValueTy &Val) {
    // We found an existing node.
    if (P->Key == Key)
      return false;

    // Point to the edge we want to replace.
    typename NodeTy::EdgeTy *Edge = nullptr;

    // The current edge value.
    NodeTy *CurrentVal;

    // Decide which edge to follow.
    if (P->Key > Key) {
      CurrentVal = P->Left.load(std::memory_order_acquire);
      if (CurrentVal) return tryToAllocateNewNode_rec(CurrentVal, Key, Val);
      Edge = &P->Left;
    } else {
      CurrentVal = P->Right.load(std::memory_order_acquire);
      if (CurrentVal) return tryToAllocateNewNode_rec(CurrentVal, Key, Val);
      Edge = &P->Right;
    }

    // Allocate a new node.
    NodeTy *New = new NodeTy(Key, Val);

    // Try to insert the new node:
    if (std::atomic_compare_exchange_weak_explicit(Edge, &CurrentVal, New,
                                                   std::memory_order_release,
                                                   std::memory_order_relaxed)){
      // We've allocated and registered a new node. Report Success.
      return true;
    }

    // We were not able to register the new node. Deallocate the new node and
    // look for a new place in the tree. Some other thread may have created a
    // new entry and we may discover it, so start searching with the current
    // node.
    delete New;
    return tryToAllocateNewNode_rec(P, Key, Val);
  }
};

#endif // SWIFT_RUNTIME_CONCURRENTUTILS_H
