//===--- Concurrent.h - Concurrent Data Structures  ------------*- C++ -*--===//
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
    // Iterate over the list and delete all the nodes.
    auto Ptr = First.load(std::memory_order_acquire);
    while (Ptr) {
      auto N = Ptr->Next;
      delete Ptr;
      Ptr = N;
    }
  }

  ConcurrentList(const ConcurrentList &) = delete;
  ConcurrentList &operator=(const ConcurrentList &) = delete;

  /// A list iterator.
  struct ConcurrentListIterator {
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
  ConcurrentMapNode(KeyTy H)
      : Left(nullptr), Right(nullptr), Key(H), Payload() {}

  ~ConcurrentMapNode() {
    delete Left.load(std::memory_order_acquire);
    delete Right.load(std::memory_order_acquire);
  }

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
/// does not support the removal of nodes, which is more difficult.
/// The method findOrAllocateNode searches the binary tree in search of the
/// exact Key value. If it finds an edge that points to NULL that should
/// contain the value then it tries to compare and swap the new node into
/// place. If it looses the race to a different thread it de-allocates
/// the node and starts the search again since the new node should
/// be placed (or found) on the new link. See findOrAllocateNode for more
/// details.
template <class KeyTy, class ValueTy> class ConcurrentMap {
public:
  ConcurrentMap() : Sentinel(0), LastSearch(&Sentinel) {}

  ConcurrentMap(const ConcurrentMap &) = delete;
  ConcurrentMap &operator=(const ConcurrentMap &) = delete;

  /// A Sentinel root node that contains no elements.
  typedef ConcurrentMapNode<KeyTy, ConcurrentList<ValueTy>> NodeTy;
  NodeTy Sentinel;

  /// This value stores the last node that we searched. This is useful for
  /// accelerating the search of the same value again and again.
  std::atomic<NodeTy *> LastSearch;

  /// Search a for a node with key value \p. If the node does not exist then
  /// allocate a new bucket and add it to the tree.
  ConcurrentList<ValueTy> &findOrAllocateNode(KeyTy Key) {
    // Try looking at the last node we searched.
    NodeTy *Last = LastSearch.load(std::memory_order_acquire);
    if (Last->Key == Key)
      return Last->Payload;

    // Search the binary tree.
    NodeTy *Found = findOrAllocateNode_rec(&Sentinel, Key);

    // Save the node that we found, in case we look for the same value again.
    LastSearch.store(Found,std:: memory_order_release);
    return Found->Payload;
  }

private:
  static NodeTy *findOrAllocateNode_rec(NodeTy *P, KeyTy Key) {
    // Found the node we were looking for.
    if (P->Key == Key)
      return P;

    // Point to the edge we want to replace.
    typename NodeTy::EdgeTy *Edge = nullptr;

    // The current edge value.
    NodeTy *CurrentVal;

    // Select the edge to follow.
    if (P->Key > Key) {
      CurrentVal = P->Left.load(std::memory_order_acquire);
      if (CurrentVal) return findOrAllocateNode_rec(CurrentVal, Key);
      Edge = &P->Left;
    } else {
      CurrentVal = P->Right.load(std::memory_order_acquire);
      if (CurrentVal) return findOrAllocateNode_rec(CurrentVal, Key);
      Edge = &P->Right;
    }

    // Allocate a new node.
    NodeTy *New = new NodeTy(Key);

    // Try to set a new node:
    if (std::atomic_compare_exchange_weak_explicit(Edge, &CurrentVal, New,
                                                   std::memory_order_release,
                                                   std::memory_order_relaxed)){
      // On success return the new node.
      return New;
    }

    // If failed, deallocate the node and look for a new place in the
    // tree. Some other thread may have created a new entry and we may
    // discover it, so start searching with the current node.
    delete New;
    return findOrAllocateNode_rec(P, Key);
  }
};

/// A concurrent bump pointer allocator bank.
template <class ElemTy, int NumElements = 256> class ConcurrentMemoryBank {
  /// The storage.
  ElemTy Storage[NumElements];

  /// Points to the first free memory location.
  std::atomic<size_t> NextFree;

public:
  ConcurrentMemoryBank() : Storage(), NextFree(0) {}

  /// Return true if there is free space in this bank.
  bool hasFreeSpace() {
    return NextFree.load(std::memory_order_acquire) < NumElements;
  }

  /// Try to allocate memory in the current bank. Return a pointer to the newly
  /// allocated memory or null in case of failure.
  ElemTy *Allocate() {
    size_t CurrNextFree = NextFree.load(std::memory_order_acquire);
    size_t Next = CurrNextFree + 1;

    if (CurrNextFree >= NumElements)
      return nullptr;

    // Try to replace the current First with the new node.
    while (!std::atomic_compare_exchange_weak_explicit(&NextFree, &CurrNextFree, Next,
                                               std::memory_order_release,
                                               std::memory_order_relaxed)) {
      CurrNextFree = NextFree.load(std::memory_order_acquire);
      Next = CurrNextFree + 1;

      // if we fail we can try again as long as there is space in the buffer.
      if (CurrNextFree >= NumElements)
        return nullptr;
    }

    return &Storage[CurrNextFree];
  }
};


/// This is a concurrent bump pointer memory allocator. It only allocates memory
/// and never frees it.
template <class ElemTy, int BankSize = 256>
class ConcurrentMemoryAllocator {
  typedef ConcurrentMemoryBank<ElemTy, BankSize> BankTy;
  /// A list of banks to allocate from.
  ConcurrentList<BankTy*> Banks;

public:
  /// Allocate memory and return a pointer to the newly
  /// allocated buffer.
  ElemTy *Allocate() {
    start:
    // Try to allocate memory in one of the banks:
    for (auto Bank : Banks) {
      // If we were able to allocate memory in one of the existing banks then
      // simply return the newly allocated pointer.
      if (ElemTy *P = Bank->Allocate())
        return P;
    }

    // We were not able to allocate memory in one of the existing banks.
    // Allocate a new bank and try again.
    Banks.push_front(new BankTy());
    goto start;
  }
};

#endif // SWIFT_RUNTIME_CONCURRENTUTILS_H
