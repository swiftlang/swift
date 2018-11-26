//===--- TreeScopedHashTable.h - Hash table with multiple active scopes ---===//
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

// Note: This file intentionally uses C++03 so that it can be eventually moved
// into LLVM ADT library.

#ifndef SWIFT_BASIC_TREESCOPEDHASHTABLE_H
#define SWIFT_BASIC_TREESCOPEDHASHTABLE_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Allocator.h"
#include "swift/Basic/Malloc.h"
#include <utility>

namespace swift {

template <typename K, typename V, typename AllocatorTy = llvm::MallocAllocator>
class TreeScopedHashTable;

template <typename K, typename V, typename AllocatorTy = llvm::MallocAllocator>
class TreeScopedHashTableScope;

template <typename K, typename V> class TreeScopedHashTableVal {
  TreeScopedHashTableVal *NextInScope;
  TreeScopedHashTableVal *NextForKey;
  K Key;
  V Val;
  TreeScopedHashTableVal(const K &Key, const V &Val) : Key(Key), Val(Val) {}

public:
  ~TreeScopedHashTableVal() = default;
  TreeScopedHashTableVal(const TreeScopedHashTableVal &) = delete;
  TreeScopedHashTableVal(TreeScopedHashTableVal &&) = delete;
  TreeScopedHashTableVal &operator=(const TreeScopedHashTableVal &) = delete;
  TreeScopedHashTableVal &operator=(TreeScopedHashTableVal &&) = delete;

  const K &getKey() const { return Key; }
  const V &getValue() const { return Val; }
  V &getValue() { return Val; }

  TreeScopedHashTableVal *getNextForKey() { return NextForKey; }
  const TreeScopedHashTableVal *getNextForKey() const { return NextForKey; }
  TreeScopedHashTableVal *getNextInScope() { return NextInScope; }

  template <typename AllocatorTy>
  static TreeScopedHashTableVal *Create(TreeScopedHashTableVal *NextInScope,
                                        TreeScopedHashTableVal *NextForKey,
                                        const K &key, const V &val,
                                        AllocatorTy &Allocator) {
    TreeScopedHashTableVal *New =
        Allocator.template Allocate<TreeScopedHashTableVal>();
    // Set up the value.
    new (New) TreeScopedHashTableVal(key, val);
    New->NextInScope = NextInScope;
    New->NextForKey = NextForKey;
    return New;
  }

  template <typename AllocatorTy> void Destroy(AllocatorTy &Allocator) {
    // Free memory referenced by the item.
    this->~TreeScopedHashTableVal();
    Allocator.Deallocate(this);
  }
};

/// \brief A reference-counted scope that actually owns the data in the
/// hashtable.
template <typename K, typename V, typename AllocatorTy = llvm::MallocAllocator>
class TreeScopedHashTableScopeImpl {
public:
  typedef TreeScopedHashTable<K, V, AllocatorTy> HTTy;

  /// The hashtable that we are active for.
  HTTy *HT;

  const typename HTTy::ScopeIDTy ID;

  /// This is the scope that we are shadowing in HT.
  TreeScopedHashTableScopeImpl *ParentScope;

  /// This is the last value that was inserted for this scope or null if none
  /// have been inserted yet.
  TreeScopedHashTableVal<K, V> *LastValInScope;

  bool MovedFrom;
  bool OwnsParentScope;

  unsigned RefCount;

  TreeScopedHashTableScopeImpl(TreeScopedHashTableScopeImpl &) = delete;
  void operator=(TreeScopedHashTableScopeImpl &) = delete;

  TreeScopedHashTableScopeImpl()
      : HT(0), ID(0), ParentScope(0), MovedFrom(true), RefCount(0) {}

  TreeScopedHashTableScopeImpl(TreeScopedHashTable<K, V, AllocatorTy> *HT,
                               TreeScopedHashTableScopeImpl *ParentScope)
      : HT(HT), ID(HT->getNextScopeID()), ParentScope(ParentScope),
        LastValInScope(0), MovedFrom(false), OwnsParentScope(false),
        RefCount(0) {}

  TreeScopedHashTableScopeImpl(TreeScopedHashTableScopeImpl &&Other)
    : HT(Other.HT), ID(Other.ID), ParentScope(Other.ParentScope),
      LastValInScope(Other.LastValInScope), MovedFrom(false),
      OwnsParentScope(Other.OwnsParentScope), RefCount(Other.RefCount) {
    assert(!Other.MovedFrom && "moving from a moved-from scope");
    Other.MovedFrom = true;
    // *Don't* set Other.ID to a trap value so that the old scope object can
    // be still used for lookup.
  }

  void retain() {
    RefCount++;
  }
  void release() {
    RefCount--;
    if (RefCount == 0)
      delete this;
  }

  ~TreeScopedHashTableScopeImpl();
};

/// \brief A scope that was detached from the stack to heap.
template <typename K, typename V, typename AllocatorTy = llvm::MallocAllocator>
class TreeScopedHashTableDetachedScope {
  friend class TreeScopedHashTableScope<K, V, AllocatorTy>;

  typedef TreeScopedHashTableScopeImpl<K, V, AllocatorTy> ImplTy;

  ImplTy *DetachedImpl;

  TreeScopedHashTableDetachedScope(TreeScopedHashTableDetachedScope &) = delete;
  void operator=(TreeScopedHashTableDetachedScope &) = delete;

  TreeScopedHashTableDetachedScope(ImplTy *DetachedImpl)
      : DetachedImpl(DetachedImpl) {
    DetachedImpl->retain();
  }

  const ImplTy *getImpl() { return DetachedImpl; }

public:
  TreeScopedHashTableDetachedScope &operator=(
                            const TreeScopedHashTableDetachedScope &) = default;
  TreeScopedHashTableDetachedScope &operator=(
                                 TreeScopedHashTableDetachedScope &&) = default;

  TreeScopedHashTableDetachedScope() : DetachedImpl(0) {}

  TreeScopedHashTableDetachedScope(TreeScopedHashTableDetachedScope &&Other)
      : DetachedImpl(Other.DetachedImpl) {
    Other.DetachedImpl = 0;
  }

  ~TreeScopedHashTableDetachedScope() {
    if (DetachedImpl)
      DetachedImpl->release();
  }
};

/// \brief A normal hashtable scope.  Objects of this class should be created only
/// on stack.  A normal scope is faster to create than a detached scope because
/// it does not do heap allocation for the reference counted
/// \c TreeScopedHashTableScopeImpl.
template <typename K, typename V, typename AllocatorTy>
class TreeScopedHashTableScope {
  friend class TreeScopedHashTable<K, V, AllocatorTy>;

  typedef TreeScopedHashTableScopeImpl<K, V, AllocatorTy> ImplTy;

  /// Inline storage for a reference-counted scope.
  ImplTy InlineImpl;

  /// Pointer to the reference-counted scope that was detached to the heap.
  ImplTy *DetachedImpl;
  TreeScopedHashTableScope *const ParentScope;

  ImplTy *getImpl() {
    assert(static_cast<bool>(DetachedImpl) == InlineImpl.MovedFrom);
    return InlineImpl.MovedFrom ? DetachedImpl : &InlineImpl;
  }

  const ImplTy *getImpl() const {
    assert(static_cast<bool>(DetachedImpl) == InlineImpl.MovedFrom);
    return InlineImpl.MovedFrom ? DetachedImpl : &InlineImpl;
  }

  TreeScopedHashTableScope(TreeScopedHashTableScope &) = delete;
  void operator=(TreeScopedHashTableScope &) = delete;

public:
  /// Install this as the current scope for the hash table.
  TreeScopedHashTableScope(TreeScopedHashTable<K, V, AllocatorTy> &HT,
                           TreeScopedHashTableScope *ParentScope)
      : InlineImpl(&HT, ParentScope ? ParentScope->getImpl() : 0),
        DetachedImpl(0), ParentScope(ParentScope) {}

  TreeScopedHashTableScope(TreeScopedHashTableDetachedScope<K, V, AllocatorTy> &&DS)
    : DetachedImpl(DS.DetachedImpl), ParentScope(0) {
      DS.DetachedImpl = 0;
    }

  ~TreeScopedHashTableScope() {
    if (DetachedImpl)
      DetachedImpl->release();
  }

  /// \brief Detach this scope to the heap.
  TreeScopedHashTableDetachedScope<K, V, AllocatorTy> detach() {
    if (DetachedImpl)
      return TreeScopedHashTableDetachedScope<K, V, AllocatorTy>(DetachedImpl);

    // Detach all parent scopes recursively.
    if (ParentScope && !ParentScope->DetachedImpl) {
      ParentScope->detach();
      InlineImpl.ParentScope = ParentScope->getImpl();
    }

    DetachedImpl = new ImplTy(std::move(InlineImpl));
    DetachedImpl->retain();
    if (DetachedImpl->ParentScope) {
      DetachedImpl->ParentScope->retain();
      DetachedImpl->OwnsParentScope = true;
    }
    return TreeScopedHashTableDetachedScope<K, V, AllocatorTy>(DetachedImpl);
  }
};

template <typename K, typename V> class TreeScopedHashTableIterator {
  TreeScopedHashTableVal<K, V> *Node;

public:
  TreeScopedHashTableIterator(TreeScopedHashTableVal<K, V> *Node)
      : Node(Node) {}

  V &operator*() const {
    assert(Node && "Dereference end()");
    return Node->getValue();
  }
  V *operator->() const { return &Node->getValue(); }

  bool operator==(const TreeScopedHashTableIterator &RHS) const {
    return Node == RHS.Node;
  }
  bool operator!=(const TreeScopedHashTableIterator &RHS) const {
    return Node != RHS.Node;
  }

  inline TreeScopedHashTableIterator &operator++() { // Preincrement
    assert(Node && "incrementing past end()");
    Node = Node->getNextForKey();
    return *this;
  }
  TreeScopedHashTableIterator operator++(int) { // Postincrement
    TreeScopedHashTableIterator tmp = *this;
    ++*this;
    return tmp;
  }
};

/// \brief A scoped hashtable that can have multiple active scopes.
///
/// There are two kinds of scopes:
///
/// \li TreeScopedHashTableScope -- normal scopes, which should be created on
/// stack.  Obviously, only one such scope can be active at a time.  As soon as
/// the scope object is destroyed, corresponding data from the hashtable is
/// deleted.
///
/// \li TreeScopedHashTableDetachedScope -- detached scopes, can be stored on
/// heap. These can be created from normal scopes by calling \c detach().
/// Detaching a scope transparently detaches all its parent scopes.
/// A detached scope takes ownership of the corresponding data in the
/// hashtable.
///
/// All scopes should be destroyed before hashtable is destroyed.
template <typename K, typename V, typename AllocatorTy>
class TreeScopedHashTable {
public:
  /// This is a helpful typedef that allows clients to get easy access
  /// to the name of the scope for this hash table.
  typedef TreeScopedHashTableScope<K, V, AllocatorTy> ScopeTy;
  typedef TreeScopedHashTableDetachedScope<K, V, AllocatorTy> DetachedScopeTy;

private:
  typedef unsigned ScopeIDTy;
  typedef std::pair<K, ScopeIDTy> KeyTy;
  typedef TreeScopedHashTableVal<K, V> ValTy;
  llvm::DenseMap<KeyTy, ValTy *> TopLevelMap;

  AllocatorTy Allocator;

  ScopeIDTy NextScopeID;

  ScopeIDTy getNextScopeID() {
    return NextScopeID++;
  }

  TreeScopedHashTable(const TreeScopedHashTable &) = delete;
  void operator=(const TreeScopedHashTable &) = delete;
  friend class TreeScopedHashTableScopeImpl<K, V, AllocatorTy>;

public:
  TreeScopedHashTable() : NextScopeID(0) {}
  TreeScopedHashTable(AllocatorTy A) : Allocator(A), NextScopeID(0) {}
  ~TreeScopedHashTable() {
    assert(TopLevelMap.empty() && "Scope imbalance!");
  }

  /// Access to the allocator.
  typedef AllocatorTy &AllocatorRefTy;
  typedef const AllocatorTy &AllocatorCRefTy;
  AllocatorRefTy getAllocator() { return Allocator; }
  AllocatorCRefTy getAllocator() const { return Allocator; }

  bool count(const ScopeTy &S, const K &Key) const {
    const typename ScopeTy::ImplTy *CurrScope = S.getImpl();
    while (CurrScope) {
      if (TopLevelMap.count(std::make_pair(Key, CurrScope->ID))) {
        return true;
      }
      CurrScope = CurrScope->ParentScope;
    }
    return false;
  }

public:
  V lookup(const ScopeTy &S, const K &Key) {
    const typename ScopeTy::ImplTy *CurrScope = S.getImpl();
    while (CurrScope) {
      typename llvm::DenseMap<KeyTy, ValTy *>::iterator I =
          TopLevelMap.find(std::make_pair(Key, CurrScope->ID));
      if (I != TopLevelMap.end())
        return I->second->getValue();
      CurrScope = CurrScope->ParentScope;
    }
    return V();
  }

  typedef TreeScopedHashTableIterator<K, V> iterator;

  iterator end() { return iterator(0); }

  iterator begin(ScopeTy &S, const K &Key) {
    typename llvm::DenseMap<KeyTy, ValTy *>::iterator I =
        TopLevelMap.find(std::make_pair(Key, S.getImpl()->ID));
    if (I == TopLevelMap.end())
      return end();
    return iterator(I->second);
  }

  /// This inserts the specified key/value at the specified
  /// (possibly not the current) scope.  While it is ok to insert into a scope
  /// that isn't the current one, it isn't ok to insert *underneath* an existing
  /// value of the specified key.
  void insertIntoScope(ScopeTy &S, const K &Key, const V &Val) {
    TreeScopedHashTableVal<K, V> *PrevEntry = 0;
    const typename ScopeTy::ImplTy *CurrScope = S.getImpl();
    while (CurrScope) {
      typename llvm::DenseMap<KeyTy, ValTy *>::iterator I =
          TopLevelMap.find(std::make_pair(Key, CurrScope->ID));
      if (I != TopLevelMap.end()) {
        PrevEntry = I->second;
        break;
      }
      CurrScope = CurrScope->ParentScope;
    }
    assert(TopLevelMap.find(std::make_pair(Key, S.getImpl()->ID)) == TopLevelMap.end());

    TreeScopedHashTableVal<K, V> *&KeyEntry =
        TopLevelMap[std::make_pair(Key, S.getImpl()->ID)];
    KeyEntry =
        ValTy::Create(S.getImpl()->LastValInScope, PrevEntry, Key, Val, Allocator);
    S.getImpl()->LastValInScope = KeyEntry;
  }
};

template <typename K, typename V, typename Allocator>
TreeScopedHashTableScopeImpl<K, V, Allocator>::~TreeScopedHashTableScopeImpl() {
  if (MovedFrom)
    return;

  // Pop and delete all values corresponding to this scope.
  while (TreeScopedHashTableVal<K, V> *ThisEntry = LastValInScope) {
    // Pop this value out of the TopLevelMap.
    assert(HT->TopLevelMap[std::make_pair(ThisEntry->getKey(), this->ID)] ==
               ThisEntry &&
           "Scope imbalance!");
    HT->TopLevelMap.erase(std::make_pair(ThisEntry->getKey(), this->ID));

    // Pop this value out of the scope.
    LastValInScope = ThisEntry->getNextInScope();

    // Delete this entry.
    ThisEntry->Destroy(HT->getAllocator());
  }

  if (OwnsParentScope)
    ParentScope->release();
}

} // end namespace swift

#endif // SWIFT_BASIC_TREESCOPEDHASHTABLE_H
