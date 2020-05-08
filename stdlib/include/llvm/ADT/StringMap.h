//===- StringMap.h - String Hash table map interface ------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the StringMap class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_STRINGMAP_H
#define LLVM_ADT_STRINGMAP_H

#include "llvm/ADT/StringMapEntry.h"
#include "llvm/Support/AllocatorBase.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <initializer_list>
#include <iterator>

namespace llvm {

template <typename ValueTy> class StringMapConstIterator;
template <typename ValueTy> class StringMapIterator;
template <typename ValueTy> class StringMapKeyIterator;

/// StringMapImpl - This is the base class of StringMap that is shared among
/// all of its instantiations.
class StringMapImpl {
protected:
  // Array of NumBuckets pointers to entries, null pointers are holes.
  // TheTable[NumBuckets] contains a sentinel value for easy iteration. Followed
  // by an array of the actual hash values as unsigned integers.
  StringMapEntryBase **TheTable = nullptr;
  unsigned NumBuckets = 0;
  unsigned NumItems = 0;
  unsigned NumTombstones = 0;
  unsigned ItemSize;

protected:
  explicit StringMapImpl(unsigned itemSize) : ItemSize(itemSize) {}
  StringMapImpl(StringMapImpl &&RHS)
      : TheTable(RHS.TheTable), NumBuckets(RHS.NumBuckets),
        NumItems(RHS.NumItems), NumTombstones(RHS.NumTombstones),
        ItemSize(RHS.ItemSize) {
    RHS.TheTable = nullptr;
    RHS.NumBuckets = 0;
    RHS.NumItems = 0;
    RHS.NumTombstones = 0;
  }

  StringMapImpl(unsigned InitSize, unsigned ItemSize);
  unsigned RehashTable(unsigned BucketNo = 0);

  /// LookupBucketFor - Look up the bucket that the specified string should end
  /// up in.  If it already exists as a key in the map, the Item pointer for the
  /// specified bucket will be non-null.  Otherwise, it will be null.  In either
  /// case, the FullHashValue field of the bucket will be set to the hash value
  /// of the string.
  unsigned LookupBucketFor(StringRef Key);

  /// FindKey - Look up the bucket that contains the specified key. If it exists
  /// in the map, return the bucket number of the key.  Otherwise return -1.
  /// This does not modify the map.
  int FindKey(StringRef Key) const;

  /// RemoveKey - Remove the specified StringMapEntry from the table, but do not
  /// delete it.  This aborts if the value isn't in the table.
  void RemoveKey(StringMapEntryBase *V);

  /// RemoveKey - Remove the StringMapEntry for the specified key from the
  /// table, returning it.  If the key is not in the table, this returns null.
  StringMapEntryBase *RemoveKey(StringRef Key);

  /// Allocate the table with the specified number of buckets and otherwise
  /// setup the map as empty.
  void init(unsigned Size);

public:
  static StringMapEntryBase *getTombstoneVal() {
    uintptr_t Val = static_cast<uintptr_t>(-1);
    Val <<= PointerLikeTypeTraits<StringMapEntryBase *>::NumLowBitsAvailable;
    return reinterpret_cast<StringMapEntryBase *>(Val);
  }

  unsigned getNumBuckets() const { return NumBuckets; }
  unsigned getNumItems() const { return NumItems; }

  bool empty() const { return NumItems == 0; }
  unsigned size() const { return NumItems; }

  void swap(StringMapImpl &Other) {
    std::swap(TheTable, Other.TheTable);
    std::swap(NumBuckets, Other.NumBuckets);
    std::swap(NumItems, Other.NumItems);
    std::swap(NumTombstones, Other.NumTombstones);
  }
};

/// StringMap - This is an unconventional map that is specialized for handling
/// keys that are "strings", which are basically ranges of bytes. This does some
/// funky memory allocation and hashing things to make it extremely efficient,
/// storing the string data *after* the value in the map.
template <typename ValueTy, typename AllocatorTy = MallocAllocator>
class StringMap : public StringMapImpl {
  AllocatorTy Allocator;

public:
  using MapEntryTy = StringMapEntry<ValueTy>;

  StringMap() : StringMapImpl(static_cast<unsigned>(sizeof(MapEntryTy))) {}

  explicit StringMap(unsigned InitialSize)
      : StringMapImpl(InitialSize, static_cast<unsigned>(sizeof(MapEntryTy))) {}

  explicit StringMap(AllocatorTy A)
      : StringMapImpl(static_cast<unsigned>(sizeof(MapEntryTy))), Allocator(A) {
  }

  StringMap(unsigned InitialSize, AllocatorTy A)
      : StringMapImpl(InitialSize, static_cast<unsigned>(sizeof(MapEntryTy))),
        Allocator(A) {}

  StringMap(std::initializer_list<std::pair<StringRef, ValueTy>> List)
      : StringMapImpl(List.size(), static_cast<unsigned>(sizeof(MapEntryTy))) {
    for (const auto &P : List) {
      insert(P);
    }
  }

  StringMap(StringMap &&RHS)
      : StringMapImpl(std::move(RHS)), Allocator(std::move(RHS.Allocator)) {}

  StringMap(const StringMap &RHS)
      : StringMapImpl(static_cast<unsigned>(sizeof(MapEntryTy))),
        Allocator(RHS.Allocator) {
    if (RHS.empty())
      return;

    // Allocate TheTable of the same size as RHS's TheTable, and set the
    // sentinel appropriately (and NumBuckets).
    init(RHS.NumBuckets);
    unsigned *HashTable = (unsigned *)(TheTable + NumBuckets + 1),
             *RHSHashTable = (unsigned *)(RHS.TheTable + NumBuckets + 1);

    NumItems = RHS.NumItems;
    NumTombstones = RHS.NumTombstones;
    for (unsigned I = 0, E = NumBuckets; I != E; ++I) {
      StringMapEntryBase *Bucket = RHS.TheTable[I];
      if (!Bucket || Bucket == getTombstoneVal()) {
        TheTable[I] = Bucket;
        continue;
      }

      TheTable[I] = MapEntryTy::Create(
          static_cast<MapEntryTy *>(Bucket)->getKey(), Allocator,
          static_cast<MapEntryTy *>(Bucket)->getValue());
      HashTable[I] = RHSHashTable[I];
    }

    // Note that here we've copied everything from the RHS into this object,
    // tombstones included. We could, instead, have re-probed for each key to
    // instantiate this new object without any tombstone buckets. The
    // assumption here is that items are rarely deleted from most StringMaps,
    // and so tombstones are rare, so the cost of re-probing for all inputs is
    // not worthwhile.
  }

  StringMap &operator=(StringMap RHS) {
    StringMapImpl::swap(RHS);
    std::swap(Allocator, RHS.Allocator);
    return *this;
  }

  ~StringMap() {
    // Delete all the elements in the map, but don't reset the elements
    // to default values.  This is a copy of clear(), but avoids unnecessary
    // work not required in the destructor.
    if (!empty()) {
      for (unsigned I = 0, E = NumBuckets; I != E; ++I) {
        StringMapEntryBase *Bucket = TheTable[I];
        if (Bucket && Bucket != getTombstoneVal()) {
          static_cast<MapEntryTy *>(Bucket)->Destroy(Allocator);
        }
      }
    }
    free(TheTable);
  }

  AllocatorTy &getAllocator() { return Allocator; }
  const AllocatorTy &getAllocator() const { return Allocator; }

  using key_type = const char *;
  using mapped_type = ValueTy;
  using value_type = StringMapEntry<ValueTy>;
  using size_type = size_t;

  using const_iterator = StringMapConstIterator<ValueTy>;
  using iterator = StringMapIterator<ValueTy>;

  iterator begin() { return iterator(TheTable, NumBuckets == 0); }
  iterator end() { return iterator(TheTable + NumBuckets, true); }
  const_iterator begin() const {
    return const_iterator(TheTable, NumBuckets == 0);
  }
  const_iterator end() const {
    return const_iterator(TheTable + NumBuckets, true);
  }

  iterator_range<StringMapKeyIterator<ValueTy>> keys() const {
    return make_range(StringMapKeyIterator<ValueTy>(begin()),
                      StringMapKeyIterator<ValueTy>(end()));
  }

  iterator find(StringRef Key) {
    int Bucket = FindKey(Key);
    if (Bucket == -1)
      return end();
    return iterator(TheTable + Bucket, true);
  }

  const_iterator find(StringRef Key) const {
    int Bucket = FindKey(Key);
    if (Bucket == -1)
      return end();
    return const_iterator(TheTable + Bucket, true);
  }

  /// lookup - Return the entry for the specified key, or a default
  /// constructed value if no such entry exists.
  ValueTy lookup(StringRef Key) const {
    const_iterator it = find(Key);
    if (it != end())
      return it->second;
    return ValueTy();
  }

  /// Lookup the ValueTy for the \p Key, or create a default constructed value
  /// if the key is not in the map.
  ValueTy &operator[](StringRef Key) { return try_emplace(Key).first->second; }

  /// count - Return 1 if the element is in the map, 0 otherwise.
  size_type count(StringRef Key) const { return find(Key) == end() ? 0 : 1; }

  template <typename InputTy>
  size_type count(const StringMapEntry<InputTy> &MapEntry) const {
    return count(MapEntry.getKey());
  }

  /// insert - Insert the specified key/value pair into the map.  If the key
  /// already exists in the map, return false and ignore the request, otherwise
  /// insert it and return true.
  bool insert(MapEntryTy *KeyValue) {
    unsigned BucketNo = LookupBucketFor(KeyValue->getKey());
    StringMapEntryBase *&Bucket = TheTable[BucketNo];
    if (Bucket && Bucket != getTombstoneVal())
      return false; // Already exists in map.

    if (Bucket == getTombstoneVal())
      --NumTombstones;
    Bucket = KeyValue;
    ++NumItems;
    assert(NumItems + NumTombstones <= NumBuckets);

    RehashTable();
    return true;
  }

  /// insert - Inserts the specified key/value pair into the map if the key
  /// isn't already in the map. The bool component of the returned pair is true
  /// if and only if the insertion takes place, and the iterator component of
  /// the pair points to the element with key equivalent to the key of the pair.
  std::pair<iterator, bool> insert(std::pair<StringRef, ValueTy> KV) {
    return try_emplace(KV.first, std::move(KV.second));
  }

  /// Inserts an element or assigns to the current element if the key already
  /// exists. The return type is the same as try_emplace.
  template <typename V>
  std::pair<iterator, bool> insert_or_assign(StringRef Key, V &&Val) {
    auto Ret = try_emplace(Key, std::forward<V>(Val));
    if (!Ret.second)
      Ret.first->second = std::forward<V>(Val);
    return Ret;
  }

  /// Emplace a new element for the specified key into the map if the key isn't
  /// already in the map. The bool component of the returned pair is true
  /// if and only if the insertion takes place, and the iterator component of
  /// the pair points to the element with key equivalent to the key of the pair.
  template <typename... ArgsTy>
  std::pair<iterator, bool> try_emplace(StringRef Key, ArgsTy &&... Args) {
    unsigned BucketNo = LookupBucketFor(Key);
    StringMapEntryBase *&Bucket = TheTable[BucketNo];
    if (Bucket && Bucket != getTombstoneVal())
      return std::make_pair(iterator(TheTable + BucketNo, false),
                            false); // Already exists in map.

    if (Bucket == getTombstoneVal())
      --NumTombstones;
    Bucket = MapEntryTy::Create(Key, Allocator, std::forward<ArgsTy>(Args)...);
    ++NumItems;
    assert(NumItems + NumTombstones <= NumBuckets);

    BucketNo = RehashTable(BucketNo);
    return std::make_pair(iterator(TheTable + BucketNo, false), true);
  }

  // clear - Empties out the StringMap
  void clear() {
    if (empty())
      return;

    // Zap all values, resetting the keys back to non-present (not tombstone),
    // which is safe because we're removing all elements.
    for (unsigned I = 0, E = NumBuckets; I != E; ++I) {
      StringMapEntryBase *&Bucket = TheTable[I];
      if (Bucket && Bucket != getTombstoneVal()) {
        static_cast<MapEntryTy *>(Bucket)->Destroy(Allocator);
      }
      Bucket = nullptr;
    }

    NumItems = 0;
    NumTombstones = 0;
  }

  /// remove - Remove the specified key/value pair from the map, but do not
  /// erase it.  This aborts if the key is not in the map.
  void remove(MapEntryTy *KeyValue) { RemoveKey(KeyValue); }

  void erase(iterator I) {
    MapEntryTy &V = *I;
    remove(&V);
    V.Destroy(Allocator);
  }

  bool erase(StringRef Key) {
    iterator I = find(Key);
    if (I == end())
      return false;
    erase(I);
    return true;
  }
};

template <typename DerivedTy, typename ValueTy>
class StringMapIterBase
    : public iterator_facade_base<DerivedTy, std::forward_iterator_tag,
                                  ValueTy> {
protected:
  StringMapEntryBase **Ptr = nullptr;

public:
  StringMapIterBase() = default;

  explicit StringMapIterBase(StringMapEntryBase **Bucket,
                             bool NoAdvance = false)
      : Ptr(Bucket) {
    if (!NoAdvance)
      AdvancePastEmptyBuckets();
  }

  DerivedTy &operator=(const DerivedTy &Other) {
    Ptr = Other.Ptr;
    return static_cast<DerivedTy &>(*this);
  }

  bool operator==(const DerivedTy &RHS) const { return Ptr == RHS.Ptr; }

  DerivedTy &operator++() { // Preincrement
    ++Ptr;
    AdvancePastEmptyBuckets();
    return static_cast<DerivedTy &>(*this);
  }

  DerivedTy operator++(int) { // Post-increment
    DerivedTy Tmp(Ptr);
    ++*this;
    return Tmp;
  }

private:
  void AdvancePastEmptyBuckets() {
    while (*Ptr == nullptr || *Ptr == StringMapImpl::getTombstoneVal())
      ++Ptr;
  }
};

template <typename ValueTy>
class StringMapConstIterator
    : public StringMapIterBase<StringMapConstIterator<ValueTy>,
                               const StringMapEntry<ValueTy>> {
  using base = StringMapIterBase<StringMapConstIterator<ValueTy>,
                                 const StringMapEntry<ValueTy>>;

public:
  StringMapConstIterator() = default;
  explicit StringMapConstIterator(StringMapEntryBase **Bucket,
                                  bool NoAdvance = false)
      : base(Bucket, NoAdvance) {}

  const StringMapEntry<ValueTy> &operator*() const {
    return *static_cast<const StringMapEntry<ValueTy> *>(*this->Ptr);
  }
};

template <typename ValueTy>
class StringMapIterator : public StringMapIterBase<StringMapIterator<ValueTy>,
                                                   StringMapEntry<ValueTy>> {
  using base =
      StringMapIterBase<StringMapIterator<ValueTy>, StringMapEntry<ValueTy>>;

public:
  StringMapIterator() = default;
  explicit StringMapIterator(StringMapEntryBase **Bucket,
                             bool NoAdvance = false)
      : base(Bucket, NoAdvance) {}

  StringMapEntry<ValueTy> &operator*() const {
    return *static_cast<StringMapEntry<ValueTy> *>(*this->Ptr);
  }

  operator StringMapConstIterator<ValueTy>() const {
    return StringMapConstIterator<ValueTy>(this->Ptr, true);
  }
};

template <typename ValueTy>
class StringMapKeyIterator
    : public iterator_adaptor_base<StringMapKeyIterator<ValueTy>,
                                   StringMapConstIterator<ValueTy>,
                                   std::forward_iterator_tag, StringRef> {
  using base = iterator_adaptor_base<StringMapKeyIterator<ValueTy>,
                                     StringMapConstIterator<ValueTy>,
                                     std::forward_iterator_tag, StringRef>;

public:
  StringMapKeyIterator() = default;
  explicit StringMapKeyIterator(StringMapConstIterator<ValueTy> Iter)
      : base(std::move(Iter)) {}

  StringRef &operator*() {
    Key = this->wrapped()->getKey();
    return Key;
  }

private:
  StringRef Key;
};

} // end namespace llvm

#endif // LLVM_ADT_STRINGMAP_H
