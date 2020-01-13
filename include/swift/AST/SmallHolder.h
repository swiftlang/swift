//===--- SmallHolder.h - Type erasing wrapper -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the SmallHolder class, which holds a type erased value,
//  optimized for the case where the value is small.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SMALLHOLDER_H
#define SWIFT_AST_SMALLHOLDER_H

#include "swift/Basic/TypeID.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/MemAlloc.h"
#include <type_traits>

namespace swift {

using llvm::hash_code;
using llvm::hash_value;

class AnyHolder final {
  uint64_t typeID;
  const void *rawStorage;

public:
  template <typename Holder>
  AnyHolder(const Holder &holder)
      : typeID(holder.getVTable()->typeID), rawStorage(holder.getRawStorage()) {
  }

  /// Retrieve the underlying value, casted to its known concrete type.
  template <typename Value> const Value &get() const {
    assert(TypeID<Value>::value == typeID && "Wrong type provided");
    return *static_cast<const Value *>(rawStorage);
  }

  template <typename Value> Value &getMutable() const {
    assert(TypeID<Value>::value == typeID && "Wrong type provided");
    return *const_cast<Value *>(static_cast<const Value *>(rawStorage));
  }
};

/// Holds a type erased value, optimized for the case where the value is small.
///
/// The \c Size and \c Align templates determine the layout of SmallHolder's
/// inline  The \c VTable template is used to provide a set of
/// function pointers which operate on the concrete underlying value.
template <std::size_t Size, std::size_t Align, typename VTable>
class SmallHolder {
  friend llvm::DenseMapInfo<SmallHolder>;
  friend class AnyHolder;

  using SmallStorage = std::aligned_storage_t<Size, Align>;

  template <typename Value> static constexpr bool is_small() {
    return sizeof(Value) <= sizeof(SmallStorage);
  }

  class HeapStorage final {
    unsigned refCount = 1;
    HeapStorage() {}

  public:
    template <typename Value, typename T>
    static HeapStorage *create(T &&value) {
      auto size = llvm::alignTo(sizeof(HeapStorage), Align) + sizeof(Value);
      auto *memory = reinterpret_cast<HeapStorage *>(llvm::safe_malloc(size));
      new (memory) HeapStorage();
      new (memory->getRawStorage()) Value(std::forward<T>(value));
      return memory;
    }

    void *getRawStorage() {
      auto endPtr = this + 1;
      return reinterpret_cast<void *>(llvm::alignAddr(endPtr, Align));
    }

    HeapStorage *retain() {
      refCount += 1;
      return this;
    }
    void release(SmallHolder &holder) {
      refCount -= 1;
      if (refCount == 0) {
        holder.getVTable()->deleter(holder);
        this->~HeapStorage();
        std::free(this);
      }
    }
  };

  union {
    HeapStorage *heap;
    SmallStorage stack;
  };

  enum StorageKind : uint8_t {
    Empty,
    Tombstone,
    Heap,
    Stack,
  };

  llvm::PointerIntPair<const VTable *, 2, StorageKind> vtableAndKind;

  explicit SmallHolder(StorageKind storageKind) {
    heap = nullptr;
    setStorageKind(storageKind);
    assert(!hasStorage());
  }

public:
  template <typename T, typename Value = std::decay_t<T>,
            typename = typename std::enable_if<
                !std::is_same<Value, SmallHolder>::value>::type>
  explicit SmallHolder(T &&value) {
    if (is_small<Value>()) {
      setStorageKind(StorageKind::Stack);
      new (&stack) Value(std::forward<T>(value));
    } else {
      setStorageKind(StorageKind::Heap);
      heap = HeapStorage::template create<Value>(std::forward<T>(value));
    }
    vtableAndKind.setPointer(VTable::template get<Value>());
  }

  SmallHolder(const SmallHolder &other) : vtableAndKind(other.vtableAndKind) {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      break;
    case StorageKind::Heap:
      heap = other.heap->retain();
      break;
    case StorageKind::Stack:
      other.getVTable()->copy(other, &stack);
      break;
    }
  }
  SmallHolder(SmallHolder &&other) : vtableAndKind(other.vtableAndKind) {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      break;
    case StorageKind::Heap:
      heap = other.heap;
      break;
    case StorageKind::Stack:
      stack = other.stack;
      break;
    }

    // Now that we've taken other's storage, overwrite it with an empty holder.
    new (&other) SmallHolder(StorageKind::Empty);
  }

  SmallHolder &operator=(SmallHolder &&other) {
    if (&other != this) {
      this->~SmallHolder();
      new (this) SmallHolder(std::move(other));
    }
    return *this;
  }
  SmallHolder &operator=(SmallHolder const &other) {
    if (&other != this) {
      this->~SmallHolder();
      new (this) SmallHolder(other);
    }
    return *this;
  }

  ~SmallHolder() {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      break;
    case StorageKind::Heap:
      assert(heap && "Didn't allocate?");
      heap->release(*this);
      break;
    case StorageKind::Stack:
      getVTable()->deleter(*this);
      break;
    }
  }

private:
  StorageKind getStorageKind() const { return vtableAndKind.getInt(); }
  void setStorageKind(StorageKind kind) { vtableAndKind.setInt(kind); }

  bool hasStorage() const {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      return false;
    case StorageKind::Stack:
    case StorageKind::Heap:
      return true;
    }
    llvm_unreachable("Unhandled case in switch");
  }

  bool isOnHeap() const {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      llvm_unreachable("Shouldn't be querying an empty holder");
    case StorageKind::Stack:
      return false;
    case StorageKind::Heap:
      return true;
    }
    llvm_unreachable("Unhandled case in switch");
  }

  void *getRawStorage() {
    return isOnHeap() ? heap->getRawStorage() : &stack;
  }

  const void *getRawStorage() const {
    return isOnHeap() ? heap->getRawStorage() : &stack;
  }

public:
  /// Retrieve the underlying value, casted to its known concrete type.
  template <typename Value> Value *get() {
    assert(TypeID<Value>::value == getTypeID() && "Wrong type provided");
    return static_cast<Value *>(getRawStorage());
  }

  /// Retrieve the underlying value, casted to its known concrete type.
  template <typename Value> const Value *get() const {
    assert(TypeID<Value>::value == getTypeID() && "Wrong type provided");
    return static_cast<const Value *>(getRawStorage());
  }

  /// Try casting to a specific (known) type, returning \c nullptr on
  /// failure.
  template <typename Value> const Value *getAs() const {
    if (TypeID<Value>::value != getTypeID())
      return nullptr;

    return static_cast<const Value *>(getRawStorage());
  }

  /// Retrieve the vtable, which can be used to perform operations on the
  /// concrete underlying value.
  const VTable *getVTable() const {
    assert(hasStorage() && "Shouldn't be querying an empty holder");
    return vtableAndKind.getPointer();
  }

  /// Retrieve the TypeID of the underlying value.
  uint64_t getTypeID() const { return getVTable()->typeID; }

  /// Compare two instances for equality.
  friend bool operator==(const SmallHolder &lhs, const SmallHolder &rhs) {
    if (lhs.getStorageKind() != rhs.getStorageKind())
      return false;

    if (!lhs.hasStorage())
      return true;

    if (lhs.getTypeID() != rhs.getTypeID())
      return false;

    return lhs.getVTable()->isEqual(lhs, rhs);
  }

  friend bool operator!=(const SmallHolder &lhs, const SmallHolder &rhs) {
    return !(lhs == rhs);
  }

  static SmallHolder getEmptyKey() {
    return SmallHolder(StorageKind::Empty);
  }

  static SmallHolder getTombstoneKey() {
    return SmallHolder(StorageKind::Tombstone);
  }
};

template <std::size_t Size, std::size_t Align, typename VTable>
hash_code hash_value(const SmallHolder<Size, Align, VTable> &holder) {
  return llvm::DenseMapInfo<SmallHolder<Size, Align, VTable>>::getHashValue(
      holder);
}

} // end namespace swift

namespace llvm {
template <std::size_t Size, std::size_t Align, typename VTable>
struct DenseMapInfo<swift::SmallHolder<Size, Align, VTable>> {
  using SmallHolder = swift::SmallHolder<Size, Align, VTable>;
  using StorageKind = typename SmallHolder::StorageKind;

  static inline SmallHolder getEmptyKey() { return SmallHolder::getEmptyKey(); }
  static inline SmallHolder getTombstoneKey() {
    return SmallHolder::getTombstoneKey();
  }
  static unsigned getHashValue(const SmallHolder &holder) {
    if (!holder.hasStorage())
      return 1;

    auto valueHash = holder.getVTable()->getHash(holder);
    return hash_combine(holder.getTypeID(), valueHash);
  }
  template <typename Value> static unsigned getHashValue(const Value &value) {
    return hash_combine(swift::TypeID<Value>::value, hash_value(value));
  }
  static bool isEqual(const SmallHolder &lhs, const SmallHolder &rhs) {
    return lhs == rhs;
  }

  template <typename Value>
  static bool isEqual(const Value &value, const SmallHolder &holder) {
    // Never equal if the holder isn't storing anything.
    if (!holder.hasStorage())
      return false;

    auto *rhs = holder.template getAs<Value>();
    return rhs && value == *rhs;
  }
};

} // end namespace llvm

#endif // SWIFT_AST_SMALLHOLDER_H
