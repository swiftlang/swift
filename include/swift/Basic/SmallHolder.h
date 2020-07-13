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

#ifndef SWIFT_BASIC_SMALLHOLDER_H
#define SWIFT_BASIC_SMALLHOLDER_H

#include "swift/Basic/TypeID.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/MemAlloc.h"
#include <type_traits>

namespace swift {

using llvm::hash_code;
using llvm::hash_value;

class AnyHolder final {
  uint64_t typeID;
  const void *rawStorage;

public:
  explicit AnyHolder(uint64_t typeID, const void *rawStorage)
      : typeID(typeID), rawStorage(rawStorage) {}

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
template <typename VTable, std::size_t Size = sizeof(void *),
          std::size_t Align = alignof(void *)>
class SmallHolder {
  friend llvm::DenseMapInfo<SmallHolder>;
  using SmallStorage = std::aligned_storage_t<Size, Align>;

  static bool is_small(size_t size) {
    return size <= sizeof(SmallStorage);
  }
  template <typename Value> static constexpr bool is_small() {
    return sizeof(Value) <= sizeof(SmallStorage);
  }

  class HeapStorage final {
    HeapStorage() = delete;

    static void *allocate(size_t valueSize) {
      auto refCountOffset = llvm::alignTo(valueSize, alignof(unsigned));
      auto size = refCountOffset + sizeof(unsigned);
      auto *memory = llvm::safe_malloc(size);
      *getRefCountPtr(memory, valueSize) = 1;
      return memory;
    }

    static unsigned *getRefCountPtr(void *ptr, size_t valueSize) {
      assert(ptr && "Didn't allocate?");
      auto refCountOffset = llvm::alignTo(valueSize, alignof(unsigned));
      auto *bytePtr = reinterpret_cast<char *>(ptr);
      return reinterpret_cast<unsigned *>(bytePtr + refCountOffset);
    }

  public:
    template <typename Value, typename T>
    static void *create(T &&value) {
      auto *memory = allocate(sizeof(Value));
      new (memory) Value(std::forward<T>(value));
      return memory;
    }
    static void *create(AnyHolder otherStorage, const VTable *otherVTable) {
      auto *memory = allocate(otherVTable->size);
      otherVTable->copy(otherStorage, memory);
      return memory;
    }

    static void *retain(const SmallHolder &holder) {
      auto *memory = holder.heap;
      *getRefCountPtr(memory, holder.getVTable()->size) += 1;
      return memory;
    }
    static void release(SmallHolder &holder) {
      auto *memory = holder.heap;
      auto *vtable = holder.getVTable();
      auto refCountPtr = getRefCountPtr(memory, vtable->size);
      *refCountPtr -= 1;
      if (*refCountPtr == 0) {
        vtable->deleter(holder);
        std::free(memory);
      }
    }
  };

  union {
    void *heap;
    SmallStorage small;
    const void *unowned;
  };

  enum StorageKind : uint8_t {
    Empty,
    Tombstone,
    Heap,
    Small,
    UnownedRef
  };

  llvm::PointerIntPair<const VTable *, 3, StorageKind> vtableAndKind;

  explicit SmallHolder(StorageKind storageKind) {
    heap = nullptr;
    setStorageKind(storageKind);
    assert(!hasStorage());
  }

  explicit SmallHolder(const void *unownedRef, const VTable *vtable) {
    unowned = unownedRef;
    setStorageKind(StorageKind::UnownedRef);
    vtableAndKind.setPointer(vtable);
  }

public:
  template <typename Value>
  static SmallHolder withUnownedRef(const Value &val) {
    static_assert(alignof(Value) <= Align, "Insufficiently aligned for value");
    return SmallHolder(&val, VTable::template get<Value>());
  }

  template <typename T, typename Value = std::decay_t<T>,
            typename = typename std::enable_if<
                !std::is_same<Value, SmallHolder>::value>::type>
  explicit SmallHolder(T &&value) {
    static_assert(alignof(Value) <= Align, "Insufficiently aligned for value");
    if (is_small<Value>()) {
      setStorageKind(StorageKind::Small);
      new (&small) Value(std::forward<T>(value));
    } else {
      setStorageKind(StorageKind::Heap);
      heap = HeapStorage::template create<Value>(std::forward<T>(value));
    }
    vtableAndKind.setPointer(VTable::template get<Value>());
  }

  explicit SmallHolder(AnyHolder otherStorage, const VTable *otherVTable) {
    if (is_small(otherVTable->size)) {
      setStorageKind(StorageKind::Small);
      otherVTable->copy(otherStorage, &small);
    } else {
      setStorageKind(StorageKind::Heap);
      heap = HeapStorage::create(otherStorage, otherVTable);
    }
    vtableAndKind.setPointer(otherVTable);
  }

  SmallHolder makePersistant() const {
    assert(hasStorage() && "Not storing anything");
    assert(getStorageKind() == StorageKind::UnownedRef && "Already persistant");
    return SmallHolder(*this, getVTable());
  }

  SmallHolder(const SmallHolder &other) : vtableAndKind(other.vtableAndKind) {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      break;
    case StorageKind::Heap:
      heap = HeapStorage::retain(other);
      break;
    case StorageKind::Small:
      other.getVTable()->copy(other, &small);
      break;
    case StorageKind::UnownedRef:
      unowned = other.unowned;
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
    case StorageKind::Small:
      small = other.small;
      break;
    case StorageKind::UnownedRef:
      unowned = other.unowned;
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
      HeapStorage::release(*this);
      break;
    case StorageKind::Small:
      getVTable()->deleter(*this);
      break;
    case StorageKind::UnownedRef:
      // Nothing to do.
      break;
    }
  }

  bool hasStorage() const {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      return false;
    case StorageKind::Small:
    case StorageKind::Heap:
    case StorageKind::UnownedRef:
      return true;
    }
    llvm_unreachable("Unhandled case in switch");
  }

  operator AnyHolder() const {
    return AnyHolder(getVTable()->typeID, getRawStorage());
  }

private:
  StorageKind getStorageKind() const { return vtableAndKind.getInt(); }
  void setStorageKind(StorageKind kind) { vtableAndKind.setInt(kind); }

  const void *getRawStorage() const {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      llvm_unreachable("Has no storage");
    case StorageKind::Small:
      return &small;
    case StorageKind::Heap:
      return heap;
    case StorageKind::UnownedRef:
      return unowned;
    }
  }

  void *getRawStorage() {
    const auto *constThis = this;
    return const_cast<void *>(constThis->getRawStorage());
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

  template <typename OtherVTable, size_t OtherSize, size_t OtherAlign>
  bool isStorageEqual(
      const SmallHolder<OtherVTable, OtherSize, OtherAlign> &other) const {
    assert(hasStorage() && other.hasStorage());

    if (getVTable()->typeID != other.getVTable()->typeID)
      return false;

    return getVTable()->isEqual(*this, other);
  }

  /// Compare two instances for equality.
  friend bool operator==(const SmallHolder &lhs, const SmallHolder &rhs) {
    if (lhs.getStorageKind() != rhs.getStorageKind())
      return false;

    if (!lhs.hasStorage())
      return true;

    return lhs.isStorageEqual(rhs);
  }

  friend bool operator!=(const SmallHolder &lhs, const SmallHolder &rhs) {
    return !(lhs == rhs);
  }

  friend hash_code hash_value(const SmallHolder &holder) {
    if (!holder.hasStorage())
      return 1;

    auto valueHash = holder.getVTable()->getHash(holder);
    return hash_combine(holder.getTypeID(), valueHash);
  }

  static SmallHolder getEmptyKey() {
    return SmallHolder(StorageKind::Empty);
  }

  static SmallHolder getTombstoneKey() {
    return SmallHolder(StorageKind::Tombstone);
  }
};

} // end namespace swift

namespace llvm {
template <typename VTable, std::size_t Size, std::size_t Align>
struct DenseMapInfo<swift::SmallHolder<VTable, Size, Align>> {
  using SmallHolder = swift::SmallHolder<VTable, Size, Align>;
  using StorageKind = typename SmallHolder::StorageKind;

  static inline SmallHolder getEmptyKey() {
    return SmallHolder::getEmptyKey();
  }
  static inline SmallHolder getTombstoneKey() {
    return SmallHolder::getTombstoneKey();
  }

  static bool isEqual(const SmallHolder &lhs, const SmallHolder &rhs) {
    return lhs == rhs;
  }
  static unsigned getHashValue(const SmallHolder &holder) {
    return hash_value(holder);
  }
};

} // end namespace llvm

#endif // SWIFT_BASIC_SMALLHOLDER_H
