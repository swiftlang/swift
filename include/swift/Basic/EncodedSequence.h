//===--- EncodedSequence.h - A byte-encoded sequence -----------*- C++ -*-===//
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
//
//  This file defines a data structure which stores a heterogeneous
//  sequence of byte-encoded values.
//
//  The data structure is optimized to minimize its required storage
//  under the assumption that the sequence is usually very short.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ENCODEDSEQUENCE_H
#define SWIFT_BASIC_ENCODEDSEQUENCE_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Host.h"

namespace swift {

/// A base class which handles most of the memory management.
class EncodedSequenceBase {
public:
  /// A chunk of individual path data.  A pointer to this type will be
  /// used to access the Data field, so either this type needs to have
  /// equivalent aliasing power to 'char' or that constraint must be
  /// expressible in some other way.
  typedef unsigned char Chunk;

private:
  enum {
    IsInline = 0x1,
    InitialDataValue = IsInline,

    IndexBitsPerChunk = CHAR_BIT * sizeof(Chunk) - 1,
    IndexContinuesMask = 1 << IndexBitsPerChunk,
    IndexChunkMask = IndexContinuesMask - 1,
  };

  /// Either a Chunk* or an array of Chunks.
  uintptr_t Data;

  /// Return whether we're using out-of-line storage.
  bool hasOutOfLineStorage() const {
    return !(Data & IsInline);
  }

  unsigned getInlineSize() const {
    assert(!hasOutOfLineStorage());
    return (Data & 0xFF) >> 1;
  }
  void setInlineSize(unsigned size) {
    assert(!hasOutOfLineStorage());
    Data = (Data & ~uintptr_t(0xFF)) | (size << 1) | IsInline;
  }

  /// A structure representing out-of-line chunk storage.
  struct OutOfLineStorage {
    uint16_t Length;
    uint16_t Capacity;

    OutOfLineStorage(size_t capacity) : Length(0), Capacity(capacity) {}

    OutOfLineStorage *clone(size_t newCapacity) const {
      size_t newSize = sizeof(OutOfLineStorage) + newCapacity * sizeof(Chunk);
      auto newData = new (operator new(newSize)) OutOfLineStorage(newCapacity);
      newData->Length = Length;
      memcpy(newData->chunkStorage().data(), chunks().data(),
             newCapacity * sizeof(Chunk));
      return newData;
    }
    static OutOfLineStorage *alloc(size_t newSizeInBytes) {
      auto capacity =
        (newSizeInBytes - sizeof(OutOfLineStorage)) / sizeof(Chunk);
      return new (operator new(newSizeInBytes)) OutOfLineStorage(capacity);
    }

    MutableArrayRef<Chunk> chunkStorage() {
      return MutableArrayRef<Chunk>(reinterpret_cast<Chunk*>(this+1), Capacity);
    }
    ArrayRef<Chunk> chunkStorage() const {
      return ArrayRef<Chunk>(reinterpret_cast<const Chunk*>(this+1), Capacity);
    }
    ArrayRef<Chunk> chunks() const {
      return ArrayRef<Chunk>(reinterpret_cast<const Chunk*>(this+1), Length);
    }
  };
  OutOfLineStorage *getOutOfLineStorage() {
    assert(hasOutOfLineStorage());
    return reinterpret_cast<OutOfLineStorage*>(Data);
  }
  const OutOfLineStorage *getOutOfLineStorage() const {
    return const_cast<EncodedSequenceBase*>(this)->getOutOfLineStorage();
  }

  /// Return the array of possibly-initialized storage chunks.
  /// The bound is the capacity of the storage.
  MutableArrayRef<Chunk> chunkStorage() {
    if (hasOutOfLineStorage()) return getOutOfLineStorage()->chunkStorage();

    // The legality of accesses from this cast depends on Chunk having
    // all-powerful aliasing rights.
    Chunk *array = reinterpret_cast<Chunk*>(&Data);
    if (llvm::sys::IsLittleEndianHost) array++;
    return MutableArrayRef<Chunk>(array, sizeof(Data) / sizeof(Chunk) - 1);
  }
  ArrayRef<Chunk> chunkStorage() const {
    return const_cast<EncodedSequenceBase*>(this)->chunkStorage();
  }

  void cloneOutOfLineStorage() {
    auto oldStorage = getOutOfLineStorage();
    Data = reinterpret_cast<uintptr_t>(oldStorage->clone(oldStorage->Capacity));
  }
  void destroyOutOfLineStorage() {
    auto oldStorage = getOutOfLineStorage();
    delete oldStorage;
  }

public:
  EncodedSequenceBase() : Data(InitialDataValue) {}

  EncodedSequenceBase(const EncodedSequenceBase &other) : Data(other.Data) {
    if (hasOutOfLineStorage()) cloneOutOfLineStorage();
  }
  EncodedSequenceBase(EncodedSequenceBase &&other) : Data(other.Data) {
    other.Data = InitialDataValue;
  }
  EncodedSequenceBase &operator=(const EncodedSequenceBase &other) {
    if (hasOutOfLineStorage()) {
      // Try to copy into the existing storage.
      auto otherChunks = other.chunks();
      auto selfStorage = getOutOfLineStorage();
      if (otherChunks.size() <= selfStorage->Capacity) {
        memcpy(selfStorage->chunkStorage().data(), otherChunks.data(),
               sizeof(Chunk) * otherChunks.size());
        selfStorage->Length = otherChunks.size();
        return *this;
      }

      // Otherwise, destroy the existing storage.
      destroyOutOfLineStorage();
    }

    Data = other.Data;
    if (hasOutOfLineStorage()) cloneOutOfLineStorage();
    return *this;
  }
  EncodedSequenceBase &operator=(EncodedSequenceBase &&other) {
    if (hasOutOfLineStorage()) destroyOutOfLineStorage();

    Data = other.Data;
    other.Data = InitialDataValue;
    return *this;
  }

  ~EncodedSequenceBase() {
    if (hasOutOfLineStorage()) destroyOutOfLineStorage();
  }

protected:
  /// Return the array of initialized storage chunks.
  /// The bound is the actual length of the storage.
  ArrayRef<Chunk> chunks() const {
    if (hasOutOfLineStorage()) {
      return getOutOfLineStorage()->chunks();
    } else {
      return chunkStorage().slice(0, getInlineSize());
    }
  }

  /// Claim N more storage chunks, reallocating the storage if necessary.
  MutableArrayRef<Chunk> claimStorage(size_t extra) {
    MutableArrayRef<Chunk> oldStorage = chunkStorage();
    auto oldLength = chunks().size();
    auto newLength = oldLength + extra;

    // If the existing storage has enough space, we're fine.
    if (newLength <= oldStorage.size()) {
      if (hasOutOfLineStorage()) {
        getOutOfLineStorage()->Length = newLength;
      } else {
        setInlineSize(newLength);
      }
      return oldStorage.slice(oldLength);
    }

    // Otherwise, allocate out-of-line storage.
    size_t newAllocSize = 32;
    while ((oldLength + extra) * sizeof(Chunk)
             + sizeof(OutOfLineStorage) > newAllocSize)
      newAllocSize *= 2;
    auto newStorage = OutOfLineStorage::alloc(newAllocSize);

    // Copy from the old storage.
    newStorage->Length = oldLength + extra;
    memcpy(newStorage->chunkStorage().data(), oldStorage.data(),
           newStorage->Length * sizeof(Chunk));

    // Destroy the old storage.
    if (hasOutOfLineStorage()) destroyOutOfLineStorage();

    // Install the new storage and return.
    Data = reinterpret_cast<uintptr_t>(newStorage);
    return newStorage->chunkStorage().slice(oldLength);
  }

public:
  /// A convenience routine for decoding an unsigned value out of a
  /// sequence of chunks.
  static unsigned decodeIndex(const Chunk *&ptr) {
    unsigned value = 0;
    unsigned shift = 0;
    while (true) {
      unsigned chunk = *ptr++;
      if (chunk & IndexContinuesMask) {
        value |= (chunk ^ IndexContinuesMask) << shift;
      } else {
        value |= chunk << shift;
        break;
      }
      shift += IndexBitsPerChunk;
    }
    return value;
  }

  /// A convenience routine for encoding an unsigned value as a
  /// sequence of chunks.
  static void encodeIndex(unsigned value, Chunk *&ptr) {
    do {
      Chunk chunk = (value & IndexChunkMask);
      auto nextValue = (value >> IndexBitsPerChunk);
      if (nextValue) chunk |= IndexContinuesMask;
      *ptr++ = chunk;
      value = nextValue;
    } while (value);
  }

  /// Returns the encoding size required by the above convenience
  /// routine for the given unsigned value.
  static unsigned getEncodedIndexSize(unsigned value) {
    unsigned count = 0;
    do {
      count++;
      value >>= IndexBitsPerChunk;
    } while (value);
    return count;
  }
};

/// An encoded sequence.
///
/// The Element type is assumed to have the following members:
///
///   static Element decode(const EncodedSequenceBase::Chunk *&ptr);
///   void encode(EncodedSequenceBase::Chunk *&ptr) const;
///   unsigned getEncodedSize() const;
///
/// encode and decode should advance the pointer by exactly the number
/// of chunks returned by getEncodedSize.
template <class Element>
class EncodedSequence : public EncodedSequenceBase {
public:
  /// An iterator over an encoded sequence.
  class iterator {
    const Chunk *Ptr;
  public:
    iterator() = default;
    explicit iterator(const Chunk *ptr) : Ptr(ptr) {}

    Element operator*() const {
      // Decode without changing Ptr.
      auto ptr = Ptr;
      return Element::decode(ptr);
    }

    iterator &operator++() {
      Element::decode(Ptr);
      return *this;
    }
    iterator operator++(int) {
      auto copy = *this;
      operator++();
      return copy;
    }
    bool operator==(iterator other) const { return Ptr == other.Ptr; }
    bool operator!=(iterator other) const { return Ptr != other.Ptr; }
  };

  iterator begin() const { return iterator(chunks().begin()); }
  iterator end() const { return iterator(chunks().end()); }
  bool empty() const { return begin() == end(); }

  /// Add a new element to the sequence.
  void push_back(Element elt) {
    // Figure out how much storage we need.
    unsigned encodedSize = elt.getEncodedSize();

    // Claim that much storage, growing if necessary.
    MutableArrayRef<Chunk> storage = claimStorage(encodedSize);

    // Initialize the storage.
    Chunk *ptr = storage.data();
    elt.encode(ptr);
  }
};

} // end namespace swift

#endif
