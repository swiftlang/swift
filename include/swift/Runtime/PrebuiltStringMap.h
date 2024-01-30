//===--- PrebuiltStringMap.h - Statically built string map ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PREBUILT_STRING_MAP_H
#define SWIFT_PREBUILT_STRING_MAP_H

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>

namespace swift {

/// A map that can be pre-built out of process. Uses a fixed hash function with
/// no per-process seeding to ensure consistent hashes between builder and user.
///
/// The elements are tail allocated. `byteSize` can be used to calculate the
/// amount of memory needed. The memory must be initialized with all string
/// values set to null. StringTy is opaque for insertion, except for using the
/// provided stringIsNull function to check for null values.
template <typename StringTy, typename ElemTy, bool (*stringIsNull)(StringTy)>
struct PrebuiltStringMap {
  uint64_t arraySize;

  struct ArrayElement {
    StringTy key;
    ElemTy value;
  };

  ArrayElement *array() {
    uintptr_t start = (uintptr_t)(&arraySize + 1);
    return (ArrayElement *)start;
  }

  const ArrayElement *array() const {
    uintptr_t start = (uintptr_t)(&arraySize + 1);
    return (ArrayElement *)start;
  }

  static size_t byteSize(uint64_t arraySize) {
    return sizeof(PrebuiltStringMap) + sizeof(ArrayElement) * arraySize;
  }

  /// Construct an empty map. Must be constructed in memory at least as large as
  /// byteSize(arraySize). The map can hold at most arraySize-1 values.
  /// Attempting to insert more than that will result in fatal errors when
  /// inserting or retrieving values.
  PrebuiltStringMap(uint64_t arraySize) : arraySize(arraySize) {}

  // Based on MurmurHash2
  uint64_t hash(const void *data, size_t len) const {
    uint64_t magic = 0xc6a4a7935bd1e995ULL;
    uint64_t salt = 47;

    uint64_t hash = len * magic;

    const uint8_t *cursor = (const uint8_t *)data;
    const uint8_t *bulkEnd = cursor + (len & ~(sizeof(uint64_t) - 1));
    size_t remaining = len;

    while (cursor != bulkEnd) {
      uint64_t value;
      memcpy(&value, cursor, sizeof(uint64_t));
      cursor += sizeof(uint64_t);
      remaining -= sizeof(uint64_t);

      value *= magic;
      value ^= value >> salt;
      value *= magic;

      hash ^= value;
      hash *= magic;
    }

    // This is never going to be false, but it's comforting.
    static_assert(sizeof(uint64_t) == 8);

    // Collect the last few bytes.
    switch (remaining & 7) {
    case 7:
      hash ^= (uint64_t)cursor[6] << 48;
      [[fallthrough]];
    case 6:
      hash ^= (uint64_t)cursor[5] << 40;
      [[fallthrough]];
    case 5:
      hash ^= (uint64_t)cursor[4] << 32;
      [[fallthrough]];
    case 4:
      hash ^= (uint64_t)cursor[3] << 24;
      [[fallthrough]];
    case 3:
      hash ^= (uint64_t)cursor[2] << 16;
      [[fallthrough]];
    case 2:
      hash ^= (uint64_t)cursor[1] << 8;
      [[fallthrough]];
    case 1:
      hash ^= (uint64_t)cursor[0];
    }

    hash *= magic;
    hash ^= hash >> salt;
    hash *= magic;
    hash ^= hash >> salt;

    return hash;
  }

  /// Perform the search portion of an insertion operation. Returns a pointer to
  /// the element where string is to be inserted. The caller is responsible for
  /// initializing the element to contain the string/value. It is assumed that
  /// the key does not already exist in the map. If it does exist, this will
  /// insert a useless duplicate.
  ArrayElement *insert(const void *string, size_t len) {
    uint64_t hashValue = hash(string, len);
    size_t index = hashValue % arraySize;

    size_t numSearched = 0;
    while (!stringIsNull(array()[index].key)) {
      index = index + 1;
      if (index >= arraySize)
        index = 0;

      numSearched++;
      if (numSearched > arraySize) {
        assert(false &&
               "Could not find empty element in PrebuiltStringMap::insert");
        return nullptr;
      }
    }

    return &array()[index];
  }

  ArrayElement *insert(const char *string) {
    return insert(string, strlen(string));
  }

  /// Look up the given string in the table. Requires that StringTy be
  /// `const char *`.
  const ArrayElement *find(const char *toFind) const {
    size_t len = strlen(toFind);
    return find(toFind, len);
  }

  const ArrayElement *find(const char *toFind, size_t len) const {
    uint64_t hashValue = hash(toFind, len);

    size_t index = hashValue % arraySize;

    size_t numSearched = 0;
    while (const char *key = array()[index].key) {
      // key is NUL terminated but toFind may not be. Check that they have equal
      // contents up to len, and check that key has a terminating NUL at the
      // right point.
      if (strncmp(key, toFind, len) == 0 && key[len] == 0)
        return &array()[index];

      index = index + 1;
      if (index >= arraySize)
        index = 0;

      numSearched++;
      if (numSearched > arraySize) {
        assert(
            false &&
            "Could not find match or empty element in PrebuiltStringMap::find");
        return nullptr;
      }
    }

    return nullptr;
  }
};

} // namespace swift

#endif // SWIFT_PREBUILT_STRING_MAP_H
