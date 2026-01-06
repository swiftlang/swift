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
#include <optional>
#include <utility>

namespace swift {

struct PrebuiltStringMapBase {
  uint64_t arraySize;

  /// Construct an empty map. Must be constructed in memory at least as large as
  /// byteSize(arraySize). The map can hold at most arraySize-1 values.
  /// Attempting to insert more than that will result in fatal errors when
  /// inserting or retrieving values.
  PrebuiltStringMapBase(uint64_t arraySize) : arraySize(arraySize) {}

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

  /// Search for a matching entry in the map. `isMatch` is called with a
  /// candidate index and returns true if there is a match at that index.
  template <typename IsMatch>
  std::optional<size_t> findIndex(const void *string, size_t len,
                                  const IsMatch &isMatch) const {
    uint64_t hashValue = hash(string, len);

    size_t index = hashValue % arraySize;

    size_t numSearched = 0;
    while (!isMatch(index)) {
      index = index + 1;
      if (index >= arraySize)
        index = 0;

      numSearched++;
      if (numSearched > arraySize) {
        assert(false &&
               "Could not find match in PrebuiltStringMapBase::findIndex");
        return std::nullopt;
      }
    }

    return index;
  }
};

/// A map that can be pre-built out of process. Uses a fixed hash function with
/// no per-process seeding to ensure consistent hashes between builder and user.
///
/// The elements are tail allocated. `byteSize` can be used to calculate the
/// amount of memory needed. The memory must be initialized with all string
/// values set to null. StringTy is opaque for insertion, except for using the
/// provided stringIsNull function to check for null values.
template <typename StringTy, typename ElemTy, bool (*stringIsNull)(StringTy)>
struct PrebuiltStringMap : PrebuiltStringMapBase {
  PrebuiltStringMap(uint64_t arraySize) : PrebuiltStringMapBase(arraySize) {}

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
    return sizeof(PrebuiltStringMapBase) + sizeof(ArrayElement) * arraySize;
  }

  /// Perform the search portion of an insertion operation. Returns a pointer to
  /// the element where string is to be inserted. The caller is responsible for
  /// initializing the element to contain the string/value. It is assumed that
  /// the key does not already exist in the map. If it does exist, this will
  /// insert a useless duplicate.
  ArrayElement *insert(const void *string, size_t len) {
    auto foundIndex = findIndex(string, len, [&](size_t index) {
      return stringIsNull(array()[index].key);
    });

    if (foundIndex)
      return &array()[*foundIndex];
    return nullptr;
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
    auto equalOrNull = [&](size_t index) {
      auto key = array()[index].key;

      // NULL is considered a "match" as we want to stop the search on NULL too.
      if (stringIsNull(key))
        return true;

      // key is NUL terminated but toFind may not be. Check that they have equal
      // contents up to len, and check that key has a terminating NUL at the
      // right point.
      if (strncmp(key, toFind, len) == 0 && key[len] == 0)
        return true;

      // Not NULL, not equal, keep searching.
      return false;
    };
    auto foundIndex = findIndex(toFind, len, equalOrNull);
    if (!foundIndex)
      return nullptr;

    const auto &elementPtr = &array()[*foundIndex];

    // If the "matching" element contains a NULL then we didn't find a match.
    if (stringIsNull(elementPtr->key))
      return nullptr;

    return elementPtr;
  }
};

/// A pre-built map with string-based keys that are implicit, i.e. equality can
/// be determined by looking at the values. The map contains auxiliary data
/// stored out of line from the main elements, to avoid padding when the aux
/// data is smaller than the alignment of the main elements.
template <typename ElemTy, typename AuxTy>
struct PrebuiltAuxDataImplicitStringMap : PrebuiltStringMapBase {
  PrebuiltAuxDataImplicitStringMap(uint64_t arraySize)
      : PrebuiltStringMapBase(arraySize) {}

  static size_t byteSize(uint64_t arraySize) {
    return sizeof(PrebuiltStringMapBase) + sizeof(ElemTy) * arraySize +
           sizeof(AuxTy) * arraySize;
  }

  using DataPointers = std::pair<ElemTy *, AuxTy *>;
  using DataPointersConst = std::pair<const ElemTy *, const AuxTy *>;

  const ElemTy *elements() const { return (const ElemTy *)(&arraySize + 1); }

  ElemTy *elements() { return (ElemTy *)(&arraySize + 1); }

  const AuxTy *aux() const { return (const AuxTy *)(elements() + arraySize); }

  AuxTy *aux() { return (AuxTy *)(elements() + arraySize); }

  DataPointersConst pointers(size_t index) const {
    return {&elements()[index], &aux()[index]};
  }

  DataPointers pointers(size_t index) {
    return {&elements()[index], &aux()[index]};
  }

  /// Perform the search portion of an insertion operation. Returns pointers to
  /// the element and aux data where the value is to be inserted. The caller is
  /// responsible for initializing the element and aux data. It is assumed that
  /// the key does not already exist in the map. If it does exist, this will
  /// insert a duplicate.
  ///
  /// isNull is a callable passed a pair of pointers to an element and
  /// corresponding auxiliary data, and must return true if the element is
  /// considered NULL (empty).
  template <typename IsNull>
  DataPointers insert(const char *string, const IsNull &isNull) {
    auto foundIndex = findIndex(string, strlen(string), [&](size_t index) {
      return isNull(pointers(index));
    });
    if (!foundIndex)
      return {nullptr, nullptr};
    return pointers(*foundIndex);
  }

  /// Look up the given key in the map.
  ///
  /// isMatch is a callable passed a pair of pointers to the element and
  /// auxiliary data, and must return true if the elements they point to are a
  /// match for what's being looked up.
  ///
  /// isNull must return true if the elements are NULL/empty, as with insert().
  ///
  /// The returned pointers point to the matched element and auxiliary data, if
  /// a match was found. They point to a NULL entry if no map was found. They
  /// will only be NULL if the table data was malformed and no match or NULL
  /// exists in it.
  template <typename IsMatch, typename IsNull>
  DataPointersConst find(const char *toFind, size_t len, const IsMatch &isMatch,
                         const IsNull &isNull) const {
    auto foundIndex = findIndex(toFind, len, [&](size_t index) {
      return isNull(pointers(index)) || isMatch(pointers(index));
    });
    if (!foundIndex)
      return {nullptr, nullptr};
    return pointers(*foundIndex);
  }
};

} // namespace swift

#endif // SWIFT_PREBUILT_STRING_MAP_H
