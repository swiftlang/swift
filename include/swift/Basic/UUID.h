//===--- UUID.h - UUID generation -------------------------------*- C++ -*-===//
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
// This is an interface over the standard OSF uuid library that gives UUIDs
// sane value semantics and operators.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_UUID_H
#define SWIFT_BASIC_UUID_H

#include <array>
#include <uuid/uuid.h>
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/Optional.h"

namespace swift {
  
class UUID {
  uuid_t Value;
  
  enum FromRandom_t { FromRandom };
  enum FromTime_t { FromTime };

  UUID(FromRandom_t) { uuid_generate_random(Value); }
  
  UUID(FromTime_t) { uuid_generate_time(Value); }
  
public:
  /// Default constructor.
  UUID() {
    uuid_clear(Value);
  }
  
  UUID(std::array<unsigned char, 16> bytes) {
    memcpy(Value, &bytes, 16);
  }
  
  /// Create a new random UUID from entropy (/dev/random).
  static UUID fromRandom() { return UUID(FromRandom); }
  
  /// Create a new pseudorandom UUID using the time, MAC address, and pid.
  static UUID fromTime() { return UUID(FromTime); }
  
  /// Parse a UUID from a C string.
  static Optional<UUID> fromString(const char *s) {
    UUID result;
    if (uuid_parse(s, result.Value))
      return Nothing;
    return result;
  }
  
  /// Convert a UUID to its string representation.
  void toString(SmallVectorImpl<char> &out) {
    out.resize(sizeof(uuid_string_t));
    uuid_unparse(Value, out.data());
    // Pop off the null terminator.
    assert(out.back() == '\0' && "did not null-terminate?!");
    out.pop_back();
  }
  
#define COMPARE_UUID(op) \
  bool operator op(UUID y) { return uuid_compare(Value, y.Value) op 0; }
  
  COMPARE_UUID(==)
  COMPARE_UUID(!=)
  COMPARE_UUID(<)
  COMPARE_UUID(<=)
  COMPARE_UUID(>)
  COMPARE_UUID(>=)
#undef COMPARE_UUID
};
  
inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os, UUID uuid) {
  llvm::SmallString<sizeof(uuid_string_t)> buf;
  uuid.toString(buf);
  os << buf;
  return os;
}
  
}

namespace llvm {
  template<>
  struct DenseMapInfo<swift::UUID> {
    static inline swift::UUID getEmptyKey() {
      return {{{0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
                0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF}}};
    }
    static inline swift::UUID getTombstoneKey() {
      return {{{0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
                0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFE}}};
    }
    
    static unsigned getHashValue(swift::UUID uuid) {
      union {
        swift::UUID uu;
        unsigned words[4];
      } reinterpret = {uuid};
      return reinterpret.words[0] ^ reinterpret.words[1]
           ^ reinterpret.words[2] ^ reinterpret.words[3];
    }
    
    static bool isEqual(swift::UUID a, swift::UUID b) {
      return a == b;
    }
  };
}

#endif
