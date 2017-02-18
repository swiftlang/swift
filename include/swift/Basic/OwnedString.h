//===--- OwnedString.h - String storage -------------------------*- C++ -*-===//
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
//
//  This file defines the 'OwnedString' storage wrapper, which can hold its own
//  unique copy of a string, or merely hold a reference to some point in a
//  source buffer, which is assumed to live at least as long as a value of this
//  type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_OWNEDSTRING_H
#define SWIFT_BASIC_OWNEDSTRING_H

#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"

using llvm::StringRef;

namespace swift {

enum class StringOwnership {
  /// An OwnedString holds a weak reference to the underlying string storage
  /// and will never attempt to free it.
  Unowned,

  /// An OwnedString has its own copy of the underlying string storage and
  /// will free the storage upon its destruction.
  Copied,
};

/// Holds a string - either statically allocated or dynamically allocated
/// and owned by this type.
class OwnedString {
  const char *Data;
  size_t Length;
  StringOwnership Ownership;

  OwnedString(const char* Data, size_t Length, StringOwnership Ownership)
      : Length(Length), Ownership(Ownership) {
    assert(Length >= 0 && "expected length to be non-negative");

    if (Ownership == StringOwnership::Copied && Data) {
      assert(
        Length <= strlen(Data) &&
        "expected length to be a valid index, within the length of the string");

      char *substring = static_cast<char *>(malloc(Length + 1));
      assert(substring && "expected successful malloc of copy");

      memcpy(substring, Data, Length);
      substring[Length] = '\0';

      this->Data = substring;
    }
    else
      this->Data = Data;
  }

public:
  OwnedString() : OwnedString(nullptr, 0, StringOwnership::Unowned) {}

  OwnedString(const char *Data, size_t Length)
    : OwnedString(Data, Length, StringOwnership::Unowned) {}

  OwnedString(StringRef Str) : OwnedString(Str.data(), Str.size()) {}

  OwnedString(const char *Data) : OwnedString(StringRef(Data)) {}

  OwnedString(const OwnedString &Other)
      : OwnedString(Other.Data, Other.Length, Other.Ownership) {}
  
  OwnedString copy() {
    return OwnedString(Data, Length, StringOwnership::Copied);
  }

  /// Returns the length of the string in bytes.
  size_t size() const {
    return Length;
  }

  /// Returns true if the length is 0.
  bool empty() const {
    return Length == 0;
  }

  /// Returns a StringRef to the underlying data. No copy is made and no
  /// ownership changes take place.
  StringRef str() const {
    return StringRef { Data, Length };
  }

  bool operator==(const OwnedString &Right) const {
    return str() == Right.str();
  }

  ~OwnedString() {
    if (Ownership == StringOwnership::Copied)
      free(const_cast<char *>(Data));
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_OWNEDSTRING_H
