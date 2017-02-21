//===--- OwnedString.h - String storage -------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

public:
  OwnedString()
    : Data(nullptr), Length(0), Ownership(StringOwnership::Unowned) {}

  OwnedString(const char *Data, size_t Length, StringOwnership Ownership)
    : Data(Ownership == StringOwnership::Copied ? strndup(Data, Length) : Data),
      Length(Length), Ownership(Ownership) {}

  OwnedString(StringRef Str, StringOwnership Ownership)
    : OwnedString(Ownership == StringOwnership::Copied
                  ? strndup(Str.data(), Str.size())
                  : Str.data(),
             Str.size(), Ownership) {}

  OwnedString(const char *Data)
    : OwnedString(StringRef(Data), StringOwnership::Unowned) {}

  OwnedString(const OwnedString &Other)
    : Data(Other.Ownership == StringOwnership::Copied
             ? strndup(Other.Data, Other.Length)
             : Other.Data),
      Length(Other.Length), Ownership(Other.Ownership) {}

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

