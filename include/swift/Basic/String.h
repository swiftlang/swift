//===--- String.h - String storage ------------------------------*- C++ -*-===//
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
//  This file defines the 'String' storage wrapper, which can hold its own
//  unique copy of a string, or merely hold a reference to some point in a
//  source buffer, which is assumed to live at least as long as a value of
//  this type.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_BASIC_STRING_H
#define SWIFT_BASIC_STRING_H

#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"

using llvm::StringRef;

namespace swift {

class String {
  const char *Data;
  size_t Length;
  bool Managed;


  static const char *copyBuffer(const String &Other) {
    auto Buffer = (char *)malloc(Other.str().size());
    memcpy(Buffer, Other.str().data(), Other.str().size());
    return Buffer;
  }

public:
  String() : Data(nullptr), Length(0), Managed(false) {}

  String(const char *Data, size_t Length, bool Managed)
    : Data(Data), Length(Length), Managed(Managed) {}

  String(StringRef Str, bool IsManaged = false)
    : String(Str.data(), Str.size(), IsManaged) {}

  String(const String &Other)
    : Data(Other.Managed ? copyBuffer(Other) : Other.Data), Length(Other.Length),
      Managed(Other.Managed) {}

  static String createManaged(const char *Str, size_t Length) {
    auto Buffer = malloc(Length);
    memcpy(Buffer, Str, Length);
    return String { reinterpret_cast<const char *>(Buffer), Length,
                            /* Managed */ true };
  }

  static String createManaged(StringRef Str) {
    return createManaged(Str.data(), Str.size());
  }

  static String createUnmanaged(StringRef Str) {
    return String { Str, /* Managed */ false };
  }

  size_t size() const {
    return Length;
  }

  bool empty() const {
    return Length == 0;
  }

  StringRef str() const {
    return StringRef { Data, Length };
  }

  bool operator==(const String &Right) const {
    return str() == Right.str();
  }

  ~String() {
    if (Managed)
      free(reinterpret_cast<void *>(const_cast<char *>(Data)));
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_STRING_H

