//===--- OwnedString.h - String storage -------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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
#include "llvm/Support/TrailingObjects.h"

using llvm::StringRef;

namespace swift {

/// Holds a string - either statically allocated or dynamically allocated
/// and owned by this type.
class OwnedString {
  /// An owner that keeps the buffer of a ref counted \c OwnedString alive.
  class TextOwner final : public llvm::ThreadSafeRefCountedBase<TextOwner>,
                          public llvm::TrailingObjects<TextOwner, char> {
    TextOwner(StringRef Text) {
      std::uninitialized_copy(Text.begin(), Text.end(),
                              getTrailingObjects<char>());
    }

  public:
    static TextOwner *make(StringRef Text) {
      auto size = totalSizeToAlloc<char>(Text.size());
      void *data = ::operator new(size);
      return new (data) TextOwner(Text);
    }

    const char *getText() const { return getTrailingObjects<char>(); }
  };

  /// The text this owned string represents
  StringRef Text;

  /// In case of a ref counted string an owner that keeps the buffer \c Text
  /// references alive.
  llvm::IntrusiveRefCntPtr<TextOwner> OwnedPtr;

  OwnedString(StringRef Text, llvm::IntrusiveRefCntPtr<TextOwner> OwnedPtr)
      : Text(Text), OwnedPtr(OwnedPtr) {}

public:
  OwnedString() : OwnedString(/*Text=*/StringRef(), /*OwnedPtr=*/nullptr) {}

  /// Create a ref counted \c OwnedString that is initialized with the text of
  /// the given \c StringRef.
  OwnedString(StringRef Str) : OwnedString(makeRefCounted(Str)) {}

  /// Create a ref counted \c OwnedString that is initialized with the text of
  /// the given buffer.
  OwnedString(const char *Str) : OwnedString(StringRef(Str)) {}

  /// Create an \c OwnedString that references the given string. The
  /// \c OwnedString will not take ownership of that buffer and will assume that
  /// the buffer outlives its lifetime.
  static OwnedString makeUnowned(StringRef Str) {
    return OwnedString(Str, /*OwnedPtr=*/nullptr);
  }

  /// Create an \c OwnedString that keeps its contents in a reference counted
  /// buffer. The contents of \p Str will be copied initially and are allowed to
  /// be disposed after the \c OwnedString has been created.
  static OwnedString makeRefCounted(StringRef Str) {
    if (Str.empty()) {
      // Copying an empty string doesn't make sense. Just create an unowned
      // string that points to the empty string.
      return makeUnowned(Str);
    } else {
      llvm::IntrusiveRefCntPtr<TextOwner> OwnedPtr(TextOwner::make(Str));
      // Allocate the StringRef on the stack first.  This is to ensure that the
      // order of evaluation of the arguments is specified.  The specification
      // does not specify the order of evaluation for the arguments.  Itanium
      // chose to evaluate left to right, while Windows evaluates right to left.
      // As such, it is possible that the OwnedPtr has already been `std::move`d
      // by the time that the StringRef is attempted to be created.  In such a
      // case, the offset of the field (+4) is used instead of the pointer to
      // the text, resulting in invalid memory references.
      StringRef S(OwnedPtr->getText(), Str.size());
      return OwnedString(S, std::move(OwnedPtr));
    }
  }

  /// Returns the length of the string in bytes.
  size_t size() const { return Text.size(); }

  /// Returns true if the length is 0.
  bool empty() const { return size() == 0; }

  /// Returns a StringRef to the underlying data. No copy is made and no
  /// ownership changes take place.
  StringRef str() const { return Text; }

  bool operator==(const OwnedString &Right) const {
    return str() == Right.str();
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_OWNEDSTRING_H
