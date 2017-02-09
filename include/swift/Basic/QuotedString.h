//===--- QuotedString.h - Print a string in double-quotes -------*- C++ -*-===//
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
/// \file Declares QuotedString, a convenient type for printing a
/// string as a string literal.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_QUOTEDSTRING_H
#define SWIFT_BASIC_QUOTEDSTRING_H

#include "llvm/ADT/StringRef.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
  /// Print the given string as if it were a quoted string.
  void printAsQuotedString(llvm::raw_ostream &out, llvm::StringRef text);

  /// A class designed to make it easy to write a string to a stream
  /// as a quoted string.
  class QuotedString {
    llvm::StringRef Text;
  public:
    explicit QuotedString(llvm::StringRef text) : Text(text) {}

    friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                         QuotedString string) {
      printAsQuotedString(out, string.Text);
      return out;
    }
  };
} // end namespace swift

#endif // SWIFT_BASIC_QUOTEDSTRING_H
