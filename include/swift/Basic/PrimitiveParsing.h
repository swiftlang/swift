//===--- PrimitiveParsing.h - Primitive parsing routines ------------------===//
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
///
/// \file
/// Primitive parsing routines useful in various places in the compiler.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PRIMITIVEPARSING_H
#define SWIFT_BASIC_PRIMITIVEPARSING_H

#include "llvm/ADT/StringRef.h"

namespace llvm {
  class StringRef;
  template<typename T> class SmallVectorImpl;
}

namespace swift {

unsigned measureNewline(const char *BufferPtr, const char *BufferEnd);

static inline unsigned measureNewline(llvm::StringRef S) {
  return measureNewline(S.data(), S.data() + S.size());
}

static inline bool startsWithNewline(llvm::StringRef S) {
  return S.startswith("\n") || S.startswith("\r\n");
}

/// Breaks a given string to lines and trims leading whitespace from them.
void trimLeadingWhitespaceFromLines(llvm::StringRef Text,
                           unsigned WhitespaceToTrim,
                           llvm::SmallVectorImpl<llvm::StringRef> &Lines);

} // namespace swift

#endif // LLVM_SWIFT_BASIC_PRIMITIVEPARSING_H

