//===--- PrimitiveParsing.h - Primitive parsing routines --------*- C++ -*-===//
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
///
/// \file
/// Primitive parsing routines useful in various places in the compiler.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PRIMITIVEPARSING_H
#define SWIFT_BASIC_PRIMITIVEPARSING_H

#include "llvm/ADT/StringRef.h"
#include "swift/Basic/LLVM.h"

namespace swift {

unsigned measureNewline(const char *BufferPtr, const char *BufferEnd);

static inline unsigned measureNewline(StringRef S) {
  return measureNewline(S.data(), S.data() + S.size());
}

static inline bool startsWithNewline(StringRef S) {
  return S.startswith("\n") || S.startswith("\r\n");
}

/// Breaks a given string to lines and trims leading whitespace from them.
void trimLeadingWhitespaceFromLines(StringRef Text, unsigned WhitespaceToTrim,
                                    SmallVectorImpl<StringRef> &Lines);

static inline void splitIntoLines(StringRef Text,
                                  SmallVectorImpl<StringRef> &Lines) {
  trimLeadingWhitespaceFromLines(Text, 0, Lines);
}

} // end namespace swift

#endif // SWIFT_BASIC_PRIMITIVEPARSING_H

