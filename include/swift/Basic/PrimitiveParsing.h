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

namespace swift {

static inline unsigned measureNewline(const char *BufferPtr,
                                      const char *BufferEnd) {
  if (BufferPtr == BufferEnd)
    return 0;

  if (*BufferPtr == '\n')
    return 1;

  assert(*BufferPtr == '\r');
  unsigned Bytes = 1;
  if (BufferPtr != BufferEnd && *BufferPtr == '\n')
    Bytes++;
  return Bytes;
}

static inline unsigned measureNewline(StringRef S) {
  return measureNewline(S.data(), S.data() + S.size());
}

static inline bool startsWithNewline(StringRef S) {
  return S.startswith("\n") || S.startswith("\r\n");
}

} // namespace swift

#endif // LLVM_SWIFT_BASIC_PRIMITIVEPARSING_H

