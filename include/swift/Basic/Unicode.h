//===--- Unicode.h - Unicode utilities --------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_UNICODE_H
#define SWIFT_BASIC_UNICODE_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
namespace unicode {

StringRef extractFirstExtendedGraphemeCluster(StringRef S);

static inline bool isSingleExtendedGraphemeCluster(StringRef S) {
  StringRef First = extractFirstExtendedGraphemeCluster(S);
  if (First.empty())
    return false;
  return First == S;
}

} // namespace unicode
} // namespace swift

#endif

