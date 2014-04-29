//===--- Unicode.cpp - Unicode utilities ----------------------------------===//
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

#include "swift/Basic/Unicode.h"
#include "llvm/Support/ConvertUTF.h"

using namespace swift;

StringRef swift::unicode::extractFirstExtendedGraphemeCluster(StringRef S) {
  // FIXME: implement as described in Unicode Standard Annex #29.
  if (S.empty())
    return StringRef();

  // FIXME: deal with broken code unit sequences.
  // For now, just extract the first code point.
  unsigned CodeUnitSeqLen = getNumBytesForUTF8(S[0]);
  return S.slice(0, CodeUnitSeqLen);
}

