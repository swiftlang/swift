//===--- SmallBitVector.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/SmallBitVector.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

/// Debug dump a location bit vector.
void swift::printBitsAsArray(raw_ostream &OS, const SmallBitVector &bits,
                             bool bracketed) {
  if (!bracketed) {
    for (unsigned i = 0, e = bits.size(); i != e; ++i)
      OS << (bits[i] ? '1' : '0');
  }
  const char *separator = "";
  OS << '[';
  for (int idx = bits.find_first(); idx >= 0; idx = bits.find_next(idx)) {
    OS << separator << idx;
    separator = ",";
  }
  OS << ']';
}

void swift::dumpBits(const SmallBitVector &bits) { dbgs() << bits << '\n'; }
