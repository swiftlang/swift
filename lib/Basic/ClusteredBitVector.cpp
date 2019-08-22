//===--- ClusteredBitVector.cpp - Out-of-line code for the bit vector -----===//
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
//  This file implements support code for ClusteredBitVector.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/ClusteredBitVector.h"

#include "llvm/Support/raw_ostream.h"

using namespace swift;

void ClusteredBitVector::dump() const {
  print(llvm::errs());
}

/// Pretty-print the vector.
void ClusteredBitVector::print(llvm::raw_ostream &out) const {
  // Print in 8 clusters of 8 bits per row.
  if (!Bits) {
    return;
  }
  auto &v = Bits.getValue();
  for (size_t i = 0, e = size(); ; ) {
    out << (v[i++] ? '1' : '0');
    if (i == e) {
      return;
    } else if ((i & 64) == 0) {
      out << '\n';
    } else if ((i & 8) == 0) {
      out << ' ';
    }
  }
}
