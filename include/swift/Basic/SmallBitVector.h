//===--- SmallBitVector.h -------------------------------------------------===//
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

#ifndef SWIFT_BASIC_SMALLBITVECTOR_H
#define SWIFT_BASIC_SMALLBITVECTOR_H

#include "llvm/ADT/SmallBitVector.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

void printBitsAsArray(llvm::raw_ostream &OS, const llvm::SmallBitVector &bits,
                      bool bracketed);

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     const llvm::SmallBitVector &bits) {
  printBitsAsArray(OS, bits, /*bracketed=*/false);
  return OS;
}

void dumpBits(const llvm::SmallBitVector &bits);

} // namespace swift

#endif
