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

namespace llvm {

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const SmallBitVector &bv) {
  for (unsigned i = 0, e = bv.size(); i != e; ++i)
    os << (bv[i] ? '1' : '0');
  return os;
}

} // namespace llvm

#endif
