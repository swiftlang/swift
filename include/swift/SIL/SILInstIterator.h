//===--- SILInstIterator.h ------------------------------------------------===//
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

#ifndef SWIFT_SIL_SILINSTITERATOR_H
#define SWIFT_SIL_SILINSTITERATOR_H

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/IR/InstIterator.h"

namespace swift {

using silinst_iterator =
    llvm::InstIterator<SILBasicBlock, SILBasicBlock::iterator,
                       SILFunction::iterator, SILInstruction>;
using const_silinst_iterator =
    llvm::InstIterator<const SILBasicBlock, SILBasicBlock::const_iterator,
                       SILFunction::const_iterator, const SILInstruction>;

using silinst_range = iterator_range<silinst_iterator>;
using const_silinst_range = iterator_range<const_silinst_iterator>;

inline silinst_iterator silinst_begin(SILFunction *F) {
  return silinst_iterator(*F);
}
inline silinst_iterator silinst_end(SILFunction *F) {
  return silinst_iterator(*F, true);
}
inline silinst_range make_silinst_range(SILFunction *F) {
  return silinst_range(silinst_begin(F), silinst_end(F));
}
inline const_silinst_iterator silinst_begin(const SILFunction *F) {
  return const_silinst_iterator(*F);
}
inline const_silinst_iterator silinst_end(const SILFunction *F) {
  return const_silinst_iterator(*F, true);
}
inline const_silinst_range make_silinst_range(const SILFunction *F) {
  return const_silinst_range(silinst_begin(F), silinst_end(F));
}

} // end anonymous namespace

#endif
