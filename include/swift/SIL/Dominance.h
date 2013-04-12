//===--- Dominance.h - SIL dominance analysis ------------------*- C++ -*-===//
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
//
// This file provides interfaces for computing and working with
// control-flow dominance in SIL.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_DOMINANCE_H
#define SWIFT_SIL_DOMINANCE_H

#include "llvm/Analysis/Dominators.h"
#include "swift/SIL/CFG.h"

extern template class llvm::DominatorTreeBase<swift::SILBasicBlock>;
extern template class llvm::DominatorBase<swift::SILBasicBlock>;
extern template class llvm::DomTreeNodeBase<swift::SILBasicBlock>;

namespace swift {

/// A class for computing basic dominance information.
class DominanceInfo : public llvm::DominatorTreeBase<SILBasicBlock> {
public:
  DominanceInfo(SILFunction *F);

  /// Does instruction A properly dominate instruction B?
  bool properlyDominates(SILInstruction *a, SILInstruction *b);
  using DominatorTreeBase::properlyDominates;
};

}  // end namespace swift

#endif
