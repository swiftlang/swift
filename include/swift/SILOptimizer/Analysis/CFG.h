//===--- CFG.h - Routines which analyze the CFG of a function ---*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_CFG_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_CFG_H

namespace llvm {

template <typename T> class TinyPtrVector;

} // end namespace llvm

namespace swift {

class SILFunction;
class SILBasicBlock;

/// Return true if we conservatively find all BB's that are non-failure exit
/// basic blocks and place them in \p BBs. If we find something we don't
/// understand, bail.
///
/// A non-failure exit BB is defined as a BB that:
///
/// 1. Has a return terminator.
/// 2. unreachable + noreturn terminator sequence.
///
/// If we just have an unreachable without a noreturn call before it, we must
/// have a failure BB.
///
/// We use a TinyPtrVector since in most cases this will only return one
/// SILBasicBlock since non-failure noreturn functions should not occur often
/// implying in most cases this will be one element.
///
/// TODO:
bool findAllNonFailureExitBBs(SILFunction *F,
                              llvm::TinyPtrVector<SILBasicBlock *> &BBs);

} // end namespace swift

#endif
