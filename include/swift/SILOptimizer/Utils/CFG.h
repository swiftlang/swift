//===--- CFG.h - Utilities for SIL CFG transformations ----------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_CFG_H
#define SWIFT_SILOPTIMIZER_UTILS_CFG_H

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBuilder.h"

namespace swift {

class DominanceInfo;
class SILLoop;
class SILLoopInfo;

/// \brief Adds a new argument to an edge between a branch and a destination
/// block.
///
/// \param Branch The terminator to add the argument to.
/// \param Dest The destination block of the edge.
/// \param Val The value to the arguments of the branch.
/// \return The created branch. The old branch is deleted.
/// The argument is appended at the end of the argument tuple.
TermInst *addNewEdgeValueToBranch(TermInst *Branch, SILBasicBlock *Dest,
                                  SILValue Val);

/// \brief Changes the edge value between a branch and destination basic block
/// at the specified index. Changes all edges from \p Branch to \p Dest to carry
/// the value.
///
/// \param Branch The branch to modify.
/// \param Dest The destination of the edge.
/// \param Idx The index of the argument to modify.
/// \param Val The new value to use.
/// \return The new branch. Deletes the old one.
TermInst *changeEdgeValue(TermInst *Branch, SILBasicBlock *Dest, size_t Idx,
                          SILValue Val);

/// \brief Replace a branch target.
///
/// \param T The terminating instruction to modify.
/// \param EdgeIdx The successor edges index that will be replaced.
/// \param NewDest The new target block.
/// \param PreserveArgs If set, preserve arguments on the replaced edge.
void changeBranchTarget(TermInst *T, unsigned EdgeIdx, SILBasicBlock *NewDest,
                        bool PreserveArgs);

/// \brief Replace a branch target.
///
/// \param T The terminating instruction to modify.
/// \param OldDest The successor block that will be replaced.
/// \param NewDest The new target block.
/// \param PreserveArgs If set, preserve arguments on the replaced edge.
void replaceBranchTarget(TermInst *T, SILBasicBlock *OldDest, SILBasicBlock *NewDest,
                         bool PreserveArgs);

/// \brief Check if the edge from the terminator is critical.
bool isCriticalEdge(TermInst *T, unsigned EdgeIdx);

/// \brief Splits the edge from terminator if it is critical.
///
/// Updates dominance information and loop information if not null.
/// Returns the newly created basic block on success or nullptr otherwise (if
/// the edge was not critical).
SILBasicBlock *splitCriticalEdge(TermInst *T, unsigned EdgeIdx,
                                 DominanceInfo *DT = nullptr,
                                 SILLoopInfo *LI = nullptr);

/// Splits the critical edges between from and to. This code assumes there is
/// exactly one edge between the two basic blocks. It will return the wrong
/// result if there are multiple edges and will assert if there are no edges in
/// between the two blocks.
///
/// Updates dominance information and loop information if not null.
SILBasicBlock *splitIfCriticalEdge(SILBasicBlock *From, SILBasicBlock *To,
                                   DominanceInfo *DT = nullptr,
                                   SILLoopInfo *LI = nullptr);

/// \brief Splits the edge from terminator.
///
/// Updates dominance information and loop information if not null.
/// Returns the newly created basic block.
SILBasicBlock *splitEdge(TermInst *T, unsigned EdgeIdx,
                         DominanceInfo *DT = nullptr,
                         SILLoopInfo *LI = nullptr);

/// \brief Splits the edges between two basic blocks.
///
/// Updates dominance information and loop information if not null.
void splitEdgesFromTo(SILBasicBlock *From, SILBasicBlock *To,
                      DominanceInfo *DT = nullptr, SILLoopInfo *LI = nullptr);

/// \brief Rotate a loop's header as long as it is exiting and not equal to the
/// passed basic block.
/// If \p RotateSingleBlockLoops is true a single basic block loop will be
/// rotated once. ShouldVerify specifies whether to perform verification after
/// the transformation.
/// Returns true if the loop could be rotated.
bool rotateLoop(SILLoop *L, DominanceInfo *DT, SILLoopInfo *LI,
                bool RotateSingleBlockLoops, SILBasicBlock *UpTo,
                bool ShouldVerify);

/// \brief Splits the basic block before the instruction with an unconditional
/// branch and updates the dominator tree and loop info.
SILBasicBlock *splitBasicBlockAndBranch(SILBuilder &B,
                                        SILInstruction *SplitBeforeInst,
                                        DominanceInfo *DT, SILLoopInfo *LI);

/// \brief Return true if the function has a critical edge, false otherwise.
bool hasCriticalEdges(SILFunction &F, bool OnlyNonCondBr);

/// \brief Split all critical edges in the function updating the dominator tree
/// and loop information (if they are not set to null). If \p OnlyNonCondBr is
/// true this will not split cond_br edges (Only edges which can't carry
/// arguments will be split).
bool splitAllCriticalEdges(SILFunction &F, bool OnlyNonCondBr,
                           DominanceInfo *DT, SILLoopInfo *LI);

/// \brief Merge a basic block ending in a branch with its successor
/// if possible. If dominance information or loop info is non null update it.
/// Return true if block was merged.
bool mergeBasicBlockWithSuccessor(SILBasicBlock *BB, DominanceInfo *DT,
                                  SILLoopInfo *LI);

} // end namespace swift
#endif
