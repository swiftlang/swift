//===--- StackNesting.cpp - Utility for stack nesting  --------------------===//
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

#include "swift/SILOptimizer/Utils/StackNesting.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/Support/Debug.h"

using namespace swift;

bool swift::hasStackDifferencesAt(SILInstruction *start,
                                  InstructionMatcher matcher) {
  struct StackStatus {
    /// The number of currently-active stack allocations on this path.
    /// Because of the stack discipline, we only have to maintain a count.
    unsigned depth;

    /// Whether we've popped anything off of the stack that was active
    /// at the start of the search.
    bool hasPops;

    bool hasDifferences() const { return hasPops || depth != 0; }
  };

  SmallPtrSet<SILBasicBlock*, 8> visited;
  SmallVector<std::pair<SILInstruction *, StackStatus>, 8> worklist;
  worklist.push_back({start, {0, false}});

  while (!worklist.empty()) {
    // Pull a block and depth off the worklist.
    // Visitation order doesn't matter.
    auto pair = worklist.pop_back_val();
    auto status = pair.second;
    auto firstInst = pair.first;
    auto block = firstInst->getParent();

    for (SILBasicBlock::iterator i(firstInst), e = block->end(); i != e; ++i) {
      auto match = matcher(&*i);

      // If this is an interesting instruction, check for stack
      // differences here.
      if (match.matches && status.hasDifferences())
        return true;

      // If we should halt, just go to the next work-list item, bypassing
      // visiting the successors.
      if (match.halt)
        goto nextWorkListItem; // a labelled continue would be nice

      // Otherwise, do stack-depth bookkeeping.
      if (i->isAllocatingStack()) {
        status.depth++;
      } else if (i->isDeallocatingStack()) {
        // If the stack depth is already zero, we're deallocating a stack
        // allocation that was live into the search.
        if (status.depth > 0) {
          status.depth--;
        } else {
          status.hasPops = true;
        }
      }
    }

    // Add the successors to the worklist.
    for (auto &succ : block->getSuccessors()) {
      auto succBlock = succ.getBB();
      auto insertResult = visited.insert(succBlock);
      if (insertResult.second) {
        worklist.push_back({&succBlock->front(), status});
      }
    }

  nextWorkListItem: ;
  }

  return false;
}


void StackNesting::setup(SILFunction *F) {
  SmallVector<BlockInfo *, 8> WorkList;
  llvm::DenseMap<SILBasicBlock *, BlockInfo *> BlockMapping;

  // We use pointers to BlockInfo structs. Therefore it's important that the
  // BlockInfos vector is never re-allocated.
  BlockInfos.reserve(F->size());

  // Start with the function entry block and add blocks while walking down along
  // the successor edges.
  // This ensures a correct ordering of stack locations: an inner location has
  // a higher bit-number than it's outer parent location.
  // This ordering is only important for inserting multiple deallocation
  // instructions (see below).
  BlockInfos.emplace_back(F->getEntryBlock());
  BlockInfo *EntryBI = &BlockInfos.back();
  BlockMapping[F->getEntryBlock()] = EntryBI;
  WorkList.push_back(EntryBI);

  while (!WorkList.empty()) {
    BlockInfo *BI = WorkList.pop_back_val();
    for (SILInstruction &I : *BI->Block) {
      if (I.isAllocatingStack()) {
        auto Alloc = cast<AllocationInst>(&I);
        // Register this stack location.
        unsigned CurrentBitNumber = StackLocs.size();
        StackLoc2BitNumbers[Alloc] = CurrentBitNumber;
        StackLocs.push_back(StackLoc(Alloc));

        BI->StackInsts.push_back(Alloc);

      } else if (I.isDeallocatingStack()) {
        auto *AllocInst = cast<SingleValueInstruction>(I.getOperand(0));
        if (!BI->StackInsts.empty() && BI->StackInsts.back() == AllocInst) {
          // As an optimization, we ignore perfectly nested alloc-dealloc pairs
          // inside a basic block.
          // Actually, this catches most of the cases and keeps our bitsets
          // small.
          assert(StackLocs.back().Alloc == AllocInst);
          StackLocs.pop_back();
          BI->StackInsts.pop_back();
        } else {
          // Register the stack deallocation.
          BI->StackInsts.push_back(&I);
        }
      }
    }
    if (BI->Block->getTerminator()->isFunctionExiting())
      BI->ExitReachable = true;

    for (auto *SuccBB : BI->Block->getSuccessorBlocks()) {
      BlockInfo *&SuccBI = BlockMapping[SuccBB];
      if (!SuccBI) {
        // Push the next reachable block onto the WorkList.
        BlockInfos.emplace_back(SuccBB);
        SuccBI = &BlockInfos.back();
        WorkList.push_back(SuccBI);
      }
      // Cache the successors in our own list.
      BI->Successors.push_back(SuccBI);
    }
  }
  assert(EntryBI == &BlockInfos[0] &&
         "BlockInfo vector should not re-allocate");

  unsigned NumLocs = StackLocs.size();
  for (unsigned Idx = 0; Idx < NumLocs; ++Idx) {
    StackLocs[Idx].AliveLocs.resize(NumLocs);
    // Initially each location gets it's own alive-bit.
    StackLocs[Idx].AliveLocs.set(Idx);
  }
}

bool StackNesting::solve() {
  bool changed = false;
  bool isNested = false;
  BitVector Bits(StackLocs.size());

  // Iterate until we reach a fixed-point.
  do {
    changed = false;

    // It's a backward dataflow problem.
    for (BlockInfo &BI : reversed(BlockInfos)) {
      // Collect the alive-bits (at the block exit) from the successor blocks.
      Bits.reset();
      for (BlockInfo *SuccBI : BI.Successors) {
        Bits |= SuccBI->AliveStackLocsAtEntry;

        // Also get the ExitReachable flag from the successor blocks.
        if (!BI.ExitReachable && SuccBI->ExitReachable) {
          BI.ExitReachable = true;
          changed = true;
        }
      }
      for (SILInstruction *StackInst : reversed(BI.StackInsts)) {
        if (StackInst->isAllocatingStack()) {
          auto AllocInst = cast<SingleValueInstruction>(StackInst);
          int BitNr = StackLoc2BitNumbers[AllocInst];
          if (Bits != StackLocs[BitNr].AliveLocs) {
            // More locations are alive around the StackInst's location.
            // Update the AlivaLocs bitset, which contains all those alive
            // locations.
            assert((Bits.test(BitNr) || (!BI.ExitReachable && !Bits.any()))
                   && "no dealloc found for alloc stack");
            StackLocs[BitNr].AliveLocs = Bits;
            changed = true;
            isNested = true;
          }
          // The allocation ends the lifetime of it's stack location (in reverse
          // order)
          Bits.reset(BitNr);
        } else if (StackInst->isDeallocatingStack()) {
          // A stack deallocation begins the lifetime of its location (in
          // reverse order). And it also begins the lifetime of all other
          // locations which are alive at the allocation point.
          auto *AllocInst =
            cast<SingleValueInstruction>(StackInst->getOperand(0));
          int BitNr = StackLoc2BitNumbers[AllocInst];
          Bits |= StackLocs[BitNr].AliveLocs;
        }
      }
      if (Bits != BI.AliveStackLocsAtEntry) {
        BI.AliveStackLocsAtEntry = Bits;
        changed = true;
      }
    }
  } while (changed);
  
  return isNested;
}

static SILInstruction *createDealloc(AllocationInst *Alloc,
                                     SILInstruction *InsertionPoint,
                                     SILLocation Location) {
  SILBuilderWithScope B(InsertionPoint);
  switch (Alloc->getKind()) {
    case SILInstructionKind::AllocStackInst:
      return B.createDeallocStack(Location, Alloc);
    case SILInstructionKind::AllocRefInst:
      assert(cast<AllocRefInst>(Alloc)->canAllocOnStack());
      return B.createDeallocRef(Location, Alloc, /*canBeOnStack*/true);
    default:
      llvm_unreachable("unknown stack allocation");
  }
}

bool StackNesting::insertDeallocs(const BitVector &AliveBefore,
                                  const BitVector &AliveAfter,
                                  SILInstruction *InsertionPoint,
                                  Optional<SILLocation> Location) {
  if (!AliveBefore.test(AliveAfter))
    return false;

  // The order matters here if we have to insert more than one
  // deallocation. We already ensured in setup() that the bit numbers
  // are allocated in the right order.
  bool changesMade = false;
  for (int LocNr = AliveBefore.find_first(); LocNr >= 0;
       LocNr = AliveBefore.find_next(LocNr)) {
    if (!AliveAfter.test(LocNr)) {
      AllocationInst *Alloc = StackLocs[LocNr].Alloc;
      InsertionPoint = createDealloc(Alloc, InsertionPoint,
                   Location.hasValue() ? Location.getValue() : Alloc->getLoc());
      changesMade = true;
    }
  }
  return changesMade;
}

StackNesting::Changes StackNesting::adaptDeallocs() {

  bool InstChanged = false;
  bool CFGChanged = false;
  BitVector Bits(StackLocs.size());

  // Visit all blocks. Actually the order doesn't matter, but let's to it in
  // the same order as in solve().
  for (const BlockInfo &BI : reversed(BlockInfos)) {
    // Collect the alive-bits (at the block exit) from the successor blocks.
    Bits.reset();
    for (BlockInfo *SuccBI : BI.Successors) {
      Bits |= SuccBI->AliveStackLocsAtEntry;
    }

    // Insert deallocations at block boundaries.
    // This can be necessary for unreachable blocks. Example:
    //
    //   %1 = alloc_stack
    //   %2 = alloc_stack
    //   cond_br %c, bb2, bb3
    // bb2:   <--- need to insert a dealloc_stack %2 at the begin of bb2
    //   dealloc_stack %1
    //   unreachable
    // bb3:
    //   dealloc_stack %2
    //   dealloc_stack %1
    //
    for (unsigned SuccIdx = 0, NumSuccs = BI.Successors.size();
         SuccIdx < NumSuccs; ++ SuccIdx) {
      BlockInfo *SuccBI = BI.Successors[SuccIdx];

      // It's acceptable to not deallocate alive locations in unreachable
      // blocks - as long as the nesting is not violated. So if there are no
      // alive locations at the unreachable successor block, we can ignore it.
      if (!SuccBI->ExitReachable && !SuccBI->AliveStackLocsAtEntry.any())
        continue;

      if (SuccBI->AliveStackLocsAtEntry == Bits)
        continue;

      // Insert deallocations for all locations which are alive at the end of
      // the current block, but not at the begin of the successor block.
      SILBasicBlock *InsertionBlock = SuccBI->Block;
      if (!InsertionBlock->getSinglePredecessorBlock()) {
        // If the current block is not the only predecessor of the successor
        // block, we have to insert a new block where we can add the
        // deallocations.
        InsertionBlock = splitEdge(BI.Block->getTerminator(), SuccIdx);
        CFGChanged = true;
      }
      InstChanged |= insertDeallocs(Bits, SuccBI->AliveStackLocsAtEntry,
                                    &InsertionBlock->front(), None);
    }

    // Insert/remove deallocations inside blocks.
    for (SILInstruction *StackInst : reversed(BI.StackInsts)) {
      if (StackInst->isAllocatingStack()) {
        // For allocations we just update the bit-set.
        auto AllocInst = cast<SingleValueInstruction>(StackInst);
        int BitNr = StackLoc2BitNumbers.lookup(AllocInst);
        assert(Bits == StackLocs[BitNr].AliveLocs &&
               "dataflow didn't converge");
        Bits.reset(BitNr);
      } else if (StackInst->isDeallocatingStack()) {
        // Handle deallocations.
        auto *AllocInst = cast<SingleValueInstruction>(StackInst->getOperand(0));
        SILLocation Loc = StackInst->getLoc();
        int BitNr = StackLoc2BitNumbers.lookup(AllocInst);
        SILInstruction *InsertionPoint = &*std::next(StackInst->getIterator());
        if (Bits.test(BitNr)) {
          // The location of StackInst is alive after StackInst. So we have to
          // remove this deallocation.
          StackInst->eraseFromParent();
          InstChanged = true;
        } else {
          // Avoid inserting another deallocation for BitNr (which is already
          // StackInst).
          Bits.set(BitNr);
        }

        // Insert deallocations for all locations which are not alive after
        // StackInst but _are_ alive at the StackInst.
        InstChanged |= insertDeallocs(StackLocs[BitNr].AliveLocs, Bits,
                                      InsertionPoint, Loc);
        Bits |= StackLocs[BitNr].AliveLocs;
      }
    }
    assert(Bits == BI.AliveStackLocsAtEntry && "dataflow didn't converge");
  }
  if (CFGChanged)
    return Changes::CFG;
  if (InstChanged)
    return Changes::Instructions;
  return Changes::None;
}

StackNesting::Changes StackNesting::correctStackNesting(SILFunction *F) {
  setup(F);
  if (solve()) {
    return adaptDeallocs();
  }
  return Changes::None;
}

void StackNesting::dump() const {
  for (const BlockInfo &BI : BlockInfos) {
    if (!BI.Block)
      continue;

    llvm::dbgs() << "Block " << BI.Block->getDebugID();
    if (!BI.ExitReachable)
      llvm::dbgs() << " (unreachable exit)";
    llvm::dbgs() << ": bits=";
    dumpBits(BI.AliveStackLocsAtEntry);
    for (SILInstruction *StackInst : BI.StackInsts) {
      if (StackInst->isAllocatingStack()) {
        auto AllocInst = cast<AllocationInst>(StackInst);
        int BitNr = StackLoc2BitNumbers.lookup(AllocInst);
        llvm::dbgs() << "  alloc #" << BitNr << ": alive=";
        dumpBits(StackLocs[BitNr].AliveLocs);
        llvm::dbgs() << "    " << *StackInst;
      } else if (StackInst->isDeallocatingStack()) {
        auto *AllocInst = cast<AllocationInst>(StackInst->getOperand(0));
        int BitNr = StackLoc2BitNumbers.lookup(AllocInst);
        llvm::dbgs() << "  dealloc for #" << BitNr << "\n"
                        "    " << *StackInst;
      }
    }
    llvm::dbgs() << "  successors:";
    for (BlockInfo *SuccBI : BI.Successors) {
      llvm::dbgs() << ' ' << SuccBI->Block->getDebugID();
    }
    llvm::dbgs() << '\n';
  }
}

void StackNesting::dumpBits(const BitVector &Bits) {
  llvm::dbgs() << '<';
  const char *separator = "";
  for (int Bit = Bits.find_first(); Bit >= 0; Bit = Bits.find_next(Bit)) {
    llvm::dbgs() << separator << Bit;
    separator = ",";
  }
  llvm::dbgs() << ">\n";
}

