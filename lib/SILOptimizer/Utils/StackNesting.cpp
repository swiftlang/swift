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

void StackNesting::setup(SILFunction *F) {
  SmallVector<BlockInfo *, 8> WorkList;
  llvm::DenseMap<SILBasicBlock *, BlockInfo *> BlockMapping;

  // We use pointers to BlockInfo structs. Therefore it's important that the
  // BlockInfos vector is never re-allocated.
  BlockInfos.clear();
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
        auto Alloc = cast<SingleValueInstruction>(&I);
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

  // Initialize all bit fields to 1s, expect 0s for the entry block.
  bool initVal = false;
  for (BlockInfo &BI : BlockInfos) {
    BI.AliveStackLocsAtEntry.resize(StackLocs.size(), initVal);
    initVal = true;
  }

  // First step: do a forward dataflow analysis to get the live stack locations
  // at the block exits.
  // This is necessary to get the live locations at blocks which end in
  // unreachable instructions (otherwise the backward data flow would be
  // sufficient). The special thing about unreachable-blocks is that it's
  // okay to have alive locations at that point, i.e. locations which are never
  // dealloced. We cannot get such locations with a purly backward dataflow.
  do {
    changed = false;

    for (BlockInfo &BI : BlockInfos) {
      Bits = BI.AliveStackLocsAtEntry;
      for (SILInstruction *StackInst : BI.StackInsts) {
        if (StackInst->isAllocatingStack()) {
          Bits.set(bitNumberForAlloc(StackInst));
        } else if (StackInst->isDeallocatingStack()) {
          Bits.reset(bitNumberForDealloc(StackInst));
        }
      }
      if (Bits != BI.AliveStackLocsAtExit) {
        BI.AliveStackLocsAtExit = Bits;
        assert(!(BI.Block->getTerminator()->isFunctionExiting() && Bits.any())
               && "stack location is missing dealloc");
        changed = true;
      }
      // Merge the bits into the successors.
      for (BlockInfo *SuccBI : BI.Successors) {
        SuccBI->AliveStackLocsAtEntry &= Bits;
      }
    }
  } while (changed);

  // Second step: do a backward dataflow analysis to extend the lifetimes of
  // no properly nested allocations.
  do {
    changed = false;

    // It's a backward dataflow problem.
    for (BlockInfo &BI : reversed(BlockInfos)) {
      // Collect the alive-bits (at the block exit) from the successor blocks.
      for (BlockInfo *SuccBI : BI.Successors) {
        BI.AliveStackLocsAtExit |= SuccBI->AliveStackLocsAtEntry;
      }
      Bits = BI.AliveStackLocsAtExit;

      if (isa<UnreachableInst>(BI.Block->getTerminator())) {
        // We treat unreachable as an implicit deallocation for all locations
        // which are still alive at this point.
        for (int BitNr = Bits.find_first(); BitNr >= 0;
             BitNr = Bits.find_next(BitNr)) {
          // For each alive location extend the lifetime of all locations which
          // are alive at the allocation point. This is the same as we do for
          // a "real" deallocation instruction (see below).
          Bits |= StackLocs[BitNr].AliveLocs;
        }
        BI.AliveStackLocsAtExit = Bits;
      }
      for (SILInstruction *StackInst : reversed(BI.StackInsts)) {
        if (StackInst->isAllocatingStack()) {
          int BitNr = bitNumberForAlloc(StackInst);
          if (Bits != StackLocs[BitNr].AliveLocs) {
            // More locations are alive around the StackInst's location.
            // Update the AlivaLocs bitset, which contains all those alive
            // locations.
            assert(Bits.test(BitNr) && "no dealloc found for alloc stack");
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
          Bits |= StackLocs[bitNumberForDealloc(StackInst)].AliveLocs;
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

static SILInstruction *createDealloc(SingleValueInstruction *Alloc,
                                     SILInstruction *InsertionPoint,
                                     SILLocation Location) {
  SILBuilderWithScope B(InsertionPoint);
  switch (Alloc->getKind()) {
    case SILInstructionKind::PartialApplyInst:
    case SILInstructionKind::AllocStackInst:
      assert((isa<AllocStackInst>(Alloc) ||
              cast<PartialApplyInst>(Alloc)->isOnStack()) &&
             "wrong instruction");
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
      auto *Alloc = StackLocs[LocNr].Alloc;
      InsertionPoint = createDealloc(Alloc, InsertionPoint,
                   Location.hasValue() ? Location.getValue() : Alloc->getLoc());
      changesMade = true;
    }
  }
  return changesMade;
}

// Insert deallocations at block boundaries.
// This can be necessary for unreachable blocks. Example:
//
//   %1 = alloc_stack
//   %2 = alloc_stack
//   cond_br %c, bb2, bb3
// bb2: <--- need to insert a dealloc_stack %2 at the begin of bb2
//   dealloc_stack %1
//   unreachable
// bb3:
//   dealloc_stack %2
//   dealloc_stack %1
StackNesting::Changes StackNesting::insertDeallocsAtBlockBoundaries() {
  Changes changes = Changes::None;
  for (const BlockInfo &BI : reversed(BlockInfos)) {
    for (unsigned SuccIdx = 0, NumSuccs = BI.Successors.size();
         SuccIdx < NumSuccs; ++SuccIdx) {

      BlockInfo *SuccBI = BI.Successors[SuccIdx];
      if (SuccBI->AliveStackLocsAtEntry == BI.AliveStackLocsAtExit)
        continue;

      // Insert deallocations for all locations which are alive at the end of
      // the current block, but not at the begin of the successor block.
      SILBasicBlock *InsertionBlock = SuccBI->Block;
      if (!InsertionBlock->getSinglePredecessorBlock()) {
        // If the current block is not the only predecessor of the successor
        // block, we have to insert a new block where we can add the
        // deallocations.
        InsertionBlock = splitEdge(BI.Block->getTerminator(), SuccIdx);
        changes = Changes::CFG;
      }
      if (insertDeallocs(BI.AliveStackLocsAtExit, SuccBI->AliveStackLocsAtEntry,
                         &InsertionBlock->front(), None)) {
        if (changes == Changes::None)
          changes = Changes::Instructions;
      }
    }
  }
  return changes;
}

bool StackNesting::adaptDeallocs() {
  bool InstChanged = false;
  BitVector Bits(StackLocs.size());

  // Visit all blocks. Actually the order doesn't matter, but let's to it in
  // the same order as in solve().
  for (const BlockInfo &BI : reversed(BlockInfos)) {
    Bits = BI.AliveStackLocsAtExit;

    // Insert/remove deallocations inside blocks.
    for (SILInstruction *StackInst : reversed(BI.StackInsts)) {
      if (StackInst->isAllocatingStack()) {
        // For allocations we just update the bit-set.
        int BitNr = bitNumberForAlloc(StackInst);
        assert(Bits == StackLocs[BitNr].AliveLocs &&
               "dataflow didn't converge");
        Bits.reset(BitNr);
      } else if (StackInst->isDeallocatingStack()) {
        // Handle deallocations.
        SILLocation Loc = StackInst->getLoc();
        int BitNr = bitNumberForDealloc(StackInst);
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
  return InstChanged;
}

StackNesting::Changes StackNesting::correctStackNesting(SILFunction *F) {
  setup(F);
  if (!solve())
    return Changes::None;

  // Insert deallocs at block boundaries. This might be necessary in CFG sub
  // graphs which don't  reach a function exit, but only an unreachable.
  Changes changes = insertDeallocsAtBlockBoundaries();
  if (changes != Changes::None) {
    // Those inserted deallocs make it necessary to re-compute the analysis.
    setup(F);
    solve();
  }
  // Do the real work: extend lifetimes by moving deallocs.
  if (adaptDeallocs()) {
    if (changes == Changes::None)
      changes = Changes::Instructions;
  }

  return changes;
}

void StackNesting::dump() const {
  for (const BlockInfo &BI : BlockInfos) {
    if (!BI.Block)
      continue;

    llvm::dbgs() << "Block " << BI.Block->getDebugID();
    llvm::dbgs() << ": entry-bits=";
    dumpBits(BI.AliveStackLocsAtEntry);
    llvm::dbgs() << ": exit-bits=";
    dumpBits(BI.AliveStackLocsAtExit);
    llvm::dbgs() << '\n';
    for (SILInstruction *StackInst : BI.StackInsts) {
      if (StackInst->isAllocatingStack()) {
        auto AllocInst = cast<SingleValueInstruction>(StackInst);
        int BitNr = StackLoc2BitNumbers.lookup(AllocInst);
        llvm::dbgs() << "  alloc #" << BitNr << ": alive=";
        dumpBits(StackLocs[BitNr].AliveLocs);
        llvm::dbgs() << ",     " << *StackInst;
      } else if (StackInst->isDeallocatingStack()) {
        auto *AllocInst = cast<SingleValueInstruction>(StackInst->getOperand(0));
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
  llvm::dbgs() << '>';
}
