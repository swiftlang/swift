//===--- TFCanonicalizeCFG.cpp - Transform CFG into SESE regions ----------===//
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
// This file is responsible for canonicalizing semi-structured CFGs into well
// structured CFGs that can be reduced into Single-Entry-Single-Exit (SESE)
// regions, composed of properly nested "if" diamonds and while loops.  This
// transformation gives the subsequent graph generation pass a simple form that
// is known to translate into XLA control flow primitives.
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "canonicalize-cfg-for-xla"

#include "TFCanonicalizeCFG.h"
#include "TFUtilities.h"
#include "swift/AST/TensorFlow.h"
#include "swift/Basic/TransformArrayRef.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/GraphOperationBuilder.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILUndef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/EquivalenceClasses.h"

using namespace swift;
using namespace tf;

static llvm::cl::opt<bool> TFEnsureSingleLoopExit(
    "tf-ensure-single-loop-exit", llvm::cl::init(true),
    llvm::cl::desc("Transform loops to have a single exit from header."));
static llvm::cl::opt<bool> TFNoUndefsInSESE(
    "tf-no-undefs-in-sese", llvm::cl::init(true),
    llvm::cl::desc(
        "Try to eliminate undefs in when performing "
        "sese canonicalization of loops (intended  for debugging)."));

//===----------------------------------------------------------------------===//
// SESERegionTree Implementation
//===----------------------------------------------------------------------===//

SESERegionTree::~SESERegionTree() {}

void SESERegionTree::dump() const {
  print(llvm::errs());
  llvm::errs() << "\n";
}

void SESERegionTree::print(llvm::raw_ostream &OS, unsigned indent) const {
  switch (getKind()) {
  case SingleBlock: return cast<SingleBlockSESERegion>(this)->print(OS, indent);
  case Sequence:    return cast<SequenceSESERegion>(this)->print(OS, indent);
  case WhileLoop:   return cast<WhileLoopSESERegion>(this)->print(OS, indent);
  case Conditional: return cast<ConditionalSESERegion>(this)->print(OS, indent);
  case Function:    return cast<FunctionSESERegion>(this)->print(OS, indent);
  }
};

void SingleBlockSESERegion::
print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "block ";
  BB->printAsOperand(OS);
}

void SequenceSESERegion::print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "[sequence";
  for (auto &n : getNodes()) {
    OS << "\n";
    n->print(OS, indent+2);
  }
  OS << "]";
}

void FunctionSESERegion::print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "{function\n";
  functionRegionTree->print(OS, indent + 2);
  OS << "\n";
  OS.indent(indent) << "}";
}

void WhileLoopSESERegion::print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "<while Preheader: ";
  preheader->printAsOperand(OS);
  OS << ", Header: ";
  header->printAsOperand(OS);
  OS << ", exit: ";
  exit->printAsOperand(OS);
  OS << "\n";
  body->print(OS, indent+2);
  OS << ">";
}

void ConditionalSESERegion::print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "{condition Header: ";
  branchBB->printAsOperand(OS);
  OS << "\n";
  if (auto t = getTrue()) {
    t->print(OS, indent+2);
  } else {
    OS.indent(indent+2) << "<<null true condition block>>";
  }
  OS << "\n";
  if (auto f = getFalse()) {
    f->print(OS, indent+2);
  } else {
    OS.indent(indent+2) << "<<null false condition block>>";
  }
  OS << "}";
}

//===----------------------------------------------------------------------===//
// CFG Canonicalization Implementation
//===----------------------------------------------------------------------===//

// Our partitioning and other transformations can leave around lots of
// unconditional branches between blocks that formerly had control edges.  Go
// through and merge those to make later passes simpler.
bool tf::contractUncondBranches(SILFunction *fn, DominanceInfo* DI, SILLoopInfo *LI) {
  bool changed = false;
  // Iterate carefully to avoid invalidating iterators: we mutate the block list
  // while we walk it.
  for (auto bbi = fn->begin(), e = fn->end(); bbi != e;) {
    auto *bb = &*bbi;
    if (mergeBasicBlockWithSuccessor(bb, DI, LI)) {
      // The block was merged with this successor. Therefore, revisit this node:
      // we have new successor(s) and may need to contract them as well.  Also,
      // bbi may be invalidated at this point.
      changed = true;
      bbi = SILFunction::iterator(bb);
    } else {
      // Move to the next block if this was not merged.
      ++bbi;
    }
  }
  return changed;
}

namespace {
  class SESERegionBuilder {
    DominanceInfo DI;
    PostDominanceInfo PDI;
    SILLoopInfo LI;
    SILFunction* F;

    /// processLoops fills in this mapping: it is keyed by the preheader block
    /// of each loop, and points to the region produced for it.
    llvm::DenseMap<SILBasicBlock*, WhileLoopSESERegion*> loopPreheaders;

    /// Map that keeps track of the underlying SESERegionTree for the
    /// FunctionSESERegion that starts at the given block.
    llvm::DenseMap<SILBasicBlock *,
                   std::pair<std::shared_ptr<SESERegionTree>, SILBasicBlock *>>
        functionRegions;

  public:
    SESERegionBuilder(SILFunction *F) : DI(F), PDI(F), LI(F, &DI), F(F) {}

    std::unique_ptr<SESERegionTree>
    processAcyclicRegion(SILBasicBlock *startBB, SILBasicBlock *endBB);

    /// Process all of the top-level loops in the function in post-order.
    void processLoops() {
      if (auto outs = getTFDumpIntermediateStream()) {
        dumpTopLevelLoopInfo(outs, "Before");
      }

      // Apply the standard SIL loop canonicalization transformations.  This
      // automatically gives us the following invariants: loops are guaranteed
      // to have a single preheader, a single backedge block, and exit??
      if (canonicalizeAllLoops(&DI, &LI)) {
        // Recalculate PDI if canonicalization made any changes.
        PDI.recalculate(*F);
      }
      // CanonicalizeAllLoops only ensures the following properties:
      //   - There is a unique preheader block.
      //   - Exit blocks have only predecessors within the loop.
      //   - There is a single latch block and a single back edge to the header.
      // We also need to ensure the following invariants for lowering:
      //   - The header is the only block from which loop is exited.
      //   - There is a single exit block.
      if (TFEnsureSingleLoopExit) {
        ensureSingleExitFromLoops();
      }

      for (auto *loop : LI)
        processLoop(loop);

      if (auto outs = getTFDumpIntermediateStream()) {
        dumpTopLevelLoopInfo(outs, "After");
      }

    }


  private:
    std::unique_ptr<SESERegionTree>
    processAcyclicRegionExcludingEnd(SILBasicBlock *startBB,
                                     SILBasicBlock *endBB);
    void processLoop(SILLoop *loop);
    void ensureSingleExitFromLoops();

    // Dump top-level loop information for debugging purposes.
    void dumpTopLevelLoopInfo(llvm::raw_ostream* outs, const char* stage) {
      *outs << "--- XLA CFG Loops " << stage << " Canonicalize: " << F->getName()
            << "\n";
      for (auto *loop : LI.getTopLevelLoops()) {
        loop->print(*outs);
      }
      *outs << "\n--- XLA CFG Loops " << stage << " Canonicalize end\n";
      outs->flush();
    }

  };
} // end anonymous namespace

/// Transform the specified acyclic region (possibly with internal while loop
/// nodes collapsed) from startBB to endBB inclusive into properly nested SESE
/// regions and return them.
std::unique_ptr<SESERegionTree>
SESERegionBuilder::processAcyclicRegion(SILBasicBlock *startBB,
                                        SILBasicBlock *endBB) {
  // Process the bulk of the region.
  auto startRegion = processAcyclicRegionExcludingEnd(startBB, endBB);

  // Create the end block.
  auto endRegion =
    std::unique_ptr<SESERegionTree>(new SingleBlockSESERegion(endBB));

  // Merge the end block into the whatever the start of the region was.
  if (!startRegion)
    return endRegion;
  if (auto seq = dyn_cast<SequenceSESERegion>(startRegion.get())) {
    seq->addNode(std::move(endRegion));
    return startRegion;
  }

  std::unique_ptr<SESERegionTree> both[] = {
    std::move(startRegion), std::move(endRegion)
  };

  auto result = new SequenceSESERegion(both);
  return std::unique_ptr<SESERegionTree>(result);
}


/// Transform the specified acyclic region (possibly with internal while loop
/// nodes collapsed) from startBB to endBB inclusive into properly nested SESE
/// regions and return them.
///
/// This does not process endBB, and returns a null region tree if
/// startBB == endBB.
///
std::unique_ptr<SESERegionTree>
SESERegionBuilder::processAcyclicRegionExcludingEnd(SILBasicBlock *startBB,
                                                    SILBasicBlock *endBB) {
  assert(PDI.dominates(endBB, startBB) &&
         "endBB is required to post-dominate startBB");
  SmallVector<std::unique_ptr<SESERegionTree>, 4> results;

  // Iteratively work our way up the post-dominator tree (moving currentBB until
  // we reach the endBB), producing a sequence of loops, diamonds, and single
  // block nodes as we go.
  SILBasicBlock *currentBB = startBB;
  while (currentBB != endBB) {
    // If currentBB is not dominated by startBB, we create a function region.
    if (!DI.dominates(startBB, currentBB)) {
      auto iter = functionRegions.find(currentBB);
      if (iter == functionRegions.end()) {
        // Create and cache the function region.
        llvm::SmallVector<SILBasicBlock *, 32> descendants;
        DI.getDescendants(currentBB, descendants);
        SILBasicBlock *subRegionEndBB = startBB;
        for (SILBasicBlock *otherBB : descendants) {
          subRegionEndBB = PDI.findNearestCommonDominator(subRegionEndBB, otherBB);
        }
        // We need to create a new SESE Region so that this will end up as a
        // separate function in the lowered graph.
        std::shared_ptr<SESERegionTree> subSESERegion(
            processAcyclicRegion(currentBB, subRegionEndBB).release());
        SILBasicBlock *nextBB = nullptr;
        if (subRegionEndBB == endBB) {
          nextBB = endBB;
        } else {
          auto *branch = dyn_cast<BranchInst>(subRegionEndBB->getTerminator());
          assert(branch &&
                 "Function region should end with an unconditional branch");
          nextBB = branch->getDestBB();
        }
        auto emplace_result = functionRegions.try_emplace(
            currentBB, std::make_pair(subSESERegion, nextBB));
        iter = emplace_result.first;
      }
      results.push_back(llvm::make_unique<FunctionSESERegion>(iter->second.first));
      currentBB = iter->second.second;
      continue;
    }
    // If this ends with a loop, it will already have been processed and
    // collapsed into a single node.  Just use it.
    auto loopIt = loopPreheaders.find(currentBB);
    if (loopIt != loopPreheaders.end()) {
      auto whileNode = loopIt->second;
      loopPreheaders.erase(loopIt);

      results.push_back(std::unique_ptr<SESERegionTree>(whileNode));
      currentBB = whileNode->getExit();
      continue;
    }

    // If currentBB ends with an unconditional branch, then just add it to the
    // sequence and keep going.  The destination of the branch may have multiple
    // successors in the case where the destination is endBB.  The successors
    // could be coming from a parent conditional region.
    if (auto *branch = dyn_cast<BranchInst>(currentBB->getTerminator())) {
      auto startRegion = new SingleBlockSESERegion(currentBB);
      results.push_back(std::unique_ptr<SESERegionTree>(startRegion));
      currentBB = branch->getDestBB();
      continue;
    }

    // Otherwise, we know that currentBB ends with a conditional branch.
    auto *condBr = cast<CondBranchInst>(currentBB->getTerminator());

    // Get the immediate postdominator of endBB, which defines a SESE
    // subregion postdominated by currentBB and postdominating endBB.  Note that
    // the postidom may in fact be endBB.
    auto postidom = PDI[currentBB]->getIDom()->getBlock();

    // Analyze the successors of the branch: each of them is post dominated by
    // endBB (and any of the successors may be exactly endBB).
    auto trueRegion =
      processAcyclicRegionExcludingEnd(condBr->getTrueBB(), postidom);
    auto falseRegion =
      processAcyclicRegionExcludingEnd(condBr->getFalseBB(), postidom);

    // Finally, form our conditional region.
    auto condRegion = new ConditionalSESERegion(currentBB, std::move(trueRegion),
                                                std::move(falseRegion));
    results.push_back(std::unique_ptr<SESERegionTree>(condRegion));
    currentBB = postidom;
  }

  switch (results.size()) {
  case 0: return std::unique_ptr<SESERegionTree>();
  case 1: return std::move(results[0]);
  default:
    auto result = new SequenceSESERegion(results);
    return std::unique_ptr<SESERegionTree>(result);
  }
}

/// Create a TF handle of the appropriate integer type for the given bitwith
/// that is initialized to the given value.
static SILValue createTFIntegerConst(GraphFunctionDeviceInfo &deviceInfo,
                                     SILBuilder &builder, SILLocation location,
                                     unsigned bitwidth, intmax_t value) {
  ASTContext& context = builder.getASTContext();
  SILType intType =
      SILType::getBuiltinIntegerType(bitwidth, builder.getASTContext());
  // Literals take attributes specifying the dtype, value, and device.
  GraphOperationBuilder opBuilder("Const");
  opBuilder.addAttribute(
      {context.getIdentifier("dtype$dtype"),
       convertSwiftTypeToConstantTFDataType(intType.getASTType())});
  opBuilder.addAttribute(
      {context.getIdentifier("value$tensor"),
       SymbolicValue::getInteger(APInt(bitwidth, value),
                                 context.getAllocator())});
  deviceInfo.handleDevicePlacement(
      "Const",
      /*opDevice*/ getDeviceString(DeviceType::ALL),
      builder.getModule().getASTContext(), &opBuilder);
  GraphOperationInst *constNode = opBuilder.build(
      builder, context, location,
      {convertElementTypeToTensorValueType(intType)});
  assert(constNode->getNumResults() == 1);
  return constNode->getResults()[0];
}

namespace {

class BasicBlockCloner : public SILClonerWithScopes<BasicBlockCloner> {
private:
  /// The flag to track if  this cloner was used to clone any blocks.
  bool cloned;

public:
  BasicBlockCloner(SILFunction &F)
      : SILClonerWithScopes(F), cloned(false) {}

  bool hasCloned() const { return cloned; }

  /// Create a block and clone the arguments alone.
  SILBasicBlock *initBlock(SILBasicBlock *bb) {
    auto bbIt = BBMap.find(bb);
    if (bbIt != BBMap.end())
      return bbIt->second;

    cloned = true;

    SILFunction &F = getBuilder().getFunction();
    SILBasicBlock *newBB = F.createBasicBlock();
    getBuilder().setInsertionPoint(newBB);
    BBMap[bb] = newBB;
    // If the basic block has arguments, clone them as well.
    for (auto *arg : bb->getArguments()) {
      // Create a new argument and copy it into the ValueMap so future
      // references use it.
      ValueMap[arg] = newBB->createPhiArgument(
          arg->getType(), arg->getOwnershipKind(), arg->getDecl());
    }
    return newBB;
  }

  // Clone all the instructions and return the cloned block.
  SILBasicBlock *cloneBlock(SILBasicBlock *bb) {
    auto bbIt = BBMap.find(bb);
    assert (bbIt != BBMap.end() && "Block is not initialied before cloning.");
    SILBasicBlock *newBB = bbIt->second;
    getBuilder().setInsertionPoint(newBB);
    for (auto &inst : *bb) {
      visit(&inst);
    }
    return newBB;
  }

  SILBasicBlock *initAndCloneBlock(SILBasicBlock *bb) {
    initBlock(bb);
    return cloneBlock(bb);
  }

  /// Handle references to basic blocks when cloning.
  SILBasicBlock *remapBasicBlock(SILBasicBlock *bb) {
    // If the block was not cloned by this cloner, directly reference it.
    // Otherwise, use the cloned block.
    auto bbIt = BBMap.find(bb);
    if (bbIt != BBMap.end())
      return bbIt->second;
    return bb;
  }

  SILValue remapValue(SILValue Value) {
    auto VI = ValueMap.find(Value);
    if (VI != ValueMap.end())
      return VI->second;
    return Value;
  }

  /// Update ValueMap so that occurrences of `oldValue` are replaced with
  /// `newValue` when cloning.
  void updateValueMap(SILValue oldValue, SILValue newValue)  {
    auto emplaceResult = ValueMap.try_emplace(oldValue, newValue);
    assert(emplaceResult.second && "Updating the same key in ValueMap multiple "
                                   "times during SESE cloning.");
  }

  /// Utility to unroll one iteration of the loop or to clone the entire loop.
  ///   - If `startBlock` is the same as the header of `loop`, we clone the
  ///     entire loop including the back edge.
  ///   - Otherwise, we unroll one iteration of the loop body starting from
  ///     `startBlock' to the latch.
  /// The unrolled or cloned version is nested into the parent loop.
  SILLoop *cloneOrUnrollLoop(SILLoopInfo *LI, SILLoop *loop, SILBasicBlock *startBlock) {
    llvm::DenseMap<SILLoop*, SILLoop*> loopClones;
    // This is for convenience as top-level loops have nullptr for parent loop.
    loopClones[nullptr] = nullptr;

    SmallVector<SILLoop *, 4> loops = LI->getBase().getLoopsInPreorder();
    auto loopIter = loops.begin();

    // Skip until we get to our loop in the pre-order.
    while (loopIter != loops.end() && *loopIter != loop) {
      loopIter++;
    }
    auto nestingLoop = loop->getParentLoop();
    if (loop->getHeader() == startBlock) {
      // If header is the start block, we are cloning the entire
      // loop. Therefore, we should create a new SILLoop.
      SILLoop *loopClone = LI->getBase().AllocateLoop();
      if (nestingLoop) {
        nestingLoop->addChildLoop(loopClone);
      } else {
        LI->addTopLevelLoop(loopClone);
      }
      loopClones[loop] = loopClone;
    } else {
      // We are not cloning the entire loop. Place cloned blocks in the
      // `outerLoop` instead.
      loopClones[loop] = nestingLoop;
    }

    // Move to the next loop.
    ++loopIter;

    // Create the loop nesting structure of the current loop's body by iterating
    // over all the loops nested within `loop` and creating empty clones. We
    // need do this first so that when we add a block to an inner loop using
    // `addBasicBlockToLoop`, it gets added to the parent loops as well.
    for (/*iterators initialized*/; loopIter != loops.end(); ++loopIter) {
      SILLoop *curLoop = *loopIter;
      SILLoop *parentLoop = curLoop->getParentLoop();
      // Break if we have reached the same nesting depth as `loop`, which
      // implies that we have visited all the subloops of `loop`.
      if (parentLoop == nestingLoop) break;
      SILLoop *loopClone = LI->getBase().AllocateLoop();
      SILLoop *parentLoopClone = loopClones[parentLoop];
      if (parentLoopClone) {
        parentLoopClone->addChildLoop(loopClone);
      } else {
        LI->addTopLevelLoop(loopClone);
      }
      loopClones[curLoop] = loopClone;
    }

    // Clone the body of the loop starting from the given startBlock.  We should
    // traverse the blocks in depth first order to ensure values are cloned
    // before they are used.
    SmallPtrSet<SILBasicBlock *, 32> worklist;
    SmallVector<SILBasicBlock *, 32> initializedBlocks;
    worklist.insert(startBlock);
    while (!worklist.empty()) {
      SILBasicBlock *current = *worklist.begin();
      worklist.erase(current);
      initBlock(current);
      initializedBlocks.push_back(current);
      for (SILBasicBlock *succ : current->getSuccessorBlocks()) {
        // Skip if succ is not a part of the loop, is already initialized, or
        // is the header.
        if (!loop->contains(succ) || remapBasicBlock(succ) != succ ||
            succ == loop->getHeader()) {
          continue;
        }
        worklist.insert(succ);
      }
    }
    for (SILBasicBlock *bb : initializedBlocks) {
      SILBasicBlock *clonedBlock = cloneBlock(bb);
      if (SILLoop *loopClone = loopClones[LI->getLoopFor(bb)]) {
        loopClone->addBasicBlockToLoop(clonedBlock, LI->getBase());
        if (LI->getLoopFor(bb)->getHeader() == bb) {
          loopClone->moveToHeader(clonedBlock);
        }
      }
    }
    return loopClones[loop];
  }
};

}  // namespace

// A helper class to transform a loop to have a single exit from the header.
class SingleExitLoopTransformer {
  public:
    // Convert the given loop into a SESE form. Returns false if the loop was
    // already in SESE form. Otherwise, returns true.
    static bool doIt(GraphFunctionDeviceInfo *deviceInfo, SILLoopInfo *LI,
                     DominanceInfo *DI, SILLoop *loop, PostDominanceInfo *PDI) {
      SingleExitLoopTransformer transformer(deviceInfo, LI, DI, loop, PDI);
      bool loopChanged = transformer.transform();
      if (loopChanged) {
        // Recalculate dominator information as it is stale now.
        DI->recalculate(*transformer.currentFn);
        PDI->recalculate(*transformer.currentFn);
      }

#ifndef NDEBUG
      {
        // Verify that the loop is OK after all the transformations.
        llvm::DenseSet<const SILLoop *> nestedLoops;
        loop->verifyLoopNest(&nestedLoops);
      }
#endif
      return loopChanged;
    }

  private:
    SingleExitLoopTransformer(GraphFunctionDeviceInfo *deviceInfo,
                              SILLoopInfo *LI, DominanceInfo *DI, SILLoop *loop,
                              PostDominanceInfo *PDI)
        : deviceInfo(deviceInfo), DI(DI), PDI(PDI), LI(LI), loop(loop),
          header(loop->getHeader()), preheader(loop->getLoopPreheader()),
          latch(loop->getLoopLatch()), currentFn(header->getParent()),
          oldHeaderNumArgs(header->getNumArguments()),
          hasUndefsAtPreheader(false) {
      assert(preheader &&
             "Canonicalization should have given us one preheader");
      assert(latch && "Canonicalization should have given us one latch block");
      initialize();
  }

  /// Transforms the loop to ensure it has a single exit from the header.
  /// Returns true if the CFG was changed.
  bool transform();

  // Helper functions

  void initialize();

  /// Return a map that captures information about what SILValue should be
  /// used at the pre-header of the loop for every SILValue in the given
  /// `values` set. If it cannot find a suitable SILValue for an entry in
  /// `values`, the corresponding will be mapped to an undef.
  llvm::DenseMap<SILValue, SILValue>
  getPreheaderSubstMap(const SmallPtrSetImpl<SILValue> &values) const;

  /// Transform the loop by moving and cloning nodes (as needed) so that the
  /// nearest common post dominator of the current exit blocks becomes a single
  /// exit block for the loop. Consider the following snippet:
  ///     while(...) {
  ///       if (...) {
  ///         ...
  ///         break;
  ///       }
  ///     }
  /// Recall that the blocks within if do not belong to the while loop in the SIL
  /// IR. The transformation implemented in this function has the effect of moving
  /// the blocks back into the loop.
  /// FIXME: Cloning is not implemented. Therefore, we don't get a single
  /// exit block right now. Once all the required components are implemented,
  /// we will get a single exit block.
  void ensureSingleExitBlock();

  void unrollLoopBodyOnce();

  /// Compute escaping values and what values to use as arguments at preheader.
  llvm::DenseMap<SILValue, SILValue> computeEscapingValuesSubstMap() const;

  /// Create a new header for the loop and return a pair consisting of
  /// the new block and the phi argument corresponding to the exitIndex.
  std::pair<SILBasicBlock *, SILValue> createNewHeader();

  /// Create a new latch block and clone all the arguments from new header.
  SILBasicBlock *createNewLatch(SILBasicBlock *newHeader);

  /// Replace the preheader->header edge with preheader->newHeader edge
  /// and update the arguments of preheader to match that of newheader.
  void patchPreheader(SILBasicBlock *newHeader);

  /// Patch the edges that go to the header and exit blocks to the new header or
  /// latch block as appropriate. Also, return the map consisting of indices
  /// assigned to the exit blocks.
  llvm::DenseMap<SILBasicBlock *, intmax_t>
  patchEdges(SILBasicBlock *newHeader, SILBasicBlock *latchBlock);

  /// Build a demuxing if..then..else block that can be used as a single
  /// exit block. This will not have any effect if there is a single exit
  /// block already.
  ///   \param exitIndices   Map from exit blocks to the assignedindex.
  ///   \param exitIndexArg  SILValue that holds the exitIndex set in the loop.
  SILBasicBlock *createNewExitBlockWithDemux(
      const llvm::DenseMap<SILBasicBlock *, intmax_t> &exitIndices,
      SILValue exitIndexArg);

  /// Replace `termInst` with a new TermInst such that the edge to the target
  /// consists of the original arguments and the given `newArgs` at the end.
  /// The old termInst is removed and this returns the newly constructed one.
  TermInst *appendArguments(TermInst *termInst, SILBasicBlock *target,
                            ArrayRef<SILValue> newArgs);

  /// Return an undef value of the given type.
  SILValue getUndef(SILType type) const {
    return SILUndef::get(type, currentFn->getModule());
  }

  // Configuration for graph construction.
  GraphFunctionDeviceInfo *deviceInfo;
  DominanceInfo *DI;
  PostDominanceInfo *PDI;
  SILLoopInfo *LI;
  SILLoop *loop;
  SILBasicBlock *header;
  SILBasicBlock *preheader;
  SILBasicBlock *latch;
  SILFunction *currentFn;
  unsigned oldHeaderNumArgs;
  /// Flag to track if we have undefs at preheader corresponding to escaping
  /// values and exit args.
  bool hasUndefsAtPreheader;
  /// Equivalence classes induced by argument passing.
  llvm::EquivalenceClasses<SILValue> equivalentValues;
  /// exit blocks of the loop.
  SmallPtrSet<SILBasicBlock*, 8> exitBlocks;
  /// The list of edges that need to rewired to the newHeader or
  // the new latchBlock as appropriate.
  SmallVector<std::pair<const SILBasicBlock *, const SILBasicBlock *>, 8>
      edgesToFix;
  /// Identify the set of values that escape the loop. The key represents the
  /// escaping value and the associated value will be used as the argument at
  /// the preheader.
  llvm::DenseMap<SILValue, SILValue> escapingValueSubstMap;
  /// Similar to escapingValueSubstMap, but arguments of exit blocks.
  llvm::DenseMap<SILValue, SILValue> exitArgSubstMap;
  // Map from an arg of an exit block to the corresponding newly added header arg.
  llvm::DenseMap<SILValue, SILValue> exitArgHeaderArgMap;
};

void SingleExitLoopTransformer::initialize() {
  // Remember some information from the loop before it gets transformed.

  // Build the equivalence classes induced by argument passing.
  for (auto &bb : *currentFn) {
    for (auto arg : bb.getArguments()) {
      SmallVector<SILValue, 8> incomingValues;
      arg->getIncomingPhiValues(incomingValues);
      for (SILValue incomingValue : incomingValues) {
        equivalentValues.unionSets(arg, incomingValue);
      }
    }
  }

  // Compute common exit block if needed.
  if (TFNoUndefsInSESE) {
    ensureSingleExitBlock();
  }

  // Initialize Exit Blocks
  {
    SmallVector<SILBasicBlock*, 8> exitBlockList;
    // This API returns duplicates if there are multiple exiting edges to the
    // same exit block. Dedup them here.
    loop->getExitBlocks(exitBlockList);
    exitBlocks.insert(exitBlockList.begin(), exitBlockList.end());
    assert(!exitBlocks.empty() && "A loop has no exit blocks.");
    if (TFNoUndefsInSESE) {
      SmallPtrSet<SILValue, 8> exitArgs;
      for (const SILBasicBlock *bb : exitBlocks) {
        for (auto arg : bb->getArguments()) {
          exitArgs.insert(arg);
        }
      }
      exitArgSubstMap = getPreheaderSubstMap(exitArgs);
    }
  }

  // All the exiting edges need to be rewired.
  loop->getExitEdges(edgesToFix);
  edgesToFix.emplace_back(latch, header);
  // Split critical edges in edgesToFix before we do any transformations.
  for (auto &edge : edgesToFix) {
    SILBasicBlock *src = const_cast<SILBasicBlock *>(edge.first);
    SILBasicBlock *tgt = const_cast<SILBasicBlock *>(edge.second);
    SILBasicBlock *splitBlock = splitIfCriticalEdge(src, tgt, /*DT*/ nullptr, LI);
    if (splitBlock != nullptr) {
      // If the edge was critical then splitBlock would have been inserted
      // between src and tgt as follows: src -> splitBlock -> tgt.  Therefore,
      // update src as the new edge to patch would be splitBlock -> tgt.
      edge.first = splitBlock;
    }
  }

  escapingValueSubstMap = computeEscapingValuesSubstMap();
}

void SingleExitLoopTransformer::ensureSingleExitBlock() {
  BasicBlockCloner cloner(*currentFn);

  // Identify the common post dominator
  SILPrintContext printContext(llvm::dbgs());
  SmallVector<SILBasicBlock*, 8> exitBlockList;
  loop->getExitBlocks(exitBlockList);

  auto exitBlockIter = exitBlockList.begin();
  SILBasicBlock *nearestCommonPD = *exitBlockIter++;
  while (exitBlockIter != exitBlockList.end()) {
    nearestCommonPD =
      PDI->findNearestCommonDominator(nearestCommonPD, *exitBlockIter++);
    assert(nearestCommonPD);
  }
  LLVM_DEBUG(
      llvm::dbgs() << "Common Exit Block : "
                   << SILPrintContext(llvm::dbgs()).getID(nearestCommonPD)
                   << "\n");

  // Compute the set of preheaders of loops that are unrelated to our loop w.r.t
  // nesting. This will be used when needing to identify cases where a loop
  // from outside is moved into the current loop. e.g.,
  //   while ... {
  //     if ... {
  //       for(...) {...} // This should be nested into the while loop.
  //       break;
  //     }
  //   }
  // The unrelated loops are those that are not contained within each other.
  SmallPtrSet<SILBasicBlock *, 32> unrelatedPreheaders;
  for (auto *otherLoop : *LI) {
    if (!otherLoop->contains(loop) && !loop->contains(otherLoop)) {
      unrelatedPreheaders.insert(otherLoop->getLoopPreheader());
    }
  }


  // Collect all the blocks from each exiting block up to nearest common PD.
  SmallPtrSet<SILBasicBlock *, 32> blocksToBeMoved;
  for (SILBasicBlock *exitBlock : exitBlockList) {
    if (exitBlock == nearestCommonPD) continue;
    SmallPtrSet<SILBasicBlock *, 32> worklist;
    worklist.insert(exitBlock);
    while (!worklist.empty()) {
      SILBasicBlock *current = *worklist.begin();
      blocksToBeMoved.insert(current);
      worklist.erase(current);
      for (unsigned edgeIdx : indices(current->getSuccessors())) {
        // We have to look up the successors each time around the loop
        // since if we split an edge, `current` block will have a new terminator
        // implying a new successor list. The new edge will be placed at the
        // same spot in the new terminator where the old edge was in the old
        // terminator. Thus as long as we use indices, we will visit all edges
        // appropriately and not deal with touching stale memory.
        auto succs = current->getSuccessors();
        auto *succ = succs[edgeIdx].getBB();
        // Skip if (1) already processed or (2) reached common pd.
        if (blocksToBeMoved.count(succ) > 0 || succ == nearestCommonPD) {
          continue;
        }

        // Check if `succ` is a preheader of another loop.
        SILLoop *succBlockLoop = nullptr;
        if (unrelatedPreheaders.count(succ) > 0) {
          // We are about a move a loop from outside. Perform canonicalization
          // of that loop first.
          SILBasicBlock *unrelatedHeader = succ->getSingleSuccessorBlock();
          assert(unrelatedHeader &&
                 "There should be a single successor for a preheader.");
          succBlockLoop = LI->getLoopFor(unrelatedHeader);
          SingleExitLoopTransformer::doIt(deviceInfo, LI, DI, succBlockLoop,
                                          PDI);
        }

        if (DI->properlyDominates(header, succ)) {
          worklist.insert(succ);
          continue;
        }
        // If `succ` is not dominated by `header`, then `succ` is reachable from
        // a node outside of this loop. We might have to clone `succ` in such
        // cases.

        // Before cloning make sure that header -> succ is *not* backedge of a
        // parent loop. This can happen when we have labeled breaks in loops. We
        // cannot clone the blocks in such cases. Simply continue. This is still
        // OK for our purposes because we will find an equivalent value at the
        // header for any value that escapes along this edge.
        if (DI->properlyDominates(succ, header)) continue;

        // Clone the block and rewire the edge.
        SILBasicBlock *clonedSucc = cloner.initAndCloneBlock(succ);
        // If `succ` is a preheader of an unrelated loop, we will have to clone
        // the entire loop now so that we can also incrementally update LoopInfo.
        if (succBlockLoop) {
          SILLoop *clonedLoop = cloner.cloneOrUnrollLoop(
              LI, succBlockLoop, succBlockLoop->getHeader());
          changeBranchTarget(clonedSucc->getTerminator(), 0,
                             clonedLoop->getHeader(), /*preserveArgs*/ true);
          // Note that all the nodes of `clonedLoop` should be moved into the
          // current loop.  We do that here itself as an optimization and also
          // because the dominator and post-dominator information for the new
          // blocks in `clonedLoop` are stale and cannot be relied upon.
          for (SILBasicBlock *bb : clonedLoop->getBlocks()) {
            blocksToBeMoved.insert(bb);
          }
          // Add the header to worklist for processing the exit edge.
          // (Other successor edges are already processed above.)
          worklist.insert(clonedLoop->getHeader());
        }
        changeBranchTarget(current->getTerminator(), edgeIdx, clonedSucc,
                           /*preserveArgs*/ true);
        worklist.insert(clonedSucc);
      }
    }
  }
  // Move blocks from each exitingBlock to commonExitBlock into the loop.
  for (SILBasicBlock *outsideBlock : blocksToBeMoved) {
    // Make sure that outsideBlock is *ONLY* reachable from a block in the loop
    // or blocksToBeMoved.
    assert(llvm::all_of(outsideBlock->getPredecessorBlocks(),
                        [this, &blocksToBeMoved](const SILBasicBlock *pred) {
                          return LI->getLoopFor(pred) == loop ||
                                 blocksToBeMoved.count(pred) > 0;
                        }) &&
           "Nodes being moved are reachable from outside loop.");

    // Update loop info if this belongs to a parent loop.
    SILLoop *outsideBlockLoop = LI->getLoopFor(outsideBlock);
    if (outsideBlockLoop == nullptr) {
      // outsideBlock is not part of any other loop. Simply add it to our loop.
      loop->addBasicBlockToLoop(outsideBlock, LI->getBase());
    } else {
      // We deal with the case where the nodes being moved in
      // belong to another loop. e.g.,
      // while ... {
      //   if ... {
      //     for(...) {...}
      //     break;
      //   }
      // }
      if (outsideBlockLoop->contains(loop)) {
        // If our `loop` is nested within `outsideBlockLoop`.  Move the node
        // from `outsideBlockLoop` into our `loop`.
        outsideBlockLoop->removeBlockFromLoop(outsideBlock);
        LI->changeLoopFor(outsideBlock, nullptr);
        loop->addBasicBlockToLoop(outsideBlock, LI->getBase());
      } else {
        // We should only nest `outsideBlockLoop` into our `loop` when we
        // process the very first node of the `outsideBlockLoop`. Check that we
        // have not already nested the `outsideBlockLoop` into our `loop`.
        if (!loop->contains(outsideBlockLoop)) {
          // Not yet nested, adjust the LoopInfo w.r.t nesting.
          if (outsideBlockLoop->getParentLoop() == nullptr) {
            // Remove from top-level loops as we are nesting it in `loop`.
            LI->removeLoop(llvm::find(*LI, outsideBlockLoop));
          }
          loop->addChildLoop(outsideBlockLoop);
        }
        // Add the block to this loop and all its parents.
        auto *L = loop;
        while (L) {
          L->addBlockEntry(outsideBlock);
          L = L->getParentLoop();
        }
      }
      // top-level loop is already correct.
    }
  }
  if (cloner.hasCloned()) {
    // TODO(https://bugs.swift.org/browse/SR-8336): the transformations here are
    // simple that we should be able to incrementally update the DI & PDI.
    DI->recalculate(*currentFn);
    PDI->recalculate(*currentFn);
  }
}

llvm::DenseMap<SILValue, SILValue>
SingleExitLoopTransformer::computeEscapingValuesSubstMap() const {
  llvm::SmallPtrSet<SILValue, 8> escapingValues;
  for (const SILBasicBlock *bb : loop->getBlocks()) {
    // Save the values that are escaping this loop in result set.
    auto saveEscaping = [this, &escapingValues](SILValue value) {
      for (const auto *use : value->getUses()) {
        const SILInstruction *useInst = use->getUser();
        if (!loop->contains(useInst->getParent())) {
          escapingValues.insert(value);
          break;
        }
      }
    };
    if (bb != header) {
      llvm::for_each(bb->getArguments(), saveEscaping);
    }
    for (const SILInstruction &inst : *bb) {
      llvm::for_each(inst.getResults(), saveEscaping);
    }
  }

  return getPreheaderSubstMap(escapingValues);
}

llvm::DenseMap<SILValue, SILValue>
SingleExitLoopTransformer::getPreheaderSubstMap(
    const SmallPtrSetImpl<SILValue> &values) const {
  llvm::DenseMap<SILValue, SILValue> result;
  for (const SILValue value : values) {
    result[value] = getUndef(value->getType());
  }
  // Do not eliminate undefs unless requested for.
  if (!TFNoUndefsInSESE) return result;

  // Replace undef with an equivalent value that is available at preheader.
  for (auto &kv : result) {
    const SILValue &escapingValue = kv.first;
    // Get the member iterator for the equivalence class of escapingValue.
    auto member_begin = equivalentValues.findLeader(escapingValue);

    // Iterate over *all* the members and find an equivalent value that
    // dominates the terminator instruction of the preheader.
    for (auto equivalentValue :
         make_range(member_begin, equivalentValues.member_end())) {
      if (DI->properlyDominates(equivalentValue, preheader->getTerminator())) {
        // Found a definition that we could use.
        kv.second = equivalentValue;
        break;
      }
    }
  }
  return result;
}

/// Appends the given arguments to the given edge. Deletes the old TermInst
/// and returns a new TermInst with the appropriate number of args.
TermInst *SingleExitLoopTransformer::appendArguments(
    TermInst *termInst, SILBasicBlock *target, ArrayRef<SILValue> newArgs) {
  SILBuilder builder(termInst);
  TermInst *newTermInst = nullptr;

  auto createArgsArray = [target,
                          &newArgs](const OperandValueArrayRef &currentArgs,
                                    bool appendNewArgs) {
    SmallVector<SILValue, 8> args(currentArgs.begin(), currentArgs.end());
    if (appendNewArgs) {
      args.append(newArgs.begin(), newArgs.end());
      assert(args.size() == target->getNumArguments() &&
             "Number of final arguments does not match target's arguments.");
    }
    return args;
  };
  if (auto *branch = dyn_cast<BranchInst>(termInst)) {
    assert(branch->getDestBB() == target &&
           "Incoming edge block and target do not match");
    SmallVector<SILValue, 8> args =
        createArgsArray(branch->getArgs(), /*appendNewArgs=*/true);
    newTermInst =
        builder.createBranch(branch->getLoc(), branch->getDestBB(), args);
  } else if (auto *condBranch = dyn_cast<CondBranchInst>(termInst)) {
    // At the moment we can only add arguments to br and cond_br.
    assert(condBranch && "Terminator is not a branch or conditional branch.");
    bool isTrueEdge = condBranch->getTrueBB() == target;
    assert(((isTrueEdge && condBranch->getTrueBB() == target) ||
            (!isTrueEdge && condBranch->getFalseBB() == target)) &&
           "Incoming edge block and target do not match");
    SmallVector<SILValue, 8> trueArgs = createArgsArray(
        condBranch->getTrueArgs(), /*appendNewArgs=*/isTrueEdge);
    SmallVector<SILValue, 8> falseArgs = createArgsArray(
        condBranch->getFalseArgs(), /*appendNewArgs=*/!isTrueEdge);
    newTermInst = builder.createCondBranch(
        condBranch->getLoc(), condBranch->getCondition(),
        condBranch->getTrueBB(), trueArgs, condBranch->getFalseBB(), falseArgs,
        condBranch->getTrueBBCount(), condBranch->getFalseBBCount());
  }
  // Remove the old terminator instruction.
  termInst->dropAllReferences();
  termInst->eraseFromParent();
  return newTermInst;
}

std::pair<SILBasicBlock *, SILValue>
SingleExitLoopTransformer::createNewHeader() {

  SILBuilder builder(header);
  ASTContext &context = builder.getASTContext();

  SILBasicBlock *newHeader = currentFn->createBasicBlock();
  loop->addBasicBlockToLoop(newHeader, LI->getBase()); // Should be done first.
  // Clone arguments and change all uses to the new header's arguments.
  newHeader->cloneArgumentList(header);
  SmallVector<SILValue, 8> headerArgs;
  for (auto i : indices(header->getArguments())) {
    header->getArgument(i)->replaceAllUsesWith(newHeader->getArgument(i));
    headerArgs.push_back(newHeader->getArgument(i));
  }
  header->dropAllArguments();
  // Add phi arguments in the new header corresponding to the escaping values.
  for (const auto &kv : escapingValueSubstMap) {
    SILValue escapingValue = kv.first;
    SILValue newValue = newHeader->createPhiArgument(
      escapingValue->getType(), escapingValue.getOwnershipKind());
    // Replace uses *outside* of the loop with the new value.
    auto UI = escapingValue->use_begin(), E = escapingValue->use_end();
    while (UI != E) {
      Operand *use = *UI;
      // Increment iterator before we invalidate it
      // when we invoke Operand::Set below.
      ++UI;
      if (loop->contains(use->getUser()->getParent())) {
        continue;
      }
      use->set(newValue);
    }
  }
  if (TFNoUndefsInSESE) {
    // Add arguments in the new header corresponding to exit block arguments.
    for (const auto &kv : exitArgSubstMap) {
      SILValue arg = kv.first;
      SILValue newValue =
        newHeader->createPhiArgument(arg->getType(), arg.getOwnershipKind());
      exitArgHeaderArgMap[kv.first] = newValue;
    }
  }
  // An integer to identify the exit edge.
  SILValue exitIndexArg = newHeader->createPhiArgument(
      convertElementTypeToTensorValueType(
          SILType::getBuiltinIntegerType(32, context)),
      ValueOwnershipKind::Owned);
  // A boolean corresponding to the stayInLoop flag.
  newHeader->createPhiArgument(convertElementTypeToTensorValueType(
                                   SILType::getBuiltinIntegerType(1, context)),
                               ValueOwnershipKind::Trivial);
  return std::make_pair(newHeader, exitIndexArg);
}

SILBasicBlock *
SingleExitLoopTransformer::createNewLatch(SILBasicBlock *newHeader) {
  // Create a new latch block.
  SILBasicBlock *latchBlock = currentFn->createBasicBlock();
  latchBlock->cloneArgumentList(newHeader);
  SILBuilder builder(latchBlock);
  SmallVector<SILValue, 8> latchArgs;
  for (const SILArgument *latchArg : latchBlock->getArguments()) {
    latchArgs.push_back(latchArg);
  }
  builder.createBranch(
    getUserSourceLocation(header->getTerminator()->getDebugLocation()),
      newHeader, latchArgs);
  loop->addBasicBlockToLoop(latchBlock, LI->getBase());
  return latchBlock;
}

void SingleExitLoopTransformer::patchPreheader(SILBasicBlock *newHeader) {
  // Update edge
  replaceBranchTarget(preheader->getTerminator(), header, newHeader,
                      /*preserveArgs*/ true);
  SILBuilder builder(preheader->getTerminator());
  SILLocation location(
    getUserSourceLocation(preheader->getTerminator()->getDebugLocation()));
  // Add arguments corresponding to escaping arguments.
  // State from within the loop is not available in the preheader.
  // Simply pass in an undef. This will never be accessed at runtime.
  SmallVector<SILValue, 8> newArgs;
  for (const auto &kv : escapingValueSubstMap) {
    hasUndefsAtPreheader |= isa<SILUndef>(kv.second);
    newArgs.push_back(kv.second);
  }
  if (TFNoUndefsInSESE) {
    for (const auto &kv : exitArgSubstMap) {
      hasUndefsAtPreheader |= isa<SILUndef>(kv.second);
      newArgs.push_back(kv.second);
    }
  }
  // `exitIndex` to identify the block to which we exit from the loop.
  newArgs.push_back(createTFIntegerConst(*deviceInfo, builder, location,
                                         /*bitwidth*/ 32,
                                         /*exitIndex*/ 0));
  // `stayInLoop` flag
  newArgs.push_back(createTFIntegerConst(*deviceInfo, builder, location,
                                         /*bitwidth*/ 1, true));
  appendArguments(preheader->getTerminator(), newHeader, newArgs);
}

llvm::DenseMap<SILBasicBlock *, intmax_t>
SingleExitLoopTransformer::patchEdges(SILBasicBlock *newHeader,
                                      SILBasicBlock *latchBlock) {

  llvm::DenseMap<SILBasicBlock *, intmax_t> exitIndices;

  // Identify the exit from the header (if any) and assign '0' as its index.
  SILBasicBlock *headerExit = nullptr;
  for (SILBasicBlock *succ : header->getSuccessorBlocks()) {
    if (loop->contains(succ))  continue;
    assert(headerExit == nullptr && "Loop header has more than one exit node.");
    headerExit = succ;
  }
  // Note: headerExit can be a nullptr as header need not be an exiting block.
  // e.g.,
  //   while (true) {
  //     ...
  //     if ... break;
  //   }
  if (headerExit != nullptr) {
    exitIndices.insert({headerExit, 0});
  }
  // edgesToFix also has edges to the header that are not exit edges.
  // To simplify the code below, simply assign an arbitrary value (say 0)
  // as the index for header. This would get simplified once we unify the
  // stayInloop flag and exitIndex into one value.
  exitIndices.insert({header, 0});
  unsigned nextExitIndex = 1;
  for (const auto &edge : edgesToFix) {
    SILBasicBlock *src = const_cast<SILBasicBlock *>(edge.first);
    SILBasicBlock *tgt = const_cast<SILBasicBlock *>(edge.second);
    SILBasicBlock *newTgt = latchBlock;
    bool stayInLoop = loop->contains(tgt);
    // Track the incoming value for the exit arguments if this is an exit edge
    // with arguments. This will be used to unify all the values to be passed
    // to the exit nodes in the loop header.
    //
    llvm::DenseMap<SILValue, SILValue> exitArgIncomingValue;
    if (TFNoUndefsInSESE && !stayInLoop && tgt->getNumArguments() != 0) {
      auto *termInst = src->getTerminator();
      auto *branch = dyn_cast<BranchInst>(termInst);
      assert(branch && "Critical edges should have been split.");
      for (unsigned i = 0; i < branch->getNumArgs(); ++i) {
        exitArgIncomingValue[tgt->getArgument(i)] = branch->getArg(i);
      }
    }

    replaceBranchTarget(src->getTerminator(), tgt, newTgt,
                        /*preserveArgs=*/stayInLoop);
    // Set up additional arguments.
    // If we are exiting the loop, then we should simply use newHeader args.
    SmallVector<SILValue, 8> newArgs;
    if (!stayInLoop) {
      for (unsigned i = 0; i < oldHeaderNumArgs; ++i) {
        newArgs.push_back(newHeader->getArgument(i));
      }
    }
    SILBuilder builder(src->getTerminator());
    SILLocation location(
        getUserSourceLocation(src->getTerminator()->getDebugLocation()));
    // Find an appropriate value to use for each escaping value.
    unsigned argIndex = oldHeaderNumArgs;
    for (const auto &kv : escapingValueSubstMap) {
      const SILValue escapingValue = kv.first;
      if (DI->properlyDominates(escapingValue, src->getTerminator())) {
        newArgs.push_back(escapingValue);
      } else {
        newArgs.push_back(newHeader->getArgument(argIndex));
      }
      ++argIndex;
    }
    if (TFNoUndefsInSESE) {
      //  Let p0, p1, ..pn be the arguments at new header corresponding to exit
      // arguments a0, a1, a2, ..., an. For a exit edge
      //      br exit_i(x, y) ->  exit_i(a2, a3)`,
      // change the source as br new_latch(p0, p1, x, y, p4, ..., pn)
      for (const auto &kv : exitArgSubstMap) {
        const SILValue exitArg = kv.first;
        auto iter = exitArgIncomingValue.find(exitArg);
        if (iter != exitArgIncomingValue.end()) {
          newArgs.push_back(iter->second);
        } else {
          newArgs.push_back(newHeader->getArgument(argIndex));
        }
        ++argIndex;
      }
    }

    // `exitIndex` to identify the block to which we exit from the loop.
    // (insert a new value or get the old key value pair.)
    auto emplaceResult = exitIndices.try_emplace(tgt, nextExitIndex);
    if (emplaceResult.second) {
      // Increment index as we inserted a new entry into the table.
      ++nextExitIndex;
    }
    auto kvPair = *emplaceResult.first;
    newArgs.push_back(createTFIntegerConst(*deviceInfo, builder, location,
                                           /*bitwidth*/ 32,
                                           /*exitIndex*/ kvPair.second));
    // `stayInLoop` flag
    newArgs.push_back(createTFIntegerConst(*deviceInfo, builder, location,
                                           /*bitwidth*/ 1, stayInLoop));
    appendArguments(src->getTerminator(), newTgt, newArgs);
  }
  return exitIndices;
}

/// Build a demuxing if..then..else block that can be used as a single
/// exit block. This will not have any effect if there is a single exit
/// block already.
SILBasicBlock *SingleExitLoopTransformer::createNewExitBlockWithDemux(
    const llvm::DenseMap<SILBasicBlock *, intmax_t> &exitIndices,
    SILValue exitIndexArg) {
  auto createBlockOutsideLoop = [this]() {
    SILBasicBlock *newBlock = currentFn->createBasicBlock();
    SILLoop *parentLoop = loop->getParentLoop();
    if (parentLoop) {
      parentLoop->addBasicBlockToLoop(newBlock, LI->getBase());
    }
    return newBlock;
  };

  // Create a new exit block. Strictly, we don't always need this block, but it
  // makes it slightly easier to implement the demux blocks. contractUncondEdges
  // will merge this block away if appropriate.
  SILBasicBlock *newExitBlock = createBlockOutsideLoop();

  SILBuilder builder(newExitBlock);
  ASTContext &context = builder.getASTContext();

  auto curBlockIter = exitBlocks.begin();
  SILBasicBlock *demuxBlock = *curBlockIter++;
  SILLocation headerLocation =
      getUserSourceLocation(header->getTerminator()->getDebugLocation());

  // Find the arguments at the header that were added for the exit arguments
  // and pass that along to the original exit block.
  auto remapExitArguments = [this](SILBasicBlock *exitingBlock,
                                   SILBasicBlock *exitBlock) {
    SmallVector<SILValue, 8> headerArgs;
    for (SILValue arg : exitBlock->getArguments()) {
      headerArgs.push_back(exitArgHeaderArgMap[arg]);
    }
    appendArguments(exitingBlock->getTerminator(), exitBlock, headerArgs);
  };

  while (curBlockIter != exitBlocks.end()) {
    SILBasicBlock *newBlock = createBlockOutsideLoop();
    SILBasicBlock *trueBlock = *curBlockIter++;
    builder.setInsertionPoint(newBlock);

    // Create a condition to compare exitIndex to a constant
    std::string equalOpName = "Equal";
    GraphOperationBuilder equalOpBuilder(equalOpName);
    equalOpBuilder.addArgument(exitIndexArg);
    equalOpBuilder.addArgument(
        createTFIntegerConst(*deviceInfo, builder, headerLocation,
                             /*bitwidth*/ 32, exitIndices.lookup(trueBlock)));
    deviceInfo->handleDevicePlacement(
        equalOpName, /*opDevice*/ getDeviceString(DeviceType::ALL),
        builder.getModule().getASTContext(), &equalOpBuilder);
    GraphOperationInst *condTensorInst = equalOpBuilder.build(
        builder, context, headerLocation,
        {convertElementTypeToTensorValueType(
            SILType::getBuiltinIntegerType(1, context))});
    assert(condTensorInst->getNumResults() == 1);

    GraphOperationInst *condValue = createTensorToInt1Inst(
        condTensorInst->getResults()[0], builder, headerLocation, *deviceInfo);
    builder.createCondBranch(headerLocation, condValue->getResults()[0],
                             trueBlock, demuxBlock);

    if (TFNoUndefsInSESE) {
      remapExitArguments(newBlock, trueBlock);
      remapExitArguments(newBlock, demuxBlock);
    }
    demuxBlock = newBlock;
  }
  builder.setInsertionPoint(newExitBlock);
  builder.createBranch(headerLocation, demuxBlock);
  if (TFNoUndefsInSESE) {
    remapExitArguments(newExitBlock, demuxBlock);
  }
  return newExitBlock;
}

bool SingleExitLoopTransformer::transform() {
  // Return if the loop is already in the required form and
  // if the header has no side-effects.
  if (loop->getExitBlock() && loop->getExitingBlock() &&
      loop->getExitingBlock() == loop->getHeader())  {
    bool hasEffectfulOps = false;
    for (const SILInstruction &inst : *loop->getHeader()) {
      if (auto graphOp = dyn_cast<GraphOperationInst>(&inst)) {
        StringRef name = graphOp->getName().str();
        // FIXME: generalize the logic for deciding side-effecting ops.
        if (name.startswith("tfc.SendToHost") ||
            name.startswith("tfc.RecvFromHost") ||
            name.startswith("WriteScalarSummary")) {
          hasEffectfulOps = true;
          break;
        }
      }
    }
    if (!hasEffectfulOps) {
      return false;
    }
  }
  SILBuilder builder(header);

  // Create a new header and get the exitIndex argument.
  std::pair<SILBasicBlock *, SILValue> headerResult = createNewHeader();
  SILBasicBlock *newHeader = headerResult.first;
  SILValue exitIndexArg = headerResult.second;

  // Create a new latch block.
  SILBasicBlock *latchBlock = createNewLatch(newHeader);

  patchPreheader(newHeader);

  // Patch the edges and return the map consisting of indices assigned
  // to the exit blocks.
  llvm::DenseMap<SILBasicBlock *, intmax_t> exitIndices =
      patchEdges(newHeader, latchBlock);

  // Create a new exit block that demuxes based on exitIndex.
  SILBasicBlock *newExitBlock =
      createNewExitBlockWithDemux(exitIndices, exitIndexArg);

  // Connect the newheader to the old header and new exit block.
  {
    builder.setInsertionPoint(newHeader);
    SILLocation headerLocation =
        getUserSourceLocation(header->getTerminator()->getDebugLocation());
    GraphOperationInst *loopExitCond = createTensorToInt1Inst(
        newHeader->getArguments().back(), builder, headerLocation, *deviceInfo);
    builder.createCondBranch(headerLocation, loopExitCond->getResults()[0],
                             header, newExitBlock);
  }

  // Update the loop header to newHeader.
  loop->moveToHeader(newHeader);

  if (TFNoUndefsInSESE) {
    // If we still have undefs at preheader, simply clone the loop body once
    // before the actual loop.
    if (hasUndefsAtPreheader) {
      unrollLoopBodyOnce();
    }
  }

  return true;
}

/// For loops in the SIL Function with multiple exit block successors,
/// transforms the loop by introducing a common exit switch block that demuxes
/// to the appropriate exit block. Updates the Dominanceinfo, PostDominanceinfo,
/// and LoopInfo as well.
void SESERegionBuilder::ensureSingleExitFromLoops() {
  GraphFunctionDeviceInfo deviceInfo(
      GraphFunctionDeviceInfo::getForFunction(*F, /*removeConfigInst*/ false));

  // Visit the loop nest hierarchy bottom up.
  bool changed = false;
  // The bool indicates whether the subloops are already processed.
  llvm::SmallVector<std::pair<SILLoop *, bool>, 16> workList;
  for (auto *L : LI.getTopLevelLoops())
    workList.push_back({L, L->empty()});
  while (!workList.empty()) {
    SILLoop *loop;
    bool subLoopsProcessed;
    std::tie(loop, subLoopsProcessed) = workList.pop_back_val();

    if (!subLoopsProcessed) {
      workList.push_back({loop, true});
      for (auto *Subloop : loop->getSubLoopRange()) {
        workList.push_back({Subloop, Subloop->empty()});
      }
      continue;
    }
    bool loopChanged =
      SingleExitLoopTransformer::doIt(&deviceInfo, &LI, &DI, loop, &PDI);
    changed |= loopChanged;
  }
  if (changed) {
    splitAllCondBrCriticalEdgesWithNonTrivialArgs(*F, &DI, &LI);
    contractUncondBranches(F, &DI, &LI);
    // TODO(https://bugs.swift.org/browse/SR-8336): the transformations here are
    // simple that we should be able to incrementally update PDI.
    PDI.recalculate(*F);
  }
}

void SingleExitLoopTransformer::unrollLoopBodyOnce() {
  // Consider the following example:
  //
  //   do {
  //     i += 1
  //     if (...) break;
  //     i += 2
  //   } while(...)
  //   return i
  //
  // The SIL CFG is shown below:
  //
  //   preheader:
  //     i0 = 0
  //     br header(i0)
  //
  //   header(i1):
  //     i2 = i1 + 1
  //     cond ??, break, body
  //
  //   break:
  //     br exit(i2)
  //
  //   body:
  //     i3 = i2 + 2
  //     cond ??, header(i3), exit(i3)
  //
  //   exit(i4):
  //     return i4
  //
  // Note that the dataflow between iterations of the loop is captured by
  // argument `i1` of the header. If we need to exit from the header of the
  // loop, we will need to "freeze" the state at the exit points of the loop
  // (`break` and `body` in the example).  The canonicalization pass does that
  // by creating a new header, which has arguments corresponding to the frozen
  // states, in addition to the arguments (such as `i1`) of the original
  // `header` block.
  //
  // The canonicalized CFG is shown below. Argument `i4` of `newHeader` is one
  // of the additional arguments added by the canonicalization pass. `newHeader`
  // also has a `stayInLoop` argument to determine when we should exit the loop.
  //
  // The propagation of the frozen state is done as follows:
  //   - The original branches to `exit` are replaced with branches to
  //     `newLatch`, where `stayInLoop` is set to false.
  //   - For the original argument `i1` at the header, we simply pass back that
  //     value. This is OK because we exit the loop before accessing it.
  //   - For the additional argument `i4` at the header, we pass the current
  //     state at the exit.
  //
  //   preheader:
  //     i0 = 0
  //     br newHeader(i0, undef, true)
  //
  //   newHeader(i1, i4, stayInLoop):
  //     cond stayInLoop, header, exit(i4)
  //
  //   header:
  //     i2 = i1 + 1
  //     cond ??, break, body
  //
  //   break:
  //     br newLatch(i1, i2, false)
  //
  //   body:
  //     i3 = i2 + 2
  //     cond ??, newLatch(i3, i4, true), newLatch(i1, i3, false)
  //
  //   newLatch(i5, i6, stayInLoop1):
  //     br newHeader(i5, i6, stayInLoop1)
  //
  //   exit(i7):
  //     return i7
  //
  // We have an undef for `i5` from the preheader because we do not know the
  // value before the loop gets executed. However, note that on all paths from
  // `header` to `newLatch`, the second argument is always defined. Therefore,
  // we can eliminate the `undef` by simply unrolling the loop body from
  // `header` to `newLatch` once. We do not need to clone `newHeader`.
  //
  // Given that we do not clone `newHeader`, references to the arguments `i1`
  // and `i4` of `newHeader` will not be cloned. For example, in the unrolled
  // body of the loop, `break` will be cloned as follows (prime refers to the
  // clones):
  //    break':
  //      br newLatch'(i1, i2', false)
  //
  // Note that `i1` still refers to the original argument of `newHeader`.
  // `newHeader` occurs after the unrolled loop body (in CFG) and hence, won't
  // dominate `break'` in the unrolled loop body. We fix this by
  // picking a value that is equivalent to the corresponding argument and
  // dominates `break'`. (Note that it is enough to search for equivalent values
  // among the immediate predecessors of the `newLatch'` block.)
  //
  // In summary, there are two steps for eliminating undefs:
  //    (1) Unroll the loop body once (excluding `newHeader`)
  //    (2) Patch values that still refer to the arguments of `newHeader`.

  // Step 1. Unroll the loop body once (excluding `newHeader`)
  //
  BasicBlockCloner cloner(*currentFn);
  // Setup cloner so that newHeader's arguments are replaced with values in
  // preheader.
  SILBasicBlock *newHeader = loop->getHeader();
  auto preheaderTermInst = dyn_cast<BranchInst>(preheader->getTerminator());
  assert(preheaderTermInst && "Preheader of a loop has a non-branch terminator");
  for (unsigned argIndex = 0; argIndex < oldHeaderNumArgs; ++argIndex) {
    auto preheaderArg = preheaderTermInst->getArg(argIndex);
    auto newHeaderArg = newHeader->getArgument(argIndex);
    cloner.updateValueMap(newHeaderArg, preheaderArg);
  }

  // Clone everything starting from the old header.
  cloner.cloneOrUnrollLoop(LI, loop, header);

  // Get the clone for old header.
  SILBasicBlock *clonedOldHeader = cloner.remapBasicBlock(header);
  replaceBranchTarget(preheader->getTerminator(), newHeader, clonedOldHeader,
                      /*preserveArgs*/ false);

  // Step 2. Patch values that still refer to the arguments of `newHeader`.
  //
  SILBasicBlock *newLatch = loop->getLoopLatch();
  SILBasicBlock *clonedNewLatch = cloner.remapBasicBlock(newLatch);
  // Note that we iterate over the predecessors of the `newLatch` and not
  // `clonedNewLatch`. This is important for a couple of reasons:
  //  (1) DI and PDI information is not valid for the cloned nodes. Therefore, we
  //      can only check for dominance using the original nodes of the loop body.
  //  (2) changeEdgeValue invalidates pred_iterator. Given that the unrolled
  //      loop body is a clone of the loop, it is convenient to iterate over the
  //      predecessors of the latch block in the loop and access the corresponding
  //      cloned predecessor whenever needed.
  for (SILBasicBlock *pred : newLatch->getPredecessorBlocks()) {
    auto predTermInst = dyn_cast<BranchInst>(pred->getTerminator());
    assert(predTermInst && "Preheader of a loop has a non-branch terminator");
    for (unsigned argIndex = 0; argIndex < predTermInst->getNumArgs(); ++argIndex) {
      // Check if the argument of *cloned* predecessor needs patching.
      SILBasicBlock *clonedPred = cloner.remapBasicBlock(pred);
      auto clonedPredTermInst = dyn_cast<BranchInst>(clonedPred->getTerminator());
      assert(clonedPredTermInst &&
             "Preheader of a loop has a non-branch terminator");
      auto arg = clonedPredTermInst->getArg(argIndex);
      // Skip if this is not an argument of the `newHeader`.
      if (!isa<SILArgument>(arg) ||
          cast<SILArgument>(arg)->getParent() != newHeader) {
        continue;
      }
      // Iterate over the incoming values of the corresponding argument in the
      // latch block and pick one that is suitable to be used here.
      auto destBBArg = newLatch->getArgument(argIndex);
      SmallVector<SILValue, 8> incomingValues;
      destBBArg->getIncomingPhiValues(incomingValues);
      bool patched = false;
      for (auto value : incomingValues) {
        if (value != arg && DI->properlyDominates(value, predTermInst)) {
          // A suitable value is found. Update the edge value in the unrolled
          // loop with the corresponding cloned value.
          changeEdgeValue(clonedPred->getTerminator(), clonedNewLatch, argIndex,
                          cloner.remapValue(value));
          patched = true;
          break;
        }
      }
      assert(patched && "Unable to patch the arguments in unrolled loop body.");
    }
  }
}

/// Process the specified loop, collapsing it into a SESE region node.  This
/// forms a WhileLoopSESERegion node and puts it into the loopPreheaders data
/// structure, allowing the outer level's acyclic region handling to pick it
/// up.
void SESERegionBuilder::processLoop(SILLoop *loop) {
  // If there are any nested loops within this one, transform them inside out.
  for (auto *nested : *loop) {
    processLoop(nested);
  }

  // We already canonicalized the loop tree, so we know that each loop should
  // have exactly one preheader node (which jumps from outside the loop to the
  // loop header) and one latch block (which jumps from inside the loop to the
  // loop header).  There will be no other predecessors of the header block.
  auto *header = loop->getHeader();
  auto *preheader = loop->getLoopPreheader();
  auto *latch = loop->getLoopLatch();
  assert(preheader && "Canonicalization should have given us one preheader");
  assert(latch && "Canonicalization should have given us one latch block");

  // Ok, at this point, the loop header has been canonicalized.  The next step
  // is to canonicalize the exits out of the loop, collect them.
  SmallVector<SILBasicBlock *, 8> exitingBlocks;
  loop->getExitingBlocks(exitingBlocks);

  assert(exitingBlocks.size() == 1 &&
         "Canonicalization should have given us one loop exit!");

  // Loop canonicalization also gives us the property that exits out of the
  // loop have critical edges split, and that any exit block jumps to a block
  // (outside the loop) that is *only* targeted by blocks inside the loop.

  // In many cases, we'll end up with the loop body in proper while form ready
  // for XLA: this is the case when there is exactly one loop exit, and it is
  // the header node itself.  In that case, we have a simpler transformation
  // to perform.
  if (exitingBlocks.size() == 1 && exitingBlocks.front() == header) {
    // The header node must have exactly two successors, one of which is the
    // exit block and one of which is the body of the loop.  We only transform
    // code produced by the partitioner, so we know it must be a condbr.
    auto headerBr = cast<CondBranchInst>(header->getTerminator());
    SILBasicBlock *exit = headerBr->getFalseBB();
    SILBasicBlock *body = headerBr->getTrueBB();
    if (loop->contains(exit))
      std::swap(exit, body);

    // The body of the loop will be the path from the bodyBB to the latchBB,
    // inclusive.  Because critical edges to exit blocks have already been
    // split, we will never see a loop header that is a self loop.  Transform
    // the acyclic region represented by the loop body into a SESE region.
    auto bodyRegion = processAcyclicRegion(body, latch);

    auto result =
        new WhileLoopSESERegion(preheader, header, exit, std::move(bodyRegion));
    loopPreheaders.insert({preheader, result});
    return;
  }

  llvm_unreachable("Canonicalization should have made loop header to be the "
                   "only loop exit!");
}

/// Transform the function into a properly nested series of
/// single-entry-single-exit regions.
std::unique_ptr<SESERegionTree> tf::canonicalizeCFGForXLA(SILFunction *F) {
#if 0
  llvm::errs() << "---- CANONICALIZE CFG INPUT ----------\n";
  F->dump();
  llvm::errs() << "---- END OF CANONICALIZE CFG INPUT ----------\n";
#endif
  SESERegionBuilder regionBuilder(F);

  // If the function contains loops, walk the loop tree in post-order,
  // collapsing the bodies of inner loops into SESE regions.  Once this is
  // done, the loop itself can be collapsed into a single node.
  regionBuilder.processLoops();

  // Finally, the top level of the function stitches together the loops with
  // acyclic control flow.  Transform that into properly nested SESE regions
  // as required.

  // Start by scanning the function to find the exit block.  We know that we
  // will have exactly one exit block due to how the partitioned region is
  // built.
  SILBasicBlock *exitBB = nullptr;
  for (auto &BB : *F) {
    if (isa<ReturnInst>(BB.getTerminator())) {
      assert(exitBB == nullptr && "Multiple exit blocks found?");
      exitBB = &BB;
    }
  }

  assert(exitBB && "Tensor program should have exactly on exit");
  return regionBuilder.processAcyclicRegion(&F->front(), exitBB);
}


//===----------------------------------------------------------------------===//
// TFXLACFGCanonicalizeTestPass
//===----------------------------------------------------------------------===//

namespace {
  /// This is a SIL pass that drives the CFG canonicalization process and prints
  /// out the generated region.  This is not used by the compiler proper, it
  /// just exists so we can drive it through sil-opt and write testcases.
  struct TFXLACFGCanonicalizeTestPass : public SILFunctionTransform {

    /// The entry point to the transformation.
    void run() override {
      auto fn = getFunction();
      contractUncondBranches(fn, /*DI*/nullptr, /*LI*/ nullptr);
      auto region = canonicalizeCFGForXLA(fn);
      llvm::outs() << "--- XLA CFG Canonicalize: " << fn->getName() << "\n";
      region->print(llvm::outs());
      llvm::outs() << "\n--- XLA CFG Canonicalize end\n";
      fn->print(llvm::outs());
      llvm::outs() << "----\n";
      llvm::outs().flush();
    }
  };
} // end anonymous namespace.

SILTransform *swift::createTFXLACFGCanonicalize() {
  return new TFXLACFGCanonicalizeTestPass();
}
