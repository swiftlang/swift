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

#include "TFCanonicalizeCFG.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/LoopInfo.h"
using namespace swift;
using namespace tf;

//===----------------------------------------------------------------------===//
// SESERegionTree Implementation
//===----------------------------------------------------------------------===//

SESERegionTree::~SESERegionTree() {}

void SESERegionTree::dump() const {
  print(llvm::errs());
}

void SESERegionTree::print(llvm::raw_ostream &OS, unsigned indent) const {
  switch (getKind()) {
  case SingleBlock: return cast<SingleBlockSESERegion>(this)->print(OS, indent);
  case Sequence:    return cast<SequenceSESERegion>(this)->print(OS, indent);
  case WhileLoop:   return cast<WhileLoopSESERegion>(this)->print(OS, indent);
  case If:          return cast<IfSESERegion>(this)->print(OS, indent);
  }
};

void SingleBlockSESERegion::
print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "Block ";
  BB->printAsOperand(OS);
  OS << "\n";
}

void SequenceSESERegion::print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "SESESequence {\n";
  for (auto &n : getNodes())
    n->print(OS, indent+2);
  OS.indent(indent) << "}\n";
}

void WhileLoopSESERegion::print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "SESEWhile Header: ";
  header->printAsOperand(OS);
  OS << "   exit: ";
  exit->printAsOperand(OS);
  OS << "\n";
  body->print(OS, indent+2);
}

void IfSESERegion::print(llvm::raw_ostream &OS, unsigned indent) const {
  OS.indent(indent) << "SESEIf XXX\n";
}

//===----------------------------------------------------------------------===//
// CFG Canonicalization Implementation
//===----------------------------------------------------------------------===//

namespace {
  class SESERegionBuilder {
    DominanceInfo DT;
    SILLoopInfo LI;
    llvm::DenseMap<SILBasicBlock*, WhileLoopSESERegion*> loopHeaders;
  public:
    SESERegionBuilder(SILFunction *F) : DT(F), LI(F, &DT) {}

    std::unique_ptr<SESERegionTree> processAcyclicRegion(SILBasicBlock *startBB,
                                                         SILBasicBlock *endBB);

    /// Process all of the top-level loops in the function in post-order.
    void processLoops() {
      // Apply the standard SIL loop canonicalization transformations.  This
      // automatically gives us the following invariants: loops are guaranteed
      // to have a single preheader, a single backedge block, and exit??
      canonicalizeAllLoops(&DT, &LI);

      for (auto *loop : LI)
        processLoop(loop);
    }

    void processLoop(SILLoop *loop);
  private:
    std::unique_ptr<SESERegionTree>
    sequence(SESERegionTree *first, std::unique_ptr<SESERegionTree> &&second);
  };
} // end anonymous namespace

/// Given two nodes where 'first' comes before 'second', return a SESE region
/// that is a sequencing of one before the other.  This flattens existing
/// sequences where possible.
std::unique_ptr<SESERegionTree>
SESERegionBuilder::sequence(SESERegionTree *first,
                            std::unique_ptr<SESERegionTree> &&second) {
  SmallVector<std::unique_ptr<SESERegionTree>, 8> results;

  if (auto *firstSeq = dyn_cast<SequenceSESERegion>(first)) {
    for (auto &elt : firstSeq->takeNodes())
      results.push_back(std::move(elt));
  } else {
    results.push_back(std::unique_ptr<SESERegionTree>(first));
  }
  if (auto *secondSeq = dyn_cast<SequenceSESERegion>(second.get())) {
    for (auto &elt : secondSeq->takeNodes())
      results.push_back(std::move(elt));
  } else {
    results.push_back(std::move(second));
  }

  auto resultNode = new SequenceSESERegion(results);
  return std::unique_ptr<SESERegionTree>(resultNode);
}


/// Transform the specified acyclic region (possibly with nodes collapsed) from
/// startBB to endBB inclusive into properly nested SESE regions and return
/// them.
std::unique_ptr<SESERegionTree>
SESERegionBuilder::processAcyclicRegion(SILBasicBlock *startBB,
                                        SILBasicBlock *endBB) {
  // If the start and end block are the same, then we've bottomed out into a
  // single basic block region.
  if (startBB == endBB) {
    auto result = new SingleBlockSESERegion(startBB);
    return std::unique_ptr<SESERegionTree>(result);
  }

  // If this is the start of a loop, it will already have been processed and
  // collapsed into a single node.
  auto loopIt = loopHeaders.find(startBB);
  if (loopIt != loopHeaders.end()) {
    auto whileNode = loopIt->second;
    loopHeaders.erase(loopIt);

    return sequence(whileNode,
                    processAcyclicRegion(whileNode->getExit(), endBB));
  }


  // Otherwise, this is some multiblock region.  No problem, there are only two
  // cases: startBB could end with an unconditional branch or a conditional one.
  // Handle the simple case first.
  if (auto *branch = dyn_cast<BranchInst>(startBB->getTerminator())) {
    return sequence(new SingleBlockSESERegion(startBB),
                    processAcyclicRegion(branch->getDestBB(), endBB));
  }

  assert(0 && "SESE FIXME: Handle conditional branches!");
}

/// Process the specified loop, collapsing it into a SESE region node.  This
/// forms a WhileLoopSESERegion node and puts it into the loopHeader data
/// structure, allowing the outer level's acyclic region handling to pick it up.
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

  // Loop canonicalization also gives us the property that exits out of the loop
  // have critical edges split, and that any exit block jumps to a block (outside
  // the loop) that is *only* targeted by blocks inside the loop.

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

    // Ok, on a normal day, the body of the loop will be the path from the
    // bodyBB to the latchBB, inclusive.  It is possible that the loop header
    // is itself a cycle, but that still satisfies the basic form.  Transform
    // the acyclic region represented by the loop body into a SESE region.
    auto bodyRegion = processAcyclicRegion(body, latch);

    auto result = new WhileLoopSESERegion(header, exit, std::move(bodyRegion));
    loopHeaders.insert({header, result});
    return;
  }

  assert(0 && "SESE FIXME: Imperfect loop exits not handled yet!");
  // splitCriticalEdge

  /// Canonicalize loops as we go: single back edge, then canonicalize the body.
  /// Keep the dominator tree up to date.
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
      auto region = canonicalizeCFGForXLA(fn);

      llvm::outs() << "--- XLA CFG Canonicalize: " << fn->getName() << "\n";
      region->print(llvm::outs());
      llvm::outs() << "--- XLA CFG Canonicalize end\n";
      fn->print(llvm::outs());
    }
  };
} // end anonymous namespace.

SILTransform *swift::createTFXLACFGCanonicalize() {
  return new TFXLACFGCanonicalizeTestPass();
}



