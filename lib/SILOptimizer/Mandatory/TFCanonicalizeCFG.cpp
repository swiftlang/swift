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
  llvm::errs() << "\n";
}

void SESERegionTree::print(llvm::raw_ostream &OS, unsigned indent) const {
  switch (getKind()) {
  case SingleBlock: return cast<SingleBlockSESERegion>(this)->print(OS, indent);
  case Sequence:    return cast<SequenceSESERegion>(this)->print(OS, indent);
  case WhileLoop:   return cast<WhileLoopSESERegion>(this)->print(OS, indent);
  case Conditional: return cast<ConditionalSESERegion>(this)->print(OS, indent);
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

namespace {
  class SESERegionBuilder {
    DominanceInfo DI;
    PostDominanceInfo PDI;
    SILLoopInfo LI;

    /// processLoops fills in this mapping: it is keyed by the preheader block
    /// of each loop, and points to the region produced for it.
    llvm::DenseMap<SILBasicBlock*, WhileLoopSESERegion*> loopPreheaders;
  public:
    SESERegionBuilder(SILFunction *F) : DI(F), PDI(F), LI(F, &DI) {}

    std::unique_ptr<SESERegionTree>
    processAcyclicRegion(SILBasicBlock *startBB, SILBasicBlock *endBB);

    std::unique_ptr<SESERegionTree>
    processAcyclicRegionExcludingEnd(SILBasicBlock *startBB,
                                     SILBasicBlock *endBB);


    /// Process all of the top-level loops in the function in post-order.
    void processLoops() {
      // Apply the standard SIL loop canonicalization transformations.  This
      // automatically gives us the following invariants: loops are guaranteed
      // to have a single preheader, a single backedge block, and exit??
      canonicalizeAllLoops(&DI, &LI);

      for (auto *loop : LI)
        processLoop(loop);
    }

    void processLoop(SILLoop *loop);
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

  // Iteratively work our way up the post-dominator tree (moving startBB until
  // we reach the endBB), producing a sequence of loops, diamonds, and single
  // block nodes as we go.
  while (startBB != endBB) {
    // If this ends with a loop, it will already have been processed and
    // collapsed into a single node.  Just use it.
    auto loopIt = loopPreheaders.find(startBB);
    if (loopIt != loopPreheaders.end()) {
      auto whileNode = loopIt->second;
      loopPreheaders.erase(loopIt);

      results.push_back(std::unique_ptr<SESERegionTree>(whileNode));
      startBB = whileNode->getExit();
      continue;
    }

    // If startBB ends with an unconditional branch, then just add it to the
    // sequence and keep going.  The destination of the branch may have multiple
    // successors in the case where the destination is endBB.  The successors
    // could be coming from a parent conditional region.
    if (auto *branch = dyn_cast<BranchInst>(startBB->getTerminator())) {
      auto startRegion = new SingleBlockSESERegion(startBB);
      results.push_back(std::unique_ptr<SESERegionTree>(startRegion));
      startBB = branch->getDestBB();
      continue;
    }

    // Otherwise, we know that startBB ends with a conditional branch.
    auto *condBr = cast<CondBranchInst>(startBB->getTerminator());

    // Get the immediate postdominator of endBB, which defines a SESE
    // subregion postdominated by startBB and postdominating endBB.  Note that
    // the postidom may in fact be endBB.
    auto postidom = PDI[startBB]->getIDom()->getBlock();

    // Analyze the successors of the branch: each of them is post dominated by
    // endBB (and any of the successors may be exactly endBB).
    auto trueRegion =
      processAcyclicRegionExcludingEnd(condBr->getTrueBB(), endBB);
    auto falseRegion =
      processAcyclicRegionExcludingEnd(condBr->getFalseBB(), endBB);

    // Finally, form our conditional region.
    auto condRegion = new ConditionalSESERegion(startBB, std::move(trueRegion),
                                                std::move(falseRegion));
    results.push_back(std::unique_ptr<SESERegionTree>(condRegion));
    startBB = postidom;
  }

  switch (results.size()) {
  case 0: return std::unique_ptr<SESERegionTree>();
  case 1: return std::move(results[0]);
  default:
    auto result = new SequenceSESERegion(results);
    return std::unique_ptr<SESERegionTree>(result);
  }
}

/// Process the specified loop, collapsing it into a SESE region node.  This
/// forms a WhileLoopSESERegion node and puts it into the loopPreheaders data
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

  if (exitingBlocks.size() != 1) {
    llvm_unreachable("SESE FIXME: Loops with multiple exits not handled yet!");
  }

  // Loop canonicalization also gives us the property that exits out of the loop
  // have critical edges split, and that any exit block jumps to a block
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

    auto result = new WhileLoopSESERegion(preheader, header, exit,
                                          std::move(bodyRegion));
    loopPreheaders.insert({preheader, result});
    return;
  }

  llvm_unreachable("SESE FIXME: Imperfect loop exits not handled yet!");
  // splitCriticalEdge

  // FIXME: Need to handle "break" edges that exit the loop, preventing the body
  // from being an SESE region.
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



