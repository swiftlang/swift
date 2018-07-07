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
#include "TFUtilities.h"
#include "llvm/ADT/iterator_range.h"
#include "swift/AST/TensorFlow.h"
#include "swift/Basic/TransformArrayRef.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILUndef.h"

using namespace swift;
using namespace tf;

static llvm::cl::opt<bool> TFEnsureSingleLoopExit(
    "tf-ensure-single-loop-exit", llvm::cl::init(false),
    llvm::cl::desc("Transform loops to have a single exit from header."));

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
    SILFunction* F;

    /// processLoops fills in this mapping: it is keyed by the preheader block
    /// of each loop, and points to the region produced for it.
    llvm::DenseMap<SILBasicBlock*, WhileLoopSESERegion*> loopPreheaders;
  public:
    SESERegionBuilder(SILFunction *F) : DI(F), PDI(F), LI(F, &DI), F(F) {}

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
      // We need to ensure more invariants as SIL loop canonicalization
      // transformations don't do everything that we need.
      if (TFEnsureSingleLoopExit) {
        ensureSingleExitFromLoops();
      }

      for (auto *loop : LI)
        processLoop(loop);
    }

    void processLoop(SILLoop *loop);

  private:
    bool ensureSingleExitFromLoops();
    bool ensureSingleExitFromLoop(SILLoop* loop);
    SILValue getUndef(SILType type);
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
      processAcyclicRegionExcludingEnd(condBr->getTrueBB(), postidom);
    auto falseRegion =
      processAcyclicRegionExcludingEnd(condBr->getFalseBB(), postidom);

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

/// For loops in the SIL Function with multiple exit block successors,
/// transforms the loop by introducing a common exit switch block that demuxes
/// to the appropriate exit block. Updates the Dominanceinfo, PostDominanceinfo,
/// and LoopInfo as well.
bool SESERegionBuilder::ensureSingleExitFromLoops() {
  // TODO: update changed appropriately.
  // Visit the loop nest hierarchy bottom up.
  bool changed = false;
  llvm::SmallVector<std::pair<SILLoop *, bool>, 16> workList;
  for (auto *L : LI.getTopLevelLoops())
    workList.push_back({L, L->empty()});
  while (workList.size() != 0) {
    SILLoop *loop;
    bool visited;
    std::tie(loop, visited) = workList.pop_back_val();

    if (!visited) {
      workList.push_back({loop, true});
      for (auto *Subloop : loop->getSubLoopRange()) {
        workList.push_back({Subloop, Subloop->empty()});
      }
      continue;
    }
    changed |= ensureSingleExitFromLoop(loop);
  }
  if (changed) {
    splitAllCondBrCriticalEdgesWithNonTrivialArgs(*F, nullptr, &LI);

    DI.recalculate(*F);
    PDI.recalculate(*F);
  }
  return changed;
}

namespace {

/// Appends the given arguments to the given edge. Deletes the old TermInst
/// and returns a new TermInst with the appropriate number of args.
TermInst* appendArguments(TermInst *termInst, SILBasicBlock *target,
                     ArrayRef<SILValue> newArgs) {

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
  } else {
    // At the moment we can only add arguments to br and cond_br.
    llvm_unreachable("Can't add argument to terminator");
  }
  // Remove the old terminator instruction.
  termInst->dropAllReferences();
  termInst->eraseFromParent();
  return newTermInst;
}

/// Creates a TF handle of appropriate type that is initialized to the given
/// element value.
SILValue createTFHandleFromElement(SILBuilder &builder, SILLocation location,
                                   SILValue element) {
  assert(builder.hasValidInsertionPoint());
  ASTContext& context = builder.getASTContext();
  SILType elementType = element->getType();
  // Literals take attributes specifying the dtype, value, and device.
  std::string opName("__tfop_Const,dtype$dtype,value$tensor,__device");
  SmallVector<SILValue, 8> operands;
  operands.push_back(builder.createIntegerLiteral(
      location, SILType::getBuiltinIntegerType(32, context),
      convertSwiftTypeToTF(elementType.getASTType())));
  operands.push_back(element);
  operands.push_back(builder.createStringLiteral(
      location, StringRef(ALL_DEVICES), StringLiteralInst::Encoding::UTF8));
  return builder.createBuiltin(location, context.getIdentifier(opName),
                               convertElementTypeToTensorValueType(elementType),
                               /*substitutionlist*/ {}, operands);
}


/// Creates a TF handle of the appropriate integer type for the given bitwith
/// that is initialized to the given value.
SILValue createTFIntegerConst(SILBuilder &builder, SILLocation location,
                              unsigned bitwidth, intmax_t value) {
  SILType intType =
      SILType::getBuiltinIntegerType(bitwidth, builder.getASTContext());
  SILValue ourCst = builder.createIntegerLiteral(location, intType, value);
  return createTFHandleFromElement(builder, location, ourCst);
}


}  // namespace


SILValue SESERegionBuilder::getUndef(SILType type) {
  return SILUndef::get(type, F->getModule());
}


bool SESERegionBuilder::ensureSingleExitFromLoop(SILLoop* loop) {
  // Return if the loop is already in the required form.
  if (loop->getExitBlock() && loop->getExitingBlock() &&
      loop->getExitingBlock() == loop->getHeader()) {
    return false;
  }

  SmallVector<SILBasicBlock*, 8> exitBlocks;
  loop->getExitBlocks(exitBlocks);

  SILBasicBlock *header = loop->getHeader();
  SILBasicBlock *preheader = loop->getLoopPreheader();
  SILBasicBlock *latch = loop->getLoopLatch();
  assert(preheader && "Canonicalization should have given us one preheader");
  assert(latch && "Canonicalization should have given us one latch block");
  SILFunction *currentFn = header->getParent();

  // Identify the set of values that escape the loop.
  llvm::SmallPtrSet<SILValue, 8> escapingValues;
  for (const SILBasicBlock* bb : loop->getBlocks()) {
    // Save the values that are escaping this loop in escapingValues set.
    auto save_escaping = [this, &loop, &escapingValues](const SILValue &value) {
      for (const auto *use : value->getUses()) {
        const SILInstruction *use_inst = use->getUser();
        if (LI.getLoopFor(use_inst->getParent()) != loop) {
          escapingValues.insert(value);
          break;
        }
      }
    };
    if (bb != header) {
      llvm::for_each(bb->getArguments(), save_escaping);
    }
    for (const SILInstruction &inst : *bb) {
      llvm::for_each(inst.getResults(), save_escaping);
    }
  }
  llvm::outs() << "The following are live outside the loop:\n";
  for (const auto &escapingValue : escapingValues) {
    escapingValue->dump();
  }

  SmallVector<std::pair<const SILBasicBlock *, const SILBasicBlock *>, 8>
      edgesToFix;
  // Exiting edges
  loop->getExitEdges(edgesToFix);
  // Edges to header
  for (SILBasicBlock *headerPred : header->getPredecessorBlocks()) {
    edgesToFix.emplace_back(headerPred, header);
  }

  SILBuilder builder(header);
  ASTContext& context = builder.getASTContext();

  // Create a new header for the loop.
  //
  SILBasicBlock* newHeader = currentFn->createBasicBlock();
  loop->addBasicBlockToLoop(newHeader, LI.getBase()); // Should be done first.
  // Clone arguments and change all uses to the new header's arguments.
  newHeader->cloneArgumentList(header);
  SmallVector<SILValue, 8> headerArgs;
  for (auto i : indices(header->getArguments())) {
    header->getArgument(i)->replaceAllUsesWith(newHeader->getArgument(i));
    headerArgs.push_back(newHeader->getArgument(i));
  }
  size_t oldHeaderNumArgs = header->getNumArguments();
  header->dropAllArguments();
  // Add phi arguments in the new header corresponding to the escaping values.
  for (const auto &escapingValue : escapingValues) {
    SILValue newValue = newHeader->createPHIArgument(
        escapingValue->getType(), escapingValue.getOwnershipKind());
    escapingValue->replaceAllUsesWith(newValue);
  }
  // An integer to identify the exit edge.
  SILValue exitIndexArg = newHeader->createPHIArgument(
      convertElementTypeToTensorValueType(
          SILType::getBuiltinIntegerType(32, context)),
      ValueOwnershipKind::Owned);
  // A boolean corresponding to the stayInLoop flag.
  newHeader->createPHIArgument(convertElementTypeToTensorValueType(
                                   SILType::getBuiltinIntegerType(1, context)),
                               ValueOwnershipKind::Trivial);

  // Create a new latch block.
  SILBasicBlock* latchBlock = currentFn->createBasicBlock();
  latchBlock->cloneArgumentList(newHeader);
  {
    builder.setInsertionPoint(latchBlock);
    SmallVector<SILValue, 8> latchArgs;
    for (const SILArgument *latchArg : latchBlock->getArguments()) {
      latchArgs.push_back(latchArg);
    }
    builder.createBranch(
        getUserSourceLocation(header->getTerminator()->getDebugLocation()),
        newHeader, latchArgs);
  }

  llvm::DenseMap<SILBasicBlock*, intmax_t> exitIndices;
  // As a simplification, simply 0 as exit index for edges to the header
  // even though header is not an exit block.
  exitIndices.insert({header, 0});
  for (const auto &edge : edgesToFix) {
    SILBasicBlock *src = const_cast<SILBasicBlock *>(edge.first);
    SILBasicBlock *tgt = const_cast<SILBasicBlock *>(edge.second);
    SILBasicBlock *newTgt = loop->contains(src) ? latchBlock : newHeader;
    bool stayInLoop = loop->contains(tgt);
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
    for (const SILValue &escapingValue : escapingValues) {
      if (DI.properlyDominates(escapingValue, src->getTerminator())) {
        newArgs.push_back(escapingValue);
      } else if (src != preheader) {
        // if src is not the preheader than new header arguments are available.
        newArgs.push_back(newHeader->getArgument(argIndex));
      } else {
        newArgs.push_back(getUndef(escapingValue->getType() ));
      }
      ++argIndex;
    }
    // `exitIndex` to identify the block to which we exit from the loop.
    // (insert a new value or get the old key value pair.)
    auto kvPair = *exitIndices.try_emplace(tgt, exitIndices.size()).first;
    newArgs.push_back(createTFIntegerConst(builder, location,
                                           /*bitwidth*/ 32,
                                           /*exitIndex*/ kvPair.second));
    // `stayInLoop` flag
    newArgs.push_back(createTFIntegerConst(builder, location,
                                           /*bitwidth*/ 1, stayInLoop));
    appendArguments(src->getTerminator(), newTgt, newArgs);
  }

  // Create a new exit block.
  SILBasicBlock* newExitBlock = currentFn->createBasicBlock();

  // Build a demuxing if..then..else block that can be used as a single exit
  // block. This will not have any effect if there is a single exit block
  // already.
  auto curBlockIter = exitBlocks.rbegin();
  SILBasicBlock *demuxBlock = *curBlockIter++;
  SILLocation headerLocation =
      getUserSourceLocation(header->getTerminator()->getDebugLocation());

  while (curBlockIter != exitBlocks.rend()) {
    SILBasicBlock *newBlock = currentFn->createBasicBlock();
    SILBasicBlock *trueBlock = *curBlockIter++;
    builder.setInsertionPoint(newBlock);
    SILValue condTensor = builder.createBuiltin(
        headerLocation, context.getIdentifier("__tfop_Equal,$in,$in,__device"),
        convertElementTypeToTensorValueType(
            SILType::getBuiltinIntegerType(1, context)),
        /* SubstitutionMap*/ {},
        {exitIndexArg,
         createTFIntegerConst(builder, headerLocation, /*bitwidth*/ 32,
                              exitIndices.lookup(trueBlock)),
         builder.createStringLiteral(headerLocation, StringRef(ALL_DEVICES),
                                     StringLiteralInst::Encoding::UTF8)});
    SILValue condValue = builder.createBuiltin(
        headerLocation, context.getIdentifier("tf_tensor_to_i1"),
        SILType::getBuiltinIntegerType(1, context), /*SubstitutionMap*/ {},
        {condTensor});
    builder.createCondBranch(headerLocation, condValue, trueBlock, demuxBlock);
    demuxBlock = newBlock;
  }
  builder.setInsertionPoint(newExitBlock);
  builder.createBranch(headerLocation, demuxBlock);

  // Now connect the newheader to the header and exit block.
  builder.setInsertionPoint(newHeader);
  {
    SILValue loopExitCond = builder.createBuiltin(
        headerLocation, context.getIdentifier("tf_tensor_to_i1"),
        SILType::getBuiltinIntegerType(1, context), /*SubstitutionMap*/ {},
        {newHeader->getArguments().back()});
    builder.createCondBranch(headerLocation, loopExitCond, header, newExitBlock);
  }
  loop->moveToHeader(newHeader);
  loop->addBasicBlockToLoop(latchBlock, LI.getBase());

  return true;
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

  if (exitingBlocks.size() != 1) {
    llvm_unreachable("SESE FIXME: Loops with multiple exits not handled yet!");
  }

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

  llvm_unreachable("SESE FIXME: Imperfect loop exits not handled yet!");
  // splitCriticalEdge

  // FIXME: Need to handle "break" edges that exit the loop, preventing the
  // body from being an SESE region.
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
