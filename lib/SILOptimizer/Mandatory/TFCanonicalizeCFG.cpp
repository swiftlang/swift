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
#include "swift/AST/TensorFlow.h"
#include "swift/Basic/TransformArrayRef.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILUndef.h"
#include "llvm/ADT/iterator_range.h"

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
    }

    void processLoop(SILLoop *loop);

  private:
    void ensureSingleExitFromLoops();
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

/// Create a TF handle of the appropriate integer type for the given bitwith
/// that is initialized to the given value.
static SILValue createTFIntegerConst(GraphFunctionDeviceInfo &deviceInfo,
                                     SILBuilder &builder, SILLocation location,
                                     unsigned bitwidth, intmax_t value) {
  ASTContext& context = builder.getASTContext();
  SILType intType =
      SILType::getBuiltinIntegerType(bitwidth, builder.getASTContext());
  // Literals take attributes specifying the dtype, value, and device.
  std::string opName("Const");
  SmallVector<GraphOperationAttribute, 3>  attributes;
  attributes.push_back({context.getIdentifier("dtype$dtype"),
                        SymbolicValue::getMetatype(intType.getASTType())});
  attributes.push_back({context.getIdentifier("value$tensor"),
                        SymbolicValue::getInteger(APInt(bitwidth, value),
                                                  context.getAllocator())});
  deviceInfo.handleDevicePlacement(
      opName,
      /*opDevice*/ getDeviceString(DeviceType::ALL),
      builder.getModule().getASTContext(), attributes);
  GraphOperationInst *constNode = builder.createGraphOperation(
      location, context.getIdentifier(opName), /*operands*/ {}, attributes,
      {convertElementTypeToTensorValueType(intType)});
  assert(constNode->getNumResults() == 1);
  return constNode->getResults()[0];
}

// A helper class to transform a loop to have a single exit from the header.
class SingleExitLoopTransformer {
public:
  SingleExitLoopTransformer(GraphFunctionDeviceInfo *deviceInfo,
                            SILLoopInfo *LI, DominanceInfo *DI, SILLoop *loop)
      : deviceInfo(deviceInfo), DI(DI), LI(LI), loop(loop),
        header(loop->getHeader()), preheader(loop->getLoopPreheader()),
        latch(loop->getLoopLatch()), currentFn(header->getParent()) {
    assert(preheader && "Canonicalization should have given us one preheader");
    assert(latch && "Canonicalization should have given us one latch block");
    initialize();
  }

  /// Transforms the loop to ensure it has a single exit from the header.
  /// Returns true if the CFG was changed.
  bool transform();

private:
  // Helper functions

  void initialize();

  /// Create a new header for the loop and return a pair consisting of
  /// the new block and the phi argument corresponding to the exitIndex.
  std::pair<SILBasicBlock *, SILValue> createNewHeader();

  /// Create a new latch block and clone all the arguments from new header.
  SILBasicBlock *createNewLatch(SILBasicBlock *newHeader);

  // Patch the edges that go to the header and exit blocks to the new header or
  // latch block as appropriate. Also, return the map consisting of indices
  // assigned to the exit blocks.
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
  SILLoopInfo *LI;
  SILLoop *loop;
  SILBasicBlock *header;
  SILBasicBlock *preheader;
  SILBasicBlock *latch;
  SILFunction *currentFn;
  /// exit blocks before the loop is transformed.
  SmallVector<SILBasicBlock*, 8> exitBlocks;
  /// The list of edges that need to rewired to the newHeader or
  // the new latchBlock as appropriate.
  SmallVector<std::pair<const SILBasicBlock *, const SILBasicBlock *>, 8>
      edgesToFix;
  /// Identify the set of values that escape the loop.
  llvm::SmallPtrSet<SILValue, 8> escapingValues;
};

void SingleExitLoopTransformer::initialize() {
  // Remember some information from the loop before it gets transformed.

  // Exit Blocks
  loop->getExitBlocks(exitBlocks);
  // All the exiting edges need to be rewired.
  loop->getExitEdges(edgesToFix);
  // All the edges to the header need to be rewired.
  for (SILBasicBlock *headerPred : header->getPredecessorBlocks()) {
    edgesToFix.emplace_back(headerPred, header);
  }

  for (const SILBasicBlock *bb : loop->getBlocks()) {
    // Save the values that are escaping this loop in escapingValues set.
    auto saveEscaping = [this](SILValue value) {
      for (const auto *use : value->getUses()) {
        const SILInstruction *useInst = use->getUser();
        if (!this->loop->contains(useInst->getParent())) {
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

llvm::DenseMap<SILBasicBlock *, intmax_t>
SingleExitLoopTransformer::patchEdges(SILBasicBlock *newHeader,
                                      SILBasicBlock *latchBlock) {

  llvm::DenseMap<SILBasicBlock *, intmax_t> exitIndices;

  unsigned oldHeaderNumArgs =
      newHeader->getNumArguments() -
      (escapingValues.size() + /* exitIndex, stayInLoop*/ 2);

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
    SILBasicBlock *newTgt = (src == preheader) ? newHeader : latchBlock;
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
      if (DI->properlyDominates(escapingValue, src->getTerminator())) {
        newArgs.push_back(escapingValue);
      } else if (src != preheader) {
        // newHeader arguments are available if src is not the preheader.
        newArgs.push_back(newHeader->getArgument(argIndex));
      } else {
        // State from within the loop is not available in the preheader.
        // Simply pass in an undef. This will never be accessed at runtime.
        newArgs.push_back(getUndef(escapingValue->getType()));
      }
      ++argIndex;
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

  // Create a new exit block.
  // FIXME: We can avoid creating an additional block and instead connect the
  // header directly to the demuxBlock created in the loop below. Alternatively,
  // we can also use contractUncondBranches in TFParititon.cpp to remove this
  // block later.
  SILBasicBlock *newExitBlock = createBlockOutsideLoop();

  SILBuilder builder(newExitBlock);
  ASTContext &context = builder.getASTContext();

  auto curBlockIter = exitBlocks.rbegin();
  SILBasicBlock *demuxBlock = *curBlockIter++;
  SILLocation headerLocation =
      getUserSourceLocation(header->getTerminator()->getDebugLocation());

  while (curBlockIter != exitBlocks.rend()) {
    SILBasicBlock *newBlock = createBlockOutsideLoop();
    SILBasicBlock *trueBlock = *curBlockIter++;
    builder.setInsertionPoint(newBlock);

    // Create a condition to compare exitIndex to a constant
    std::string equalOpName("Equal");
    equalOpName +=
        GraphOperationInfo::getInputMarker(GraphOperationInfo::IM_Normal);
    equalOpName +=
        GraphOperationInfo::getInputMarker(GraphOperationInfo::IM_Normal);
    SmallVector<GraphOperationAttribute, 2> attributes;
    deviceInfo->handleDevicePlacement(
        "Equal", /*opDevice*/ getDeviceString(DeviceType::ALL),
        builder.getModule().getASTContext(), attributes);
    GraphOperationInst *condTensorInst = builder.createGraphOperation(
        headerLocation, context.getIdentifier(equalOpName),
        /*operands*/
        {exitIndexArg,
         createTFIntegerConst(*deviceInfo, builder, headerLocation,
                              /*bitwidth*/ 32, exitIndices.lookup(trueBlock))},
        attributes,
        {convertElementTypeToTensorValueType(
            SILType::getBuiltinIntegerType(1, context))});
    assert(condTensorInst->getNumResults() == 1);
    SILValue condTensor = condTensorInst->getResults()[0];
    GraphOperationInst *condValue = builder.createGraphOperation(
        headerLocation, context.getIdentifier("tf_tensor_to_i1"),
        /*operands*/ {condTensor}, /*attributes */ {},
        {SILType::getBuiltinIntegerType(1, context)});
    assert(condValue->getNumResults() == 1);
    builder.createCondBranch(headerLocation, condValue->getResults()[0],
                             trueBlock, demuxBlock);
    demuxBlock = newBlock;
  }
  builder.setInsertionPoint(newExitBlock);
  builder.createBranch(headerLocation, demuxBlock);
  return newExitBlock;
}

bool SingleExitLoopTransformer::transform() {
  // Return if the loop is already in the required form.
  if (loop->getExitBlock() && loop->getExitingBlock() &&
      loop->getExitingBlock() == loop->getHeader()) {
    return false;
  }
  SILBuilder builder(header);
  ASTContext &context = builder.getASTContext();

  // Create a new header and get the exitIndex argument.
  std::pair<SILBasicBlock *, SILValue> headerResult = createNewHeader();
  SILBasicBlock *newHeader = headerResult.first;
  SILValue exitIndexArg = headerResult.second;

  // Create a new latch block.
  SILBasicBlock *latchBlock = createNewLatch(newHeader);

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
    GraphOperationInst *loopExitCond = builder.createGraphOperation(
        headerLocation, context.getIdentifier("tf_tensor_to_i1"),
        /*operands*/ {newHeader->getArguments().back()}, /*attributes*/ {},
          {SILType::getBuiltinIntegerType(1, context)});
    assert(loopExitCond->getNumResults() == 1);
    builder.createCondBranch(headerLocation, loopExitCond->getResults()[0],
                             header, newExitBlock);
  }

  // Update the loop header to newHeader.
  loop->moveToHeader(newHeader);
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
    SingleExitLoopTransformer transformer(&deviceInfo, &LI, &DI, loop);
    changed |= transformer.transform();
  }
  if (changed) {
    splitAllCondBrCriticalEdgesWithNonTrivialArgs(*F, nullptr, &LI);

    DI.recalculate(*F);
    PDI.recalculate(*F);
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
