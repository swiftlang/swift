#include "../../Sema/TypeCheckConcurrency.h"
#include "../../Sema/TypeChecker.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "swift/SIL/BasicBlockData.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/PartitionUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

static const bool DEBUG = true;
static const char *SEP_STR = "╾──────────────────────────────╼\n";

static bool SILApplyCrossesIsolation(const SILInstruction *inst) {
  ApplyExpr *apply = inst->getLoc().getAsASTNode<ApplyExpr>();
  // if the instruction doesn't correspond to an ApplyExpr, then it can't
  // cross an isolation domain
  if (!apply)
    return false;
  return apply->getIsolationCrossing().has_value();
}

class PartitionOpTranslator {
  SILFunction *function;
  ProtocolDecl *sendableProtocol;

  // nodeIDMap stores unique IDs for all SILNodes corresponding to
  // non-Sendable values. Implicit conversion from SILValue used pervasively
  llvm::DenseMap<const SILNode *, unsigned> nodeIDMap;
  unsigned nextNodeID = 0;

  bool nodeHasID(SILValue value) {
    assert(isNonSendable(value) &&
           "only non-Sendable values should be entered in the map");
    return nodeIDMap.count(value);
  }

  unsigned lookupNodeID(SILValue value) {
    assert(isNonSendable(value) &&
           "only non-Sendable values should be entered in the map");
    if (nodeIDMap.count(value)) {
      return nodeIDMap[value];
    }
    nodeIDMap[value] = nextNodeID;
    return nextNodeID++;
  }

  bool isNonSendableType(SILType type) {
    if (type.getASTType()->getKind() == TypeKind::BuiltinNativeObject) {
      // these are very unsafe... definitely not Sendable
      return true;
    }
    // consider caching this if it's a bottleneck
    return TypeChecker::conformsToProtocol(
        type.getASTType(), sendableProtocol, function->getParentModule())
        .hasMissingConformance(function->getParentModule());
  }

  bool isNonSendable(SILValue value) {
    SILInstruction *defInst = value.getDefiningInstruction();
    if (defInst && isa<ClassMethodInst, FunctionRefInst>(defInst)) {
      // though these values are technically non-Sendable, we can safely
      // and consistently treat them as Sendable
      return false;
    }

    //consider caching this if it's a bottleneck
    return isNonSendableType(value->getType());
  }

  // used to statefully track the instruction currently being translated,
  // for insertion into generated PartitionOps
  SILInstruction *currentInstruction;

  PartitionOp AssignFresh(SILValue value) {
    return PartitionOp::AssignFresh(lookupNodeID(value),
        currentInstruction);
  }

  PartitionOp Assign(SILValue tgt, SILValue src) {
    assert(nodeHasID(src) &&
           "source value of assignment should already have been encountered");
    return PartitionOp::Assign(lookupNodeID(tgt), lookupNodeID(src),
        currentInstruction);
  }

  PartitionOp Consume(SILValue value) {
    assert(nodeHasID(value) &&
           "consumed value should already have been encountered");
    return PartitionOp::Consume(lookupNodeID(value),
                                currentInstruction);
  }

  PartitionOp Merge(SILValue fst, SILValue snd) {
    assert(nodeHasID(fst) && nodeHasID(snd) &&
           "merged values should already have been encountered");
    return PartitionOp::Merge(lookupNodeID(fst), lookupNodeID(snd),
                              currentInstruction);
  }

  PartitionOp Require(SILValue value) {
    assert(nodeHasID(value) &&
           "required value should already have been encountered");
    return PartitionOp::Require(lookupNodeID(value),
                                currentInstruction);
  }

public:
  PartitionOpTranslator(SILFunction *function) :
      function(function),
      sendableProtocol(function->getASTContext()
                           .getProtocol(KnownProtocolKind::Sendable)) {
    assert(sendableProtocol && "PartitionOpTranslators should only be created "
                               "in contexts in which the availability of the "
                               "Sendable protocol has already been checked.");
  }

  // Create a partition that places all arguments from this function,
  // including self if available, into the same region, ensuring those
  // arguments get IDs in doing so. This Partition will be used as the
  // entry point for the full partition analysis.
  Partition getEntryPartition() {
    std::vector<unsigned> entryIDs;
    for (SILArgument *arg : function->getArguments()) {
      if (isNonSendableType(arg->getType())) {
        entryIDs.push_back(lookupNodeID(arg));
      }
    }
    if (function->hasSelfParam() &&
        isNonSendableType(function->getSelfArgument()->getType())) {
      entryIDs.push_back(lookupNodeID(function->getSelfArgument()));
    }
    return Partition::singleRegion(entryIDs);
  }

  std::vector<PartitionOp> translateSILApply(const SILInstruction *applyInst) {
    // accumulates the non-Sendable operands to this apply, include self and
    // the callee
    std::vector<SILValue> nonSendableOperands;

    for (SILValue operand : applyInst->getOperandValues())
      if (isNonSendable(operand))
        nonSendableOperands.push_back(operand);

    // check whether the result is non-Sendable
    bool nonSendableResult = isNonSendable(applyInst->getResult(0));

    std::vector<PartitionOp> translated;

    if (SILApplyCrossesIsolation(applyInst)) {
      // for calls that cross isolation domains, consume all operands
      for (SILValue operand : nonSendableOperands)
        translated.push_back(Consume(operand));

      if (nonSendableResult) {
        // returning non-Sendable values from a cross isolation call will always
        // be an error, but doesn't need to be diagnosed here, so let's pretend
        // it gets a fresh region
        translated.push_back(AssignFresh(applyInst->getResult(0)));
      }
      return translated;
    }

    // for calls that do not cross isolation domains, merge all non-Sendable
    // operands and assign the result to the region of the operands

    if (nonSendableOperands.empty()) {
      // if no operands, a non-Sendable result gets a fresh region
      if (nonSendableResult) {
        translated.push_back(AssignFresh(applyInst->getResult(0)));
      }
      return translated;
    }

    if (nonSendableOperands.size() == 1) {
      // only one operand, so no merges required; just a `Require`
      translated.push_back(Require(nonSendableOperands.front()));
    } else {
      // merge all operands
      for (unsigned i = 1; i < nonSendableOperands.size(); i++) {
        translated.push_back(Merge(nonSendableOperands.at(i-1),
                                   nonSendableOperands.at(i)));
      }
    }

    // if the result is non-Sendable, assign it to the region of the operands
    if (nonSendableResult) {
      translated.push_back(
          Assign(applyInst->getResult(0), nonSendableOperands.front()));
    }

    return translated;
  }

  std::vector<PartitionOp> translateSILAssign(SILValue tgt, SILValue src) {
    // no work to be done if assignment is to a Sendable target
    if (!isNonSendable(tgt))
      return {};

    if (isNonSendable(src)) {
      return {Assign(tgt, src)};
    }

    // a non-Sendable value is extracted from a Sendable value,
    // seems to only occur when performing unchecked casts like
    // `unchecked_ref_cast`
    return {AssignFresh(tgt)};
  }

  // If the passed SILValue is NonSendable, then create a fresh region for it,
  // otherwise do nothing.
  std::vector<PartitionOp> translateSILAssignFresh(SILValue fresh) {
    if (isNonSendable(fresh)) {
      return { AssignFresh(fresh)};
    }
    return {};
  }

  // Some SILInstructions contribute to the partition of non-Sendable values
  // being analyzed. translateSILInstruction translate a SILInstruction
  // to its effect on the non-Sendable partition, if it has one
  std::vector<PartitionOp> translateSILInstruction(SILInstruction *instruction) {
    currentInstruction = instruction;

    if (isa<AllocRefInst,
            AllocStackInst,
            LiteralInst>(instruction)) {
      return translateSILAssignFresh(instruction->getResult(0));
    }

    // the following instructions are treated as assignments
    if (isa<AddressToPointerInst,
            BeginAccessInst,
            BeginBorrowInst,
            CopyValueInst,
            ConvertEscapeToNoEscapeInst,
            IndexAddrInst,
            LoadInst,
            LoadBorrowInst,
            LoadWeakInst,
            PointerToAddressInst,
            RefElementAddrInst,
            StrongCopyUnownedValueInst,
            TailAddrInst,
            UncheckedAddrCastInst,
            UncheckedRefCastInst>(instruction)) {
      return translateSILAssign(
          instruction->getResult(0),
          instruction->getOperand(0));
    }

    if (isa<CopyAddrInst,
        ExplicitCopyAddrInst>(instruction)) {
      return translateSILAssign(
          instruction->getOperand(1),
          instruction->getOperand(0));
    }

    if (isa<StoreInst,
        StoreBorrowInst,
        StoreWeakInst>(instruction)) {
      return {Merge(instruction->getOperand(0), instruction->getOperand(1))};
    }

    if (isa<ApplyInst, PartialApplyInst>(instruction)) {
      return translateSILApply(instruction);
    }

    if (auto destructTupleInst = dyn_cast<DestructureTupleInst>(instruction)) {
      std::vector<PartitionOp> translated;
      for (SILValue result : destructTupleInst->getResults())
        for (auto op : translateSILAssign(result, instruction->getOperand(0)))
          translated.push_back(op);
      return translated;
    }

    return {};
  }

  // translateSILBasicBlock reduces a SIL basic block to the vector of
  // transformations to the non-Sendable partition that it induces.
  // it accomplished this by sequentially calling translateSILInstruction
  std::vector<PartitionOp> translateSILBasicBlock(SILBasicBlock *basicBlock) {
    if (DEBUG) {
      llvm::dbgs() << SEP_STR
                   << "Compiling basic block for function "
                   << basicBlock->getFunction()->getName()
                   << ": ";
      basicBlock->dumpID();
      llvm::dbgs() << SEP_STR;
      basicBlock->dump();
      llvm::dbgs() << SEP_STR << "Results:\n";
    }

    //translate each SIL instruction to a PartitionOp, if necessary
    std::vector<PartitionOp> partitionOps;
    for (SILInstruction &instruction : *basicBlock) {
      auto ops = translateSILInstruction(&instruction);
      for (PartitionOp &op : ops) {
        partitionOps.push_back(op);

        if (DEBUG) {
          instruction.dump();
          llvm::dbgs() << " └───╼ ";
          op.dump();
        }
      }
    }

    return partitionOps;
  }
};

class BlockPartitionState {
  friend class PartitionAnalysis;

  bool needsUpdate = false;
  bool reached = false;

  Partition entryPartition;
  Partition exitPartition;

  SILBasicBlock *basicBlock;
  PartitionOpTranslator &translator;

  bool blockPartitionOpsPopulated = false;
  std::vector<PartitionOp> blockPartitionOps = {};

  BlockPartitionState(SILBasicBlock *basicBlock,
                      PartitionOpTranslator &translator)
      : basicBlock(basicBlock), translator(translator) {}

  void ensureBlockPartitionOpsPopulated() {
    if (blockPartitionOpsPopulated) return;
    blockPartitionOpsPopulated = true;
    blockPartitionOps = translator.translateSILBasicBlock(basicBlock);
  }

  // recomputes the exit partition from the entry partition,
  // and returns whether this changed the exit partition.
  // Note that this method ignored errors that arise.
  bool recomputeExitFromEntry() {
    ensureBlockPartitionOpsPopulated();

    Partition workingPartition = entryPartition;
    for (auto partitionOp : blockPartitionOps) {
      // by calling apply without providing a `handleFailure` closure,
      // errors will be suppressed
      workingPartition.apply(partitionOp);
    }
    bool exitUpdated = !Partition::equals(exitPartition, workingPartition);
    exitPartition = workingPartition;
    return exitUpdated;
  }

  // apply each PartitionOP in this block to the entry partition,
  // but this time pass in a handleFailure closure that can be used
  // to diagnose any failures
  void diagnoseFailures(llvm::function_ref<void(const PartitionOp &, unsigned)> handleFailure) {
    Partition workingPartition = entryPartition;
    for (auto &partitionOp : blockPartitionOps) {
      workingPartition.apply(partitionOp, handleFailure);
    }
  }

public:
  void dump() {
    llvm::dbgs() << SEP_STR
                 << "BlockPartitionState[reached="
                 << reached
                 << ", needsUpdate="
                 << needsUpdate
                 << "]\nid: ";
    basicBlock->dumpID();
    llvm::dbgs() << "entry partition: ";
    entryPartition.dump();
    llvm::dbgs() << "exit partition: ";
    exitPartition.dump();
    llvm::dbgs() << "instructions:\n┌──────────╼\n";
    for (PartitionOp op : blockPartitionOps) {
      llvm::dbgs() << "│ ";
      op.dump();
    }
    llvm::dbgs() << "└──────────╼\nSuccs:\n";
    for (auto succ : basicBlock->getSuccessorBlocks()) {
      llvm::dbgs() << "→";
      succ->dumpID();
    }
    llvm::dbgs() << "Preds:\n";
    for (auto pred : basicBlock->getPredecessorBlocks()) {
      llvm::dbgs() << "←";
      pred->dumpID();
    }
    llvm::dbgs()<< SEP_STR;
  }
};

class PartitionAnalysis {
  PartitionOpTranslator translator;
  BasicBlockData<BlockPartitionState> blockStates;
  SILFunction *function;
  bool solved;

  // The constructor initializes each block in the function by compiling it
  // to PartitionOps, then seeds the solve method by setting `needsUpdate` to
  // true for the entry block
  PartitionAnalysis(SILFunction *fn)
      : translator(fn),
        blockStates(fn,
                    [this](SILBasicBlock *block) {
                      return BlockPartitionState(block, translator);
                    }),
        function(fn),
        solved(false) {
    // initialize the entry block as needing an update, and having a partition
    // that places all its non-sendable args in a single region
    blockStates[fn->getEntryBlock()].needsUpdate = true;
    blockStates[fn->getEntryBlock()].entryPartition =
        translator.getEntryPartition();
  }

  void solve() {
    assert(!solved && "solve should only be called once");
    solved = true;

    bool anyNeedUpdate = true;
    while (anyNeedUpdate) {
      anyNeedUpdate = false;

      for (auto [block, blockState] : blockStates) {
        if (!blockState.needsUpdate) continue;

        // mark this block as no longer needing an update
        blockState.needsUpdate = false;

        // mark this block as reached by the analysis
        blockState.reached = true;

        // compute the new entry partition to this block
        Partition newEntryPartition;
        bool firstPred = true;

        // this loop computes the join of the exit partitions of all
        // predecessors of this block
        for (SILBasicBlock *predBlock : block.getPredecessorBlocks()) {
          BlockPartitionState &predState = blockStates[predBlock];
          // ignore predecessors that haven't been reached by the analysis yet
          if (!predState.reached) continue;

          if (firstPred) {
            firstPred = false;
            newEntryPartition = predState.exitPartition;
            continue;
          }

          newEntryPartition = Partition::join(
              newEntryPartition, predState.exitPartition);
        }

        // if we found predecessor blocks, then attempt to use them to
        // update the entry partition for this block, and abort this block's
        // update if the entry partition was not updated
        if (!firstPred) {
          // if the recomputed entry partition is the same as the current one,
          // perform no update
          if (Partition::equals(newEntryPartition, blockState.entryPartition))
            continue;

          // otherwise update the entry partition
          blockState.entryPartition = newEntryPartition;
        }

        // recompute this block's exit partition from its (updated) entry
        // partition, and if this changed the exit partition notify all
        // successor blocks that they need to update as well
        if (blockState.recomputeExitFromEntry()) {
          for (SILBasicBlock *succBlock : block.getSuccessorBlocks()) {
            anyNeedUpdate = true;
            blockStates[succBlock].needsUpdate = true;
          }
        }
      }
    }
  }

  // once the fixpoint has been solved for, run one more pass over each basic
  // block, reporting any failures due to requiring consumed regions in the
  // fixpoint state
  void diagnose() {
    assert(solved && "diagnose should not be called before solve");
    for (auto [_, blockState] : blockStates) {
      blockState.diagnoseFailures([&](
          const PartitionOp & partitionOp, unsigned consumedVal) {
        SILInstruction *sourceInstr = partitionOp.getSourceInst();
        assert(sourceInstr && "PartitionOps used in PartitionAnalysis should "
                              "always have been generated with a sourceInst");
        function->getASTContext().Diags.diagnose(
            sourceInstr->getLoc().getSourceLoc(),
            diag::send_non_sendable, consumedVal);
      });
    }
  }

public:

  void dump() {
    llvm::dbgs() << "\nPartitionAnalysis[fname=" << function->getName() << "]\n";

    for (auto [_, blockState] : blockStates) {
      blockState.dump();
    }
  }

  static void performForFunction(SILFunction *function) {
    auto analysis = PartitionAnalysis(function);
    analysis.solve();
    if (DEBUG) {
      llvm::dbgs() << "SOLVED: ";
      analysis.dump();
    }
    analysis.diagnose();
  }
};

class SendNonSendable : public SILFunctionTransform {

  // find any ApplyExprs in this function, and check if any of them make an
  // unsatisfied isolation jump, emitting appropriate diagnostics if so
  void run() override {
    SILFunction *function = getFunction();
    DeclContext *declContext = function->getDeclContext();

    // if this function does not correspond to a syntactic declContext, don't
    // check it
    // TODO: revisit this assumption, in particular perhaps verify no isolation
    // crossing applies occurs within these
    if (!declContext)
      return;

    // if the experimental feature DeferredSendableChecking is not provided,
    // do not perform this pass
    if (!function->getASTContext().LangOpts.hasFeature(
            Feature::DeferredSendableChecking))
      return;

    // if the Sendable protocol is not available, don't perform this checking
    // because we'd have to conservatively treat every value as non-Sendable
    // which would be very expensive
    if (!function->getASTContext().getProtocol(KnownProtocolKind::Sendable))
      return;

    PartitionAnalysis::performForFunction(function);
  }
};
}

/// This pass is known to depend on the following passes having run before it:
/// none so far
SILTransform *swift::createSendNonSendable() {
  return new SendNonSendable();
}
