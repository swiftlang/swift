//===-- FlowIsolation.cpp - Enforces flow-sensitive actor isolation rules -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-flow-isolation"

#include "DiagnosticHelpers.h"

#include "swift/AST/ActorIsolation.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BitDataflow.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/OperandDatastructures.h"
#include "swift/SILOptimizer/Analysis/RegionAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/VariableNameUtils.h"
#include "swift/Sema/Concurrency.h"

#include "llvm/Support/WithColor.h"

using namespace swift;
using namespace swift::siloptimizer;

namespace {

class FunctionInfo;

} // namespace

//===----------------------------------------------------------------------===//
//                              MARK: Utilities
//===----------------------------------------------------------------------===//

static SILFunction* getCallee(SILInstruction *someInst) {
  if (auto apply = ApplySite::isa(someInst))
    if (SILFunction *callee = apply.getCalleeFunction())
      return callee;
  return nullptr;
}

template <typename... T, typename... U>
static InFlightDiagnostic
loggedDiagnoseErrorAndHighlight(const SILInstruction *inst, Diag<T...> diag,
                                U &&...args) {
  LLVM_DEBUG(llvm::dbgs() << "Emitting error at: " << *inst);
  return diagnoseErrorAndHighlight(inst, std::move(diag), std::move(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic
loggedDiagnoseNoteAndHighlight(const SILInstruction *inst, Diag<T...> diag,
                               U &&...args) {
  LLVM_DEBUG(llvm::dbgs() << "Emitting note at: " << *inst);
  return diagnoseNoteAndHighlight(inst, std::move(diag), std::move(args)...);
}

//===----------------------------------------------------------------------===//
//                             MARK: LatticeState
//===----------------------------------------------------------------------===//

namespace {

/// Represents the state of isolation for `self` during the flow-analysis,
/// at entry and exit to a block. The states are part of a semi-lattice,
/// where the extra top element represents a conflict in isolation:
///
///         T = "top"
///        / \
///      Iso  NonIso
///        \ /
///         B = "bottom"
///
/// While we will be talking about isolated vs nonisolated uses, the only
/// isolated uses that we consider are stored property accesses.
struct LatticeState {
  /// Each state kind, as an integer, is its position in any bit vectors.
  enum Kind {
    Isolated = 0,
    Nonisolated = 1
  };

  /// Number of states, excluding Top or Bottom, in this flow problem.
  static constexpr unsigned NumStates = 2;
};

} // namespace

//===----------------------------------------------------------------------===//
//                            MARK: Per Block Info
//===----------------------------------------------------------------------===//

namespace {

/// Information gathered for analysis that is specific to a block.
struct BlockInfo {
  /// Records all nonisolated uses of `self` in the block, and their kind of
  /// use to aid diagnostics.
  SmallPtrSet<SILInstruction *, 8> nonisolatedUses;

  /// Records all stored property uses based on `self` in the block.
  /// These are the only isolated uses that we care about.
  SmallPtrSet<Operand *, 8> propertyUses;

  BlockInfo() : nonisolatedUses(), propertyUses() {}

  // Diagnoses all property uses as being an error.
  void diagnoseAll(FunctionInfo &info, bool forDeinit,
                   SILInstruction *blame = nullptr);

  /// Returns the block corresponding to this information.
  SILBasicBlock* getBlock() const {
    if (!propertyUses.empty())
      return (*(propertyUses.begin()))->getParentBlock();

    if (!nonisolatedUses.empty())
      return (*(nonisolatedUses.begin()))->getParent();

    // I only expect to call this when there's a use, so to save memory
    // we compute the corresponding block from its stored uses.
    assert(false && "no uses to determine block");
    return nullptr;
  }

  SILInstruction *firstPropertyUse() const {
    auto *blk = getBlock();

    for (auto &inst : *blk) {
      for (auto &op : inst.getAllOperands()) {
        if (propertyUses.count(&op))
          return &inst;
      }
    }

    assert(false && "no first property use found!");
    return nullptr;
  }

  bool hasNonisolatedUse() const {
    return !nonisolatedUses.empty();
  }

  bool hasPropertyUse() const {
    return !propertyUses.empty();
  }

  void print(llvm::raw_ostream &os) const {
    os << "nonisolatedUses:\n";
    for (auto const *i : nonisolatedUses)
      i->print(os);

    os << "propertyUses:\n";
    for (auto const *i : propertyUses)
      i->print(os);
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

} // namespace

/// \returns true iff the function is a deinit, or a defer of a deinit.
static bool isWithinDeinit(SILFunction *fn) {
  auto *astFn = fn->getDeclContext()->getAsDecl();

  if (auto *funcDecl = dyn_cast<FuncDecl>(astFn))
    if (funcDecl->isDeferBody())
      astFn = funcDecl->getParent()->getAsDecl();

  return isa<DestructorDecl>(astFn);
}

//===----------------------------------------------------------------------===//
//                          MARK: Per Function Info
//===----------------------------------------------------------------------===//

namespace {

class IsolationInfoCache {
  RegionAnalysisFunctionInfo *rafi;
  llvm::DenseMap<std::pair<SILInstruction *, SILValue>,
                 SILDynamicMergedIsolationInfo>
      cache;

  struct ComputeEvaluator final
      : public PartitionOpEvaluatorBaseImpl<ComputeEvaluator> {
    RegionAnalysisFunctionInfo *rafi;
    ComputeEvaluator(RegionAnalysisFunctionInfo *rafi,
                     Partition &workingPartition)
        : PartitionOpEvaluatorBaseImpl(workingPartition,
                                       rafi->getOperandSetFactory(),
                                       rafi->getSendingOperandToStateMap()),
          rafi(rafi) {}

    SILIsolationInfo getIsolationRegionInfo(Element elt) const {
      return rafi->getValueMap().getIsolationRegion(elt);
    }

    std::optional<Element> getElement(SILValue value) const {
      auto trackableValue = rafi->getValueMap().getTrackableValue(value);
      if (trackableValue.value.isSendable())
        return {};
      return trackableValue.value.getID();
    }

    regionanalysisimpl::TrackableValueLookupResult
    lookupValue(SILValue value) const {
      return rafi->getValueMap().getTrackableValue(value);
    }

    SILValue getRepresentative(SILValue value) const {
      return rafi->getValueMap()
          .getTrackableValue(value)
          .value.getRepresentative()
          .maybeGetValue();
    }

    SILDynamicMergedIsolationInfo getIsolation(Region reg) {
      return PartitionOpEvaluator<ComputeEvaluator>::getIsolationRegionInfo(
          reg);
    }

    RepresentativeValue getRepresentativeValue(Element element) const {
      return rafi->getValueMap().getRepresentativeValue(element);
    }

    bool isClosureCaptured(Element elt, Operand *op) const {
      auto iter = rafi->getValueMap().maybeGetRepresentative(elt);
      if (!iter)
        return false;
      return rafi->isClosureCaptured(iter, op);
    }
  };

public:
  IsolationInfoCache(RegionAnalysisFunctionInfo *rafi) : rafi(rafi) {}

  SILDynamicMergedIsolationInfo getIsolationInfoAtInst(SILInstruction *inst,
                                                       SILValue value) const {
    // If we are not in a supported function, just return invalid always.
    if (!rafi->isSupportedFunction())
      return {};

    // First try_emplace with a default value.
    auto *self = const_cast<IsolationInfoCache *>(this);
    auto iter = self->cache.try_emplace({inst, value}, SILIsolationInfo());

    // If we failed to insert, we already have a value... just return that.
    if (!iter.second)
      return iter.first->second;

    // Otherwise, we need to find the actual isolation of the value.
    auto blockState = rafi->getBlockState(inst->getParent());
    if (blockState.isNull() || !blockState.get()->getLiveness()) {
      // If our block state is null or we have a dead block, just return
      // invalid.
      iter.first->getSecond() = SILIsolationInfo();
      return iter.first->getSecond();
    }

    // Grab its entry partition and setup an evaluator for the partition that
    // has callbacks that emit diagnsotics...
    Partition workingPartition = blockState.get()->getEntryPartition();
    ComputeEvaluator eval(rafi, workingPartition);

    // And then evaluate all of our partition ops on the entry partition until
    // we hit our instruction.
    auto partitionOps = blockState.get()->getPartitionOps();
    while (!partitionOps.empty()) {
      const auto &next = partitionOps.front();
      if (next.getSourceInst() == inst)
        break;
      partitionOps = partitionOps.drop_front();
      eval.apply(next);
    }

    // Now look up our trackable value lookup result.
    auto lookupResult = eval.lookupValue(value);

    // First see if our lookup result is Sendable...
    if (lookupResult.value.isSendable()) {
      // In such a case, see if we have a base value that is non-Sendable. If
      // so, use its isolation.
      if (auto base = lookupResult.base) {
        auto isolation =
            eval.getIsolation(workingPartition.getRegion(base->getID()));
        iter.first->getSecond() = isolation;
        return iter.first->getSecond();
      }

      // Otherwise, return an invalid isolation.
      iter.first->getSecond() = SILIsolationInfo();
      return iter.first->getSecond();
    }

    auto isolation = eval.getIsolation(
        workingPartition.getRegion(lookupResult.value.getID()));
    iter.first->getSecond() = isolation;
    return iter.first->getSecond();
  }
};

/// Carries the state of analysis for an entire SILFunction.
class FunctionInfo : public BasicBlockData<BlockInfo> {
private:
  /// Isolation state at the start of the entry block to this function.
  /// This should always be `isolated`, unless if this is a `defer`.
  LatticeState::Kind startingIsolation = LatticeState::Isolated;

public:

  // The deferBlocks information is shared between all blocks of
  // this analysis information's function.
  llvm::SmallMapVector<SILFunction *, std::unique_ptr<FunctionInfo>, 8>
      deferBlocks;

  // Only computed after calling solve()
  BitDataflow flow;

  /// This value represents the outgoing isolation state of the function if
  /// a normal return is reached, along with the block that returns normally.
  /// Only computed after calling solve(), where it remains None if the function
  /// doesn't return normally.
  std::optional<std::pair<SILBasicBlock *, LatticeState::Kind>> normalReturn =
      std::nullopt;

  RegionAnalysis *ra;
  RegionAnalysisFunctionInfo *rafi;

  IsolationInfoCache isolationInfoCache;

  /// Indicates whether the SILFunction is (or contained in) a deinit.
  bool forDeinit;

  // Use a worklist to track the uses left to be searched.
  std::optional<OperandWorklist> worklist;

  FunctionInfo(SILFunction *fn, RegionAnalysis *ra)
      : BasicBlockData<BlockInfo>(fn), flow(fn, LatticeState::NumStates),
        ra(ra), rafi(ra->get(fn)), isolationInfoCache(rafi),
        forDeinit(isWithinDeinit(fn)), worklist(fn) {}

  // analyzes the function for uses of `self`.
  void analyze(SILValue selfParam);

  // Solves the data-flow problem, assuming analysis has been performed.
  void solve();

  // Verifies uses in this function, assuming solving has been performed.
  void emitDiagnostics();

  /// Finds an appropriate instruction that can be blamed for introducing a
  /// source of `nonisolation` in a control-flow path leading the given
  /// instruction. Preferring the closest block. Use for diagnostics.
  /// \param start an instruction that can be reached by a `nonisolated`
  /// use in the CFG.
  /// \returns an instruction that can be used for blame in a diagnostic.
  SILInstruction *findNonisolatedBlame(SILInstruction *start);

  void diagnoseEntireFunction(SILInstruction* blame) {
    assert(blame);
    for (auto bnd : *this)
      bnd.data.diagnoseAll(*this, forDeinit, blame);
  }

  /// Does this function have a nonisolated use?
  bool hasNonisolatedUse() const {
    for (auto const& bnd : *this)
      if (bnd.data.hasNonisolatedUse())
        return true;

    return false;
  }

  /// Does this function have a property use?
  bool hasPropertyUse() const {
    for (auto const& bnd : *this)
      if (bnd.data.hasPropertyUse())
        return true;

    return false;
  }

  /// Do we have sub-analysis information for this function, as a defer body?
  bool haveDeferInfo(SILFunction *someFn) {
    assert(someFn);
    return deferBlocks.count(someFn) > 0;
  }

  FunctionInfo &getOrCreateDeferInfo(SILFunction *someFn) {
    assert(someFn);

    if (haveDeferInfo(someFn))
      return *(deferBlocks[someFn]);

    // otherwise, insert fresh info and retry.
    deferBlocks.insert({someFn, std::make_unique<FunctionInfo>(someFn, ra)});
    return getOrCreateDeferInfo(someFn);
  }

  /// Records an incoming isolation kind to this function from a call-site.
  /// \returns true iff the start state has changed from isolated to nonisolated
  bool setNonisolatedStart() {
    // once we enter the nonisolated state, nothing will change that.
    if (startingIsolation == LatticeState::Nonisolated)
      return false;

    startingIsolation = LatticeState::Nonisolated;
    return true;
  }

  /// Test whether the incoming isolation kind was set to nonisolated.
  bool hasNonisolatedStart() const {
    return startingIsolation == LatticeState::Nonisolated;
  }

  void lookThroughInst(SILInstruction *i) {
    LLVM_DEBUG(llvm::dbgs() << "Looking through: " << *i);
    worklist->pushResultOperandsIfNotVisited(i);
  }

  void lookThroughValue(SILValue v) {
    LLVM_DEBUG(llvm::dbgs() << "Looking through: " << v);
    worklist->pushResultOperandsIfNotVisited(v);
  }

  void markIgnored(SILInstruction *i) {
    LLVM_DEBUG(llvm::dbgs() << "Ignoring: " << *i);
  }

  /// Records that the instruction accesses an isolated property.
  void markPropertyUse(Operand *i, bool isDefault = false) {
    // If we have an actor isolated function and the isolation of our value at
    // out user matches the function, we should not error.
    //
    // E.x.:
    //
    // @MainActor
    // struct S {
    //   @MainActor var x: NS
    //   @CustomActor var y: NS
    //
    //   nonisolated func trigger() {}
    //
    //   @CustomActor init() {
    //     x = NS()
    //     y = NS()
    //     trigger()
    //     _ = x // Error here.
    //     _ = y // But not here b/c y is @CustomActor.
    //   }
    //
    auto funcIsolation = rafi->getFunction()->getActorIsolation();
    if (funcIsolation.isActorIsolated()) {
      // If our use is Non-Sendable, then we can rely on region isolation.
      if (SILIsolationInfo::isNonSendable(i->get())) {
        if (auto iso = isolationInfoCache.getIsolationInfoAtInst(i->getUser(),
                                                                 i->get())) {
          if (iso->isActorIsolated() &&
              iso->getActorIsolation() == funcIsolation) {
            LLVM_DEBUG(llvm::dbgs() << "isolated use that is safe b/c value is "
                                       "isolated to same as constructor: "
                                    << "Op Num. " << i->getOperandNumber()
                                    << ". User: " << *i->getUser());
            return;
          }
        }
      } else {
        // If our operand was Sendable, we may have our traversal at a
        // projection. See if we can find a VarDecl for it.
        if (auto *svi = dyn_cast<SingleValueInstruction>(i->getUser());
            llvm::isa_and_present<StructElementAddrInst, RefElementAddrInst>(
                svi)) {
          Projection proj(svi);
          if (auto *decl = proj.getVarDecl(svi->getOperand(0)->getType())) {
            if (auto declIsolation = swift::getActorIsolation(decl);
                declIsolation && declIsolation.isActorIsolated() &&
                declIsolation == funcIsolation) {
              LLVM_DEBUG(llvm::dbgs()
                         << "isolated use that is safe b/c value is "
                            "isolated to same as constructor. Op Num: "
                         << i->getOperandNumber()
                         << ". User: " << *i->getUser());
              return;
            }
          }
        }
      }
    }

    LLVM_DEBUG(llvm::dbgs()
               << (isDefault ? "Default pattern match. " : "")
               << "Marking as isolated: OpNum: " << i->getOperandNumber()
               << ". User: " << *i->getUser());
    auto &blockData = this->operator[](i->getParentBlock());
    blockData.propertyUses.insert(i);
  }

  /// Records that the instruction causes 'self' to become nonisolated.
  void markNonIsolated(SILInstruction *i) {
    LLVM_DEBUG(llvm::dbgs() << "Marking as non-isolated: " << *i);
    auto &blockData = this->operator[](i->getParent());
    blockData.nonisolatedUses.insert(i);
  }

  void print(llvm::raw_ostream &os) const {
    os << "analysis-info for " << getFunction()->getName() << "\n";
    for (auto const& bnd : *this) {
      os << "bb" << bnd.block.getDebugID() << "\n";
      bnd.data.print(os);
    }
    os << "flow-problem state:\n";
    flow.print(os);

    // print the defer information in a different color, if supported.
    llvm::WithColor color(os, raw_ostream::BLUE);
    for (auto const& entry : deferBlocks)
      entry.second->print(os);
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

} // namespace

//===----------------------------------------------------------------------===//
//                             MARK: Diagnostics
//===----------------------------------------------------------------------===//

SILInstruction *FunctionInfo::findNonisolatedBlame(SILInstruction *startInst) {
  assert(startInst);

  SILBasicBlock* firstBlk = startInst->getParent();
  assert(firstBlk->getParent() == getFunction());

  // searches the a block starting at the provided position in reverse
  // order of instructions (i.e., from terminator to first instruction).
  auto searchBlockForNonisolated =
      [&](SILBasicBlock::reverse_iterator cursor) -> SILInstruction * {
    SILBasicBlock *block = cursor->getParent();
    auto &state = flow[block];

    // does this block generate non-isolation?
    if (state.genSet[LatticeState::Nonisolated]) {
      auto &data = this->operator[](block);
      assert(!data.nonisolatedUses.empty());

      // scan from the cursor backwards in this block.
      while (cursor != block->rend()) {
        auto *inst = &*cursor;
        cursor++;

        if (data.nonisolatedUses.count(inst)) {
          return inst;
        }
      }
    }

    return nullptr;
  };

  // whether we should visit a given predecessor block in the search.
  auto shouldVisit = [&](SILBasicBlock *pred) {
    // visit blocks that contribute nonisolation to successors.
    return flow[pred].exitSet[LatticeState::Nonisolated];
  };

  // first check if the nonisolated use precedes the start instruction in
  // this same block.
  if (auto *inst = searchBlockForNonisolated(startInst->getReverseIterator()))
    return inst;

  // Seed a workQueue with the predecessors of this start block to
  // begin a breadth-first search to find one of the closest predecessors.
  BasicBlockWorkqueue workQueue(firstBlk->getFunction());
  for (auto *pred : firstBlk->getPredecessorBlocks())
    if (shouldVisit(pred))
      workQueue.push(pred);

  while (auto *block = workQueue.pop()) {
    // do we have a nonisolated use here?
    if (auto *inst = searchBlockForNonisolated(block->rbegin()))
      return inst;

    // otherwise keep looking
    for (auto *pred : block->getPredecessorBlocks()) {
      if (shouldVisit(pred))
        workQueue.pushIfNotVisited(pred);
    }
  }

  llvm_unreachable("failed to find nonisolated blame.");
}

static StringRef verbForInvoking(ValueDecl *value) {
  // Only computed properties need a different verb.
  if (isa<AbstractStorageDecl>(value))
    return "accessing ";

  return "calling ";
}

/// For a specific note diagnostic that describes the blamed instruction for
/// introducing non-isolation, this function produces the values needed
/// to describe it to the user. Thus, the implementation of this function is
/// closely tied to that diagnostic.
static std::tuple<StringRef, StringRef, DeclName>
describe(SILInstruction *blame) {
  auto &ctx = blame->getModule().getASTContext();

  // check if it's a call-like thing.
  if (auto apply = ApplySite::isa(blame)) {
    /// First, look for a callee declaration.
    ///
    /// We can't use ApplySite::getCalleeFunction because it is overly
    /// precise in finding the specific corresponding SILFunction. We only care
    /// about describing the referenced AST decl, since that's all programmers
    /// know.

    ValueDecl *callee = nullptr;

    auto inspect = [](ValueDecl *decl) -> ValueDecl* {
      // if this is an accessor, then return the storage instead.
      if (auto accessor = dyn_cast<AccessorDecl>(decl))
        return accessor->getStorage();

      return decl;
    };

    SILValue silCallee = apply.getCalleeOrigin();
    if (auto *methInst = dyn_cast<MethodInst>(silCallee))
      callee = inspect(methInst->getMember().getDecl());

    if (auto *funcInst = dyn_cast<FunctionRefBaseInst>(silCallee)) {
      auto *refFunc = funcInst->getInitiallyReferencedFunction();

      if (auto *declCxt = refFunc->getDeclContext()) {
        if (auto *absFn =
            dyn_cast_or_null<AbstractFunctionDecl>(declCxt->getAsDecl())) {
          callee = inspect(absFn);
        } else if (isa<AbstractClosureExpr>(declCxt)) {
          // TODO: determine if the closure captures self, or is applied to it,
          // so we can be more specific in this message.
          return std::make_tuple("this closure involving", "", ctx.Id_self);
        }
      }
    }

    // if we have no callee info, all we know is it's a call involving self.
    if (!callee)
      return std::make_tuple("a call involving", "", ctx.Id_self);

    // otherwise, form the tuple relative to the callee decl.
    return std::make_tuple(
      verbForInvoking(callee),
      callee->getDescriptiveKindName(callee->getDescriptiveKind()),
      callee->getName()
    );
  }

  // handle other non-call blames.
  switch (blame->getKind()) {
  case SILInstructionKind::CopyValueInst:
    return std::make_tuple("making a copy of", "", ctx.Id_self);
  default:
    return std::make_tuple("this use of", "", ctx.Id_self);
  }
}

/// Emits errors for all isolated uses of `self` in the given block.
/// \param blame the instruction to blame for introducing non-isolation.
/// If not provided, a suitable instruction will be automatically found using a
/// search.
/// \param info the AnalysisInfo corresponding to the function containing this
/// block.
void BlockInfo::diagnoseAll(FunctionInfo &info, bool forDeinit,
                            SILInstruction *blame) {
  if (propertyUses.empty())
    return;

  auto *fn = info.getFunction();
  auto &ctx = fn->getASTContext();

  // Disable these diagnostics in deinitializers unless complete checking is
  // enabled.
  if (forDeinit && ctx.LangOpts.StrictConcurrencyLevel
        != StrictConcurrency::Complete)
    return;

  // Blame that is valid for the first property use is valid for all uses
  // in this block.
  if (!blame)
    blame = info.findNonisolatedBlame(firstPropertyUse());

  // if needed, find the blame inside of the defer callee.
  if (auto *callee = getCallee(blame)) {
    if (info.haveDeferInfo(callee)) {
      auto &defer = info.getOrCreateDeferInfo(callee);
      assert(defer.normalReturn && "noreturn defer should never be blamed!");

      auto *retBlk = defer.normalReturn->first;
      blame = defer.findNonisolatedBlame(retBlk->getTerminator());
    }
  }

  for (auto *use : propertyUses) {
    // If the illegal use is a call to a defer, then recursively diagnose
    // all of the defer's uses, if this is the first time encountering it.
    if (auto *callee = getCallee(use->getUser())) {
      if (info.haveDeferInfo(callee)) {
        auto &defer = info.getOrCreateDeferInfo(callee);
        if (defer.setNonisolatedStart()) {
          defer.diagnoseEntireFunction(blame);
        }
        continue;
      }

      // Init accessor `setter` use.
      auto *accessor =
          cast<AccessorDecl>(callee->getLocation().getAsDeclContext());
      loggedDiagnoseErrorAndHighlight(
          use->getUser(),
          diag::isolated_property_mutation_in_nonisolated_context,
          accessor->getStorage(), accessor->isSetter())
          .warnUntilLanguageMode(LanguageMode::v6);
      continue;
    }

    auto *user = use->getUser();
    StringRef isolation = "nonisolated";
    auto functionIsolation = user->getFunction()->getActorIsolation();
    if (functionIsolation.isActorIsolated()) {
      SmallString<64> temp;
      {
        llvm::raw_svector_ostream os(temp);
        functionIsolation.printForDiagnostics(os);
      }

      isolation =
          user->getFunction()->getASTContext().getIdentifier(temp).str();
    }

    if (auto *rfi = dyn_cast<RefElementAddrInst>(user)) {
      loggedDiagnoseErrorAndHighlight(rfi, diag::isolated_after_nonisolated,
                                      forDeinit, rfi->getField(), isolation)
          .warnUntilLanguageMode(LanguageMode::v6);
    } else if (auto *seai = dyn_cast<StructElementAddrInst>(user)) {
      loggedDiagnoseErrorAndHighlight(seai, diag::isolated_after_nonisolated,
                                      forDeinit, seai->getField(), isolation)
          .warnUntilLanguageMode(LanguageMode::v6);
    } else {
      auto name = VariableNameInferrer::inferName(use->get());
      loggedDiagnoseErrorAndHighlight(
          user, diag::isolated_after_nonisolated_identifier, forDeinit, *name,
          isolation)
          .warnUntilLanguageMode(LanguageMode::v6);
    }

    // after <verb><adjective> <subject>, ... can't use self anymore, etc ...
    //   example:
    // after calling function 'hello()', ...
    StringRef verb;
    StringRef adjective;
    DeclName subject;
    std::tie(verb, adjective, subject) = describe(blame);
    loggedDiagnoseNoteAndHighlight(blame, diag::nonisolated_blame, forDeinit,
                                   verb, adjective, subject, isolation);
  }
}

//===----------------------------------------------------------------------===//
//                               MARK: Analysis
//===----------------------------------------------------------------------===//

/// \returns true iff the access is concurrency-safe in a nonisolated context
/// without an await.
static bool accessIsConcurrencySafe(SILInstruction *inst, VarDecl *var) {
  // must be accessible from nonisolated.
  return isLetAccessibleAnywhere(
      inst->getFunction()->getModule().getSwiftModule(), var);
}

/// \returns true iff the ref_element_addr instruction is only used
/// to deinitialize the referenced element.
static bool onlyDeinitAccess(RefElementAddrInst *inst) {
  if (auto operand = inst->getSingleUse()) {
    if (auto *access = dyn_cast<BeginAccessInst>(operand->getUser())) {
      return access->getAccessKind() == SILAccessKind::Deinit;
    }
  }
  return false;
}

/// Checks that the accessed element conforms to Sendable; emitting a
/// diagnostic if it is not Sendable. The diagnostic assumes that the access
/// is happening in a deinit that uses flow-isolation.
/// \returns true iff a diagnostic was emitted for this reference.
static bool diagnoseNonSendableFromDeinit(RefElementAddrInst *inst) {
  auto dc = inst->getFunction()->getDeclContext();

  // For historical reasons, only diagnose this issue in strict mode.
  if (dc->getASTContext().LangOpts.StrictConcurrencyLevel
        != StrictConcurrency::Complete)
    return false;

  return swift::diagnoseNonSendableFromDeinit(
      inst->getLoc().getSourceLoc(),
      inst->getField(),
      dc);
}

/// Analyzes a function for uses of `self` and records the kinds of isolation
/// required.
/// \param selfParam the parameter of \c getFunction() that should be
/// treated as \c self
void FunctionInfo::analyze(SILValue selfParam) {
  if (!selfParam) {
    LLVM_DEBUG(llvm::dbgs() << "Analysis. Nullptr selfParam. Skipping!\n");
    return;
  }

  LLVM_DEBUG(llvm::dbgs() << "Analysis. Starting value: " << selfParam);

  // Seed with direct users of `self`
  worklist->pushResultOperandsIfNotVisited(selfParam);

  while (Operand *operand = worklist->pop()) {
    // A type-dependent use of `self` is an instruction that contains the
    // DynamicSelfType. These instructions do not access any protected
    // state.
    if (operand->isTypeDependent())
      continue;

    SILInstruction *user = operand->getUser();

    // First, check if this is an apply that involves `self`
    if (auto apply = ApplySite::isa(user)) {

      // Check if the callee is a function representing a defer block.
      if (SILFunction *callee = apply.getCalleeFunction()) {
        if (auto *dc = callee->getDeclContext()) {
          if (auto *decl = dyn_cast_or_null<FuncDecl>(dc->getAsDecl())) {
            if (decl->isDeferBody()) {

              // If we need to analyze the defer first, do so.
              if (!haveDeferInfo(callee)) {
                // NOTE: the defer function is not like a method, because it
                // doesn't satisfy hasSelfParam().
                auto const* calleeSelfParam =
                  callee->getArgument(apply.getAppliedArgIndex(*operand));

                // Recursion depth is bounded by the lexical nesting of
                // defer blocks in the input program.
                auto &defer = getOrCreateDeferInfo(callee);
                defer.analyze(calleeSelfParam);
                defer.solve();
              }

              auto const& defer = getOrCreateDeferInfo(callee);

              // A defer effectively has one exit block, because it can't throw.
              // Otherwise, it may never return (e.g., fatalError).
              // So, we say that this instruction generates nonisolation only
              // if it can return normally, and if it does, it carries
              // nonisolation.
              if (defer.normalReturn) {
                if (defer.normalReturn->second == LatticeState::Nonisolated) {
                  markNonIsolated(user);
                }
              }

              // If the defer body has any stored property uses, we record that
              // in the parent by declaring this call-site being a property use.
              if (defer.hasPropertyUse())
                markPropertyUse(operand);

              continue;
            }
          }
        }

        // Detect and handle use of init accessor properties.
        if (callee->hasLocation()) {
          auto loc = callee->getLocation();
          if (auto *accessor =
                  dyn_cast_or_null<AccessorDecl>(loc.getAsDeclContext())) {
            auto *storage = accessor->getStorage();

            // Note 'nonisolated' property use.
            if (storage->getAttrs().hasAttribute<NonisolatedAttr>()) {
              markNonIsolated(user);
              continue;
            }

            // Init accessor is used exclusively for initialization
            // of properties while 'self' is not fully initialized.
            if (accessor->isInitAccessor()) {
              markNonIsolated(user);
              continue;
            }

            // Otherwise if this is an init accessor property, it's either
            // a call to a getter or a setter and should be treated like
            // an isolated computed property reference.

            if (storage->hasInitAccessor()) {
              markPropertyUse(operand);
              continue;
            }
          }
        }
      }

      // For all other call-sites, uses of `self` are nonisolated.
      markNonIsolated(user);
      continue;
    }

    // Handle non-ApplySite instructions.
    switch (user->getKind()) {
      // Look for a property access.
      // Sadly, formal accesses are not always emitted by SILGen, particularly,
      // within the initializers we care about. So we rely on ref_element_addr.
      case SILInstructionKind::RefElementAddrInst: {
        RefElementAddrInst *refInst = cast<RefElementAddrInst>(user);

        // skip auto-generated deinit accesses.
        if (onlyDeinitAccess(refInst)) {
          markIgnored(user);
          continue;
        }

        // skip known-safe accesses.
        if (accessIsConcurrencySafe(refInst, refInst->getField())) {
          markIgnored(user);
          continue;
        }

        // emit a diagnostic and skip if it's non-sendable in a deinit
        if (forDeinit && diagnoseNonSendableFromDeinit(refInst)) {
          markIgnored(user);
          continue;
        }

        markPropertyUse(operand);
        continue;
      }
      case SILInstructionKind::StructElementAddrInst: {
        auto *seai = cast<StructElementAddrInst>(user);

        if (accessIsConcurrencySafe(seai, seai->getField())) {
          markIgnored(user);
          continue;
        }

        markPropertyUse(operand);
        continue;
      }
      // Look through certian kinds of single-value instructions.
      case SILInstructionKind::CopyValueInst:
        // TODO: If we had some actual escape analysis information, we could
        // avoid marking a trivial copy as a nonisolated use, since it doesn't
        // actually escape the function. We have to be conservative here
        // and assume it might.
        markNonIsolated(user);
        continue;

      // Treat any load as a nonisolated use. (We are treating this as an
      // escape).
      case SILInstructionKind::LoadInst:
      case SILInstructionKind::LoadBorrowInst:
        markNonIsolated(user);
        continue;

      case SILInstructionKind::StoreInst:
      case SILInstructionKind::StoreBorrowInst:
      case SILInstructionKind::StoreUnownedInst:
      case SILInstructionKind::StoreWeakInst:
      case SILInstructionKind::CopyAddrInst: {
        // If we are the dest of a copy addr inst, treat it as a property
        // use. If we are the src of a copy addr inst, treat it as a nonisolated
        // use.
        if (operand->getOperandNumber() == CopyLikeInstruction::Src) {
          markNonIsolated(user);
          continue;
        }
        markPropertyUse(operand);
        continue;
      }
      case SILInstructionKind::MarkUnresolvedMoveAddrInst:
      case SILInstructionKind::MarkUnresolvedNonCopyableValueInst:
      case SILInstructionKind::MarkUnresolvedReferenceBindingInst:
      case SILInstructionKind::UncheckedRefCastInst:
      case SILInstructionKind::UnconditionalCheckedCastInst:
      case SILInstructionKind::UncheckedOwnershipConversionInst:
      case SILInstructionKind::BeginAccessInst:
      case SILInstructionKind::BeginBorrowInst:
      case SILInstructionKind::EndInitLetRefInst: {
        lookThroughInst(user);
        continue;
      }

      case SILInstructionKind::BuiltinInst: {
        auto *bi = cast<BuiltinInst>(user);
        if (auto bk = bi->getBuiltinKind()) {
          switch (*bk) {
          case BuiltinValueKind::DestroyDefaultActor:
          case BuiltinValueKind::InitializeDefaultActor:
            markIgnored(user);
            continue;
          default:
            break;
          }
        }
        markPropertyUse(operand, true /*default*/);
        continue;
      }

      case SILInstructionKind::BranchInst:
        lookThroughValue(cast<BranchInst>(user)->getArgForOperand(operand));
        continue;

        // We ignore return inst. We rely on the type checker to make sure that
        // this is safe.
      case SILInstructionKind::ReturnInst:
        markIgnored(user);
        continue;

      case SILInstructionKind::DestroyAddrInst:
      case SILInstructionKind::DestroyValueInst:
      case SILInstructionKind::DeallocStackInst:
      case SILInstructionKind::DebugValueInst:
      case SILInstructionKind::EndBorrowInst:
      case SILInstructionKind::EndAccessInst:
      case SILInstructionKind::EndLifetimeInst:
      case SILInstructionKind::ClassMethodInst:
      case SILInstructionKind::ValueMetatypeInst:
        markIgnored(user);
        // Ignore these.
        continue;
      default:
#if false        
        // Anything we do not understand mark as a property use to be
        // conservative.
        markPropertyUse(operand, true /*default*/);
#else
        // For now if we do not understand something, just ignore it to restore
        // the previous behavior.
        markIgnored(user);
#endif
        continue;
      }
  }
}

/// Initialize and solve the dataflow problem, assuming the entry block starts
/// isolated.
void FunctionInfo::solve() {
  SILFunction *fn = getFunction();
  SILBasicBlock *returnBlk = nullptr;

  // NOTE: if the starting isolation is nonisolated, the solution is trivial.
  // Since we don't expect calls to solve in that situation, I haven't
  // implemented that

  // Initialize the forward dataflow problem.
  for (auto pair : flow) {
    SILBasicBlock *blk = &(pair.block);
    auto &data = pair.data;

    // record the return block
    if (isa<ReturnInst>(blk->getTerminator())) {
      assert(returnBlk == nullptr); // should only be one!
      returnBlk = blk;
    }

    // Set everything to Bottom.
    data.entrySet.reset();
    data.genSet.reset();
    data.killSet.reset();
    data.exitSet.reset();

    if (blk == fn->getEntryBlock())
      data.entrySet.set(startingIsolation);

    // A nonisolated use "kills" isolation and generates nonisolation.
    if (this->operator[](blk).hasNonisolatedUse()) {
      data.killSet.set(LatticeState::Isolated);
      data.genSet.set(LatticeState::Nonisolated);
    }
  }

  // Solve using a union so that Top represents a conflict.
  flow.solveForwardWithUnion();

  // If this function can return normally, update the outgoing isolation
  // in that case. This is needed to implement `defer`.
  if (returnBlk) {
    auto &returnInfo = flow[returnBlk];
    if (returnInfo.exitSet[LatticeState::Nonisolated])
      normalReturn = std::make_pair(returnBlk, LatticeState::Nonisolated);
    else
      normalReturn = std::make_pair(returnBlk, LatticeState::Isolated);
  }
}

/// Enforces isolation rules, given the flow and block-local information.
void FunctionInfo::emitDiagnostics() {
  // go through all the blocks.
  for (auto entry : *this) {
    auto &block = entry.block;
    auto &data = entry.data;
    auto &flowInfo = flow[&block];

    // If the block has no isolated uses, then skip it.
    if (data.propertyUses.empty())
      continue;

    // If flow-analysis determined that we might be `nonisolated` coming
    // into this block, then all isolated uses in this block are invalid.
    if (flowInfo.entrySet[LatticeState::Nonisolated]) {
      data.diagnoseAll(*this, forDeinit);
      continue;
    }

    // Otherwise, we must be starting off isolated.
    assert(flowInfo.entrySet[LatticeState::Isolated]);

    // If this block doesn't introduce nonisolation, then we can skip it.
    if (data.nonisolatedUses.empty()) {
      // make sure flow analysis agrees.
      assert(flowInfo.exitSet[LatticeState::Nonisolated] == 0);
      continue;
    }

    // Finally, we must scan the block to determine which isolated uses
    // are illegal. If isolated uses appear after nonisolated ones, then
    // that is an error. So, our strategy is to remove the valid isolated
    // uses, until we find the first nonisolated use. Then, we can simply
    // diagnose the remaining uses.
    SILInstruction *nonisolatedUse = nullptr;
    auto current = block.begin();
    while (current != block.end()) {
      SILInstruction *inst = &*current;

      auto result = data.nonisolatedUses.find(inst);
      if (result != data.nonisolatedUses.end()) {
        nonisolatedUse = *result;
        break;
      }

      for (auto &op : inst->getAllOperands()) {
        data.propertyUses.erase(&op);
      }
      current++;
    }

    assert(nonisolatedUse && "should have found a use!");
    data.diagnoseAll(*this, forDeinit, nonisolatedUse);
  }

  // recursively verify isolation of defer functions.
  for (auto &entry : deferBlocks) {
    // skip those with nonisolated start, since we've already diagnosed those.
    if (entry.second->hasNonisolatedStart())
      continue;

    entry.second->emitDiagnostics();
  }
}

//===----------------------------------------------------------------------===//
//                            MARK: Top Level Code
//===----------------------------------------------------------------------===//

/// If \p arg is not a metatype, just wrap it in a SILValue and return
/// it. Otherwise, we need to go look for the alloc_stack that is storing self.
static SILValue findSelf(const SILArgument *arg) {
  if (!arg->getType().isMetatype())
    return arg;
  // Self will always be defined in the first block. So if we have a metatype,
  // just walk the first block to find the stack, ref, box that contains it.
  for (auto &ii : *arg->getParent()) {
    // TODO: Can we for a non-copyable type use an alloc_box if it is captured?
    if (auto *abi = dyn_cast<AllocStackInst>(&ii)) {
      if (auto *decl = abi->getDecl(); decl && decl->isSelfParameter()) {
        return abi;
      }
      continue;
    }

    if (auto *ari = dyn_cast<AllocRefInst>(&ii)) {
      if (auto *decl = ari->getDecl(); decl && decl->isSelfParameter())
        return ari;
      continue;
    }

    if (auto *abi = dyn_cast<AllocBoxInst>(&ii)) {
      if (auto *decl = abi->getDecl(); decl && decl->isSelfParameter())
        return abi;
      continue;
    }
  }
  return SILValue();
}

/// Performs flow-sensitive actor-isolation checking on the given SILFunction.
static void checkFlowIsolation(SILFunction *fn, RegionAnalysis *ra) {
  assert(fn->hasSelfParam() && "cannot analyze without a self param!");
  LLVM_DEBUG(llvm::dbgs() << "**** CHECKING FLOW ISOLATION: " << fn->getName()
                          << '\n');

  // Step 1 -- Analyze uses of `self` within the function.
  FunctionInfo info(fn, ra);
  info.analyze(findSelf(fn->getSelfArgument()));

  // Step 2 -- Initialize and solve the dataflow problem.
  info.solve();

  LLVM_DEBUG(info.print(llvm::dbgs()));

  // Step 3 -- With the information gathered, check for flow-isolation issues.
  info.emitDiagnostics();
}

/// The FlowIsolation pass performs flow-sensitive actor-isolation checking in
/// the body of actor member functions that treat `self` as `nonisolated` after
/// the first `nonisolated` use. This pass uses a simple forward dataflow
/// analysis to track these changes and emits diagnostics if an isolated use of
/// `self` appears when `self` may be `nonisolated` at that point in the
/// function.
class FlowIsolation : public SILFunctionTransform {

  /// The entry point to the checker.
  void run() override {
    SILFunction *fn = getFunction();

    // Don't rerun diagnostics on deserialized functions.
    if (fn->wasDeserializedCanonical())
      return;

    // Do not run on thunks. We trust those and there isn't a benefit to the
    // user. Any issue there is a compiler bug.
    if (fn->isThunk())
      return;

    // Look for functions that use flow-isolation.
    if (auto *dc = fn->getDeclContext())
      if (auto *afd = dyn_cast_or_null<AbstractFunctionDecl>(dc->getAsDecl()))
        if (usesFlowSensitiveIsolation(afd))
          checkFlowIsolation(fn, getAnalysis<RegionAnalysis>());
  }

}; // class

/// This pass is known to depend on the following passes having run before it:
///   - NoReturnFolding
SILTransform *swift::createFlowIsolation() {
  return new FlowIsolation();
}
