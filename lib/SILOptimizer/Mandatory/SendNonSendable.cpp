//===--- SendNonSendable.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "send-non-sendable"

#include "swift/AST/ASTWalker.h"
#include "swift/AST/Concurrency.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/SIL/BasicBlockData.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/OperandDatastructures.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/Test.h"
#include "swift/SILOptimizer/Analysis/RegionAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/PartitionUtils.h"
#include "swift/SILOptimizer/Utils/VariableNameUtils.h"
#include "swift/Sema/Concurrency.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::PartitionPrimitives;
using namespace swift::PatternMatch;
using namespace swift::regionanalysisimpl;

namespace {

using SendingOperandSetFactory = Partition::SendingOperandSetFactory;
using Element = PartitionPrimitives::Element;
using Region = PartitionPrimitives::Region;

} // namespace

// This option is used so we can test typed errors. Typed errors are a fallback
// case which are emitted when we are unable to infer the name of a value. We in
// most cases do succeed inferring, so it makes sense to add an asserts only
// option that can be used by the compiler to test that we emit these correctly.
static llvm::cl::opt<bool> ForceTypedErrors(
    "sil-regionbasedisolation-force-use-of-typed-errors",
    llvm::cl::desc("Force the usage of typed instead of named errors to make "
                   "it easier to test typed errors"),
    llvm::cl::Hidden);

//===----------------------------------------------------------------------===//
//                              MARK: Utilities
//===----------------------------------------------------------------------===//

static SILValue stripFunctionConversions(SILValue val) {
  while (true) {
    if (auto ti = dyn_cast<ThinToThickFunctionInst>(val)) {
      val = ti->getOperand();
      continue;
    }

    if (auto cfi = dyn_cast<ConvertFunctionInst>(val)) {
      val = cfi->getOperand();
      continue;
    }

    if (auto cvt = dyn_cast<ConvertEscapeToNoEscapeInst>(val)) {
      val = cvt->getOperand();
      continue;
    }

    break;
  }

  return val;
}

static std::optional<DiagnosticBehavior>
getDiagnosticBehaviorLimitForCapturedValue(SILFunction *fn,
                                           CapturedValue value) {
  ValueDecl *decl = value.getDecl();
  auto *ctx = decl->getInnermostDeclContext();
  auto type = fn->mapTypeIntoContext(decl->getInterfaceType());
  return type->getConcurrencyDiagnosticBehaviorLimit(ctx);
}

/// Find the most conservative diagnostic behavior by taking the max over all
/// DiagnosticBehavior for the captured values.
static std::optional<DiagnosticBehavior>
getDiagnosticBehaviorLimitForCapturedValues(
    SILFunction *fn, ArrayRef<CapturedValue> capturedValues) {
  std::optional<DiagnosticBehavior> diagnosticBehavior;
  for (auto value : capturedValues) {
    auto lhs = diagnosticBehavior.value_or(DiagnosticBehavior::Unspecified);
    auto rhs = getDiagnosticBehaviorLimitForCapturedValue(fn, value).value_or(
        DiagnosticBehavior::Unspecified);
    auto result = lhs.merge(rhs);
    if (result != DiagnosticBehavior::Unspecified)
      diagnosticBehavior = result;
  }
  return diagnosticBehavior;
}

static std::optional<SILDeclRef> getDeclRefForCallee(SILInstruction *inst) {
  auto fas = FullApplySite::isa(inst);
  if (!fas)
    return {};

  SILValue calleeOrigin = fas.getCalleeOrigin();

  while (true) {
    // Intentionally don't lookup through dynamic_function_ref and
    // previous_dynamic_function_ref as the target of those functions is not
    // statically known.
    if (auto *fri = dyn_cast<FunctionRefInst>(calleeOrigin)) {
      if (auto *callee = fri->getReferencedFunctionOrNull()) {
        if (auto declRef = callee->getDeclRef())
          return declRef;
      }
    }

    if (auto *mi = dyn_cast<MethodInst>(calleeOrigin)) {
      return mi->getMember();
    }

    if (auto *pai = dyn_cast<PartialApplyInst>(calleeOrigin)) {
      calleeOrigin = pai->getCalleeOrigin();
      continue;
    }

    return {};
  }
}

static std::optional<std::pair<DescriptiveDeclKind, DeclName>>
getSendingApplyCalleeInfo(SILInstruction *inst) {
  auto declRef = getDeclRefForCallee(inst);
  if (!declRef)
    return {};

  auto *decl = declRef->getDecl();
  if (!decl || !decl->hasName())
    return {};

  return {{decl->getDescriptiveKind(), decl->getName()}};
}

static Expr *inferArgumentExprFromApplyExpr(ApplyExpr *sourceApply,
                                            FullApplySite fai,
                                            const Operand *op) {

  Expr *foundExpr = nullptr;

  // If we have self, then infer it.
  if (fai.hasSelfArgument() && op == &fai.getSelfArgumentOperand()) {
    if (auto callExpr = dyn_cast<CallExpr>(sourceApply))
      if (auto calledExpr =
              dyn_cast<DotSyntaxCallExpr>(callExpr->getDirectCallee()))
        foundExpr = calledExpr->getBase();
  } else {
    // Otherwise, try to infer using the operand of the ApplyExpr.
    unsigned argNum = [&]() -> unsigned {
      if (fai.isCalleeOperand(*op))
        return op->getOperandNumber();
      return fai.getAppliedArgIndexWithoutIndirectResults(*op);
    }();

    // Something happened that we do not understand.
    if (argNum >= sourceApply->getArgs()->size()) {
      return nullptr;
    }

    foundExpr = sourceApply->getArgs()->getExpr(argNum);

    // If we have an erasure expression, lets use the original type. We do
    // this since we are not saying the specific parameter that is the
    // issue and we are using the type to explain it to the user.
    if (auto *erasureExpr = dyn_cast<ErasureExpr>(foundExpr))
      foundExpr = erasureExpr->getSubExpr();
  }

  return foundExpr;
}

/// Attempt to infer a name for \p value. Returns none if we fail or if we are
/// asked to force typed errors since we are testing.
static std::optional<Identifier> inferNameHelper(SILValue value) {
  if (ForceTypedErrors)
    return {};
  return VariableNameInferrer::inferName(value);
}

/// Attempt to infer a name and root for \p value. Returns none if we fail or if
/// we are asked to force typed errors since we are testing.
static std::optional<std::pair<Identifier, SILValue>>
inferNameAndRootHelper(SILValue value) {
  if (ForceTypedErrors)
    return {};
  return VariableNameInferrer::inferNameAndRoot(value);
}

//===----------------------------------------------------------------------===//
//                             MARK: Diagnostics
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseError(ASTContext &context, SourceLoc loc,
                                        Diag<T...> diag, U &&...args) {
  return std::move(context.Diags.diagnose(loc, diag, std::forward<U>(args)...)
                       .warnUntilSwiftVersion(6));
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseError(ASTContext &context, SILLocation loc,
                                        Diag<T...> diag, U &&...args) {
  return ::diagnoseError(context, loc.getSourceLoc(), diag,
                         std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseError(const PartitionOp &op, Diag<T...> diag,
                                        U &&...args) {
  return ::diagnoseError(op.getSourceInst()->getFunction()->getASTContext(),
                         op.getSourceLoc().getSourceLoc(), diag,
                         std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseError(const Operand *op, Diag<T...> diag,
                                        U &&...args) {
  return ::diagnoseError(op->getUser()->getFunction()->getASTContext(),
                         op->getUser()->getLoc().getSourceLoc(), diag,
                         std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseError(const SILInstruction *inst,
                                        Diag<T...> diag, U &&...args) {
  return ::diagnoseError(inst->getFunction()->getASTContext(),
                         inst->getLoc().getSourceLoc(), diag,
                         std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseNote(ASTContext &context, SourceLoc loc,
                                       Diag<T...> diag, U &&...args) {
  return context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseNote(ASTContext &context, SILLocation loc,
                                       Diag<T...> diag, U &&...args) {
  return ::diagnoseNote(context, loc.getSourceLoc(), diag,
                        std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseNote(const PartitionOp &op, Diag<T...> diag,
                                       U &&...args) {
  return ::diagnoseNote(op.getSourceInst()->getFunction()->getASTContext(),
                        op.getSourceLoc().getSourceLoc(), diag,
                        std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseNote(const Operand *op, Diag<T...> diag,
                                       U &&...args) {
  return ::diagnoseNote(op->getUser()->getFunction()->getASTContext(),
                        op->getUser()->getLoc().getSourceLoc(), diag,
                        std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseNote(const SILInstruction *inst,
                                       Diag<T...> diag, U &&...args) {
  return ::diagnoseNote(inst->getFunction()->getASTContext(),
                        inst->getLoc().getSourceLoc(), diag,
                        std::forward<U>(args)...);
}

//===----------------------------------------------------------------------===//
//                           MARK: Require Liveness
//===----------------------------------------------------------------------===//

namespace {

class BlockLivenessInfo {
  // Generation counter so we do not need to reallocate.
  unsigned generation = 0;
  SILInstruction *firstRequireInst = nullptr;

  void resetIfNew(unsigned newGeneration) {
    if (generation == newGeneration)
      return;
    generation = newGeneration;
    firstRequireInst = nullptr;
  }

public:
  SILInstruction *getInst(unsigned callerGeneration) {
    resetIfNew(callerGeneration);
    return firstRequireInst;
  }

  void setInst(unsigned callerGeneration, SILInstruction *newValue) {
    resetIfNew(callerGeneration);
    firstRequireInst = newValue;
  }
};

/// We only want to emit errors for the first requires along a path from a
/// sending instruction. We discover this by walking from user blocks to
struct RequireLiveness {
  unsigned generation;
  SILInstruction *sendingInst;
  BasicBlockData<BlockLivenessInfo> &blockLivenessInfo;
  InstructionSet allRequires;
  InstructionSetWithSize finalRequires;

  /// If we have requires in the def block before our send, this is the
  /// first require.
  SILInstruction *firstRequireBeforeSendInDefBlock = nullptr;

  RequireLiveness(unsigned generation, Operand *sendingOp,
                  BasicBlockData<BlockLivenessInfo> &blockLivenessInfo)
      : generation(generation), sendingInst(sendingOp->getUser()),
        blockLivenessInfo(blockLivenessInfo),
        allRequires(sendingOp->getParentFunction()),
        finalRequires(sendingOp->getParentFunction()) {}

  template <typename Collection>
  void process(Collection collection);

  /// Attempt to process requireInst for our def block. Returns false if
  /// requireInst was before our def and we need to do interprocedural
  /// processing. Returns true if requireInst was after our seningInst and we
  /// were able to appropriately determine if we should emit it or not.
  void processDefBlock();

  /// Process all requires in block, updating blockLivenessInfo.
  void processNonDefBlock(SILBasicBlock *block);
};

} // namespace

void RequireLiveness::processDefBlock() {
  REGIONBASEDISOLATION_LOG(llvm::dbgs() << "    Processing def block!\n");
  // First walk from the beginning of the block to the send instruction to
  // see if we have any requires before our def. Once we find one, we can skip
  // the traversal and jump straight to the send.
  for (auto ii = sendingInst->getParent()->begin(),
            ie = sendingInst->getIterator();
       ii != ie; ++ii) {
    if (allRequires.contains(&*ii) && !firstRequireBeforeSendInDefBlock) {
      firstRequireBeforeSendInDefBlock = &*ii;
      REGIONBASEDISOLATION_LOG(llvm::dbgs()
                               << "        Found send before def: "
                               << *firstRequireBeforeSendInDefBlock);
      break;
    }
  }

  // Then walk from our sendingInst to the end of the block looking for the
  // first require inst. Once we find it... return.
  //
  // NOTE: We start walking at the sendingInst since the sendingInst could use
  // the requireInst as well.
  for (auto ii = sendingInst->getIterator(),
            ie = sendingInst->getParent()->end();
       ii != ie; ++ii) {
    if (!allRequires.contains(&*ii))
      continue;

    finalRequires.insert(&*ii);
    REGIONBASEDISOLATION_LOG(llvm::dbgs()
                             << "        Found send after def: " << *ii);
    return;
  }
}

void RequireLiveness::processNonDefBlock(SILBasicBlock *block) {
  // Walk from the bottom to the top... assigning to our block state.
  auto blockState = blockLivenessInfo.get(block);
  for (auto &inst : llvm::make_range(block->rbegin(), block->rend())) {
    if (!finalRequires.contains(&inst))
      continue;
    blockState.get()->setInst(generation, &inst);
  }
}

template <typename Collection>
void RequireLiveness::process(Collection requireInstList) {
  REGIONBASEDISOLATION_LOG(
      llvm::dbgs() << "==> Performing Require Liveness for: " << *sendingInst);

  // Then put all of our requires into our allRequires set.
  BasicBlockWorklist initializingWorklist(sendingInst->getFunction());
  for (auto require : requireInstList) {
    REGIONBASEDISOLATION_LOG(llvm::dbgs()
                             << "        Require Inst: " << **require);
    allRequires.insert(*require);
    initializingWorklist.pushIfNotVisited(require->getParent());
  }

  // Then process our def block to see if we have any requires before and after
  // the sendingInst...
  processDefBlock();

  // If we found /any/ requries after the sendingInst, we can bail early since
  // that is guaranteed to dominate all further requires.
  if (!finalRequires.empty()) {
    REGIONBASEDISOLATION_LOG(
        llvm::dbgs()
        << "        Found send after def in def block! Exiting early!\n");
    return;
  }

  REGIONBASEDISOLATION_LOG(llvm::dbgs()
                           << "        Did not find send after def in def "
                              "block! Walking blocks!\n");

  // If we found a send in the def block before our def, add it to the block
  // state for the def.
  if (firstRequireBeforeSendInDefBlock) {
    REGIONBASEDISOLATION_LOG(
        llvm::dbgs()
        << "        Found a require before send! Adding to block state!\n");
    auto blockState = blockLivenessInfo.get(sendingInst->getParent());
    blockState.get()->setInst(generation, firstRequireBeforeSendInDefBlock);
  }

  // Then for each require block that isn't a def block send, find the
  // earliest send inst.
  while (auto *requireBlock = initializingWorklist.pop()) {
    auto blockState = blockLivenessInfo.get(requireBlock);
    for (auto &inst : *requireBlock) {
      if (!allRequires.contains(&inst))
        continue;
      REGIONBASEDISOLATION_LOG(llvm::dbgs() << "        Mapping Block bb"
                                            << requireBlock->getDebugID()
                                            << " to: " << inst);
      blockState.get()->setInst(generation, &inst);
      break;
    }
  }

  // Then walk from our def block looking for setInst blocks.
  auto *sendingBlock = sendingInst->getParent();
  BasicBlockWorklist worklist(sendingInst->getFunction());
  for (auto *succBlock : sendingBlock->getSuccessorBlocks())
    worklist.pushIfNotVisited(succBlock);

  while (auto *next = worklist.pop()) {
    // Check if we found an earliest requires... if so, add that to final
    // requires and continue. We don't want to visit successors.
    auto blockState = blockLivenessInfo.get(next);
    if (auto *inst = blockState.get()->getInst(generation)) {
      finalRequires.insert(inst);
      continue;
    }

    // Do not look at successors of the sending block.
    if (next == sendingBlock)
      continue;

    // Otherwise, we did not find a requires and need to search further
    // successors.
    for (auto *succBlock : next->getSuccessorBlocks())
      worklist.pushIfNotVisited(succBlock);
  }
}

//===----------------------------------------------------------------------===//
//            MARK: Forward Declaration Of SendNonSendableImpl
//===----------------------------------------------------------------------===//

namespace {

/// Wrapper around a SILInstruction that internally specifies whether we are
/// dealing with an inout reinitialization needed or if it is just a normal
/// use after send.
class RequireInst {
public:
  enum Kind {
    UseAfterSend,
    InOutReinitializationNeeded,
  };

private:
  llvm::PointerIntPair<SILInstruction *, 1> instAndKind;

  RequireInst(SILInstruction *inst, Kind kind) : instAndKind(inst, kind) {}

public:
  static RequireInst forUseAfterSend(SILInstruction *inst) {
    return {inst, Kind::UseAfterSend};
  }

  static RequireInst forInOutReinitializationNeeded(SILInstruction *inst) {
    return {inst, Kind::InOutReinitializationNeeded};
  }

  SILInstruction *getInst() const { return instAndKind.getPointer(); }
  Kind getKind() const { return Kind(instAndKind.getInt()); }

  SILInstruction *operator*() const { return getInst(); }
  SILInstruction *operator->() const { return getInst(); }
};

class SendNonSendableImpl {
  RegionAnalysisFunctionInfo *info;
  SmallFrozenMultiMap<Operand *, RequireInst, 8> sendingOpToRequireInstMultiMap;
  SmallVector<PartitionOpError, 8> foundVerbatimErrors;

public:
  SendNonSendableImpl(RegionAnalysisFunctionInfo *info) : info(info) {}

  void emitDiagnostics();

private:
  void runDiagnosticEvaluator();

  void emitUseAfterSendDiagnostics();
  void emitVerbatimErrors();
};

} // namespace

//===----------------------------------------------------------------------===//
//                MARK: UseAfterSend Diagnostic Inference
//===----------------------------------------------------------------------===//

namespace {

class UseAfterSendDiagnosticEmitter {
  Operand *sendingOp;
  SmallVectorImpl<RequireInst> &requireInsts;
  bool emittedErrorDiagnostic = false;

public:
  UseAfterSendDiagnosticEmitter(Operand *sendingOp,
                                SmallVectorImpl<RequireInst> &requireInsts)
      : sendingOp(sendingOp), requireInsts(requireInsts) {}

  ~UseAfterSendDiagnosticEmitter() {
    // If we were supposed to emit a diagnostic and didn't emit an unknown
    // pattern error.
    if (!emittedErrorDiagnostic)
      emitUnknownPatternError();
  }

  SILFunction *getFunction() const { return sendingOp->getFunction(); }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    return sendingOp->get()->getType().getConcurrencyDiagnosticBehavior(
        getFunction());
  }

  /// If we can find a callee decl name, return that. None otherwise.
  std::optional<std::pair<DescriptiveDeclKind, DeclName>>
  getSendingCalleeInfo() const {
    return getSendingApplyCalleeInfo(sendingOp->getUser());
  }

  void
  emitNamedIsolationCrossingError(SILLocation loc, Identifier name,
                                  SILIsolationInfo namedValuesIsolationInfo,
                                  ApplyIsolationCrossing isolationCrossing) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());

    // Then emit the note with greater context.
    SmallString<64> descriptiveKindStr;
    {
      if (!namedValuesIsolationInfo.isDisconnected()) {
        llvm::raw_svector_ostream os(descriptiveKindStr);
        namedValuesIsolationInfo.printForDiagnostics(os);
        os << ' ';
      }
    }

    if (auto calleeInfo = getSendingCalleeInfo()) {
      diagnoseNote(
          loc, diag::regionbasedisolation_named_info_send_yields_race_callee,
          name, descriptiveKindStr, isolationCrossing.getCalleeIsolation(),
          calleeInfo->first, calleeInfo->second,
          isolationCrossing.getCallerIsolation());
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_named_info_send_yields_race,
                   name, descriptiveKindStr,
                   isolationCrossing.getCalleeIsolation(),
                   isolationCrossing.getCallerIsolation());
    }
    emitRequireInstDiagnostics();
  }

  void
  emitNamedIsolationCrossingError(SILLocation loc, Identifier name,
                                  SILIsolationInfo namedValuesIsolationInfo,
                                  ApplyIsolationCrossing isolationCrossing,
                                  DeclName calleeDeclName,
                                  DescriptiveDeclKind calleeDeclKind) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());

    // Then emit the note with greater context.
    SmallString<64> descriptiveKindStr;
    {
      if (!namedValuesIsolationInfo.isDisconnected()) {
        llvm::raw_svector_ostream os(descriptiveKindStr);
        namedValuesIsolationInfo.printForDiagnostics(os);
        os << ' ';
      }
    }

    diagnoseNote(
        loc, diag::regionbasedisolation_named_info_send_yields_race_callee,
        name, descriptiveKindStr, isolationCrossing.getCalleeIsolation(),
        calleeDeclKind, calleeDeclName, isolationCrossing.getCallerIsolation());
    emitRequireInstDiagnostics();
  }

  void emitNamedAsyncLetNoIsolationCrossingError(SILLocation loc,
                                                 Identifier name) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());

    diagnoseNote(
        loc, diag::regionbasedisolation_named_nonisolated_asynclet_name, name);
    emitRequireInstDiagnostics();
  }

  void emitTypedIsolationCrossing(SILLocation loc, Type inferredType,
                                  ApplyIsolationCrossing isolationCrossing) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());

    if (auto calleeInfo = getSendingCalleeInfo()) {
      diagnoseNote(loc, diag::regionbasedisolation_type_use_after_send_callee,
                   inferredType, isolationCrossing.getCalleeIsolation(),
                   calleeInfo->first, calleeInfo->second,
                   isolationCrossing.getCallerIsolation());
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_type_use_after_send,
                   inferredType, isolationCrossing.getCalleeIsolation(),
                   isolationCrossing.getCallerIsolation());
    }
    emitRequireInstDiagnostics();
  }

  void emitNamedUseofStronglySentValue(SILLocation loc, Identifier name) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());

    // Then emit the note with greater context.
    diagnoseNote(
        loc, diag::regionbasedisolation_named_value_used_after_explicit_sending,
        name)
        .highlight(loc.getSourceRange());

    // Finally the require points.
    emitRequireInstDiagnostics();
  }

  void emitTypedUseOfStronglySentValue(SILLocation loc, Type inferredType) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());
    if (auto calleeInfo = getSendingCalleeInfo()) {
      diagnoseNote(loc,
                   diag::regionbasedisolation_typed_use_after_sending_callee,
                   inferredType, calleeInfo->first, calleeInfo->second);
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_typed_use_after_sending,
                   inferredType);
    }
    emitRequireInstDiagnostics();
  }

  void emitNamedIsolationCrossingDueToCapture(
      SILLocation loc, Identifier name,
      SILIsolationInfo namedValuesIsolationInfo,
      ApplyIsolationCrossing isolationCrossing) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());

    SmallString<64> descriptiveKindStr;
    {
      if (!namedValuesIsolationInfo.isDisconnected()) {
        llvm::raw_svector_ostream os(descriptiveKindStr);
        namedValuesIsolationInfo.printForDiagnostics(os);
        os << ' ';
      }
    }

    diagnoseNote(
        loc, diag::regionbasedisolation_named_isolated_closure_yields_race,
        descriptiveKindStr, name, isolationCrossing.getCalleeIsolation(),
        isolationCrossing.getCallerIsolation())
        .highlight(loc.getSourceRange());
    emitRequireInstDiagnostics();
  }

  void emitTypedIsolationCrossingDueToCapture(
      SILLocation loc, Type inferredType,
      ApplyIsolationCrossing isolationCrossing) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());
    diagnoseNote(loc,
                 diag::regionbasedisolation_type_isolated_capture_yields_race,
                 inferredType, isolationCrossing.getCalleeIsolation(),
                 isolationCrossing.getCallerIsolation());
    emitRequireInstDiagnostics();
  }

  void emitUnknownPatternError() {
    if (shouldAbortOnUnknownPatternMatchError()) {
      llvm::report_fatal_error(
          "RegionIsolation: Aborting on unknown pattern match error");
    }

    diagnoseError(sendingOp->getUser(),
                  diag::regionbasedisolation_unknown_pattern)
        .limitBehaviorIf(getBehaviorLimit());
  }

private:
  ASTContext &getASTContext() const {
    return sendingOp->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilSwiftVersion(6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  void emitRequireInstDiagnostics() {
    // Now actually emit the require notes.
    while (!requireInsts.empty()) {
      auto require = requireInsts.pop_back_val();
      switch (require.getKind()) {
      case RequireInst::UseAfterSend:
        diagnoseNote(*require, diag::regionbasedisolation_maybe_race)
            .highlight(require->getLoc().getSourceRange());
        continue;
      case RequireInst::InOutReinitializationNeeded:
        diagnoseNote(
            *require,
            diag::regionbasedisolation_inout_sending_must_be_reinitialized)
            .highlight(require->getLoc().getSourceRange());
        continue;
      }
      llvm_unreachable("Covered switch isn't covered?!");
    }
  }
};

class UseAfterSendDiagnosticInferrer {
  Operand *sendingOp;
  UseAfterSendDiagnosticEmitter diagnosticEmitter;
  RegionAnalysisValueMap &valueMap;
  SendingOperandToStateMap &sendingOpToStateMap;
  SILLocation baseLoc = SILLocation::invalid();
  Type baseInferredType;

  struct AutoClosureWalker;

public:
  UseAfterSendDiagnosticInferrer(Operand *sendOp,
                                 SmallVectorImpl<RequireInst> &requireInsts,
                                 RegionAnalysisValueMap &valueMap,
                                 SendingOperandToStateMap &sendingOpToStateMap)
      : sendingOp(sendOp), diagnosticEmitter(sendOp, requireInsts),
        valueMap(valueMap), sendingOpToStateMap(sendingOpToStateMap),
        baseLoc(sendOp->getUser()->getLoc()),
        baseInferredType(sendOp->get()->getType().getASTType()) {}
  void infer();

  Operand *getSendingOperand() const { return sendingOp; }

private:
  bool initForIsolatedPartialApply(Operand *op, AbstractClosureExpr *ace);

  void initForApply(Operand *op, ApplyExpr *expr);
  void initForAutoclosure(Operand *op, AutoClosureExpr *expr);
};

} // namespace

bool UseAfterSendDiagnosticInferrer::initForIsolatedPartialApply(
    Operand *op, AbstractClosureExpr *ace) {
  SmallVector<std::tuple<CapturedValue, unsigned, ApplyIsolationCrossing>, 8>
      foundCapturedIsolationCrossing;
  ace->getIsolationCrossing(foundCapturedIsolationCrossing);
  if (foundCapturedIsolationCrossing.empty())
    return false;

  unsigned opIndex = ApplySite(op->getUser()).getASTAppliedArgIndex(*op);
  bool emittedDiagnostic = false;
  for (auto &p : foundCapturedIsolationCrossing) {
    if (std::get<1>(p) != opIndex)
      continue;
    emittedDiagnostic = true;

    auto &state = sendingOpToStateMap.get(sendingOp);
    if (auto rootValueAndName = inferNameAndRootHelper(sendingOp->get())) {
      diagnosticEmitter.emitNamedIsolationCrossingDueToCapture(
          RegularLocation(std::get<0>(p).getLoc()), rootValueAndName->first,
          state.isolationInfo.getIsolationInfo(), std::get<2>(p));
      continue;
    }

    diagnosticEmitter.emitTypedIsolationCrossingDueToCapture(
        RegularLocation(std::get<0>(p).getLoc()), baseInferredType,
        std::get<2>(p));
  }

  return emittedDiagnostic;
}

void UseAfterSendDiagnosticInferrer::initForApply(Operand *op,
                                                  ApplyExpr *sourceApply) {
  auto isolationCrossing = sourceApply->getIsolationCrossing().value();

  // Grab out full apply site and see if we can find a better expr.
  SILInstruction *i = const_cast<SILInstruction *>(op->getUser());
  auto fai = FullApplySite::isa(i);

  assert(!fai.getArgumentConvention(*op).isIndirectOutParameter() &&
         "An indirect out parameter is never sent");
  auto *foundExpr = inferArgumentExprFromApplyExpr(sourceApply, fai, op);

  auto inferredArgType =
      foundExpr ? foundExpr->findOriginalType() : baseInferredType;
  diagnosticEmitter.emitTypedIsolationCrossing(baseLoc, inferredArgType,
                                               isolationCrossing);
}

/// This walker visits an AutoClosureExpr and looks for uses of a specific
/// captured value. We want to error on the uses in the autoclosure.
struct UseAfterSendDiagnosticInferrer::AutoClosureWalker : ASTWalker {
  UseAfterSendDiagnosticInferrer &foundTypeInfo;
  ValueDecl *targetDecl;
  SILIsolationInfo targetDeclIsolationInfo;
  SmallPtrSet<Expr *, 8> visitedCallExprDeclRefExprs;

  AutoClosureWalker(UseAfterSendDiagnosticInferrer &foundTypeInfo,
                    ValueDecl *targetDecl,
                    SILIsolationInfo targetDeclIsolationInfo)
      : foundTypeInfo(foundTypeInfo), targetDecl(targetDecl),
        targetDeclIsolationInfo(targetDeclIsolationInfo) {}

  Expr *lookThroughArgExpr(Expr *expr) {
    while (true) {
      if (auto *memberRefExpr = dyn_cast<MemberRefExpr>(expr)) {
        expr = memberRefExpr->getBase();
        continue;
      }

      if (auto *cvt = dyn_cast<ImplicitConversionExpr>(expr)) {
        expr = cvt->getSubExpr();
        continue;
      }

      if (auto *e = dyn_cast<ForceValueExpr>(expr)) {
        expr = e->getSubExpr();
        continue;
      }

      if (auto *t = dyn_cast<TupleElementExpr>(expr)) {
        expr = t->getBase();
        continue;
      }

      return expr;
    }
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
    if (auto *declRef = dyn_cast<DeclRefExpr>(expr)) {
      // If this decl ref expr was not visited as part of a callExpr and is our
      // target decl... emit a simple async let error.
      if (!visitedCallExprDeclRefExprs.count(declRef)) {
        if (declRef->getDecl() == targetDecl) {
          foundTypeInfo.diagnosticEmitter
              .emitNamedAsyncLetNoIsolationCrossingError(
                  foundTypeInfo.baseLoc, targetDecl->getBaseIdentifier());
          return Action::Continue(expr);
        }
      }
    }

    // If we have a call expr, see if any of its arguments will cause our sent
    // value to be sent into another isolation domain.
    if (auto *callExpr = dyn_cast<CallExpr>(expr)) {
      // Search callExpr's arguments to see if we have our targetDecl.
      auto *argList = callExpr->getArgs();
      for (auto pair : llvm::enumerate(argList->getArgExprs())) {
        auto *arg = lookThroughArgExpr(pair.value());
        auto *declRef = dyn_cast<DeclRefExpr>(arg);
        if (!declRef)
          continue;

        if (declRef->getDecl() != targetDecl)
          continue;

        // Found our target!
        visitedCallExprDeclRefExprs.insert(declRef);

        auto isolationCrossing = callExpr->getIsolationCrossing();

        // If we do not have an isolation crossing, then we must be just sending
        // a value in a nonisolated fashion into an async let. So emit the
        // simple async let error.
        if (!isolationCrossing) {
          foundTypeInfo.diagnosticEmitter
              .emitNamedAsyncLetNoIsolationCrossingError(
                  foundTypeInfo.baseLoc, targetDecl->getBaseIdentifier());
          continue;
        }

        // Otherwise, we are calling an actor isolated function in the async
        // let. Emit a better error.

        // See if we can find a valueDecl/name for our callee so we can
        // emit a nicer error.
        ConcreteDeclRef concreteDecl =
            callExpr->getDirectCallee()->getReferencedDecl();

        // If we do not find a direct one, see if we are calling a method
        // on a nominal type.
        if (!concreteDecl) {
          if (auto *dot =
                  dyn_cast<DotSyntaxCallExpr>(callExpr->getDirectCallee())) {
            concreteDecl = dot->getSemanticFn()->getReferencedDecl();
          }
        }

        if (!concreteDecl)
          continue;

        auto *valueDecl = concreteDecl.getDecl();
        assert(valueDecl && "Should be non-null if concreteDecl is valid");

        if (auto isolationCrossing = callExpr->getIsolationCrossing()) {
          // If we have an isolation crossing, use that information.
          if (valueDecl->hasName()) {
            foundTypeInfo.diagnosticEmitter.emitNamedIsolationCrossingError(
                foundTypeInfo.baseLoc, targetDecl->getBaseIdentifier(),
                targetDeclIsolationInfo, *isolationCrossing,
                valueDecl->getName(), valueDecl->getDescriptiveKind());
            continue;
          }

          // Otherwise default back to the "callee" error.
          foundTypeInfo.diagnosticEmitter.emitNamedIsolationCrossingError(
              foundTypeInfo.baseLoc, targetDecl->getBaseIdentifier(),
              targetDeclIsolationInfo, *isolationCrossing);
          continue;
        }
      }
    }

    return Action::Continue(expr);
  }
};

void UseAfterSendDiagnosticInferrer::infer() {
  // Otherwise, see if our operand's instruction is a sending parameter.
  if (auto fas = FullApplySite::isa(sendingOp->getUser())) {
    assert(!fas.getArgumentConvention(*sendingOp).isIndirectOutParameter() &&
           "We should never send an indirect out parameter");
    if (fas.getArgumentParameterInfo(*sendingOp)
            .hasOption(SILParameterInfo::Sending)) {

      // First try to do the named diagnostic if we can find a name.
      if (auto rootValueAndName = inferNameAndRootHelper(sendingOp->get())) {
        return diagnosticEmitter.emitNamedUseofStronglySentValue(
            baseLoc, rootValueAndName->first);
      }

      // See if we have an ApplyExpr and if we can infer a better type.
      Type type = baseInferredType;
      if (auto *applyExpr =
              sendingOp->getUser()->getLoc().getAsASTNode<ApplyExpr>()) {
        if (auto *foundExpr =
                inferArgumentExprFromApplyExpr(applyExpr, fas, sendingOp))
          type = foundExpr->findOriginalType();
      }

      // Otherwise, emit the typed diagnostic.
      return diagnosticEmitter.emitTypedUseOfStronglySentValue(baseLoc, type);
    }
  }

  auto loc = sendingOp->getUser()->getLoc();

  // If we have a partial_apply that is actor isolated, see if we found a
  // send error due to us sending a value into the partial apply.
  if (auto *ace = loc.getAsASTNode<AbstractClosureExpr>()) {
    if (ace->getActorIsolation().isActorIsolated()) {
      if (initForIsolatedPartialApply(sendingOp, ace)) {
        return;
      }
    }
  }

  if (auto *sourceApply = loc.getAsASTNode<ApplyExpr>()) {
    // Before we do anything further, see if we can find a name and emit a name
    // error.
    if (auto rootValueAndName = inferNameAndRootHelper(sendingOp->get())) {
      auto &state = sendingOpToStateMap.get(sendingOp);
      return diagnosticEmitter.emitNamedIsolationCrossingError(
          baseLoc, rootValueAndName->first,
          state.isolationInfo.getIsolationInfo(),
          *sourceApply->getIsolationCrossing());
    }

    // Otherwise, try to infer from the ApplyExpr.
    return initForApply(sendingOp, sourceApply);
  }

  if (auto fas = FullApplySite::isa(sendingOp->getUser())) {
    if (auto isolationCrossing = fas.getIsolationCrossing()) {
      return diagnosticEmitter.emitTypedIsolationCrossing(
          baseLoc, baseInferredType, *isolationCrossing);
    }
  }

  auto *autoClosureExpr = loc.getAsASTNode<AutoClosureExpr>();
  if (!autoClosureExpr) {
    return diagnosticEmitter.emitUnknownPatternError();
  }

  auto *i = sendingOp->getUser();
  auto pai = ApplySite::isa(i);
  unsigned captureIndex = pai.getASTAppliedArgIndex(*sendingOp);

  auto &state = sendingOpToStateMap.get(sendingOp);
  auto captureInfo =
      autoClosureExpr->getCaptureInfo().getCaptures()[captureIndex];
  auto *captureDecl = captureInfo.getDecl();
  AutoClosureWalker walker(*this, captureDecl,
                           state.isolationInfo.getIsolationInfo());
  autoClosureExpr->walk(walker);
}

// Top level entrypoint for use after send diagnostics.
void SendNonSendableImpl::emitUseAfterSendDiagnostics() {
  auto *function = info->getFunction();
  BasicBlockData<BlockLivenessInfo> blockLivenessInfo(function);
  // We use a generation counter so we can lazily reset blockLivenessInfo
  // since we cannot clear it without iterating over it.
  unsigned blockLivenessInfoGeneration = 0;

  if (sendingOpToRequireInstMultiMap.empty())
    return;

  REGIONBASEDISOLATION_LOG(
      llvm::dbgs() << "Emitting Error. Kind: Use After Send diagnostics.\n");

  for (auto [sendingOp, requireInsts] :
       sendingOpToRequireInstMultiMap.getRange()) {
    REGIONBASEDISOLATION_LOG(
        llvm::dbgs() << "Sending Op. Number: " << sendingOp->getOperandNumber()
                     << ". User: " << *sendingOp->getUser());

    // Then look for our requires before we emit any error. We want to emit a
    // single we don't understand error if we do not find the require.
    bool didEmitRequireNote = false;
    InstructionSet requireInstsUnique(function);
    RequireLiveness liveness(blockLivenessInfoGeneration, sendingOp,
                             blockLivenessInfo);
    ++blockLivenessInfoGeneration;
    liveness.process(requireInsts);

    SmallVector<RequireInst, 8> requireInstsForError;
    for (auto require : requireInsts) {
      // We can have multiple of the same require insts if we had a require
      // and an assign from the same instruction. Our liveness checking
      // above doesn't care about that, but we still need to make sure we do
      // not emit twice.
      if (!requireInstsUnique.insert(*require))
        continue;

      // If this was not a last require, do not emit an error.
      if (!liveness.finalRequires.contains(*require))
        continue;

      requireInstsForError.push_back(require);
      didEmitRequireNote = true;
    }

    // If we did not emit a require, emit an "unknown pattern" error that
    // tells the user to file a bug. This importantly ensures that we can
    // guarantee that we always find the require if we successfully compile.
    if (!didEmitRequireNote) {
      if (shouldAbortOnUnknownPatternMatchError()) {
        llvm::report_fatal_error(
            "RegionIsolation: Aborting on unknown pattern match error");
      }

      diagnoseError(sendingOp, diag::regionbasedisolation_unknown_pattern);
      continue;
    }

    UseAfterSendDiagnosticInferrer diagnosticInferrer(
        sendingOp, requireInstsForError, info->getValueMap(),
        info->getSendingOperandToStateMap());
    diagnosticInferrer.infer();
  }
}

//===----------------------------------------------------------------------===//
//            MARK: Send Never-Sent Diagnostic Inference
//===----------------------------------------------------------------------===//

namespace {

class SendNeverSentDiagnosticEmitter {
  /// The use that actually caused the send.
  Operand *sendingOperand;

  /// The never-sent value that is in the same region as \p
  /// sendingOperand.get().
  llvm::PointerUnion<SILValue, SILInstruction *> neverSent;

  /// The region info that describes the dynamic dataflow derived isolation
  /// region info for the never-sent value.
  ///
  /// This is equal to the merge of the IsolationRegionInfo from all elements in
  /// the never-sent value's region when the error was diagnosed.
  SILDynamicMergedIsolationInfo isolationRegionInfo;

  bool emittedErrorDiagnostic = false;

public:
  SendNeverSentDiagnosticEmitter(
      Operand *sendingOperand,
      llvm::PointerUnion<SILValue, SILInstruction *> neverSent,
      SILDynamicMergedIsolationInfo isolationRegionInfo)
      : sendingOperand(sendingOperand), neverSent(neverSent),
        isolationRegionInfo(isolationRegionInfo) {}

  ~SendNeverSentDiagnosticEmitter() {
    if (!emittedErrorDiagnostic) {
      emitUnknownPatternError();
    }
  }

  Operand *getOperand() const { return sendingOperand; }

  SILFunction *getFunction() const { return getOperand()->getFunction(); }

  SILValue getNeverSentValue() const { return neverSent.dyn_cast<SILValue>(); }

  SILInstruction *getNeverSentActorIntroducingInst() const {
    return neverSent.dyn_cast<SILInstruction *>();
  }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    return sendingOperand->get()->getType().getConcurrencyDiagnosticBehavior(
        getOperand()->getFunction());
  }

  /// If we can find a callee decl name, return that. None otherwise.
  std::optional<std::pair<DescriptiveDeclKind, DeclName>>
  getSendingCalleeInfo() const {
    return getSendingApplyCalleeInfo(sendingOperand->getUser());
  }

  SILLocation getLoc() const { return sendingOperand->getUser()->getLoc(); }

  /// Return the isolation region info for \p getNeverSentValue().
  SILDynamicMergedIsolationInfo getIsolationRegionInfo() const {
    return isolationRegionInfo;
  }

  void emitUnknownPatternError() {
    if (shouldAbortOnUnknownPatternMatchError()) {
      llvm::report_fatal_error(
          "RegionIsolation: Aborting on unknown pattern match error");
    }

    diagnoseError(getOperand()->getUser(),
                  diag::regionbasedisolation_unknown_pattern)
        .limitBehaviorIf(getBehaviorLimit());
  }

  void emitUnknownUse(SILLocation loc) {
    // TODO: This will eventually be an unknown pattern error.
    diagnoseError(loc, diag::regionbasedisolation_task_or_actor_isolated_sent)
        .limitBehaviorIf(getBehaviorLimit());
  }

  void emitPassToApply(SILLocation loc, Type inferredType,
                       ApplyIsolationCrossing crossing) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());

    SmallString<64> descriptiveKindStr;
    {
      llvm::raw_svector_ostream os(descriptiveKindStr);
      getIsolationRegionInfo().printForDiagnostics(os);
    }

    if (auto calleeInfo = getSendingCalleeInfo()) {
      diagnoseNote(
          loc,
          diag::regionbasedisolation_typed_sendneversendable_via_arg_callee,
          descriptiveKindStr, inferredType, crossing.getCalleeIsolation(),
          calleeInfo->first, calleeInfo->second);
    } else {
      diagnoseNote(
          loc, diag::regionbasedisolation_typed_sendneversendable_via_arg,
          descriptiveKindStr, inferredType, crossing.getCalleeIsolation());
    }
  }

  void emitNamedFunctionArgumentClosure(SILLocation loc, Identifier name,
                                        ApplyIsolationCrossing crossing) {
    emitNamedOnlyError(loc, name);
    SmallString<64> descriptiveKindStr;
    {
      if (!getIsolationRegionInfo().isDisconnected()) {
        llvm::raw_svector_ostream os(descriptiveKindStr);
        getIsolationRegionInfo().printForDiagnostics(os);
        os << ' ';
      }
    }
    diagnoseNote(loc,
                 diag::regionbasedisolation_named_isolated_closure_yields_race,
                 descriptiveKindStr, name, crossing.getCalleeIsolation(),
                 crossing.getCallerIsolation())
        .highlight(loc.getSourceRange());
  }

  void emitTypedSendingNeverSendableToSendingParam(SILLocation loc,
                                                   Type inferredType) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());

    SmallString<64> descriptiveKindStr;
    {
      llvm::raw_svector_ostream os(descriptiveKindStr);
      getIsolationRegionInfo().printForDiagnostics(os);
    }

    if (auto calleeInfo = getSendingCalleeInfo()) {
      diagnoseNote(
          loc, diag::regionbasedisolation_typed_tns_passed_to_sending_callee,
          descriptiveKindStr, inferredType, calleeInfo->first,
          calleeInfo->second);
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_typed_tns_passed_to_sending,
                   descriptiveKindStr, inferredType);
    }
  }

  /// Only use if we were able to find the actual isolated value.
  void emitTypedSendingNeverSendableToSendingClosureParamDirectlyIsolated(
      SILLocation loc, CapturedValue capturedValue) {
    SmallString<64> descriptiveKindStr;
    {
      llvm::raw_svector_ostream os(descriptiveKindStr);
      if (getIsolationRegionInfo().getIsolationInfo().isTaskIsolated()) {
        os << "code in the current task";
      } else {
        getIsolationRegionInfo().printForDiagnostics(os);
        os << " code";
      }
    }

    diagnoseError(loc,
                  diag::regionbasedisolation_typed_tns_passed_sending_closure,
                  descriptiveKindStr)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(getDiagnosticBehaviorLimitForCapturedValue(
            getFunction(), capturedValue));

    auto capturedLoc = RegularLocation(capturedValue.getLoc());
    if (getIsolationRegionInfo().getIsolationInfo().isTaskIsolated()) {
      // If we have a closure capture box, emit a special diagnostic.
      if (auto *fArg = dyn_cast<SILFunctionArgument>(
              getIsolationRegionInfo().getIsolationInfo().getIsolatedValue())) {
        if (fArg->isClosureCapture() && fArg->getType().is<SILBoxType>()) {
          auto diag = diag::
              regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_boxed_value_task_isolated;
          auto *decl = capturedValue.getDecl();
          diagnoseNote(capturedLoc, diag, decl->getName(),
                       decl->getDescriptiveKind());
          return;
        }
      }

      auto diag = diag::
          regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_value_task_isolated;
      diagnoseNote(capturedLoc, diag, capturedValue.getDecl()->getName());
      return;
    }

    descriptiveKindStr.clear();
    {
      llvm::raw_svector_ostream os(descriptiveKindStr);
      getIsolationRegionInfo().printForDiagnostics(os);
    }

    auto diag = diag::
        regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_value;
    diagnoseNote(capturedLoc, diag, descriptiveKindStr,
                 capturedValue.getDecl()->getName());
  }

  void emitTypedSendingNeverSendableToSendingClosureParam(
      SILLocation loc, ArrayRef<CapturedValue> capturedValues) {
    SmallString<64> descriptiveKindStr;
    {
      llvm::raw_svector_ostream os(descriptiveKindStr);
      if (getIsolationRegionInfo().getIsolationInfo().isTaskIsolated()) {
        os << "code in the current task";
      } else {
        getIsolationRegionInfo().printForDiagnostics(os);
        os << " code";
      }
    }

    auto behaviorLimit = getDiagnosticBehaviorLimitForCapturedValues(
        getFunction(), capturedValues);
    diagnoseError(loc,
                  diag::regionbasedisolation_typed_tns_passed_sending_closure,
                  descriptiveKindStr)
        .highlight(loc.getSourceRange())
        .limitBehaviorIf(behaviorLimit);

    if (capturedValues.size() == 1) {
      auto captured = capturedValues.front();
      auto capturedLoc = RegularLocation(captured.getLoc());
      if (getIsolationRegionInfo().getIsolationInfo().isTaskIsolated()) {
        auto diag = diag::
            regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_value_task_isolated;
        diagnoseNote(capturedLoc, diag, captured.getDecl()->getName());
        return;
      }

      descriptiveKindStr.clear();
      {
        llvm::raw_svector_ostream os(descriptiveKindStr);
        getIsolationRegionInfo().printForDiagnostics(os);
      }
      auto diag = diag::
          regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_value_region;
      diagnoseNote(capturedLoc, diag, descriptiveKindStr,
                   captured.getDecl()->getName());
      return;
    }

    for (auto captured : capturedValues) {
      auto capturedLoc = RegularLocation(captured.getLoc());
      auto diag = diag::
          regionbasedisolation_typed_tns_passed_to_sending_closure_helper_multiple_value;
      diagnoseNote(capturedLoc, diag, captured.getDecl()->getName());
    }
  }

  void emitNamedOnlyError(SILLocation loc, Identifier name) {
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .highlight(getOperand()->getUser()->getLoc().getSourceRange())
        .limitBehaviorIf(getBehaviorLimit());
  }

  void emitNamedAsyncLetCapture(SILLocation loc, Identifier name,
                                SILIsolationInfo sentValueIsolation) {
    assert(!getIsolationRegionInfo().isDisconnected() &&
           "Should never be disconnected?!");
    emitNamedOnlyError(loc, name);

    SmallString<64> descriptiveKindStr;
    {
      llvm::raw_svector_ostream os(descriptiveKindStr);
      getIsolationRegionInfo().printForDiagnostics(os);
    }

    diagnoseNote(loc, diag::regionbasedisolation_named_send_nt_asynclet_capture,
                 name, descriptiveKindStr)
        .limitBehaviorIf(getBehaviorLimit());
  }

  void emitNamedIsolation(SILLocation loc, Identifier name,
                          ApplyIsolationCrossing isolationCrossing) {
    emitNamedOnlyError(loc, name);
    SmallString<64> descriptiveKindStr;
    SmallString<64> descriptiveKindStrWithSpace;
    {
      if (!getIsolationRegionInfo().isDisconnected()) {
        {
          llvm::raw_svector_ostream os(descriptiveKindStr);
          getIsolationRegionInfo().printForDiagnostics(os);
        }
        descriptiveKindStrWithSpace = descriptiveKindStr;
        descriptiveKindStrWithSpace.push_back(' ');
      }
    }
    if (auto calleeInfo = getSendingCalleeInfo()) {
      diagnoseNote(loc,
                   diag::regionbasedisolation_named_send_never_sendable_callee,
                   name, descriptiveKindStrWithSpace,
                   isolationCrossing.getCalleeIsolation(), calleeInfo->first,
                   calleeInfo->second, descriptiveKindStr);
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_named_send_never_sendable,
                   name, descriptiveKindStrWithSpace,
                   isolationCrossing.getCalleeIsolation(), descriptiveKindStr);
    }
  }

  void emitNamedSendingNeverSendableToSendingParam(SILLocation loc,
                                                   Identifier varName) {
    emitNamedOnlyError(loc, varName);
    SmallString<64> descriptiveKindStr;
    {
      if (!getIsolationRegionInfo().isDisconnected()) {
        llvm::raw_svector_ostream os(descriptiveKindStr);
        getIsolationRegionInfo().printForDiagnostics(os);
        os << ' ';
      }
    }
    auto diag = diag::regionbasedisolation_named_send_into_sending_param;
    diagnoseNote(loc, diag, descriptiveKindStr, varName);
  }

  void emitNamedSendingReturn(SILLocation loc, Identifier varName) {
    emitNamedOnlyError(loc, varName);
    SmallString<64> descriptiveKindStr;
    SmallString<64> descriptiveKindStrWithSpace;
    {
      if (!getIsolationRegionInfo().isDisconnected()) {
        {
          llvm::raw_svector_ostream os(descriptiveKindStr);
          getIsolationRegionInfo().printForDiagnostics(os);
        }
        descriptiveKindStrWithSpace = descriptiveKindStr;
        descriptiveKindStrWithSpace.push_back(' ');
      }
    }
    auto diag = diag::regionbasedisolation_named_nosend_send_into_result;
    diagnoseNote(loc, diag, descriptiveKindStrWithSpace, varName,
                 descriptiveKindStr);
  }

private:
  ASTContext &getASTContext() const {
    return getOperand()->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilSwiftVersion(6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }
};

class SentNeverSendableDiagnosticInferrer {
  struct AutoClosureWalker;

  RegionAnalysisValueMap &valueMap;
  SendNeverSentDiagnosticEmitter diagnosticEmitter;

  using SentNeverSendableError = PartitionOpError::SentNeverSendableError;

public:
  SentNeverSendableDiagnosticInferrer(RegionAnalysisValueMap &valueMap,
                                      SentNeverSendableError error)
      : valueMap(valueMap),
        diagnosticEmitter(error.op->getSourceOp(),
                          valueMap.getRepresentative(error.sentElement),
                          error.isolationRegionInfo) {}

  /// Gathers diagnostics. Returns false if we emitted a "I don't understand
  /// error". If we emit such an error, we should bail without emitting any
  /// further diagnostics, since we may not have any diagnostics or be in an
  /// inconcistent state.
  bool run();

private:
  /// \p actualCallerIsolation is used to override the caller isolation we use
  /// when emitting the error if the closure would have the incorrect one.
  bool initForIsolatedPartialApply(
      Operand *op, AbstractClosureExpr *ace,
      std::optional<ActorIsolation> actualCallerIsolation = {});

  bool initForSendingPartialApply(FullApplySite fas, Operand *pai);

  std::optional<unsigned>
  getIsolatedValuePartialApplyIndex(PartialApplyInst *pai,
                                    SILValue isolatedValue) {
    for (auto &paiOp : ApplySite(pai).getArgumentOperands()) {
      if (valueMap.getTrackableValue(paiOp.get()).getRepresentative() ==
          isolatedValue) {
        return ApplySite(pai).getASTAppliedArgIndex(paiOp);
      }
    }

    return {};
  }
};

} // namespace

bool SentNeverSendableDiagnosticInferrer::initForSendingPartialApply(
    FullApplySite fas, Operand *paiOp) {
  auto *pai =
      dyn_cast<PartialApplyInst>(stripFunctionConversions(paiOp->get()));
  if (!pai)
    return false;

  // For now we want this to be really narrow and to only apply to closure
  // literals.
  auto *ce = pai->getLoc().getAsASTNode<ClosureExpr>();
  if (!ce)
    return false;

  // Ok, we now know we have a partial apply and it is a closure literal. Lets
  // see if we can find the exact thing that caused the closure literal to be
  // actor isolated.
  auto isolationInfo = diagnosticEmitter.getIsolationRegionInfo();
  if (isolationInfo->hasIsolatedValue()) {
    // Now that we have the value, see if that value is one of our captured
    // values.
    auto isolatedValue = isolationInfo->getIsolatedValue();
    auto matchingElt = getIsolatedValuePartialApplyIndex(pai, isolatedValue);
    if (matchingElt) {
      // Ok, we found the matching element. Lets emit our diagnostic!
      auto capture = ce->getCaptureInfo().getCaptures()[*matchingElt];
      diagnosticEmitter
          .emitTypedSendingNeverSendableToSendingClosureParamDirectlyIsolated(
              ce, capture);
      return true;
    }
  }

  // Ok, we are not tracking an actual isolated value or we do not capture the
  // isolated value directly... we need to be smarter here. First lets gather up
  // all non-Sendable values captured by the closure.
  SmallVector<CapturedValue, 8> nonSendableCaptures;
  for (auto capture : ce->getCaptureInfo().getCaptures()) {
    auto *decl = capture.getDecl();
    auto type = decl->getInterfaceType()->getCanonicalType();
    auto silType = SILType::getPrimitiveObjectType(type);
    if (!SILIsolationInfo::isNonSendableType(silType, pai->getFunction()))
      continue;

    auto *fromDC = decl->getInnermostDeclContext();
    auto *nom = silType.getNominalOrBoundGenericNominal();
    if (nom && fromDC) {
      if (auto diagnosticBehavior =
              getConcurrencyDiagnosticBehaviorLimit(nom, fromDC)) {
        if (*diagnosticBehavior == DiagnosticBehavior::Ignore)
          continue;
      }
    }
    nonSendableCaptures.push_back(capture);
  }

  // If we do not have any non-Sendable captures... bail.
  if (nonSendableCaptures.empty())
    return false;

  // Otherwise, emit the diagnostic.
  diagnosticEmitter.emitTypedSendingNeverSendableToSendingClosureParam(
      ce, nonSendableCaptures);
  return true;
}

bool SentNeverSendableDiagnosticInferrer::initForIsolatedPartialApply(
    Operand *op, AbstractClosureExpr *ace,
    std::optional<ActorIsolation> actualCallerIsolation) {
  SmallVector<std::tuple<CapturedValue, unsigned, ApplyIsolationCrossing>, 8>
      foundCapturedIsolationCrossing;
  ace->getIsolationCrossing(foundCapturedIsolationCrossing);
  if (foundCapturedIsolationCrossing.empty())
    return false;

  // We use getASTAppliedArgIndex instead of getAppliedArgIndex to ensure that
  // we ignore for our indexing purposes any implicit initial parameters like
  // isolated(any).
  unsigned opIndex = ApplySite(op->getUser()).getASTAppliedArgIndex(*op);
  for (auto &p : foundCapturedIsolationCrossing) {
    if (std::get<1>(p) == opIndex) {
      auto loc = RegularLocation(std::get<0>(p).getLoc());
      auto crossing = std::get<2>(p);
      auto declIsolation = crossing.getCallerIsolation();
      auto closureIsolation = crossing.getCalleeIsolation();
      if (!bool(declIsolation) && actualCallerIsolation) {
        declIsolation = *actualCallerIsolation;
      }
      diagnosticEmitter.emitNamedFunctionArgumentClosure(
          loc, std::get<0>(p).getDecl()->getBaseIdentifier(),
          ApplyIsolationCrossing(declIsolation, closureIsolation));
      return true;
    }
  }

  return false;
}

/// This walker visits an AutoClosureExpr and looks for uses of a specific
/// captured value. We want to error on the uses in the autoclosure.
struct SentNeverSendableDiagnosticInferrer::AutoClosureWalker : ASTWalker {
  SendNeverSentDiagnosticEmitter &foundTypeInfo;
  ValueDecl *targetDecl;
  SILIsolationInfo targetDeclIsolationInfo;
  SmallPtrSet<Expr *, 8> visitedCallExprDeclRefExprs;
  SILLocation captureLoc;
  bool isAsyncLet;

  AutoClosureWalker(SendNeverSentDiagnosticEmitter &foundTypeInfo,
                    ValueDecl *targetDecl,
                    SILIsolationInfo targetDeclIsolationInfo,
                    SILLocation captureLoc, bool isAsyncLet)
      : foundTypeInfo(foundTypeInfo), targetDecl(targetDecl),
        targetDeclIsolationInfo(targetDeclIsolationInfo),
        captureLoc(captureLoc), isAsyncLet(isAsyncLet) {}

  Expr *lookThroughArgExpr(Expr *expr) {
    while (true) {
      if (auto *memberRefExpr = dyn_cast<MemberRefExpr>(expr)) {
        expr = memberRefExpr->getBase();
        continue;
      }

      if (auto *cvt = dyn_cast<ImplicitConversionExpr>(expr)) {
        expr = cvt->getSubExpr();
        continue;
      }

      if (auto *e = dyn_cast<ForceValueExpr>(expr)) {
        expr = e->getSubExpr();
        continue;
      }

      if (auto *t = dyn_cast<TupleElementExpr>(expr)) {
        expr = t->getBase();
        continue;
      }

      return expr;
    }
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
    if (auto *declRef = dyn_cast<DeclRefExpr>(expr)) {
      // If this decl ref expr was not visited as part of a callExpr and is our
      // target decl... emit a simple async let error.
      //
      // This occurs if we do:
      //
      // ```
      // let x = ...
      // async let y = x
      // ```
      if (declRef->getDecl() == targetDecl) {
        foundTypeInfo.emitNamedAsyncLetCapture(captureLoc,
                                               targetDecl->getBaseIdentifier(),
                                               targetDeclIsolationInfo);
        return Action::Continue(expr);
      }
    }

    return Action::Continue(expr);
  }
};

bool SentNeverSendableDiagnosticInferrer::run() {
  // We need to find the isolation info.
  auto *op = diagnosticEmitter.getOperand();
  auto loc = op->getUser()->getLoc();

  if (auto *sourceApply = loc.getAsASTNode<ApplyExpr>()) {
    // First see if we have a 'sending' argument.
    if (auto fas = FullApplySite::isa(op->getUser())) {
      if (fas.getArgumentParameterInfo(*op).hasOption(
              SILParameterInfo::Sending)) {
        // Before we do anything, lets see if we are passing a sendable closure
        // literal. If we do, we want to emit a special error that states which
        // captured value caused the actual error.
        if (initForSendingPartialApply(fas, op))
          return true;

        // See if we can infer a name from the value.
        SmallString<64> resultingName;
        if (auto varName = inferNameHelper(op->get())) {
          diagnosticEmitter.emitNamedSendingNeverSendableToSendingParam(
              loc, *varName);
          return true;
        }

        Type type = op->get()->getType().getASTType();
        if (auto *inferredArgExpr =
                inferArgumentExprFromApplyExpr(sourceApply, fas, op)) {
          type = inferredArgExpr->findOriginalType();
        }

        diagnosticEmitter.emitTypedSendingNeverSendableToSendingParam(loc,
                                                                      type);
        return true;
      }
    }

    // First try to get the apply from the isolation crossing.
    auto isolation = sourceApply->getIsolationCrossing();

    // If we could not infer an isolation...
    if (!isolation) {
      // Otherwise, emit a "we don't know error" that tells the user to file a
      // bug.
      diagnosticEmitter.emitUnknownPatternError();
      return false;
    }
    assert(isolation && "Expected non-null");

    // Then if we are calling a closure expr. If so, we should use the loc of
    // the closure.
    if (auto *closureExpr =
            dyn_cast<AbstractClosureExpr>(sourceApply->getFn())) {
      initForIsolatedPartialApply(op, closureExpr,
                                  isolation->getCallerIsolation());
      return true;
    }

    // See if we can infer a name from the value.
    SmallString<64> resultingName;
    if (auto name = inferNameHelper(op->get())) {
      diagnosticEmitter.emitNamedIsolation(loc, *name, *isolation);
      return true;
    }

    // Attempt to find the specific sugared ASTType if we can to emit a better
    // diagnostic.
    Type type = op->get()->getType().getASTType();
    if (auto fas = FullApplySite::isa(op->getUser())) {
      if (auto *inferredArgExpr =
              inferArgumentExprFromApplyExpr(sourceApply, fas, op)) {
        type = inferredArgExpr->findOriginalType();
      }
    }

    diagnosticEmitter.emitPassToApply(loc, type, *isolation);
    return true;
  }

  if (auto *ace = loc.getAsASTNode<AbstractClosureExpr>()) {
    if (ace->getActorIsolation().isActorIsolated()) {
      if (initForIsolatedPartialApply(op, ace)) {
        return true;
      }
    }
  }

  // See if we are in SIL and have an apply site specified isolation.
  if (auto fas = FullApplySite::isa(op->getUser())) {
    if (auto isolation = fas.getIsolationCrossing()) {
      diagnosticEmitter.emitPassToApply(loc, op->get()->getType().getASTType(),
                                        *isolation);
      return true;
    }
  }

  if (auto *ri = dyn_cast<ReturnInst>(op->getUser())) {
    auto fType = ri->getFunction()->getLoweredFunctionType();
    if (fType->getNumResults() &&
        fType->getResults()[0].hasOption(SILResultInfo::IsSending)) {
      assert(llvm::all_of(fType->getResults(),
                          [](SILResultInfo resultInfo) {
                            return resultInfo.hasOption(
                                SILResultInfo::IsSending);
                          }) &&
             "All result info must be the same... if that changes... update "
             "this code!");
      SmallString<64> resultingName;
      if (auto name = inferNameHelper(op->get())) {
        diagnosticEmitter.emitNamedSendingReturn(loc, *name);
        return true;
      }
    } else {
      assert(llvm::none_of(fType->getResults(),
                           [](SILResultInfo resultInfo) {
                             return resultInfo.hasOption(
                                 SILResultInfo::IsSending);
                           }) &&
             "All result info must be the same... if that changes... update "
             "this code!");
    }
  }

  // If we are failing due to an autoclosure... see if we can find the captured
  // value that is causing the issue.
  if (auto *autoClosureExpr = loc.getAsASTNode<AutoClosureExpr>()) {
    // To split up this work, we only do this for async let for now.
    if (autoClosureExpr->getThunkKind() == AutoClosureExpr::Kind::AsyncLet) {
      auto *i = op->getUser();
      auto pai = ApplySite::isa(i);
      unsigned captureIndex = pai.getASTAppliedArgIndex(*op);
      auto captureInfo =
          autoClosureExpr->getCaptureInfo().getCaptures()[captureIndex];
      auto loc = RegularLocation(captureInfo.getLoc(), false /*implicit*/);
      AutoClosureWalker walker(
          diagnosticEmitter, captureInfo.getDecl(),
          diagnosticEmitter.getIsolationRegionInfo().getIsolationInfo(), loc,
          autoClosureExpr->getThunkKind() == AutoClosureExpr::Kind::AsyncLet);
      autoClosureExpr->walk(walker);
      return true;
    }
  }

  diagnosticEmitter.emitUnknownUse(loc);
  return true;
}

//===----------------------------------------------------------------------===//
//              MARK: InOutSendingNotDisconnected Error Emitter
//===----------------------------------------------------------------------===//

namespace {

class InOutSendingNotDisconnectedDiagnosticEmitter {
  /// The function exiting inst where the 'inout sending' parameter was actor
  /// isolated.
  TermInst *functionExitingInst;

  /// The 'inout sending' param that we are emitting an error for.
  SILValue inoutSendingParam;

  /// The dynamic actor isolated region info of our 'inout sending' value's
  /// region at the terminator inst.
  SILDynamicMergedIsolationInfo actorIsolatedRegionInfo;

  bool emittedErrorDiagnostic = false;

public:
  InOutSendingNotDisconnectedDiagnosticEmitter(
      TermInst *functionExitingInst, SILValue inoutSendingParam,
      SILDynamicMergedIsolationInfo actorIsolatedRegionInfo)
      : functionExitingInst(functionExitingInst),
        inoutSendingParam(inoutSendingParam),
        actorIsolatedRegionInfo(actorIsolatedRegionInfo) {}

  ~InOutSendingNotDisconnectedDiagnosticEmitter() {
    // If we were supposed to emit a diagnostic and didn't emit an unknown
    // pattern error.
    if (!emittedErrorDiagnostic)
      emitUnknownPatternError();
  }

  SILFunction *getFunction() const { return inoutSendingParam->getFunction(); }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    return inoutSendingParam->getType().getConcurrencyDiagnosticBehavior(
        getFunction());
  }

  void emitUnknownPatternError() {
    if (shouldAbortOnUnknownPatternMatchError()) {
      llvm::report_fatal_error(
          "RegionIsolation: Aborting on unknown pattern match error");
    }

    diagnoseError(functionExitingInst,
                  diag::regionbasedisolation_unknown_pattern)
        .limitBehaviorIf(getBehaviorLimit());
  }

  void emit();

  ASTContext &getASTContext() const {
    return functionExitingInst->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilSwiftVersion(6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }
};

} // namespace

void InOutSendingNotDisconnectedDiagnosticEmitter::emit() {
  // We should always be able to find a name for an inout sending param. If we
  // do not, emit an unknown pattern error.
  auto varName = inferNameHelper(inoutSendingParam);
  if (!varName) {
    return emitUnknownPatternError();
  }

  // Then emit the note with greater context.
  SmallString<64> descriptiveKindStr;
  {
    llvm::raw_svector_ostream os(descriptiveKindStr);
    actorIsolatedRegionInfo.printForDiagnostics(os);
    os << ' ';
  }

  diagnoseError(
      functionExitingInst,
      diag::regionbasedisolation_inout_sending_cannot_be_actor_isolated,
      *varName, descriptiveKindStr)
      .limitBehaviorIf(getBehaviorLimit());

  diagnoseNote(
      functionExitingInst,
      diag::regionbasedisolation_inout_sending_cannot_be_actor_isolated_note,
      *varName, descriptiveKindStr);
}

//===----------------------------------------------------------------------===//
//           MARK: AssignIsolatedIntoSendingResultDiagnosticEmitter
//===----------------------------------------------------------------------===//

namespace {

class AssignIsolatedIntoSendingResultDiagnosticEmitter {
  /// The use that actually caused the send.
  Operand *srcOperand;

  /// The specific out sending result.
  SILFunctionArgument *outSendingResult;

  /// The never-sent value that is in the same region as \p
  /// outSendingResult.
  SILValue neverSentValue;

  /// The region info that describes the dynamic dataflow derived isolation
  /// region info for the never-sent value.
  ///
  /// This is equal to the merge of the IsolationRegionInfo from all elements in
  /// the never-sent value's region when the error was diagnosed.
  SILDynamicMergedIsolationInfo isolatedValueIsolationRegionInfo;

  bool emittedErrorDiagnostic = false;

public:
  AssignIsolatedIntoSendingResultDiagnosticEmitter(
      Operand *srcOperand, SILFunctionArgument *outSendingResult,
      SILValue neverSentValue,
      SILDynamicMergedIsolationInfo isolatedValueIsolationRegionInfo)
      : srcOperand(srcOperand), outSendingResult(outSendingResult),
        neverSentValue(neverSentValue),
        isolatedValueIsolationRegionInfo(isolatedValueIsolationRegionInfo) {}

  ~AssignIsolatedIntoSendingResultDiagnosticEmitter() {
    // If we were supposed to emit a diagnostic and didn't emit an unknown
    // pattern error.
    if (!emittedErrorDiagnostic)
      emitUnknownPatternError();
  }

  SILFunction *getFunction() const { return srcOperand->getFunction(); }

  std::optional<DiagnosticBehavior> getConcurrencyDiagnosticBehavior() const {
    return outSendingResult->getType().getConcurrencyDiagnosticBehavior(
        getFunction());
  }

  void emitUnknownPatternError() {
    if (shouldAbortOnUnknownPatternMatchError()) {
      llvm::report_fatal_error(
          "RegionIsolation: Aborting on unknown pattern match error");
    }

    diagnoseError(srcOperand->getUser(),
                  diag::regionbasedisolation_unknown_pattern)
        .limitBehaviorIf(getConcurrencyDiagnosticBehavior());
  }

  void emit();

  ASTContext &getASTContext() const {
    return srcOperand->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilSwiftVersion(6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(Operand *op, Diag<T...> diag, U &&...args) {
    return diagnoseError(op->getUser()->getLoc(), diag,
                         std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(Operand *op, Diag<T...> diag, U &&...args) {
    return diagnoseNote(op->getUser()->getLoc(), diag,
                        std::forward<U>(args)...);
  }
};

} // namespace

/// Look through values looking for our out parameter. We want to tightly
/// control this to be conservative... so we handroll this.
static SILValue findOutParameter(SILValue v) {
  while (true) {
    SILValue temp = v;
    if (auto *initOpt = dyn_cast<InitEnumDataAddrInst>(temp)) {
      if (initOpt->getElement()->getParentEnum() ==
          initOpt->getFunction()->getASTContext().getOptionalDecl()) {
        temp = initOpt->getOperand();
      }
    }

    if (temp == v) {
      return v;
    }

    v = temp;
  }
}

void AssignIsolatedIntoSendingResultDiagnosticEmitter::emit() {
  // Then emit the note with greater context.
  SmallString<64> descriptiveKindStr;
  {
    llvm::raw_svector_ostream os(descriptiveKindStr);
    isolatedValueIsolationRegionInfo.printForDiagnostics(os);
  }

  // Grab the var name if we can find it.
  if (auto varName = VariableNameInferrer::inferName(srcOperand->get())) {
    // In general, when we do an assignment like this, we assume that srcOperand
    // and our outSendingResult have the same type. This doesn't always happen
    // though especially if our outSendingResult is used as an out parameter of
    // a class_method. Check for such a case and if so, add to the end of our
    // string a path component for that class_method.
    if (srcOperand->get()->getType() != outSendingResult->getType()) {
      if (auto fas = FullApplySite::isa(srcOperand->getUser())) {
        if (fas.hasSelfArgument() &&
            fas.getSelfArgument() == srcOperand->get() &&
            fas.getNumIndirectSILResults() == 1) {
          // First check if our function argument is exactly our out parameter.
          bool canEmit = outSendingResult == fas.getIndirectSILResults()[0];

          // If that fails, see if we are storing into a temporary
          // alloc_stack. In such a case, find the root value that the temporary
          // is initialized to and see if that is our target function
          // argument. In such a case, we also want to add the decl name to our
          // type.
          if (!canEmit) {
            canEmit = outSendingResult ==
                      findOutParameter(fas.getIndirectSILResults()[0]);
          }

          if (canEmit) {
            if (auto *callee =
                    dyn_cast_or_null<MethodInst>(fas.getCalleeOrigin())) {
              SmallString<64> resultingString;
              resultingString.append(varName->str());
              resultingString += '.';
              resultingString += VariableNameInferrer::getNameFromDecl(
                  callee->getMember().getDecl());
              varName = fas->getFunction()->getASTContext().getIdentifier(
                  resultingString);
            }
          }
        }
      }
    }

    diagnoseError(
        srcOperand,
        diag::regionbasedisolation_out_sending_cannot_be_actor_isolated_named,
        *varName, descriptiveKindStr)
        .limitBehaviorIf(getConcurrencyDiagnosticBehavior());

    diagnoseNote(
        srcOperand,
        diag::
            regionbasedisolation_out_sending_cannot_be_actor_isolated_note_named,
        *varName, descriptiveKindStr);
    return;
  }

  Type type = neverSentValue->getType().getASTType();

  diagnoseError(
      srcOperand,
      diag::regionbasedisolation_out_sending_cannot_be_actor_isolated_type,
      type, descriptiveKindStr)
      .limitBehaviorIf(getConcurrencyDiagnosticBehavior());

  diagnoseNote(
      srcOperand,
      diag::regionbasedisolation_out_sending_cannot_be_actor_isolated_note_type,
      type, descriptiveKindStr);
  diagnoseNote(srcOperand, diag::regionbasedisolation_type_is_non_sendable,
               type);
}

//===----------------------------------------------------------------------===//
//              MARK: NonSendableIsolationCrossingResult Emitter
//===----------------------------------------------------------------------===//

/// Add Fix-It text for the given nominal type to adopt Sendable.
static void addSendableFixIt(const NominalTypeDecl *nominal,
                             InFlightDiagnostic &diag, bool unchecked) {
  if (nominal->getInherited().empty()) {
    SourceLoc fixItLoc = nominal->getBraces().Start;
    diag.fixItInsert(fixItLoc,
                     unchecked ? ": @unchecked Sendable" : ": Sendable");
  } else {
    auto fixItLoc = nominal->getInherited().getEndLoc();
    diag.fixItInsertAfter(fixItLoc,
                          unchecked ? ", @unchecked Sendable" : ", Sendable");
  }
}

/// Add Fix-It text for the given generic param declaration type to adopt
/// Sendable.
static void addSendableFixIt(const GenericTypeParamDecl *genericArgument,
                             InFlightDiagnostic &diag, bool unchecked) {
  if (genericArgument->getInherited().empty()) {
    auto fixItLoc = genericArgument->getLoc();
    diag.fixItInsertAfter(fixItLoc,
                          unchecked ? ": @unchecked Sendable" : ": Sendable");
  } else {
    auto fixItLoc = genericArgument->getInherited().getEndLoc();
    diag.fixItInsertAfter(fixItLoc,
                          unchecked ? ", @unchecked Sendable" : ", Sendable");
  }
}

namespace {

struct NonSendableIsolationCrossingResultDiagnosticEmitter {
  RegionAnalysisValueMap &valueMap;

  using Error = PartitionOpError::NonSendableIsolationCrossingResultError;
  Error error;

  bool emittedErrorDiagnostic = false;

  /// The value assigned as the equivalence class representative. It is
  /// guaranteed to be from the isolation crossing function since we never treat
  /// isolation crossing functions as being look through.
  SILValue representative;

  NonSendableIsolationCrossingResultDiagnosticEmitter(
      RegionAnalysisValueMap &valueMap, Error error)
      : valueMap(valueMap), error(error),
        representative(valueMap.getRepresentative(error.returnValueElement)) {}

  void emit();

  ASTContext &getASTContext() const {
    return error.op->getSourceInst()->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilSwiftVersion(6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    return representative->getType().getConcurrencyDiagnosticBehavior(
        representative->getFunction());
  }

  void emitUnknownPatternError() {
    if (shouldAbortOnUnknownPatternMatchError()) {
      llvm::report_fatal_error(
          "RegionIsolation: Aborting on unknown pattern match error");
    }

    diagnoseError(error.op->getSourceInst(),
                  diag::regionbasedisolation_unknown_pattern)
        .limitBehaviorIf(getBehaviorLimit());
  }

  Type getType() const {
    if (auto *applyExpr =
            error.op->getSourceInst()->getLoc().getAsASTNode<ApplyExpr>()) {
      return applyExpr->getType();
    }

    // If we do not have an ApplyExpr, see if we can just infer the type from
    // the SILFunction type. This is only used in SIL test cases.
    if (auto fas = FullApplySite::isa(error.op->getSourceInst())) {
      return fas.getSubstCalleeType()
          ->getAllResultsSubstType(fas.getModule(),
                                   fas.getFunction()->getTypeExpansionContext())
          .getASTType();
    }

    return Type();
  }

  const ValueDecl *getCalledDecl() const {
    if (auto *applyExpr =
            error.op->getSourceInst()->getLoc().getAsASTNode<ApplyExpr>()) {
      if (auto calledValue =
              applyExpr->getCalledValue(true /*look through conversions*/)) {
        return calledValue;
      }
    }

    return nullptr;
  }

  std::optional<ApplyIsolationCrossing> getIsolationCrossing() const {
    if (auto *applyExpr =
            error.op->getSourceInst()->getLoc().getAsASTNode<ApplyExpr>()) {
      if (auto isolationCrossing = applyExpr->getIsolationCrossing()) {
        return *isolationCrossing;
      }
    }

    // If we have a SIL based test case, just return the actual isolation
    // crossing.
    if (auto fas = FullApplySite::isa(error.op->getSourceInst())) {
      if (auto isolationCrossing = fas.getIsolationCrossing())
        return *isolationCrossing;
    }

    return {};
  }
};

} // namespace

void NonSendableIsolationCrossingResultDiagnosticEmitter::emit() {
  auto isolationCrossing = getIsolationCrossing();
  if (!isolationCrossing)
    return emitUnknownPatternError();

  auto type = getType();
  if (auto *decl = getCalledDecl()) {
    diagnoseError(error.op->getSourceInst(), diag::rbi_isolation_crossing_result,
                  type, isolationCrossing->getCalleeIsolation(), getCalledDecl(),
                  isolationCrossing->getCallerIsolation())
      .limitBehaviorIf(getBehaviorLimit());
  } else {
    diagnoseError(error.op->getSourceInst(), diag::rbi_isolation_crossing_result_no_decl,
                  type, isolationCrossing->getCalleeIsolation(),
                  isolationCrossing->getCallerIsolation())
      .limitBehaviorIf(getBehaviorLimit());
  }
  if (type->is<FunctionType>()) {
    diagnoseNote(error.op->getSourceInst(),
                 diag::rbi_nonsendable_function_type);
    return;
  }

  auto *moduleDecl = error.op->getSourceInst()->getModule().getSwiftModule();
  if (auto *nominal = type->getNominalOrBoundGenericNominal()) {
    // If the nominal type is in the current module, suggest adding `Sendable`
    // if it makes sense.
    if (nominal->getParentModule() == moduleDecl &&
        (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal))) {
      auto note = nominal->diagnose(diag::rbi_add_nominal_sendable_conformance,
                                    nominal);
      addSendableFixIt(nominal, note, /*unchecked*/ false);
    } else {
      nominal->diagnose(diag::rbi_non_sendable_nominal, nominal);
    }
    return;
  }

  if (auto genericArchetype = type->getAs<ArchetypeType>()) {
    auto interfaceType = genericArchetype->getInterfaceType();
    if (auto genericParamType = interfaceType->getAs<GenericTypeParamType>()) {
      auto *genericParamTypeDecl = genericParamType->getDecl();
      if (genericParamTypeDecl &&
          genericParamTypeDecl->getModuleContext() == moduleDecl) {
        auto diag = genericParamTypeDecl->diagnose(
            diag::rbi_add_generic_parameter_sendable_conformance, type);
        addSendableFixIt(genericParamTypeDecl, diag, /*unchecked=*/false);
        return;
      }
    }
  }
}

//===----------------------------------------------------------------------===//
//                         MARK: Diagnostic Evaluator
//===----------------------------------------------------------------------===//

namespace {

struct DiagnosticEvaluator final
    : PartitionOpEvaluatorBaseImpl<DiagnosticEvaluator> {
  RegionAnalysisFunctionInfo *info;
  SmallFrozenMultiMap<Operand *, RequireInst, 8>
      &sendingOpToRequireInstMultiMap;

  /// An error that we know how to emit verbatim without needing to preprocess.
  ///
  /// A contrasting case here is the use after send error where we need to pair
  /// sending operands to require insts.
  SmallVectorImpl<PartitionOpError> &foundVerbatimErrors;

  DiagnosticEvaluator(Partition &workingPartition,
                      RegionAnalysisFunctionInfo *info,
                      SmallFrozenMultiMap<Operand *, RequireInst, 8>
                          &sendingOpToRequireInstMultiMap,
                      SmallVectorImpl<PartitionOpError> &foundVerbatimErrors,
                      SendingOperandToStateMap &operandToStateMap)
      : PartitionOpEvaluatorBaseImpl(
            workingPartition, info->getOperandSetFactory(), operandToStateMap),
        info(info),
        sendingOpToRequireInstMultiMap(sendingOpToRequireInstMultiMap),
        foundVerbatimErrors(foundVerbatimErrors) {}

  void handleLocalUseAfterSend(LocalUseAfterSendError error) const {
    const auto &partitionOp = *error.op;

    auto &operandState = operandToStateMap.get(error.sendingOp);
    // Ignore this if we have a gep like instruction that is returning a
    // sendable type and sendingOp was not set with closure
    // capture.
    if (auto *svi =
            dyn_cast<SingleValueInstruction>(partitionOp.getSourceInst())) {
      if (isa<TupleElementAddrInst, StructElementAddrInst>(svi) &&
          !SILIsolationInfo::isNonSendableType(svi->getType(),
                                               svi->getFunction())) {
        bool isCapture = operandState.isClosureCaptured;
        if (!isCapture) {
          return;
        }
      }
    }

    REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));
    sendingOpToRequireInstMultiMap.insert(
        error.sendingOp, RequireInst::forUseAfterSend(partitionOp.getSourceInst()));
  }

  void handleInOutSendingNotInitializedAtExitError(
      InOutSendingNotInitializedAtExitError error) const {
    const PartitionOp &partitionOp = *error.op;
    Operand *sendingOp = error.sendingOp;

    REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));

    sendingOpToRequireInstMultiMap.insert(
        sendingOp, RequireInst::forInOutReinitializationNeeded(
                       partitionOp.getSourceInst()));
  }

  void handleUnknownCodePattern(UnknownCodePatternError error) const {
    const PartitionOp &op = *error.op;

    if (shouldAbortOnUnknownPatternMatchError()) {
      llvm::report_fatal_error(
          "RegionIsolation: Aborting on unknown pattern match error");
    }

    diagnoseError(op.getSourceInst(),
                  diag::regionbasedisolation_unknown_pattern);
  }

  void handleError(PartitionOpError error) {
    switch (error.getKind()) {
    case PartitionOpError::LocalUseAfterSend: {
      return handleLocalUseAfterSend(error.getLocalUseAfterSendError());
    }
    case PartitionOpError::InOutSendingNotDisconnectedAtExit:
    case PartitionOpError::SentNeverSendable:
    case PartitionOpError::AssignNeverSendableIntoSendingResult:
    case PartitionOpError::NonSendableIsolationCrossingResult:
      // We are going to process these later... but dump so we can see that we
      // handled an error here. The rest of the explicit handlers will dump as
      // appropriate if they want to emit an error here (some will squelch the
      // error).
      REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));
      foundVerbatimErrors.emplace_back(error);
      return;
    case PartitionOpError::InOutSendingNotInitializedAtExit: {
      return handleInOutSendingNotInitializedAtExitError(
          error.getInOutSendingNotInitializedAtExitError());
    }
    case PartitionOpError::UnknownCodePattern: {
      return handleUnknownCodePattern(error.getUnknownCodePatternError());
    }
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }

  bool isActorDerived(Element element) const {
    return info->getValueMap().getIsolationRegion(element).isActorIsolated();
  }

  bool isTaskIsolatedDerived(Element element) const {
    return info->getValueMap().getIsolationRegion(element).isTaskIsolated();
  }

  SILIsolationInfo::Kind hasSpecialDerivation(Element element) const {
    return info->getValueMap().getIsolationRegion(element).getKind();
  }

  SILIsolationInfo getIsolationRegionInfo(Element element) const {
    return info->getValueMap().getIsolationRegion(element);
  }

  std::optional<Element> getElement(SILValue value) const {
    return info->getValueMap().getTrackableValue(value).getID();
  }

  SILValue getRepresentative(SILValue value) const {
    return info->getValueMap()
        .getTrackableValue(value)
        .getRepresentative()
        .maybeGetValue();
  }

  RepresentativeValue getRepresentativeValue(Element element) const {
    return info->getValueMap().getRepresentativeValue(element);
  }

  bool isClosureCaptured(Element element, Operand *op) const {
    auto value = info->getValueMap().maybeGetRepresentative(element);
    if (!value)
      return false;
    return info->isClosureCaptured(value, op);
  }
};

} // namespace

void SendNonSendableImpl::runDiagnosticEvaluator() {
  // Then for each block...
  REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Walking blocks for diagnostics.\n");
  for (auto [block, blockState] : info->getRange()) {
    REGIONBASEDISOLATION_LOG(llvm::dbgs()
                             << "|--> Block bb" << block.getDebugID() << "\n");

    if (!blockState.getLiveness()) {
      REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Dead block... skipping!\n");
      continue;
    }

    REGIONBASEDISOLATION_LOG(
        llvm::dbgs() << "Entry Partition: ";
        blockState.getEntryPartition().print(llvm::dbgs()));

    // Grab its entry partition and setup an evaluator for the partition that
    // has callbacks that emit diagnsotics...
    Partition workingPartition = blockState.getEntryPartition();
    DiagnosticEvaluator eval(
        workingPartition, info, sendingOpToRequireInstMultiMap,
        foundVerbatimErrors, info->getSendingOperandToStateMap());

    // And then evaluate all of our partition ops on the entry partition.
    for (auto &partitionOp : blockState.getPartitionOps()) {
      eval.apply(partitionOp);
    }

    REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Exit Partition: ";
                             workingPartition.print(llvm::dbgs()));
  }

  REGIONBASEDISOLATION_LOG(llvm::dbgs()
                           << "Finished walking blocks for diagnostics.\n");

  // Now that we have found all of our sendingInsts/Requires emit errors.
  sendingOpToRequireInstMultiMap.setFrozen();
}

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

void SendNonSendableImpl::emitVerbatimErrors() {
  for (auto &erasedError : foundVerbatimErrors) {
    switch (erasedError.getKind()) {
    case PartitionOpError::UnknownCodePattern:
    case PartitionOpError::LocalUseAfterSend:
    case PartitionOpError::InOutSendingNotInitializedAtExit:
      llvm_unreachable("Handled elsewhere");
    case PartitionOpError::AssignNeverSendableIntoSendingResult: {
      auto error = erasedError.getAssignNeverSendableIntoSendingResultError();
      REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));
      AssignIsolatedIntoSendingResultDiagnosticEmitter emitter(
          error.op->getSourceOp(), error.destValue, error.srcValue,
          error.srcIsolationRegionInfo);
      emitter.emit();
      continue;
    }
    case PartitionOpError::InOutSendingNotDisconnectedAtExit: {
      auto error = erasedError.getInOutSendingNotDisconnectedAtExitError();
      auto inoutSendingVal =
          info->getValueMap().getRepresentative(error.inoutSendingElement);
      auto isolationRegionInfo = error.isolationInfo;

      REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));

      InOutSendingNotDisconnectedDiagnosticEmitter emitter(
          cast<TermInst>(error.op->getSourceInst()), inoutSendingVal,
          isolationRegionInfo);
      emitter.emit();
      continue;
    }
    case PartitionOpError::SentNeverSendable: {
      auto e = erasedError.getSentNeverSendableError();
      REGIONBASEDISOLATION_LOG(e.print(llvm::dbgs(), info->getValueMap()));
      SentNeverSendableDiagnosticInferrer diagnosticInferrer(
          info->getValueMap(), e);
      diagnosticInferrer.run();
      continue;
    }
    case PartitionOpError::NonSendableIsolationCrossingResult: {
      auto e = erasedError.getNonSendableIsolationCrossingResultError();
      REGIONBASEDISOLATION_LOG(e.print(llvm::dbgs(), info->getValueMap()));
      NonSendableIsolationCrossingResultDiagnosticEmitter diagnosticInferrer(
          info->getValueMap(), e);
      diagnosticInferrer.emit();
      continue;
    }
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }
}

/// Once we have reached a fixpoint, this routine runs over all blocks again
/// reporting any failures by applying our ops to the converged dataflow
/// state.
void SendNonSendableImpl::emitDiagnostics() {
  auto *function = info->getFunction();
  REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Emitting diagnostics for function "
                                        << function->getName() << "\n");

  runDiagnosticEvaluator();
  emitUseAfterSendDiagnostics();
  emitVerbatimErrors();
}

namespace {

class SendNonSendable : public SILFunctionTransform {
  void run() override {
    SILFunction *function = getFunction();

    auto *functionInfo = getAnalysis<RegionAnalysis>()->get(function);
    if (!functionInfo->isSupportedFunction()) {
      REGIONBASEDISOLATION_LOG(llvm::dbgs()
                               << "===> SKIPPING UNSUPPORTED FUNCTION: "
                               << function->getName() << '\n');

      return;
    }

    REGIONBASEDISOLATION_LOG(
        llvm::dbgs() << "===> PROCESSING: " << function->getName() << '\n');

    SendNonSendableImpl impl(functionInfo);
    impl.emitDiagnostics();
  }
};

} // end anonymous namespace

SILTransform *swift::createSendNonSendable() { return new SendNonSendable(); }
