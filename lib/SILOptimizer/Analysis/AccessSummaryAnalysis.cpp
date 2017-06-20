//===--- AccessSummaryAnalysis.cpp - SIL Access Summary Analysis ----------===//
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

#define DEBUG_TYPE "sil-access-summary-analysis"
#include "swift/SIL/SILArgument.h"
#include "swift/SILOptimizer/Analysis/AccessSummaryAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"

using namespace swift;

void AccessSummaryAnalysis::processFunction(FunctionInfo *info,
                                            FunctionOrder &order) {
  // Does the summary need to be recomputed?
  if (order.prepareForVisiting(info))
    return;

  // Compute function summary on a per-argument basis.
  unsigned index = 0;
  for (SILArgument *arg : info->getFunction()->getArguments()) {
    FunctionSummary &functionSummary = info->getSummary();
    ArgumentSummary &argSummary =
        functionSummary.getAccessForArgument(index);
    index++;

    auto *functionArg = cast<SILFunctionArgument>(arg);
    // Only summarize @inout_aliasable arguments.
    SILArgumentConvention convention =
        functionArg->getArgumentConvention().Value;
    if (convention != SILArgumentConvention::Indirect_InoutAliasable)
      continue;

    processArgument(info, functionArg, argSummary, order);
  }
}

/// Track uses of the arguments, recording in the summary any accesses
/// started by a begin_access and any flows of the arguments to other
/// functions.
void AccessSummaryAnalysis::processArgument(FunctionInfo *info,
                                             SILFunctionArgument *argument,
                                             ArgumentSummary &summary,
                                             FunctionOrder &order) {
  unsigned argumentIndex = argument->getIndex();

  // Use a worklist to track argument uses to be processed.
  llvm::SmallVector<Operand *, 32> worklist;

  // Start by adding the immediate uses of the argument to the worklist.
  worklist.append(argument->use_begin(), argument->use_end());

  // Iterate to follow uses of the arguments.
  while (!worklist.empty()) {
    Operand *operand = worklist.pop_back_val();
    SILInstruction *user = operand->getUser();

    switch (user->getKind()) {
    case ValueKind::BeginAccessInst: {
      auto *BAI = cast<BeginAccessInst>(user);
      summary.mergeWith(BAI->getAccessKind(), BAI->getLoc());
      // We don't add the users of the begin_access to the worklist because
      // even if these users eventually begin an access to the address
      // or a projection from it, that access can't begin more exclusive
      // access than this access -- otherwise it will be diagnosed
      // elsewhere.
      break;
    }
    case ValueKind::EndUnpairedAccessInst:
      // Don't diagnose unpaired access statically.
      assert(cast<EndUnpairedAccessInst>(user)->getEnforcement() ==
             SILAccessEnforcement::Dynamic);
      break;
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
      // Eventually we'll summarize individual struct elements separately.
      // For now an access to a part of the struct is treated as an access
      // to the whole struct.
      worklist.append(user->use_begin(), user->use_end());
      break;
    case ValueKind::DebugValueAddrInst:
    case ValueKind::AddressToPointerInst:
      // Ignore these uses, they don't affect formal accesses.
      break;
    case ValueKind::PartialApplyInst:
      processPartialApply(info, argumentIndex, cast<PartialApplyInst>(user),
                          operand, order);
      break;
    case ValueKind::ApplyInst:
      processFullApply(info, argumentIndex, cast<ApplyInst>(user), operand,
                       order);
      break;
    case ValueKind::TryApplyInst:
      processFullApply(info, argumentIndex, cast<TryApplyInst>(user), operand,
                       order);
      break;
    case ValueKind::CopyAddrInst:
    case ValueKind::ExistentialMetatypeInst:
    case ValueKind::ValueMetatypeInst:
    case ValueKind::LoadInst:
    case ValueKind::OpenExistentialAddrInst:
    case ValueKind::ProjectBlockStorageInst:
      // These likely represent scenarios in which we're not generating
      // begin access markers. Ignore these for now. But we really should
      // add SIL verification to ensure all loads and stores have associated
      // access markers.
      break;
    default:
      // TODO: These requirements should be checked for in the SIL verifier.
      // This is an assertion rather than llvm_unreachable() because
      // it is likely the whitelist above for scenarios in which we'ren
      // not generating access markers is not comprehensive.
      assert(false && "Unrecognized argument use");
      break;
    }
  }
}

#ifndef NDEBUG
/// Sanity check to make sure that a noescape partial apply is
/// only ultimately used by an apply, a try_apply or as an argument (but not
/// the called function) in a partial_apply.
/// TODO: This really should be checked in the SILVerifier.
static bool isExpectedUseOfNoEscapePartialApply(SILInstruction *user) {
  if (!user)
    return true;

  // It is fine to call the partial apply
  if (isa<ApplyInst>(user) || isa<TryApplyInst>(user)) {
    return true;
  }

  if (isa<ConvertFunctionInst>(user)) {
    return isExpectedUseOfNoEscapePartialApply(user->getSingleUse()->getUser());
  }

  if (auto *PAI = dyn_cast<PartialApplyInst>(user)) {
    return user != PAI->getCallee();
  }

  return false;
}
#endif

void AccessSummaryAnalysis::processPartialApply(FunctionInfo *callerInfo,
                                                unsigned callerArgumentIndex,
                                                PartialApplyInst *apply,
                                                Operand *applyArgumentOperand,
                                                FunctionOrder &order) {
  SILFunction *calleeFunction = apply->getCalleeFunction();
  assert(calleeFunction && !calleeFunction->empty() &&
         "Missing definition of noescape closure?");

  // Make sure the partial_apply is not calling the result of another
  // partial_apply.
  assert(isa<FunctionRefInst>(apply->getCallee()) &&
         "Noescape partial apply of non-functionref?");

  SILInstruction *user = apply->getSingleUse()->getUser();
  assert(isExpectedUseOfNoEscapePartialApply(user) &&
         "noescape partial_apply has unexpected use!");
  (void)user;

  // The arguments to partial_apply are a suffix of the arguments to the
  // the actually-called function. Translate the index of the argument to
  // the partial_apply into to the corresponding index into the arguments of
  // the called function.

  // The argument index in the called function.
  ApplySite site(apply);
  unsigned calleeArgumentIndex = site.getCalleeArgIndex(*applyArgumentOperand);

  processCall(callerInfo, callerArgumentIndex, calleeFunction,
              calleeArgumentIndex, order);
}

void AccessSummaryAnalysis::processFullApply(FunctionInfo *callerInfo,
                                             unsigned callerArgumentIndex,
                                             FullApplySite apply,
                                             Operand *argumentOperand,
                                             FunctionOrder &order) {
  unsigned operandNumber = argumentOperand->getOperandNumber();
  assert(operandNumber > 0 && "Summarizing apply for non-argument?");

  unsigned calleeArgumentIndex = operandNumber - 1;
  SILFunction *callee = apply.getCalleeFunction();
  // We can't apply a summary for function whose body we can't see.
  // Since user-provided closures are always in the same module as their callee
  // This likely indicates a missing begin_access before an open-coded
  // call.
  if (!callee || callee->empty())
    return;

  processCall(callerInfo, callerArgumentIndex, callee, calleeArgumentIndex,
              order);
}

void AccessSummaryAnalysis::processCall(FunctionInfo *callerInfo,
                                        unsigned callerArgumentIndex,
                                        SILFunction *callee,
                                        unsigned argumentIndex,
                                        FunctionOrder &order) {
  // Record the flow of an argument from  the caller to the callee so that
  // the interprocedural analysis can iterate to a fixpoint.
  FunctionInfo *calleeInfo = getFunctionInfo(callee);
  ArgumentFlow flow = {callerArgumentIndex, argumentIndex, calleeInfo};
  callerInfo->recordFlow(flow);
  if (!calleeInfo->isVisited()) {
    processFunction(calleeInfo, order);
    order.tryToSchedule(calleeInfo);
  }

  propagateFromCalleeToCaller(callerInfo, flow);
}

bool AccessSummaryAnalysis::ArgumentSummary::mergeWith(SILAccessKind otherKind,
                                                        SILLocation otherLoc) {
  // In the lattice, a modification-like accesses subsume a read access or no
  // access.
  if (!Kind.hasValue() ||
      (*Kind == SILAccessKind::Read && otherKind != SILAccessKind::Read)) {
    Kind = otherKind;
    AccessLoc = otherLoc;
    return true;
  }

  return false;
}

bool AccessSummaryAnalysis::ArgumentSummary::mergeWith(
    const ArgumentSummary &other) {
  if (other.Kind.hasValue())
    return mergeWith(*other.Kind, other.AccessLoc);
  return false;
}

void AccessSummaryAnalysis::recompute(FunctionInfo *initial) {
  allocNewUpdateID();

  FunctionOrder order(getCurrentUpdateID());

  // Summarize the function and its callees.
  processFunction(initial, order);

  // Build the bottom-up order.
  order.tryToSchedule(initial);
  order.finishScheduling();

  // Iterate the interprocedural analysis to a fixed point.
  bool needAnotherIteration;
  do {
    needAnotherIteration = false;
    for (FunctionInfo *calleeInfo : order) {
      for (const auto &callerEntry : calleeInfo->getCallers()) {
        assert(callerEntry.isValid());
        if (!order.wasRecomputedWithCurrentUpdateID(calleeInfo))
          continue;

        FunctionInfo *callerInfo = callerEntry.Caller;

        // Propagate from callee to caller.
        for (const auto &argumentFlow : callerInfo->getArgumentFlows()) {
          if (argumentFlow.CalleeFunctionInfo != calleeInfo)
            continue;

          bool changed = propagateFromCalleeToCaller(callerInfo, argumentFlow);
          if (changed && !callerInfo->isScheduledAfter(calleeInfo)) {
            needAnotherIteration = true;
          }
        }
      }
    }
  } while (needAnotherIteration);
}

StringRef AccessSummaryAnalysis::ArgumentSummary::getDescription() const {
  if (Optional<SILAccessKind> kind = getAccessKind()) {
    return getSILAccessKindName(*kind);
  }

  return "none";
}

bool AccessSummaryAnalysis::propagateFromCalleeToCaller(
    FunctionInfo *callerInfo, ArgumentFlow flow) {
  // For a given flow from a caller's argument to a callee's argument,
  // propagate the argument summary information to the caller.

  FunctionInfo *calleeInfo = flow.CalleeFunctionInfo;
  const auto &calleeArgument =
      calleeInfo->getSummary().getAccessForArgument(flow.CalleeArgumentIndex);
  auto &callerArgument =
      callerInfo->getSummary().getAccessForArgument(flow.CallerArgumentIndex);

  bool changed = callerArgument.mergeWith(calleeArgument);
  return changed;
}

AccessSummaryAnalysis::FunctionInfo *
AccessSummaryAnalysis::getFunctionInfo(SILFunction *F) {
  FunctionInfo *&FInfo = FunctionInfos[F];
  if (!FInfo) {
    FInfo = new (Allocator.Allocate()) FunctionInfo(F);
  }
  return FInfo;
}

const AccessSummaryAnalysis::FunctionSummary &
AccessSummaryAnalysis::getOrCreateSummary(SILFunction *fn) {
  FunctionInfo *info = getFunctionInfo(fn);
  if (!info->isValid())
    recompute(info);

  return info->getSummary();
}

void AccessSummaryAnalysis::AccessSummaryAnalysis::invalidate() {
  FunctionInfos.clear();
  Allocator.DestroyAll();
  delete SubPathTrie;
  SubPathTrie = new IndexTrieNode();
}

void AccessSummaryAnalysis::invalidate(SILFunction *F, InvalidationKind K) {
  FunctionInfos.erase(F);
}

SILAnalysis *swift::createAccessSummaryAnalysis(SILModule *M) {
  return new AccessSummaryAnalysis();
}

raw_ostream &swift::
operator<<(raw_ostream &os,
           const AccessSummaryAnalysis::FunctionSummary &summary) {
  unsigned argCount = summary.getArgumentCount();
  os << "(";

  if (argCount > 0) {
    os << summary.getAccessForArgument(0).getDescription();
    for (unsigned i = 1; i < argCount; i++) {
      os << ",  " << summary.getAccessForArgument(i).getDescription();
    }
  }

  os << ")";
  return os;
}
