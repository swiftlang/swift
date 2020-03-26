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
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SILOptimizer/Analysis/AccessSummaryAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SIL/DebugUtils.h"

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
    ++index;

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
    case SILInstructionKind::BeginAccessInst: {
      auto *BAI = cast<BeginAccessInst>(user);
      if (BAI->getEnforcement() != SILAccessEnforcement::Unsafe) {
        const IndexTrieNode *subPath = findSubPathAccessed(BAI);
        summary.mergeWith(BAI->getAccessKind(), BAI->getLoc(), subPath);
        // We don't add the users of the begin_access to the worklist because
        // even if these users eventually begin an access to the address
        // or a projection from it, that access can't begin more exclusive
        // access than this access -- otherwise it will be diagnosed
        // elsewhere.
      }
      break;
    }
    case SILInstructionKind::EndUnpairedAccessInst:
      // Don't diagnose unpaired access statically.
      assert(cast<EndUnpairedAccessInst>(user)->getEnforcement() ==
             SILAccessEnforcement::Dynamic);
      break;
    case SILInstructionKind::StructElementAddrInst:
    case SILInstructionKind::TupleElementAddrInst: {
      // Eventually we'll summarize individual struct elements separately.
      // For now an access to a part of the struct is treated as an access
      // to the whole struct.
      auto inst = cast<SingleValueInstruction>(user);
      worklist.append(inst->use_begin(), inst->use_end());
      break;
    }
    case SILInstructionKind::DebugValueAddrInst:
    case SILInstructionKind::AddressToPointerInst:
      // Ignore these uses, they don't affect formal accesses.
      break;
    case SILInstructionKind::PartialApplyInst:
      processPartialApply(info, argumentIndex, cast<PartialApplyInst>(user),
                          operand, order);
      break;
    case SILInstructionKind::ApplyInst:
      processFullApply(info, argumentIndex, cast<ApplyInst>(user), operand,
                       order);
      break;
    case SILInstructionKind::TryApplyInst:
      processFullApply(info, argumentIndex, cast<TryApplyInst>(user), operand,
                       order);
      break;
    default:
      // FIXME: These likely represent scenarios in which we're not generating
      // begin access markers. Ignore these for now. But we really should
      // add SIL verification to ensure all loads and stores have associated
      // access markers. Once SIL verification is implemented, enable the
      // following assert to verify that the cases handled above are
      // comprehensive, which guarantees that exclusivity enforcement is
      // complete.
      //   assert(false && "Unrecognized argument use");
      break;
    }
  }
}

#ifndef NDEBUG
/// Sanity check to make sure that a noescape partial apply is
/// only ultimately used by an apply, a try_apply or as an argument (but not
/// the called function) in a partial_apply.
///
/// FIXME: This needs to be checked in the SILVerifier.
static bool hasExpectedUsesOfNoEscapePartialApply(Operand *partialApplyUse) {
  SILInstruction *user = partialApplyUse->getUser();

  // It is fine to call the partial apply
  switch (user->getKind()) {
  case SILInstructionKind::ApplyInst:
  case SILInstructionKind::TryApplyInst:
    return true;
  // partial_apply [stack] is terminated by a dealloc_stack.
  case SILInstructionKind::DeallocStackInst:
    return true;

  case SILInstructionKind::ConvertFunctionInst:
    return llvm::all_of(cast<ConvertFunctionInst>(user)->getUses(),
                        hasExpectedUsesOfNoEscapePartialApply);

  case SILInstructionKind::ConvertEscapeToNoEscapeInst:
    return llvm::all_of(cast<ConvertEscapeToNoEscapeInst>(user)->getUses(),
                        hasExpectedUsesOfNoEscapePartialApply);

  case SILInstructionKind::PartialApplyInst:
    return partialApplyUse->get() != cast<PartialApplyInst>(user)->getCallee();

  // Look through begin_borrow.
  case SILInstructionKind::BeginBorrowInst:
    return llvm::all_of(cast<BeginBorrowInst>(user)->getUses(),
                        hasExpectedUsesOfNoEscapePartialApply);

  // Look through mark_dependence.
  case SILInstructionKind::MarkDependenceInst:
    return llvm::all_of(cast<MarkDependenceInst>(user)->getUses(),
                        hasExpectedUsesOfNoEscapePartialApply);

  case SILInstructionKind::CopyBlockWithoutEscapingInst:
    return partialApplyUse->getOperandNumber() ==
           CopyBlockWithoutEscapingInst::Closure;

  // A copy_value that is only used by the store to a block storage is fine.
  // It is part of the pattern we emit for verifying that a noescape closure
  // passed to objc has not escaped.
  //  %4 = convert_escape_to_noescape [not_guaranteed] %3 :
  //    $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
  //  %5 = function_ref @withoutEscapingThunk
  //  %6 = partial_apply [callee_guaranteed] %5(%4) :
  //    $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  //  %7 = mark_dependence %6 : $@callee_guaranteed () -> () on %4 :
  //    $@noescape @callee_guaranteed () -> ()
  //  %8 = copy_value %7 : $@callee_guaranteed () -> ()
  //  %9 = alloc_stack $@block_storage @callee_guaranteed () -> ()
  //  %10 = project_block_storage %9 :
  //    $*@block_storage @callee_guaranteed () -> ()
  //  store %8 to [init] %10 : $*@callee_guaranteed () -> ()
  //  %13 = init_block_storage_header %9 :
  //    $*@block_storage @callee_guaranteed () -> (),
  //    invoke %12
  //  %14 = copy_block_without_escaping %13 : $() -> () withoutEscaping %7
  case SILInstructionKind::CopyValueInst:
    return isa<StoreInst>(getSingleNonDebugUser(cast<CopyValueInst>(user)));

  // End borrow is always ok.
  case SILInstructionKind::EndBorrowInst:
    return true;

  case SILInstructionKind::StoreInst:
  case SILInstructionKind::DestroyValueInst:
    // @block_storage is passed by storing it to the stack. We know this is
    // still nonescaping simply because our original argument convention is
    // @inout_aliasable. In this SIL, both store and destroy_value are users
    // of %closure:
    //
    // %closure = partial_apply %f1(%arg)
    //   : $@convention(thin) (@inout_aliasable T) -> ()
    // %storage = alloc_stack $@block_storage @callee_owned () -> ()
    // %block_addr = project_block_storage %storage
    //   : $*@block_storage @callee_owned () -> ()
    // store %closure to [init] %block_addr : $*@callee_owned () -> ()
    // %block = init_block_storage_header %storage
    //     : $*@block_storage @callee_owned () -> (),
    //   invoke %f2 : $@convention(c)
    //     (@inout_aliasable @block_storage @callee_owned () -> ()) -> (),
    //   type $@convention(block) () -> ()
    // %copy = copy_block %block : $@convention(block) () -> ()
    // destroy_value %storage : $@callee_owned () -> ()
    return true;
  default:
    return false;
  }
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
  assert(isa<FunctionRefBaseInst>(apply->getCallee())
         && "Noescape partial apply of non-functionref?");

  assert(llvm::all_of(apply->getUses(),
                      hasExpectedUsesOfNoEscapePartialApply) &&
         "noescape partial_apply has unexpected use!");

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

bool AccessSummaryAnalysis::ArgumentSummary::mergeWith(
    SILAccessKind otherKind, SILLocation otherLoc,
    const IndexTrieNode *otherSubPath) {
  bool changed = false;

  auto found =
      SubAccesses.try_emplace(otherSubPath, otherKind, otherLoc, otherSubPath);
  if (!found.second) {
    // We already have an entry for otherSubPath, so merge with it.
    changed = found.first->second.mergeWith(otherKind, otherLoc, otherSubPath);
  } else {
    // We just added a new entry for otherSubPath.
    changed = true;
  }

  return changed;
}

bool AccessSummaryAnalysis::ArgumentSummary::mergeWith(
    const ArgumentSummary &other) {
  bool changed = false;

  const SubAccessMap &otherAccesses = other.SubAccesses;
  for (auto it = otherAccesses.begin(), e = otherAccesses.end(); it != e;
       ++it) {
    const SubAccessSummary &otherSubAccess = it->getSecond();
    if (mergeWith(otherSubAccess.getAccessKind(), otherSubAccess.getAccessLoc(),
                  otherSubAccess.getSubPath())) {
      changed = true;
    }
  }

  return changed;
}

bool AccessSummaryAnalysis::SubAccessSummary::mergeWith(
    SILAccessKind otherKind, SILLocation otherLoc,
    const IndexTrieNode *otherSubPath) {
  assert(otherSubPath == this->SubPath);
  // In the lattice, a modification-like accesses subsume a read access or no
  // access.
  if (Kind == SILAccessKind::Read && otherKind != SILAccessKind::Read) {
    Kind = otherKind;
    AccessLoc = otherLoc;
    return true;
  }

  return false;
}

bool AccessSummaryAnalysis::SubAccessSummary::mergeWith(
    const SubAccessSummary &other) {
  // We don't currently support merging accesses for different sub paths.
  assert(SubPath == other.SubPath);
  return mergeWith(other.Kind, other.AccessLoc, SubPath);
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

std::string AccessSummaryAnalysis::SubAccessSummary::getDescription(
    SILType BaseType, SILModule &M, TypeExpansionContext context) const {
  std::string sbuf;
  llvm::raw_string_ostream os(sbuf);

  os << AccessSummaryAnalysis::getSubPathDescription(BaseType, SubPath, M,
                                                     context);

  if (!SubPath->isRoot())
    os << " ";
  os << getSILAccessKindName(getAccessKind());
  return os.str();
}

void AccessSummaryAnalysis::ArgumentSummary::getSortedSubAccesses(
    SmallVectorImpl<SubAccessSummary> &storage) const {
  for (auto it = SubAccesses.begin(), e = SubAccesses.end(); it != e; ++it) {
    storage.push_back(it->getSecond());
  }

  const auto &compare = [](const SubAccessSummary &lhs,
                           const SubAccessSummary &rhs) {
    return compareSubPaths(lhs.getSubPath(), rhs.getSubPath());
  };
  std::sort(storage.begin(), storage.end(), compare);

  assert(storage.size() == SubAccesses.size());
}

std::string AccessSummaryAnalysis::ArgumentSummary::getDescription(
    SILType BaseType, SILModule &M, TypeExpansionContext context) const {
  std::string sbuf;
  llvm::raw_string_ostream os(sbuf);
  os << "[";
  unsigned index = 0;

  SmallVector<AccessSummaryAnalysis::SubAccessSummary, 8> Sorted;
  Sorted.reserve(SubAccesses.size());
  getSortedSubAccesses(Sorted);

  for (auto &subAccess : Sorted) {
    if (index > 0) {
      os << ", ";
    }
    os << subAccess.getDescription(BaseType, M, context);
    ++index;
  }
  os << "]";

  return os.str();
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
  SubPathTrie.reset(new IndexTrieNode());
}

void AccessSummaryAnalysis::invalidate(SILFunction *F, InvalidationKind K) {
  FunctionInfos.erase(F);
}

SILAnalysis *swift::createAccessSummaryAnalysis(SILModule *M) {
  return new AccessSummaryAnalysis();
}

/// If the instruction is a field or tuple projection and it has a single
/// user return a pair of the single user and the projection index.
/// Otherwise, return a pair with the component nullptr and the second
/// unspecified.
static std::pair<SingleValueInstruction *, unsigned>
getSingleAddressProjectionUser(SingleValueInstruction *I) {
  SingleValueInstruction *SingleUser = nullptr;
  unsigned ProjectionIndex = 0;

  for (Operand *Use : I->getUses()) {
    SILInstruction *User = Use->getUser();
    if (isa<BeginAccessInst>(I) && isa<EndAccessInst>(User))
      continue;

    // Ignore sanitizer instrumentation when looking for a single projection
    // user. This ensures that we're able to find a single projection subpath
    // even when sanitization is enabled.
    if (isSanitizerInstrumentation(User))
      continue;

    // We have more than a single user so bail.
    if (SingleUser)
      return std::make_pair(nullptr, 0);

    switch (User->getKind()) {
    case SILInstructionKind::StructElementAddrInst: {
      auto inst = cast<StructElementAddrInst>(User);
      ProjectionIndex = inst->getFieldNo();
      SingleUser = inst;
      break;
    }
    case SILInstructionKind::TupleElementAddrInst: {
      auto inst = cast<TupleElementAddrInst>(User);
      ProjectionIndex = inst->getFieldNo();
      SingleUser = inst;
      break;
    }
    default:
      return std::make_pair(nullptr, 0);
    }
  }

  return std::make_pair(SingleUser, ProjectionIndex);
}

const IndexTrieNode *
AccessSummaryAnalysis::findSubPathAccessed(BeginAccessInst *BAI) {
  IndexTrieNode *SubPath = getSubPathTrieRoot();

  // For each single-user projection of BAI, construct or get a node
  // from the trie representing the index of the field or tuple element
  // accessed by that projection.
  SingleValueInstruction *Iter = BAI;
  while (true) {
    std::pair<SingleValueInstruction *, unsigned> ProjectionUser =
        getSingleAddressProjectionUser(Iter);
    if (!ProjectionUser.first)
      break;

    SubPath = SubPath->getChild(ProjectionUser.second);
    Iter = ProjectionUser.first;
  }

  return SubPath;
}

/// Returns a string representation of the SubPath
/// suitable for use in diagnostic text. Only supports the Projections
/// that stored-property relaxation supports: struct stored properties
/// and tuple elements.
std::string AccessSummaryAnalysis::getSubPathDescription(
    SILType baseType, const IndexTrieNode *subPath, SILModule &M,
    TypeExpansionContext context) {
  // Walk the trie to the root to collect the sequence (in reverse order).
  llvm::SmallVector<unsigned, 4> reversedIndices;
  const IndexTrieNode *I = subPath;
  while (!I->isRoot()) {
    reversedIndices.push_back(I->getIndex());
    I = I->getParent();
  }

  std::string sbuf;
  llvm::raw_string_ostream os(sbuf);

  SILType containingType = baseType;
  for (unsigned index : llvm::reverse(reversedIndices)) {
    os << ".";

    if (StructDecl *D = containingType.getStructOrBoundGenericStruct()) {
      VarDecl *var = D->getStoredProperties()[index];
      os << var->getBaseName();
      containingType = containingType.getFieldType(var, M, context);
      continue;
    }

    if (auto tupleTy = containingType.getAs<TupleType>()) {
      Identifier elementName = tupleTy->getElement(index).getName();
      if (elementName.empty())
        os << index;
      else
        os << elementName;
      containingType = containingType.getTupleElementType(index);
      continue;
    }

    llvm_unreachable("Unexpected type in projection SubPath!");
  }

  return os.str();
}

static unsigned subPathLength(const IndexTrieNode *subPath) {
  unsigned length = 0;

  const IndexTrieNode *iter = subPath;
  while (iter) {
    ++length;
    iter = iter->getParent();
  }

  return length;
}

bool AccessSummaryAnalysis::compareSubPaths(const IndexTrieNode *lhs,
                                            const IndexTrieNode *rhs) {
  unsigned lhsLength = subPathLength(lhs);
  unsigned rhsLength = subPathLength(rhs);

  if (lhsLength != rhsLength)
    return lhsLength < rhsLength;


  while (lhs) {
    if (lhs->getIndex() != rhs->getIndex())
      return lhs->getIndex() < rhs->getIndex();

    lhs = lhs->getParent();
    rhs = rhs->getParent();
  }

  assert(!rhs && "Equal paths with different lengths?");
  // The two paths are equal.
  return false;
}

void AccessSummaryAnalysis::FunctionSummary::print(raw_ostream &os,
                                                   SILFunction *fn) const {
  unsigned argCount = getArgumentCount();
  os << "(";

  for (unsigned i = 0; i < argCount; ++i) {
    if (i > 0) {
      os << ",  ";
    }
    SILArgument *arg = fn->getArgument(i);
    SILModule &m = fn->getModule();
    os << getAccessForArgument(i).getDescription(arg->getType(), m,
                                                 TypeExpansionContext(*fn));
  }

  os << ")";
}
