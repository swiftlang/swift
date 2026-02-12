//===--- DiagnoseInvalidEscapingCaptures.cpp ------------------------------===//
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
// This file implements a diagnostic pass to diagnose escaping closures that
// capture mutable storage locations or noescape function values.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/LayoutConstraint.h"
#include "swift/SIL/SILValue.h"
#define DEBUG_TYPE "sil-diagnose-invalid-escaping-captures"

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SILLocation loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc.getSourceLoc(), diag,
                                std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc, diag,
                                std::forward<U>(args)...);
}

// Check if a use of a value derived from a partial_apply can cause the
// closure to escape. For "pass-through" uses that build a new value from
// the partial_apply, followUses() is called to evaluate the uses of the
// derived value.
template <typename FollowUse>
static bool checkNoEscapePartialApplyUse(Operand *oper, FollowUse followUses) {
  SILInstruction *user = oper->getUser();

  if (isa<ConvertEscapeToNoEscapeInst>(user) ||
      isa<CopyBlockWithoutEscapingInst>(user))
    return false;

  // Look through copies, borrows, and conversions.
  // getSingleValueCopyOrCast handles all result producing instructions for
  // which onlyAffectsRefCount returns true.
  if (SingleValueInstruction *copy = getSingleValueCopyOrCast(user)) {
    // Only follow the copied operand. Other operands are incidental,
    // as in the second operand of mark_dependence.
    if (oper->getOperandNumber() == 0)
      followUses(copy);

    return false;
  }

  // Ignore uses that are totally uninteresting. partial_apply [stack] is
  // terminated by a dealloc_stack instruction.
  if (isIncidentalUse(user) || onlyAffectsRefCount(user) ||
      isa<DeallocStackInst>(user)) {
    assert(user->getNumResults() == 0);
    return false;
  }

  // Before checking conversions in general below (getSingleValueCopyOrCast),
  // check for convert_function to [without_actually_escaping]. Assume such
  // conversion are not actually escaping without following their uses.
  if (auto *CFI = dyn_cast<ConvertFunctionInst>(user)) {
    if (CFI->withoutActuallyEscaping())
      return false;
  }

  // Look through `differentiable_function`.
  if (auto *DFI = dyn_cast<DifferentiableFunctionInst>(user)) {
    followUses(DFI);
    return false;
  }

  // @noescape block storage can be passed as an Optional (Nullable).
  if (auto *EI = dyn_cast<EnumInst>(user)) {
    followUses(EI);
    return false;
  }

  // Look through Phis.
  if (auto *BI = dyn_cast<BranchInst>(user)) {
    const SILPhiArgument *arg = BI->getArgForOperand(oper);
    followUses(arg);
    return false;
  }

  if (auto *CBI = dyn_cast<CondBranchInst>(user)) {
    const SILPhiArgument *arg = CBI->getArgForOperand(oper);
    if (arg) // If the use isn't the branch condition, follow it.
      followUses(arg);
    return false;
  }

  // Look through ObjC closures.
  if (auto *SI = dyn_cast<StoreInst>(user)) {
    if (oper->getOperandNumber() == StoreInst::Src) {
      if (auto *PBSI = dyn_cast<ProjectBlockStorageInst>(
            SI->getDest())) {
        SILValue storageAddr = PBSI->getOperand();
        // The closure is stored to block storage. Recursively visit all
        // uses of any initialized block storage values derived from this
        // storage address..
        for (Operand *oper : storageAddr->getUses()) {
          if (auto *IBS = dyn_cast<InitBlockStorageHeaderInst>(oper->getUser()))
            followUses(IBS);
        }
        return false;
      }
    }
  }

  if (auto *PAI = dyn_cast<PartialApplyInst>(user)) {
    // Recurse through partial_apply chains.
    if (oper->get() == PAI->getCallee()) {
      followUses(PAI);
      return false;
    }

    // Look through re-abstraction thunks.
    if (isPartialApplyOfReabstractionThunk(PAI)) {
      // However, first check for withoutActuallyEscaping, which is always
      // a valid non-escaping use.
      SILFunction *thunkDef = PAI->getReferencedFunctionOrNull();
      if (!thunkDef)
        return true;
      if (!thunkDef->isWithoutActuallyEscapingThunk())
        followUses(PAI);
      return false;
    }
  }

  // Anything else is flagged as an escaping use.
  return true;
}

/// Given an \p instruction try to find the argument pack that is allocated
/// to it as a best effort. If no appropriate value can be found, return
/// an empty \c SILValue.
/// 
/// While any instruction *could* be passed, this pattern matching was only written
/// with the following patterns in mind, which start with alloc_stack and load.
/// 
/// Given an escaping use of a pack:
/// 
/// ```swift
/// func foo<each T>(_ fn: repeat () -> each T) {
///   escapes {
///     let _ = (repeat each fn)
///   }
/// }
/// ```
/// 
/// We start at
/// 
/// ```
/// %3 = alloc_stack $(repeat @noescape <τ_0_0> () -> @out τ_0_0 for <each T>) // users: %20, %18, %14
/// ```
/// 
/// And need to navigate
/// 
/// ```
/// // %0 "fn"                                        // users: %13, %1
/// bb0(%0 : $*Pack{repeat @noescape <τ_0_0> () -> @out τ_0_0 for <each T>}):
/// // ...
///   %3 = alloc_stack $(repeat @noescape <τ_0_0> () -> @out τ_0_0 for <each T>) // users: %20, %18, %14
/// // ...
///   %13 = pack_element_get %11 of %0 as $*@noescape <τ_0_0> () -> @out τ_0_0 for <each T> // user: %15
///   %14 = tuple_pack_element_addr %11 of %3 as $*@noescape <τ_0_0> () -> @out τ_0_0 for <each T> // user: %15
///   copy_addr %13 to [init] %14                     // id: %15
/// // ...
/// ```
/// 
/// Taking a path like
/// 
/// ```
///     %3 alloc_stack             [get users]
/// -> %14 tuple_pack_element_addr [get users] (since tuple isn't an arg!)
/// -> %15 copy_addr               [get src inst]
/// -> %13 pack_element_get        [get pack arg]
/// ->  %0 "fn"
/// ```
/// 
/// We can follow use to user, until we hit copy_addr and need to find the
/// source of the copy. When we hit pack_element_get, the pack operand is the
/// argument "fn" and we are done. Defer within an escaping function and pack
/// iteration follows this pattern. The arg check on tuple_pack_element_addr exists
/// for the next case, the use of a pack inside defer:
/// 
/// ```swift
/// func foo<each T>(_ fn: repeat () -> each T) {
///   defer {
///     escapes {
///       let _ = (repeat each fn)
///     }
///   }
///   _ = 42
/// }
/// ```
/// 
/// We start at
/// 
/// ```
/// %17 = alloc_stack $(repeat @noescape <τ_0_0> () -> @out τ_0_0 for <each T>) // users: %34, %32, %28
/// ```
/// 
/// And need to navigate
/// 
/// ```
/// // %0 "fn"                                        // user: %11
/// bb0(%0 : @closureCapture $*(repeat @noescape <τ_0_0> () -> @out τ_0_0 for <each T>)):
///   %1 = alloc_pack $Pack{repeat @noescape <τ_0_0> () -> @out τ_0_0 for <each T>} // users: %38, %27, %15, %12
/// // ...
///   %11 = tuple_pack_element_addr %9 of %0 as $*@noescape <τ_0_0> () -> @out τ_0_0 for <each T> // user: %12
///   pack_element_set %11 into %9 of %1              // id: %12
/// // ...
///   %17 = alloc_stack $(repeat @noescape <τ_0_0> () -> @out τ_0_0 for <each T>) // users: %34, %32, %28
/// // ...
///   %25 = dynamic_pack_index %22 of $Pack{repeat () -> each T} // users: %28, %27, %26
///   %26 = open_pack_element %25 of <each T> at <Pack{repeat each T}>, shape $each T // users: %28, %27
///   %27 = pack_element_get %25 of %1 as $*@noescape <τ_0_0> () -> @out τ_0_0 for <each T> // user: %29
///   %28 = tuple_pack_element_addr %25 of %17 as $*@noescape <τ_0_0> () -> @out τ_0_0 for <each T> // user: %29
///   copy_addr %27 to [init] %28                     // id: %29
/// // ...
/// ```
/// 
/// Taking a path like
/// 
/// ```
///    %17 alloc_stack             [get users]
/// -> %28 tuple_pack_element_addr [get users]      (since tuple isn't an arg!)
/// -> %29 copy_addr               [get src inst]
/// -> %27 pack_element_get        [get pack users] (since pack isn't an arg!)
/// ->  %1 alloc_pack              [get users]
/// -> %12 pack_element_set        [get value users]
/// -> %11 tuple_pack_element_addr [get tuple arg]
/// ->  %0 "fn"
/// ```
/// 
/// The same logic applies, until we hit pack_element_get, where defer introduces
/// additional indirection. We need to find the allocation of the pack it is
/// accessing, then find it's users to discover the addr instruction, which
/// will have the argument "fn" as the tuple operand. We need to support this
/// indirection. You can see that we hit two different pack instructions, and finish
/// on a tuple_pack_element_addr instruction, not a pack_element_get instruction.
/// 
/// One more case (that I'm aware of) exists, where an escaping capture is repeated:
/// 
/// ```swift
/// repeat escapes {
///   (each fn)()
/// }
/// ```
/// 
/// ```
/// %13 = load [copy] %12 : $*@noescape <τ_0_0> () -> @out τ_0_0 for <each T> // user: %14
/// ```
/// 
/// ```
/// // %0 "fn"                                        // users: %12, %1
/// bb0(%0 : $*Pack{repeat @noescape <τ_0_0> () -> @out τ_0_0 for <each T>}):
/// // ...
///   %12 = pack_element_get %9 of %0 as $*@noescape <τ_0_0> () -> @out τ_0_0 for <each T> // user: %13
///   %13 = load [copy] %12                           // user: %14
/// ```
/// 
/// With a nice and simple path:
/// 
/// ```
///    %13 load [get src inst]
/// -> %12 pack_element_get [get pack arg]
/// -> %0 "fn"
/// ```
/// 
/// We follow the src of the load, like copy_addr, although load is single operand.
static const SILValue tryGetPackFromIntermediaryInstruction(SILInstruction *instruction) {
  InstructionWorklist worklist(instruction);
  while (SILInstruction *inst = worklist.pop()) {
    // Follow the users of the stack allocation.
    if (auto *allocStack = dyn_cast<AllocStackInst>(inst)) {
      worklist.pushInstructionsIfNotVisited(allocStack->getUsers());
      continue;
    }

    // Follow the users of the pack allocation.
    if (auto *allocPack = dyn_cast<AllocPackInst>(inst)) {
      worklist.pushInstructionsIfNotVisited(allocPack->getUsers());
      continue;
    }

    // Follow the source of copy addr.
    if (auto *copyAddr = dyn_cast<CopyAddrInst>(inst)) {
      worklist.pushIfNotVisited(copyAddr->getSrc()->getDefiningInstruction());
      continue;
    }

    // Follow the source of load inst.
    if (auto *load = dyn_cast<LoadInst>(inst)) {
      worklist.pushIfNotVisited(load->getOperand()->getDefiningInstruction());
      continue;
    }

    // Follow the value of a pack set, ideally to a tuple pack element addr
    // inst.
    if (auto *packElemSet = dyn_cast<PackElementSetInst>(inst)) {
      worklist.pushIfNotVisited(
          packElemSet->getValue()->getDefiningInstruction());
      continue;
    }

    // TuplePackElementAddrInst may have a function argument as its tuple
    // operand's definition. Otherwise, follow the users of the address.
    if (auto *tuplePackElemAddr = dyn_cast<TuplePackElementAddrInst>(inst)) {
      if (auto *arg = dyn_cast<SILArgument>(tuplePackElemAddr->getTuple())) {
        return arg;
      }
      worklist.pushInstructionsIfNotVisited(tuplePackElemAddr->getUsers());
      continue;
    }

    // PackElementGetInst may have a function argument as its pack operand's
    // definition. Otherwise, follow to the definition of the pack.
    if (auto *packElemGet = dyn_cast<PackElementGetInst>(inst)) {
      if (auto *arg = dyn_cast<SILArgument>(packElemGet->getPack())) {
        return arg;
      }
      worklist.pushIfNotVisited(
          packElemGet->getPack()->getDefiningInstruction());
      continue;
    }
  }

  return SILValue();
}

const ParamDecl *getParamDeclFromOperand(SILValue value) {
  while (true) {
    // Look through mark must check.
    if (auto *mmci = dyn_cast<MarkUnresolvedNonCopyableValueInst>(value)) {
      value = mmci->getOperand();
      continue;
    }

    // Look through copies.
    if (auto *ci = dyn_cast<CopyValueInst>(value)) {
      value = ci->getOperand();
      continue;
    }

    // Try to find the pack that is stored in a temporary stack allocation or
    // accessed in a load.
    if (isa<AllocStackInst>(value) || isa<LoadInst>(value)) {
      value = tryGetPackFromIntermediaryInstruction(
          value->getDefiningInstruction());
      if (value) // Only continue if we got a non-empty SILValue.
        continue;
      // If we weren't able to find a pack, return early; we won't be able to
      // find the param declaration.
      return nullptr;
    }

    break;
  }

  if (auto *arg = dyn_cast<SILArgument>(value))
    if (auto *decl = dyn_cast_or_null<ParamDecl>(arg->getDecl()))
      return decl;

  return nullptr;
}

bool isUseOfSelfInInitializer(Operand *oper) {
  if (auto *PBI = dyn_cast<ProjectBoxInst>(oper->get())) {
    SILValue value = PBI->getOperand();
    if (auto *bbi = dyn_cast<BeginBorrowInst>(value)) {
      value = bbi->getOperand();
    }
    if (auto *MUI = dyn_cast<MarkUninitializedInst>(value)) {
      switch (MUI->getMarkUninitializedKind()) {
      case MarkUninitializedInst::Kind::Var:
      case MarkUninitializedInst::Kind::Out:
        return false;
      case MarkUninitializedInst::Kind::RootSelf:
      case MarkUninitializedInst::Kind::CrossModuleRootSelf:
      case MarkUninitializedInst::Kind::DerivedSelf:
      case MarkUninitializedInst::Kind::DerivedSelfOnly:
      case MarkUninitializedInst::Kind::DelegatingSelf:
      case MarkUninitializedInst::Kind::DelegatingSelfAllocated:
        return true;
      }

      llvm_unreachable("Bad MarkUninitializedInst::Kind");
    }
  }

  return false;
}

static bool checkForEscapingPartialApplyUses(PartialApplyInst *PAI) {
  LLVM_DEBUG(llvm::dbgs() << "Checking for escaping partial apply uses.\n");

  // Avoid exponential path exploration.
  SmallVector<Operand *, 8> uses;
  llvm::SmallDenseSet<Operand *, 8> visited;
  auto uselistInsert = [&](Operand *operand) {
    if (visited.insert(operand).second)
      uses.push_back(operand);
  };

  for (Operand *use : PAI->getUses())
    uselistInsert(use);


  // Search for any uses of the closure that might potentially escape.
  bool foundEscapingUse = false;
  while (!uses.empty()) {
    Operand *oper = uses.pop_back_val();
    LLVM_DEBUG(llvm::dbgs() << "Visiting user: " << *oper->getUser());
    bool localFoundEscapingUse = checkNoEscapePartialApplyUse(oper, [&](SILValue V) {
      for (Operand *use : V->getUses())
        uselistInsert(use);
    });
    LLVM_DEBUG(
        if (localFoundEscapingUse)
          llvm::dbgs() << "    Escapes!\n";
    );
    foundEscapingUse |= localFoundEscapingUse;
  }

  // If there aren't any, we're fine.
  return foundEscapingUse;
}

// Given a partial_apply forming a closure, together with one of its operands,
// find a usage of the corresponding argument inside the closure body, and
// diagnose it as a capture use.
//
// This makes a best-effort attempt at finding a "good" capture usage; it may
// not emit anything.
//
// The \c DC parameter is the DeclContext of the original function being
// analyzed by this diagnostic pass. We use it to distinguish calls of closures
// from calls of other unrelated functions, by checking the DeclContext of the
// called closure.
static void diagnoseCaptureLoc(ASTContext &Context, DeclContext *DC,
                               PartialApplyInst *PAI, Operand *oper) {
  assert(DC != nullptr &&
         "Invalid capture in function with no source location information");

  SmallVector<Operand *, 8> uses;
  llvm::SmallDenseSet<Operand *, 8> visited;
  auto uselistInsert = [&](Operand *operand) {
    if (visited.insert(operand).second)
      uses.push_back(operand);
  };

  auto lookInsideClosure = [&](ApplySite site, Operand *oper) -> bool {
    auto *F = site.getCalleeFunction();
    if (F == nullptr || F->empty())
      return false;

    auto *otherDC = F->getDeclContext();
    if (otherDC == nullptr || DC == nullptr ||
        !otherDC->isChildContextOf(DC))
      return false;

    // Map an operand of an apply instruction to an argument inside
    // the callee.
    auto args = F->getArguments();
    auto argIndex = site.getCalleeArgIndex(*oper);
    auto arg = args[argIndex];

    // Look for a usage of the callee argument.
    for (Operand *use : arg->getUses())
      uselistInsert(use);

    return true;
  };

  lookInsideClosure(PAI, oper);

  while (!uses.empty()) {
    Operand *oper = uses.pop_back_val();
    SILInstruction *user = oper->getUser();

    // Look through copy_value.
    if (auto *ci = dyn_cast<CopyValueInst>(user)) {
      for (auto *use : ci->getUses()) {
        uselistInsert(use);
      }
      continue;
    }

    if (isIncidentalUse(user) || onlyAffectsRefCount(user) ||
        isa<DeallocationInst>(user) || isa<DestroyAddrInst>(user))
      continue;

    // Look through mark must check inst.
    if (auto *mmci = dyn_cast<MarkUnresolvedNonCopyableValueInst>(user)) {
      for (auto *use : mmci->getUses())
        uselistInsert(use);
      continue;
    }

    // Look through copies, borrows, and conversions.
    if (SingleValueInstruction *copy = getSingleValueCopyOrCast(user)) {
      // Only follow the copied operand. Other operands are incidental,
      // as in the second operand of mark_dependence.
      if (oper->getOperandNumber() == 0) {
        for (auto *use : copy->getUses())
          uselistInsert(use);
        continue;
      }
    }

    // If the usage is a capture of the value by another closure, look inside
    // the body of that closure.
    if (auto site = ApplySite::isa(user)) {
      if (lookInsideClosure(site, oper)) {
        diagnose(Context, site.getLoc(), diag::value_captured_transitively);
        continue;
      }
    }

    // Look through indirect pack references. A function with a pack parameter
    // will get the address, set the pack, and later get and load the pack. We
    // want to follow this indirection until the load, so we can diagnose where
    // the pack is used, like `repeat each x`
    if (auto *svi = dyn_cast<SingleValueInstruction>(user)) {
      if (isa<TuplePackElementAddrInst>(user) ||
          isa<PackElementGetInst>(user)) {
        for (auto *use : svi->getUses())
          uselistInsert(use);
        continue;
      }
    }

    // Follow the uses of the pack that is set, to discover get/load
    // instructions
    if (auto *packElemSetInst = dyn_cast<PackElementSetInst>(user)) {
      for (auto *use : packElemSetInst->getPack()->getUses()) {
        uselistInsert(use);
      }
      continue;
    }

    // Follow indirection for defer
    if (auto *copyAddrInst = dyn_cast<CopyAddrInst>(user)) {
      // We came from the source, follow it to where its copied to
      if (oper->getOperandNumber() == CopyAddrInst::Src) {
        SILValue dest = copyAddrInst->getDest();

        // Walk through address projections to find the base
        while (true) {
          if (auto *tuplePackInst = dyn_cast<TuplePackElementAddrInst>(dest)) {
            dest = tuplePackInst->getTuple();
          } else if (auto *packInst = dyn_cast<PackElementGetInst>(dest)) {
            dest = packInst->getPack();
          } else {
            break;
          }
        }

        for (auto *use : dest->getUses())
          uselistInsert(use);
      }
      continue;
    }

    // Otherwise, we might have found one of the "real" usages of the capture.
    // Diagnose it here.
    SILValue val = oper->get();
    SILLocation loc = val.getLoc();
    if (loc.isASTNode<VarDecl>())
      loc = user->getLoc();
    diagnose(Context, loc, diag::value_captured_here);
  }
}

// Diagnose this partial_apply if it captures a non-escaping value and has
// an escaping use.
static void checkPartialApply(ASTContext &Context, DeclContext *DC,
                              PartialApplyInst *PAI) {
  // Re-abstraction thunks are not useful to look at. We'll diagnose the
  // original closure instead.
  if (isPartialApplyOfReabstractionThunk(PAI))
    return;

  LLVM_DEBUG(llvm::dbgs() << "Checking Partial Apply: " << *PAI);

  ApplySite apply(PAI);

  // Collect any non-escaping captures.
  SmallVector<Operand *, 2> inoutCaptures;
  SmallVector<Operand *, 2> noEscapeCaptures;

  for (auto &oper : apply.getArgumentOperands()) {
    SILValue value = oper.get();

    // Captures of inout parameters cannot escape.
    if (apply.getArgumentConvention(oper)
            == SILArgumentConvention::Indirect_InoutAliasable)
      inoutCaptures.push_back(&oper);

    // Captures of noescape function types or tuples containing noescape
    // function types cannot escape.
    if (value->getType().containsNoEscapeFunction())
      noEscapeCaptures.push_back(&oper);
  }

  // A partial_apply without non-escaping captures is always valid.
  if (inoutCaptures.empty() && noEscapeCaptures.empty())
    return;

  // A partial_apply without escaping uses is always valid.
  if (!checkForEscapingPartialApplyUses(PAI))
    return;

  // Otherwise, we have at least one escaping use of a partial_apply
  // capturing a non-escaping value. We need to emit diagnostics.

  // Should match SELECT_ESCAPING_CLOSURE_KIND in DiagnosticsSIL.def.
  enum {
    EscapingLocalFunction,
    EscapingClosure,
    EscapingAutoClosure,
  } functionKind = EscapingClosure;

  if (auto *F = PAI->getReferencedFunctionOrNull()) {
    if (auto loc = F->getLocation()) {
      if (loc.isASTNode<FuncDecl>()) {
        functionKind = EscapingLocalFunction;
      } else if (loc.isASTNode<AutoClosureExpr>()) {
        functionKind = EscapingAutoClosure;
      }
    }
  }

  bool emittedError = false;

  // First, diagnose the inout captures, if any.
  for (auto inoutCapture : inoutCaptures) {
    std::optional<Identifier> paramName = std::nullopt;
    if (isUseOfSelfInInitializer(inoutCapture)) {
      emittedError = true;
      diagnose(Context, PAI->getLoc(), diag::escaping_mutable_self_capture,
               functionKind);
    } else {
      auto *param = getParamDeclFromOperand(inoutCapture->get());
      if (param->isSelfParameter()) {
        emittedError = true;
        diagnose(Context, PAI->getLoc(), diag::escaping_mutable_self_capture,
                 functionKind);
      } else {
        emittedError = true;
        paramName = param->getName();
        diagnose(Context, PAI->getLoc(), diag::escaping_inout_capture,
                 functionKind, param->getName());
        diagnose(Context, param->getLoc(), diag::inout_param_defined_here,
                 param->getName());
      }
    }
    if (functionKind != EscapingAutoClosure) {
      emittedError = true;
      diagnoseCaptureLoc(Context, DC, PAI, inoutCapture);
      continue;
    }
    // For an autoclosure capture, present a way to fix the problem.
    if (paramName) {
      emittedError = true;
      diagnose(Context, PAI->getLoc(), diag::copy_inout_captured_by_autoclosure,
               paramName.value());
    } else {
      emittedError = true;
      diagnose(Context, PAI->getLoc(), diag::copy_self_captured_by_autoclosure);
    }
  }

  // Finally, diagnose captures of values with noescape type.
  for (auto noEscapeCapture : noEscapeCaptures) {
    if (auto *param = getParamDeclFromOperand(noEscapeCapture->get())) {
      emittedError = true;
      diagnose(Context, PAI->getLoc(), diag::escaping_noescape_param_capture,
               functionKind, param->getName());
      diagnose(Context, param->getLoc(), diag::noescape_param_defined_here,
               param->getName());
    } else {
      emittedError = true;
      diagnose(Context, PAI->getLoc(), diag::escaping_noescape_var_capture,
               functionKind);
    }

    diagnoseCaptureLoc(Context, DC, PAI, noEscapeCapture);
  }

  // If we emitted an error, mark the closure function as not being suitable for
  // noncopyable diagnostics. The user can fix the issue and then recompile.
  if (emittedError) {
    if (auto *f = apply.getCalleeFunction()) {
      auto s = semantics::NO_MOVEONLY_DIAGNOSTICS;
      f->addSemanticsAttr(s);
    }
  }
}

// Enforce exclusivity restrictions on recursive uses of non-escaping closures.
// Exclusivity requires a Non-Escaping Recursion Restriction rule (SE-0176):
// A non-escaping closure A may not be recursively invoked during the
// execution of a non-escaping closure B which captures the same local
// variable or inout parameter unless:
// - A is defined within B or
// - A is a local function declaration which is referenced directly by B.
//
// This is conservatively approximated with a Non-Escaping Parameter Call
// Restriction rule (NPCR), as implemented below:
// A function may not call a non-escaping function parameter passing a
// non-escaping function parameter as an argument.
// For the purposes of this rule, a closure which captures a non-escaping
// function parameter is treated the same as the parameter.
//
// Note: The compiler does not enforce recursion via
// withoutActuallyEscaping. This undefined behavior is exposed to programmers.
//
// TODO: Verify that all uses of noescaping function arguments are SIL patterns
// that are recognized below to prove that this diagnostic is complete.
static void checkApply(ASTContext &Context, FullApplySite site) {
  auto isNoEscapeParam = [&](SILValue value) -> const ParamDecl * {
    // If the value is an escaping, do not enforce any restrictions.
    if (!value->getType().containsNoEscapeFunction())
      return nullptr;

    // If the value is not a function parameter, do not enforce any restrictions.
    return getParamDeclFromOperand(value);
  };

  // If the callee is not a no-escape parameter, there is nothing to check.
  auto callee = site.getCalleeOrigin();
  if (!isNoEscapeParam(callee))
    return;

  // See if any of our arguments are noescape parameters, or closures capturing
  // noescape parameters.
  SmallVector<std::pair<SILValue, bool>, 4> args;
  llvm::SmallDenseSet<SILValue, 4> visited;
  auto arglistInsert = [&](SILValue arg, bool capture) {
    if (visited.insert(arg).second)
      args.emplace_back(arg, capture);
  };

  for (auto arg : site.getArguments())
    arglistInsert(arg, /*capture=*/false);

  while (!args.empty()) {
    auto pair = args.pop_back_val();
    auto arg = pair.first;
    bool capture = pair.second;

    if (auto CI = ConversionOperation(arg)) {
      arglistInsert(CI.getConverted(), /*capture=*/false);
      continue;
    }

    if (auto *Copy = dyn_cast<CopyValueInst>(arg)) {
      arglistInsert(Copy->getOperand(), capture);
    }

    // If one of our call arguments is a noescape parameter, diagnose the
    // violation.
    if (auto *param = isNoEscapeParam(arg)) {
      diagnose(Context, site.getLoc(), diag::err_noescape_param_call,
               param->getName(), capture);
      return;
    }

    // If one of our call arguments is a closure, recursively visit all of
    // the closure's captures.
    if (auto *PAI = dyn_cast<PartialApplyInst>(arg)) {
      ApplySite site(PAI);
      for (auto arg : site.getArguments())
        arglistInsert(arg, /*capture=*/true);
      continue;
    }
  }
}

static void checkForViolationsAtInstruction(ASTContext &Context,
                                            DeclContext *DC,
                                            SILInstruction *I) {
  if (auto *PAI = dyn_cast<PartialApplyInst>(I))
    checkPartialApply(Context, DC, PAI);

  if (isa<ApplyInst>(I) || isa<TryApplyInst>(I)) {
    FullApplySite site(I);
    checkApply(Context, site);
  }
}

static void checkEscapingCaptures(SILFunction *F) {
  if (F->empty())
    return;

  auto &Context = F->getASTContext();
  auto *DC = F->getDeclContext();

  for (auto &BB : *F) {
    for (auto &I : BB)
      checkForViolationsAtInstruction(Context, DC, &I);
  }
}

namespace {

class DiagnoseInvalidEscapingCaptures : public SILFunctionTransform {
public:
  DiagnoseInvalidEscapingCaptures() {}

private:
  void run() override {
    SILFunction *F = getFunction();

    // Don't rerun diagnostics on deserialized functions.
    if (F->wasDeserializedCanonical())
      return;

    LLVM_DEBUG(llvm::dbgs() << "*** Diagnosing escaping captures in function: "
                            << F->getName() << '\n');
    checkEscapingCaptures(F);
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseInvalidEscapingCaptures() {
  return new DiagnoseInvalidEscapingCaptures();
}
