//===--- CapturePropagation.cpp - Propagate closure capture constants -----===//
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

#define DEBUG_TYPE "capture-prop"
#include "swift/AST/GenericEnvironment.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/Demangling/Demangle.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumCapturesPropagated, "Number of constant captures propagated");

namespace {
/// Propagate constants through closure captures by specializing the partially
/// applied function.
/// Also optimize away partial_apply instructions where all partially applied
/// arguments are dead.
class CapturePropagation : public SILFunctionTransform
{
public:
  void run() override;

protected:
  bool optimizePartialApply(PartialApplyInst *PAI);
  SILFunction *specializeConstClosure(PartialApplyInst *PAI,
                                      SILFunction *SubstF);
  void rewritePartialApply(PartialApplyInst *PAI, SILFunction *SpecialF);
};
} // end anonymous namespace

static LiteralInst *getConstant(SILValue V) {
  if (auto I = dyn_cast<ThinToThickFunctionInst>(V))
    return getConstant(I->getOperand());
  if (auto I = dyn_cast<ConvertFunctionInst>(V))
    return getConstant(I->getOperand());
  return dyn_cast<LiteralInst>(V);
}

static bool isOptimizableConstant(SILValue V) {
  // We do not optimize string literals of length > 32 since we would need to
  // encode them into the symbol name for uniqueness.
  if (auto *SLI = dyn_cast<StringLiteralInst>(V))
    return SLI->getValue().size() <= 32;
  return true;
}

static bool isConstant(SILValue V) {
  V = getConstant(V);
  return V && isOptimizableConstant(V);
}

static std::string getClonedName(PartialApplyInst *PAI, IsSerialized_t Serialized,
                                 SILFunction *F) {
  auto P = Demangle::SpecializationPass::CapturePropagation;
  Mangle::FunctionSignatureSpecializationMangler Mangler(P, Serialized, F);

  // We know that all arguments are literal insts.
  unsigned argIdx = ApplySite(PAI).getCalleeArgIndexOfFirstAppliedArg();
  for (auto arg : PAI->getArguments()) {
    Mangler.setArgumentConstantProp(argIdx, getConstant(arg));
    ++argIdx;
  }
  return Mangler.mangle();
}

namespace {
/// Clone the partially applied function, replacing incoming arguments with
/// literal constants.
///
/// The cloned literals will retain the SILLocation from the partial apply's
/// caller, so the cloned function will have a mix of locations from different
/// functions.
class CapturePropagationCloner
  : public TypeSubstCloner<CapturePropagationCloner> {
  using SuperTy = TypeSubstCloner<CapturePropagationCloner>;
  friend class SILInstructionVisitor<CapturePropagationCloner>;
  friend class SILCloner<CapturePropagationCloner>;

  SILFunction *OrigF;
  bool IsCloningConstant;
public:
  CapturePropagationCloner(SILFunction *OrigF, SILFunction *NewF,
                           SubstitutionMap Subs)
      : SuperTy(*NewF, *OrigF, Subs), OrigF(OrigF), IsCloningConstant(false) {}

  void cloneBlocks(OperandValueArrayRef Args);

protected:
  /// Literals cloned from the caller drop their location so the debug line
  /// tables don't senselessly jump around. As a placeholder give them the
  /// location of the newly cloned function.
  SILLocation remapLocation(SILLocation InLoc) {
    if (IsCloningConstant)
      return getBuilder().getFunction().getLocation();
    return InLoc;
  }

  /// Literals cloned from the caller take on the new function's debug scope.
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    assert(IsCloningConstant == (Orig->getFunction() != OrigF) &&
           "Expect only cloned constants from the caller function.");
    SILClonerWithScopes<CapturePropagationCloner>::postProcess(Orig, Cloned);
  }

  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    if (IsCloningConstant)
      return getBuilder().getFunction().getDebugScope();
    else
      return SILClonerWithScopes<CapturePropagationCloner>::remapScope(DS);
  }

  void cloneConstValue(SILValue Const);
};
} // end anonymous namespace

/// Clone a constant value. Recursively walk the operand chain through cast
/// instructions to ensure that all dependents are cloned. Note that the
/// original value may not belong to the same function as the one being cloned
/// by cloneBlocks() (they may be from the partial apply caller).
void CapturePropagationCloner::cloneConstValue(SILValue Val) {
  assert(IsCloningConstant && "incorrect mode");

  if (ValueMap.find(Val) != ValueMap.end())
    return;

  // TODO: MultiValueInstruction?
  auto Inst = dyn_cast<SingleValueInstruction>(Val);
  if (!Inst)
    return;

  if (Inst->getNumOperands() > 0) {
    // Only handle single operands for simple recursion without a worklist.
    assert(Inst->getNumOperands() == 1 && "expected single-operand cast");
    cloneConstValue(Inst->getOperand(0));
  }
  visit(Inst);
}

/// Clone the original partially applied function into the new specialized
/// function, replacing some arguments with literals.
void CapturePropagationCloner::cloneBlocks(
  OperandValueArrayRef PartialApplyArgs) {

  SILFunction &CloneF = getBuilder().getFunction();

  // Create the entry basic block with the function arguments.
  SILBasicBlock *OrigEntryBB = &*OrigF->begin();
  SILBasicBlock *ClonedEntryBB = CloneF.createBasicBlock();
  auto cloneConv = CloneF.getConventions();

  // Only clone the arguments that remain in the new function type. The trailing
  // arguments are now propagated through the partial apply.
  assert(!IsCloningConstant && "incorrect mode");
  unsigned ParamIdx = 0;
  for (unsigned NewParamEnd = cloneConv.getNumSILArguments();
       ParamIdx != NewParamEnd; ++ParamIdx) {

    SILArgument *Arg = OrigEntryBB->getArgument(ParamIdx);

    SILValue MappedValue = ClonedEntryBB->createFunctionArgument(
        remapType(Arg->getType()), Arg->getDecl());
    ValueMap.insert(std::make_pair(Arg, MappedValue));
  }
  assert(OrigEntryBB->args_size() - ParamIdx == PartialApplyArgs.size() &&
         "unexpected number of partial apply arguments");

  // Replace the rest of the old arguments with constants.
  BBMap.insert(std::make_pair(OrigEntryBB, ClonedEntryBB));
  getBuilder().setInsertionPoint(ClonedEntryBB);
  IsCloningConstant = true;
  for (SILValue PartialApplyArg : PartialApplyArgs) {
    assert(isConstant(PartialApplyArg) &&
           "expected a constant arg to partial apply");

    cloneConstValue(PartialApplyArg);

    // The PartialApplyArg from the caller is now mapped to its cloned
    // instruction.  Also map the original argument to the cloned instruction.
    SILArgument *InArg = OrigEntryBB->getArgument(ParamIdx);
    ValueMap.insert(std::make_pair(InArg, remapValue(PartialApplyArg)));
    ++ParamIdx;
  }
  IsCloningConstant = false;
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(OrigEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    visit(BI->first->getTerminator());
  }
}

CanSILFunctionType getPartialApplyInterfaceResultType(PartialApplyInst *PAI) {
  // The new partial_apply will no longer take any arguments--they are all
  // expressed as literals. So its callee signature will be the same as its
  // return signature.
  auto FTy = PAI->getType().castTo<SILFunctionType>();
  assert(!PAI->hasSubstitutions() ||
         !PAI->getSubstitutionMap().hasArchetypes());
  FTy = cast<SILFunctionType>(
    FTy->mapTypeOutOfContext()->getCanonicalType());
  auto NewFTy = FTy;
  return NewFTy;
}

/// Given a partial_apply instruction, create a specialized callee by removing
/// all constant arguments and adding constant literals to the specialized
/// function body.
SILFunction *CapturePropagation::specializeConstClosure(PartialApplyInst *PAI,
                                                        SILFunction *OrigF) {
  IsSerialized_t Serialized = IsNotSerialized;
  if (PAI->getFunction()->isSerialized() && OrigF->isSerialized())
    Serialized = IsSerializable;

  std::string Name = getClonedName(PAI, Serialized, OrigF);

  // See if we already have a version of this function in the module. If so,
  // just return it.
  if (auto *NewF = OrigF->getModule().lookUpFunction(Name)) {
    assert(NewF->isSerialized() == Serialized);
    LLVM_DEBUG(llvm::dbgs()
                 << "  Found an already specialized version of the callee: ";
               NewF->printName(llvm::dbgs()); llvm::dbgs() << "\n");
    return NewF;
  }

  // The new partial_apply will no longer take any arguments--they are all
  // expressed as literals. So its callee signature will be the same as its
  // return signature.
  auto NewFTy = getPartialApplyInterfaceResultType(PAI);
  NewFTy = NewFTy->getWithRepresentation(SILFunctionType::Representation::Thin);

  GenericEnvironment *GenericEnv = nullptr;
  if (NewFTy->getGenericSignature())
    GenericEnv = OrigF->getGenericEnvironment();
  SILFunction *NewF = OrigF->getModule().createFunction(
      SILLinkage::Shared, Name, NewFTy, GenericEnv, OrigF->getLocation(),
      OrigF->isBare(), OrigF->isTransparent(), Serialized,
      OrigF->getEntryCount(), OrigF->isThunk(), OrigF->getClassSubclassScope(),
      OrigF->getInlineStrategy(), OrigF->getEffectsKind(),
      /*InsertBefore*/ OrigF, OrigF->getDebugScope());
  if (!OrigF->hasQualifiedOwnership()) {
    NewF->setUnqualifiedOwnership();
  }
  LLVM_DEBUG(llvm::dbgs() << "  Specialize callee as ";
             NewF->printName(llvm::dbgs());
             llvm::dbgs() << " " << NewFTy << "\n");

  LLVM_DEBUG(if (PAI->hasSubstitutions()) {
    llvm::dbgs() << "CapturePropagation of generic partial_apply:\n";
    PAI->dumpInContext();
  });
  CapturePropagationCloner cloner(OrigF, NewF, PAI->getSubstitutionMap());
  cloner.cloneBlocks(PAI->getArguments());
  assert(OrigF->getDebugScope()->Parent != NewF->getDebugScope()->Parent);
  return NewF;
}

void CapturePropagation::rewritePartialApply(PartialApplyInst *OrigPAI,
                                             SILFunction *SpecialF) {
  LLVM_DEBUG(llvm::dbgs() << "\n  Rewriting a partial apply:\n";
             OrigPAI->dumpInContext();
             llvm::dbgs() << "   with special function: "
                          << SpecialF->getName() << "\n";
             llvm::dbgs() << "\nThe function being rewritten is:\n";
             OrigPAI->getFunction()->dump());

  SILBuilderWithScope Builder(OrigPAI);
  auto FuncRef = Builder.createFunctionRef(OrigPAI->getLoc(), SpecialF);
  auto *T2TF = Builder.createThinToThickFunction(OrigPAI->getLoc(), FuncRef,
                                                 OrigPAI->getType());
  OrigPAI->replaceAllUsesWith(T2TF);
  recursivelyDeleteTriviallyDeadInstructions(OrigPAI, true);
  LLVM_DEBUG(llvm::dbgs() << "  Rewrote caller:\n" << *T2TF);
}

/// For now, we conservative only specialize if doing so can eliminate dynamic
/// dispatch.
///
/// TODO: Check for other profitable constant propagation, like builtin compare.
static bool isProfitable(SILFunction *Callee) {
  SILBasicBlock *EntryBB = &*Callee->begin();
  for (auto *Arg : EntryBB->getArguments()) {
    for (auto *Operand : Arg->getUses()) {
      if (FullApplySite FAS = FullApplySite::isa(Operand->getUser())) {
        if (FAS.getCallee() == Operand->get())
          return true;
      }
    }
  }
  return false;
}

/// Returns true if block \p BB only contains a return or throw of the first
/// block argument and side-effect-free instructions.
static bool onlyContainsReturnOrThrowOfArg(SILBasicBlock *BB) {
  for (SILInstruction &I : *BB) {
    if (isa<ReturnInst>(&I) || isa<ThrowInst>(&I)) {
      SILValue RetVal = I.getOperand(0);
      return BB->getNumArguments() == 1 && RetVal == BB->getArgument(0);
    }
    if (I.mayHaveSideEffects() || isa<TermInst>(&I))
      return false;
  }
  llvm_unreachable("should have seen a terminator instruction");
}

/// Checks if \p Orig is a thunk which calls another function but without
/// passing the trailing \p numDeadParams dead parameters.
/// If a generic specialization was performed for a generic capture,
/// GenericSpecialized contains a tuple:
/// (new specialized function, old function)
static SILFunction *getSpecializedWithDeadParams(
    PartialApplyInst *PAI, SILFunction *Orig, int numDeadParams,
    std::pair<SILFunction *, SILFunction *> &GenericSpecialized) {
  SILBasicBlock &EntryBB = *Orig->begin();
  unsigned NumArgs = EntryBB.getNumArguments();
  SILModule &M = Orig->getModule();

  // Check if all dead parameters have trivial types. We don't support non-
  // trivial types because it's very hard to find places where we can release
  // those parameters (as a replacement for the removed partial_apply).
  // TODO: maybe we can skip this restriction when we have semantic ARC.
  for (unsigned Idx = NumArgs - numDeadParams; Idx < NumArgs; ++Idx) {
    SILType ArgTy = EntryBB.getArgument(Idx)->getType();
    if (!ArgTy.isTrivial(M))
      return nullptr;
  }
  SILFunction *Specialized = nullptr;
  SILValue RetValue;

  // Check all instruction of the entry block.
  for (SILInstruction &I : EntryBB) {
    if (auto FAS = FullApplySite::isa(&I)) {
      // Check if this is the call of the specialized function.
      // If the original partial_apply didn't have substitutions,
      // also the specialized function must be not generic.
      if (!PAI->hasSubstitutions() && FAS.hasSubstitutions())
        return nullptr;

      // Is it the only call?
      if (Specialized)
        return nullptr;

      Specialized = FAS.getReferencedFunction();
      if (!Specialized)
        return nullptr;

      // Check if parameters are passes 1-to-1
      unsigned NumArgs = FAS.getNumArguments();
      if (EntryBB.getNumArguments() - numDeadParams != NumArgs)
        return nullptr;

      for (unsigned Idx = 0; Idx < NumArgs; ++Idx) {
        if (FAS.getArgument(Idx) != (ValueBase *)EntryBB.getArgument(Idx))
          return nullptr;
      }

      if (auto *TAI = dyn_cast<TryApplyInst>(&I)) {
        // Check the normal and throw blocks of the try_apply.
        if (onlyContainsReturnOrThrowOfArg(TAI->getNormalBB()) &&
            onlyContainsReturnOrThrowOfArg(TAI->getErrorBB()))
          return Specialized;
        return nullptr;
      }
      assert(isa<ApplyInst>(&I) && "unknown FullApplySite instruction");
      RetValue = cast<ApplyInst>(&I);
      continue;
    }
    if (auto *RI = dyn_cast<ReturnInst>(&I)) {
      // Check if we return the result of the apply.
      if (RI->getOperand() != RetValue)
        return nullptr;
      continue;
    }
    if (I.mayHaveSideEffects() || isa<TermInst>(&I))
      return nullptr;
  }

  auto Rep = Specialized->getLoweredFunctionType()->getRepresentation();
  if (getSILFunctionLanguage(Rep) != SILFunctionLanguage::Swift)
    return nullptr;

  GenericSpecialized = std::make_pair(nullptr, nullptr);

  if (PAI->hasSubstitutions()) {
    if (Specialized->isExternalDeclaration())
      return nullptr;
    if (!Orig->shouldOptimize())
      return nullptr;

    // Perform a generic specialization of the Specialized function.
    ReabstractionInfo ReInfo(ApplySite(), Specialized,
                             PAI->getSubstitutionMap(),
                             /* ConvertIndirectToDirect */ false);
    GenericFuncSpecializer FuncSpecializer(
                                         Specialized,
                                         ReInfo.getClonerParamSubstitutionMap(),
                                         Specialized->isSerialized(), ReInfo);

    SILFunction *GenericSpecializedFunc = FuncSpecializer.trySpecialization();
    if (!GenericSpecializedFunc)
      return nullptr;
    GenericSpecialized = std::make_pair(GenericSpecializedFunc, Specialized);
    return GenericSpecializedFunc;
  }
  return Specialized;
}

bool CapturePropagation::optimizePartialApply(PartialApplyInst *PAI) {
  SILFunction *SubstF = PAI->getReferencedFunction();
  if (!SubstF)
    return false;
  if (SubstF->isExternalDeclaration())
    return false;

  if (PAI->hasSubstitutions() && PAI->getSubstitutionMap().hasArchetypes()) {
    LLVM_DEBUG(llvm::dbgs()
                 << "CapturePropagation: cannot handle partial specialization "
                    "of partial_apply:\n";
               PAI->dumpInContext());
    return false;
  }


  // First possibility: Is it a partial_apply where all partially applied
  // arguments are dead?
  std::pair<SILFunction *, SILFunction *> GenericSpecialized;
  if (auto *NewFunc = getSpecializedWithDeadParams(
          PAI, SubstF, PAI->getNumArguments(), GenericSpecialized)) {
    rewritePartialApply(PAI, NewFunc);
    if (GenericSpecialized.first) {
      // Notify the pass manager about the new function.
      notifyAddFunction(GenericSpecialized.first, GenericSpecialized.second);
    }
    return true;
  }

  // Second possibility: Are all partially applied arguments constant?
  for (auto Arg : PAI->getArguments()) {
    if (!isConstant(Arg))
      return false;
  }
  if (!isProfitable(SubstF))
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Specializing closure for constant arguments:\n"
                          << "  " << SubstF->getName() << "\n"
                          << *PAI);
  ++NumCapturesPropagated;
  SILFunction *NewF = specializeConstClosure(PAI, SubstF);
  rewritePartialApply(PAI, NewF);

  notifyAddFunction(NewF, SubstF);
  return true;
}

void CapturePropagation::run() {
  DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
  auto *F = getFunction();
  bool HasChanged = false;

  // Don't optimize functions that are marked with the opt.never attribute.
  if (!F->shouldOptimize())
    return;

  // Cache cold blocks per function.
  ColdBlockInfo ColdBlocks(DA);
  for (auto &BB : *F) {
    if (ColdBlocks.isCold(&BB))
      continue;

    auto I = BB.begin();
    while (I != BB.end()) {
      SILInstruction *Inst = &*I;
      ++I;
      if (auto *PAI = dyn_cast<PartialApplyInst>(Inst))
        HasChanged |= optimizePartialApply(PAI);
    }
  }
  if (HasChanged) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);
  }
}

SILTransform *swift::createCapturePropagation() {
  return new CapturePropagation();
}
