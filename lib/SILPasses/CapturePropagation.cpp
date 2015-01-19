//===---- CapturePropagation.cpp - Propagate closure capture constants ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "capture-prop"
#include "swift/SILPasses/Passes.h"
#include "swift/Basic/Demangle.h"
#include "swift/SIL/Mangle.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/ColdBlockInfo.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumCapturesPropagated, "Number of constant captures propagated");

namespace {
/// Propagate constants through closure captures by specializing the partially
/// applied function.
class CapturePropagation : public SILModuleTransform
{
public:
  void run() override;

  StringRef getName() override { return "Captured Constant Propagation"; }

protected:
  bool optimizePartialApply(PartialApplyInst *PAI);
  SILFunction *specializeConstClosure(PartialApplyInst *PAI,
                                      SILFunction *SubstF);
  void rewritePartialApply(PartialApplyInst *PAI, SILFunction *SpecialF);
};
} // namespace

static LiteralInst *getConstant(SILValue V) {
  if (auto I = dyn_cast<ThinToThickFunctionInst>(V))
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

static llvm::SmallString<64> getClonedName(PartialApplyInst *PAI,
                                           SILFunction *F) {
  llvm::SmallString<64> ClonedName;

  llvm::raw_svector_ostream buffer(ClonedName);
  Mangle::Mangler M(buffer);
  auto P = Mangle::SpecializationPass::CapturePropagation;
  Mangle::FunctionSignatureSpecializationMangler Mangler(P, M, F);

  // We know that all arguments are literal insts.
  auto Args = PAI->getArguments();
  for (unsigned i : indices(Args))
    Mangler.setArgumentConstantProp(i, getConstant(Args[i]));
  Mangler.mangle();

  return ClonedName;
}

namespace {
/// Clone the partially applied function, replacing incoming arguments with
/// literal constants.
///
/// The cloned literals will retain the SILLocation from the partial apply's
/// caller, so the cloned function will have a mix of locations from different
/// functions.
class CapturePropagationCloner
  : public SILClonerWithScopes<CapturePropagationCloner> {
  using SuperTy = SILClonerWithScopes<CapturePropagationCloner>;
  friend class SILVisitor<CapturePropagationCloner>;
  friend class SILCloner<CapturePropagationCloner>;

  SILFunction *OrigF;
  bool IsCloningConstant;
public:
  CapturePropagationCloner(SILFunction *OrigF, SILFunction *NewF)
    : SuperTy(*NewF), OrigF(OrigF), IsCloningConstant(false) {}

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
    if (IsCloningConstant) {
      Cloned->setDebugScope(getBuilder().getFunction().getDebugScope());
      SILCloner<CapturePropagationCloner>::postProcess(Orig, Cloned);
    } else
      SILClonerWithScopes<CapturePropagationCloner>::postProcess(Orig, Cloned);
  }

  void cloneConstValue(SILValue Const);
};
} // namespace

/// Clone a constant value. Recursively walk the operand chain through cast
/// instructions to ensure that all dependents are cloned. Note that the
/// original value may not belong to the same function as the one being cloned
/// by cloneBlocks() (they may be from the partial apply caller).
void CapturePropagationCloner::cloneConstValue(SILValue Val) {
  assert(IsCloningConstant && "incorrect mode");

  auto Inst = dyn_cast<SILInstruction>(Val);
  if (!Inst)
    return;

  auto II = InstructionMap.find(Inst);
  if (II != InstructionMap.end())
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
  SILModule &M = CloneF.getModule();

  // Create the entry basic block with the function arguments.
  SILBasicBlock *OrigEntryBB = OrigF->begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(&CloneF);
  CanSILFunctionType CloneFTy = CloneF.getLoweredFunctionType();

  // Only clone the arguments that remain in the new function type. The trailing
  // arguments are now propagated through the partial apply.
  assert(!IsCloningConstant && "incorrect mode");
  unsigned ParamIdx = 0;
  for (unsigned NewParamEnd = CloneFTy->getParameters().size();
       ParamIdx != NewParamEnd; ++ParamIdx) {

    SILArgument *Arg = OrigEntryBB->getBBArg(ParamIdx);

    SILValue MappedValue = new (M)
        SILArgument(ClonedEntryBB, remapType(Arg->getType()), Arg->getDecl());
    ValueMap.insert(std::make_pair(Arg, MappedValue));
  }
  assert(OrigEntryBB->bbarg_size() - ParamIdx == PartialApplyArgs.size()
         && "unexpected number of partial apply arguments");

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
    SILArgument *InArg = OrigEntryBB->getBBArg(ParamIdx);
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

/// Given a partial_apply instruction, create a specialized callee by removing
/// all constant arguments and adding constant literals to the specialized
/// function body.
SILFunction *CapturePropagation::specializeConstClosure(PartialApplyInst *PAI,
                                                        SILFunction *OrigF) {
  llvm::SmallString<64> Name = getClonedName(PAI, OrigF);

  // See if we already have a version of this function in the module. If so,
  // just return it.
  if (auto *NewF = OrigF->getModule().lookUpFunction(Name.str())) {
    DEBUG(llvm::dbgs() << "  Found an already specialized version of the callee: ";
          NewF->printName(llvm::dbgs()); llvm::dbgs() << "\n");
    return NewF;
  }

  // The new partial_apply will no longer take any arguments--they are all
  // expressed as literals. So its callee signature will be the same as its
  // return signature.
  CanSILFunctionType NewFTy =
    Lowering::adjustFunctionType(PAI->getType().castTo<SILFunctionType>(),
                                 FunctionType::Representation::Thin);
  SILFunction *NewF = SILFunction::create(
      *getModule(), SILLinkage::Shared, Name, NewFTy,
      /*contextGenericParams*/ nullptr, OrigF->getLocation(), OrigF->isBare(),
      OrigF->isTransparent(), OrigF->isFragile(), OrigF->getClassVisibility(),
      OrigF->getInlineStrategy(), OrigF->getEffectsKind(),
      /*InsertBefore*/ OrigF, OrigF->getDebugScope(), OrigF->getDeclContext());
  DEBUG(llvm::dbgs() << "  Specialize callee as ";
        NewF->printName(llvm::dbgs()); llvm::dbgs() << " " << NewFTy << "\n");

  CapturePropagationCloner cloner(OrigF, NewF);
  cloner.cloneBlocks(PAI->getArguments());
  return NewF;
}

void CapturePropagation::rewritePartialApply(PartialApplyInst *OrigPAI,
                                             SILFunction *SpecialF) {
  SILBuilderWithScope<2> Builder(OrigPAI);
  auto FuncRef = Builder.createFunctionRef(OrigPAI->getLoc(), SpecialF);
  auto NewPAI = Builder.createPartialApply(OrigPAI->getLoc(),
                                           FuncRef,
                                           SpecialF->getLoweredType(),
                                           ArrayRef<Substitution>(),
                                           ArrayRef<SILValue>(),
                                           OrigPAI->getType());
  OrigPAI->replaceAllUsesWith(NewPAI);
  recursivelyDeleteTriviallyDeadInstructions(OrigPAI, true);
  DEBUG(llvm::dbgs() << "  Rewrote caller:\n" << *NewPAI);
}

/// For now, we conservative only specialize if doing so can eliminate dynamic
/// dispatch.
///
/// TODO: Check for other profitable constant propagation, like builtin compare.
static bool isProfitable(SILFunction *Callee) {
  SILBasicBlock *EntryBB = Callee->begin();
  for (auto *Arg : EntryBB->getBBArgs()) {
    for (auto *Operand : Arg->getUses()) {
      if (auto *AI = dyn_cast<ApplyInst>(Operand->getUser())) {
        if (AI->getCallee() == Operand->get())
          return true;
      }
    }
  }
  return false;
}

bool CapturePropagation::optimizePartialApply(PartialApplyInst *PAI) {
  // Check if the partial_apply has generic substitutions.
  // FIXME: We could handle generic thunks if it's worthwhile.
  if (PAI->hasSubstitutions())
    return false;

  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
  if (!FRI)
    return false;

  assert(!FRI->getFunctionType()->isPolymorphic() &&
         "cannot specialize generic partial apply");

  for (auto Arg : PAI->getArguments()) {
    if (!isConstant(Arg))
      return false;
  }
  SILFunction *SubstF = FRI->getReferencedFunction();
  if (SubstF->isExternalDeclaration() || !isProfitable(SubstF))
    return false;

  DEBUG(llvm::dbgs() << "Specializing closure for constant arguments:\n"
        << "  " << SubstF->getName() << "\n" << *PAI);
  ++NumCapturesPropagated;
  SILFunction *NewF = specializeConstClosure(PAI, SubstF);
  rewritePartialApply(PAI, NewF);
  return true;
}

void CapturePropagation::run() {
  DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
  bool HasChanged = false;
  for (auto &F : *getModule()) {
    // Cache cold blocks per function.
    ColdBlockInfo ColdBlocks(DA);
    for (auto &BB : F) {
      if (ColdBlocks.isCold(&BB))
        continue;

      auto I = BB.begin();
      while (I != BB.end()) {
        SILInstruction *Inst = &*I;
        ++I;
        if (PartialApplyInst *PAI = dyn_cast<PartialApplyInst>(Inst))
          HasChanged |= optimizePartialApply(PAI);
      }
    }
  }
  // FIXME: This conservatively invalidates everything. But the transform
  // actually changes neither the CFG nor the static call graph. I only made
  // this conservative in case someone implements interprocedural/dynamic call
  // graph analysis later.
  if (HasChanged)
    invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
}

SILTransform *swift::createCapturePropagation() {
  return new CapturePropagation();
}
