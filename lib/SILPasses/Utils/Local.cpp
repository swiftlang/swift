//===--- Local.cpp - Functions that perform local SIL transformations. ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/CommandLine.h"
#include "swift/Strings.h"
#include <deque>

using namespace swift;

llvm::cl::opt<bool>
DebugValuesPropagateLiveness("debug-values-propagate-liveness",
                             llvm::cl::init(false));

bool swift::debugValuesPropagateLiveness() {
  return DebugValuesPropagateLiveness;
}

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This routine only examines the state of the instruction at hand.
bool
swift::isInstructionTriviallyDead(SILInstruction *I) {
  if (!I->use_empty() || isa<TermInst>(I))
    return false;

  if (auto *BI = dyn_cast<BuiltinInst>(I)) {
    return !BI->mayHaveSideEffects();
  }

  // condfail instructions that obviously can't fail are dead.
  if (auto *CFI = dyn_cast<CondFailInst>(I))
    if (auto *ILI = dyn_cast<IntegerLiteralInst>(CFI->getOperand()))
      if (!ILI->getValue())
        return true;

  // mark_uninitialized is never dead.
  if (isa<MarkUninitializedInst>(I))
    return false;

  if (debugValuesPropagateLiveness() &&
      (isa<DebugValueInst>(I) || isa<DebugValueAddrInst>(I)))
    return false;

  // These invalidate enums so "write" memory, but that is not an essential
  // operation so we can remove these if they are trivially dead.
  if (isa<UncheckedTakeEnumDataAddrInst>(I))
    return true;
  
  if (!I->mayHaveSideEffects())
    return true;

  return false;
}

namespace {
  using CallbackTy = std::function<void(SILInstruction *)>;
} // end anonymous namespace

bool swift::
recursivelyDeleteTriviallyDeadInstructions(ArrayRef<SILInstruction *> IA,
                                           bool Force, CallbackTy Callback) {
  // Delete these instruction and others that become dead after it's deleted.
  llvm::SmallPtrSet<SILInstruction *, 8> DeadInsts;
  for (auto I : IA) {
    // If the instruction is not dead and force is false, do nothing.
    if (Force || isInstructionTriviallyDead(I))
      DeadInsts.insert(I);
  }
  llvm::SmallPtrSet<SILInstruction *, 8> NextInsts;
  while (!DeadInsts.empty()) {
    for (auto I : DeadInsts) {
      // Call the callback before we mutate the to be deleted instruction in any
      // way.
      Callback(I);

      // Check if any of the operands will become dead as well.
      MutableArrayRef<Operand> Ops = I->getAllOperands();
      for (Operand &Op : Ops) {
        SILValue OpVal = Op.get();
        if (!OpVal)
          continue;

        // Remove the reference from the instruction being deleted to this
        // operand.
        Op.drop();

        // If the operand is an instruction that is only used by the instruction
        // being deleted, delete it.
        if (SILInstruction *OpValInst = dyn_cast<SILInstruction>(OpVal))
          if (!DeadInsts.count(OpValInst) &&
              isInstructionTriviallyDead(OpValInst))
            NextInsts.insert(OpValInst);
      }

      // If we have a function ref inst, we need to especially drop its function
      // argument so that it gets a proper ref decement.
      auto *FRI = dyn_cast<FunctionRefInst>(I);
      if (FRI && FRI->getReferencedFunction())
        FRI->dropReferencedFunction();
    }

    for (auto I : DeadInsts) {
      // This will remove this instruction and all its uses.
      I->eraseFromParent();
    }

    NextInsts.swap(DeadInsts);
    NextInsts.clear();
  }

  return true;
}

/// \brief If the given instruction is dead, delete it along with its dead
/// operands.
///
/// \param I The instruction to be deleted.
/// \param Force If Force is set, don't check if the top level instruction is
///        considered dead - delete it regardless.
/// \return Returns true if any instructions were deleted.
bool swift::recursivelyDeleteTriviallyDeadInstructions(SILInstruction *I,
                                                       bool Force,
                                                       CallbackTy Callback) {

  ArrayRef<SILInstruction *> AI = ArrayRef<SILInstruction *>(I);
  return recursivelyDeleteTriviallyDeadInstructions(AI, Force, Callback);
}

void swift::eraseUsesOfInstruction(SILInstruction *Inst) {
  for (auto UI : Inst->getUses()) {
    auto *User = UI->getUser();

    // If the instruction itself has any uses, recursively zap them so that
    // nothing uses this instruction.
    eraseUsesOfInstruction(User);

    // Walk through the operand list and delete any random instructions that
    // will become trivially dead when this instruction is removed.

    for (auto &Op : User->getAllOperands()) {
      if (auto *OpI = dyn_cast<SILInstruction>(Op.get())) {
        // Don't recursively delete the pointer we're getting in.
        if (OpI != Inst) {
          Op.drop();
          recursivelyDeleteTriviallyDeadInstructions(OpI);
        }
      }
    }

    User->eraseFromParent();
  }
}

// Devirtualization of functions with covariant return types produces
// a result that is not an apply, but takes an apply as an
// argument. Attempt to dig the apply out from this result.
ApplyInst *swift::findApplyFromDevirtualizedResult(SILInstruction *I) {
  if (auto *Apply = dyn_cast<ApplyInst>(I))
    return Apply;

  if (!I->getNumOperands())
    return nullptr;

  return dyn_cast<ApplyInst>(I->getOperand(0));
}

// Replace a dead apply with a new instruction that computes the same
// value, and delete the old apply.
void swift::replaceDeadApply(FullApplySite Old, SILInstruction *New) {
  auto *OldApply = Old.getInstruction();
  OldApply->replaceAllUsesWith(New);

  auto *CalleeInst = dyn_cast<SILInstruction>(Old.getCallee());

  OldApply->eraseFromParent();
  if (CalleeInst)
    recursivelyDeleteTriviallyDeadInstructions(CalleeInst);
}

bool swift::hasUnboundGenericTypes(TypeSubstitutionMap &SubsMap) {
  // Check whether any of the substitutions are dependent.
  for (auto &entry : SubsMap)
    if (entry.second->getCanonicalType()->hasArchetype())
      return true;

  return false;
}

bool swift::hasUnboundGenericTypes(ArrayRef<Substitution> Subs) {
  // Check whether any of the substitutions are dependent.
  for (auto &sub : Subs)
    if (sub.getReplacement()->getCanonicalType()->hasArchetype())
      return true;
  return false;
}

/// Find a new position for an ApplyInst's FuncRef so that it dominates its
/// use. Not that FuncionRefInsts may be shared by multiple ApplyInsts.
void swift::placeFuncRef(ApplyInst *AI, DominanceInfo *DT) {
  FunctionRefInst *FuncRef = cast<FunctionRefInst>(AI->getCallee());
  SILBasicBlock *DomBB =
    DT->findNearestCommonDominator(AI->getParent(), FuncRef->getParent());
  if (DomBB == AI->getParent() && DomBB != FuncRef->getParent())
    // Prefer to place the FuncRef immediately before the call. Since we're
    // moving FuncRef up, this must be the only call to it in the block.
    FuncRef->moveBefore(AI);
  else
    // Otherwise, conservatively stick it at the beginning of the block.
    FuncRef->moveBefore(DomBB->begin());
}

/// \brief Add an argument, \p val, to the branch-edge that is pointing into
/// block \p Dest. Return a new instruction and do not erase the old
/// instruction.
TermInst *swift::addArgumentToBranch(SILValue Val, SILBasicBlock *Dest,
                                     TermInst *Branch) {
  SILBuilderWithScope<2> Builder(Branch);

  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(Branch)) {
    SmallVector<SILValue, 8> TrueArgs;
    SmallVector<SILValue, 8> FalseArgs;

    for (auto A : CBI->getTrueArgs())
      TrueArgs.push_back(A);

    for (auto A : CBI->getFalseArgs())
      FalseArgs.push_back(A);

    if (Dest == CBI->getTrueBB()) {
      TrueArgs.push_back(Val);
      assert(TrueArgs.size() == Dest->getNumBBArg());
    } else {
      FalseArgs.push_back(Val);
      assert(FalseArgs.size() == Dest->getNumBBArg());
    }

    return Builder.createCondBranch(CBI->getLoc(), CBI->getCondition(),
                                    CBI->getTrueBB(), TrueArgs,
                                    CBI->getFalseBB(), FalseArgs);
  }

  if (BranchInst *BI = dyn_cast<BranchInst>(Branch)) {
    SmallVector<SILValue, 8> Args;

    for (auto A : BI->getArgs())
      Args.push_back(A);

    Args.push_back(Val);
    assert(Args.size() == Dest->getNumBBArg());
    return Builder.createBranch(BI->getLoc(), BI->getDestBB(), Args);
  }

  llvm_unreachable("unsupported terminator");
}

SILLinkage swift::getSpecializedLinkage(SILLinkage L) {
  switch (L) {
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::Shared:
  case SILLinkage::SharedExternal:
  case SILLinkage::Hidden:
  case SILLinkage::HiddenExternal:
    // Specializations of public or hidden symbols can be shared by all TUs
    // that specialize the definition.
    return SILLinkage::Shared;

  case SILLinkage::Private:
  case SILLinkage::PrivateExternal:
    // Specializations of private symbols should remain so.
    // TODO: maybe PrivateExternals should get SharedExternal (these are private
    // functions from the stdlib which are specialized in another module).
    return SILLinkage::Private;
  }
}

/// Remove all instructions in the body of \p BB in safe manner by using
/// undef.
void swift::clearBlockBody(SILBasicBlock *BB) {
  // Instructions in the dead block may be used by other dead blocks.  Replace
  // any uses of them with undef values.
  while (!BB->empty()) {
    // Grab the last instruction in the BB.
    auto *Inst = &BB->getInstList().back();

    // Replace any still-remaining uses with undef values and erase.
    Inst->replaceAllUsesWithUndef();
    Inst->eraseFromParent();
  }
}

// Handle the mechanical aspects of removing an unreachable block.
void swift::removeDeadBlock(SILBasicBlock *BB) {
  // Clear the body of BB.
  clearBlockBody(BB);

  // Now that the BB is empty, eliminate it.
  BB->eraseFromParent();
}

//===----------------------------------------------------------------------===//
//                       String Concatenation Optimizer
//===----------------------------------------------------------------------===//

namespace {
/// This is a helper class that performs optimization of string literals
/// concatenation.
class StringConcatenationOptimizer {
  /// Apply instruction being optimized.
  ApplyInst *AI;
  /// Builder to be used for creation of new instructions.
  SILBuilder &Builder;
  /// Left string literal operand of a string concatenation.
  StringLiteralInst *SLILeft = nullptr;
  /// Right string literal operand of a string concatenation.
  StringLiteralInst *SLIRight = nullptr;
  /// Function used to construct the left string literal.
  FunctionRefInst *FRILeft = nullptr;
  /// Function used to construct the right string literal.
  FunctionRefInst *FRIRight = nullptr;
  /// Apply instructions used to construct left string literal.
  ApplyInst *AILeft = nullptr;
  /// Apply instructions used to construct right string literal.
  ApplyInst *AIRight = nullptr;
  /// String literal conversion function to be used.
  FunctionRefInst *FRIConvertFromBuiltin = nullptr;
  /// Result type of a function producing the concatenated string literal.
  SILValue FuncResultType;

  /// Internal helper methods
  bool extractStringConcatOperands();
  void adjustEncodings();
  APInt getConcatenatedLength();
  bool isAscii() const;

public:
  StringConcatenationOptimizer(ApplyInst *AI, SILBuilder &Builder)
      : AI(AI), Builder(Builder) {}

  /// Tries to optimize a given apply instruction if it is a
  /// concatenation of string literals.
  ///
  /// Returns a new instruction if optimization was possible.
  SILInstruction *optimize();
};

} // end anonymous namespace

/// Checks operands of a string concatenation operation to see if
/// optimization is applicable.
///
/// Returns false if optimization is not possible.
/// Returns true and initializes internal fields if optimization is possible.
bool StringConcatenationOptimizer::extractStringConcatOperands() {
  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return false;

  auto *FRIFun = FRI->getReferencedFunction();

  if (AI->getNumOperands() != 3 ||
      !FRIFun->hasSemanticsString("string.concat"))
    return false;

  // Left and right operands of a string concatenation operation.
  AILeft = dyn_cast<ApplyInst>(AI->getOperand(1));
  AIRight = dyn_cast<ApplyInst>(AI->getOperand(2));

  if (!AILeft || !AIRight)
    return false;

  FRILeft = dyn_cast<FunctionRefInst>(AILeft->getCallee());
  FRIRight = dyn_cast<FunctionRefInst>(AIRight->getCallee());

  if (!FRILeft || !FRIRight)
    return false;

  auto *FRILeftFun = FRILeft->getReferencedFunction();
  auto *FRIRightFun = FRIRight->getReferencedFunction();

  if (FRILeftFun->getEffectsKind() >= EffectsKind::ReadWrite ||
      FRIRightFun->getEffectsKind() >= EffectsKind::ReadWrite)
    return false;

  if (!FRILeftFun->hasDefinedSemantics() ||
      !FRIRightFun->hasDefinedSemantics())
    return false;

  auto SemanticsLeft = FRILeftFun->getSemanticsString();
  auto SemanticsRight = FRIRightFun->getSemanticsString();
  auto AILeftOperandsNum = AILeft->getNumOperands();
  auto AIRightOperandsNum = AIRight->getNumOperands();

  // makeUTF16 should have following parameters:
  // (start: RawPointer, numberOfCodeUnits: Word)
  // makeUTF8 should have following parameters:
  // (start: RawPointer, byteSize: Word, isASCII: Int1)
  if (!((SemanticsLeft == "string.makeUTF16" && AILeftOperandsNum == 4) ||
        (SemanticsLeft == "string.makeUTF8" && AILeftOperandsNum == 5) ||
        (SemanticsRight == "string.makeUTF16" && AIRightOperandsNum == 4) ||
        (SemanticsRight == "string.makeUTF8" && AIRightOperandsNum == 5)))
    return false;

  SLILeft = dyn_cast<StringLiteralInst>(AILeft->getOperand(1));
  SLIRight = dyn_cast<StringLiteralInst>(AIRight->getOperand(1));

  if (!SLILeft || !SLIRight)
    return false;

  // Only UTF-8 and UTF-16 encoded string literals are supported by this
  // optimization.
  if (SLILeft->getEncoding() != StringLiteralInst::Encoding::UTF8 &&
      SLILeft->getEncoding() != StringLiteralInst::Encoding::UTF16)
    return false;

  if (SLIRight->getEncoding() != StringLiteralInst::Encoding::UTF8 &&
      SLIRight->getEncoding() != StringLiteralInst::Encoding::UTF16)
    return false;

  return true;
}

/// Ensures that both string literals to be concatenated use the same
/// UTF encoding. Converts UTF-8 into UTF-16 if required.
void StringConcatenationOptimizer::adjustEncodings() {
  if (SLILeft->getEncoding() == SLIRight->getEncoding()) {
    FRIConvertFromBuiltin = FRILeft;
    if (SLILeft->getEncoding() == StringLiteralInst::Encoding::UTF8) {
      FuncResultType = AILeft->getOperand(4);
    } else {
      FuncResultType = AILeft->getOperand(3);
    }
    return;
  }

  // If one of the string literals is UTF8 and another one is UTF16,
  // convert the UTF8-encoded string literal into UTF16-encoding first.
  if (SLILeft->getEncoding() == StringLiteralInst::Encoding::UTF8 &&
      SLIRight->getEncoding() == StringLiteralInst::Encoding::UTF16) {
    FuncResultType = AIRight->getOperand(3);
    FRIConvertFromBuiltin = FRIRight;
    // Convert UTF8 representation into UTF16.
    SLILeft = Builder.createStringLiteral(AI->getLoc(), SLILeft->getValue(),
                                          StringLiteralInst::Encoding::UTF16);
    SLILeft->setDebugScope(AI->getDebugScope());
  }

  if (SLIRight->getEncoding() == StringLiteralInst::Encoding::UTF8 &&
      SLILeft->getEncoding() == StringLiteralInst::Encoding::UTF16) {
    FuncResultType = AILeft->getOperand(3);
    FRIConvertFromBuiltin = FRILeft;
    // Convert UTF8 representation into UTF16.
    SLIRight = Builder.createStringLiteral(AI->getLoc(), SLIRight->getValue(),
                                           StringLiteralInst::Encoding::UTF16);
    SLIRight->setDebugScope(AI->getDebugScope());
  }

  // It should be impossible to have two operands with different
  // encodings at this point.
  assert(SLILeft->getEncoding() == SLIRight->getEncoding() &&
        "Both operands of string concatenation should have the same encoding");
}

/// Computes the length of a concatenated string literal.
APInt StringConcatenationOptimizer::getConcatenatedLength() {
  // Real length of string literals computed based on its contents.
  // Length is in code units.
  auto SLILenLeft = SLILeft->getCodeUnitCount();
  (void) SLILenLeft;
  auto SLILenRight = SLIRight->getCodeUnitCount();
  (void) SLILenRight;

  // Length of string literals as reported by string.make functions.
  auto *LenLeft = dyn_cast<IntegerLiteralInst>(AILeft->getOperand(2));
  auto *LenRight = dyn_cast<IntegerLiteralInst>(AIRight->getOperand(2));

  // Real and reported length should be the same.
  assert(SLILenLeft == LenLeft->getValue() &&
         "Size of string literal in @semantics(string.make) is wrong");

  assert(SLILenRight == LenRight->getValue() &&
         "Size of string literal in @semantics(string.make) is wrong");


  // Compute length of the concatenated literal.
  return LenLeft->getValue() + LenRight->getValue();
}

/// Computes the isAscii flag of a concatenated UTF8-encoded string literal.
bool StringConcatenationOptimizer::isAscii() const{
  // Add the isASCII argument in case of UTF8.
  // IsASCII is true only if IsASCII of both literals is true.
  auto *AsciiLeft = dyn_cast<IntegerLiteralInst>(AILeft->getOperand(3));
  auto *AsciiRight = dyn_cast<IntegerLiteralInst>(AIRight->getOperand(3));
  auto IsAsciiLeft = AsciiLeft->getValue() == 1;
  auto IsAsciiRight = AsciiRight->getValue() == 1;
  return IsAsciiLeft && IsAsciiRight;
}

SILInstruction *StringConcatenationOptimizer::optimize() {
  // Bail out if string literals concatenation optimization is
  // not possible.
  if (!extractStringConcatOperands())
    return nullptr;

  // Perform string literal encodings adjustments if needed.
  adjustEncodings();

  // Arguments of the new StringLiteralInst to be created.
  SmallVector<SILValue, 4> Arguments;

  // Encoding to be used for the concatenated string literal.
  auto Encoding = SLILeft->getEncoding();

  // Create a concatenated string literal.
  auto LV = SLILeft->getValue();
  auto RV = SLIRight->getValue();
  auto *NewSLI =
      Builder.createStringLiteral(AI->getLoc(), LV + Twine(RV), Encoding);
  NewSLI->setDebugScope(AI->getDebugScope());
  Arguments.push_back(NewSLI);

  // Length of the concatenated literal according to its encoding.
  auto *Len = Builder.createIntegerLiteral(
      AI->getLoc(), AILeft->getOperand(2).getType(), getConcatenatedLength());
  Len->setDebugScope(AI->getDebugScope());
  Arguments.push_back(Len);

  // isAscii flag for UTF8-encoded string literals.
  if (Encoding == StringLiteralInst::Encoding::UTF8) {
    bool IsAscii = isAscii();
    auto ILType = AILeft->getOperand(3).getType();
    auto *Ascii =
        Builder.createIntegerLiteral(AI->getLoc(), ILType, intmax_t(IsAscii));
    Ascii->setDebugScope(AI->getDebugScope());
    Arguments.push_back(Ascii);
  }

  // Type.
  Arguments.push_back(FuncResultType);

  auto FnTy = FRIConvertFromBuiltin->getType();
  auto STResultType = FnTy.castTo<SILFunctionType>()->getResult().getSILType();
  return ApplyInst::create(AI->getLoc(),
                           FRIConvertFromBuiltin,
                           FnTy,
                           STResultType,
                           ArrayRef<Substitution>(),
                           Arguments,
                           *FRIConvertFromBuiltin->getReferencedFunction());
}

/// Top level entry point
SILInstruction *swift::tryToConcatenateStrings(ApplyInst *AI, SILBuilder &B) {
  return StringConcatenationOptimizer(AI, B).optimize();
}

//===----------------------------------------------------------------------===//
//                              Closure Deletion
//===----------------------------------------------------------------------===//

static bool isARCOperationRemovableIfObjectIsDead(const SILInstruction *I) {
  switch (I->getKind()) {
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongReleaseInst:
  case ValueKind::RetainValueInst:
  case ValueKind::ReleaseValueInst:
    return true;
  default:
    return false;
  }
}

/// TODO: Generalize this to general objects.
bool swift::tryDeleteDeadClosure(SILInstruction *Closure) {
  // We currently only handle locally identified values that do not escape. We
  // also assume that the partial apply does not capture any addresses.
  if (!isa<PartialApplyInst>(Closure) && !isa<ThinToThickFunctionInst>(Closure))
    return false;

  // We only accept a user if it is an ARC object that can be removed if the
  // object is dead. This should be expanded in the future. This also ensures
  // that we are locally identified and non-escaping since we only allow for
  // specific ARC users.
  ReleaseTracker Tracker([](const SILInstruction *I) -> bool {
    return isARCOperationRemovableIfObjectIsDead(I);
  });

  // Find the ARC Users and the final retain, release.
  if (!getFinalReleasesForValue(SILValue(Closure), Tracker))
    return false;

  // If we have a partial_apply, release each captured argument at each one of
  // the final release locations of the partial apply.
  SILBuilder Builder(Closure);
  SILModule &M = Closure->getModule();
  if (auto *PAI = dyn_cast<PartialApplyInst>(Closure)) {
    for (auto *FinalRelease : Tracker.getFinalReleases()) {
      Builder.setInsertionPoint(FinalRelease);
      for (SILValue Arg : PAI->getArguments()) {
        if (Arg.getType().isTrivial(M))
          continue;
        Builder.createReleaseValue(FinalRelease->getLoc(), Arg);
      }
    }
  }

  // Then delete all user instructions.
  for (auto *User : Tracker.getTrackedUsers()) {
    assert(User->getNumTypes() == 0 && "We expect only ARC operations without "
                                       "results. This is true b/c of "
                                       "isARCOperationRemovableIfObjectIsDead");
    User->eraseFromParent();
  }

  // Finally delete the closure.
  Closure->eraseFromParent();

  return true;
}

// Is any successor of BB in the LiveIn set?
static bool successorHasLiveIn(SILBasicBlock *BB,
                         const llvm::SmallPtrSetImpl<SILBasicBlock *> &LiveIn) {
  for (auto &Succ : BB->getSuccessors())
    if (LiveIn.count(Succ))
      return true;

  return false;
}


// Walk backwards in BB looking for last use of value V and adding the
// instruction using the value to LastUsers.
static void addLastUser(SILValue V, SILBasicBlock *BB,
                        llvm::SmallPtrSetImpl<SILInstruction *> &LastUsers) {
  for (auto I = BB->rbegin(); I != BB->rend(); ++I) {
    assert(V.getDef() != &*I && "Found def before finding use!");

    for (auto &O : I->getAllOperands()) {
      if (O.get() != V)
        continue;

      LastUsers.insert(&*I);
      return;
    }
  }

  llvm_unreachable("Expected to find use of value in block!");
}

// Propagate liveness backwards from an initial set of blocks in our
// LiveIn set.
static void propagateLiveness(llvm::SmallPtrSetImpl<SILBasicBlock*> &LiveIn,
                              SILBasicBlock *DefBB) {

  // First populate a worklist of predecessors.
  llvm::SmallVector<SILBasicBlock *, 64> Worklist;
  for (auto *BB : LiveIn)
    for (auto Pred : BB->getPreds())
      Worklist.push_back(Pred);

  // Now propagate liveness backwards until we hit the block that
  // defines the value.
  while (!Worklist.empty()) {
    auto *BB = Worklist.pop_back_val();

    // If it's already in the set, then we've already queued and/or
    // processed the predecessors.
    if (BB == DefBB || !LiveIn.insert(BB).second)
      continue;

    for (auto Pred : BB->getPreds())
      Worklist.push_back(Pred);
  }
}

void LifetimeTracker::computeLifetime() {
  llvm::SmallPtrSet<SILBasicBlock *, 16> LiveIn;
  llvm::SmallPtrSet<SILBasicBlock *, 16> UseBlocks;

  auto *DefInst = cast<SILInstruction>(TheValue.getDef());
  auto *DefBB = DefInst->getParent();

  if (TheValue->hasOneUse()) {
    Endpoints.insert(TheValue->use_begin().getUser());
    return;
  }

  for (auto UI : TheValue.getUses()) {
    auto *BB = UI->getUser()->getParent();

    UseBlocks.insert(BB);
    if (BB != DefBB)
      LiveIn.insert(BB);
  }

  propagateLiveness(LiveIn, DefBB);

  for (auto *BB : UseBlocks)
    if (!successorHasLiveIn(BB, LiveIn))
        addLastUser(TheValue, BB, Endpoints);

  LifetimeComputed = true;
}

//===----------------------------------------------------------------------===//
//                    Casts Optimization and Simplification
//===----------------------------------------------------------------------===//

/// \brief  Get a substitution corresponding to the type witness.
/// Inspired by ProtocolConformance::getTypeWitnessByName.
static const Substitution *
getTypeWitnessByName(ProtocolConformance *conformance,
                     Identifier name,
                     LazyResolver *resolver) {
  // Find the named requirement.
  AssociatedTypeDecl *assocType = nullptr;
  auto members = conformance->getProtocol()->lookupDirect(name);
  for (auto member : members) {
    assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (assocType)
      break;
  }

  if (!assocType)
    return nullptr;

  assert(conformance && "Missing conformance information");
  if (!conformance->hasTypeWitness(assocType, resolver)) {
    return nullptr;
  }
  return &conformance->getTypeWitness(assocType, resolver);
}

/// Check if is a bridging cast, i.e. one of the sides is
/// a bridged type.
static bool isBridgingCast(CanType SourceType, CanType TargetType) {
  // Bridging casts cannot be further simplified.
  auto TargetIsBridgeable = TargetType->isBridgeableObjectType();
  auto SourceIsBridgeable = SourceType->isBridgeableObjectType();

  if (TargetIsBridgeable != SourceIsBridgeable)
    return true;

  return false;
}

/// If target is a Swift type bridging to an ObjC type,
/// return the ObjC type it bridges to.
/// If target is an ObjC type, return this type.
static Type getCastFromObjC(SILModule &M, CanType source, CanType target) {
  Optional<Type> BridgedTy = M.getASTContext().getBridgedToObjC(
      M.getSwiftModule(),
      /*inExpression*/ false, target, nullptr);
  if (!BridgedTy.hasValue() || !BridgedTy.getValue())
    return Type();
  return BridgedTy.getValue();
}

/// Create a call of _forceBridgeFromObjectiveC or
/// _knownConditionallyBridgeFromObjectiveC  which converts an an ObjC instance
/// into a corresponding Swift type, conforming to  _ObjectiveCBridgeable.
SILInstruction *
CastOptimizer::
optimizeBridgedObjCToSwiftCast(SILInstruction *Inst,
                     bool isConditional,
                     SILValue Src,
                     SILValue Dest,
                     CanType Source,
                     CanType Target,
                     Type BridgedSourceTy,
                     Type BridgedTargetTy,
                     SILBasicBlock *SuccessBB,
                     SILBasicBlock *FailureBB) {
  auto &M = Inst->getModule();
  auto Loc = Inst->getLoc();

  CanType CanBridgedTy(BridgedTargetTy);
  SILType SILBridgedTy = SILType::getPrimitiveObjectType(CanBridgedTy);

  SILBuilderWithScope<1> Builder(Inst);
  SILValue SrcOp;
  SILInstruction *NewI = nullptr;

  assert(Src.getType().isAddress() && "Source should have an address type");
  assert(Dest.getType().isAddress() && "Source should have an address type");

  if (SILBridgedTy != Src.getType()) {
    // Check if we can simplify a cast into:
    // - ObjCTy to _ObjectiveCBridgeable._ObjectiveCType.
    // - then convert _ObjectiveCBridgeable._ObjectiveCType to
    // a Swift type using _forceBridgeFromObjectiveC.

    // Generate a load for the source argument.
    auto *Load = Builder.createLoad(Loc, Src);
    // Try to convert the source into the expected ObjC type first.
    // TODO: If type of the source and the expected ObjC type are
    // equal, there is no need to generate the conversion.
    if (isConditional) {
      SILBasicBlock *CastSuccessBB = Inst->getFunction()->createBasicBlock();
      CastSuccessBB->createBBArg(SILBridgedTy);
      NewI = Builder.createCheckedCastBranch(Loc, false, SILValue(Load, 0),
                                             SILBridgedTy, CastSuccessBB,
                                             FailureBB);
      Builder.setInsertionPoint(CastSuccessBB);
      SrcOp = SILValue(CastSuccessBB->getBBArg(0), 0);
    } else {
      NewI = Builder.createUnconditionalCheckedCast(Loc, SILValue(Load, 0),
                                                    SILBridgedTy);
      SrcOp = SILValue(NewI, 0);
    }
  } else {
    SrcOp = Src;
  }

  // Now emit the a cast from the casted ObjC object into a target type.
  // This is done by means of calling _forceBridgeFromObjectiveC or
  // _knownConditionallyBridgeFromObjectiveC from the Target type.
  // Lookup the required function in the Target type.

  // Lookup the _ObjectiveCBridgeable protocol.
  auto BridgedProto =
      M.getASTContext().getProtocol(KnownProtocolKind::_ObjectiveCBridgeable);
  auto Conf =
      M.getSwiftModule()->lookupConformance(Target, BridgedProto, nullptr);
  assert(Conf.getInt() == ConformanceKind::Conforms &&
         "_ObjectiveCBridgeable conformance should exist");

  auto *Conformance = Conf.getPointer();

  // The conformance to _BridgedToObjectiveC is statically known.
  // Retrieve the  bridging operation to be used if a static conformance
  // to _BridgedToObjectiveC can be proven.
  FuncDecl *BridgeFuncDecl =
      isConditional
          ? M.getASTContext().getKnownConditionallyBridgeFromObjectiveC(nullptr)
          : M.getASTContext().getKnownForceBridgeFromObjectiveC(nullptr);

  assert(BridgeFuncDecl && "_forceBridgeFromObjectiveC should exist");
  StringRef BridgeFuncName = isConditional
                                 ? "_knownConditionallyBridgeFromObjectiveC"
                                 : "_knownForceBridgeFromObjectiveC";

  SILDeclRef FuncDeclRef(BridgeFuncDecl, SILDeclRef::Kind::Func);

  // Lookup a function from the stdlib.
  SILFunction *BridgedFunc = M.lookUpFunction(BridgeFuncName);
  if (!BridgedFunc) {
    // If function is not found, try to link it.
    M.linkFunction(FuncDeclRef);
    BridgedFunc = M.lookUpFunction(BridgeFuncName);
  }

  assert(BridgedFunc && "Bridging function was not found");

  auto *FuncRef = Builder.createFunctionRef(Loc, BridgedFunc);

  auto MetaTy = MetatypeType::get(Target, MetatypeRepresentation::Thick);
  auto SILMetaTy = M.Types.getTypeLowering(MetaTy, 0).getLoweredType();
  auto *MetaTyVal = Builder.createMetatype(Loc, SILMetaTy);
  SmallVector<SILValue, 1> Args;

  auto PolyFuncTy = BridgeFuncDecl->getType()->getAs<PolymorphicFunctionType>();
  ArrayRef<ArchetypeType *> Archetypes =
      PolyFuncTy->getGenericParams().getAllArchetypes();

  // Add substitutions
  SmallVector<Substitution, 2> Subs;
  auto Conformances = M.getASTContext().Allocate<ProtocolConformance *>(1);
  Conformances[0] = Conformance;
  Subs.push_back(Substitution(Archetypes[0], Target, Conformances));
  const Substitution *DepTypeSubst = getTypeWitnessByName(
      Conformance, M.getASTContext().getIdentifier("_ObjectiveCType"), nullptr);
  Subs.push_back(Substitution(Archetypes[1], DepTypeSubst->getReplacement(),
                              DepTypeSubst->getConformances()));
  auto SILFnTy = FuncRef->getType();
  SILType SubstFnTy = SILFnTy.substGenericArgs(M, Subs);
  SILType ResultTy = SubstFnTy.castTo<SILFunctionType>()->getSILResult();

  // Temporary to hold the intermediate result.
  AllocStackInst *Tmp = nullptr;
  CanType OptionalTy;
  OptionalTypeKind OTK;
  SILValue InOutOptionalParam;
  if (isConditional) {
    // Create a temporary
    OptionalTy = OptionalType::get(Dest.getType().getSwiftRValueType())
                     ->getImplementationType()
                     .getCanonicalTypeOrNull();
    OptionalTy.getAnyOptionalObjectType(OTK);
    Tmp = Builder.createAllocStack(Loc,
                                   SILType::getPrimitiveObjectType(OptionalTy));
    InOutOptionalParam = SILValue(Tmp, 1);
  } else {
    InOutOptionalParam = Dest;
  }

  Args.push_back(InOutOptionalParam);
  Args.push_back(SrcOp);
  Args.push_back(SILValue(MetaTyVal, 0));

  auto *AI = Builder.createApply(Loc, FuncRef, SubstFnTy, ResultTy, Subs, Args);
  if (isConditional) {
    // Copy the temporary into Dest.
    // Load from the optional
    auto *SomeDecl = Builder.getASTContext().getOptionalSomeDecl(OTK);
    bool isNotAddressOnly = !InOutOptionalParam.getType().isTrivial(M) &&
                            !InOutOptionalParam.getType().isAddressOnly(M);
    auto Addr = Builder.createUncheckedTakeEnumDataAddr(Loc, InOutOptionalParam,
                                                        SomeDecl);
    auto LoadFromOptional = Builder.createLoad(Loc, SILValue(Addr, 0));
    if (isNotAddressOnly)
      Builder.createRetainValue(Loc, LoadFromOptional);
    // Store into Dest
    Builder.createStore(Loc, LoadFromOptional, Dest);
    if (isNotAddressOnly)
      Builder.createReleaseValue(Loc, LoadFromOptional);
    Builder.createDeallocStack(Loc, SILValue(Tmp, 0));
    SmallVector<SILValue, 1> SuccessBBArgs;
    Builder.createBranch(Loc, SuccessBB, SuccessBBArgs);
  }

  EraseInstAction(Inst);
  return (NewI) ? NewI : AI;
}

/// Create a call of _bridgeToObjectiveC which converts an _ObjectiveCBridgeable
/// instance into a bridged ObjC type.
SILInstruction *
CastOptimizer::
optimizeBridgedSwiftToObjCCast(SILInstruction *Inst,
                     bool isConditional,
                     SILValue Src,
                     SILValue Dest,
                     CanType Source,
                     CanType Target,
                     Type BridgedSourceTy,
                     Type BridgedTargetTy,
                     SILBasicBlock *SuccessBB,
                     SILBasicBlock *FailureBB) {

  auto &M = Inst->getModule();
  auto Loc = Inst->getLoc();

  // Find the _BridgedToObjectiveC protocol.
  auto BridgedProto =
      M.getASTContext().getProtocol(KnownProtocolKind::_ObjectiveCBridgeable);

  auto Conf =
      M.getSwiftModule()->lookupConformance(Source, BridgedProto, nullptr);

  assert(Conf.getInt() == ConformanceKind::Conforms &&
         "_ObjectiveCBridgeable conformance should exist");

  // Generate code to invoke _bridgeToObjectiveC
  SILBuilderWithScope<1> Builder(Inst);

  auto Members = Source.getNominalOrBoundGenericNominal()->lookupDirect(
      M.getASTContext().Id_bridgeToObjectiveC);
  assert(Members.size() == 1 &&
         "There should be exactly one implementation of _bridgeToObjectiveC");
  auto BridgeFuncDecl = Members.front();
  auto BridgeFuncDeclRef = SILDeclRef(BridgeFuncDecl);
  Module *Mod = M.getASTContext().getLoadedModule(
      M.getASTContext().getIdentifier(FOUNDATION_MODULE_NAME));
  assert(Mod && "Foundation module should be present");
  SmallVector<ValueDecl *, 2> Results;
  Mod->lookupMember(Results, Source.getNominalOrBoundGenericNominal(),
                    M.getASTContext().Id_bridgeToObjectiveC, Identifier());
  ArrayRef<ValueDecl *> ResultsRef(Results);
  assert(ResultsRef.size() == 1 && "There should be only one declaration of _bridgeToObjectiveC");

  auto MemberDeclRef = SILDeclRef(Results.front());
  auto *BridgeFunc = M.getOrCreateFunction(Loc, MemberDeclRef, ForDefinition_t::NotForDefinition);
  assert(BridgeFunc && "Implementation of _bridgeToObjectiveC could not be found");

  auto SILFnTy = SILType::getPrimitiveObjectType(
      M.Types.getConstantFunctionType(BridgeFuncDeclRef));
  ArrayRef<Substitution> Subs;
  if (Source.getNominalOrBoundGenericNominal()->getGenericSignature()) {
    // Get substitutions, if source is a bound generic type.
    Subs = Source->castTo<BoundGenericType>()->getSubstitutions(
        M.getSwiftModule(), nullptr);
  }

  SILType SubstFnTy = SILFnTy.substGenericArgs(M, Subs);
  SILType ResultTy = SubstFnTy.castTo<SILFunctionType>()->getSILResult();

  auto FnRef = Builder.createFunctionRef(Loc, BridgeFunc);
  if (Src.getType().isAddress()) {
    // Create load
    Src = SILValue(Builder.createLoad(Loc, Src), 0);
  }

  // Generate a code to invoke the briding function.
  auto *NewAI = Builder.createApply(Loc, FnRef, SubstFnTy, ResultTy, Subs, Src);

  SILInstruction *NewI = NewAI;

  if (Dest) {
    // If it is addr cast then store the result.
    NewI = Builder.createStore(Loc, SILValue(NewAI, 0), Dest);
    // Insert a retain if required
    if (Dest.getType().isAddress()) {
      NewI = Builder.createStrongRetain(Loc, NewAI);
    }
    EraseInstAction(Inst);
  }

  return NewI;
}

/// Make use of the fact that some of these casts cannot fail.
/// For example, if the ObjC type is exactly the expected
/// _ObjectiveCType type, then it would always succeed for
/// NSString, NSNumber, etc.
/// Casts from NSArray, NSDictionary and NSSet may fail.
///
/// If ObjC class is not exactly _ObjectiveCType, then
/// its conversion to a required _ObjectiveCType may fail.
SILInstruction *
CastOptimizer::
optimizeBridgedCasts(SILInstruction *Inst,
                     bool isConditional,
                     SILValue Src,
                     SILValue Dest,
                     CanType source,
                     CanType target,
                     SILBasicBlock *SuccessBB,
                     SILBasicBlock *FailureBB) {

  auto &M = Inst->getModule();

  // To apply the bridged optimizations, we should
  // ensure that types are not existential,
  // and that one of the types is a class and another
  // one is a struct.
  if (source.isAnyExistentialType() ||
      target.isAnyExistentialType() ||
      (source.getClassOrBoundGenericClass() &&
       !target.getStructOrBoundGenericStruct()) ||
      (target.getClassOrBoundGenericClass() &&
       !source.getStructOrBoundGenericStruct()))
    return nullptr;

  auto BridgedTargetTy = getCastFromObjC(M, source, target);
  if (!BridgedTargetTy)
    return nullptr;

  auto BridgedSourceTy = getCastFromObjC(M, target, source);

  CanType CanBridgedTargetTy(BridgedTargetTy);
  CanType CanBridgedSourceTy(BridgedSourceTy);

  if (CanBridgedSourceTy == source && CanBridgedTargetTy == target) {
    assert("Both source and target type are ObjC types");
  }

  if (CanBridgedSourceTy != source && CanBridgedTargetTy != target) {
    assert("Both source and target type are Swift types");
  }

  if (CanBridgedSourceTy || CanBridgedTargetTy) {
    // Check what kind of conversion it is? ObjC->Swift or Swift-ObjC?
    if (CanBridgedTargetTy != target) {
      // This is an ObjC to Swift cast.
      return optimizeBridgedObjCToSwiftCast(Inst, isConditional, Src, Dest, source,
          target, BridgedSourceTy, BridgedTargetTy, SuccessBB, FailureBB);
    } else {
      // This is a Swift to ObjC cast
      return optimizeBridgedSwiftToObjCCast(Inst, isConditional, Src, Dest, source,
          target, BridgedSourceTy, BridgedTargetTy, SuccessBB, FailureBB);
    }
  }

  llvm_unreachable("Unknown kind of bridging");
}


SILInstruction *
CastOptimizer::
simplifyCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *Inst) {
  if (auto *I = optimizeCheckedCastAddrBranchInst(Inst))
    Inst = dyn_cast<CheckedCastAddrBranchInst>(I);

  auto Loc = Inst->getLoc();
  auto Src = Inst->getSrc();
  auto Dest = Inst->getDest();
  auto SourceType = Inst->getSourceType();
  auto TargetType = Inst->getTargetType();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();
  auto &Mod = Inst->getModule();

  SILBuilderWithScope<1> Builder(Inst);

  // Try to determine the outcome of the cast from a known type
  // to a protocol type at compile-time.
  bool isSourceTypeExact = isa<MetatypeInst>(Inst->getSrc());

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility = classifyDynamicCast(Mod.getSwiftModule(),
                          Src.getType().getSwiftRValueType(),
                          Dest.getType().getSwiftRValueType(),
                          isSourceTypeExact,
                          Mod.isWholeModule());

  if (Feasibility == DynamicCastFeasibility::MaySucceed) {
    return nullptr;
  }

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    if (shouldDestroyOnFailure(Inst->getConsumptionKind())) {
      auto &srcTL = Builder.getModule().getTypeLowering(Src.getType());
      srcTL.emitDestroyAddress(Builder, Loc, Src);
    }
    auto NewI = Builder.createBranch(Loc, FailureBB);
    EraseInstAction(Inst);
    WillFailAction();
    return NewI;
  }

  // Cast will succeed

  // Replace by unconditional_addr_cast, followed by a branch.
  // The unconditional_addr_cast can be skipped, if the result of a cast
  // is not used afterwards.
  bool ResultNotUsed = isa<AllocStackInst>(Dest.getDef());
  for (auto Use : Dest.getUses()) {
    auto *User = Use->getUser();
    if (isa<DeallocStackInst>(User) || User == Inst)
      continue;
    ResultNotUsed = false;
    break;
  }

  auto *BB = Inst->getParent();

  if (!ResultNotUsed) {
    SILInstruction *BridgedI = nullptr;

    // To apply the bridged optimizations, we should
    // ensure that types are not existential,
    // and that not both types are classes.
    BridgedI = optimizeBridgedCasts(Inst, true, Src, Dest, SourceType,
                                       TargetType, SuccessBB, FailureBB);


    if (!BridgedI) {
      if (!emitSuccessfulIndirectUnconditionalCast(
            Builder, Mod.getSwiftModule(), Loc, Inst->getConsumptionKind(), Src,
            SourceType, Dest, TargetType, Inst))
        // No optimization was possible.
        return nullptr;
      EraseInstAction(Inst);
    }
    SILInstruction *NewI = &BB->getInstList().back();
    if (!isa<TermInst>(NewI)) {
      //Builder.setInsertionPoint(BB->getInstList().end());
      NewI = BranchInst::create(Loc, SuccessBB, *BB->getParent());
      BB->getInstList().insert(BB->end(), NewI);
    }
    WillSucceedAction();
    return NewI;
  } else {
    // Result is not used.
    EraseInstAction(Inst);
    auto *NewI = BranchInst::create(Loc, SuccessBB, *BB->getParent());
    BB->getInstList().insert(BB->end(), NewI);
    WillSucceedAction();
    return NewI;
  }
}

SILInstruction *
CastOptimizer::simplifyCheckedCastBranchInst(CheckedCastBranchInst *Inst) {
  if (Inst->isExact()) {
    // Check if the exact dynamic type of the operand can be determined.
    if (auto *ARI = dyn_cast<AllocRefInst>(Inst->getOperand().stripUpCasts())) {
      SILBuilderWithScope<1> Builder(Inst);
      auto Loc = Inst->getLoc();
      auto *SuccessBB = Inst->getSuccessBB();
      auto *FailureBB = Inst->getFailureBB();

      if (ARI->getType() == Inst->getCastType()) {
        // This exact cast will succeed.
        SmallVector<SILValue, 1> Args;
        Args.push_back(ARI);
        auto *NewI = Builder.createBranch(Loc, SuccessBB, Args);
        EraseInstAction(Inst);
        WillSucceedAction();
        return NewI;
      } else {
        // This exact cast will fail.
        auto *NewI = Builder.createBranch(Loc, FailureBB);
        EraseInstAction(Inst);
        WillFailAction();
        return NewI;
      }
    }

    return nullptr;
  }

  if (auto *I = optimizeCheckedCastBranchInst(Inst))
    Inst = dyn_cast<CheckedCastBranchInst>(I);

  auto LoweredSourceType = Inst->getOperand().getType();
  auto LoweredTargetType = Inst->getCastType();
  auto SourceType = LoweredSourceType.getSwiftRValueType();
  auto TargetType = LoweredTargetType.getSwiftRValueType();
  auto Loc = Inst->getLoc();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();
  auto Op = Inst->getOperand();
  auto &Mod = Inst->getModule();
  bool isSourceTypeExact = isa<MetatypeInst>(Op);


  // Check if we can statically predict the outcome of the cast.
  auto Feasibility = classifyDynamicCast(Mod.getSwiftModule(),
                          SourceType,
                          TargetType,
                          isSourceTypeExact);

  if (Feasibility == DynamicCastFeasibility::MaySucceed) {
    return nullptr;
  }

  SILBuilderWithScope<1> Builder(Inst);

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    auto *NewI = Builder.createBranch(Loc, FailureBB);
    EraseInstAction(Inst);
    WillFailAction();
    return NewI;
  }

  // Casting will succeed.

  // Replace by unconditional_cast, followed by a branch.
  // The unconditional_cast can be skipped, if the result of a cast
  // is not used afterwards.
  bool ResultNotUsed = SuccessBB->getBBArg(0)->use_empty();
  SILValue CastedValue;
  if (Op.getType() != LoweredTargetType) {
    if (!ResultNotUsed) {
      auto Src = Inst->getOperand();
      auto Dest = SILValue();
      // To apply the bridged casts optimizations.
      auto BridgedI = optimizeBridgedCasts(Inst, false, Src, Dest, SourceType,
          TargetType, nullptr, nullptr);

      if (BridgedI) {
        CastedValue = SILValue(BridgedI, 0);
      }
      else
        CastedValue = emitSuccessfulScalarUnconditionalCast(
          Builder, Mod.getSwiftModule(), Loc, Op, LoweredTargetType,
          SourceType, TargetType, Inst);
      if (!CastedValue)
        CastedValue =
            Builder.createUnconditionalCheckedCast(Loc, Op, LoweredTargetType);
    } else {
      CastedValue = SILUndef::get(LoweredTargetType, Mod);
    }
  } else {
    // No need to cast.
    CastedValue = Op;
  }

  auto *NewI = Builder.createBranch(Loc, SuccessBB, CastedValue);
  EraseInstAction(Inst);
  WillSucceedAction();
  return NewI;
}

SILInstruction *
CastOptimizer::
optimizeCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *Inst) {
  auto Loc = Inst->getLoc();
  auto Src = Inst->getSrc();
  auto Dest = Inst->getDest();
  auto TargetType = Inst->getTargetType();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();

  // %1 = metatype $A.Type
  // [%2 = init_existential_metatype %1 ...]
  // %3 = alloc_stack
  // store %1 to %3 or store %2 to %3
  // checked_cast_addr_br %3 to ...
  // ->
  // %1 = metatype $A.Type
  // checked_cast_addr_br %1 to ...
  if (auto *ASI = dyn_cast<AllocStackInst>(Src.getDef())) {
    // Check if the value of this alloc_stack is set only once by a store
    // instruction, used only by CCABI and then deallocated.
    bool isLegal = true;
    StoreInst *Store = nullptr;
    for (auto Use : ASI->getUses()) {
      auto *User = Use->getUser();
      if (isa<DeallocStackInst>(User) || User == Inst)
        continue;
      if (auto *SI = dyn_cast<StoreInst>(User)) {
        if (!Store) {
          Store = SI;
          continue;
        }
      }
      isLegal = false;
      break;
    }

    if (isLegal && Store) {
      // Check what was the value stored in the allocated stack slot.
      auto Src = Store->getSrc();
      MetatypeInst *MI = nullptr;
      if (auto *IEMI = dyn_cast<InitExistentialMetatypeInst>(Src)) {
        MI = dyn_cast<MetatypeInst>(IEMI->getOperand());
      }

      if (!MI)
        MI = dyn_cast<MetatypeInst>(Src);

      if (MI) {
        SILBuilderWithScope<1> B(Inst);
        auto NewI = B.createCheckedCastAddrBranch(Loc,
                                  Inst->getConsumptionKind(),
                                  MI,
                                  MI->getType().getSwiftRValueType(),
                                  Dest,
                                  TargetType,
                                  SuccessBB,
                                  FailureBB);
        EraseInstAction(Inst);
        return NewI;
      }
    }
  }

  return nullptr;
}

SILInstruction *
CastOptimizer::optimizeCheckedCastBranchInst(CheckedCastBranchInst *Inst) {
  if (Inst->isExact())
    return nullptr;

  auto LoweredTargetType = Inst->getCastType();
  auto Loc = Inst->getLoc();
  auto *SuccessBB = Inst->getSuccessBB();
  auto *FailureBB = Inst->getFailureBB();
  auto Op = Inst->getOperand();

  // Try to simplify checked_cond_br instructions using existential
  // metatypes by propagating a concrete type whenever it can be
  // determined statically.

  // %0 = metatype $A.Type
  // %1 = init_existential_metatype ..., %0: $A
  // checked_cond_br %1, ....
  // ->
  // %1 = metatype $A.Type
  // checked_cond_br %1, ....
  if (auto *IEMI = dyn_cast<InitExistentialMetatypeInst>(Op)) {
    if (auto *MI = dyn_cast<MetatypeInst>(IEMI->getOperand())) {
      SILBuilderWithScope<1> B(Inst);
      auto *NewI = B.createCheckedCastBranch(Loc, /* isExact */ false, MI,
                                LoweredTargetType,
                                SuccessBB,
                                FailureBB);
      EraseInstAction(Inst);
      return NewI;
    }
  }

  if (auto *EMI = dyn_cast<ExistentialMetatypeInst>(Op)) {
    // Operand of the existential_metatype instruction.
    auto Op = EMI->getOperand();
    auto EmiTy = EMI->getType();

    // %0 = alloc_stack ..
    // %1 = init_existential_addr %0: $A
    // %2 = existential_metatype %0, ...
    // checked_cond_br %2, ....
    // ->
    // %1 = metatype $A.Type
    // checked_cond_br %1, ....

    if (auto *ASI = dyn_cast<AllocStackInst>(Op)) {
      // Should be in the same BB.
      if (ASI->getParent() != EMI->getParent())
        return nullptr;
      // Check if this alloc_stac is is only initialized once by means of
      // single init_existential_addr.
      bool isLegal = true;
      // init_existental instruction used to initialize this alloc_stack.
      InitExistentialAddrInst *FoundIEI = nullptr;
      for (auto Use: ASI->getUses()) {
        auto *User = Use->getUser();
        if (isa<ExistentialMetatypeInst>(User) ||
            isa<DestroyAddrInst>(User) ||
            isa<DeallocStackInst>(User))
           continue;
        if (auto *IEI = dyn_cast<InitExistentialAddrInst>(User)) {
          if (!FoundIEI) {
            FoundIEI = IEI;
            continue;
          }
        }
        isLegal = false;
        break;
      }

      if (isLegal && FoundIEI) {
        // Should be in the same BB.
        if (FoundIEI->getParent() != EMI->getParent())
          return nullptr;
        // Get the type used to initialize the existential.
        auto LoweredConcreteTy = FoundIEI->getLoweredConcreteType();
        if (LoweredConcreteTy.isAnyExistentialType())
          return nullptr;
        // Get the metatype of this type.
        auto EMT = dyn_cast<AnyMetatypeType>(EmiTy.getSwiftRValueType());
        auto *MetaTy = MetatypeType::get(LoweredConcreteTy.getSwiftRValueType(),
                                         EMT->getRepresentation());
        auto CanMetaTy = CanMetatypeType::CanTypeWrapper(MetaTy);
        auto SILMetaTy = SILType::getPrimitiveObjectType(CanMetaTy);
        SILBuilderWithScope<1> B(Inst);
        auto *MI = B.createMetatype(FoundIEI->getLoc(), SILMetaTy);

        auto *NewI = B.createCheckedCastBranch(Loc, /* isExact */ false, MI,
                                  LoweredTargetType,
                                  SuccessBB,
                                  FailureBB);
        EraseInstAction(Inst);
        return NewI;
      }
    }

    // %0 = alloc_ref $A
    // %1 = init_existential_ref %0: $A, $...
    // %2 = existential_metatype ..., %1 :  ...
    // checked_cond_br %2, ....
    // ->
    // %1 = metatype $A.Type
    // checked_cond_br %1, ....
    if (auto *FoundIERI = dyn_cast<InitExistentialRefInst>(Op)) {
      auto *ASRI = dyn_cast<AllocRefInst>(FoundIERI->getOperand());
      if (!ASRI)
        return nullptr;
      // Should be in the same BB.
      if (ASRI->getParent() != EMI->getParent())
        return nullptr;
      // Check if this alloc_stac is is only initialized once by means of
      // a single initt_existential_ref.
      bool isLegal = true;
      for (auto Use: ASRI->getUses()) {
        auto *User = Use->getUser();
        if (isa<ExistentialMetatypeInst>(User) || isa<StrongReleaseInst>(User))
           continue;
        if (auto *IERI = dyn_cast<InitExistentialRefInst>(User)) {
          if (IERI == FoundIERI) {
            continue;
          }
        }
        isLegal = false;
        break;
      }

      if (isLegal && FoundIERI) {
        // Should be in the same BB.
        if (FoundIERI->getParent() != EMI->getParent())
          return nullptr;
        // Get the type used to initialize the existential.
        auto ConcreteTy = FoundIERI->getFormalConcreteType();
        if (ConcreteTy.isAnyExistentialType())
          return nullptr;
        // Get the SIL metatype of this type.
        auto EMT = dyn_cast<AnyMetatypeType>(EMI->getType().getSwiftRValueType());
        auto *MetaTy = MetatypeType::get(ConcreteTy, EMT->getRepresentation());
        auto CanMetaTy = CanMetatypeType::CanTypeWrapper(MetaTy);
        auto SILMetaTy = SILType::getPrimitiveObjectType(CanMetaTy);
        SILBuilderWithScope<1> B(Inst);
        auto *MI = B.createMetatype(FoundIERI->getLoc(), SILMetaTy);

        auto *NewI = B.createCheckedCastBranch(Loc, /* isExact */ false, MI,
                                  LoweredTargetType,
                                  SuccessBB,
                                  FailureBB);
        EraseInstAction(Inst);
        return NewI;
      }
    }
  }

  return nullptr;
}

SILInstruction *
CastOptimizer::
optimizeUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *Inst) {
  auto LoweredSourceType = Inst->getOperand().getType();
  auto LoweredTargetType = Inst->getType();
  auto Loc = Inst->getLoc();
  auto Op = Inst->getOperand();
  auto &Mod = Inst->getModule();

  bool isSourceTypeExact = isa<MetatypeInst>(Op);

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility = classifyDynamicCast(Mod.getSwiftModule(),
                          LoweredSourceType.getSwiftRValueType(),
                          LoweredTargetType.getSwiftRValueType(),
                          isSourceTypeExact);

  if (Feasibility == DynamicCastFeasibility::MaySucceed) {
    return nullptr;
  }

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    // Remove the cast and insert a trap, followed by an
    // unreachable instruction.
    SILBuilderWithScope<1> Builder(Inst);
    auto *Trap = Builder.createBuiltinTrap(Loc);
    Inst->replaceAllUsesWithUndef();
    EraseInstAction(Inst);
    Builder.setInsertionPoint(std::next(SILBasicBlock::iterator(Trap)));
    Builder.createUnreachable(ArtificialUnreachableLocation());
    WillFailAction();
    return Trap;
  }

  if (Feasibility == DynamicCastFeasibility::WillSucceed) {
    SILBuilderWithScope<1> Builder(Inst);

    // Try to apply the bridged casts optimizations
    auto SourceType = LoweredSourceType.getSwiftRValueType();
    auto TargetType = LoweredTargetType.getSwiftRValueType();
    auto Src = Inst->getOperand();
    auto NewI = optimizeBridgedCasts(Inst, false, Src, SILValue(), SourceType,
        TargetType, nullptr, nullptr);
    if (NewI) {
      ReplaceInstUsesAction(Inst, NewI);
      EraseInstAction(Inst);
      WillSucceedAction();
      return NewI;
    }

    if (isBridgingCast(SourceType, TargetType))
      return nullptr;

    auto Result = emitSuccessfulScalarUnconditionalCast(Builder,
                      Mod.getSwiftModule(), Loc, Op,
                      LoweredTargetType,
                      LoweredSourceType.getSwiftRValueType(),
                      LoweredTargetType.getSwiftRValueType(),
                      Inst);

    if (!Result) {
      // No optimization was possible.
      return nullptr;
    }

    ReplaceInstUsesAction(Inst, Result.getDef());
    EraseInstAction(Inst);
    WillSucceedAction();
    return dyn_cast<SILInstruction>(Result.getDef());
  }

  return nullptr;
}


SILInstruction *
CastOptimizer::
optimizeUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *Inst) {
  auto Loc = Inst->getLoc();
  auto Src = Inst->getSrc();
  auto Dest = Inst->getDest();
  auto SourceType = Inst->getSourceType();
  auto TargetType = Inst->getTargetType();
  auto &Mod = Inst->getModule();

  bool isSourceTypeExact = isa<MetatypeInst>(Src);

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility = classifyDynamicCast(Mod.getSwiftModule(), SourceType,
                                         TargetType, isSourceTypeExact);

  if (Feasibility == DynamicCastFeasibility::MaySucceed) {
    return nullptr;
  }

  if (Feasibility == DynamicCastFeasibility::WillFail) {
    // Remove the cast and insert a trap, followed by an
    // unreachable instruction.
    SILBuilderWithScope<1> Builder(Inst);
    SILInstruction *NewI = Builder.createBuiltinTrap(Loc);
    // mem2reg's invariants get unhappy if we don't try to
    // initialize a loadable result.
    auto DestType = Dest.getType();
    auto &resultTL = Mod.Types.getTypeLowering(DestType);
    if (!resultTL.isAddressOnly()) {
      auto undef = SILValue(SILUndef::get(DestType.getObjectType(),
                                          Builder.getModule()));
      NewI = Builder.createStore(Loc, undef, Dest);
    }
    Inst->replaceAllUsesWithUndef();
    EraseInstAction(Inst);
    Builder.setInsertionPoint(std::next(SILBasicBlock::iterator(NewI)));
    Builder.createUnreachable(ArtificialUnreachableLocation());
    WillFailAction();
  }

  if (Feasibility == DynamicCastFeasibility::WillSucceed) {

    bool ResultNotUsed = isa<AllocStackInst>(Dest.getDef());
    for (auto Use : Dest.getUses()) {
      auto *User = Use->getUser();
      if (isa<DeallocStackInst>(User) || User == Inst)
        continue;
      ResultNotUsed = false;
      break;
    }

    if (ResultNotUsed) {
      EraseInstAction(Inst);
      WillSucceedAction();
      return nullptr;
    }

    // Try to apply the bridged casts optimizations
    auto NewI = optimizeBridgedCasts(Inst, false, Src, Dest, SourceType,
                                         TargetType, nullptr, nullptr);
    if (NewI) {
        WillSucceedAction();
        return nullptr;
    }

    if (isBridgingCast(SourceType, TargetType))
      return nullptr;

    SILBuilderWithScope<1> Builder(Inst);
    if (!emitSuccessfulIndirectUnconditionalCast(Builder, Mod.getSwiftModule(),
                                            Loc, Inst->getConsumptionKind(),
                                            Src, SourceType,
                                            Dest, TargetType, Inst)) {
      // No optimization was possible.
      return nullptr;
    }

    Inst->replaceAllUsesWithUndef();
    EraseInstAction(Inst);
    WillSucceedAction();
  }

  return nullptr;
}

