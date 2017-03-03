//===--- SpeculativeDevirtualizer.cpp - Speculatively devirtualize calls --===//
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
// Speculatively devirtualizes witness- and class-method calls into direct
// calls.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-speculative-devirtualizer"

#include "swift/Basic/DemangleWrappers.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/AST/ASTContext.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

// This is the limit for the number of subclasses (jump targets) that the
// speculative devirtualizer will try to predict.
static const int MaxNumSpeculativeTargets = 6;

STATISTIC(NumTargetsPredicted, "Number of monomorphic functions predicted");

// A utility function for cloning the apply instruction.
static FullApplySite CloneApply(FullApplySite AI, SILBuilder &Builder) {
  // Clone the Apply.
  Builder.setCurrentDebugScope(AI.getDebugScope());
  Builder.addOpenedArchetypeOperands(AI.getInstruction());
  auto Args = AI.getArguments();
  SmallVector<SILValue, 8> Ret(Args.size());
  for (unsigned i = 0, e = Args.size(); i != e; ++i)
    Ret[i] = Args[i];

  FullApplySite NAI;

  switch (AI.getInstruction()->getKind()) {
  case ValueKind::ApplyInst:
    NAI = Builder.createApply(AI.getLoc(), AI.getCallee(),
                                   AI.getSubstCalleeSILType(),
                                   AI.getType(),
                                   AI.getSubstitutions(),
                                   Ret,
                                   cast<ApplyInst>(AI)->isNonThrowing());
    break;
  case ValueKind::TryApplyInst: {
    auto *TryApplyI = cast<TryApplyInst>(AI.getInstruction());
    NAI = Builder.createTryApply(AI.getLoc(), AI.getCallee(),
                                      AI.getSubstCalleeSILType(),
                                      AI.getSubstitutions(),
                                      Ret,
                                      TryApplyI->getNormalBB(),
                                      TryApplyI->getErrorBB());
    }
    break;
  default:
    llvm_unreachable("Trying to clone an unsupported apply instruction");
  }

  NAI.getInstruction();
  return NAI;
}

/// Insert monomorphic inline caches for a specific class or metatype
/// type \p SubClassTy.
static FullApplySite speculateMonomorphicTarget(FullApplySite AI,
                                                SILType SubType,
                                                CheckedCastBranchInst *&CCBI) {
  CCBI = nullptr;
  // Bail if this class_method cannot be devirtualized.
  if (!canDevirtualizeClassMethod(AI, SubType))
    return FullApplySite();

  if (SubType.getSwiftRValueType()->hasDynamicSelfType())
    return FullApplySite();

  // Create a diamond shaped control flow and a checked_cast_branch
  // instruction that checks the exact type of the object.
  // This cast selects between two paths: one that calls the slow dynamic
  // dispatch and one that calls the specific method.
  auto It = AI.getInstruction()->getIterator();
  SILFunction *F = AI.getFunction();
  SILBasicBlock *Entry = AI.getParent();

  // Iden is the basic block containing the direct call.
  SILBasicBlock *Iden = F->createBasicBlock();
  // Virt is the block containing the slow virtual call.
  SILBasicBlock *Virt = F->createBasicBlock();
  Iden->createPHIArgument(SubType, ValueOwnershipKind::Owned);

  SILBasicBlock *Continue = Entry->split(It);

  SILBuilderWithScope Builder(Entry, AI.getInstruction());
  // Create the checked_cast_branch instruction that checks at runtime if the
  // class instance is identical to the SILType.

  ClassMethodInst *CMI = cast<ClassMethodInst>(AI.getCallee());

  CCBI = Builder.createCheckedCastBranch(AI.getLoc(), /*exact*/ true,
                                       CMI->getOperand(), SubType, Iden,
                                       Virt);
  It = CCBI->getIterator();

  SILBuilderWithScope VirtBuilder(Virt, AI.getInstruction());
  SILBuilderWithScope IdenBuilder(Iden, AI.getInstruction());
  // This is the class reference downcasted into subclass SubType.
  SILValue DownCastedClassInstance = Iden->getArgument(0);

  // Copy the two apply instructions into the two blocks.
  FullApplySite IdenAI = CloneApply(AI, IdenBuilder);
  FullApplySite VirtAI = CloneApply(AI, VirtBuilder);

  // See if Continue has a release on self as the instruction right after the
  // apply. If it exists, move it into position in the diamond.
  SILBasicBlock::iterator next =
      next_or_end(Continue->begin(), Continue->end());
  auto *Release =
      (next == Continue->end()) ? nullptr : dyn_cast<StrongReleaseInst>(next);
  if (Release && Release->getOperand() == CMI->getOperand()) {
    VirtBuilder.createStrongRelease(Release->getLoc(), CMI->getOperand(),
                                    Release->getAtomicity());
    IdenBuilder.createStrongRelease(Release->getLoc(), DownCastedClassInstance,
                                    Release->getAtomicity());
    Release->eraseFromParent();
  }

  // Create a PHInode for returning the return value from both apply
  // instructions.
  SILArgument *Arg =
      Continue->createPHIArgument(AI.getType(), ValueOwnershipKind::Owned);
  if (!isa<TryApplyInst>(AI)) {
    IdenBuilder.createBranch(AI.getLoc(), Continue,
                             ArrayRef<SILValue>(IdenAI.getInstruction()));
    VirtBuilder.createBranch(AI.getLoc(), Continue,
                             ArrayRef<SILValue>(VirtAI.getInstruction()));
  }

  // Remove the old Apply instruction.
  assert(AI.getInstruction() == &Continue->front() &&
         "AI should be the first instruction in the split Continue block");
  if (!isa<TryApplyInst>(AI)) {
    AI.getInstruction()->replaceAllUsesWith(Arg);
    AI.getInstruction()->eraseFromParent();
    assert(!Continue->empty() &&
           "There should be at least a terminator after AI");
  } else {
    AI.getInstruction()->eraseFromParent();
    assert(Continue->empty() &&
           "There should not be an instruction after try_apply");
    Continue->eraseFromParent();
  }

  // Update the stats.
  NumTargetsPredicted++;

  // Devirtualize the apply instruction on the identical path.
  auto NewInstPair = devirtualizeClassMethod(IdenAI, DownCastedClassInstance);
  assert(NewInstPair.first && "Expected to be able to devirtualize apply!");
  replaceDeadApply(IdenAI, NewInstPair.first);

  // Split critical edges resulting from VirtAI.
  if (auto *TAI = dyn_cast<TryApplyInst>(VirtAI)) {
    auto *ErrorBB = TAI->getFunction()->createBasicBlock();
    ErrorBB->createPHIArgument(TAI->getErrorBB()->getArgument(0)->getType(),
                               ValueOwnershipKind::Owned);
    Builder.setInsertionPoint(ErrorBB);
    Builder.createBranch(TAI->getLoc(), TAI->getErrorBB(),
                         {ErrorBB->getArgument(0)});

    auto *NormalBB = TAI->getFunction()->createBasicBlock();
    NormalBB->createPHIArgument(TAI->getNormalBB()->getArgument(0)->getType(),
                                ValueOwnershipKind::Owned);
    Builder.setInsertionPoint(NormalBB);
    Builder.createBranch(TAI->getLoc(), TAI->getNormalBB(),
                         {NormalBB->getArgument(0)});

    Builder.setInsertionPoint(VirtAI.getInstruction());
    SmallVector<SILValue, 4> Args;
    for (auto Arg : VirtAI.getArguments()) {
      Args.push_back(Arg);
    }
    FullApplySite NewVirtAI = Builder.createTryApply(VirtAI.getLoc(), VirtAI.getCallee(),
        VirtAI.getSubstCalleeSILType(), VirtAI.getSubstitutions(),
        Args, NormalBB, ErrorBB);
    VirtAI.getInstruction()->eraseFromParent();
    VirtAI = NewVirtAI;
  }

  return VirtAI;
}

/// \brief Returns true, if a method implementation to be called by the
/// default case handler of a speculative devirtualization is statically
/// known. This happens if it can be proven that generated
/// checked_cast_br instructions cover all other possible cases.
///
/// \p CHA class hierarchy analysis to be used
/// \p AI  invocation instruction
/// \p CD  static class of the instance whose method is being invoked
/// \p Subs set of direct subclasses of this class
static bool isDefaultCaseKnown(ClassHierarchyAnalysis *CHA,
                               FullApplySite AI,
                               ClassDecl *CD,
                               ClassHierarchyAnalysis::ClassList &Subs) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI.getCallee());
  auto *Method = CMI->getMember().getFuncDecl();
  const DeclContext *DC = AI.getModule().getAssociatedContext();

  if (CD->isFinal())
    return true;

  // If the class has an @objc ancestry it can be dynamically subclassed and we
  // can't therefore statically know the default case.
  auto Ancestry = CD->checkObjCAncestry();
  if (Ancestry != ObjCClassKind::NonObjC)
    return false;

  // Without an associated context we cannot perform any
  // access-based optimizations.
  if (!DC)
    return false;

  // Only handle classes defined within the SILModule's associated context.
  if (!CD->isChildContextOf(DC))
    return false;

  if (!CD->hasAccessibility())
    return false;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (CD->getEffectiveAccess()) {
  case Accessibility::Open:
    return false;
  case Accessibility::Public:
  case Accessibility::Internal:
    if (!AI.getModule().isWholeModule())
      return false;
    break;
  case Accessibility::FilePrivate:
  case Accessibility::Private:
    break;
  }

  // This is a private or a module internal class.
  //
  // We can analyze the class hierarchy rooted at it and
  // eventually devirtualize a method call more efficiently.

  // First, analyze all direct subclasses.
  // We know that a dedicated checked_cast_br check is
  // generated for each direct subclass by tryToSpeculateTarget.
  for (auto S : Subs) {
    // Check if the subclass overrides a method
    auto *FD = S->findOverridingDecl(Method);
    if (!FD)
      continue;
    if (CHA->hasKnownDirectSubclasses(S)) {
      // This subclass has its own subclasses and
      // they will use this implementation or provide
      // their own. In either case it is not covered by
      // checked_cast_br instructions generated by
      // tryToSpeculateTarget. Therefore it increases
      // the number of remaining cases to be handled
      // by the default case handler.
      return false;
    }
  }

  // Then, analyze indirect subclasses.

  // Set of indirect subclasses for the class.
  auto &IndirectSubs = CHA->getIndirectSubClasses(CD);

  // Check if any indirect subclasses use an implementation
  // of the method different from the implementation in
  // the current class. If this is the case, then such
  // an indirect subclass would need a dedicated
  // checked_cast_br check to be devirtualized. But this is
  // not done by tryToSpeculateTarget yet and therefore
  // such a subclass should be handled by the "default"
  // case handler, which essentially means that "default"
  // case cannot be devirtualized since it covers more
  // then one alternative.
  for (auto S : IndirectSubs) {
    auto *ImplFD = S->findImplementingMethod(Method);
    if (ImplFD != Method) {
      // Different implementation is used by a subclass.
      // Therefore, the default case is not known.
      return false;
    }
  }

  return true;
}

/// \brief Try to speculate the call target for the call \p AI. This function
/// returns true if a change was made.
static bool tryToSpeculateTarget(FullApplySite AI,
                                 ClassHierarchyAnalysis *CHA) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI.getCallee());

  // We cannot devirtualize in cases where dynamic calls are
  // semantically required.
  if (CMI->isVolatile())
    return false;

  // Strip any upcasts off of our 'self' value, potentially leaving us
  // with a value whose type is closer (in the class hierarchy) to the
  // actual dynamic type.
  auto SubTypeValue = stripUpCasts(CMI->getOperand());
  SILType SubType = SubTypeValue->getType();

  // Bail if any generic types parameters of the class instance type are
  // unbound.
  // We cannot devirtualize unbound generic calls yet.
  if (SubType.getSwiftRValueType()->hasArchetype())
    return false;

  auto &M = CMI->getModule();
  auto ClassType = SubType;
  if (SubType.is<MetatypeType>())
    ClassType = SubType.getMetatypeInstanceType(M);

  CheckedCastBranchInst *LastCCBI = nullptr;

  ClassDecl *CD = ClassType.getClassOrBoundGenericClass();
  assert(CD && "Expected decl for class type!");

  if (!CHA->hasKnownDirectSubclasses(CD)) {
    // If there is only one possible alternative for this method,
    // try to devirtualize it completely.
    ClassHierarchyAnalysis::ClassList Subs;
    if (isDefaultCaseKnown(CHA, AI, CD, Subs)) {
      auto NewInstPair = tryDevirtualizeClassMethod(AI, SubTypeValue);
      if (NewInstPair.first)
        replaceDeadApply(AI, NewInstPair.first);
      return NewInstPair.second.getInstruction() != nullptr;
    }

    DEBUG(llvm::dbgs() << "Inserting monomorphic speculative call for class " <<
          CD->getName() << "\n");
    return !!speculateMonomorphicTarget(AI, SubType, LastCCBI);
  }

  // True if any instructions were changed or generated.
  bool Changed = false;

  // Collect the direct and indirect subclasses for the class.
  // Sort these subclasses in the order they should be tested by the
  // speculative devirtualization. Different strategies could be used,
  // E.g. breadth-first, depth-first, etc.
  // Currently, let's use the breadth-first strategy.
  // The exact static type of the instance should be tested first.
  auto &DirectSubs = CHA->getDirectSubClasses(CD);
  auto &IndirectSubs = CHA->getIndirectSubClasses(CD);

  SmallVector<ClassDecl *, 8> Subs(DirectSubs);
  Subs.append(IndirectSubs.begin(), IndirectSubs.end());

  if (isa<BoundGenericClassType>(ClassType.getSwiftRValueType())) {
    // Filter out any subclasses that do not inherit from this
    // specific bound class.
    auto RemovedIt = std::remove_if(Subs.begin(),
        Subs.end(),
        [&ClassType, &M](ClassDecl *Sub){
          auto SubCanTy = Sub->getDeclaredType()->getCanonicalType();
          // Unbound generic type can override a method from
          // a bound generic class, but this unbound generic
          // class is not considered to be a subclass of a
          // bound generic class in a general case.
          if (isa<UnboundGenericType>(SubCanTy))
            return false;
          // Handle the usual case here: the class in question
          // should be a real subclass of a bound generic class.
          return !ClassType.isBindableToSuperclassOf(
              SILType::getPrimitiveObjectType(SubCanTy));
        });
    Subs.erase(RemovedIt, Subs.end());
  }

  // Number of subclasses which cannot be handled by checked_cast_br checks.
  int NotHandledSubsNum = 0;
  if (Subs.size() > MaxNumSpeculativeTargets) {
    DEBUG(llvm::dbgs() << "Class " << CD->getName() << " has too many ("
                       << Subs.size() << ") subclasses. Performing speculative "
                         "devirtualization only for the first "
                       << MaxNumSpeculativeTargets << " of them.\n");

    NotHandledSubsNum += (Subs.size() - MaxNumSpeculativeTargets);
    Subs.erase(&Subs[MaxNumSpeculativeTargets], Subs.end());
  }

  DEBUG(llvm::dbgs() << "Class " << CD->getName() << " is a superclass. "
        "Inserting polymorphic speculative call.\n");

  // Try to devirtualize the static class of instance
  // if it is possible.
  if (auto F = getTargetClassMethod(M, SubType, CMI)) {
    // Do not devirtualize if a method in the base class is marked
    // as non-optimizable. This way it is easy to disable the
    // devirtualization of this method in the base class and
    // any classes derived from it.
    if (!F->shouldOptimize())
      return false;
  }

  auto FirstAI = speculateMonomorphicTarget(AI, SubType, LastCCBI);
  if (FirstAI) {
    Changed = true;
    AI = FirstAI;
  }

  // Perform a speculative devirtualization of a method invocation.
  // It replaces an indirect class_method-based call by a code to perform
  // a direct call of the method implementation based on the dynamic class
  // of the instance.
  //
  // The code is generated according to the following principles:
  //
  // - For each direct subclass, a dedicated checked_cast_br instruction
  // is generated to check if a dynamic class of the instance is exactly
  // this subclass.
  //
  // - If this check succeeds, then it jumps to the code which performs a
  // direct call of a method implementation specific to this subclass.
  //
  // - If this check fails, then a different subclass is checked by means of
  // checked_cast_br in a similar way.
  //
  // - Finally, if the instance does not exactly match any of the direct
  // subclasses, the "default" case code is generated, which should handle
  // all remaining alternatives, i.e. it should be able to dispatch to any
  // possible remaining method implementations. Typically this is achieved by
  // using a class_method instruction, which performs an indirect invocation.
  // But if it can be proven that only one specific implementation of
  // a method will be always invoked by this code, then a class_method-based
  // call can be devirtualized and replaced by a more efficient direct
  // invocation of this specific method implementation.
  //
  // Remark: With the current implementation of a speculative devirtualization,
  // if devirtualization of the "default" case is possible, then it would
  // by construction directly invoke the implementation of the method
  // corresponding to the static type of the instance. This may change
  // in the future, if we start using PGO for ordering of checked_cast_br
  // checks.

  // TODO: The ordering of checks may benefit from using a PGO, because
  // the most probable alternatives could be checked first.

  for (auto S : Subs) {
    DEBUG(llvm::dbgs() << "Inserting a speculative call for class "
          << CD->getName() << " and subclass " << S->getName() << "\n");

    CanType CanClassType = S->getDeclaredType()->getCanonicalType();
    SILType ClassType = SILType::getPrimitiveObjectType(CanClassType);
    if (!ClassType.getClassOrBoundGenericClass()) {
      // This subclass cannot be handled. This happens e.g. if it is
      // a generic class.
      NotHandledSubsNum++;
      continue;
    }

    auto ClassOrMetatypeType = ClassType;
    if (auto EMT = SubType.getAs<AnyMetatypeType>()) {
      auto InstTy = ClassType.getSwiftRValueType();
      auto *MetaTy = MetatypeType::get(InstTy, EMT->getRepresentation());
      auto CanMetaTy = CanMetatypeType::CanTypeWrapper(MetaTy);
      ClassOrMetatypeType = SILType::getPrimitiveObjectType(CanMetaTy);
    }

    // Pass the metatype of the subclass.
    auto NewAI = speculateMonomorphicTarget(AI, ClassOrMetatypeType, LastCCBI);
    if (!NewAI) {
      NotHandledSubsNum++;
      continue;
    }
    AI = NewAI;
    Changed = true;
  }

  // Check if there is only a single statically known implementation
  // of the method which can be called by the default case handler.
  if (NotHandledSubsNum || !isDefaultCaseKnown(CHA, AI, CD, Subs)) {
    // Devirtualization of remaining cases is not possible,
    // because more than one implementation of the method
    // needs to be handled here. Thus, an indirect call through
    // the class_method cannot be eliminated completely.
    //
    return Changed;
  }

  // At this point it is known that there is only one remaining method
  // implementation which is not covered by checked_cast_br checks yet.
  // So, it is safe to replace a class_method invocation by
  // a direct call of this remaining implementation.
  if (LastCCBI && SubTypeValue == LastCCBI->getOperand()) {
    // Remove last checked_cast_br, because it will always succeed.
    SILBuilderWithScope B(LastCCBI);
    auto CastedValue = B.createUncheckedBitCast(LastCCBI->getLoc(),
                                                LastCCBI->getOperand(),
                                                LastCCBI->getCastType());
    B.createBranch(LastCCBI->getLoc(), LastCCBI->getSuccessBB(), {CastedValue});
    LastCCBI->eraseFromParent();
    return true;
  }
  auto NewInstPair = tryDevirtualizeClassMethod(AI, SubTypeValue);
  if (NewInstPair.first) {
    replaceDeadApply(AI, NewInstPair.first);
    return true;
  }
  return Changed;
}

namespace {
  /// Speculate the targets of virtual calls by assuming that the requested
  /// class is at the bottom of the class hierarchy.
  class SpeculativeDevirtualization : public SILFunctionTransform {
  public:
    ~SpeculativeDevirtualization() override {}

    void run() override {
      ClassHierarchyAnalysis *CHA = PM->getAnalysis<ClassHierarchyAnalysis>();

      bool Changed = false;

      // Collect virtual calls that may be specialized.
      SmallVector<FullApplySite, 16> ToSpecialize;
      for (auto &BB : *getFunction()) {
        for (auto II = BB.begin(), IE = BB.end(); II != IE; ++II) {
          FullApplySite AI = FullApplySite::isa(&*II);
          if (AI && isa<ClassMethodInst>(AI.getCallee()))
            ToSpecialize.push_back(AI);
        }
      }

      // Go over the collected calls and try to insert speculative calls.
      for (auto AI : ToSpecialize)
        Changed |= tryToSpeculateTarget(AI, CHA);

      if (Changed) {
        invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
      }
    }

    StringRef getName() override { return "Speculative Devirtualization"; }
  };

} // end anonymous namespace

SILTransform *swift::createSpeculativeDevirtualization() {
  return new SpeculativeDevirtualization();
}
