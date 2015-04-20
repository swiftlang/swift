//===-- Devirtualizer.cpp ------ Devirtualize virtual calls ---------------===//
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
//
// Devirtualizes virtual function calls into direct function calls.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-devirtualizer-pass"
#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILAnalysis/ClassHierarchyAnalysis.h"
#include "swift/SILPasses/Utils/Generics.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Devirtualize.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "swift/AST/ASTContext.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
using namespace swift;

// The number of subclasses to allow when placing polymorphic inline caches.
static const int MaxNumPolymorphicInlineCaches = 6;

STATISTIC(NumInlineCaches, "Number of monomorphic inline caches inserted");

namespace {

class SILDevirtualizationPass : public SILModuleTransform {
public:
  virtual ~SILDevirtualizationPass() {}

  /// The entry point to the transformation.
  void run() override {

    /// A list of devirtualized calls.
    llvm::SmallVector<SILInstruction *, 16> DevirtualizedCalls;

    bool Changed = false;

    // Perform devirtualization locally and compute potential polymorphic
    // arguments for all existing functions.
    for (auto &F : *getModule()) {

      // Don't optimize functions that are marked with the opt.never attribute.
      if (!F.shouldOptimize())
        return;

      DEBUG(llvm::dbgs() << "*** Devirtualizing Function: "
              << demangle_wrappers::demangleSymbolAsString(F.getName())
              << "\n");
      for (auto &BB : F) {
        for (auto II = BB.begin(), IE = BB.end(); II != IE;) {
          ApplyInst *AI = dyn_cast<ApplyInst>(&*II);
          ++II;

          if (!AI)
            continue;

          if (auto *NewInst = tryDevirtualizeApply(AI)) {
            replaceDeadApply(AI, NewInst);

            DevirtualizedCalls.push_back(NewInst);
            Changed |= true;
          }
        }
      }
      DEBUG(llvm::dbgs() << "\n");
    }

    // Invalidate the analysis of caller functions.
    for (auto AI : DevirtualizedCalls) {
      invalidateAnalysis(AI->getFunction(),
                         SILAnalysis::PreserveKind::Branches);
    }

    if (Changed) {
      PM->scheduleAnotherIteration();
    }
  }

  StringRef getName() override { return "Devirtualization"; }
};

} // end anonymous namespace

SILTransform *swift::createDevirtualizer() {
  return new SILDevirtualizationPass();
}

// A utility function for cloning the apply instruction.
static ApplyInst *CloneApply(ApplyInst *AI, SILBuilder &Builder) {
  // Clone the Apply.
  auto Args = AI->getArguments();
  SmallVector<SILValue, 8> Ret(Args.size());
  for (unsigned i = 0, e = Args.size(); i != e; ++i)
    Ret[i] = Args[i];

  auto NAI = Builder.createApply(AI->getLoc(), AI->getCallee(),
                                 AI->getSubstCalleeSILType(),
                                 AI->getType(),
                                 AI->getSubstitutions(),
                                 Ret);
  NAI->setDebugScope(AI->getDebugScope());
  return NAI;
}

/// Insert monomorphic inline caches for a specific class or metatype
/// type \p SubClassTy.
static ApplyInst* insertMonomorphicInlineCaches(ApplyInst *AI,
                                                SILType SubType) {
  // Bail if this class_method cannot be devirtualized.
  if (!canDevirtualizeClassMethod(AI, SubType))
    return nullptr;

  // Create a diamond shaped control flow and a checked_cast_branch
  // instruction that checks the exact type of the object.
  // This cast selects between two paths: one that calls the slow dynamic
  // dispatch and one that calls the specific method.
  SILBasicBlock::iterator It = AI;
  SILFunction *F = AI->getFunction();
  SILBasicBlock *Entry = AI->getParent();

  // Iden is the basic block containing the direct call.
  SILBasicBlock *Iden = F->createBasicBlock();
  // Virt is the block containing the slow virtual call.
  SILBasicBlock *Virt = F->createBasicBlock();
  Iden->createBBArg(SubType);

  SILBasicBlock *Continue = Entry->splitBasicBlock(It);

  SILBuilderWithScope<> Builder(Entry, AI->getDebugScope());
  // Create the checked_cast_branch instruction that checks at runtime if the
  // class instance is identical to the SILType.

  ClassMethodInst *CMI = cast<ClassMethodInst>(AI->getCallee());

  It = Builder.createCheckedCastBranch(AI->getLoc(), /*exact*/ true,
                                       CMI->getOperand(), SubType, Iden,
                                       Virt);

  SILBuilder VirtBuilder(Virt);
  SILBuilder IdenBuilder(Iden);
  // This is the class reference downcasted into subclass SubType.
  SILValue DownCastedClassInstance = Iden->getBBArg(0);

  // Try sinking the retain of the class instance into the diamond. This may
  // allow additional ARC optimizations on the fast path.
  if (It != Entry->begin()) {
    auto *SRI = dyn_cast<StrongRetainInst>(--It);
    // Try to skip another instruction, in case the class_method came first.
    if (!SRI && It != Entry->begin())
      SRI = dyn_cast<StrongRetainInst>(--It);
    if (SRI && SRI->getOperand() == CMI->getOperand()) {
      VirtBuilder.createStrongRetain(SRI->getLoc(), CMI->getOperand())
        ->setDebugScope(SRI->getDebugScope());
      IdenBuilder.createStrongRetain(SRI->getLoc(), DownCastedClassInstance)
        ->setDebugScope(SRI->getDebugScope());
      SRI->eraseFromParent();
    }
  }

  // Copy the two apply instructions into the two blocks.
  ApplyInst *IdenAI = CloneApply(AI, IdenBuilder);
  ApplyInst *VirtAI = CloneApply(AI, VirtBuilder);

  // See if Continue has a release on self as the instruction right after the
  // apply. If it exists, move it into position in the diamond.
  if (auto *Release =
          dyn_cast<StrongReleaseInst>(std::next(Continue->begin()))) {
    if (Release->getOperand() == CMI->getOperand()) {
      VirtBuilder.createStrongRelease(Release->getLoc(), CMI->getOperand())
          ->setDebugScope(Release->getDebugScope());
      IdenBuilder.createStrongRelease(Release->getLoc(),
                                      DownCastedClassInstance)
          ->setDebugScope(Release->getDebugScope());
      Release->eraseFromParent();
    }
  }

  // Create a PHInode for returning the return value from both apply
  // instructions.
  SILArgument *Arg = Continue->createBBArg(AI->getType());
  IdenBuilder.createBranch(AI->getLoc(), Continue, ArrayRef<SILValue>(IdenAI))
    ->setDebugScope(AI->getDebugScope());
  VirtBuilder.createBranch(AI->getLoc(), Continue, ArrayRef<SILValue>(VirtAI))
    ->setDebugScope(AI->getDebugScope());

  // Remove the old Apply instruction.
  AI->replaceAllUsesWith(Arg);
  AI->eraseFromParent();

  // Update the stats.
  NumInlineCaches++;

  // Devirtualize the apply instruction on the identical path.
  auto *NewInst = devirtualizeClassMethod(IdenAI, DownCastedClassInstance);
  assert(NewInst && "Expected to be able to devirtualize apply!");
  replaceDeadApply(IdenAI, NewInst);

  // Sink class_method instructions down to their single user.
  if (CMI->hasOneUse())
    CMI->moveBefore(CMI->use_begin()->getUser());

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
                               ApplyInst *AI,
                               ClassDecl *CD,
                               ClassHierarchyAnalysis::ClassList &Subs) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI->getCallee());
  auto *Method = CMI->getMember().getFuncDecl();
  const DeclContext *DC = AI->getModule().getAssociatedContext();

  if (CD->isFinal())
    return true;

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
  case Accessibility::Public:
    return false;
  case Accessibility::Internal:
    if (!AI->getModule().isWholeModule())
      return false;
    break;
  case Accessibility::Private:
    break;
  }

  // This is a private or a module internal class.
  //
  // We can analyze the class hierarchy rooted at it and
  // eventually devirtualize a method call more efficiently.

  // First, analyze all direct subclasses.
  // We know that a dedicated checked_cast_br check is
  // generated for each direct subclass by insertInlineCaches.
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
      // insertInlineCaches. Therefore it increases
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
  // not done by insertInlineCaches yet and therefore
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

/// \brief Try to insert inline cahces for the call \p AI. This function
/// returns true if a change was made.
static bool insertInlineCaches(ApplyInst *AI, ClassHierarchyAnalysis *CHA) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI->getCallee());

  // We cannot devirtualize in cases where dynamic calls are
  // semantically required.
  if (CMI->isVolatile())
    return false;

  // Strip any upcasts off of our 'self' value, potentially leaving us
  // with a value whose type is closer (in the class hierarchy) to the
  // actual dynamic type.
  auto SubTypeValue = CMI->getOperand().stripUpCasts();
  SILType SubType = SubTypeValue.getType();

  // Bail if any generic types parameters of the class instance type are
  // unbound.
  // We cannot devirtualize unbound generic calls yet.
  if (isClassWithUnboundGenericParameters(SubType, AI->getModule()))
    return false;

  auto &M = CMI->getModule();
  auto ClassType = SubType;
  if (SubType.is<MetatypeType>())
    ClassType = SubType.getMetatypeInstanceType(M);

  ClassDecl *CD = ClassType.getClassOrBoundGenericClass();
  assert(CD && "Expected decl for class type!");

  if (!CHA->hasKnownDirectSubclasses(CD)) {
    // If there is only one possible alternative for this method,
    // try to devirtualize it completely.
    ClassHierarchyAnalysis::ClassList Subs;
    if (isDefaultCaseKnown(CHA, AI, CD, Subs)) {
      auto *NewInst = tryDevirtualizeClassMethod(AI, SubTypeValue);
      if (NewInst)
        replaceDeadApply(AI, NewInst);
      return NewInst;
    }

    DEBUG(llvm::dbgs() << "Inserting monomorphic inline caches for class " <<
          CD->getName() << "\n");
    return insertMonomorphicInlineCaches(AI, SubType);
  }

  // Collect the direct subclasses for the class.
  auto &Subs = CHA->getDirectSubClasses(CD);

  if (Subs.size() > MaxNumPolymorphicInlineCaches) {
    DEBUG(llvm::dbgs() << "Class " << CD->getName() << " has too many (" <<
          Subs.size() << ") subclasses. Not inserting inline caches.\n");
    return false;
  }

  DEBUG(llvm::dbgs() << "Class " << CD->getName() << " is a superclass. "
        "Inserting polymorphic inline caches.\n");

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

  // Number of subclasses which cannot be handled by checked_cast_br checks.
  int NotHandledSubsNum = 0;
  // True if any instructions were changed or generated.
  bool Changed = false;

  for (auto S : Subs) {
    DEBUG(llvm::dbgs() << "Inserting a cache for class " << CD->getName() <<
          " and subclass " << S->getName() << "\n");

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
    AI = insertMonomorphicInlineCaches(AI, ClassOrMetatypeType);
    if (!AI) {
      NotHandledSubsNum++;
      continue;
    }
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
    // But we can still try to devirtualize the static class of instance
    // if it is possible.
    return insertMonomorphicInlineCaches(AI, SubType);
  }

  // At this point it is known that there is only one remaining method
  // implementation which is not covered by checked_cast_br checks yet.
  // So, it is safe to replace a class_method invocation by
  // a direct call of this remaining implementation.
  auto *NewInst = tryDevirtualizeClassMethod(AI, SubTypeValue);
  assert(NewInst && "Expected to be able to devirtualize apply!");
  replaceDeadApply(AI, NewInst);

  return true;
}

namespace {
  /// Generate inline caches of virtual calls by speculating that the requested
  /// class is at the bottom of the class hierarchy.
  class SILInlineCaches : public SILFunctionTransform {
  public:
    virtual ~SILInlineCaches() {}

    void run() override {
      ClassHierarchyAnalysis *CHA = PM->getAnalysis<ClassHierarchyAnalysis>();

      bool Changed = false;

      // Collect virtual calls that may be specialized.
      SmallVector<ApplyInst *, 16> ToSpecialize;
      for (auto &BB : *getFunction()) {
        for (auto II = BB.begin(), IE = BB.end(); II != IE; ++II) {
          auto *AI = dyn_cast<ApplyInst>(&*II);
          if (AI && isa<ClassMethodInst>(AI->getCallee()))
            ToSpecialize.push_back(AI);
        }
      }

      // Create the inline caches.
      for (auto AI : ToSpecialize)
        Changed |= insertInlineCaches(AI, CHA);

      if (Changed) {
        invalidateAnalysis(SILAnalysis::PreserveKind::Nothing);
      }
    }

    StringRef getName() override { return "Inline Caches"; }
  };

} // end anonymous namespace

SILTransform *swift::createInlineCaches() {
  return new SILInlineCaches();
}

