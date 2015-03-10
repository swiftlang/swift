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
    GenericSpecializer::AIList DevirtualizedCalls;

    bool Changed = false;

    // Perform devirtualization locally and compute potential polymorphic
    // arguments for all existing functions.
    for (auto &F : *getModule()) {
      DEBUG(llvm::dbgs() << "*** Devirtualizing Function: "
              << demangle_wrappers::demangleSymbolAsString(F.getName())
              << "\n");
      for (auto &BB : F) {
        for (auto II = BB.begin(), IE = BB.end(); II != IE;) {
          ApplyInst *AI = dyn_cast<ApplyInst>(&*II);
          ++II;

          if (!AI)
            continue;

          if (ApplyInst *NewAI = devirtualizeApply(AI)) {
            DevirtualizedCalls.push_back(NewAI);
            Changed |= true;
          }
        }
      }
      DEBUG(llvm::dbgs() << "\n");
    }

    if (Changed) {
      // Try to specialize the devirtualized calls.
      auto GS = GenericSpecializer(getModule());

      // Try to specialize the newly devirtualized calls.
      if (GS.specialize(DevirtualizedCalls)) {
        DEBUG(llvm::dbgs() << "Specialized some generic functions\n");
      }

      PM->scheduleAnotherIteration();
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
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
                                 Ret, AI->isTransparent());
  NAI->setDebugScope(AI->getDebugScope());
  return NAI;
}

/// Insert monomorphic inline caches for a specific class type \p SubClassTy.
static ApplyInst* insertMonomorphicInlineCaches(ApplyInst *AI,
                                                SILType SubClassTy) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI->getCallee());
  SILValue ClassInstance = CMI->getOperand();
  ClassDecl *CD = SubClassTy.getClassOrBoundGenericClass();

  SILType RealSubClassTy = SubClassTy;

  if (auto *VMTI = dyn_cast<ValueMetatypeInst>(ClassInstance.stripUpCasts())) {
    if (isa<AnyMetatypeType>(SubClassTy.getSwiftRValueType())) {
      CD = SubClassTy.getMetatypeInstanceType(AI->getModule())
               .getClassOrBoundGenericClass();
    } else {
      auto InstTy = SubClassTy.getSwiftRValueType();
      CD = InstTy.getClassOrBoundGenericClass();
      // Convert instance type to its metatype type.
      auto EMT = dyn_cast<AnyMetatypeType>(VMTI->getType().
                                                 getSwiftRValueType());
      auto *MetaTy = MetatypeType::get(InstTy, EMT->getRepresentation());
      auto CanMetaTy = CanMetatypeType::CanTypeWrapper(MetaTy);
      RealSubClassTy = SILType::getPrimitiveObjectType(CanMetaTy);
    }
  } else {
    assert(SubClassTy.getClassOrBoundGenericClass() &&
           "Dest type must be a class type");
  }

  // Placeholder for keeping the results of analysis performed
  // by canDevirtualizeClassMethod.
  DevirtClassMethodInfo DCMI;

  // Bail if this class_method cannot be devirtualized.
  if (!canDevirtualizeClassMethod(AI, RealSubClassTy, CD, DCMI))
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
  Iden->createBBArg(RealSubClassTy);

  SILBasicBlock *Continue = Entry->splitBasicBlock(It);

  SILBuilderWithScope<> Builder(Entry, AI->getDebugScope());
  // Create the checked_cast_branch instruction that checks at runtime if the
  // class instance is identical to the SILType.

  It = Builder.createCheckedCastBranch(AI->getLoc(), /*exact*/ true,
                                       ClassInstance, RealSubClassTy, Iden,
                                       Virt);

  SILBuilder VirtBuilder(Virt);
  SILBuilder IdenBuilder(Iden);
  // This is the class reference downcasted into subclass SubClassTy.
  SILValue DownCastedClassInstance = Iden->getBBArg(0);

  // Try sinking the retain of the class instance into the diamond. This may
  // allow additional ARC optimizations on the fast path.
  if (It != Entry->begin()) {
    StrongRetainInst *SRI = dyn_cast<StrongRetainInst>(--It);
    // Try to skip another instruction, in case the class_method came first.
    if (!SRI && It != Entry->begin())
      SRI = dyn_cast<StrongRetainInst>(--It);
    if (SRI && SRI->getOperand() == ClassInstance) {
      VirtBuilder.createStrongRetain(SRI->getLoc(), ClassInstance)
        ->setDebugScope(SRI->getDebugScope());
      IdenBuilder.createStrongRetain(SRI->getLoc(), DownCastedClassInstance)
        ->setDebugScope(SRI->getDebugScope());
      SRI->eraseFromParent();
    }
  }

  // Copy the two apply instructions into the two blocks.
  ApplyInst *IdenAI = CloneApply(AI, IdenBuilder);
  ApplyInst *VirtAI = CloneApply(AI, VirtBuilder);

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
  ApplyInst *NewAI = devirtualizeClassMethod(IdenAI, DownCastedClassInstance,
                                             DCMI);
  assert(NewAI && "Expected to be able to devirtualize apply!");
  (void) NewAI;

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
  switch (CD->getAccessibility()) {
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

  SILValue ClassInstance = CMI->getOperand();
  // The static type used by the class_method instruction
  // is the class which a given method belongs to.
  // Is either the class of the instance itself or one of its superclasses.
  // Therefore, strip all upcasts to get the real static type
  // of the instance.
  // Specifically, only the upcast to the static class which method belongs to
  // should be stripped.
  SILType InstanceType = ClassInstance.stripUpCasts().getType();
  ClassDecl *CD = InstanceType.getClassOrBoundGenericClass();

  if (auto *VMTI = dyn_cast<ValueMetatypeInst>(ClassInstance.stripUpCasts())) {
    CanType InstTy = VMTI->getType().castTo<MetatypeType>().getInstanceType();
    CD = InstTy.getClassOrBoundGenericClass();
  }

  // Check if it is legal to insert inline caches.
  if (!CD)
    return false;

  if (ClassInstance.getType() != InstanceType) {
    // The implementation of a method to be invoked may actually
    // be defined by one of the superclasses.
    if (ClassInstance.getType().getAs<MetatypeType>()) {
      auto &Module = AI->getModule();
      if (!ClassInstance.getType().getMetatypeInstanceType(Module).
             isSuperclassOf(InstanceType.getMetatypeInstanceType(Module)))
        return false;
    } else {
      if (!ClassInstance.getType().isSuperclassOf(InstanceType))
        return false;
    }
    // ClassInstance and InstanceType should match for
    // devirtualizeClassMethod to work.
    ClassInstance = ClassInstance.stripUpCasts();
  }

  // Bail if any generic types parameters of the class instance type are
  // unbound.
  // We cannot devirtualize unbound generic calls yet.
  if (isClassWithUnboundGenericParameters(InstanceType, AI->getModule()))
    return false;

  if (!CHA->hasKnownDirectSubclasses(CD)) {
    DEBUG(llvm::dbgs() << "Inserting monomorphic inline caches for class " <<
          CD->getName() << "\n");
    return insertMonomorphicInlineCaches(AI, InstanceType);
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
    SILType InstanceType = SILType::getPrimitiveObjectType(CanClassType);
    if (!InstanceType.getClassOrBoundGenericClass()) {
      // This subclass cannot be handled. This happens e.g. if it is
      // a generic class.
      NotHandledSubsNum++;
      continue;
    }

    AI = insertMonomorphicInlineCaches(AI, InstanceType);
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
    return insertMonomorphicInlineCaches(AI, InstanceType);
  }

  // At this point it is known that there is only one remaining method
  // implementation which is not covered by checked_cast_br checks yet.
  // So, it is safe to replace a class_method invocation by
  // a direct call of this remaining implementation.
  ApplyInst *NewAI = devirtualizeClassMethod(AI, ClassInstance, CD);
  assert(NewAI && "Expected to be able to devirtualize apply!");
  (void) NewAI;

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
          ApplyInst *AI = dyn_cast<ApplyInst>(&*II);
          if (AI && isa<ClassMethodInst>(AI->getCallee()))
            ToSpecialize.push_back(AI);
        }
      }

      // Create the inline caches.
      for (auto AI : ToSpecialize)
        Changed |= insertInlineCaches(AI, CHA);

      if (Changed) {
        invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
      }
    }

    StringRef getName() override { return "Inline Caches"; }
  };

} // end anonymous namespace

SILTransform *swift::createInlineCaches() {
  return new SILInlineCaches();
}

