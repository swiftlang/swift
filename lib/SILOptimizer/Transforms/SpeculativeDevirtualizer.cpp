//===--- SpeculativeDevirtualizer.cpp - Speculatively devirtualize calls --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Speculatively devirtualizes witness- and class-method calls into direct
// calls.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-speculative-devirtualizer"
#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/Fallthrough.h"
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
#include "swift/SIL/DebugUtils.h"

using namespace swift;

static const char *Functions[] = {
};

static int startIdx = 78;
// Start: minIdx = 0: max index: 201
// static int endIdx = sizeof(Functions)/sizeof(Functions[0]);
static int endIdx = 79;

static bool shouldProcessFunction(const char *Functions[], StringRef FuncName,
                                  int startIdx, int endIdx) {
  return true;
  for (auto i = startIdx, e = endIdx; i < e; ++i) {
    if (FuncName == Functions[i]) {
      llvm::dbgs() << "Should process function at index " << i
                   << ": minIdx = " << startIdx << ": max index: " << endIdx
                   << ":" << FuncName << "\n";
      return true;
    }
  }
  return false;
}

llvm::cl::opt<bool> SpecDevirtNonClassProtocols(
    "specdevirt-non-class-protocols", llvm::cl::init(true),
    llvm::cl::desc(
        "Enable speculative devirtualization of non-class protocols"));

// This is the limit for the number of subclasses (jump targets) that the
// speculative devirtualizer will try to predict.
static const int MaxNumSpeculativeTargets = 6;

STATISTIC(NumTargetsPredicted, "Number of monomorphic functions predicted");

// A utility function for cloning the apply instruction.
static FullApplySite CloneApply(FullApplySite AI, SILBuilder &Builder) {
  // Clone the Apply.
  Builder.setCurrentDebugScope(AI.getDebugScope());
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
  Iden->createBBArg(SubType);

  SILBasicBlock *Continue = Entry->splitBasicBlock(It);

  SILBuilderWithScope Builder(Entry, AI.getInstruction());
  // Create the checked_cast_branch instruction that checks at runtime if the
  // class instance is identical to the SILType.

  //ClassMethodInst *CMI = cast<ClassMethodInst>(AI.getCallee());
  MethodInst *CMI = cast<MethodInst>(AI.getCallee());
  auto Self = CMI->getOperand(0);

  CCBI = Builder.createCheckedCastBranch(AI.getLoc(), /*exact*/ true,
                                       Self, SubType, Iden,
                                       Virt);
  It = CCBI->getIterator();

  SILBuilderWithScope VirtBuilder(Virt, AI.getInstruction());
  SILBuilderWithScope IdenBuilder(Iden, AI.getInstruction());
  // This is the class reference downcasted into subclass SubType.
  SILValue DownCastedClassInstance = Iden->getBBArg(0);

  // Copy the two apply instructions into the two blocks.
  FullApplySite IdenAI = CloneApply(AI, IdenBuilder);
  FullApplySite VirtAI = CloneApply(AI, VirtBuilder);

  // See if Continue has a release on self as the instruction right after the
  // apply. If it exists, move it into position in the diamond.
  if (auto *Release =
          dyn_cast<StrongReleaseInst>(std::next(Continue->begin()))) {
    if (Release->getOperand() == Self) {
      VirtBuilder.createStrongRelease(Release->getLoc(), Self,
                                      Atomicity::Atomic);
      IdenBuilder.createStrongRelease(
          Release->getLoc(), DownCastedClassInstance, Atomicity::Atomic);
      Release->eraseFromParent();
    }
  }

  // Create a PHInode for returning the return value from both apply
  // instructions.
  SILArgument *Arg = Continue->createBBArg(AI.getType());
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
    ErrorBB->createBBArg(TAI->getErrorBB()->getBBArg(0)->getType());
    Builder.setInsertionPoint(ErrorBB);
    Builder.createBranch(TAI->getLoc(), TAI->getErrorBB(),
                         {ErrorBB->getBBArg(0)});

    auto *NormalBB = TAI->getFunction()->createBasicBlock();
    NormalBB->createBBArg(TAI->getNormalBB()->getBBArg(0)->getType());
    Builder.setInsertionPoint(NormalBB);
    Builder.createBranch(TAI->getLoc(), TAI->getNormalBB(),
                        {NormalBB->getBBArg(0) });

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

/// Returns the thick metatype for the given SILType.
/// e.g. $*T -> $@thick T.Type
static SILType getThickMetatypeType(CanType Ty) {
  // If it is a metatype already, simply return it.
  if (isa<MetatypeType>(Ty))
    return SILType::getPrimitiveObjectType(Ty);

  auto SwiftTy = CanMetatypeType::get(Ty, MetatypeRepresentation::Thick);
  return SILType::getPrimitiveObjectType(SwiftTy);
}

// If possible, replace a sequence of instructions for obtaining
// the dynamic type of the existential by a sequence of instructions
// for obtaining a static type of the existential.
static SILValue replaceDynamicTypeByStaticType(FullApplySite AI,
                                               SILValue Self) {
  auto &Module = AI.getModule();
  WitnessMethodInst *CMI = cast<WitnessMethodInst>(AI.getCallee());

  // Check for a special pattern
  // %emt = open_existential_metatype ( existential_metatype ...)
  // apply %f(%emt)
  if (auto *OEMI = dyn_cast<OpenExistentialMetatypeInst>(Self)) {
    if (auto *EMI = dyn_cast<ExistentialMetatypeInst>(OEMI->getOperand())) {
      if (!isa<TryApplyInst>(AI.getInstruction())) {
        // Check that open_existential_metatype is only used by
        // witness_metho
        // and apply instructions.
        bool isValid = true;
        unsigned Count = 0;
        for (auto Use : getNonDebugUses(OEMI)) {
          auto User = Use->getUser();
          Count++;
          if (isa<ApplyInst>(User) || isa<WitnessMethodInst>(User))
            continue;
          isValid = false;
          break;
        }
        if (isValid && Count == 2) {
          // Use a more efficient sequence
          // metatype (open_existential_addr)
          SILBuilderWithScope Builder(CMI);
          auto OEAI = Builder.createOpenExistentialAddr(
              AI.getLoc(), EMI->getOperand(),
              Self->getType().getMetatypeInstanceType(Module));
          Self = Builder.createMetatype(AI.getLoc(), Self->getType());
          SILBuilderWithScope Builder2(
              std::next(AI.getInstruction()->getIterator()));
          Builder2.createFixLifetime(AI.getLoc(), OEAI);
          OEMI->replaceAllUsesWith(Self);
          // Remove the old Self
          OEMI->eraseFromParent();
          if (onlyHaveDebugUses(EMI))
            EMI->eraseFromParent();
        }
      }
    }
  }

  // Check for a special pattern:
  // %vmt = value_metatype
  // apply %f(%vmt)
  if (auto *VMT = dyn_cast<ValueMetatypeInst>(Self)) {
    bool isValid = true;
    for (auto Use : getNonDebugUses(VMT)) {
      if (auto UAI = FullApplySite::isa(Use->getUser())) {
        if (UAI.getSelfArgument() == Self && UAI.getCallee() == CMI)
          continue;
      }
      isValid = false;
      break;
    }

    if (isValid) {
      SILBuilderWithScope Builder(VMT);
      Self = Builder.createMetatype(AI.getLoc(), VMT->getType());
      VMT->replaceAllUsesWith(Self);
      VMT->eraseFromParent();
    }
  }
  return Self;
}

// Insert monomorphic inline caches for a specific class or metatype
/// type \p SubClassTy.
static FullApplySite speculateMonomorphicTarget(FullApplySite AI,
                                                SILType SubType,
                                                CheckedCastAddrBranchInst *&CCBI
) {
  CCBI = nullptr;

  auto &Module = AI.getModule();
  WitnessMethodInst *CMI = cast<WitnessMethodInst>(AI.getCallee());
  ProtocolDecl *WMIProtocol = CMI->getLookupProtocol();
  assert(AI.hasSelfArgument());
  auto Self = AI.getSelfArgument();
  bool isMetatype = false;
  // Is it a call on a metatype? E.g. a constructor call or static/class method call?
  //if (isa<MetatypeType>(Self->getType().getSwiftRValueType())) {
  if (Self->getType().is<MetatypeType>()) {
    //return speculateMonomorphicMetatypeTarget(AI, SubType, CCBI);
    isMetatype = true;

    // NOTE: For speculative devirtualization we do not always need
    // the exact dynamic type. A static type of the opaque existention is
    // also OK.
    // This is because for the non-class payloads, the static type and the
    // dynamic type are always the same. For classes, it may be different,
    // because the actual instance may be of a derived type of the static type.
    if (!WMIProtocol->requiresClass()) {
      Self = replaceDynamicTypeByStaticType(AI, Self);
    }
  }

  SILType LookupType = SubType;
  if (isMetatype && SubType.is<MetatypeType>()) {
    //assert(SubType.is<MetatypeType>() && "Sub-type should be a metatype");
    LookupType = SubType.getMetatypeInstanceType(Module);
  }

  // Bail if this witness_method cannot be devirtualized.
  if (!canDevirtualizeClassMethod(AI, LookupType))
    return FullApplySite();

  // Create a diamond shaped control flow and a checked_cast_addr_br
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

  auto SubTypeMetatype = MetatypeType::get(SubType.getSwiftRValueType(),
                                           MetatypeRepresentation::Thick);

  SILType TargetType;
  if ((isMetatype && SubType.is<MetatypeType>()) || WMIProtocol->requiresClass())
    TargetType = SubType;
  else
    TargetType = Module.Types.getLoweredType(SubTypeMetatype, 0);

  Iden->createBBArg(TargetType);

  SILBasicBlock *Continue = Entry->splitBasicBlock(It);

  SILBuilderWithScope Builder(Entry, AI.getInstruction());

  auto Loc = AI.getLoc();
  // Check if the type of of self is identical to the subtype.
  // To do this, extract the information about the static type from the
  // existential or extract the information from the argument of a generic type.
  //
  // Create the checked_cast_addr_branch instruction that checks at runtime if the
  // class instance is identical to the SILType.
  //
  if (isMetatype || WMIProtocol->requiresClass()) {
    assert((TargetType.is<MetatypeType>() ||
            WMIProtocol->requiresClass()) &&
           "Target type should be a metatype");
    Builder.createCheckedCastBranch(AI.getLoc(), /*exact*/ true, Self,
                                    TargetType, Iden, Virt);
  } else {
    // It is OK to look at the static type of the existential.
    auto MetaTy = Builder.createMetatype(
        Loc, Module.Types.getLoweredType(
                 MetatypeType::get(
                     Self->getType().getSwiftRValueType(),
                     SubTypeMetatype->getRepresentation()),
                 0));
    // CCBI =
    Builder.createCheckedCastBranch(AI.getLoc(), /*exact*/ true,
                                    MetaTy, TargetType, Iden,
                                    Virt);


  }

  SILBuilderWithScope VirtBuilder(Virt, AI.getInstruction());
  SILBuilderWithScope IdenBuilder(Iden, AI.getInstruction());


  // Load the result of the cast.
  //SILValue DownCastedClassInstance = IdenBuilder.createLoad(AI.getLoc(), TargetAlloc);
  SILValue DownCastedClassInstance;
  // TODO: It could happen that this copy is used later. Thus, TargetAlloc cannot
  // be deallocated yet.
//  DownCastedClassInstance = TargetAlloc;

  // Self is always the address of the actual payload inside the existential or
  // the address of the argument of a generic type conforming to a protocol.
  DownCastedClassInstance = Self;
  if (!isMetatype) {
    if (WMIProtocol->requiresClass())
      DownCastedClassInstance = Iden->getBBArg(0);
    else
      // Cast self to type required by the devirtualized function.
      DownCastedClassInstance = castValueToABICompatibleType(
                                    &IdenBuilder, AI.getLoc(), Self,
                                    Self->getType(), SubType.getAddressType())
                                    .getValue();
  } else {
    DownCastedClassInstance = IdenBuilder.createMetatype(
        AI.getLoc(), getThickMetatypeType(SubType.getSwiftRValueType()));
  }
  //  IdenBuilder.createDeallocStack(Loc, TargetAlloc);

  // Copy the two apply instructions into the two blocks.
  FullApplySite IdenAI = CloneApply(AI, IdenBuilder);
  FullApplySite VirtAI = CloneApply(AI, VirtBuilder);

  // See if Continue has a release on self as the instruction right after the
  // apply. If it exists, move it into position in the diamond.
  if (auto *Release =
          dyn_cast<StrongReleaseInst>(std::next(Continue->begin()))) {
    if (Release->getOperand() == Self) {
      VirtBuilder.createStrongRelease(Release->getLoc(), CMI->getOperand(),
                                      Atomicity::Atomic);
      IdenBuilder.createStrongRelease(
          Release->getLoc(), DownCastedClassInstance, Atomicity::Atomic);
      Release->eraseFromParent();
    }
  }

  // Create a PHInode for returning the return value from both apply
  // instructions.
  SILArgument *Arg = Continue->createBBArg(AI.getType());
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
  // TODO: If we are performing a devirtualized call on a copy of self,
  // the call may mutate this copy. But it will not mutate the original
  // existential self. Therefore, we need to write the mutated copy
  // back to the existential self after the call to mimic the original
  // semantics.
  auto NewInstPair = devirtualizeClassMethod(IdenAI, DownCastedClassInstance);
  assert(NewInstPair.first && "Expected to be able to devirtualize apply!");
  replaceDeadApply(IdenAI, NewInstPair.first);

  // It is safe to dealloc values after the call.
  if (NewInstPair.first->getParentBB()->getTerminator() != NewInstPair.first) {
    SILBuilderWithScope Builder(
        &*std::next(NewInstPair.second.getInstruction()->getIterator()),
        NewInstPair.second.getInstruction());
  } else {
    // Insert the dealloc at the beginning of all successor blocks.
    for (auto SuccBB : NewInstPair.first->getParentBB()->getSuccessorBlocks()) {
      assert(SuccBB->getSinglePredecessor() &&
             "Successor blocks should have a single predecessor");
      ////SILBuilderWithScope Builder(&SuccBB->front());
    }
    //llvm_unreachable("try_apply not supported yet");
  }

  // Split critical edges resulting from VirtAI.
  if (auto *TAI = dyn_cast<TryApplyInst>(VirtAI)) {
    auto *ErrorBB = TAI->getFunction()->createBasicBlock();
    ErrorBB->createBBArg(TAI->getErrorBB()->getBBArg(0)->getType());
    Builder.setInsertionPoint(ErrorBB);
    Builder.createBranch(TAI->getLoc(), TAI->getErrorBB(),
                         {ErrorBB->getBBArg(0)});

    auto *NormalBB = TAI->getFunction()->createBasicBlock();
    NormalBB->createBBArg(TAI->getNormalBB()->getBBArg(0)->getType());
    Builder.setInsertionPoint(NormalBB);
    Builder.createBranch(TAI->getLoc(), TAI->getNormalBB(),
                        {NormalBB->getBBArg(0) });

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
  case Accessibility::Public:
    return false;
  case Accessibility::Internal:
    if (!AI.getModule().isWholeModule())
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
                                 ClassHierarchyAnalysis *CHA,
                                 ClassMethodInst *CMI) {
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
  if (isNominalTypeWithUnboundGenericParameters(SubType, AI.getModule()))
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

  // TODO: Remove any candidates that are not profitable from the inlining
  // point of view.

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

/// \brief Try to speculate the call target for the call \p AI. This function
/// returns true if a change was made.
static bool tryToSpeculateTarget(FullApplySite AI,
                                 ClassHierarchyAnalysis *CHA,
                                 WitnessMethodInst *CMI) {
  // We cannot devirtualize in cases where dynamic calls are
  // semantically required.
  if (CMI->isVolatile())
    return false;

  ProtocolDecl *WMIProtocol = CMI->getLookupProtocol();

  if (!SpecDevirtNonClassProtocols) {
    // Support only devirtualization of class protocols.
    if (!WMIProtocol->requiresClass())
      return false;
  }

  if (!WMIProtocol->requiresClass()) {
    if (!shouldProcessFunction(Functions, AI.getFunction()->getName(), startIdx, endIdx))
      return false;
  }

  SILType SubType;
  if (CMI->hasOperand()) {
    SubType = CMI->getOperand()->getType();
  } else {
    SubType = SILType::getPrimitiveObjectType(CMI->getLookupType());
  }

  auto &M = CMI->getModule();
  auto ClassType = SubType;
  // In case of metatypes, switch on the instance type.
  if (SubType.is<MetatypeType>())
    ClassType = SubType.getMetatypeInstanceType(M);

  if (!CHA->hasKnownImplementations(WMIProtocol)) {
    return false;
  }

  auto &Impls = CHA->getProtocolImplementations(WMIProtocol);

  SmallVector<NominalTypeDecl *, 8> Subs(Impls);

  if (isa<BoundGenericClassType>(ClassType.getSwiftRValueType())) {
    // Filter out any subclasses that do not inherit from this
    // specific bound class.
    auto RemovedIt = std::remove_if(Subs.begin(),
        Subs.end(),
        [&ClassType, &M](NominalTypeDecl *Sub){
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

  // TODO: Remove any candidates that are not profitable from the inlining
  // point of view.

  if (Subs.size() > MaxNumSpeculativeTargets) {
    // TODO: Use PGO to handle the most probable alternatives.
    DEBUG(llvm::dbgs() << "Protocol " << WMIProtocol->getName() << " has too many (" <<
          Subs.size() << ") implementations. Not speculating.\n");
    return false;
  }

  DEBUG(llvm::dbgs() << "Protocol " << WMIProtocol->getName()
                     << " has multiple known implementations. "
                        "Inserting polymorphic speculative call.\n");

  // Perform a speculative devirtualization of a method invocation.
  // It replaces an indirect witness_method-based call by a code to perform
  // a direct call of the method implementation based on the dynamic class
  // of the instance.
  //
  // The code is generated according to the following principles:
  //
  // - For each class implementing a protocol, a dedicated checked_cast_br instruction
  // is generated to check if a dynamic class of the instance is exactly
  // this class.
  //
  // - If this check succeeds, then it jumps to the code which performs a
  // direct call of a method implementation specific to this class.
  //
  // - If this check fails, then a different class is checked by means of
  // checked_cast_br in a similar way.
  //
  // TODO: The ordering of checks may benefit from using a PGO, because
  // the most probable alternatives could be checked first.
  //
  // TODO: If we have a class existential, then IRGen will lower dispatch
  // on its type into a call of swift_getObjectType() followed by a switch.
  // But it can be possible to have a cheaper way to to do this. We could
  // check that the pointer it not a tagged pointer and then extract that
  // static type as using a load. If the pointer is a tagged pointer,
  // it is an ObjC object and thus its type cannot be equal to any
  // Swift candidate type, which does not have an ObjC base.
  // We may need to take non-ISA cases into account too.

  // TODO: Group candidates into class and non-class candidates, if
  // this is an opaque existential. This can be used to quickly skip
  // useless checks if the existential contains a class, and the candidate
  // is not a class.

  // Number of subclasses which cannot be handled by checked_cast_br checks.
  int NotHandledSubsNum = 0;
  // True if any instructions were changed or generated.
  bool Changed = false;

  for (auto S : Subs) {
    DEBUG(llvm::dbgs() << "Inserting a speculative call for class "
          << WMIProtocol->getName() << " and implementation " << S->getName() << "\n");

    CanType CanTy = S->getDeclaredType()->getCanonicalType();
    if (!CanTy) {
      NotHandledSubsNum++;
      continue;
    }

    SILType ObjectTy = SILType::getPrimitiveObjectType(CanTy);

    if (!ObjectTy.getClassOrBoundGenericClass() &&
        !ObjectTy.getStructOrBoundGenericStruct() &&
        !ObjectTy.getEnumOrBoundGenericEnum()) {
      // This subclass cannot be handled. This happens e.g. if it is
      // a generic class.
      //llvm::dbgs() << "Cannot speculatively devirtualize type: "
      //             << (int)ObjectTy.getSwiftRValueType()->getKind() << "\n";
      //CanTy.dump();
      NotHandledSubsNum++;
      continue;
    }

    auto ObjectOrMetatypeType = ObjectTy;
    if (auto EMT = SubType.getAs<AnyMetatypeType>()) {
      auto InstTy = ObjectTy.getSwiftRValueType();
      auto *MetaTy = MetatypeType::get(InstTy, EMT->getRepresentation());
      auto CanMetaTy = CanMetatypeType::CanTypeWrapper(MetaTy);
      ObjectOrMetatypeType = SILType::getPrimitiveObjectType(CanMetaTy);
    }

    FullApplySite NewAI;
    // Pass the metatype of the subclass.
    CheckedCastAddrBranchInst *LastCCABI = nullptr;
    NewAI = speculateMonomorphicTarget(AI, ObjectOrMetatypeType,
                                       LastCCABI);
    if (!NewAI) {
      NotHandledSubsNum++;
      continue;
    }
    AI = NewAI;
    Changed = true;
  }

  return Changed;
}

static bool tryToSpeculateTarget(FullApplySite AI,
                                 ClassHierarchyAnalysis *CHA) {
  if (auto *CMI = dyn_cast<ClassMethodInst>(AI.getCallee()))
      return tryToSpeculateTarget(AI, CHA, CMI);
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI.getCallee())) {
      return tryToSpeculateTarget(AI, CHA, WMI);
  }
  return false;
}

namespace {
/// Speculate the targets of virtual calls by assuming that the requested
/// class is at the bottom of the class hierarchy.
class SpeculativeDevirtualization : public SILFunctionTransform {
public:
  virtual ~SpeculativeDevirtualization() {}

  void run() override {
    ClassHierarchyAnalysis *CHA = PM->getAnalysis<ClassHierarchyAnalysis>();

    bool Changed = false;

    // Collect virtual calls that may be specialized.
    SmallVector<FullApplySite, 16> ToSpecialize;
    for (auto &BB : *getFunction()) {
      for (auto II = BB.begin(), IE = BB.end(); II != IE; ++II) {
        FullApplySite AI = FullApplySite::isa(&*II);
        if (!AI)
          continue;
        // if (AI.getSubstCalleeType()->getRepresentation() ==
        //    SILFunctionType::Representation::ObjCMethod)
        //  continue;
        if (AI && isa<ClassMethodInst>(AI.getCallee())) {
          ToSpecialize.push_back(AI);
          continue;
        }
        if (AI && isa<WitnessMethodInst>(AI.getCallee())) {
          ToSpecialize.push_back(AI);
          continue;
        }
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
