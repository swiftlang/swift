//===--- GenericSpecializer.cpp - Specialization of generic functions -----===//
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
// Specialize calls to generic functions by substituting static type
// information.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-generic-specializer"

#include "swift/SIL/OptimizationRemark.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/ConstantFolding.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

namespace {
static void transferSpecializeAttributeTargets(SILModule &M,
                                               SILOptFunctionBuilder &builder,
                                               Decl *d) {
  auto *vd = cast<AbstractFunctionDecl>(d);
  for (auto *A : vd->getAttrs().getAttributes<SpecializeAttr>()) {
    auto *SA = cast<SpecializeAttr>(A);
    // Filter _spi.
    auto spiGroups = SA->getSPIGroups();
    auto hasSPIGroup = !spiGroups.empty();
    if (hasSPIGroup) {
      if (vd->getModuleContext() != M.getSwiftModule() &&
          !M.getSwiftModule()->isImportedAsSPI(SA, vd)) {
        continue;
      }
    }
    if (auto *targetFunctionDecl = SA->getTargetFunctionDecl(vd)) {
      auto target = SILDeclRef(targetFunctionDecl);
      auto targetSILFunction = builder.getOrCreateFunction(
          SILLocation(vd), target, NotForDefinition,
          [&builder](SILLocation loc, SILDeclRef constant) -> SILFunction * {
            return builder.getOrCreateFunction(loc, constant, NotForDefinition);
          });
      auto kind = SA->getSpecializationKind() ==
                          SpecializeAttr::SpecializationKind::Full
                      ? SILSpecializeAttr::SpecializationKind::Full
                      : SILSpecializeAttr::SpecializationKind::Partial;
      Identifier spiGroupIdent;
      if (hasSPIGroup) {
        spiGroupIdent = spiGroups[0];
      }
      auto availability = AvailabilityInference::annotatedAvailableRangeForAttr(
          SA, M.getSwiftModule()->getASTContext());
      targetSILFunction->addSpecializeAttr(SILSpecializeAttr::create(
          M, SA->getSpecializedSignature(), SA->isExported(), kind, nullptr,
          spiGroupIdent, vd->getModuleContext(), availability));
    }
  }
}

static bool specializeAppliesInFunction(SILFunction &F,
                                        SILTransform *transform,
                                        bool isMandatory) {
  SILOptFunctionBuilder FunctionBuilder(*transform);
  DeadInstructionSet DeadApplies;
  llvm::SmallSetVector<SILInstruction *, 8> Applies;
  OptRemark::Emitter ORE(DEBUG_TYPE, F);

  bool Changed = false;
  for (auto &BB : F) {
    // Collect the applies for this block in reverse order so that we
    // can pop them off the end of our vector and process them in
    // forward order.
    for (auto &I : llvm::reverse(BB)) {

      // Skip non-apply instructions, apply instructions with no
      // substitutions, apply instructions where we do not statically
      // know the called function, and apply instructions where we do
      // not have the body of the called function.
      ApplySite Apply = ApplySite::isa(&I);
      if (!Apply || !Apply.hasSubstitutions())
        continue;

      auto *Callee = Apply.getReferencedFunctionOrNull();
      if (!Callee)
        continue;

      FunctionBuilder.getModule().performOnceForPrespecializedImportedExtensions(
        [&FunctionBuilder](AbstractFunctionDecl *pre) {
        transferSpecializeAttributeTargets(FunctionBuilder.getModule(), FunctionBuilder,
                                           pre);
        });

      if (!Callee->isDefinition() && !Callee->hasPrespecialization()) {
        ORE.emit([&]() {
          using namespace OptRemark;
          return RemarkMissed("NoDef", I)
                 << "Unable to specialize generic function "
                 << NV("Callee", Callee) << " since definition is not visible";
        });
        continue;
      }

      Applies.insert(Apply.getInstruction());
    }

    // Attempt to specialize each apply we collected, deleting any
    // that we do specialize (along with other instructions we clone
    // in the process of doing so). We pop from the end of the list to
    // avoid tricky iterator invalidation issues.
    while (!Applies.empty()) {
      auto *I = Applies.pop_back_val();
      auto Apply = ApplySite::isa(I);
      assert(Apply && "Expected an apply!");
      SILFunction *Callee = Apply.getReferencedFunctionOrNull();
      assert(Callee && "Expected to have a known callee");

      if (!Apply.canOptimize())
        continue;

      if (!isMandatory && !Callee->shouldOptimize())
        continue;

      // We have a call that can potentially be specialized, so
      // attempt to do so.
      llvm::SmallVector<SILFunction *, 2> NewFunctions;
      trySpecializeApplyOfGeneric(FunctionBuilder, Apply, DeadApplies,
                                  NewFunctions, ORE, isMandatory);

      // Remove all the now-dead applies. We must do this immediately
      // rather than defer it in order to avoid problems with cloning
      // dead instructions when doing recursive specialization.
      while (!DeadApplies.empty()) {
        auto *AI = DeadApplies.pop_back_val();

        // Remove any applies we are deleting so that we don't attempt
        // to specialize them.
        Applies.remove(AI);

        recursivelyDeleteTriviallyDeadInstructions(AI, true);
        Changed = true;
      }

      if (auto *sft = dyn_cast<SILFunctionTransform>(transform)) {
        // If calling the specialization utility resulted in new functions
        // (as opposed to returning a previous specialization), we need to notify
        // the pass manager so that the new functions get optimized.
        for (SILFunction *NewF : reverse(NewFunctions)) {
          sft->addFunctionToPassManagerWorklist(NewF, Callee);
        }
      }
    }
  }

  return Changed;
}

/// The generic specializer, used in the optimization pipeline.
class GenericSpecializer : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    SILFunction &F = *getFunction();

    LLVM_DEBUG(llvm::dbgs() << "***** GenericSpecializer on function:"
                            << F.getName() << " *****\n");

    if (specializeAppliesInFunction(F, this, /*isMandatory*/ false)) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);
    }
  }
};

/// The mandatory specializer, which runs in the mandatory pipeline.
///
/// It specializes functions, called from performance-annotated functions
/// (@_noLocks, @_noAllocation).
class MandatoryGenericSpecializer : public SILModuleTransform {

  void run() override;

  bool optimize(SILFunction *func, ClassHierarchyAnalysis *cha);

  bool optimizeInst(SILInstruction *inst, SILOptFunctionBuilder &funcBuilder,
                    InstructionDeleter &deleter, ClassHierarchyAnalysis *cha);
};


void MandatoryGenericSpecializer::run() {
  SILModule *module = getModule();

  if (!module->getOptions().EnablePerformanceAnnotations)
    return;

  ClassHierarchyAnalysis *cha = getAnalysis<ClassHierarchyAnalysis>();
  
  llvm::SmallVector<SILFunction *, 8> workList;
  llvm::SmallPtrSet<SILFunction *, 16> visited;
  
  // Look for performance-annotated functions.
  for (SILFunction &function : *module) {
    if (function.getPerfConstraints() != PerformanceConstraints::None) {
      workList.push_back(&function);
      visited.insert(&function);
    }
  }
  
  while (!workList.empty()) {
    SILFunction *func = workList.pop_back_val();
    module->linkFunction(func, SILModule::LinkingMode::LinkAll);
    if (!func->isDefinition())
      continue;

    // Perform generic specialization and other related optimzations.

    // To avoid phase ordering problems of the involved optimizations, iterate
    // until we reach a fixed point.
    // This should always happen, but to be on the save side, limit the number
    // of iterations to 10 (which is more than enough - usually the loop runs
    // 1 to 3 times).
    for (int i = 0; i < 10; i++) {
      bool changed = optimize(func, cha);
      if (changed) {
        invalidateAnalysis(func, SILAnalysis::InvalidationKind::Everything);
      } else {
        break;
      }
    }

    // Continue specializing called functions.
    for (SILBasicBlock &block : *func) {
      for (SILInstruction &inst : block) {
        if (auto as = ApplySite::isa(&inst)) {
          if (SILFunction *callee = as.getReferencedFunctionOrNull()) {
            if (visited.insert(callee).second)
              workList.push_back(callee);
          }
        }
      }
    }
  }
}

/// Specialize generic calls in \p func and do some other related optimizations:
/// devirtualization and constant-folding of the Builtin.canBeClass.
bool MandatoryGenericSpecializer::optimize(SILFunction *func,
                                           ClassHierarchyAnalysis *cha) {
  bool changed = false;
  SILOptFunctionBuilder funcBuilder(*this);
  InstructionDeleter deleter;
  ReachingReturnBlocks rrBlocks(func);
  NonErrorHandlingBlocks neBlocks(func);

  // If this is a just specialized function, try to optimize copy_addr, etc.
  // instructions.
  if (optimizeMemoryAccesses(*func)) {
    eliminateDeadAllocations(*func);
    changed = true;
  }

  // Visiting blocks in reverse order avoids revisiting instructions after block
  // splitting, which would be quadratic.
  for (SILBasicBlock &block : llvm::reverse(*func)) {
    // Only consider blocks which are not on a "throw" path.
    if (!rrBlocks.reachesReturn(&block) || !neBlocks.isNonErrorHandling(&block))
      continue;
  
    for (SILInstruction *inst : deleter.updatingReverseRange(&block)) {
      changed |= optimizeInst(inst, funcBuilder, deleter, cha);
    }
  }
  deleter.cleanupDeadInstructions();

  if (specializeAppliesInFunction(*func, this, /*isMandatory*/ true))
    changed = true;

  return changed;
}

bool MandatoryGenericSpecializer::
optimizeInst(SILInstruction *inst, SILOptFunctionBuilder &funcBuilder,
             InstructionDeleter &deleter, ClassHierarchyAnalysis *cha) {
  if (auto as = ApplySite::isa(inst)) {
    // Specialization opens opportunities to devirtualize method calls.
    ApplySite newAS = tryDevirtualizeApply(as, cha).first;
    if (!newAS)
      return false;
    deleter.forceDelete(as.getInstruction());
    auto newFAS = FullApplySite::isa(newAS.getInstruction());
    if (!newFAS)
      return true;
      
    SILFunction *callee = newFAS.getReferencedFunctionOrNull();
    if (!callee || callee->isTransparent() == IsNotTransparent)
      return true;

    if (callee->isExternalDeclaration())
      getModule()->loadFunction(callee, SILModule::LinkingMode::LinkAll);

    if (callee->isExternalDeclaration())
      return true;

    // If the de-virtualized callee is a transparent function, inline it.
    SILInliner::inlineFullApply(newFAS, SILInliner::InlineKind::MandatoryInline,
                                funcBuilder, deleter);
    return true;
  }
  if (auto *bi = dyn_cast<BuiltinInst>(inst)) {
    // Constant-fold the Builtin.canBeClass. This is essential for Array code.
    if (bi->getBuiltinInfo().ID != BuiltinValueKind::CanBeObjCClass)
      return false;

    SILBuilderWithScope builder(bi);
    IntegerLiteralInst *lit = optimizeBuiltinCanBeObjCClass(bi, builder);
    if (!lit)
      return false;

    bi->replaceAllUsesWith(lit);
    ConstantFolder constFolder(funcBuilder, getOptions().AssertConfig,
                               /*EnableDiagnostics*/ false);
    constFolder.addToWorklist(lit);
    constFolder.processWorkList();
    deleter.forceDelete(bi);
    return true;
  }
  return false;
}

} // end anonymous namespace

SILTransform *swift::createGenericSpecializer() {
  return new GenericSpecializer();
}

SILTransform *swift::createMandatoryGenericSpecializer() {
  return new MandatoryGenericSpecializer();
}
