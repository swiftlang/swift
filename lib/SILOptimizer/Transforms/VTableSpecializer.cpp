//===--- VTableSpecializer.cpp - Specialization of vtables ----------------===//
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
// Specialize vtables of generic classes for embedded Swift.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-vtable-specializer"

#include "llvm/ADT/SmallVector.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/OptimizationRemark.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/ConstantFolding.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/StackNesting.h"

using namespace swift;

namespace {

class VTableSpecializer : public SILModuleTransform {
  bool specializeVTables(SILModule &module);
  bool specializeVTableFor(AllocRefInst *allocRef, SILModule &module);
  SILFunction *specializeVTableMethod(SILFunction *origMethod,
                                      SubstitutionMap subs, SILModule &module);
  bool specializeClassMethodInst(ClassMethodInst *cm);

  /// The entry point to the transformation.
  void run() override {
    SILModule &module = *getModule();

    if (!module.getASTContext().LangOpts.hasFeature(Feature::Embedded)) return;

    LLVM_DEBUG(llvm::dbgs() << "***** VTableSpecializer\n");

    if (specializeVTables(module)) invalidateFunctionTables();
  }
};

}  // end anonymous namespace

bool VTableSpecializer::specializeVTables(SILModule &module) {
  bool changed = false;
  for (SILFunction &func : module) {
    if (func.getLoweredFunctionType()->isPolymorphic()) continue;

    for (SILBasicBlock &block : func) {
      for (SILInstruction &inst : block) {
        if (auto *allocRef = dyn_cast<AllocRefInst>(&inst)) {
          changed |= specializeVTableFor(allocRef, module);
          continue;
        }
        if (auto *cm = dyn_cast<ClassMethodInst>(&inst)) {
          changed |= specializeClassMethodInst(cm);
          continue;
        }
      }
    }
  }

  for (SILVTable *vtable : module.getVTables()) {
    if (vtable->getClass()->isGenericContext()) continue;

    for (SILVTableEntry &entry : vtable->getMutableEntries()) {
      SILFunction *method = entry.getImplementation();
      if (!method->getLoweredFunctionType()->isPolymorphic()) continue;

      if (entry.getKind() != SILVTableEntry::Kind::Inherited) {
        vtable->dump();
        entry.getMethod().dump();
      }
      assert(entry.getKind() == SILVTableEntry::Kind::Inherited);
      Decl *classOfMethod =
          entry.getMethod().getDecl()->getDeclContext()->getAsDecl();
      SILType classTy = vtable->getClassType();
      while (classTy.getClassOrBoundGenericClass() != classOfMethod) {
        classTy = classTy.getSuperclass();
      }
      auto *classDecl = cast<ClassDecl>(classOfMethod);
      SubstitutionMap subs = classTy.getASTType()->getContextSubstitutionMap(
          classDecl->getParentModule(), classDecl);

      SILFunction *specializedMethod =
          specializeVTableMethod(method, subs, module);
      entry.setImplementation(specializedMethod);
      vtable->updateVTableCache(entry);
    }
  }

  return changed;
}

bool VTableSpecializer::specializeVTableFor(AllocRefInst *allocRef,
                                            SILModule &module) {
  SILType classTy = allocRef->getType();
  CanType astType = classTy.getASTType();
  BoundGenericClassType *genClassTy = dyn_cast<BoundGenericClassType>(astType);
  if (!genClassTy) return false;

  if (module.lookUpSpecializedVTable(classTy)) return false;

  LLVM_DEBUG(llvm::dbgs() << "specializeVTableFor "
                          << genClassTy->getDecl()->getName() << '\n');

  ClassDecl *classDecl = genClassTy->getDecl();
  SILVTable *origVtable = module.lookUpVTable(classDecl);
  if (!origVtable) {
    llvm::errs() << "No vtable available for "
                 << genClassTy->getDecl()->getName() << '\n';
    llvm::report_fatal_error("no vtable");
  }

  SubstitutionMap subs = astType->getContextSubstitutionMap(
      classDecl->getParentModule(), classDecl);

  llvm::SmallVector<SILVTableEntry, 8> newEntries;

  for (const SILVTableEntry &entry : origVtable->getEntries()) {
    SILFunction *origMethod = entry.getImplementation();
    SILFunction *specializedMethod =
        specializeVTableMethod(origMethod, subs, module);
    newEntries.push_back(SILVTableEntry(entry.getMethod(), specializedMethod,
                                        entry.getKind(),
                                        entry.isNonOverridden()));
  }

  SILVTable::create(module, classDecl, classTy, IsNotSerialized, newEntries);
  return true;
}

SILFunction *VTableSpecializer::specializeVTableMethod(SILFunction *origMethod,
                                                       SubstitutionMap subs,
                                                       SILModule &module) {
  LLVM_DEBUG(llvm::dbgs() << "specializeVTableMethod " << origMethod->getName()
                          << '\n');

  if (!origMethod->getLoweredFunctionType()->isPolymorphic()) return origMethod;

  LLVM_DEBUG(llvm::dbgs() << "specializeVTableMethod " << origMethod->getName()
                          << '\n');

  ReabstractionInfo ReInfo(module.getSwiftModule(), module.isWholeModule(),
                           ApplySite(), origMethod, subs, IsNotSerialized,
                           /*ConvertIndirectToDirect=*/true,
                           /*dropMetatypeArgs=*/false);

  if (!ReInfo.canBeSpecialized()) {
    llvm::errs() << "Cannot specialize vtable method " << origMethod->getName()
                 << '\n';
    llvm::report_fatal_error("cannot specialize vtable method");
  }

  SILOptFunctionBuilder FunctionBuilder(*this);

  GenericFuncSpecializer FuncSpecializer(FunctionBuilder, origMethod, subs,
                                         ReInfo, /*isMandatory=*/true);
  SILFunction *SpecializedF = FuncSpecializer.lookupSpecialization();
  if (!SpecializedF) SpecializedF = FuncSpecializer.tryCreateSpecialization();
  if (!SpecializedF || SpecializedF->getLoweredFunctionType()->hasError()) {
    llvm::errs()
        << "Cannot specialize vtable method " << origMethod->getName() << '\n'
        << "Generic class methods are not supported in embedded mode\n";
    llvm::report_fatal_error("cannot specialize vtable method");
  }

  // Link after specializing to pull in everything referenced from another
  // module in case some referenced functions have non-public linkage.
  module.linkFunction(SpecializedF, SILModule::LinkingMode::LinkAll);

  SpecializedF->setLinkage(SILLinkage::Public);
  SpecializedF->setSerialized(IsNotSerialized);

  return SpecializedF;
}

bool VTableSpecializer::specializeClassMethodInst(ClassMethodInst *cm) {
  SILValue instance = cm->getOperand();
  SILType classTy = instance->getType();
  CanType astType = classTy.getASTType();
  BoundGenericClassType *genClassTy = dyn_cast<BoundGenericClassType>(astType);
  if (!genClassTy) return false;

  ClassDecl *classDecl = genClassTy->getDecl();
  SubstitutionMap subs = astType->getContextSubstitutionMap(
      classDecl->getParentModule(), classDecl);

  SILType funcTy = cm->getType();

  SILFunction *f = cm->getFunction();
  SILModule &m = f->getModule();
  SILType substitutedType =
      funcTy.substGenericArgs(m, subs, TypeExpansionContext::minimal());

  ReabstractionInfo reInfo(substitutedType.getAs<SILFunctionType>(), m);
  reInfo.createSubstitutedAndSpecializedTypes();
  CanSILFunctionType finalFuncTy = reInfo.getSpecializedType();
  SILType finalSILTy = SILType::getPrimitiveObjectType(finalFuncTy);

  SILBuilder builder(cm);
  auto *newCM = builder.createClassMethod(cm->getLoc(), cm->getOperand(),
                                          cm->getMember(), finalSILTy);

  while (!cm->use_empty()) {
    Operand *use = *cm->use_begin();
    SILInstruction *user = use->getUser();
    ApplySite AI = ApplySite::isa(user);
    if (AI && AI.getCalleeOperand() == use) {
      replaceWithSpecializedCallee(AI, newCM, reInfo);
      AI.getInstruction()->eraseFromParent();
      continue;
    }
    llvm::errs() << "unsupported use of class method "
                 << newCM->getMember().getDecl()->getName() << " in function "
                 << newCM->getFunction()->getName() << '\n';
    llvm::report_fatal_error("unsupported class method");
  }

  return true;
}

SILTransform *swift::createVTableSpecializer() {
  return new VTableSpecializer();
}
