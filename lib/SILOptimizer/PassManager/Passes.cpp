//===--- Passes.cpp - Swift Compiler SIL Pass Entrypoints -----------------===//
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
///
///  \file
///  \brief This file provides implementations of a few helper functions
///  which provide abstracted entrypoints to the SILPasses stage.
///
///  \note The actual SIL passes should be implemented in per-pass source files,
///  not in this file.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-optimizer"

#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;

bool swift::runSILDiagnosticPasses(SILModule &Module) {
  // Verify the module, if required.
  if (Module.getOptions().VerifyAll)
    Module.verify();

  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;

  auto &Ctx = Module.getASTContext();

  SILPassManager PM(&Module);
  PM.executePassPipelinePlan(
      SILPassPipelinePlan::getDiagnosticPassPipeline(Module.getOptions()));

  // If we were asked to debug serialization, exit now.
  if (Module.getOptions().DebugSerialization)
    return Ctx.hadError();

  // Generate diagnostics.
  Module.setStage(SILStage::Canonical);

  // Verify the module, if required.
  if (Module.getOptions().VerifyAll)
    Module.verify();
  else {
    DEBUG(Module.verify());
  }

  // If errors were produced during SIL analysis, return true.
  return Ctx.hadError();
}

bool swift::runSILOwnershipEliminatorPass(SILModule &Module) {
  auto &Ctx = Module.getASTContext();

  SILPassManager PM(&Module);
  PM.executePassPipelinePlan(
      SILPassPipelinePlan::getOwnershipEliminatorPassPipeline());

  return Ctx.hadError();
}

void swift::runSILOptimizationPasses(SILModule &Module) {
  // Verify the module, if required.
  if (Module.getOptions().VerifyAll)
    Module.verify();

  if (Module.getOptions().DisableSILPerfOptimizations)
    return;

  SILPassManager PM(&Module);
  PM.executePassPipelinePlan(
      SILPassPipelinePlan::getPerformancePassPipeline(Module.getOptions()));

  // If we were asked to debug serialization, exit now.
  if (Module.getOptions().DebugSerialization)
    return;

  // Verify the module, if required.
  if (Module.getOptions().VerifyAll)
    Module.verify();
  else {
    DEBUG(Module.verify());
  }
}

void swift::runSILPassesForOnone(SILModule &Module) {
  // Verify the module, if required.
  if (Module.getOptions().VerifyAll)
    Module.verify();

  SILPassManager PM(&Module, "Onone");
  PM.executePassPipelinePlan(SILPassPipelinePlan::getOnonePassPipeline());

  // Verify the module, if required.
  if (Module.getOptions().VerifyAll)
    Module.verify();
  else {
    DEBUG(Module.verify());
  }
}

void swift::runSILOptimizationPassesWithFileSpecification(SILModule &M,
                                                          StringRef Filename) {
  SILPassManager PM(&M);
  PM.executePassPipelinePlan(
      SILPassPipelinePlan::getPassPipelineFromFile(Filename));
}

PassKind swift::PassKindFromString(StringRef Name) {
  return llvm::StringSwitch<PassKind>(Name)
#define PASS(ID, NAME, DESCRIPTION) .Case(#ID, PassKind::ID)
#include "swift/SILOptimizer/PassManager/Passes.def"
      .Default(PassKind::invalidPassKind);
}

StringRef swift::PassKindName(PassKind Kind) {
  switch (Kind) {
#define PASS(ID, NAME, DESCRIPTION)                                            \
  case PassKind::ID:                                                           \
    return #NAME;
#include "swift/SILOptimizer/PassManager/Passes.def"
  case PassKind::invalidPassKind:
    llvm_unreachable("Invalid pass kind?!");
  }

  llvm_unreachable("Unhandled PassKind in switch.");
}

StringRef swift::PassKindID(PassKind Kind) {
  switch (Kind) {
#define PASS(ID, NAME, DESCRIPTION)                                            \
  case PassKind::ID:                                                           \
    return #ID;
#include "swift/SILOptimizer/PassManager/Passes.def"
  case PassKind::invalidPassKind:
    llvm_unreachable("Invalid pass kind?!");
  }

  llvm_unreachable("Unhandled PassKind in switch.");
}
