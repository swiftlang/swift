//===--- AccessMarkerElimination.cpp - Eliminate access markers. ----------===//
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
/// This pass eliminates the instructions that demarcate memory access regions.
/// If no memory access markers exist, then the pass does nothing. Otherwise, it
/// unconditionally eliminates all non-dynamic markers (plus any dynamic markers
/// if dynamic exclusivity checking is disabled).
/// 
/// This is an always-on pass for temporary bootstrapping. It allows running
/// test cases through the pipeline and exercising SIL verification before all
/// passes support access markers.
///
/// This must only run before inlining _semantic calls. If we inline and drop
/// the @_semantics("optimize.sil.preserve_exclusivity") attribute, the inlined
/// markers will be eliminated, but the noninlined markers will not. This would
/// result in inconsistent begin/end_unpaired_access resulting in unpredictable,
/// potentially catastrophic runtime behavior.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-marker-elim"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/Strings.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

// This temporary option allows markers during optimization passes. Enabling
// this flag causes this pass to preserve only dynamic checks when dynamic
// checking is enabled. Otherwise, this pass removes all checks.
//
// This is currently unsupported because tail duplication results in
// address-type block arguments.
llvm::cl::opt<bool> EnableOptimizedAccessMarkers(
    "sil-optimized-access-markers", llvm::cl::init(true),
    llvm::cl::desc("Enable memory access markers during optimization passes."));

namespace {

struct AccessMarkerElimination {
  SILModule *Mod;
  SILFunction *F;

  bool removedAny = false;

  AccessMarkerElimination(SILFunction *F)
      : Mod(&F->getModule()), F(F) {}

  SILBasicBlock::iterator eraseInst(SILInstruction *inst) {
    DEBUG(llvm::dbgs() << "Erasing access marker: " << *inst);
    removedAny = true;
    return inst->getParent()->erase(inst);
  };

  void replaceBeginAccessUsers(BeginAccessInst *beginAccess);

  bool shouldPreserveAccess(SILAccessEnforcement enforcement);

  // Check if the instruction is a marker that should be eliminated. If so,
  // updated the SIL, short of erasing the marker itself, and return true.
  bool checkAndEliminateMarker(SILInstruction *inst);

  // Entry point called either by the pass by the same name
  // or as a utility (e.g. during deserialization).
  bool stripMarkers();
};

void AccessMarkerElimination::replaceBeginAccessUsers(
    BeginAccessInst *beginAccess) {
  // Handle all the uses:
  while (!beginAccess->use_empty()) {
    Operand *op = *beginAccess->use_begin();

    // Delete any associated end_access instructions.
    if (auto endAccess = dyn_cast<EndAccessInst>(op->getUser())) {
      endAccess->eraseFromParent();

      // Forward all other uses to the original address.
    } else {
      op->set(beginAccess->getSource());
    }
  }
}

bool AccessMarkerElimination::shouldPreserveAccess(
    SILAccessEnforcement enforcement) {
  if (!EnableOptimizedAccessMarkers)
    return false;

  switch (enforcement) {
  case SILAccessEnforcement::Static:
  case SILAccessEnforcement::Unsafe:
    return false;
  case SILAccessEnforcement::Unknown:
  case SILAccessEnforcement::Dynamic:
    return Mod->getOptions().EnforceExclusivityDynamic;
  }
}

// Check if the instruction is a marker that should be eliminated. If so,
// updated the SIL, short of erasing the marker itself, and return true.
bool AccessMarkerElimination::checkAndEliminateMarker(SILInstruction *inst) {
  if (auto beginAccess = dyn_cast<BeginAccessInst>(inst)) {
    // Leave dynamic accesses in place, but delete all others.
    if (shouldPreserveAccess(beginAccess->getEnforcement()))
      return false;

    replaceBeginAccessUsers(beginAccess);
    return true;
  }

  // end_access instructions will be handled when we process the
  // begin_access.

  // begin_unpaired_access instructions will be directly removed and
  // simply replaced with their operand.
  if (auto BUA = dyn_cast<BeginUnpairedAccessInst>(inst)) {
    if (shouldPreserveAccess(BUA->getEnforcement()))
      return false;

    return true;
  }
  // end_unpaired_access instructions will be directly removed and
  // simply replaced with their operand.
  if (auto EUA = dyn_cast<EndUnpairedAccessInst>(inst)) {
    if (shouldPreserveAccess(EUA->getEnforcement()))
      return false;

    return true;
  }
  return false;
}

// Top-level per-function entry-point.
// Return `true` if any markers were removed.
bool AccessMarkerElimination::stripMarkers() {
  // Iterating in reverse eliminates more begin_access users before they
  // need to be replaced.
  for (auto &BB : reversed(*F)) {
    // Don't cache the begin iterator since we're reverse iterating.
    for (auto II = BB.end(); II != BB.begin();) {
      SILInstruction *inst = &*(--II);
      if (checkAndEliminateMarker(inst))
        II = eraseInst(inst);
    }
  }
  return removedAny;
}

} // end anonymous namespace

// Implement a SILModule::SILFunctionBodyCallback that strips all access
// markers from newly deserialized function bodies.
static void prepareSILFunctionForOptimization(ModuleDecl *, SILFunction *F) {
  DEBUG(llvm::dbgs() << "Stripping all markers in: " << F->getName() << "\n");

  AccessMarkerElimination(F).stripMarkers();
}

namespace {

struct AccessMarkerEliminationPass : SILModuleTransform {
  void run() override {
    auto &M = *getModule();
    for (auto &F : M) {
      if (F.hasSemanticsAttr(OPTIMIZE_SIL_PRESERVE_EXCLUSIVITY)) {
        DEBUG(llvm::dbgs() << "Skipping " << F.getName() << ". Found "
                           << OPTIMIZE_SIL_PRESERVE_EXCLUSIVITY << " tag!\n");
        continue;
      }

      bool removedAny = AccessMarkerElimination(&F).stripMarkers();

      // Only invalidate analyses if we removed some markers.
      if (removedAny) {
        auto InvalidKind = SILAnalysis::InvalidationKind::Instructions;
        invalidateAnalysis(&F, InvalidKind);
      }

      // Markers from all current SIL functions are stripped. Register a
      // callback to strip an subsequently loaded functions on-the-fly.
      if (!EnableOptimizedAccessMarkers)
        M.registerDeserializationCallback(prepareSILFunctionForOptimization);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessMarkerElimination() {
  return new AccessMarkerEliminationPass();
}
