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
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-marker-elim"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

// This option allows markers to remain in -Onone as a structural SIL property.
// Regardless of this option, sufficient markers are always emitted to satisfy
// the current enforcement level. This options simply allows markers to remain
// for testing and validation.
//
// This option only applies to InactiveAccessMarkerElimination. Any occurrence
// of FullAccessMarkerElimination in the pass pipeline effectively overrides the
// options and removes all markers.
//
// At -Onone, with EnableMarkers, no static markers are removed.
// With !EnableMarkers:
// Enforcement | Static               | Dynamic
// none        | Remove after Diag    | Remove ASAP
// unchecked   | Remain through IRGen | Remove ASAP
// checked     | Remain through IRGen | Remain through IRGen
// dynamic-only| Remove after Diag    | Remain through IRGen
llvm::cl::opt<bool> EnableAccessMarkers(
    "sil-access-markers", llvm::cl::init(true),
    llvm::cl::desc("Enable memory access makers that aren't needed for "
                   "diagnostics."));

namespace {

struct AccessMarkerElimination {
  SILModule *Mod;
  SILFunction *F;
  bool isFullElimination;

  bool removedAny = false;

  AccessMarkerElimination(SILFunction *F, bool isFullElimination)
      : Mod(&F->getModule()), F(F), isFullElimination(isFullElimination) {}

  SILBasicBlock::iterator eraseInst(SILInstruction *inst) {
    DEBUG(llvm::dbgs() << "Erasing access marker: " << *inst);
    removedAny = true;
    return inst->getParent()->erase(inst);
  };

  void replaceBeginAccessUsers(BeginAccessInst *beginAccess);

  // Precondition: !EnableAccessMarkers || isFullElimination
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
      assert(endAccess->use_empty() && "found use of end_access");
      endAccess->eraseFromParent();

      // Forward all other uses to the original address.
    } else {
      op->set(beginAccess->getSource());
    }
  }
}

// Precondition: !EnableAccessMarkers || isFullElimination
bool AccessMarkerElimination::shouldPreserveAccess(
    SILAccessEnforcement enforcement) {
  if (isFullElimination)
    return false;

  switch (enforcement) {
  case SILAccessEnforcement::Unknown:
    return false;
  case SILAccessEnforcement::Static:
    // Even though static enforcement is already performed, this flag is
    // useful to control marker preservation for now.
    return EnableAccessMarkers || Mod->getOptions().EnforceExclusivityStatic;
  case SILAccessEnforcement::Dynamic:
    // FIXME: when dynamic markers are fully supported, don't strip:
    //   return
    //     EnableAccessMarkers || Mod->getOptions().EnforceExclusivityDynamic;
    return Mod->getOptions().EnforceExclusivityDynamic;
  case SILAccessEnforcement::Unsafe:
    return false;
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

    BUA->replaceAllUsesWith(BUA->getSource());
    return true;
  }
  // end_unpaired_access instructions will be directly removed and
  // simply replaced with their operand.
  if (auto EUA = dyn_cast<EndUnpairedAccessInst>(inst)) {
    if (shouldPreserveAccess(EUA->getEnforcement()))
      return false;

    assert(EUA->use_empty() && "use of end_unpaired_access");
    return true;
  }
  return false;
}

// Top-level per-function entry-point.
// Return `true` if any markers were removed.
bool AccessMarkerElimination::stripMarkers() {
  // FIXME: When dynamic markers are fully supported, just skip this pass:
  //   if (EnableAccessMarkers && !isFullElimination)
  //     return false;

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

  AccessMarkerElimination(F, /*isFullElimination=*/true).stripMarkers();
}

namespace {

struct AccessMarkerEliminationPass : SILModuleTransform {
  virtual bool isFullElimination() = 0;

  void run() override {
    auto &M = *getModule();
    for (auto &F : M) {
      bool removedAny = AccessMarkerElimination(&F, isFullElimination())
                            .stripMarkers();

      // Only invalidate analyses if we removed some markers.
      if (removedAny) {
        auto InvalidKind = SILAnalysis::InvalidationKind::Instructions;
        invalidateAnalysis(&F, InvalidKind);
      }

      // Markers from all current SIL functions are stripped. Register a
      // callback to strip an subsequently loaded functions on-the-fly.
      if (isFullElimination())
        M.registerDeserializationCallback(prepareSILFunctionForOptimization);
    }
  }
};

struct InactiveAccessMarkerElimination : AccessMarkerEliminationPass {
  virtual bool isFullElimination() { return false; }
};

struct FullAccessMarkerElimination : AccessMarkerEliminationPass {
  virtual bool isFullElimination() { return true; }
};

} // end anonymous namespace

SILTransform *swift::createInactiveAccessMarkerElimination() {
  return new InactiveAccessMarkerElimination();
}

SILTransform *swift::createFullAccessMarkerElimination() {
  return new FullAccessMarkerElimination();
}
