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
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-marker-elim"
#include "swift/Basic/Range.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

// This temporary option allows markers during optimization passes. Enabling
// this flag causes this pass to preserve all access markers. Otherwise, it only
// preserved "dynamic" markers.
llvm::cl::opt<bool> EnableOptimizedAccessMarkers(
    "sil-optimized-access-markers", llvm::cl::init(false),
    llvm::cl::desc("Enable memory access markers during optimization passes."));

namespace {

struct AccessMarkerElimination {
  SILModule *Mod;
  SILFunction *F;

  bool removedAny = false;

  AccessMarkerElimination(SILFunction *F)
      : Mod(&F->getModule()), F(F) {}

  void notifyErased(SILInstruction *inst) {
    LLVM_DEBUG(llvm::dbgs() << "Erasing access marker: " << *inst);
    removedAny = true;
  }

  SILBasicBlock::iterator eraseInst(SILInstruction *inst) {
    auto nextIter = std::next(inst->getIterator());
    notifyErased(inst);
    return nextIter;
  };

  bool shouldPreserveAccess(SILAccessEnforcement enforcement);

  // Check if the instruction is a marker that should be eliminated. If so,
  // updated the SIL, short of erasing the marker itself, and return true.
  SILBasicBlock::iterator checkAndEliminateMarker(SILInstruction *inst);

  // Entry point called either by the pass by the same name
  // or as a utility (e.g. during deserialization).
  bool stripMarkers();
};

bool AccessMarkerElimination::shouldPreserveAccess(
    SILAccessEnforcement enforcement) {
  if (EnableOptimizedAccessMarkers || Mod->getOptions().VerifyExclusivity)
    return true;

  switch (enforcement) {
  case SILAccessEnforcement::Static:
  case SILAccessEnforcement::Unsafe:
    return false;
  // Signed access should be preserved until IRGen
  case SILAccessEnforcement::Signed:
    return true;
  case SILAccessEnforcement::Unknown:
  case SILAccessEnforcement::Dynamic:
    return Mod->getOptions().EnforceExclusivityDynamic;
  }
  llvm_unreachable("unhandled enforcement");
}

// Check if the instruction is a marker that should be eliminated. If so, delete
// the begin_access along with all associated end_access and a valid instruction
// iterator pointing to the first remaining instruction following the
// begin_access. If the marker is not eliminated, return an iterator pointing to
// the marker.
SILBasicBlock::iterator
AccessMarkerElimination::checkAndEliminateMarker(SILInstruction *inst) {
  if (auto beginAccess = dyn_cast<BeginAccessInst>(inst)) {
    // Builtins used by the standard library must emit markers regardless of the
    // current compiler options so that any user code that initiates access via
    // the standard library is fully enforced.
    if (beginAccess->isFromBuiltin())
      return inst->getIterator();

    // Leave dynamic and signed accesses in place, but delete all others.
    if (shouldPreserveAccess(beginAccess->getEnforcement()))
      return inst->getIterator();

    notifyErased(beginAccess);
    return removeBeginAccess(beginAccess);
  }

  // end_access instructions will be handled when we process the
  // begin_access.

  // begin_unpaired_access instructions will be directly removed and
  // simply replaced with their operand.
  if (auto BUA = dyn_cast<BeginUnpairedAccessInst>(inst)) {
    // Builtins used by the standard library must emit markers regardless of the
    // current compiler options.
    if (BUA->isFromBuiltin())
      return inst->getIterator();

    if (shouldPreserveAccess(BUA->getEnforcement()))
      return inst->getIterator();

    return eraseInst(BUA);
  }
  // end_unpaired_access instructions will be directly removed and
  // simply replaced with their operand.
  if (auto EUA = dyn_cast<EndUnpairedAccessInst>(inst)) {
    // Builtins used by the standard library must emit markers regardless of the
    // current compiler options.
    if (EUA->isFromBuiltin())
      return inst->getIterator();

    if (shouldPreserveAccess(EUA->getEnforcement()))
      return inst->getIterator();

    return eraseInst(EUA);
  }
  return inst->getIterator();
}

// Top-level per-function entry-point.
// Return `true` if any markers were removed.
bool AccessMarkerElimination::stripMarkers() {
  // Iterating in reverse eliminates more begin_access users before they
  // need to be replaced.
  for (auto &BB : llvm::reverse(*F)) {
    // Don't cache the begin iterator since we're reverse iterating.
    for (auto II = BB.end(); II != BB.begin();) {
      SILInstruction *inst = &*(--II);
      // checkAndEliminateMarker returns the next non-deleted instruction. The
      // following iteration moves the iterator backward.
      II = checkAndEliminateMarker(inst);
    }
  }
  return removedAny;
}

} // end anonymous namespace

// Implement a SILModule::SILFunctionBodyCallback that strips all access
// markers from newly deserialized function bodies.
static void prepareSILFunctionForOptimization(ModuleDecl *, SILFunction *F) {
  LLVM_DEBUG(llvm::dbgs() << "Stripping all markers in: " << F->getName()
                          << "\n");

  AccessMarkerElimination(F).stripMarkers();
}

namespace {

struct AccessMarkerEliminationPass : SILModuleTransform {
  void run() override {
    auto &M = *getModule();
    for (auto &F : M) {
      bool removedAny = AccessMarkerElimination(&F).stripMarkers();

      // Only invalidate analyses if we removed some markers.
      if (removedAny) {
        auto InvalidKind = SILAnalysis::InvalidationKind::Instructions;
        invalidateAnalysis(&F, InvalidKind);
      }
    }
    // Markers from all current SIL functions are stripped. Register a
    // callback to strip an subsequently loaded functions on-the-fly.
    if (!EnableOptimizedAccessMarkers && !M.checkHasAccessMarkerHandler()) {
      using NotificationHandlerTy =
        FunctionBodyDeserializationNotificationHandler;
      auto *n = new NotificationHandlerTy(prepareSILFunctionForOptimization);
      std::unique_ptr<DeserializationNotificationHandler> ptr(n);
      M.registerDeserializationNotificationHandler(std::move(ptr));
      M.setHasAccessMarkerHandler();
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessMarkerElimination() {
  return new AccessMarkerEliminationPass();
}
