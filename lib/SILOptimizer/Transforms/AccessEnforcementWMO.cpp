//===--- AccessEnforcementWMO.cpp - Whole-module access enforcement opt ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This module pass removes dynamic access enforcement based on whole module
/// information.
///
/// This maps each access of identified storage onto a disjoint access
/// location. Local accesses (Box and Stack) already have unique AccessedStorage
/// and should be removed by an earlier function pass if possible. This pass
/// handles Class and Global access by partitioning their non-unique
/// AccessedStorage objects into unique DisjointAccessLocations. These disjoint
/// access locations may be accessed across multiple functions, so a module pass
/// is required to identify and optimize them.
///
/// Class accesses are partitioned by their fully qualified property
/// name. Global accesses are partitioned by the global variable name. Argument
/// accesses are ignored because they are considered an access in the caller,
/// while Class and Global access always occurs in the callee. Note that a SIL
/// function argument value may be the source of a either a Class-kind
/// AccessedStorage or an Argument-kind AccessedStorage. When the argument is
/// used to access a class, it will always be identified as a Class-kind
/// AccessedStorage even though its source is an argument.
///
/// For each function, discover the disjointly accessed locations. Each location
/// is optimistically mapped to a `noNestedConflict` flag (if a location has not
/// been mapped, then it is assumed not to have nested conflict). Each location
/// without any nested conflict maps to a set of accesses that the optimization
/// may be able to remove.
///
/// After analyzing all functions in the module, disable any class property and
/// global variable access that still have not seen any nested conflicts by
/// giving them static enforcement.
///
/// Warning: This is only sound when unidentified accesses can never alias with
/// Class/Global access. To enforce this, the SILVerifier calls
/// findAccessedStorage() for every access, which asserts that any Unidentified
/// access belongs to a know pattern that cannot originate from Class or Global
/// accesses.
///
/// Note: This optimization must be aware of all possible access to a Class or
/// Global address. This includes unpaired access instructions and keypath
/// instructions. Ignoring any access pattern would weaken enforcement. For
/// example, AccessedStorageAnalysis cannot be used here because that analysis
/// may conservatively summarize some functions.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-enforcement-wmo"

#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

using namespace swift;

using llvm::DenseMap;
using llvm::SmallDenseSet;

// Get the VarDecl that represents the DisjointAccessLocation for the given
// AccessedStorage. Returns nullptr for any storage that can't be partitioned
// into a disjoint location.
//
// findAccessedStorage may only return Unidentified storage for a global
// variable access if the global is defined in a different module.
//
// WARNING: Retrieving VarDecl for Class access is not constant time.
const VarDecl *getDisjointAccessLocation(const AccessedStorage &storage) {
  switch (storage.getKind()) {
  case AccessedStorage::Global:
    // A global variable may return a null decl. These variables are
    // implementation details that aren't formally accessed.
    return storage.getGlobal()->getDecl();
  case AccessedStorage::Class: {
    return cast<VarDecl>(storage.getDecl());
  }
  case AccessedStorage::Box:
  case AccessedStorage::Stack:
  case AccessedStorage::Argument:
  case AccessedStorage::Yield:
  case AccessedStorage::Unidentified:
    return nullptr;
  case AccessedStorage::Nested:
    llvm_unreachable("Unexpected Nested access.");
  }
  llvm_unreachable("unhandled kind");
}

namespace {
// Implements an optimization to remove access markers on disjoint memory
// locations that are never reentrantly accessed. For a given memory location,
// if there are no potential nested conflicts, then enforcement must succeed for
// any access to that location.
//
// The existence of unidentified access complicates this problem. For this
// optimization to be valid, Global and Class property access must always be
// identifiable. findAccessedStorage() in MemAccessUtils enforces a short list
// of unidentified producers (non-address PhiArgument, PointerToAddress, Undef,
// & local-init). We cannot allow the address of a global variable or class
// property to be exposed via one of these instructions, unless the declaration
// is considered "visible externally".
//
// Note: This assumes that PointerToAddress is never used to access a global or
// class property unless it's done via an Unsafe API that doesn't need
// enforcement.
//
// FIXME: Tail-allocated heap storage is currently categorized as
// Unidentified. This is conservatively safe because it can't also be accessed
// as a class property. However, it is inconsistent with the general rule that
// only local storage is unidentified, and we can probably remove a lot of
// accesses by being more precise here (OTOH, if this is known CoW storage, we
// should eventually add an even stronger optimization to remove exclusivity
// checks).
class GlobalAccessRemoval {
  SILModule &module;

  using BeginAccessSet = SmallDenseSet<BeginAccessInst *, 8>;

  /// Information for an access location that, if it is valid, must be disjoint
  /// from all other access locations.
  struct DisjointAccessLocationInfo {
    AccessedStorage::Kind accessKind = AccessedStorage::Unidentified;
    // False if any nested conflict has been seen at this location.
    bool noNestedConflict = true;
    BeginAccessSet beginAccessSet;
  };

  DenseMap<const VarDecl *, DisjointAccessLocationInfo> disjointAccessMap;

public:
  GlobalAccessRemoval(SILModule &module) : module(module) {}

  void perform();

protected:
  void visitInstruction(SILInstruction *I);
  void recordAccess(SILInstruction *beginAccess, const VarDecl *decl,
                    AccessedStorage::Kind storageKind,
                    bool hasNoNestedConflict);
  void removeNonreentrantAccess();
};
} // namespace

/// Process all begin_access instructions in the module, recording their
/// DisjointAccessLocation. Ignore accesses at call sites because Arguments and
/// Unidentified access can only local storage.
void GlobalAccessRemoval::perform() {
  for (auto &F : module) {
    if (F.empty())
      continue;

    for (auto &BB : F) {
      for (auto &I : BB)
        visitInstruction(&I);
    }
  }
  removeNonreentrantAccess();
}

void GlobalAccessRemoval::visitInstruction(SILInstruction *I) {
  if (auto *BAI = dyn_cast<BeginAccessInst>(I)) {
    AccessedStorage storage = findAccessedStorageNonNested(BAI->getSource());
    const VarDecl *decl = getDisjointAccessLocation(storage);
    recordAccess(BAI, decl, storage.getKind(), BAI->hasNoNestedConflict());
    return;
  }
  if (auto *BUAI = dyn_cast<BeginUnpairedAccessInst>(I)) {
    AccessedStorage storage = findAccessedStorageNonNested(BUAI->getSource());
    const VarDecl *decl = getDisjointAccessLocation(storage);
    recordAccess(BUAI, decl, storage.getKind(), BUAI->hasNoNestedConflict());
    return;
  }
  if (auto *KPI = dyn_cast<KeyPathInst>(I)) {
    for (const KeyPathPatternComponent &component :
         KPI->getPattern()->getComponents()) {
      switch (component.getKind()) {
      case KeyPathPatternComponent::Kind::StoredProperty:
        recordAccess(KPI, component.getStoredPropertyDecl(),
                     AccessedStorage::Class, /*hasNoNestedConflict=*/false);
        break;
      case KeyPathPatternComponent::Kind::GettableProperty:
      case KeyPathPatternComponent::Kind::SettableProperty:
      case KeyPathPatternComponent::Kind::OptionalChain:
      case KeyPathPatternComponent::Kind::OptionalForce:
      case KeyPathPatternComponent::Kind::OptionalWrap:
      case KeyPathPatternComponent::Kind::TupleElement:
        break;
      }
    }
    return;
  }
}

// Record an access in the disjointAccessMap.
//
// `beginAccess` is any instruction that represents a potential access,
// including key_path. One of the following conditions must be true for every
// begin_[unpaired_]access instructions:
// - it is guaranteed to access local memory, not a class property or global.
// - it has an identifiable source.
// - is is generated from a Builtin, which is assumed to have an associated
// key_path instruction somewhere else in the same module (or it must be dead
// code, or only access public properties).
//
// `decl` may be nullptr if the declaration can't be determined from the
// access. This is only legal when the access is known to be a local access, not
// a class property or global.
void GlobalAccessRemoval::recordAccess(SILInstruction *beginAccess,
                                       const VarDecl *decl,
                                       AccessedStorage::Kind storageKind,
                                       bool hasNoNestedConflict) {
  if (!decl || module.isVisibleExternally(decl))
    return;

  LLVM_DEBUG(if (!hasNoNestedConflict) llvm::dbgs()
             << "Nested conflict on " << decl->getName() << " at"
             << *beginAccess << "\n");

  auto accessLocIter = disjointAccessMap.find(decl);
  if (accessLocIter != disjointAccessMap.end()) {
    // Add this begin_access to an existing DisjointAccessLocationInfo.
    DisjointAccessLocationInfo &info = accessLocIter->second;
    assert(info.accessKind == storageKind);
    info.noNestedConflict &= hasNoNestedConflict;
    // Don't currently optimize unpaired access.
    if (auto *BAI = dyn_cast<BeginAccessInst>(beginAccess))
      info.beginAccessSet.insert(BAI);
    return;
  }

  // Create the first DisjointAccessLocationInfo for this begin_access.
  DisjointAccessLocationInfo info;
  info.accessKind = storageKind;
  info.noNestedConflict = hasNoNestedConflict;
  if (auto *BA = dyn_cast<BeginAccessInst>(beginAccess))
    info.beginAccessSet.insert(BA);
  disjointAccessMap.insert(std::make_pair(decl, info));
}

// For each unique storage within this function that is never reentrantly
// accessed, promote all access checks for that storage to static enforcement.
void GlobalAccessRemoval::removeNonreentrantAccess() {
  for (auto &declAndInfo : disjointAccessMap) {
    const DisjointAccessLocationInfo &info = declAndInfo.second;
    if (!info.noNestedConflict)
      continue;

    const VarDecl *decl = declAndInfo.first;
    LLVM_DEBUG(llvm::dbgs() << "Eliminating all formal access on "
                            << decl->getName() << "\n");
    assert(!module.isVisibleExternally(decl));
    (void)decl;

    // Non-deterministic iteration, only used to set a flag.
    for (BeginAccessInst *beginAccess : info.beginAccessSet) {
      LLVM_DEBUG(llvm::dbgs() << "  Disabling access marker " << *beginAccess);
      beginAccess->setEnforcement(SILAccessEnforcement::Static);
    }
  }
}

namespace {
struct AccessEnforcementWMO : public SILModuleTransform {
  void run() override {
    GlobalAccessRemoval eliminationPass(*getModule());
    eliminationPass.perform();
  }
};
}

SILTransform *swift::createAccessEnforcementWMO() {
  return new AccessEnforcementWMO();
}
