//===--- AccessedStorageAnalysis.cpp  - Accessed Storage Analysis ---------===//
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

#define DEBUG_TYPE "sil-sea"

#include "swift/SILOptimizer/Analysis/AccessedStorageAnalysis.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"

using namespace swift;

// -----------------------------------------------------------------------------
// MARK: Accessing the results.
// -----------------------------------------------------------------------------

bool FunctionAccessedStorage::hasNoNestedConflict(
    const AccessedStorage &otherStorage) const {
  assert(otherStorage.isUniquelyIdentified());
  assert(!hasUnidentifiedAccess());

  return getStorageAccessInfo(otherStorage).hasNoNestedConflict();
}

bool FunctionAccessedStorage::mayConflictWith(
    SILAccessKind otherAccessKind, const AccessedStorage &otherStorage) const {
  if (hasUnidentifiedAccess()
      && accessKindMayConflict(otherAccessKind,
                               unidentifiedAccess.getValue())) {
    return true;
  }
  for (auto &storageAccess : storageAccessSet) {
    assert(storageAccess && "FunctionAccessedStorage mapped invalid storage.");

    if (!accessKindMayConflict(otherAccessKind, storageAccess.getAccessKind()))
      continue;

    if (!otherStorage.isDistinctFrom(storageAccess))
      return true;
  }
  return false;
}

StorageAccessInfo FunctionAccessedStorage::getStorageAccessInfo(
    const AccessedStorage &otherStorage) const {
  // Construct a fake StorageAccessInfo to do a hash lookup for the real
  // StorageAccessInfo. The DenseSet key is limited to the AccessedStorage base
  // class members.
  StorageAccessInfo storageKey(otherStorage, SILAccessKind::Read, false);
  auto iter = storageAccessSet.find(storageKey);
  assert(iter != storageAccessSet.end());
  return *iter;
}

// -----------------------------------------------------------------------------
// MARK: Constructing the results.
// -----------------------------------------------------------------------------

static bool updateAccessKind(SILAccessKind &LHS, SILAccessKind RHS) {
  bool changed = false;
  // Assume we don't track Init/Deinit.
  if (LHS == SILAccessKind::Read && RHS == SILAccessKind::Modify) {
    LHS = RHS;
    changed = true;
  }
  return changed;
}

static bool updateOptionalAccessKind(Optional<SILAccessKind> &LHS,
                                     Optional<SILAccessKind> RHS) {
  if (RHS == None)
    return false;

  if (LHS == None) {
    LHS = RHS;
    return true;
  }
  return updateAccessKind(LHS.getValue(), RHS.getValue());
}

bool StorageAccessInfo::mergeFrom(const StorageAccessInfo &RHS) {
  bool changed = false;
  SILAccessKind accessKind = getAccessKind();
  assert(accessKind == SILAccessKind::Read
         || accessKind == SILAccessKind::Modify && "uninitialized info");
  if (updateAccessKind(accessKind, RHS.getAccessKind())) {
    setAccessKind(accessKind);
    changed = true;
  }
  if (hasNoNestedConflict() && !RHS.hasNoNestedConflict()) {
    setNoNestedConflict(false);
    changed = true;
  }
  return changed;
}

bool FunctionAccessedStorage::summarizeFunction(SILFunction *F) {
  assert(storageAccessSet.empty() && "expected uninitialized results.");

  if (F->isDefinition())
    return false;

  // If the function definition is unavailable, set unidentifiedAccess to a
  // conservative value, since analyzeInstruction will never be called.
  //
  // If FunctionSideEffects can be summarized, use that information.
  FunctionSideEffects functionSideEffects;
  if (!functionSideEffects.summarizeFunction(F)) {
    setWorstEffects();
    // May as well consider this a successful summary since there are no
    // instructions to visit anyway.
    return true;
  }
  bool mayRead = functionSideEffects.getGlobalEffects().mayRead();
  bool mayWrite = functionSideEffects.getGlobalEffects().mayWrite();
  for (auto &paramEffects : functionSideEffects.getParameterEffects()) {
    mayRead |= paramEffects.mayRead();
    mayWrite |= paramEffects.mayWrite();
  }
  if (mayWrite)
    unidentifiedAccess = SILAccessKind::Modify;
  else if (mayRead)
    unidentifiedAccess = SILAccessKind::Read;

  // If function side effects is "readnone" then this result will have an empty
  // storageAccessSet and unidentifiedAccess == None.
  return true;
}

bool FunctionAccessedStorage::updateUnidentifiedAccess(
    SILAccessKind accessKind) {
  if (unidentifiedAccess == None) {
    unidentifiedAccess = accessKind;
    return true;
  }
  return updateAccessKind(unidentifiedAccess.getValue(), accessKind);
}

// Merge the given FunctionAccessedStorage in `other` into this
// FunctionAccessedStorage. Use the given `transformStorage` to map `other`
// AccessedStorage into this context. If `other` is from a callee, argument
// substitution will be performed if possible. However, there's no guarantee
// that the merged access values will belong to this function.
//
// Note that we may have `this` == `other` for self-recursion. We still need to
// propagate and merge in that case in case arguments are recursively dependent.
bool FunctionAccessedStorage::mergeAccesses(
    const FunctionAccessedStorage &other,
    std::function<StorageAccessInfo(const StorageAccessInfo &)>
      transformStorage) {

  // Insertion in DenseMap invalidates the iterator in the rare case of
  // self-recursion (`this` == `other`) that passes accessed storage though an
  // argument. Rather than complicate the code, make a temporary copy of the
  // AccessedStorage.
  //
  // Also note that the storageAccessIndex from otherStorage is relative to its
  // original context and should not be copied into this context.
  SmallVector<StorageAccessInfo, 8> otherStorageAccesses;
  otherStorageAccesses.reserve(other.storageAccessSet.size());
  otherStorageAccesses.append(other.storageAccessSet.begin(),
                              other.storageAccessSet.end());

  bool changed = false;
  for (auto &rawStorageInfo : otherStorageAccesses) {
    const StorageAccessInfo &otherStorageInfo =
      transformStorage(rawStorageInfo);
    // If transformStorage() returns invalid storage object for local storage,
    // that should not be merged with the caller.
    if (!otherStorageInfo)
      continue;

    if (otherStorageInfo.getKind() == AccessedStorage::Unidentified) {
      changed |= updateUnidentifiedAccess(otherStorageInfo.getAccessKind());
      continue;
    }
    // Attempt to add identified AccessedStorage to this map.
    auto result = insertStorageAccess(otherStorageInfo);
    if (result.second) {
      // A new AccessedStorage key was added to this map.
      changed = true;
      continue;
    }
    // Merge StorageAccessInfo into already-mapped AccessedStorage.
    changed |= result.first->mergeFrom(otherStorageInfo);
  }
  if (other.unidentifiedAccess != None)
    changed |= updateUnidentifiedAccess(other.unidentifiedAccess.getValue());

  return changed;
}

bool FunctionAccessedStorage::mergeFrom(const FunctionAccessedStorage &other) {
  // Merge accesses from other. Both `this` and `other` are either from the same
  // function or are both callees of the same call site, so their parameters
  // indices coincide. transformStorage is the identity function.
  return mergeAccesses(other, [](const StorageAccessInfo &s) { return s; });
}

/// Returns the argument of the full apply or partial apply corresponding to the
/// callee's parameter index, or returns an invalid SILValue if the applied
/// closure cannot be found. This walks up the apply chain starting at the given
/// `fullApply` to find the applied argument.
static SILValue getCallerArg(FullApplySite fullApply, unsigned paramIndex) {
  if (paramIndex < fullApply.getNumArguments())
    return fullApply.getArgument(paramIndex);

  SILValue callee = fullApply.getCalleeOrigin();
  auto *PAI = dyn_cast<PartialApplyInst>(callee);
  if (!PAI)
    return SILValue();

  unsigned appliedIndex =
    paramIndex - ApplySite(PAI).getCalleeArgIndexOfFirstAppliedArg();
  if (appliedIndex < PAI->getNumArguments())
    return PAI->getArgument(appliedIndex);

  // This must be a chain of partial_applies. We don't expect this in practice,
  // so handle it conservatively.
  return SILValue();
}

/// Transform AccessedStorage from a callee into the caller context. If this is
/// uniquely identified local storage, then return an invalid storage object.
///
/// For correctness, AccessEnforcementOpts relies on all Argument access to
/// either be mapped into the caller's context or marked as an unidentified
/// access at the call site.
///
/// Note: This does *not* map the storage index into the caller function's index
/// range. (When the storage value doesn't need to be remapped, it returns the
/// original storage value.) It's simpler to set the storage index later when it
/// is actually added to the function's storageAccessSet.
static StorageAccessInfo
transformCalleeStorage(const StorageAccessInfo &storage,
                       FullApplySite fullApply) {
  switch (storage.getKind()) {
  case AccessedStorage::Box:
  case AccessedStorage::Stack:
    // Do not merge local storage.
    return StorageAccessInfo(AccessedStorage(), storage);
  case AccessedStorage::Global:
    // Global accesses is universal.
    return storage;
  case AccessedStorage::Class: {
    // If the object's value is an argument, translate it into a value on the
    // caller side.
    SILValue obj = storage.getObjectProjection().getObject();
    if (auto *arg = dyn_cast<SILFunctionArgument>(obj)) {
      SILValue argVal = getCallerArg(fullApply, arg->getIndex());
      if (argVal) {
        auto &proj = storage.getObjectProjection().getProjection();
        // Remap the argument source value and inherit the old storage info.
        return StorageAccessInfo(AccessedStorage(argVal, proj), storage);
      }
    }
    // Otherwise, continue to reference the value in the callee because we don't
    // have any better placeholder for a callee-defined object.
    return storage;
  }
  case AccessedStorage::Argument: {
    // Transitively search for the storage base in the caller.
    SILValue argVal = getCallerArg(fullApply, storage.getParamIndex());
    if (argVal) {
      // Remap the argument source value and inherit the old storage info.
      auto calleeStorage = findAccessedStorageNonNested(argVal);
      if (calleeStorage)
        return StorageAccessInfo(calleeStorage, storage);
    }
    // If the argument can't be transformed, demote it to an unidentified
    // access.
    return StorageAccessInfo(
      AccessedStorage(storage.getValue(), AccessedStorage::Unidentified),
      storage);
  }
  case AccessedStorage::Nested:
    llvm_unreachable("Unexpected nested access");
  case AccessedStorage::Unidentified:
    // For unidentified storage, continue to reference the value in the callee
    // because we don't have any better placeholder for a callee-defined object.
    return storage;
  }
}

bool FunctionAccessedStorage::mergeFromApply(
    const FunctionAccessedStorage &calleeAccess, FullApplySite fullApply) {
  // Merge accesses from calleeAccess. Transform any Argument type
  // AccessedStorage into the caller context to be added to `this` storage map.
  return mergeAccesses(calleeAccess, [&fullApply](const StorageAccessInfo &s) {
    return transformCalleeStorage(s, fullApply);
  });
}

template <typename B>
void FunctionAccessedStorage::visitBeginAccess(B *beginAccess) {
  if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic)
    return;

  const AccessedStorage &storage =
      findAccessedStorageNonNested(beginAccess->getSource());

  if (storage.getKind() == AccessedStorage::Unidentified) {
    // This also catches invalid storage.
    updateOptionalAccessKind(unidentifiedAccess, beginAccess->getAccessKind());
    return;
  }
  StorageAccessInfo storageAccess(storage, beginAccess);
  auto result = insertStorageAccess(storageAccess);
  if (!result.second)
    result.first->mergeFrom(storageAccess);
}

void FunctionAccessedStorage::analyzeInstruction(SILInstruction *I) {
  if (auto *BAI = dyn_cast<BeginAccessInst>(I))
    visitBeginAccess(BAI);
  else if (auto *BUAI = dyn_cast<BeginUnpairedAccessInst>(I))
    visitBeginAccess(BUAI);
}

void StorageAccessInfo::print(raw_ostream &os) const {
  os << "  [" << getSILAccessKindName(getAccessKind()) << "] ";
  if (hasNoNestedConflict())
    os << "[no_nested_conflict] ";
  AccessedStorage::print(os);
}

void StorageAccessInfo::dump() const { print(llvm::dbgs()); }

void FunctionAccessedStorage::print(raw_ostream &os) const {
  for (auto &storageAccess : storageAccessSet)
    storageAccess.print(os);

  if (unidentifiedAccess != None) {
    os << "  unidentified accesses: "
       << getSILAccessKindName(unidentifiedAccess.getValue()) << "\n";
  }
}

void FunctionAccessedStorage::dump() const { print(llvm::dbgs()); }

SILAnalysis *swift::createAccessedStorageAnalysis(SILModule *) {
  return new AccessedStorageAnalysis();
}
