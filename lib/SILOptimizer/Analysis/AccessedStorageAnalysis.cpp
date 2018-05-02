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

#define DEBUG_TYPE "accessed-storage"

#include "swift/SILOptimizer/Analysis/AccessedStorageAnalysis.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"

using namespace swift;

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
  assert(accessKind == SILAccessKind::Read
         || accessKind == SILAccessKind::Modify && "uninitialized info");
  bool changed = updateAccessKind(accessKind, RHS.accessKind);
  if (noNestedConflict && !RHS.noNestedConflict) {
    noNestedConflict = false;
    changed = true;
  }
  return changed;
}

bool FunctionAccessedStorage::summarizeFunction(SILFunction *F) {
  assert(storageAccessMap.empty() && "expected uninitialized results.");

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
  // storageAccessMap and unidentifiedAccess == None.
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

bool FunctionAccessedStorage::mergeAccesses(
    const FunctionAccessedStorage &RHS,
    std::function<AccessedStorage(const AccessedStorage &)> transformStorage) {

  bool changed = false;

  for (auto &accessEntry : RHS.storageAccessMap) {

    const AccessedStorage &storage = transformStorage(accessEntry.first);
    // transformStorage returns invalid storage object for local storage
    // that should not be merged with the caller.
    if (!storage)
      continue;

    if (storage.getKind() == AccessedStorage::Unidentified)
      changed |= updateUnidentifiedAccess(accessEntry.second.accessKind);
    else {
      auto result = storageAccessMap.try_emplace(storage, accessEntry.second);
      if (result.second)
        changed = true;
      else
        changed |= result.first->second.mergeFrom(accessEntry.second);
    }
  }

  if (RHS.unidentifiedAccess != None)
    changed |= updateUnidentifiedAccess(RHS.unidentifiedAccess.getValue());

  return changed;
}

bool FunctionAccessedStorage::mergeFrom(const FunctionAccessedStorage &RHS) {
  // Merge accesses from RHS. Both `this` and `RHS` are either from the same
  // function or are both callees of the same call site, so their parameters
  // indices coincide. transformStorage is the identity function.
  return mergeAccesses(RHS, [](const AccessedStorage &s) { return s; });
}

/// Transform AccessedStorage from a callee into the caller context. If this is
/// uniquely identified local storage, then return an invalid storage object.
static AccessedStorage transformCalleeStorage(const AccessedStorage &storage,
                                              FullApplySite fullApply) {
  switch (storage.getKind()) {
  case AccessedStorage::Box:
  case AccessedStorage::Stack:
    // Do not merge local storage.
    return AccessedStorage();
  case AccessedStorage::Global:
    // Global accesses is universal.
    return storage;
  case AccessedStorage::Class: {
    // If the object's value is an argument, translate it into a value on the
    // caller side.
    SILValue obj = storage.getObjectProjection().getObject();
    if (auto *arg = dyn_cast<SILFunctionArgument>(obj)) {
      return AccessedStorage(fullApply.getArgument(arg->getIndex()),
                             storage.getObjectProjection().getProjection());
    }
    // Otherwise, continue to reference the value in the callee because we don't
    // have any better placeholder for a callee-defined object.
    return storage;
  }
  case AccessedStorage::Argument:
    // Transitively search for the storage base in the caller.
    return findAccessedStorageOrigin(fullApply.getArgument(storage.getParamIndex()));
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
  // Merge accesses from RHS using the identity transform on it's
  // AccessedStorage. Transform any Argument type AccessedStorage into the
  // caller context to be added to `this` storage map.
  return mergeAccesses(calleeAccess, [&fullApply](const AccessedStorage &s) {
    return transformCalleeStorage(s, fullApply);
  });
}

template <typename B>
void FunctionAccessedStorage::visitBeginAccess(B *beginAccess) {
  const AccessedStorage &storage =
      findAccessedStorageOrigin(beginAccess->getSource());

  if (storage.getKind() == AccessedStorage::Unidentified) {
    updateOptionalAccessKind(unidentifiedAccess, beginAccess->getAccessKind());
    return;
  }
  StorageAccessInfo accessInfo(beginAccess);
  auto result = storageAccessMap.try_emplace(storage, accessInfo);
  if (!result.second)
    result.first->second.mergeFrom(accessInfo);
}

void FunctionAccessedStorage::analyzeInstruction(SILInstruction *I) {
  if (auto *BAI = dyn_cast<BeginAccessInst>(I))
    visitBeginAccess(BAI);
  else if (auto *BUAI = dyn_cast<BeginUnpairedAccessInst>(I))
    visitBeginAccess(BUAI);
}

bool FunctionAccessedStorage::mayConflictWith(
    SILAccessKind otherAccessKind, const AccessedStorage &otherStorage) {
  if (unidentifiedAccess != None
      && accessKindMayConflict(otherAccessKind,
                               unidentifiedAccess.getValue())) {
    return true;
  }
  for (auto &accessEntry : storageAccessMap) {

    const AccessedStorage &storage = accessEntry.first;
    assert(storage && "FunctionAccessedStorage mapped invalid storage.");

    StorageAccessInfo accessInfo = accessEntry.second;
    if (!accessKindMayConflict(otherAccessKind, accessInfo.accessKind))
      continue;

    if (!otherStorage.isDistinct(storage))
      return true;
  }
  return false;
}

void FunctionAccessedStorage::print(raw_ostream &os) const {
  for (auto &accessEntry : storageAccessMap) {
    const AccessedStorage &storage = accessEntry.first;
    const StorageAccessInfo &info = accessEntry.second;
    os << "  [" << getSILAccessKindName(info.accessKind) << "] ";
    if (info.noNestedConflict)
      os << "[no_nested_conflict] ";
    storage.print(os);
  }
  if (unidentifiedAccess != None) {
    os << "  unidentified accesses: "
       << getSILAccessKindName(unidentifiedAccess.getValue()) << "\n";
  }
}

void FunctionAccessedStorage::dump() const { print(llvm::errs()); }

SILAnalysis *swift::createAccessedStorageAnalysis(SILModule *) {
  return new AccessedStorageAnalysis();
}
