//===--- BasicCalleeAnalysis.cpp - Determine callees per call site --------===//
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

#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"

#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Statistic.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/Support/Compiler.h"

#include <algorithm>

#define DEBUG_TYPE "BasicCalleeAnalysis"

using namespace swift;

bool CalleeList::allCalleesVisible() {
  if (isIncomplete())
    return false;

  for (SILFunction *Callee : *this) {
    if (Callee->isExternalDeclaration())
      return false;
    // Do not consider functions in other modules (libraries) because of library
    // evolution: such function may behave differently in future/past versions
    // of the library.
    // TODO: exclude functions which are deserialized from modules in the same
    // resilience domain.
    if (Callee->isAvailableExternally() &&
        // shared_external functions are always emitted in the client.
        Callee->getLinkage() != SILLinkage::SharedExternal)
      return false;
  }
  return true;
}

void CalleeCache::sortAndUniqueCallees() {
  // Sort the callees for each decl and remove duplicates.
  for (auto &Pair : TheCache) {
    auto &Callees = *Pair.second.getPointer();

    // Sort by enumeration number so that clients get a stable order.
    std::sort(Callees.begin(), Callees.end(),
              [](SILFunction *Left, SILFunction *Right) {
                // Check if Right's lexicographical order is greater than Left.
                return 1 == Right->getName().compare(Left->getName());
              });

    // Remove duplicates.
    Callees.erase(std::unique(Callees.begin(), Callees.end()), Callees.end());
  }
}

CalleeCache::CalleesAndCanCallUnknown &
CalleeCache::getOrCreateCalleesForMethod(SILDeclRef Decl) {
  auto Found = TheCache.find(Decl);
  if (Found != TheCache.end())
    return Found->second;

  auto *TheCallees = new (Allocator.Allocate()) Callees;

  bool canCallUnknown = !calleesAreStaticallyKnowable(M, Decl);
  CalleesAndCanCallUnknown Entry(TheCallees, canCallUnknown);

  bool Inserted;
  CacheType::iterator It;
  std::tie(It, Inserted) = TheCache.insert(std::make_pair(Decl, Entry));
  assert(Inserted && "Expected new entry to be inserted!");

  return It->second;
}

/// Update the callees for each method of a given class, along with
/// all the overridden methods from superclasses.
void CalleeCache::computeClassMethodCalleesForClass(ClassDecl *CD) {
  assert(!CD->hasClangNode());

  for (auto *Member : CD->getMembers()) {
    auto *AFD = dyn_cast<AbstractFunctionDecl>(Member);
    if (!AFD)
      continue;

    if (auto *ConstrDecl = dyn_cast<ConstructorDecl>(AFD)) {
      computeClassMethodCallees(CD, SILDeclRef(AFD,
                                               SILDeclRef::Kind::Initializer));
      if (ConstrDecl->isRequired()) {
        computeClassMethodCallees(CD, SILDeclRef(AFD,
                                                 SILDeclRef::Kind::Allocator));
      }
    } else {
      computeClassMethodCallees(CD, SILDeclRef(AFD));
    }
  }
}

void CalleeCache::computeClassMethodCallees(ClassDecl *CD, SILDeclRef Method) {
  auto *CalledFn = M.lookUpFunctionInVTable(CD, Method);
  if (!CalledFn)
    return;

  bool canCallUnknown = !calleesAreStaticallyKnowable(M, Method);

  // Update the callees for this method and all the methods it
  // overrides by adding this function to their lists.
  do {
    auto &TheCallees = getOrCreateCalleesForMethod(Method);
    assert(TheCallees.getPointer() && "Unexpected null callees!");

    TheCallees.getPointer()->push_back(CalledFn);
    if (canCallUnknown)
      TheCallees.setInt(true);

    Method = Method.getNextOverriddenVTableEntry();
  } while (Method);
}

void CalleeCache::computeWitnessMethodCalleesForWitnessTable(
    SILWitnessTable &WT) {
  for (const SILWitnessTable::Entry &Entry : WT.getEntries()) {
    if (Entry.getKind() != SILWitnessTable::Method)
      continue;

    auto &WitnessEntry = Entry.getMethodWitness();
    auto Requirement = WitnessEntry.Requirement;
    auto *WitnessFn = WitnessEntry.Witness;

    // Dead function elimination nulls out entries for functions it removes.
    if (!WitnessFn)
      continue;

    auto &TheCallees = getOrCreateCalleesForMethod(Requirement);
    assert(TheCallees.getPointer() && "Unexpected null callees!");

    TheCallees.getPointer()->push_back(WitnessFn);

    // If we can't resolve the witness, conservatively assume it can call
    // anything.
    if (!Requirement.getDecl()->isProtocolRequirement() ||
        !WT.getConformance()->hasWitness(Requirement.getDecl())) {
      TheCallees.setInt(true);
      continue;
    }

    bool canCallUnknown = false;

    auto Conf = WT.getConformance();
    switch (Conf->getProtocol()->getEffectiveAccess()) {
      case AccessLevel::Open:
        llvm_unreachable("protocols cannot have open access level");
      case AccessLevel::Public:
        canCallUnknown = true;
        break;
      case AccessLevel::Internal:
        if (!M.isWholeModule()) {
          canCallUnknown = true;
          break;
        }
        LLVM_FALLTHROUGH;
      case AccessLevel::FilePrivate:
      case AccessLevel::Private: {
        auto Witness = Conf->getWitness(Requirement.getDecl());
        auto DeclRef = SILDeclRef(Witness.getDecl());
        canCallUnknown = !calleesAreStaticallyKnowable(M, DeclRef);
      }
    }
    if (canCallUnknown)
      TheCallees.setInt(true);
  }
}

/// Compute the callees for each method that appears in a VTable or
/// Witness Table.
void CalleeCache::computeMethodCallees() {
  SWIFT_FUNC_STAT;
  for (auto &VTable : M.getVTableList())
    computeClassMethodCalleesForClass(VTable.getClass());

  for (auto &WTable : M.getWitnessTableList())
    computeWitnessMethodCalleesForWitnessTable(WTable);
}

SILFunction *
CalleeCache::getSingleCalleeForWitnessMethod(WitnessMethodInst *WMI) const {
  SILFunction *CalleeFn;
  SILWitnessTable *WT;

  // Attempt to find a specific callee for the given conformance and member.
  std::tie(CalleeFn, WT) = WMI->getModule().lookUpFunctionInWitnessTable(
      WMI->getConformance(), WMI->getMember());

  return CalleeFn;
}

// Look up the precomputed callees for an abstract function and
// return it as a CalleeList.
CalleeList CalleeCache::getCalleeList(SILDeclRef Decl) const {
  auto Found = TheCache.find(Decl);
  if (Found == TheCache.end())
    return CalleeList();

  auto &Pair = Found->second;
  return CalleeList(*Pair.getPointer(), Pair.getInt());
}

// Return a callee list for the given witness method.
CalleeList CalleeCache::getCalleeList(WitnessMethodInst *WMI) const {
  // First attempt to see if we can narrow it down to a single
  // function based on the conformance.
  if (auto *CalleeFn = getSingleCalleeForWitnessMethod(WMI))
    return CalleeList(CalleeFn);

  // Otherwise see if we previously computed the callees based on
  // witness tables.
  return getCalleeList(WMI->getMember());
}

// Return a callee list for a given class method.
CalleeList CalleeCache::getCalleeList(ClassMethodInst *CMI) const {
  // Look for precomputed callees based on vtables.
  return getCalleeList(CMI->getMember());
}

// Return the list of functions that can be called via the given callee.
CalleeList CalleeCache::getCalleeListForCalleeKind(SILValue Callee) const {
  switch (Callee->getKind()) {
  default:
    assert(!isa<MethodInst>(Callee) &&
           "Unhandled method instruction in callee determination!");
    return CalleeList();

  case ValueKind::FunctionRefInst:
    return CalleeList(
        cast<FunctionRefInst>(Callee)->getInitiallyReferencedFunction());

  case ValueKind::DynamicFunctionRefInst:
  case ValueKind::PreviousDynamicFunctionRefInst:
    return CalleeList(); // Don't know the dynamic target.

  case ValueKind::PartialApplyInst:
    return getCalleeListForCalleeKind(
        cast<PartialApplyInst>(Callee)->getCallee());

  case ValueKind::WitnessMethodInst:
    return getCalleeList(cast<WitnessMethodInst>(Callee));

  case ValueKind::ClassMethodInst:
    return getCalleeList(cast<ClassMethodInst>(Callee));

  case ValueKind::SuperMethodInst:
  case ValueKind::ObjCMethodInst:
  case ValueKind::ObjCSuperMethodInst:
    return CalleeList();
  }
}

// Return the list of functions that can be called via the given apply
// site.
CalleeList CalleeCache::getCalleeList(FullApplySite FAS) const {
  return getCalleeListForCalleeKind(FAS.getCalleeOrigin());
}

// Return the list of functions that can be called via the given instruction.
CalleeList CalleeCache::getCalleeList(SILInstruction *I) const {
  // We support only deallocation instructions at the moment.
  assert((isa<StrongReleaseInst>(I) || isa<ReleaseValueInst>(I)) &&
         "A deallocation instruction expected");
  auto Ty = I->getOperand(0)->getType();
  while (auto payloadTy = Ty.getOptionalObjectType())
    Ty = payloadTy;
  auto Class = Ty.getClassOrBoundGenericClass();
  if (!Class || Class->hasClangNode())
    return CalleeList();
  SILDeclRef Destructor = SILDeclRef(Class->getDestructor());
  return getCalleeList(Destructor);
}
