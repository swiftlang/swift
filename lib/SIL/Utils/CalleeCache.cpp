//===--- CalleeCache.cpp - Determine callees per call site ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/CalleeCache.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Assertions.h"
#include "llvm/Support/Compiler.h"

#define DEBUG_TYPE "CalleeCache"

using namespace swift;

/// Are the callees that could be called through Decl statically
/// knowable based on the Decl and the compilation mode?
bool swift::calleesAreStaticallyKnowable(SILModule &module, SILDeclRef decl) {
  if (decl.isForeign)
    return false;
  return calleesAreStaticallyKnowable(module, decl.getDecl());
}

/// Are the callees that could be called through Decl statically
/// knowable based on the Decl and the compilation mode?
bool swift::calleesAreStaticallyKnowable(SILModule &module, ValueDecl *vd) {
  assert(isa<AbstractFunctionDecl>(vd) || isa<EnumElementDecl>(vd));

  // Only handle members defined within the SILModule's associated context.
  if (!cast<DeclContext>(vd)->isChildContextOf(module.getAssociatedContext()))
    return false;

  if (vd->isDynamic()) {
    return false;
  }

  if (!vd->hasAccess())
    return false;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (vd->getEffectiveAccess()) {
  case AccessLevel::Open:
    return false;
  case AccessLevel::Public:
  case AccessLevel::Package:
    if (isa<ConstructorDecl>(vd)) {
      // Constructors are special: a derived class in another module can
      // "override" a constructor if its class is "open", although the
      // constructor itself is not open.
      auto *nd = vd->getDeclContext()->getSelfNominalTypeDecl();
      if (nd->getEffectiveAccess() == AccessLevel::Open)
        return false;
    }
    LLVM_FALLTHROUGH;
  case AccessLevel::Internal:
    return module.isWholeModule();
  case AccessLevel::FilePrivate:
  case AccessLevel::Private:
    return true;
  }

  llvm_unreachable("Unhandled access level in switch.");
}

void CalleeList::dump() const {
  print(llvm::errs());
}

void CalleeList::print(llvm::raw_ostream &os) const {
  os << "Incomplete callee list? : "
               << (isIncomplete() ? "Yes" : "No");
  if (!allCalleesVisible())
    os <<", not all callees visible";
  os << '\n';
  os << "Known callees:\n";
  for (auto *CalleeFn : *this) {
    os << "  " << CalleeFn->getName() << "\n";
  }
  os << "\n";
}

bool CalleeList::allCalleesVisible() const {
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
        // shared functions are always emitted in the client.
        Callee->getLinkage() != SILLinkage::Shared)
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

  auto *TheCallees = new (Allocator.Allocate()) CalleeList::Callees;

  bool canCallUnknown = !calleesAreStaticallyKnowable(M, Decl);
  CalleesAndCanCallUnknown Entry(TheCallees, canCallUnknown);

  bool Inserted;
  CacheType::iterator It;
  std::tie(It, Inserted) = TheCache.insert(std::make_pair(Decl, Entry));
  assert(Inserted && "Expected new entry to be inserted!");

  return It->second;
}

/// Update the callees for each method of a given vtable.
void CalleeCache::computeClassMethodCallees() {
  SmallPtrSet<AbstractFunctionDecl *, 16> unknownCallees;

  // First mark all method declarations which might be overridden in another
  // translation unit, i.e. outside the visibility of the optimizer.
  // This is a little bit more complicated than to just check the VTable
  // entry.Method itself, because an overridden method might be more accessible
  // than the base method (e.g. a public method overrides a private method).
  for (auto &VTable : M.getVTables()) {
    assert(!VTable->getClass()->hasClangNode());

    for (Decl *member : VTable->getClass()->getMembers()) {
      if (auto *afd = dyn_cast<AbstractFunctionDecl>(member)) {
        // If a method implementation might be overridden in another translation
        // unit, also mark all the base methods as 'unknown'.
        bool unknown = false;
        do {
          if (!calleesAreStaticallyKnowable(M, afd))
            unknown = true;
          if (unknown)
            unknownCallees.insert(afd);
          afd = afd->getOverriddenDecl();
        } while (afd);
      }
    }
  }

  // Second step: collect all implementations of a method.
  for (auto &VTable : M.getVTables()) {
    for (const SILVTable::Entry &entry : VTable->getEntries()) {
      if (auto *afd = entry.getMethod().getAbstractFunctionDecl()) {
        CalleesAndCanCallUnknown &callees =
            getOrCreateCalleesForMethod(entry.getMethod());
        if (unknownCallees.count(afd) != 0)
          callees.setInt(1);
        callees.getPointer()->push_back(entry.getImplementation());
      }
    }
  }
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
        !WT.getConformance()->getRootConformance()->hasWitness(Requirement.getDecl())) {
      TheCallees.setInt(true);
      continue;
    }

    bool canCallUnknown = false;

    auto Conf = WT.getConformance();
    switch (Conf->getProtocol()->getEffectiveAccess()) {
      case AccessLevel::Open:
        llvm_unreachable("protocols cannot have open access level");
      case AccessLevel::Package:
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
        auto Witness = Conf->getRootConformance()->getWitness(Requirement.getDecl());
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

  computeClassMethodCallees();

  for (auto &WTable : M.getWitnessTableList())
    computeWitnessMethodCalleesForWitnessTable(WTable);
}

SILFunction *
CalleeCache::getSingleCalleeForWitnessMethod(WitnessMethodInst *WMI) const {
  SILFunction *CalleeFn;
  SILWitnessTable *WT;

  // Attempt to find a specific callee for the given conformance and member.
  std::tie(CalleeFn, WT) = lookUpFunctionInWitnessTable(WMI, SILModule::LinkingMode::LinkNormal);

  return CalleeFn;
}

// Look up the precomputed callees for an abstract function and
// return it as a CalleeList.
CalleeList CalleeCache::getCalleeList(SILDeclRef Decl) const {
  auto Found = TheCache.find(Decl);
  if (Found == TheCache.end())
    return CalleeList();

  auto &Pair = Found->second;
  return CalleeList(Pair.getPointer(), Pair.getInt());
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
        cast<FunctionRefInst>(Callee)->getReferencedFunction());

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

/// Return the list of destructors of the class type \p type.
///
/// If \p type is an optional, look through that optional.
/// If \p exactType is true, then \p type is treated like a final class type.
CalleeList CalleeCache::getDestructors(SILType type, bool isExactType) const {
  while (auto payloadTy = type.getOptionalObjectType()) {
    type = payloadTy;
  }
  ClassDecl *classDecl = type.getClassOrBoundGenericClass();
  if (!classDecl || classDecl->hasClangNode())
    return CalleeList();

  if (isExactType || classDecl->isFinal()) {
    // In case of a final class, just pick the deinit of the class.
    SILDeclRef destructor = SILDeclRef(classDecl->getDestructor());
    
    // In embedded Swift, we need the specialized destructor, not the generic
    // one.
    if (auto *vtable = M.lookUpSpecializedVTable(type)) {
      if (auto entry = vtable->getEntry(M, destructor))
        return CalleeList(entry->getImplementation());
      return CalleeList();
    }

    if (SILFunction *destrImpl = M.lookUpFunction(destructor))
      return CalleeList(destrImpl);
    return CalleeList();
  }
  // If all that doesn't help get the list of deinits as we do for regular class
  // methods.
  return getCalleeList(SILDeclRef(classDecl->getDestructor()));
}
