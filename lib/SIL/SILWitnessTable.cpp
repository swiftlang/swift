//===--- SILWitnessTable.cpp - Defines the SILWitnessTable class ----------===//
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
// This file defines the SILWitnessTable class, which is used to map a protocol
// conformance for a type to its implementing SILFunctions. This information is
// (FIXME will be) used by IRGen to create witness tables for protocol dispatch.
// It can also be used by generic specialization and existential
// devirtualization passes to promote witness_method and protocol_method
// instructions to static function_refs.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILWitnessTable.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

static std::string mangleConstant(NormalProtocolConformance *C) {
  Mangle::ASTMangler Mangler;
  return Mangler.mangleWitnessTable(C);
}

void SILWitnessTable::addWitnessTable() {
  // Make sure we have not seen this witness table yet.
  assert(Mod.WitnessTableMap.find(Conformance) ==
         Mod.WitnessTableMap.end() && "Attempting to create duplicate "
         "witness table.");
  Mod.WitnessTableMap[Conformance] = this;
  Mod.witnessTables.push_back(this);
}

SILWitnessTable *SILWitnessTable::create(
    SILModule &M, SILLinkage Linkage, IsSerialized_t Serialized,
    NormalProtocolConformance *Conformance,
    ArrayRef<SILWitnessTable::Entry> entries,
    ArrayRef<ConditionalConformance> conditionalConformances) {
  assert(Conformance && "Cannot create a witness table for a null "
         "conformance.");

  // Create the mangled name of our witness table...
  Identifier Name = M.getASTContext().getIdentifier(mangleConstant(Conformance));

  // Allocate the witness table and initialize it.
  void *buf = M.allocate(sizeof(SILWitnessTable), alignof(SILWitnessTable));
  SILWitnessTable *wt = ::new (buf)
      SILWitnessTable(M, Linkage, Serialized, Name.str(), Conformance, entries,
                      conditionalConformances);

  wt->addWitnessTable();

  // Return the resulting witness table.
  return wt;
}

SILWitnessTable *
SILWitnessTable::create(SILModule &M, SILLinkage Linkage,
                        NormalProtocolConformance *Conformance) {
  assert(Conformance && "Cannot create a witness table for a null "
         "conformance.");

  // Create the mangled name of our witness table...
  Identifier Name = M.getASTContext().getIdentifier(mangleConstant(Conformance));


  // Allocate the witness table and initialize it.
  void *buf = M.allocate(sizeof(SILWitnessTable), alignof(SILWitnessTable));
  SILWitnessTable *wt = ::new (buf) SILWitnessTable(M, Linkage, Name.str(),
                                                    Conformance);

  wt->addWitnessTable();

  // Return the resulting witness table.
  return wt;
}

SILWitnessTable::SILWitnessTable(
    SILModule &M, SILLinkage Linkage, IsSerialized_t Serialized, StringRef N,
    NormalProtocolConformance *Conformance, ArrayRef<Entry> entries,
    ArrayRef<ConditionalConformance> conditionalConformances)
    : Mod(M), Name(N), Linkage(Linkage), Conformance(Conformance), Entries(),
      ConditionalConformances(), IsDeclaration(true), Serialized(false) {
  convertToDefinition(entries, conditionalConformances, Serialized);
}

SILWitnessTable::SILWitnessTable(SILModule &M, SILLinkage Linkage, StringRef N,
                                 NormalProtocolConformance *Conformance)
    : Mod(M), Name(N), Linkage(Linkage), Conformance(Conformance), Entries(),
      ConditionalConformances(), IsDeclaration(true), Serialized(false) {}

SILWitnessTable::~SILWitnessTable() {
  if (isDeclaration())
    return;

  // Drop the reference count of witness functions referenced by this table.
  for (auto entry : getEntries()) {
    switch (entry.getKind()) {
    case Method:
      if (entry.getMethodWitness().Witness) {
        entry.getMethodWitness().Witness->decrementRefCount();
      }
      break;
    // SWIFT_ENABLE_TENSORFLOW
    case AutoDiffAssociatedFunction:
      if (entry.getAutoDiffAssociatedFunctionWitness().Witness) {
        entry.getAutoDiffAssociatedFunctionWitness().Witness
            ->decrementRefCount();
      }
      break;
    case AssociatedType:
    case AssociatedTypeProtocol:
    case BaseProtocol:
    case Invalid:
      break;
    }
  }
}

void SILWitnessTable::convertToDefinition(
    ArrayRef<Entry> entries,
    ArrayRef<ConditionalConformance> conditionalConformances,
    IsSerialized_t isSerialized) {
  assert(isDeclaration() && "Definitions should never call this method.");
  IsDeclaration = false;
  assert(isSerialized != IsSerializable);
  Serialized = (isSerialized == IsSerialized);

  Entries = Mod.allocateCopy(entries);
  ConditionalConformances = Mod.allocateCopy(conditionalConformances);

  // Bump the reference count of witness functions referenced by this table.
  for (auto entry : getEntries()) {
    switch (entry.getKind()) {
    case Method:
      if (entry.getMethodWitness().Witness) {
        entry.getMethodWitness().Witness->incrementRefCount();
      }
      break;
    // SWIFT_ENABLE_TENSORFLOW
    case AutoDiffAssociatedFunction:
      if (entry.getAutoDiffAssociatedFunctionWitness().Witness) {
        entry.getAutoDiffAssociatedFunctionWitness().Witness
            ->incrementRefCount();
      }
      break;
    case AssociatedType:
    case AssociatedTypeProtocol:
    case BaseProtocol:
    case Invalid:
      break;
    }
  }
}

bool SILWitnessTable::conformanceIsSerialized(ProtocolConformance *conformance) {
  // Serialize witness tables for conformances synthesized by
  // the ClangImporter.
  if (isa<ClangModuleUnit>(conformance->getDeclContext()->getModuleScopeContext()))
    return true;

  auto *nominal = conformance->getType()->getAnyNominal();
  // Only serialize witness tables for fixed layout types.
  //
  // FIXME: This is not the right long term solution. We need an explicit
  // mechanism for declaring conformances as 'fragile'.
  auto protocolIsPublic =
      conformance->getProtocol()->getEffectiveAccess() >= AccessLevel::Public;
  auto typeIsPublic = nominal->getEffectiveAccess() >= AccessLevel::Public;
  return !nominal->isResilient() && protocolIsPublic && typeIsPublic;
}

bool SILWitnessTable::enumerateWitnessTableConditionalConformances(
    const ProtocolConformance *conformance,
    llvm::function_ref<bool(unsigned, CanType, ProtocolDecl *)> fn) {
  unsigned conformanceIndex = 0;
  for (auto req : conformance->getConditionalRequirements()) {
    if (req.getKind() != RequirementKind::Conformance)
      continue;

    auto proto = req.getSecondType()->castTo<ProtocolType>()->getDecl();

    if (Lowering::TypeConverter::protocolRequiresWitnessTable(proto)) {
      if (fn(conformanceIndex, req.getFirstType()->getCanonicalType(), proto))
        return true;

      conformanceIndex++;
    }
  }
  return false;
}
