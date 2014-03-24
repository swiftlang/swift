//===--- SILWitnessTable.h - Defines the SILWitnessTable class ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/AST/Mangle.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

static void mangleConstant(NormalProtocolConformance *C,
                           llvm::raw_ostream &buffer) {
  using namespace Mangle;
  Mangler mangler(buffer);

  //   mangled-name ::= '_T' global
  //   global ::= 'WP' protocol-conformance
  buffer << "_TWP";
  mangler.mangleProtocolConformance(C);
  return;
}

SILWitnessTable *
SILWitnessTable::create(SILModule &M, SILLinkage Linkage,
                        NormalProtocolConformance *Conformance,
                        ArrayRef<SILWitnessTable::Entry> entries) {
  // Create the mangled name of our witness table...
  llvm::SmallString<32> buffer;
  llvm::raw_svector_ostream stream(buffer);
  mangleConstant(Conformance, stream);
  Identifier Name = M.getASTContext().getIdentifier(buffer);

  // Allocate the witness table and initialize it.
  void *buf = M.allocate(sizeof(SILWitnessTable), alignof(SILWitnessTable));
  SILWitnessTable *wt = ::new (buf) SILWitnessTable(M, Linkage, Name.str(),
                                                    Conformance, entries);

  // Update the SILModule state in light of wT.
  M.witnessTables.push_back(wt);
  M.WitnessTableLookupCache[Conformance] = wt;

  // Return the resulting witness table.
  return wt;
}

SILWitnessTable *
SILWitnessTable::create(SILModule &M, SILLinkage Linkage,
                        NormalProtocolConformance *Conformance) {
  // Create the mangled name of our witness table...
  llvm::SmallString<32> buffer;
  llvm::raw_svector_ostream stream(buffer);
  mangleConstant(Conformance, stream);
  Identifier Name = M.getASTContext().getIdentifier(buffer);

  // Allocate the witness table and initialize it.
  void *buf = M.allocate(sizeof(SILWitnessTable), alignof(SILWitnessTable));
  SILWitnessTable *wt = ::new (buf) SILWitnessTable(M, Linkage, Name.str(),
                                                    Conformance);

  // Update the SILModule state in light of wT.
  M.witnessTables.push_back(wt);
  M.WitnessTableLookupCache[Conformance] = wt;

  // Return the resulting witness table.
  return wt;
}

SILWitnessTable::SILWitnessTable(SILModule &M, SILLinkage Linkage, StringRef N,
                                 NormalProtocolConformance *Conformance,
                                 ArrayRef<Entry> entries)
  : Mod(M), Name(N), Linkage(Linkage), Conformance(Conformance), Entries(),
    IsDeclaration(true) {
  convertToDefinition(entries);
}

SILWitnessTable::SILWitnessTable(SILModule &M, SILLinkage Linkage, StringRef N,
                                 NormalProtocolConformance *Conformance)
  : Mod(M), Name(N), Linkage(Linkage), Conformance(Conformance), Entries(),
    IsDeclaration(true)
{}

SILWitnessTable::~SILWitnessTable() {
  if (isDeclaration())
    return;

  // Drop the reference count of witness functions referenced by this table.
  for (auto entry : getEntries()) {
    switch (entry.getKind()) {
    case Method:
      entry.getMethodWitness().Witness->RefCount--;
      break;
    case AssociatedType:
    case AssociatedTypeProtocol:
    case BaseProtocol:
    case Invalid:
      break;
    }
  }
}

void SILWitnessTable::convertToDefinition(ArrayRef<Entry> entries) {
  assert(isDeclaration() && "Definitions should never call this method.");
  IsDeclaration = false;

  void *buf = Mod.allocate(sizeof(Entry)*entries.size(), alignof(Entry));
  memcpy(buf, entries.begin(), sizeof(Entry)*entries.size());
  Entries = ArrayRef<Entry>(static_cast<Entry*>(buf), entries.size());

  // Bump the reference count of witness functions referenced by this table.
  for (auto entry : getEntries()) {
    switch (entry.getKind()) {
    case Method:
      entry.getMethodWitness().Witness->RefCount++;
      break;
    case AssociatedType:
    case AssociatedTypeProtocol:
    case BaseProtocol:
    case Invalid:
      break;
    }
  }
}
