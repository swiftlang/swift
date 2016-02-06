//===--- SILDefaultWitnessTable.cpp ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the SILDefaultWitnessTable class, which is used to provide
// default implementations of protocol requirements for resilient protocols,
// allowing IRGen to generate the appropriate metadata so that the runtime can
// insert those requirements to witness tables that were emitted prior to the
// requirement being added.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILDefaultWitnessTable.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

void SILDefaultWitnessTable::addDefaultWitnessTable() {
  // Make sure we have not seen this witness table yet.
  assert(Mod.DefaultWitnessTableMap.find(Protocol) ==
         Mod.DefaultWitnessTableMap.end() && "Attempting to create duplicate "
         "default witness table.");
  Mod.DefaultWitnessTableMap[Protocol] = this;
  Mod.defaultWitnessTables.push_back(this);
}

SILDefaultWitnessTable *
SILDefaultWitnessTable::create(SILModule &M, const ProtocolDecl *Protocol,
                               unsigned MinimumWitnessTableSizeInWords,
                               ArrayRef<SILDefaultWitnessTable::Entry> entries){
  // Allocate the witness table and initialize it.
  void *buf = M.allocate(sizeof(SILDefaultWitnessTable),
                         alignof(SILDefaultWitnessTable));
  SILDefaultWitnessTable *wt =
      ::new (buf) SILDefaultWitnessTable(M, Protocol,
                                         MinimumWitnessTableSizeInWords,
                                         entries);

  wt->addDefaultWitnessTable();

  // Return the resulting default witness table.
  return wt;
}

SILDefaultWitnessTable *
SILDefaultWitnessTable::create(SILModule &M, const ProtocolDecl *Protocol) {
  // Allocate the witness table and initialize it.
  void *buf = M.allocate(sizeof(SILDefaultWitnessTable),
                         alignof(SILDefaultWitnessTable));
  SILDefaultWitnessTable *wt =
      ::new (buf) SILDefaultWitnessTable(M, Protocol);

  wt->addDefaultWitnessTable();

  // Return the resulting default witness table.
  return wt;
}

SILDefaultWitnessTable::
SILDefaultWitnessTable(SILModule &M,
                       const ProtocolDecl *Protocol,
                       unsigned MinimumWitnessTableSizeInWords,
                       ArrayRef<Entry> entries)
  : Mod(M), Protocol(Protocol), MinimumWitnessTableSizeInWords(0), Entries(),
    IsDeclaration(true) {

  convertToDefinition(MinimumWitnessTableSizeInWords, entries);
}

SILDefaultWitnessTable::SILDefaultWitnessTable(SILModule &M,
                                               const ProtocolDecl *Protocol)
  : Mod(M), Protocol(Protocol), MinimumWitnessTableSizeInWords(0), Entries(),
    IsDeclaration(true) {}

void SILDefaultWitnessTable::
convertToDefinition(unsigned MinimumWitnessTableSizeInWords,
                    ArrayRef<Entry> entries) {
  assert(IsDeclaration);
  IsDeclaration = false;

  this->MinimumWitnessTableSizeInWords = MinimumWitnessTableSizeInWords;

  void *buf = Mod.allocate(sizeof(Entry)*entries.size(), alignof(Entry));
  memcpy(buf, entries.begin(), sizeof(Entry)*entries.size());
  Entries = MutableArrayRef<Entry>(static_cast<Entry*>(buf), entries.size());

  // Bump the reference count of witness functions referenced by this table.
  for (auto entry : getEntries()) {
    if (entry.isValid()) {
      entry.getWitness()->incrementRefCount();
    }
  }
}

SILDefaultWitnessTable::~SILDefaultWitnessTable() {
  // Drop the reference count of witness functions referenced by this table.
  for (auto entry : getEntries()) {
    if (entry.isValid()) {
      entry.getWitness()->decrementRefCount();
    }
  }
}
