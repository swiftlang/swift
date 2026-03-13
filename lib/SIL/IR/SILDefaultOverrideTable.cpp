//===--- SILDefaultOverrideTable.cpp --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the SILDefaultOverrideTable class, which is used to
// provide default override implementations of class routines which have been
// come to implement the same semantic class member that was previously
// implemented by a different routine.  As with SILDefaultWitnessTable, this
// type enables IRGen to generate metadata which in turn allows the runtime to
// instantiate vtables which contain these default overrides when they are
// needed: in the vtables of subclasses which were emitted prior to the
// replacement of the routine that implements the semantic member AND which
// already provided an override of the routine that previously implemented the
// semantic member.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILDefaultOverrideTable.h"
#include "swift/AST/ASTMangler.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILDefaultOverrideTable::SILDefaultOverrideTable(SILModule &M,
                                                 SILLinkage Linkage,
                                                 const ClassDecl *decl)
    : module(M), linkage(Linkage), decl(decl), entries({}),
      state(State::Declared) {}

SILDefaultOverrideTable *
SILDefaultOverrideTable::declare(SILModule &module, SILLinkage linkage,
                                 const ClassDecl *decl) {
  auto *buffer = module.allocate<SILDefaultOverrideTable>(1);
  SILDefaultOverrideTable *table =
      ::new (buffer) SILDefaultOverrideTable(module, linkage, decl);

  table->registerWithModule();

  return table;
}

SILDefaultOverrideTable *SILDefaultOverrideTable::define(
    SILModule &module, SILLinkage linkage, const ClassDecl *decl,
    ArrayRef<SILDefaultOverrideTable::Entry> entries) {
  auto *buffer = module.allocate<SILDefaultOverrideTable>(1);
  SILDefaultOverrideTable *table =
      ::new (buffer) SILDefaultOverrideTable(module, linkage, decl);
  table->define(entries);

  table->registerWithModule();

  // Return the resulting default witness table.
  return table;
}

void SILDefaultOverrideTable::registerWithModule() {
  assert(module.DefaultOverrideTableMap.find(decl) ==
         module.DefaultOverrideTableMap.end());

  module.DefaultOverrideTableMap[decl] = this;
  module.defaultOverrideTables.push_back(this);
}

std::string SILDefaultOverrideTable::getUniqueName() const {
  Mangle::ASTMangler Mangler(decl->getASTContext());
  return Mangler.mangleTypeWithoutPrefix(
      decl->getDeclaredInterfaceType()->getCanonicalType());
}

void SILDefaultOverrideTable::define(ArrayRef<Entry> entries) {
  assert(state == State::Declared);
  state = State::Defined;

  this->entries = module.allocateCopy(entries);

  // Retain referenced functions.
  for (auto entry : getEntries()) {
    entry.impl->incrementRefCount();
  }
}

SILDefaultOverrideTable::~SILDefaultOverrideTable() {
  // Release referenced functions.
  for (auto entry : getEntries()) {
    entry.impl->decrementRefCount();
  }
}
