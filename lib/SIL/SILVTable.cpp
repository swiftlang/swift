//===--- SILVTable.cpp - Defines the SILVTable class ----------------------===//
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
// This file defines the SILVTable class, which is used to map dynamically
// dispatchable class methods and properties to their concrete implementations
// for a dynamic type. This information (FIXME) will be used by IRGen to lay
// out class vtables, and can be used by devirtualization passes to lower
// class_method instructions to static function_refs.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILVTable *SILVTable::create(SILModule &M, ClassDecl *Class,
                             ArrayRef<Pair> Entries) {
  // SILVTable contains one element declared in Entries.  We must allocate
  // space for it, because its default ctor will write to it.
  unsigned NumTailElements = std::max((unsigned)Entries.size(), 1U)-1;
  void *buf = M.allocate(sizeof(SILVTable) + sizeof(Pair) * NumTailElements,
                         alignof(SILVTable));
  SILVTable *vt = ::new (buf) SILVTable(Class, Entries);
  M.vtables.push_back(vt);
  M.VTableMap[Class] = vt;
  return vt;
}

SILFunction *
SILVTable::getImplementation(SILModule &M, SILDeclRef method) const {
  // FIXME: We should build a sidetable cache in the module. Linear lookup here
  // is lame.
  
  for (auto &entry : getEntries()) {
    // Check whether this mapping matches either the given decl directly or
    // one of its overridden decl.
    SILDeclRef m = method;
    do {
      if (entry.first == m)
        return entry.second;
    } while ((m = m.getOverridden()));
  }
  
  return nullptr;
}

SILVTable::SILVTable(ClassDecl *c, ArrayRef<Pair> entries)
  : Class(c), NumEntries(entries.size())
{
  memcpy(Entries, entries.begin(), sizeof(Pair) * NumEntries);
  
  // Bump the reference count of functions referenced by this table.
  for (auto entry : getEntries()) {
    entry.second->incrementRefCount();
  }
}

SILVTable::~SILVTable() {
  // Drop the reference count of functions referenced by this table.
  for (auto entry : getEntries()) {
    entry.second->decrementRefCount();
  }
}
