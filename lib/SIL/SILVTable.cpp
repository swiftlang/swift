//===--- SILVTable.cpp - Defines the SILVTable class ----------------------===//
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
                             ArrayRef<Entry> Entries) {
  // SILVTable contains one element declared in Entries.  We must allocate
  // space for it, because its default ctor will write to it.
  unsigned NumTailElements = std::max((unsigned)Entries.size(), 1U)-1;
  void *buf = M.allocate(sizeof(SILVTable) + sizeof(Entry) * NumTailElements,
                         alignof(SILVTable));
  SILVTable *vt = ::new (buf) SILVTable(Class, Entries);
  M.vtables.push_back(vt);
  M.VTableMap[Class] = vt;
  // Update the Module's cache with new vtable + vtable entries:
  for (const Entry &entry : Entries) {
    M.VTableEntryCache.insert({{vt, entry.Method}, entry.Implementation});
  }
  return vt;
}

SILFunction *
SILVTable::getImplementation(SILModule &M, SILDeclRef method) const {
  SILDeclRef m = method;
  do {
    auto entryIter = M.VTableEntryCache.find({this, m});
    if (entryIter != M.VTableEntryCache.end()) {
      return (*entryIter).second;
    }
  } while ((m = m.getOverridden()));
  return nullptr;
}

void SILVTable::removeFromVTableCache(Entry &entry) {
  SILModule &M = entry.Implementation->getModule();
  M.VTableEntryCache.erase({this, entry.Method});
}

SILVTable::SILVTable(ClassDecl *c, ArrayRef<Entry> entries)
  : Class(c), NumEntries(entries.size())
{
  memcpy(Entries, entries.begin(), sizeof(Entry) * NumEntries);
  
  // Bump the reference count of functions referenced by this table.
  for (const Entry &entry : getEntries()) {
    entry.Implementation->incrementRefCount();
  }
}

SILVTable::~SILVTable() {
  // Drop the reference count of functions referenced by this table.
  for (const Entry &entry : getEntries()) {
    entry.Implementation->decrementRefCount();
  }
}
