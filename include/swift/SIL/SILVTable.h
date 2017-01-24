//===--- SILVTable.h - Defines the SILVTable class --------------*- C++ -*-===//
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
// for a dynamic type. This information is (FIXME will be) used by IRGen to lay
// out class vtables, and can be used by devirtualization passes to promote
// class_method instructions to static function_refs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILVTABLE_H
#define SWIFT_SIL_SILVTABLE_H

#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"
#include <algorithm>

namespace swift {

class ClassDecl;
class SILFunction;
class SILModule;

/// A mapping from each dynamically-dispatchable method of a class to the
/// SILFunction that implements the method for that class.
/// Note that dead methods are completely removed from the vtable.
class SILVTable : public llvm::ilist_node<SILVTable>,
                  public SILAllocated<SILVTable> {
public:
  // TODO: Entry should include substitutions needed to invoke an overridden
  // generic base class method.
  struct Entry {

    Entry() : Implementation(nullptr), Linkage(SILLinkage::Private) { }

    Entry(SILDeclRef Method, SILFunction *Implementation, SILLinkage Linkage) :
      Method(Method), Implementation(Implementation), Linkage(Linkage) { }

    /// The declaration reference to the least-derived method visible through
    /// the class.
    SILDeclRef Method;

    /// The function which implements the method for the class.
    SILFunction *Implementation;

    /// The linkage of the implementing function.
    ///
    /// This is usually the same as
    ///   stripExternalFromLinkage(Implementation->getLinkage())
    /// except if Implementation is a thunk (which has private or shared
    /// linkage).
    SILLinkage Linkage;
  };

  // Disallow copying into temporary objects.
  SILVTable(const SILVTable &other) = delete;
  SILVTable &operator=(const SILVTable &) = delete;

private:
  /// The ClassDecl mapped to this VTable.
  ClassDecl *Class;

  /// The number of SILVTables entries.
  unsigned NumEntries;

  /// Tail-allocated SILVTable entries.
  Entry Entries[1];

  /// Private constructor. Create SILVTables by calling SILVTable::create.
  SILVTable(ClassDecl *c, ArrayRef<Entry> entries);

public:
  ~SILVTable();

  /// Create a new SILVTable with the given method-to-implementation mapping.
  /// The SILDeclRef keys should reference the most-overridden members available
  /// through the class.
  static SILVTable *create(SILModule &M, ClassDecl *Class,
                           ArrayRef<Entry> Entries);

  /// Return the class that the vtable represents.
  ClassDecl *getClass() const { return Class; }

  /// Return all of the method entries.
  ArrayRef<Entry> getEntries() const { return {Entries, NumEntries}; }

  /// Look up the implementation function for the given method.
  SILFunction *getImplementation(SILModule &M, SILDeclRef method) const;

  /// Removes entries from the vtable.
  /// \p predicate Returns true if the passed entry should be removed.
  template <typename Predicate> void removeEntries_if(Predicate predicate) {
    Entry *end = std::remove_if(Entries, Entries + NumEntries,
                                [&](Entry &entry) -> bool {
      if (predicate(entry)) {
        entry.Implementation->decrementRefCount();
        removeFromVTableCache(entry);
        return true;
      }
      return false;
    });
    NumEntries = end - Entries;
  }
                    
  /// Verify that the vtable is well-formed for the given class.
  void verify(const SILModule &M) const;

  /// Print the vtable.
  void print(llvm::raw_ostream &OS, bool Verbose = false) const;
  void dump() const;

private:
  void removeFromVTableCache(Entry &entry);
};

} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for SILVTable
//===----------------------------------------------------------------------===//

namespace llvm {

template <>
struct ilist_traits<::swift::SILVTable> :
public ilist_default_traits<::swift::SILVTable> {
  typedef ::swift::SILVTable SILVTable;

  static void deleteNode(SILVTable *VT) { VT->~SILVTable(); }

private:
  void createNode(const SILVTable &);
};

} // end llvm namespace

#endif
