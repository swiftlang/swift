//===--- SILDefaultOverrideTable.h ------------------------------*- C++ -*-===//
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
// TLDR: class : protocol :: SILDefaultOverrideTable : SILDefaultWitnessTable
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
// +--Example--------------------------------------------------------------{{ -+
//
// ResilientFramework v1:
// open class C {
//   open var x: X {
//     _read {
//       ...
//     }
//   }
// }
//
// func useCOrDerived(_ c: C) {
//   // calls C.x._read
//   let x = C.x
// }
//
// ResilientFramework v2:
// open class C {
//   open var: x: X {
//     read {
//       ...
//     }
//     // compiler generated thunk
//     _read {
//       yield x.read() // this isn't actually expressible in Swift
//     }
//   }
// }
//
// func useCOrDerived(_ c: C) {
//   // calls C.x.read (!!!)
//   let x = C.x
//   pass(x)
// }
//
// The "semantic class member" here is the "reader" for C.x.  In 1, this member
// was implemented by C.x._read.  In 2.0, this was implemented by C.x.read.  In
// other words, when C.x is read from some instance `Instance` of C or a
// subclass (e.g. in useCOrDerived), the C.x.read routine of `Instance`'s class
// is used.  More concretely, the routine stored in the C.x.read slot from
// class(`Instance`)'s vtable is dispatched to.
//
// Without the solution provided SILDefaultOverrideTable, that slot could
// contain the wrong routine!, namely C's implementation of C.x.read when the
// subclass overrode C.x._read, as illustrated below.
//
// ClientBinary, built against version 1:
// class D : C {
//   override var x: X {
//     _read {
//       ...
//     }
//   }
// }
//
// When C.x._read is called on an instance of D, D's override of that routine is
// called.  When running ClientBinary against ResilientFramework v1, this is
// sufficient: useCOrDerived's reading of x dispatches to the routine in the
// x._read slot of the vtable, which D's vtable contains an override of.
//
// In detail, here's what happens at runtime: {{ // Runtime situation with v1
//
// // Pseudocode describing dispatch in ResilientFramework.useCOrDerived.
// func useCOrDerived(_ c: C) {
//   let vtable = typeof(c).VTable
//   let reader = vtable[#slot(x._read)]
//   let x = reader(c)
//   pass(x)
// }
//
// +- C.VTable ----------+
// | slot    | impl      |
// +---------+-----------+
// | x._read | C.x._read |
// +---------+-----------+
//
// +- D.VTable ----------+
// | slot    | impl      |
// +---------+-----------+
// | x._read | D.x._read |
// +---------+-----------+
//
// When an instance d of D is passed, execution will proceed thus:
//
// func useCOrDerived(_ c: C = d: D) {
//   let vtable = typeof(c).VTable = typeof(d).VTable = D.VTable
//   let reader = vtable[#slot(x._read)] = D.VTable[#slot(x._read)] = D.x._read
//   let x = reader(c) = D.x._read(d)
//   pass (x)
// }
//
// In other words, D's override of x._read will be looked up in D's vtable and
// invoked.
//
// }} // Runtime situation with v1
//
// Now, suppose no additional machinery were added to the runtime, and consider
// the same situation when running against ResilientFramework v2.  When running
// ClientBinary against ResilientFramework v2, useCOrDerived's reading of x
// dispatches to the routine in the x.read slot of the vtable (NOT the x._read
// slot).  And D's vtable contains no override of that routine!
//
// Here's the v2 runtime situation WITHOUT additional machinery: {{ // BAD runtime situation with v2
//
// // Pseudocode describing dispatch in ResilientFramework.useCOrDerived.
// func useCOrDerived(_ c: C) {
//   let vtable = typeof(c).VTable
//   let reader = vtable[#slot(x.read)] // NOTE! This is now x.read NOT x._read!
//   let x = reader(c)
//   pass(x)
// }
//
// +- C.VTable ----------+
// | slot    | impl      |
// +---------+-----------+
// | x._read | C.x._read |
// | x.read  | C.x.read  |
// +---------+-----------+
//
// +- D.VTable ----------+
// | slot    | impl      |
// +---------+-----------+
// | x._read | D.x._read |
// | x.read  | C.x.read  | <- The bad entry!
// +---------+-----------+
//
// When an instance d of D is passed, execution will proceed thus:
//
// func useCOrDerived(_ c: C = d: D) {
//   let vtable = typeof(c).VTable = typeof(d).VTable = D.VTable
//   // The wrong implementation is looked up!
//   let reader = vtable[#slot(x.read)] = D.VTable[#slot(x.read)] = C.x.read
//   // The wrong implementation is called!
//   let x = reader(c) = C.x._read(d)
//   pass (x)
// }
//
// In other words, D's override of x._read is ignored, and C's implementation is
// used.
//
// }} // BAD runtime situation with v2
//
// As described so far, this problem has a few solutions.  Those not taken are
// mentioned now with their reason for rejection.
//
// Rejected solutions:
//
// (a) Don't change the slot dispatched to by useCOrDerived.
//
//     Rejection rationale: The new routine C.x.read has an improved ABI.
//
// (b) Reuse D.x._read in the x.read slot.
//
//     Rejection rationale: D.x._read has the ABI appropriate for x._read and
//                          not the ABI appropriate for x.read.
//
// Accepted solution:
//
// Provide an implementation of x.read for use by D.  The implementation is as
// follows, in pseudo-code that can't be written in Swift:
//
// ResilientFrameworkv2:
// func C_x_read_default_override(self: C) yields_once -> X {
//   yield self.x._read // vtable dispatch
// }
//
// At runtime, when the VTable for D is assembled (which occurs at runtime
// because D's is a subclass of C which is defined in a resilient framework),
// this default override is slotted into D's VTable for x.read:
//
// +- D.VTable --------------------------+
// | slot    | impl                      |
// +---------+---------------------------+
// | x._read | D.x._read                 |
// | x.read  | C_x_read_default_override |
// +---------+---------------------------+
//
// +-}} Example ---------------------------------------------------------------+
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILDEFAULTOVERRIDETABLE_H
#define SWIFT_SIL_SILDEFAULTOVERRIDETABLE_H

#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILDeclRef.h"
#include "llvm/ADT/ilist.h"

namespace swift {

struct PrintOptions;

/// Map the pair of overridable member and semantically equivalent overridable
/// member to the SIL-level entity used to override the former for subclasses
/// which do override the latter.
///
/// In the example above, an entry in the table would be
///
///     (C.x.read, C.x._read) -> C_x_read_default_override.
class SILDefaultOverrideTable
    : public llvm::ilist_node<SILDefaultOverrideTable>,
      public SILAllocated<SILDefaultOverrideTable> {
public:
  struct Entry {
    /// The method for which a default implementation may be needed by
    /// subclasses in old clients.
    SILDeclRef method;
    /// The method the presence of whose override in subclasses in old clients
    /// necessitates the provision of the default override.
    SILDeclRef original;
    /// The default override of `method` to be provided when `original` is
    /// present.
    SILFunction *impl;

    /// Print the entry.
    void print(llvm::raw_ostream &os, bool verbose = false) const;

    /// Dump the entry to stderr.
    void dump() const;
  };

private:
  friend class SILModule;

  /// The containing module.
  SILModule &module;

  /// The linkage of the override table.
  SILLinkage linkage;

  /// The ClassDecl this table pertains to.
  const ClassDecl *decl;

  /// The contents of the table.
  MutableArrayRef<Entry> entries;

  enum class State {
    Declared,
    Defined,
  };
  /// The table's current state.  This must eventually become ::Defined.
  State state;

  /// Private constructor for making SILDefaultOverrideTable declarations.
  SILDefaultOverrideTable(SILModule &M, SILLinkage Linkage,
                          const ClassDecl *decl);

  void registerWithModule();

public:
  /// Declare a new table.
  static SILDefaultOverrideTable *declare(SILModule &module, SILLinkage linkage,
                                          const ClassDecl *decl);

  /// Define a new table.
  static SILDefaultOverrideTable *define(SILModule &module, SILLinkage linkage,
                                         const ClassDecl *decl,
                                         ArrayRef<Entry> entries);

  /// A name unique among override tables but not symbols.
  std::string getUniqueName() const;

  /// Get the linkage of the default override table.
  SILLinkage getLinkage() const { return linkage; }

  /// Set the linkage of the default override table.
  void setLinkage(SILLinkage linkage) { this->linkage = linkage; }

  /// Promote the current table from a declaration to a definition consisting of
  /// the specified entries.
  void define(ArrayRef<Entry> entries);

  ~SILDefaultOverrideTable();

  SILModule &getModule() const { return module; }

  /// Return true if this is a declaration with no body.
  bool isDeclaration() const { return state == State::Declared; }

  /// Return the ClassDecl this table pertains to.
  const ClassDecl *getClass() const { return decl; }

  /// Return all of the default override table entries.
  ArrayRef<Entry> getEntries() const { return entries; }

  /// Verify that the default override table is well-formed.
  void verify(const SILModule &M) const;

  /// Print the default override table.
  void print(llvm::raw_ostream &os, bool verbose = false) const;

  /// Dump the default override table to stderr.
  void dump() const;
};

} // namespace swift

//===----------------------------------------------------------------------===//
// ilist_traits for SILDefaultOverrideTable
//===----------------------------------------------------------------------===//

namespace llvm {

template <>
struct ilist_traits<::swift::SILDefaultOverrideTable>
    : public ilist_node_traits<::swift::SILDefaultOverrideTable> {
  using SILDefaultOverrideTable = ::swift::SILDefaultOverrideTable;

public:
  static void deleteNode(SILDefaultOverrideTable *WT) {
    WT->~SILDefaultOverrideTable();
  }

private:
  void createNode(const SILDefaultOverrideTable &);
};

} // namespace llvm

#endif
