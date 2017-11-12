//===--- SILDefaultWitnessTable.h -------------------------------*- C++ -*-===//
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
// This file defines the SILDefaultWitnessTable class, which is used to provide
// default implementations of protocol requirements for resilient protocols,
// allowing IRGen to generate the appropriate metadata so that the runtime can
// insert those requirements to witness tables that were emitted prior to the
// requirement being added.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILDEFAULTWITNESSTABLE_H
#define SWIFT_SIL_SILDEFAULTWITNESSTABLE_H

#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"
#include <string>

namespace swift {

class ProtocolDecl;
class SILFunction;
class SILModule;

/// A mapping from each requirement of a protocol to the SIL-level entity
/// satisfying the requirement for conformances which do not explicitly
/// provide a witness.
class SILDefaultWitnessTable : public llvm::ilist_node<SILDefaultWitnessTable>,
                               public SILAllocated<SILDefaultWitnessTable>
{
public:
  /// A default witness table entry describing the default witness for a method.
  class Entry {
    /// The method required.
    SILDeclRef Requirement;
    /// The witness for the method.
    /// This can be null in case no default implementation is available.
    SILFunction *Witness;
 
  public:
    Entry()
      : Requirement(), Witness(nullptr) {}
    
    Entry(SILDeclRef Requirement, SILFunction *Witness)
      : Requirement(Requirement), Witness(Witness) {}

    bool isValid() const {
      return !Requirement.isNull() && Witness;
    }

    const SILDeclRef &getRequirement() const {
      assert(isValid());
      return Requirement;
    }
    SILFunction *getWitness() const {
      assert(Witness != nullptr);
      return Witness;
    }
    void removeWitnessMethod() {
      if (Witness) {
        Witness->decrementRefCount();
      }
      Witness = nullptr;
    }
  };
 
private:
  /// The module which contains the SILDefaultWitnessTable.
  SILModule &Mod;

  /// The linkage of the witness table.
  SILLinkage Linkage;

  /// The protocol declaration to which this default witness table applies.
  const ProtocolDecl *Protocol;

  /// The minimum size of a valid witness table conforming to this protocol,
  /// with all resilient default requirements omitted.
  unsigned MinimumWitnessTableSizeInWords;

  /// The various witnesses containing in this default witness table.
  MutableArrayRef<Entry> Entries;

  /// Temporary state while SILGen is emitting a default witness table.
  /// We can never have a true declaration since there's no way to reference
  /// the default witness table from outside its defining translation unit.
  bool IsDeclaration;

  /// Private constructor for making SILDefaultWitnessTable declarations.
  SILDefaultWitnessTable(SILModule &M, SILLinkage Linkage,
                         const ProtocolDecl *Protocol);

  /// Private constructor for making SILDefaultWitnessTable definitions.
  SILDefaultWitnessTable(SILModule &M, SILLinkage Linkage,
                         const ProtocolDecl *Protocol,
                         ArrayRef<Entry> entries);

  void addDefaultWitnessTable();

public:
  /// Create a new SILDefaultWitnessTable declaration.
  static SILDefaultWitnessTable *create(SILModule &M, SILLinkage Linkage,
                                        const ProtocolDecl *Protocol);

  /// Create a new SILDefaultWitnessTable definition with the given entries.
  static SILDefaultWitnessTable *create(SILModule &M, SILLinkage Linkage,
                                        const ProtocolDecl *Protocol,
                                        ArrayRef<Entry> entries);

  Identifier getIdentifier() const;

  /// Get the linkage of the default witness table.
  SILLinkage getLinkage() const { return Linkage; }

  /// Set the linkage of the default witness table.
  void setLinkage(SILLinkage l) { Linkage = l; }

  void convertToDefinition(ArrayRef<Entry> entries);

  ~SILDefaultWitnessTable();

  /// Return true if this is a declaration with no body.
  bool isDeclaration() const { return IsDeclaration; }

  /// Return the AST ProtocolDecl this default witness table is associated with.
  const ProtocolDecl *getProtocol() const { return Protocol; }

  /// Clears methods in witness entries.
  /// \p predicate Returns true if the passed entry should be set to null.
  template <typename Predicate> void clearMethods_if(Predicate predicate) {
    for (Entry &entry : Entries) {
      if (!entry.isValid())
        continue;
      auto *MW = entry.getWitness();
      if (MW && predicate(MW)) {
        entry.removeWitnessMethod();
      }
    }
  }

  /// Return the minimum witness table size, in words.
  ///
  /// This will not change if requirements with default implementations are
  /// added at the end of the protocol.
  unsigned getMinimumWitnessTableSize() const;

  /// Return the default witness table size, in words.
  ///
  /// This is the number of resilient default entries that were known when the
  /// protocol definition was compiled; at runtime, it may be smaller or larger,
  /// so this should only be used when emitting metadata for the protocol
  /// definition itself.
  unsigned getDefaultWitnessTableSize() const {
    return Entries.size() - getMinimumWitnessTableSize();
  }

  /// Return all of the default witness table entries.
  ArrayRef<Entry> getEntries() const { return Entries; }

  /// Return all of the resilient default implementations.
  ///
  /// This is the array of witnesses actually emitted as part of the protocol's
  /// metadata; see the comment in getMinimumWitnessTableSize().
  ArrayRef<Entry> getResilientDefaultEntries() {
    return Entries.slice(getMinimumWitnessTableSize());
  }

  /// Verify that the default witness table is well-formed.
  void verify(const SILModule &M) const;
  
  /// Print the default witness table.
  void print(llvm::raw_ostream &OS, bool Verbose = false) const;

  /// Dump the default witness table to stderr.
  void dump() const;
};
  
} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for SILDefaultWitnessTable
//===----------------------------------------------------------------------===//

namespace llvm {
  
template <>
struct ilist_traits<::swift::SILDefaultWitnessTable> :
public ilist_default_traits<::swift::SILDefaultWitnessTable> {
  typedef ::swift::SILDefaultWitnessTable SILDefaultWitnessTable;

public:
  static void deleteNode(SILDefaultWitnessTable *WT) { WT->~SILDefaultWitnessTable(); }
  
private:
  void createNode(const SILDefaultWitnessTable &);
};

} // end llvm namespace

#endif
