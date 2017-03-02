//===--- SILWitnessTable.h - Defines the SILWitnessTable class --*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILWITNESSTABLE_H
#define SWIFT_SIL_SILWITNESSTABLE_H

#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"
#include <string>

namespace swift {

class SILFunction;
class SILModule;
class NormalProtocolConformance;

/// A mapping from each requirement of a protocol to the SIL-level entity
/// satisfying the requirement for a concrete type.
class SILWitnessTable : public llvm::ilist_node<SILWitnessTable>,
                        public SILAllocated<SILWitnessTable>
{
public:
  /// A witness table entry describing the witness for a method.
  struct MethodWitness {
    /// The method required.
    SILDeclRef Requirement;
    /// The witness for the method.
    /// This can be null in case dead function elimination has removed the method.
    SILFunction *Witness;
  };
  
  /// A witness table entry describing the witness for an associated type.
  struct AssociatedTypeWitness {
    /// The associated type required.
    AssociatedTypeDecl *Requirement;
    /// The concrete semantic type of the witness.
    CanType Witness;
  };
  
  /// A witness table entry describing the witness for an associated type's
  /// protocol requirement.
  struct AssociatedTypeProtocolWitness {
    /// The associated type required.  A dependent type in the protocol's
    /// context.
    CanType Requirement;
    /// The protocol requirement on the type.
    ProtocolDecl *Protocol;
    /// The ProtocolConformance satisfying the requirement. Null if the
    /// conformance is dependent.
    ProtocolConformanceRef Witness;
  };
  
  /// A witness table entry referencing the protocol conformance for a refined
  /// base protocol.
  struct BaseProtocolWitness {
    /// The base protocol.
    ProtocolDecl *Requirement;
    /// The ProtocolConformance for the base protocol.
    ProtocolConformance *Witness;
  };
                          
  /// A witness table entry for an optional requirement that is not present.
  struct MissingOptionalWitness {
    /// The witness for the optional requirement that wasn't present.
    ValueDecl *Witness;
  };
  
  /// A witness table entry kind.
  enum WitnessKind {
    Invalid,
    Method,
    AssociatedType,
    AssociatedTypeProtocol,
    BaseProtocol,
    MissingOptional
  };
  
  /// A witness table entry.
  class Entry {
    WitnessKind Kind;
    union {
      MethodWitness Method;
      AssociatedTypeWitness AssociatedType;
      AssociatedTypeProtocolWitness AssociatedTypeProtocol;
      BaseProtocolWitness BaseProtocol;
      MissingOptionalWitness MissingOptional;
    };
    
  public:
    Entry() : Kind(WitnessKind::Invalid) {}
    
    Entry(const MethodWitness &Method)
      : Kind(WitnessKind::Method), Method(Method)
    {}
    
    Entry(const AssociatedTypeWitness &AssociatedType)
      : Kind(WitnessKind::AssociatedType), AssociatedType(AssociatedType)
    {}
    
    Entry(const AssociatedTypeProtocolWitness &AssociatedTypeProtocol)
      : Kind(WitnessKind::AssociatedTypeProtocol),
        AssociatedTypeProtocol(AssociatedTypeProtocol)
    {}
    
    Entry(const BaseProtocolWitness &BaseProtocol)
      : Kind(WitnessKind::BaseProtocol),
        BaseProtocol(BaseProtocol)
    {}
    
    Entry(const MissingOptionalWitness &MissingOptional)
      : Kind(WitnessKind::MissingOptional), MissingOptional(MissingOptional) {
    }
    
    WitnessKind getKind() const { return Kind; }
    
    const MethodWitness &getMethodWitness() const {
      assert(Kind == WitnessKind::Method);
      return Method;
    }
    const AssociatedTypeWitness &getAssociatedTypeWitness() const {
      assert(Kind == WitnessKind::AssociatedType);
      return AssociatedType;
    }
    const AssociatedTypeProtocolWitness &
    getAssociatedTypeProtocolWitness() const {
      assert(Kind == WitnessKind::AssociatedTypeProtocol);
      return AssociatedTypeProtocol;
    }
    const BaseProtocolWitness &getBaseProtocolWitness() const {
      assert(Kind == WitnessKind::BaseProtocol);
      return BaseProtocol;
    }
    
    const MissingOptionalWitness &getMissingOptionalWitness() const {
      assert(Kind == WitnessKind::MissingOptional);
      return MissingOptional;
    }

    void removeWitnessMethod() {
      assert(Kind == WitnessKind::Method);
      if (Method.Witness) {
        Method.Witness->decrementRefCount();
      }
      Method.Witness = nullptr;
    }
  };
  
private:
  /// The module which contains the SILWitnessTable.
  SILModule &Mod;

  /// The symbol name of the witness table that will be propagated to the object
  /// file level.
  StringRef Name;

  /// The linkage of the witness table.
  SILLinkage Linkage;

  /// The conformance mapped to this witness table.
  NormalProtocolConformance *Conformance;

  /// The various witnesses containing in this witness table. Is empty if the
  /// table has no witness entries or if it is a declaration.
  MutableArrayRef<Entry> Entries;

  /// Whether or not this witness table is a declaration. This is separate from
  /// whether or not entries is empty since you can have an empty witness table
  /// that is not a declaration.
  bool IsDeclaration;
 
  /// Whether or not this witness table is fragile. Fragile means that the
  /// table may be serialized and "inlined" into another module.
  bool IsFragile;

  /// Private constructor for making SILWitnessTable definitions.
  SILWitnessTable(SILModule &M, SILLinkage Linkage,
                  bool IsFragile, StringRef Name,
                  NormalProtocolConformance *Conformance,
                  ArrayRef<Entry> entries);

  /// Private constructor for making SILWitnessTable declarations.
  SILWitnessTable(SILModule &M, SILLinkage Linkage, StringRef Name,
                  NormalProtocolConformance *Conformance);

  void addWitnessTable();

public:
  /// Create a new SILWitnessTable definition with the given entries.
  static SILWitnessTable *create(SILModule &M, SILLinkage Linkage,
                                 bool IsFragile,
                                 NormalProtocolConformance *Conformance,
                                 ArrayRef<Entry> entries);

  /// Create a new SILWitnessTable declaration.
  static SILWitnessTable *create(SILModule &M, SILLinkage Linkage,
                                 NormalProtocolConformance *Conformance);

  ~SILWitnessTable();
  
  /// Return the AST ProtocolConformance this witness table represents.
  NormalProtocolConformance *getConformance() const { return Conformance; }

  /// Return the symbol name of the witness table that will be propagated to the
  /// object file level.
  StringRef getName() const { return Name; }

  /// Return the symbol name of the witness table that will be propagated to the
  /// object file as an Identifier.
  Identifier getIdentifier() const;

  /// Returns true if this witness table is a declaration.
  bool isDeclaration() const { return IsDeclaration; }

  /// Returns true if this witness table is a definition.
  bool isDefinition() const { return !isDeclaration(); }
 
  /// Returns true if this witness table is fragile.
  bool isFragile() const { return IsFragile; }

  /// Return all of the witness table entries.
  ArrayRef<Entry> getEntries() const { return Entries; }

  /// Clears methods in MethodWitness entries.
  /// \p predicate Returns true if the passed entry should be set to null.
  template <typename Predicate> void clearMethods_if(Predicate predicate) {
    for (Entry &entry : Entries) {
      if (entry.getKind() == WitnessKind::Method) {
        const MethodWitness &MW = entry.getMethodWitness();
        if (MW.Witness && predicate(MW)) {
          entry.removeWitnessMethod();
        }
      }
    }
  }
  
  /// Verify that the witness table is well-formed.
  void verify(const SILModule &M) const;
  
  /// Get the linkage of the witness table.
  SILLinkage getLinkage() const { return Linkage; }

  /// Set the linkage of the witness table.
  void setLinkage(SILLinkage l) { Linkage = l; }

  /// Change a SILWitnessTable declaration into a SILWitnessTable definition.
  void convertToDefinition(ArrayRef<Entry> newEntries, bool isFragile);

  /// Print the witness table.
  void print(llvm::raw_ostream &OS, bool Verbose = false) const;

  /// Dump the witness table to stderr.
  void dump() const;
};
  
} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for SILWitnessTable
//===----------------------------------------------------------------------===//

namespace llvm {
  
template <>
struct ilist_traits<::swift::SILWitnessTable> :
public ilist_default_traits<::swift::SILWitnessTable> {
  typedef ::swift::SILWitnessTable SILWitnessTable;

public:
  static void deleteNode(SILWitnessTable *WT) { WT->~SILWitnessTable(); }
  
private:
  void createNode(const SILWitnessTable &);
};

} // end llvm namespace

#endif
