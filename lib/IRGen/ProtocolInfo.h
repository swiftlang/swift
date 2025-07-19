//===--- ProtocolInfo.h - Abstract protocol witness layout ------*- C++ -*-===//
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
// This file defines types for representing the abstract layout of a
// protocol.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_PROTOCOLINFO_H
#define SWIFT_IRGEN_PROTOCOLINFO_H

#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolAssociations.h"

#include "swift/IRGen/ValueWitness.h"
#include "WitnessIndex.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {
  class CanType;
  class ProtocolConformance;

namespace irgen {
  class IRGenModule;
  class TypeInfo;

/// A witness to a specific element of a protocol.  Every
/// ProtocolTypeInfo stores one of these for each requirement
/// introduced by the protocol.
class WitnessTableEntry {
  enum WitnessKind {
    PlaceholderKind,
    OutOfLineBaseKind,
    MethodKind,
    AssociatedTypeKind,
    AssociatedConformanceKind
  };

  struct OutOfLineBaseWitness {
    ProtocolDecl *Protocol;
  };

  struct MethodWitness {
    SILDeclRef Witness;
  };

  struct AssociatedTypeWitness {
    AssociatedTypeDecl *Association;
  };

  struct AssociatedConformanceWitness {
    TypeBase *AssociatedType;
    ProtocolDecl *Protocol;
  };

  WitnessKind Kind;
  union {
    OutOfLineBaseWitness OutOfLineBaseEntry;
    MethodWitness MethodEntry;
    AssociatedTypeWitness AssociatedTypeEntry;
    AssociatedConformanceWitness AssociatedConformanceEntry;
  };

  WitnessTableEntry(WitnessKind Kind) : Kind(Kind) {}

public:
  static WitnessTableEntry forPlaceholder() {
    return WitnessTableEntry(WitnessKind::PlaceholderKind);
  }

  static WitnessTableEntry forOutOfLineBase(ProtocolDecl *proto) {
    assert(proto != nullptr);
    WitnessTableEntry entry(WitnessKind::OutOfLineBaseKind);
    entry.OutOfLineBaseEntry = {proto};
    return entry;
  }

  /// Is this a base-protocol entry?
  bool isBase() const { return Kind == WitnessKind::OutOfLineBaseKind; }

  bool matchesBase(ProtocolDecl *proto) const {
    assert(proto != nullptr);
    return isBase() && OutOfLineBaseEntry.Protocol == proto;
  }

  /// Given that this is a base-protocol entry, is the table
  /// "out of line"?
  bool isOutOfLineBase() const {
    assert(isBase());
    return true;
  }

  ProtocolDecl *getBase() const {
    assert(isBase());
    return OutOfLineBaseEntry.Protocol;
  }

  static WitnessTableEntry forFunction(SILDeclRef declRef) {
    assert(!declRef.isNull());
    WitnessTableEntry entry(WitnessKind::MethodKind);
    entry.MethodEntry = {declRef};
    return entry;
  }

  bool isFunction() const { return Kind == WitnessKind::MethodKind; }

  bool matchesFunction(SILDeclRef declRef) const {
    return isFunction() && MethodEntry.Witness == declRef;
  }

  SILDeclRef getFunction() const {
    assert(isFunction());
    return MethodEntry.Witness;
  }

  static WitnessTableEntry forAssociatedType(AssociatedTypeDecl *assocType) {
    WitnessTableEntry entry(WitnessKind::AssociatedTypeKind);
    entry.AssociatedTypeEntry = {assocType};
    return entry;
  }
  
  bool isAssociatedType() const {
    return Kind == WitnessKind::AssociatedTypeKind;
  }

  bool matchesAssociatedType(AssociatedTypeDecl *assocType) const {
    return isAssociatedType() && AssociatedTypeEntry.Association == assocType;
  }

  AssociatedTypeDecl *getAssociatedType() const {
    assert(isAssociatedType());
    return AssociatedTypeEntry.Association;
  }

  static WitnessTableEntry
  forAssociatedConformance(AssociatedConformance conf) {
    WitnessTableEntry entry(WitnessKind::AssociatedConformanceKind);
    entry.AssociatedConformanceEntry = {conf.getAssociation().getPointer(),
                                        conf.getAssociatedRequirement()};
    return entry;
  }

  bool isAssociatedConformance() const {
    return Kind == WitnessKind::AssociatedConformanceKind;
  }

  bool matchesAssociatedConformance(const AssociatedConformance &conf) const {
    return isAssociatedConformance() &&
           AssociatedConformanceEntry.AssociatedType ==
               conf.getAssociation().getPointer() &&
           AssociatedConformanceEntry.Protocol ==
               conf.getAssociatedRequirement();
  }

  CanType getAssociatedConformancePath() const {
    assert(isAssociatedConformance());
    return CanType(AssociatedConformanceEntry.AssociatedType);
  }

  ProtocolDecl *getAssociatedConformanceRequirement() const {
    assert(isAssociatedConformance());
    return AssociatedConformanceEntry.Protocol;
  }

  friend bool operator==(WitnessTableEntry left, WitnessTableEntry right) {
    if (left.Kind != right.Kind)
      return false;
    switch (left.Kind) {
    case WitnessKind::PlaceholderKind:
      return true;
    case WitnessKind::OutOfLineBaseKind:
      return left.OutOfLineBaseEntry.Protocol ==
             right.OutOfLineBaseEntry.Protocol;
    case WitnessKind::MethodKind:
      return left.MethodEntry.Witness == right.MethodEntry.Witness;
    case WitnessKind::AssociatedTypeKind:
      return left.AssociatedTypeEntry.Association ==
             right.AssociatedTypeEntry.Association;
    case WitnessKind::AssociatedConformanceKind:
      return left.AssociatedConformanceEntry.AssociatedType ==
                 right.AssociatedConformanceEntry.AssociatedType &&
             left.AssociatedConformanceEntry.Protocol ==
                 right.AssociatedConformanceEntry.Protocol;
    }
    llvm_unreachable("invalid witness kind");
  }
};

/// Describes the information available in a ProtocolInfo.
///
/// Each kind includes the information of the kinds before it.
enum class ProtocolInfoKind : uint8_t {
  RequirementSignature,
  Full
};

/// An abstract description of a protocol.
class ProtocolInfo final :
    private llvm::TrailingObjects<ProtocolInfo, WitnessTableEntry> {
  friend TrailingObjects;
  friend class TypeConverter;

  /// The number of table entries in this protocol layout.
  unsigned NumTableEntries;

  ProtocolInfoKind Kind;

  ProtocolInfoKind getKind() const {
    return Kind;
  }

  ProtocolInfo(ArrayRef<WitnessTableEntry> table, ProtocolInfoKind kind)
      : NumTableEntries(table.size()), Kind(kind) {
    std::uninitialized_copy(table.begin(), table.end(), getTrailingObjects());
  }

  static std::unique_ptr<ProtocolInfo> create(ArrayRef<WitnessTableEntry> table,
                                              ProtocolInfoKind kind);

public:
  /// The number of witness slots in a conformance to this protocol;
  /// in other words, the size of the table in words.
  unsigned getNumWitnesses() const {
    assert(getKind() == ProtocolInfoKind::Full);
    return NumTableEntries;
  }

  /// Return all of the entries in this protocol witness table.
  ///
  /// The addresses of the entries in this array can be passed to
  /// getBaseWitnessIndex/getNonBaseWitnessIndex, below.
  ArrayRef<WitnessTableEntry> getWitnessEntries() const {
    return getTrailingObjects(NumTableEntries);
  }

  /// Given the address of a witness entry from this PI for a base protocol
  /// conformance, return its witness index.
  WitnessIndex getBaseWitnessIndex(const WitnessTableEntry *witness) const {
    assert(witness && witness->isBase());
    auto entries = getWitnessEntries();
    assert(entries.begin() <= witness && witness < entries.end() &&
           "argument witness entry does not belong to this ProtocolInfo");
    if (witness->isOutOfLineBase()) {
      return WitnessIndex(witness - entries.begin(), false);
    } else {
      return WitnessIndex(0, true);
    }
  }

  /// Given the address of a witness entry from this PI for a non-base
  /// witness, return its witness index.
  WitnessIndex getNonBaseWitnessIndex(const WitnessTableEntry *witness) const {
    assert(witness && !witness->isBase());
    auto entries = getWitnessEntries();
    assert(entries.begin() <= witness && witness < entries.end());
    return WitnessIndex(witness - entries.begin(), false);
  }

  /// Return the witness index for the protocol conformance pointer
  /// for the given base protocol requirement.
  WitnessIndex getBaseIndex(ProtocolDecl *protocol) const {
    for (auto &witness : getWitnessEntries()) {
      if (witness.matchesBase(protocol))
        return getBaseWitnessIndex(&witness);
    }
    llvm_unreachable("didn't find entry for base");
  }

  /// Return the witness index for the witness function for the given
  /// function requirement.
  WitnessIndex getFunctionIndex(SILDeclRef declRef) const {
    assert(getKind() >= ProtocolInfoKind::Full);
    for (auto &witness : getWitnessEntries()) {
      if (witness.matchesFunction(declRef))
        return getNonBaseWitnessIndex(&witness);
    }
    llvm_unreachable("didn't find entry for function");
  }

  /// Return the witness index for the type metadata access function
  /// for the given associated type.
  WitnessIndex getAssociatedTypeIndex(IRGenModule &IGM,
                                      AssociatedTypeDecl *assocType) const;

  /// Return the witness index for the protocol witness table access
  /// function for the given associated protocol conformance.
  WitnessIndex
  getAssociatedConformanceIndex(const AssociatedConformance &conf) const {
    for (auto &witness : getWitnessEntries()) {
      if (witness.matchesAssociatedConformance(conf))
        return getNonBaseWitnessIndex(&witness);
    }
    llvm_unreachable("didn't find entry for associated conformance");
  }
};

/// Detail about how an object conforms to a protocol.
class ConformanceInfo {
  virtual void anchor();
public:
  virtual ~ConformanceInfo() = default;
  virtual llvm::Value *getTable(IRGenFunction &IGF,
                               llvm::Value **conformingMetadataCache) const = 0;
  /// Try to get this table as a constant pointer.  This might just
  /// not be supportable at all.
  virtual llvm::Constant *tryGetConstantTable(IRGenModule &IGM,
                                              CanType conformingType) const = 0;
};

} // end namespace irgen
} // end namespace swift

#endif
