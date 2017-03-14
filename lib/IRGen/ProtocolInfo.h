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

#include "ValueWitness.h"
#include "WitnessIndex.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {
  class CanType;
  class Decl;
  class ProtocolConformance;
  class ProtocolDecl;

namespace irgen {
  class ConformanceInfo; // private to GenProto.cpp
  class IRGenModule;
  class TypeInfo;

/// A witness to a specific element of a protocol.  Every
/// ProtocolTypeInfo stores one of these for each requirement
/// introduced by the protocol.
class WitnessTableEntry {
public:
  void *MemberOrAssociatedType;
  ProtocolDecl *Protocol;

  WitnessTableEntry(void *member, ProtocolDecl *protocol)
    : MemberOrAssociatedType(member), Protocol(protocol) {}

public:
  WitnessTableEntry() = default;

  static WitnessTableEntry forOutOfLineBase(ProtocolDecl *proto) {
    assert(proto != nullptr);
    return WitnessTableEntry(nullptr, proto);
  }

  /// Is this a base-protocol entry?
  bool isBase() const { return MemberOrAssociatedType == nullptr; }

  bool matchesBase(ProtocolDecl *proto) const {
    assert(proto != nullptr);
    return MemberOrAssociatedType == nullptr && Protocol == proto;
  }

  /// Given that this is a base-protocol entry, is the table
  /// "out of line"?
  bool isOutOfLineBase() const {
    assert(isBase());
    return true;
  }

  ProtocolDecl *getBase() const {
    assert(isBase());
    return Protocol;
  }

  static WitnessTableEntry forFunction(AbstractFunctionDecl *func) {
    assert(func != nullptr);
    return WitnessTableEntry(func, nullptr);
  }
  
  bool isFunction() const {
    return Protocol == nullptr &&
           isa<AbstractFunctionDecl>(
             static_cast<Decl*>(MemberOrAssociatedType));
  }

  bool matchesFunction(AbstractFunctionDecl *func) const {
    assert(func != nullptr);
    return MemberOrAssociatedType == func && Protocol == nullptr;
  }

  AbstractFunctionDecl *getFunction() const {
    assert(isFunction());
    return static_cast<AbstractFunctionDecl*>(MemberOrAssociatedType);
  }

  static WitnessTableEntry forAssociatedType(AssociatedTypeDecl *ty) {
    return WitnessTableEntry(ty, nullptr);
  }
  
  bool isAssociatedType() const {
    return Protocol == nullptr &&
           isa<AssociatedTypeDecl>(
             static_cast<Decl*>(MemberOrAssociatedType));
  }

  bool matchesAssociatedType(AssociatedTypeDecl *assocType) const {
    assert(assocType != nullptr);
    return MemberOrAssociatedType == assocType && Protocol == nullptr;
  }

  AssociatedTypeDecl *getAssociatedType() const {
    assert(isAssociatedType());
    return static_cast<AssociatedTypeDecl*>(MemberOrAssociatedType);
  }

  static WitnessTableEntry forAssociatedConformance(CanType path,
                                                    ProtocolDecl *requirement) {
    assert(path && requirement != nullptr);
    return WitnessTableEntry(path.getPointer(), requirement);
  }

  bool isAssociatedConformance() const {
    return Protocol != nullptr && MemberOrAssociatedType != nullptr;
  }

  bool matchesAssociatedConformance(CanType path,
                                    ProtocolDecl *requirement) const {
    assert(path && requirement != nullptr);
    return MemberOrAssociatedType == path.getPointer() &&
           Protocol == requirement;
  }

  CanType getAssociatedConformancePath() const {
    assert(isAssociatedConformance());
    return CanType(static_cast<TypeBase *>(MemberOrAssociatedType));
  }

  ProtocolDecl *getAssociatedConformanceRequirement() const {
    assert(isAssociatedConformance());
    return Protocol;
  }
};

/// An abstract description of a protocol.
class ProtocolInfo final :
    private llvm::TrailingObjects<ProtocolInfo, WitnessTableEntry> {
  friend TrailingObjects;

  /// A singly-linked-list of all the protocols that have been laid out.
  const ProtocolInfo *NextConverted;
  friend class TypeConverter;

  /// The number of table entries in this protocol layout.
  unsigned NumTableEntries;

  /// A table of all the conformances we've needed so far for this
  /// protocol.  We expect this to be quite small for most protocols.
  mutable llvm::SmallDenseMap<const ProtocolConformance*, ConformanceInfo*, 2>
    Conformances;

  ProtocolInfo(ArrayRef<WitnessTableEntry> table)
      : NumTableEntries(table.size()) {
    std::uninitialized_copy(table.begin(), table.end(),
                            getTrailingObjects<WitnessTableEntry>());
  }

  static ProtocolInfo *create(ArrayRef<WitnessTableEntry> table);

public:
  const ConformanceInfo &getConformance(IRGenModule &IGM,
                                        ProtocolDecl *protocol,
                                        const ProtocolConformance *conf) const;

  /// The number of witness slots in a conformance to this protocol;
  /// in other words, the size of the table in words.
  unsigned getNumWitnesses() const {
    return NumTableEntries;
  }

  /// Return all of the entries in this protocol witness table.
  ///
  /// The addresses of the entries in this array can be passed to
  /// getBaseWitnessIndex/getNonBaseWitnessIndex, below.
  ArrayRef<WitnessTableEntry> getWitnessEntries() const {
    return {getTrailingObjects<WitnessTableEntry>(), NumTableEntries};
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
  WitnessIndex getFunctionIndex(AbstractFunctionDecl *function) const {
    for (auto &witness : getWitnessEntries()) {
      if (witness.matchesFunction(function))
        return getNonBaseWitnessIndex(&witness);
    }
    llvm_unreachable("didn't find entry for function");
  }

  /// Return the witness index for the type metadata access function
  /// for the given associated type.
  WitnessIndex getAssociatedTypeIndex(AssociatedTypeDecl *assocType) const {
    for (auto &witness : getWitnessEntries()) {
      if (witness.matchesAssociatedType(assocType))
        return getNonBaseWitnessIndex(&witness);
    }
    llvm_unreachable("didn't find entry for associated type");
  }

  /// Return the witness index for the protocol witness table access
  /// function for the given associated protocol conformance.
  WitnessIndex getAssociatedConformanceIndex(CanType path,
                                             ProtocolDecl *requirement) const {
    for (auto &witness : getWitnessEntries()) {
      if (witness.matchesAssociatedConformance(path, requirement))
        return getNonBaseWitnessIndex(&witness);
    }
    llvm_unreachable("didn't find entry for associated conformance");
  }

  ~ProtocolInfo();
};

} // end namespace irgen
} // end namespace swift

#endif
