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
public:
  llvm::PointerUnion<Decl *, TypeBase *> MemberOrAssociatedType;
  ProtocolDecl *Protocol;
  SILDeclRef declRef;

  WitnessTableEntry(llvm::PointerUnion<Decl *, TypeBase *> member,
                    ProtocolDecl *protocol, SILDeclRef declRef)
    : MemberOrAssociatedType(member), Protocol(protocol), declRef(declRef) {}

public:
  WitnessTableEntry() = default;

  static WitnessTableEntry forOutOfLineBase(ProtocolDecl *proto) {
    assert(proto != nullptr);
    return WitnessTableEntry({}, proto, SILDeclRef());
  }

  /// Is this a base-protocol entry?
  bool isBase() const { return MemberOrAssociatedType.isNull(); }

  bool matchesBase(ProtocolDecl *proto) const {
    assert(proto != nullptr);
    return MemberOrAssociatedType.isNull() && Protocol == proto;
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

  static WitnessTableEntry forFunction(SILDeclRef declRef) {
    auto func = cast<AbstractFunctionDecl>(declRef.getDecl());
    assert(func != nullptr);
    return WitnessTableEntry(func, nullptr, declRef);
  }
  
  bool isFunction() const {
    auto decl = MemberOrAssociatedType.dyn_cast<Decl*>();
    return Protocol == nullptr && decl && isa<AbstractFunctionDecl>(decl);
  }

  bool matchesFunction(SILDeclRef dr) const {
    auto func = cast<AbstractFunctionDecl>(dr.getDecl());
    assert(func != nullptr);
    if (auto decl = MemberOrAssociatedType.dyn_cast<Decl*>())
      return decl == func && Protocol == nullptr && declRef == dr;
    return false;
  }

  AbstractFunctionDecl *getFunction() const {
    assert(isFunction());
    auto decl = MemberOrAssociatedType.get<Decl*>();
    return static_cast<AbstractFunctionDecl*>(decl);
  }

  static WitnessTableEntry forAssociatedType(AssociatedType ty) {
    return WitnessTableEntry(ty.getAssociation(), nullptr, SILDeclRef());
  }
  
  bool isAssociatedType() const {
    if (auto decl = MemberOrAssociatedType.dyn_cast<Decl*>())
      return Protocol == nullptr && isa<AssociatedTypeDecl>(decl);
    return false;
  }

  bool matchesAssociatedType(AssociatedType assocType) const {
    if (auto decl = MemberOrAssociatedType.dyn_cast<Decl*>())
      return decl == assocType.getAssociation() && Protocol == nullptr;
    return false;
  }

  AssociatedTypeDecl *getAssociatedType() const {
    assert(isAssociatedType());
    auto decl = MemberOrAssociatedType.get<Decl*>();
    return static_cast<AssociatedTypeDecl*>(decl);
  }

  static WitnessTableEntry forAssociatedConformance(AssociatedConformance conf){
    return WitnessTableEntry(conf.getAssociation().getPointer(),
                             conf.getAssociatedRequirement(),
                             SILDeclRef());
  }

  bool isAssociatedConformance() const {
    return Protocol != nullptr && !MemberOrAssociatedType.isNull();
  }

  bool matchesAssociatedConformance(const AssociatedConformance &conf) const {
    if (auto type = MemberOrAssociatedType.dyn_cast<TypeBase*>())
      return type == conf.getAssociation().getPointer() &&
             Protocol == conf.getAssociatedRequirement();
    return false;
  }

  CanType getAssociatedConformancePath() const {
    assert(isAssociatedConformance());
    auto type = MemberOrAssociatedType.get<TypeBase*>();
    return CanType(type);
  }

  ProtocolDecl *getAssociatedConformanceRequirement() const {
    assert(isAssociatedConformance());
    return Protocol;
  }

  friend bool operator==(WitnessTableEntry left, WitnessTableEntry right) {
    return left.MemberOrAssociatedType == right.MemberOrAssociatedType &&
           left.Protocol == right.Protocol;
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
    std::uninitialized_copy(table.begin(), table.end(),
                            getTrailingObjects<WitnessTableEntry>());
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
                                      AssociatedType assocType) const;

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
