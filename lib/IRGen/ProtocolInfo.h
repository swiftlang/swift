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

/// A class which encapsulates an index into a witness table.
class WitnessIndex {
  unsigned Value : 31;
  unsigned IsPrefix : 1;
public:
  WitnessIndex() = default;
  WitnessIndex(ValueWitness index) : Value(unsigned(index)) {}
  explicit WitnessIndex(unsigned index, bool isPrefix)
    : Value(index), IsPrefix(isPrefix) {}

  unsigned getValue() const { return Value; }

  bool isPrefix() const { return IsPrefix; }
};

/// A witness to a specific element of a protocol.  Every
/// ProtocolTypeInfo stores one of these for each declaration in the
/// protocol.
/// 
/// The structure of a witness varies by the type of declaration:
///   - a function requires a single witness, the function;
///   - a variable requires two witnesses, a getter and a setter;
///   - a subscript requires two witnesses, a getter and a setter;
///   - a type requires a pointer to the metadata for that type and
///     to witness tables for each of the protocols it obeys.
class WitnessTableEntry {
  Decl *Member;
  WitnessIndex BeginIndex;
  /// The protocols (in the expected order) for the conformances an associated
  /// type witness conforms to.
  // FIXME: this is not needed once the requirement signature is used for laying
  // out conformances in a witness table.
  ArrayRef<ProtocolDecl *> CanonicalProtocols;

  WitnessTableEntry(Decl *member, WitnessIndex begin)
      : Member(member), BeginIndex(begin) {}

  WitnessTableEntry(Decl *member, WitnessIndex begin,
                    ArrayRef<ProtocolDecl *> protocols)
      : Member(member), BeginIndex(begin), CanonicalProtocols(protocols) {
    assert(isAssociatedType());
  }

public:
  WitnessTableEntry() = default;

  Decl *getMember() const {
    return Member;
  }

  static WitnessTableEntry forPrefixBase(ProtocolDecl *proto) {
    return WitnessTableEntry(proto, WitnessIndex(0, /*isPrefix=*/ true));
  }

  static WitnessTableEntry forOutOfLineBase(ProtocolDecl *proto,
                                            WitnessIndex index) {
    return WitnessTableEntry(proto, index);
  }

  /// Is this a base-protocol entry?
  bool isBase() const { return isa<ProtocolDecl>(Member); }

  /// Is the table for this base-protocol entry "out of line",
  /// i.e. there is a witness which indirectly points to it, or is
  /// it a prefix of the layout of this protocol?
  bool isOutOfLineBase() const {
    assert(isBase());
    return !BeginIndex.isPrefix();
  }

  /// Return the index at which to find the table for this
  /// base-protocol entry.
  WitnessIndex getOutOfLineBaseIndex() const {
    assert(isOutOfLineBase());
    return BeginIndex;
  }

  static WitnessTableEntry forFunction(AbstractFunctionDecl *func,
                                       WitnessIndex index) {
    return WitnessTableEntry(func, index);
  }
  
  bool isFunction() const { return isa<AbstractFunctionDecl>(Member); }

  WitnessIndex getFunctionIndex() const {
    assert(isFunction());
    return BeginIndex;
  }

  /// The \c protocols array must live at least as long as the ProtocolInfo for
  /// this Entry.
  static WitnessTableEntry
  forAssociatedType(AssociatedTypeDecl *ty, WitnessIndex index,
                    ArrayRef<ProtocolDecl *> protocols) {
    return WitnessTableEntry(ty, index, protocols);
  }
  
  bool isAssociatedType() const { return isa<AssociatedTypeDecl>(Member); }
  
  WitnessIndex getAssociatedTypeIndex() const {
    assert(isAssociatedType());
    return BeginIndex;
  }

  WitnessIndex
  getAssociatedTypeWitnessTableIndex(ProtocolDecl *target) const {
    assert(!BeginIndex.isPrefix());
    auto index = BeginIndex.getValue() + 1;
    for (auto protocol : CanonicalProtocols) {
      if (protocol == target) {
        return WitnessIndex(index, false);
      }
      index++;
    }
    llvm_unreachable("protocol not in direct conformance list?");
  }
};

/// An abstract description of a protocol.
class ProtocolInfo final :
    private llvm::TrailingObjects<ProtocolInfo, WitnessTableEntry> {
  friend TrailingObjects;

  /// A singly-linked-list of all the protocols that have been laid out.
  const ProtocolInfo *NextConverted;
  friend class TypeConverter;

  /// The number of witnesses in the protocol.
  unsigned NumWitnesses;

  /// The number of table entries in this protocol layout.
  unsigned NumTableEntries;

  /// A table of all the conformances we've needed so far for this
  /// protocol.  We expect this to be quite small for most protocols.
  mutable llvm::SmallDenseMap<const ProtocolConformance*, ConformanceInfo*, 2>
    Conformances;

  ProtocolInfo(unsigned numWitnesses, ArrayRef<WitnessTableEntry> table)
    : NumWitnesses(numWitnesses), NumTableEntries(table.size()) {
    std::uninitialized_copy(table.begin(), table.end(),
                            getTrailingObjects<WitnessTableEntry>());
  }

  static ProtocolInfo *create(unsigned numWitnesses,
                              ArrayRef<WitnessTableEntry> table);

public:
  const ConformanceInfo &getConformance(IRGenModule &IGM,
                                        ProtocolDecl *protocol,
                                        const ProtocolConformance *conf) const;

  /// The number of witness slots in a conformance to this protocol;
  /// in other words, the size of the table in words.
  unsigned getNumWitnesses() const {
    return NumWitnesses;
  }

  ArrayRef<WitnessTableEntry> getWitnessEntries() const {
    return {getTrailingObjects<WitnessTableEntry>(), NumTableEntries};
  }

  const WitnessTableEntry &getWitnessEntry(Decl *member) const {
    // FIXME: do a binary search if the number of witnesses is large
    // enough.
    for (auto &witness : getWitnessEntries())
      if (witness.getMember() == member)
        return witness;
    llvm_unreachable("didn't find entry for member!");
  }

  ~ProtocolInfo();
};

} // end namespace irgen
} // end namespace swift

#endif
