//===--- ProtocolAssociations.h - Associated types/conformances -*- C++ -*-===//
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
// This file defines types for representing types and conformances
// associated with a protocol.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PROTOCOLASSOCIATIONS_H
#define SWIFT_AST_PROTOCOLASSOCIATIONS_H

#include "swift/AST/Decl.h"
#include "llvm/ADT/DenseMapInfo.h"

namespace swift {

/// A type associated with a protocol.
///
/// This struct exists largely so that we can maybe eventually
/// generalize it to an arbitrary path.
class AssociatedType {
  AssociatedTypeDecl *Association;
  using AssociationInfo = llvm::DenseMapInfo<AssociatedTypeDecl*>;

  struct SpecialValue {};
  explicit AssociatedType(SpecialValue _, AssociatedTypeDecl *specialValue)
      : Association(specialValue) {}

public:
  explicit AssociatedType(AssociatedTypeDecl *association)
      : Association(association) {
    assert(association);
  }

  ProtocolDecl *getSourceProtocol() const {
    return Association->getProtocol();
  }

  AssociatedTypeDecl *getAssociation() const {
    return Association;
  }

  friend bool operator==(AssociatedType lhs, AssociatedType rhs) {
    return lhs.Association == rhs.Association;
  }
  friend bool operator!=(AssociatedType lhs, AssociatedType rhs) {
    return !(lhs == rhs);
  }

  unsigned getHashValue() const {
    return llvm::hash_value(Association);
  }

  static AssociatedType getEmptyKey() {
    return AssociatedType(SpecialValue(), AssociationInfo::getEmptyKey());
  }
  static AssociatedType getTombstoneKey() {
    return AssociatedType(SpecialValue(), AssociationInfo::getTombstoneKey());
  }
};

/// A base conformance of a protocol.
class BaseConformance {
  ProtocolDecl *Source;
  ProtocolDecl *Requirement;

public:
  explicit BaseConformance(ProtocolDecl *source,
                                 ProtocolDecl *requirement)
      : Source(source), Requirement(requirement) {
    assert(source && requirement);
  }

  ProtocolDecl *getSourceProtocol() const {
    return Source;
  }

  ProtocolDecl *getBaseRequirement() const {
    return Requirement;
  }
};

/// A conformance associated with a protocol.
class AssociatedConformance {
  ProtocolDecl *Source;
  CanType Association;
  ProtocolDecl *Requirement;

  using SourceInfo = llvm::DenseMapInfo<ProtocolDecl*>;

  explicit AssociatedConformance(ProtocolDecl *specialValue)
      : Source(specialValue), Association(CanType()), Requirement(nullptr) {}

public:
  explicit AssociatedConformance(ProtocolDecl *source, CanType association,
                                 ProtocolDecl *requirement)
      : Source(source), Association(association), Requirement(requirement) {
    assert(source && association && requirement);
  }

  ProtocolDecl *getSourceProtocol() const {
    return Source;
  }

  CanType getAssociation() const {
    return Association;
  }

  ProtocolDecl *getAssociatedRequirement() const {
    return Requirement;
  }

  friend bool operator==(const AssociatedConformance &lhs,
                         const AssociatedConformance &rhs) {
    return lhs.Source == rhs.Source &&
           lhs.Association == rhs.Association &&
           lhs.Requirement == rhs.Requirement;
  }
  friend bool operator!=(const AssociatedConformance &lhs,
                         const AssociatedConformance &rhs) {
    return !(lhs == rhs);
  }

  unsigned getHashValue() const {
    return hash_value(llvm::hash_combine(Source,
                                         Association.getPointer(),
                                         Requirement));
  }

  static AssociatedConformance getEmptyKey() {
    return AssociatedConformance(SourceInfo::getEmptyKey());
  }
  static AssociatedConformance getTombstoneKey() {
    return AssociatedConformance(SourceInfo::getTombstoneKey());
  }
};

} // end namespace swift

namespace llvm {
  template <> struct DenseMapInfo<swift::AssociatedType> {
    static inline swift::AssociatedType getEmptyKey() {
      return swift::AssociatedType::getEmptyKey();
    }

    static inline swift::AssociatedType getTombstoneKey() {
      return swift::AssociatedType::getTombstoneKey();
    }

    static unsigned getHashValue(swift::AssociatedType val) {
      return val.getHashValue();
    }

    static bool isEqual(swift::AssociatedType lhs, swift::AssociatedType rhs) {
      return lhs == rhs;
    }
  };

  template <> struct DenseMapInfo<swift::AssociatedConformance> {
    static inline swift::AssociatedConformance getEmptyKey() {
      return swift::AssociatedConformance::getEmptyKey();
    }

    static inline swift::AssociatedConformance getTombstoneKey() {
      return swift::AssociatedConformance::getTombstoneKey();
    }

    static unsigned getHashValue(swift::AssociatedConformance val) {
      return val.getHashValue();
    }

    static bool isEqual(swift::AssociatedConformance lhs,
                        swift::AssociatedConformance rhs) {
      return lhs == rhs;
    }
  };
}

#endif
