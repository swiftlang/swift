//===--- GenericRequirement.h - Generic requirement -------------*- C++ -*-===//
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

#ifndef SWIFT_AST_GENERIC_REQUIREMENT_H
#define SWIFT_AST_GENERIC_REQUIREMENT_H

#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class ProtocolDecl;

/// The three kinds of entities passed in the runtime calling convention for
/// generic code: pack shapes, type metadata, and witness tables.
///
/// A pack shape describes an equivalence class of type parameter packs; the
/// runtime value is a single integer, which is the length of the pack.
///
/// Type metadata is emitted for each reduced generic parameter (that is,
/// not same-type constrained to another generic parameter or concrete type).
///
/// A witness table is emitted for each conformance requirement in the
/// generic signature.
class GenericRequirement {
public:
  enum class Kind: uint8_t {
    Shape,
    Metadata,
    WitnessTable
  };

private:
  Kind kind;
  CanType type;
  ProtocolDecl *proto;

  GenericRequirement(Kind kind, CanType type, ProtocolDecl *proto)
    : kind(kind), type(type), proto(proto) {}

public:
  Kind getKind() const {
    return kind;
  }

  CanType getTypeParameter() const {
    return type;
  }

  ProtocolDecl *getProtocol() const {
    return proto;
  }

  bool isShape() const {
    return kind == Kind::Shape;
  }

  static GenericRequirement forShape(CanType type) {
    assert(type->isParameterPack());
    return GenericRequirement(Kind::Shape, type, nullptr);
  }

  bool isMetadata() const {
    return kind == Kind::Metadata;
  }

  static GenericRequirement forMetadata(CanType type) {
    return GenericRequirement(Kind::Metadata, type, nullptr);
  }

  bool isWitnessTable() const {
    return kind == Kind::WitnessTable;
  }

  static GenericRequirement forWitnessTable(CanType type, ProtocolDecl *proto) {
    assert(proto != nullptr);
    return GenericRequirement(Kind::WitnessTable, type, proto);
  }

  void dump(llvm::raw_ostream &out) const {
    switch (kind) {
    case Kind::Shape:
      out << "shape: " << type;
      break;
    case Kind::Metadata:
      out << "metadata: " << type;
      break;
    case Kind::WitnessTable:
      out << "witness_table: " << type << " : " << proto->getName();
      break;
    }
  }
};

} // end namespace swift

namespace llvm {
template <> struct DenseMapInfo<swift::GenericRequirement> {
  using GenericRequirement = swift::GenericRequirement;
  using CanTypeInfo = llvm::DenseMapInfo<swift::CanType>;
  static GenericRequirement getEmptyKey() {
    return GenericRequirement::forMetadata(CanTypeInfo::getEmptyKey());
  }
  static GenericRequirement getTombstoneKey() {
    return GenericRequirement::forMetadata(CanTypeInfo::getTombstoneKey());
  }
  static llvm::hash_code getHashValue(GenericRequirement req) {
    return hash_combine(CanTypeInfo::getHashValue(req.getTypeParameter()),
                        hash_value(req.getProtocol()));
  }
  static bool isEqual(GenericRequirement lhs, GenericRequirement rhs) {
    return (lhs.getKind() == rhs.getKind() &&
            lhs.getTypeParameter() == rhs.getTypeParameter() &&
            lhs.getProtocol() == rhs.getProtocol());
  }
};
} // end namespace llvm

#endif // SWIFT_AST_GENERIC_REQUIREMENT_H
