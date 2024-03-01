//===--- RequirementSignature.h - Requirement Signature AST -----*- C++ -*-===//
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
// This file defines the RequirementSignature class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_REQUIREMENT_SIGNATURE_H
#define SWIFT_AST_REQUIREMENT_SIGNATURE_H

#include "swift/AST/GenericSignature.h"
#include "swift/AST/Type.h"

namespace swift {

/// A description of a typealias defined in a protocol.
class ProtocolTypeAlias final {
  Identifier Name;
  Type UnderlyingType;

public:
  ProtocolTypeAlias(Identifier name, Type underlyingType)
    : Name(name), UnderlyingType(underlyingType) {}

  /// Returns the name of the typealias.
  Identifier getName() const { return Name; }

  /// Returns the underlying type of the typealias.
  Type getUnderlyingType() const { return UnderlyingType; }
};

/// The requirements that describe a protocol from the viewpoint of the
/// generics system.
class RequirementSignature final {
  ArrayRef<Requirement> Requirements;
  ArrayRef<ProtocolTypeAlias> TypeAliases;
  GenericSignatureErrors Errors;

public:
  RequirementSignature(GenericSignatureErrors errors = GenericSignatureErrors())
    : Errors(errors) {}

  RequirementSignature(ArrayRef<Requirement> requirements,
                       ArrayRef<ProtocolTypeAlias> typeAliases,
                       GenericSignatureErrors errors = GenericSignatureErrors())
    : Requirements(requirements), TypeAliases(typeAliases), Errors(errors) {}

  /// The requirements including any inherited protocols and conformances for
  /// associated types that are introduced in this protocol.
  ///
  /// Requirements implied via any other protocol (e.g., inherited protocols
  /// of the inherited protocols) are not mentioned.
  ///
  /// The conformance requirements listed here become entries in witness tables
  /// for conformances to this protocol.
  ArrayRef<Requirement> getRequirements() const {
    return Requirements;
  }

  ArrayRef<ProtocolTypeAlias> getTypeAliases() const {
    return TypeAliases;
  }

  GenericSignatureErrors getErrors() const {
    return Errors;
  }

  void getRequirementsWithInverses(
      ProtocolDecl *owner,
      SmallVector<Requirement, 2> &reqs,
      SmallVector<InverseRequirement, 2> &inverses) const;

  void print(ProtocolDecl *owner, raw_ostream &OS,
             const PrintOptions &Options = PrintOptions()) const;
  void print(ProtocolDecl *owner, ASTPrinter &Printer,
             const PrintOptions &Opts = PrintOptions()) const;

  static RequirementSignature getPlaceholderRequirementSignature(
      const ProtocolDecl *proto, GenericSignatureErrors errors);
};

} // end namespace swift

#endif // SWIFT_AST_REQUIREMENT_SIGNATURE_H
