//===--- DerivedConformances.h - Derived protocol conformance ---*- C++ -*-===//
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
//  This file defines entry points to synthesize compiler-derived conformances
//  to certain known protocols.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_DERIVEDCONFORMANCES_H
#define SWIFT_SEMA_DERIVEDCONFORMANCES_H

#include <utility>

namespace swift {
class Decl;
class DeclRefExpr;
class AccessorDecl;
class NominalTypeDecl;
class PatternBindingDecl;
class Type;
class TypeChecker;
class ValueDecl;
class VarDecl;

class DerivedConformance {
public:
  TypeChecker &TC;
  Decl *ConformanceDecl;
  NominalTypeDecl *Nominal;
  ProtocolDecl *Protocol;

  DerivedConformance(TypeChecker &tc, Decl *conformanceDecl,
                     NominalTypeDecl *nominal, ProtocolDecl *protocol);

  /// Retrieve the context in which the conformance is declared (either the
  /// nominal type, or an extension of it) as a \c DeclContext.
  DeclContext *getConformanceContext() const;

  /// Add \c children as members of the context that declares the conformance.
  void addMembersToConformanceContext(ArrayRef<Decl *> children);

  /// Get the declared type of the protocol that this is conformance is for.
  Type getProtocolType() const;

  /// True if the type can implicitly derive a conformance for the given
  /// protocol.
  ///
  /// If true, explicit conformance checking will synthesize implicit
  /// declarations for requirements of the protocol that are not satisfied by
  /// the type's explicit members.
  ///
  /// \param tc The type checker.
  ///
  /// \param nominal The nominal type for which we are determining whether to
  /// derive a witness.
  ///
  /// \param protocol The protocol whose requirements are being derived.
  ///
  /// \return True if the type can implicitly derive a conformance for the
  /// given protocol.
  static bool derivesProtocolConformance(TypeChecker &tc, DeclContext *DC,
                                         NominalTypeDecl *nominal,
                                         ProtocolDecl *protocol);

  /// Determine the derivable requirement that would satisfy the given
  /// requirement, if there is one.
  ///
  /// \param tc The type checker.
  ///
  /// \param nominal The nominal type for which we are determining whether to
  /// derive a witness.
  ///
  /// \param requirement The requirement for which we are checking for a
  /// derivation. This requirement need not be within a derivable protocol,
  /// because derivable requirements can get restated in inherited unrelated
  /// or unrelated protocols.
  ///
  /// \returns The requirement whose witness could be derived to potentially
  /// satisfy this given requirement, or NULL if there is no such requirement.
  static ValueDecl *getDerivableRequirement(TypeChecker &tc,
                                            NominalTypeDecl *nominal,
                                            ValueDecl *requirement);

  /// Derive a CaseIterable requirement for an enum if it has no associated
  /// values for any of its cases.
  ///
  /// \returns the derived member, which will also be added to the type.
  ValueDecl *deriveCaseIterable(ValueDecl *requirement);

  /// Derive a CaseIterable type witness for an enum if it has no associated
  /// values for any of its cases.
  ///
  /// \returns the derived member, which will also be added to the type.
  Type deriveCaseIterable(AssociatedTypeDecl *assocType);

  /// Derive a RawRepresentable requirement for an enum, if it has a valid
  /// raw type and raw values for all of its cases.
  ///
  /// \returns the derived member, which will also be added to the type.
  ValueDecl *deriveRawRepresentable(ValueDecl *requirement);

  /// Derive a RawRepresentable type witness for an enum, if it has a valid
  /// raw type and raw values for all of its cases.
  ///
  /// \returns the derived member, which will also be added to the type.
  Type deriveRawRepresentable(AssociatedTypeDecl *assocType);

  /// Determine if an Equatable requirement can be derived for a type.
  ///
  /// This is implemented for enums without associated values or all-Equatable
  /// associated values, and for structs with all-Equatable stored properties.
  ///
  /// \returns True if the requirement can be derived.
  static bool canDeriveEquatable(TypeChecker &tc, DeclContext *DC,
                                 NominalTypeDecl *type);

  /// Derive an Equatable requirement for a type.
  ///
  /// This is implemented for enums without associated values or all-Equatable
  /// associated values, and for structs with all-Equatable stored properties.
  ///
  /// \returns the derived member, which will also be added to the type.
  ValueDecl *deriveEquatable(ValueDecl *requirement);

  /// Determine if a Hashable requirement can be derived for a type.
  ///
  /// This is implemented for enums without associated values or all-Hashable
  /// associated values, and for structs with all-Hashable stored properties.
  ///
  /// \returns True if the requirement can be derived.
  static bool canDeriveHashable(NominalTypeDecl *type);

  /// Derive a Hashable requirement for a type.
  ///
  /// This is implemented for enums without associated values or all-Hashable
  /// associated values, and for structs with all-Hashable stored properties.
  ///
  /// \returns the derived member, which will also be added to the type.
  ValueDecl *deriveHashable(ValueDecl *requirement);

  /// Derive a _BridgedNSError requirement for an @objc enum type.
  ///
  /// \returns the derived member, which will also be added to the type.
  ValueDecl *deriveBridgedNSError(ValueDecl *requirement);

  /// Derive a CodingKey requirement for an enum type.
  ///
  /// \returns the derived member, which will also be added to the type.
  ValueDecl *deriveCodingKey(ValueDecl *requirement);

  /// Derive an Encodable requirement for a struct type.
  ///
  /// \returns the derived member, which will also be added to the type.
  ValueDecl *deriveEncodable(ValueDecl *requirement);

  /// Derive a Decodable requirement for a struct type.
  ///
  /// \returns the derived member, which will also be added to the type.
  ValueDecl *deriveDecodable(ValueDecl *requirement);

  /// Declare a read-only property.
  std::pair<VarDecl *, PatternBindingDecl *>
  declareDerivedProperty(Identifier name, Type propertyInterfaceType,
                         Type propertyContextType, bool isStatic, bool isFinal);

  /// Add a getter to a derived property.  The property becomes read-only.
  static AccessorDecl *
  addGetterToReadOnlyDerivedProperty(TypeChecker &tc, VarDecl *property,
                                     Type propertyContextType);

  /// Declare a getter for a derived property.
  /// The getter will not be added to the property yet.
  static AccessorDecl *declareDerivedPropertyGetter(TypeChecker &tc,
                                                    VarDecl *property,
                                                    Type propertyContextType);

  /// Build a reference to the 'self' decl of a derived function.
  static DeclRefExpr *createSelfDeclRef(AbstractFunctionDecl *fn);

  /// Returns true if this derivation is trying to use a context that isn't
  /// appropriate for deriving.
  ///
  /// \param synthesizing The decl that is being synthesized.
  bool checkAndDiagnoseDisallowedContext(ValueDecl *synthesizing) const;
};
}

#endif
