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
  
namespace DerivedConformance {

/// True if the type can implicitly derive a conformance for the given protocol.
///
/// If true, explicit conformance checking will synthesize implicit declarations
/// for requirements of the protocol that are not satisfied by the type's
/// explicit members.
///
/// \param tc The type checker.
///
/// \param nominal The nominal type for which we are determining whether to
/// derive a witness.
///
/// \param protocol The protocol whose requirements are being derived.
///
/// \return True if the type can implicitly derive a conformance for the given
/// protocol.
bool derivesProtocolConformance(TypeChecker &tc,
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
/// because derivable requirements can get restated in inherited unrelated or
/// unrelated protocols.
///
/// \returns The requirement whose witness could be derived to potentially
/// satisfy this given requirement, or NULL if there is no such requirement.
ValueDecl *getDerivableRequirement(TypeChecker &tc,
                                   NominalTypeDecl *nominal,
                                   ValueDecl *requirement);


/// Derive a CaseIterable requirement for an enum if it has no associated
/// values for any of its cases.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveCaseIterable(TypeChecker &tc,
                              Decl *parentDecl,
                              NominalTypeDecl *type,
                              ValueDecl *requirement);

/// Derive a CaseIterable type witness for an enum if it has no associated
/// values for any of its cases.
///
/// \returns the derived member, which will also be added to the type.
Type deriveCaseIterable(TypeChecker &tc,
                        Decl *parentDecl,
                        NominalTypeDecl *type,
                        AssociatedTypeDecl *assocType);

/// Derive a RawRepresentable requirement for an enum, if it has a valid
/// raw type and raw values for all of its cases.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveRawRepresentable(TypeChecker &tc,
                                  Decl *parentDecl,
                                  NominalTypeDecl *type,
                                  ValueDecl *requirement);

/// Derive a RawRepresentable type witness for an enum, if it has a valid
/// raw type and raw values for all of its cases.
///
/// \returns the derived member, which will also be added to the type.
Type deriveRawRepresentable(TypeChecker &tc,
                            Decl *parentDecl,
                            NominalTypeDecl *type,
                            AssociatedTypeDecl *assocType);

/// Determine if an Equatable requirement can be derived for a type.
///
/// This is implemented for enums without associated values or all-Equatable
/// associated values, and for structs with all-Equatable stored properties.
///
/// \returns True if the requirement can be derived.
bool canDeriveEquatable(TypeChecker &tc,
                        NominalTypeDecl *type,
                        ValueDecl *requirement);

/// Derive an Equatable requirement for a type.
///
/// This is implemented for enums without associated values or all-Equatable
/// associated values, and for structs with all-Equatable stored properties.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveEquatable(TypeChecker &tc,
                           Decl *parentDecl,
                           NominalTypeDecl *type,
                           ValueDecl *requirement);

/// Determine if a Hashable requirement can be derived for a type.
///
/// This is implemented for enums without associated values or all-Hashable
/// associated values, and for structs with all-Hashable stored properties.
///
/// \returns True if the requirement can be derived.
bool canDeriveHashable(TypeChecker &tc,
                       NominalTypeDecl *type,
                       ValueDecl *requirement);
  
/// Derive a Hashable requirement for a type.
///
/// This is implemented for enums without associated values or all-Hashable
/// associated values, and for structs with all-Hashable stored properties.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveHashable(TypeChecker &tc,
                          Decl *parentDecl,
                          NominalTypeDecl *type,
                          ValueDecl *requirement);

/// Derive a _BridgedNSError requirement for an @objc enum type.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveBridgedNSError(TypeChecker &tc,
                                Decl *parentDecl,
                                NominalTypeDecl *type,
                                ValueDecl *requirement);

/// Derive a CodingKey requirement for an enum type.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveCodingKey(TypeChecker &tc,
                           Decl *parentDecl,
                           NominalTypeDecl *type,
                           ValueDecl *requirement);

/// Derive an Encodable requirement for a struct type.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveEncodable(TypeChecker &tc,
                           Decl *parentDecl,
                           NominalTypeDecl *type,
                           ValueDecl *requirement);

/// Derive a Decodable requirement for a struct type.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveDecodable(TypeChecker &tc,
                           Decl *parentDecl,
                           NominalTypeDecl *type,
                           ValueDecl *requirement);

/// Declare a read-only property.
std::pair<VarDecl *, PatternBindingDecl *>
declareDerivedProperty(TypeChecker &tc,
                       Decl *parentDecl,
                       NominalTypeDecl *typeDecl,
                       Identifier name,
                       Type propertyInterfaceType,
                       Type propertyContextType,
                       bool isStatic,
                       bool isFinal);

/// Add a getter to a derived property.  The property becomes read-only.
AccessorDecl *addGetterToReadOnlyDerivedProperty(TypeChecker &tc,
                                                 VarDecl *property,
                                                 Type propertyContextType);

/// Declare a getter for a derived property.
/// The getter will not be added to the property yet.
AccessorDecl *declareDerivedPropertyGetter(TypeChecker &tc,
                                           VarDecl *property,
                                           Type propertyContextType);

/// Build a reference to the 'self' decl of a derived function.
DeclRefExpr *createSelfDeclRef(AbstractFunctionDecl *fn);

}
  
}

#endif
