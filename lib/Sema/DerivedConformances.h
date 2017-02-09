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
  class FuncDecl;
  class NominalTypeDecl;
  class Type;
  class TypeChecker;
  class ValueDecl;
  
namespace DerivedConformance {

/// Determine the derivable requirement that would satisfy the given
/// requirement, if there is one.
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
ValueDecl *getDerivableRequirement(NominalTypeDecl *nominal,
                                   ValueDecl *requirement);

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

/// Derive an Equatable requirement for a type.
///
/// Currently this is only implemented for enums without associated values.
/// Obvious generalizations would be to enums with all-Hashable payloads and to
/// structs with all-Hashable stored properties.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveEquatable(TypeChecker &tc,
                           Decl *parentDecl,
                           NominalTypeDecl *type,
                           ValueDecl *requirement);
  
/// Derive a Hashable requirement for a type.
///
/// Currently this is only implemented for enums without associated values.
/// Obvious generalizations would be to enums with all-Hashable payloads and to
/// structs with all-Hashable stored properties.
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

/// Declare a getter for a derived property.
FuncDecl *declareDerivedPropertyGetter(TypeChecker &tc,
                                       Decl *parentDecl,
                                       NominalTypeDecl *typeDecl,
                                       Type propertyInterfaceType,
                                       Type propertyContextType,
                                       bool isStatic,
                                       bool isFinal);

/// Declare a read-only property with an existing getter.
std::pair<VarDecl *, PatternBindingDecl *>
declareDerivedReadOnlyProperty(TypeChecker &tc,
                               Decl *parentDecl,
                               NominalTypeDecl *typeDecl,
                               Identifier name,
                               Type propertyInterfaceType,
                               Type propertyContextType,
                               FuncDecl *getterDecl,
                               bool isStatic,
                               bool isFinal);


/// Build a reference to the 'self' decl of a derived function.
DeclRefExpr *createSelfDeclRef(AbstractFunctionDecl *fn);

}
  
}

#endif
