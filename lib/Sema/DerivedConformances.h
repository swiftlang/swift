//===--- DerivedConformances.h - Derived protocol conformance ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

/// Derive a RawRepresentable requirement for an enum, if it has a valid
/// raw type and raw values for all of its cases.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveRawRepresentable(TypeChecker &tc,
                                  NominalTypeDecl *type,
                                  ValueDecl *requirement);

/// Derive a RawRepresentable type witness for an enum, if it has a valid
/// raw type and raw values for all of its cases.
///
/// \returns the derived member, which will also be added to the type.
Type deriveRawRepresentable(TypeChecker &tc,
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
                          NominalTypeDecl *type,
                          ValueDecl *requirement);
  
/// Derive an ErrorType requirement for an enum type.
///
/// A unique string representation of the enum type will be used as the domain
/// for members of the enum, and each case will have its own integer code.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveErrorType(TypeChecker &tc,
                           NominalTypeDecl *type,
                           ValueDecl *requirement);

/// Derive a _BridgedNSError requirement for an @objc enum type.
///
/// \returns the derived member, which will also be added to the type.
ValueDecl *deriveBridgedNSError(TypeChecker &tc,
                                NominalTypeDecl *type,
                                ValueDecl *requirement);

/// Insert an operator declaration associated with a declaration
/// context. The operator declaration is added at global scope.
void _insertOperatorDecl(ASTContext &C,
                         IterableDeclContext *scope,
                         Decl *member);
  
/// Insert an operator declaration associated with a declaration
/// context. The operator declaration is added at global scope.
template<typename SomeDecl>
inline SomeDecl *insertOperatorDecl(ASTContext &C,
                                    IterableDeclContext *scope,
                                    SomeDecl *member) {
  ::swift::DerivedConformance::_insertOperatorDecl(C, scope, member);
  return member;
}

/// Declare a getter for a derived property.
FuncDecl *declareDerivedPropertyGetter(TypeChecker &tc,
                                       NominalTypeDecl *typeDecl,
                                       Type contextType,
                                       Type propertyInterfaceType,
                                       Type propertyContextType,
                                       bool isStatic = false);

/// Declare a read-only property with an existing getter.
std::pair<VarDecl *, PatternBindingDecl *>
declareDerivedReadOnlyProperty(TypeChecker &tc,
                               NominalTypeDecl *typeDecl,
                               Identifier name,
                               Type propertyInterfaceType,
                               Type propertyContextType,
                               FuncDecl *getterDecl,
                               bool isStatic = false);


/// Build a reference to the 'self' decl of a derived function.
DeclRefExpr *createSelfDeclRef(AbstractFunctionDecl *fn);

}
  
}

#endif
