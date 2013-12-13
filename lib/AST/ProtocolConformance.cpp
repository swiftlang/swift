//===--- ProtocolConformance.cpp - AST Protocol Conformance -----*- C++ -*-===//
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
// This file implements the protocol conformance data structures.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTContext.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"

using namespace swift;

void *ProtocolConformance::operator new(size_t bytes, ASTContext &context,
                                        unsigned alignment) {
  return context.Allocate(bytes, alignment);

}

#define CONFORMANCE_SUBCLASS_DISPATCH(Method, Args)                        \
switch (getKind()) {                                                       \
  case ProtocolConformanceKind::Normal:                                    \
    static_assert(&ProtocolConformance::Method !=                          \
                    &NormalProtocolConformance::Method,                    \
                  "Must override NormalProtocolConformance::" #Method);    \
    return cast<NormalProtocolConformance>(this)->Method Args;             \
  case ProtocolConformanceKind::Specialized:                               \
    static_assert(&ProtocolConformance::Method !=                          \
                    &InheritedProtocolConformance::Method,                 \
                  "Must override InheritedProtocolConformance::" #Method); \
    return cast<SpecializedProtocolConformance>(this)->Method Args;        \
  case ProtocolConformanceKind::Inherited:                                 \
    static_assert(&ProtocolConformance::Method !=                          \
                    &InheritedProtocolConformance::Method,                 \
                  "Must override InheritedProtocolConformance::" #Method); \
    return cast<InheritedProtocolConformance>(this)->Method Args;          \
}

/// Get the protocol being conformed to.
ProtocolDecl *ProtocolConformance::getProtocol() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getProtocol, ())
}

/// Get the module that contains the conforming extension or type declaration.
Module *ProtocolConformance::getContainingModule() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getContainingModule, ())
}

/// Retrieve the complete set of type witnesses.
const TypeWitnessMap &ProtocolConformance::getTypeWitnesses() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getTypeWitnesses, ())
}

const WitnessMap &ProtocolConformance::getWitnesses() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getWitnesses, ())
}

const InheritedConformanceMap &
ProtocolConformance::getInheritedConformances() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getInheritedConformances, ())
}

/// Determine whether the witness for the given requirement
/// is either the default definition or was otherwise deduced.
bool ProtocolConformance::usesDefaultDefinition(ValueDecl *requirement) const {
  CONFORMANCE_SUBCLASS_DISPATCH(usesDefaultDefinition, (requirement))
}

GenericParamList *ProtocolConformance::getGenericParams() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Normal:
    // FIXME: This should be an independent property of the conformance.
    // Assuming a BoundGenericType conformance is always for the
    // DeclaredTypeInContext is unsound if we ever add constrained extensions.
    if (auto bgt = getType()->getAs<BoundGenericType>()) {
      auto decl = bgt->getDecl();
      assert(bgt->isEqual(decl->getDeclaredTypeInContext())
             && "conformance for constrained generic type not implemented");
      return decl->getGenericParams();
    }
    return nullptr;
  case ProtocolConformanceKind::Specialized:
  case ProtocolConformanceKind::Inherited:
    // FIXME: These could reasonably have open type variables.
    return nullptr;
  }
}
