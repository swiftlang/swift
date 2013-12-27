//===--- LazyResolver.h - Lazy Resolution for ASTs --------------*- C++ -*-===//
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
// This file defines the LazyResolver abstract interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LAZYRESOLVER_H
#define SWIFT_AST_LAZYRESOLVER_H

#include "swift/Basic/Optional.h"

namespace swift {

class AssociatedTypeDecl;
class DeclContext;
class ExtensionDecl;
class Identifier;
class NominalTypeDecl;
class NormalProtocolConformance;
class ProtocolConformance;
class ProtocolDecl;
class Substitution;
class Type;
class ValueDecl;

/// Abstract interface used to lazily resolve aspects of the AST, such as the
/// types of declarations or protocol conformance structures.
class LazyResolver {
public:
  virtual ~LazyResolver();

  /// Resolve the conformance of the given nominal type to the given protocol.
  ///
  /// \param type The nominal type that conforms to the given protocol.
  ////
  /// \param protocol The protocol to which the type conforms.
  ///
  /// \param ext If the conforms occurs via an extension, the extension
  /// declaration.
  ///
  /// \returns the protocol conformance, or null if the type does not conform
  /// to the protocol.
  virtual ProtocolConformance *resolveConformance(NominalTypeDecl *type,
                                                  ProtocolDecl *protocol,
                                                  ExtensionDecl *ext) = 0;

  /// Resolve the type witness for the given associated type within the given
  /// protocol conformance.
  virtual void resolveTypeWitness(const NormalProtocolConformance *conformance,
                                  AssociatedTypeDecl *assocType) = 0;

  /// Resolve the witness for the given non-type requirement within
  /// the given protocol conformance.
  virtual void resolveWitness(const NormalProtocolConformance *conformance,
                              ValueDecl *requirement) = 0;

  /// Resolve the "existential conforms to itself" bit for the given protocol.
  virtual void resolveExistentialConformsToItself(ProtocolDecl *proto) = 0;

  /// Resolve a member type.
  ///
  /// \param dc The context in which to resolve the type.
  /// \param type The type in which we will search for the member type.
  /// \param name The name of the member type.
  ///
  /// \returns the member type, or an empty type if no such type could be
  /// found.
  virtual Type resolveMemberType(DeclContext *dc, Type type,
                                 Identifier name) = 0;

  /// Resolve the type and declaration attributes of a value.
  ///
  /// This can be called when the type or signature of a value is needed.
  /// It does not perform full type-checking, only checks for basic
  /// consistency and provides the value a type.
  virtual void resolveDeclSignature(ValueDecl *VD) = 0;
};


/// A class that can lazily load members from a serialized format.
class alignas(void*) LazyMemberLoader {
  virtual void anchor();
public:
  virtual ~LazyMemberLoader() = default;

  /// Returns an ASTContext-allocated array containing all member decls.
  virtual ArrayRef<Decl *> loadAllMembers(uint64_t contextData) = 0;
};

}

#endif // LLVM_SWIFT_AST_LAZYRESOLVER_H
