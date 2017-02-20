//===--- LazyResolver.h - Lazy Resolution for ASTs --------------*- C++ -*-===//
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
// This file defines the LazyResolver abstract interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LAZYRESOLVER_H
#define SWIFT_AST_LAZYRESOLVER_H

#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/PointerEmbeddedInt.h"

namespace swift {

class AssociatedTypeDecl;
class Decl;
class DeclContext;
class ExtensionDecl;
class Identifier;
class NominalTypeDecl;
class NormalProtocolConformance;
class ProtocolConformance;
class ProtocolDecl;
class Substitution;
class TypeDecl;
class ValueDecl;
class VarDecl;

/// Abstract interface used to lazily resolve aspects of the AST, such as the
/// types of declarations or protocol conformance structures.
class LazyResolver {
public:
  virtual ~LazyResolver();

  /// Resolve the type witnesses for the given associated type within the given
  /// protocol conformance.
  virtual void resolveTypeWitness(const NormalProtocolConformance *conformance,
                                  AssociatedTypeDecl *assocType) = 0;

  /// Resolve the witness for the given non-type requirement within
  /// the given protocol conformance.
  virtual void resolveWitness(const NormalProtocolConformance *conformance,
                              ValueDecl *requirement) = 0;

  /// Resolve an inherited conformance.
  virtual ProtocolConformance *resolveInheritedConformance(
                                 const NormalProtocolConformance *conformance,
                                 ProtocolDecl *inherited) = 0;

  /// Resolve the accessibility of a value.
  ///
  /// It does no type-checking.
  virtual void resolveAccessibility(ValueDecl *VD) = 0;

  /// Resolve the type and declaration attributes of a value.
  ///
  /// This can be called when the type or signature of a value is needed.
  /// It does not perform full type-checking, only checks for basic
  /// consistency and provides the value a type.
  virtual void resolveDeclSignature(ValueDecl *VD) = 0;

  /// Resolve the types in the inheritance clause of the given
  /// declaration context, which will be a type declaration or
  /// extension declaration.
  virtual void resolveInheritanceClause(
                 llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl) = 0;

  /// Resolve the superclass of the given class.
  virtual void resolveSuperclass(ClassDecl *classDecl) = 0;

  /// Resolve the raw type of the given enum.
  virtual void resolveRawType(EnumDecl *enumDecl) = 0;

  /// Resolve the inherited protocols of a given protocol.
  virtual void resolveInheritedProtocols(ProtocolDecl *protocol) = 0;

  /// Bind an extension to its extended type.
  virtual void bindExtension(ExtensionDecl *ext) = 0;

  /// Introduce the accessors for a 'lazy' variable.
  virtual void introduceLazyVarAccessors(VarDecl *var) = 0;

  /// Resolve the type of an extension.
  ///
  /// This can be called to ensure that the members of an extension can be
  /// considered to be members of the extended type.
  virtual void resolveExtension(ExtensionDecl *ext) = 0;

  /// Resolve any implicitly-declared constructors within the given nominal.
  virtual void resolveImplicitConstructors(NominalTypeDecl *nominal) = 0;

  /// Resolve any implicitly-generated members and conformances for generated
  /// external decls.
  virtual void resolveExternalDeclImplicitMembers(NominalTypeDecl *nominal) = 0;

  /// Determine whether the given (potentially constrained) protocol extension
  /// is usable for the given type.
  virtual bool isProtocolExtensionUsable(DeclContext *dc, Type type,
                                         ExtensionDecl *protocolExtension) = 0;
};

/// An implementation of LazyResolver that delegates to another.
class DelegatingLazyResolver : public LazyResolver {
protected:
  LazyResolver &Principal;
public:
  DelegatingLazyResolver(LazyResolver &principal) : Principal(principal) {}
  ~DelegatingLazyResolver(); // v-table anchor

  void resolveTypeWitness(const NormalProtocolConformance *conformance,
                          AssociatedTypeDecl *assocType) override {
    Principal.resolveTypeWitness(conformance, assocType);
  }

  void resolveWitness(const NormalProtocolConformance *conformance,
                      ValueDecl *requirement) override {
    Principal.resolveWitness(conformance, requirement);
  }

  ProtocolConformance *resolveInheritedConformance(
                         const NormalProtocolConformance *conformance,
                         ProtocolDecl *inherited) override {
    return Principal.resolveInheritedConformance(conformance, inherited);
  }

  void resolveAccessibility(ValueDecl *VD) override {
    Principal.resolveAccessibility(VD);
  }

  void resolveDeclSignature(ValueDecl *VD) override {
    Principal.resolveDeclSignature(VD);
  }

  void resolveInheritanceClause(
                llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl) override {
    Principal.resolveInheritanceClause(decl);
  }

  void resolveSuperclass(ClassDecl *classDecl) override {
    Principal.resolveSuperclass(classDecl);
  }

  void resolveRawType(EnumDecl *enumDecl) override {
    Principal.resolveRawType(enumDecl);
  }

  void resolveInheritedProtocols(ProtocolDecl *protocol) override {
    Principal.resolveInheritedProtocols(protocol);
  }

  void bindExtension(ExtensionDecl *ext) override {
    Principal.bindExtension(ext);
  }

  void introduceLazyVarAccessors(VarDecl *var) override {
    Principal.introduceLazyVarAccessors(var);
  }

  void resolveExtension(ExtensionDecl *ext) override {
    Principal.resolveExtension(ext);
  }

  void resolveImplicitConstructors(NominalTypeDecl *nominal) override {
    Principal.resolveImplicitConstructors(nominal);
  }

  void resolveExternalDeclImplicitMembers(NominalTypeDecl *nominal) override {
    Principal.resolveExternalDeclImplicitMembers(nominal);
  }

  bool isProtocolExtensionUsable(DeclContext *dc, Type type,
                                 ExtensionDecl *protocolExtension) override {
    return Principal.isProtocolExtensionUsable(dc, type, protocolExtension);
  }
};

/// Context data for lazy deserialization.
class LazyContextData {
public:
  /// The lazy member loader for this context.
  LazyMemberLoader *loader;
};

/// Context data for generic contexts.
class LazyGenericContextData : public LazyContextData {
public:
  /// The context data used for loading the generic environment.
  uint64_t genericEnvData = 0;
};

/// Context data for iterable decl contexts.
class LazyIterableDeclContextData : public LazyGenericContextData {
public:
  /// The context data used for loading all of the members of the iterable
  /// context.
  uint64_t memberData = 0;

  /// The context data used for loading all of the conformances of the
  /// iterable context.
  uint64_t allConformancesData = 0;
};

/// A class that can lazily load members from a serialized format.
class alignas(void*) LazyMemberLoader {
  virtual void anchor();
public:
  virtual ~LazyMemberLoader() = default;

  /// Populates the given vector with all member decls for \p D.
  ///
  /// The implementation should add the members to D.
  virtual void
  loadAllMembers(Decl *D, uint64_t contextData) {
    llvm_unreachable("unimplemented");
  }

  /// Populates the given vector with all conformances for \p D.
  ///
  /// The implementation should \em not call setConformances on \p D.
  virtual void
  loadAllConformances(const Decl *D, uint64_t contextData,
                      SmallVectorImpl<ProtocolConformance *> &Conformances) {
    llvm_unreachable("unimplemented");
  }

  /// Populates the given vector with all conformances for \p D.
  virtual void
  finishNormalConformance(NormalProtocolConformance *conformance,
                          uint64_t contextData) {
    llvm_unreachable("unimplemented");
  }

  /// Returns the default definition type for \p ATD.
  virtual TypeLoc loadAssociatedTypeDefault(const AssociatedTypeDecl *ATD,
                                            uint64_t contextData) {
    llvm_unreachable("unimplemented");
  }

  /// Returns the generic environment.
  virtual GenericEnvironment *loadGenericEnvironment(const DeclContext *decl,
                                                     uint64_t contextData) {
    llvm_unreachable("unimplemented");
  }
};

}

#endif // LLVM_SWIFT_AST_LAZYRESOLVER_H
