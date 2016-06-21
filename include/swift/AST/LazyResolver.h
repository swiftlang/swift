//===--- LazyResolver.h - Lazy Resolution for ASTs --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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


  Type resolveMemberType(DeclContext *dc, Type type, Identifier name) override {
    return Principal.resolveMemberType(dc, type, name);
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
};

/// A placeholder for either an array or a member loader.
template <typename T>
class LazyLoaderArray {
  using LengthTy = llvm::PointerEmbeddedInt<size_t, 31>;
  PointerUnion<LengthTy, LazyMemberLoader *> lengthOrLoader;
  uint64_t data = 0;
public:
  explicit LazyLoaderArray() = default;

  /*implicit*/ LazyLoaderArray(ArrayRef<T> members) {
    *this = members;
  }

  LazyLoaderArray(LazyMemberLoader *loader, uint64_t contextData) {
    setLoader(loader, contextData);
  }

  LazyLoaderArray &operator=(ArrayRef<T> members) {
    lengthOrLoader = members.size();
    data = reinterpret_cast<uint64_t>(members.data());
    return *this;
  }

  void setLoader(LazyMemberLoader *loader, uint64_t contextData) {
    lengthOrLoader = loader;
    data = contextData;
  }

  ArrayRef<T> getArray() const {
    assert(!isLazy());
    return llvm::makeArrayRef(reinterpret_cast<T *>(data),
                              lengthOrLoader.get<LengthTy>());
  }

  LazyMemberLoader *getLoader() const {
    assert(isLazy());
    return lengthOrLoader.get<LazyMemberLoader *>();
  }

  uint64_t getLoaderContextData() const {
    assert(isLazy());
    return data;
  }

  bool isLazy() const {
    return lengthOrLoader.is<LazyMemberLoader *>();
  }
};

}

#endif // LLVM_SWIFT_AST_LAZYRESOLVER_H
