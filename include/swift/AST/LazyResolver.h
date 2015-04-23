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

#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/Fixnum.h"

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
class ValueDecl;

/// Abstract interface used to lazily resolve aspects of the AST, such as the
/// types of declarations or protocol conformance structures.
class LazyResolver {
public:
  virtual ~LazyResolver();

  /// Completely check the given normal protocol conformance.
  ///
  /// \param conformance The normal protocol conformance.
  ///
  /// FIXME: We shouldn't need this as an entry to the lazy resolver, because
  /// completely checking of conformances is only interesting when we're doing
  /// complete checking of the declaration context. However, it is needed now
  /// to maintain the order of checking, because resolveTypeWitness/
  /// resolveWitness aren't lazy enough.
  virtual void checkConformance(NormalProtocolConformance *conformance) = 0;

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
  /// declaration context, which will be a nominal type declaration or
  /// extension declaration.
  virtual void resolveInheritanceClause(DeclContext *dc) = 0;

  /// Resolve the superclass of the given class.
  virtual void resolveSuperclass(ClassDecl *classDecl) = 0;

  /// Resolve the raw type of the given enum.
  virtual void resolveRawType(EnumDecl *enumDecl) = 0;

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


/// A class that can lazily load members from a serialized format.
class alignas(void*) LazyMemberLoader {
  virtual void anchor();
public:
  virtual ~LazyMemberLoader() = default;

  /// Populates the given vector with all member decls for \p D.
  ///
  /// The implementation should add the members to D.
  ///
  /// \param[out] hasMissingRequiredMembers If present, set to true if any
  /// members failed to import and were non-optional protocol requirements.
  virtual void
  loadAllMembers(Decl *D, uint64_t contextData,
                 bool *hasMissingRequiredMembers = nullptr) {
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

  /// Returns the default definition type for \p ATD.
  virtual TypeLoc loadAssociatedTypeDefault(const AssociatedTypeDecl *ATD,
                                            uint64_t contextData) {
    llvm_unreachable("unimplemented");
  }
};

/// A placeholder for either an array or a member loader.
template <typename T>
class LazyLoaderArray {
  using LengthTy = llvm::Fixnum<31>;
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
