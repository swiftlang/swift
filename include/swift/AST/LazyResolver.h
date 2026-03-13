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
// This file defines the abstract interfaces for lazily resolving declarations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LAZYRESOLVER_H
#define SWIFT_AST_LAZYRESOLVER_H

#include "llvm/ADT/PointerEmbeddedInt.h"

namespace swift {

class AssociatedTypeDecl;
class Decl;
class DeclContext;
class IterableDeclContext;
class ExtensionDecl;
class Identifier;
class NominalTypeDecl;
class NormalProtocolConformance;
class ProtocolConformance;
class ProtocolDecl;
class ProtocolTypeAlias;
class TypeDecl;
class ValueDecl;
class VarDecl;

class LazyMemberLoader;

/// Context data for lazy deserialization.
class LazyContextData {
public:
  /// The lazy member loader for this context.
  LazyMemberLoader *loader;
};

/// Context data for iterable decl contexts.
class LazyIterableDeclContextData : public LazyContextData {
public:
  /// The context data used for loading all of the members of the iterable
  /// context.
  uint64_t memberData = 0;

  /// The context data used for loading all of the conformances of the
  /// iterable context.
  uint64_t allConformancesData = 0;
};

class LazyAssociatedTypeData : public LazyContextData {
public:
  /// The context data used for loading the default type.
  uint64_t defaultDefinitionTypeData = 0;
};

class LazyOpaqueTypeData : public LazyContextData {
public:
  /// The context data used for loading the underlying type substitution map.
  uint64_t underlyingSubsData = 0;
};

/// Context data for protocols.
class LazyProtocolData : public LazyIterableDeclContextData {
public:
  /// The context data used for loading a requirement signature.
  uint64_t requirementSignatureData = 0;
  /// The context data used for loading the list of associated types.
  uint64_t associatedTypesData = 0;
  /// The context data used for loading the list of primary associated types.
  uint64_t primaryAssociatedTypesData = 0;
};

/// A class that can lazily load members from a serialized format.
class alignas(void*) LazyMemberLoader {
  virtual void anchor();

public:
  virtual ~LazyMemberLoader() = default;

  /// Populates a given decl \p D with all members that affect storage.
  ///
  /// INVARIANT: this method should only ever be called once for \p D, to avoid
  /// adding duplicates to the linked list of members. It is the responsibility
  /// of the caller (IterableDeclContext) to ensure this.
  ///
  /// An exception is when \p D is an imported clang::NamespaceDecl, in which
  /// case no members are ever added (kind of a hack, should consider fixing it)
  virtual void loadStorageMembers(Decl *D, uint64_t contextData) = 0;

  /// Populates a given decl \p D with all members that do not affect storage.
  ///
  /// INVARIANT: this method should only ever be called once for \p D, to avoid
  /// adding duplicates to the linked list of members. It is the responsibility
  /// of the caller (IterableDeclContext) to ensure this.
  ///
  /// INVARIANT: the caller (an IterableDeclContext with a lazy loader)
  /// must have already called loadStorageMembers() before calling this.
  virtual void loadNonStorageMembers(Decl *D, uint64_t contextData) = 0;

  /// Populates a vector with all members of \p IDC that have DeclName
  /// matching \p N.
  virtual TinyPtrVector<ValueDecl *>
  loadNamedMembers(const IterableDeclContext *IDC, DeclBaseName N,
                   uint64_t contextData) = 0;

  /// Populates the given vector with all conformances for \p D.
  ///
  /// The implementation should \em not call setConformances on \p D.
  virtual void
  loadAllConformances(const Decl *D, uint64_t contextData,
                      SmallVectorImpl<ProtocolConformance *> &Conformances) = 0;

  /// Returns the default definition type for \p ATD.
  virtual Type loadAssociatedTypeDefault(const AssociatedTypeDecl *ATD,
                                         uint64_t contextData) = 0;

  /// Loads the requirement signature for a protocol.
  virtual void
  loadRequirementSignature(const ProtocolDecl *proto, uint64_t contextData,
                           SmallVectorImpl<Requirement> &requirements,
                           SmallVectorImpl<ProtocolTypeAlias> &typeAliases) = 0;

  /// Loads the associated types of a protocol.
  virtual void
  loadAssociatedTypes(const ProtocolDecl *proto, uint64_t contextData,
                      SmallVectorImpl<AssociatedTypeDecl *> &assocTypes) = 0;

  /// Loads the primary associated types of a protocol.
  virtual void
  loadPrimaryAssociatedTypes(const ProtocolDecl *proto, uint64_t contextData,
                             SmallVectorImpl<AssociatedTypeDecl *> &assocTypes) = 0;

  /// Returns the replaced decl for a given @_dynamicReplacement(for:) attribute.
  virtual ValueDecl *
  loadDynamicallyReplacedFunctionDecl(const DynamicReplacementAttr *DRA,
                                      uint64_t contextData) = 0;

  /// Returns the referenced original declaration for a `@derivative(of:)`
  /// attribute.
  virtual AbstractFunctionDecl *
  loadReferencedFunctionDecl(const DerivativeAttr *DA,
                             uint64_t contextData) = 0;

  /// Returns the type for a given @_typeEraser() attribute.
  virtual Type loadTypeEraserType(const TypeEraserAttr *TRA,
                                  uint64_t contextData) = 0;

  // Returns the target parameter of the `@_specialize` attribute or null.
  virtual ValueDecl *loadTargetFunctionDecl(const AbstractSpecializeAttr *attr,
                                            uint64_t contextData) = 0;

  /// Loads the underlying type substitution map of an opaque result declaration.
  virtual void
  finishOpaqueTypeDecl(OpaqueTypeDecl *opaqueDecl, uint64_t contextData) = 0;
};

/// A class that can lazily load conformances from a serialized format.
class alignas(void*) LazyConformanceLoader {
  virtual void anchor();

public:
  virtual ~LazyConformanceLoader() = default;

  /// Populates the given protocol conformance.
  virtual void
  finishNormalConformance(NormalProtocolConformance *conformance,
                          uint64_t contextData) = 0;
};

}

#endif // LLVM_SWIFT_AST_LAZYRESOLVER_H
