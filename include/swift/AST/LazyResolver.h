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

#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/TypeLoc.h"
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

/// Context data for protocols.
class LazyProtocolData : public LazyIterableDeclContextData {
public:
  /// The context data used for loading all of the members of the iterable
  /// context.
  uint64_t requirementSignatureData = 0;
};

/// A class that can lazily load members from a serialized format.
class alignas(void*) LazyMemberLoader {
  virtual void anchor();

public:
  virtual ~LazyMemberLoader() = default;

  /// Populates a given decl \p D with all of its members.
  ///
  /// The implementation should add the members to D.
  virtual void
  loadAllMembers(Decl *D, uint64_t contextData) = 0;

  /// Populates a vector with all members of \p IDC that have DeclName
  /// matching \p N.
  ///
  /// Returns None if an error occurred \em or named member-lookup
  /// was otherwise unsupported in this implementation or Decl.
  virtual Optional<TinyPtrVector<ValueDecl *>>
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
                           SmallVectorImpl<Requirement> &requirements) = 0;

  /// Returns the replaced decl for a given @_dynamicReplacement(for:) attribute.
  virtual ValueDecl *
  loadDynamicallyReplacedFunctionDecl(const DynamicReplacementAttr *DRA,
                                      uint64_t contextData) = 0;
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
