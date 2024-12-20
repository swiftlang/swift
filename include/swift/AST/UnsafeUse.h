//===--- UnsafeUse.h - A use of an unsafe construct -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_UNSAFEUSE_H
#define SWIFT_AST_UNSAFEUSE_H

#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Type.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Unreachable.h"

namespace swift {

class NormalProtocolConformance;

/// Describes a use of an unsafe construct.
///
/// Every use of an unsafe construct that should be diagnosed will be captured
/// in an instance of this type.
class UnsafeUse {
public:
  enum Kind {
    /// An unsafe declaration overriding a safe one.
    Override,
    /// An unsafe declaration witnessing a safe one.
    Witness,
    /// An unsafe type witnesses a safe associated type.
    TypeWitness,
    /// An @unsafe or @unchecked conformance (e.g., Sendable).
    UnsafeConformance,
    /// A reference to an unowned(unsafe) entity.
    UnownedUnsafe,
    /// A reference to a nonisolated(unsafe) entity.
    NonisolatedUnsafe,
    /// A reference to an unsafe declaration.
    ReferenceToUnsafe,
    /// A call to an unsafe declaration.
    CallToUnsafe,
  };

private:
  Kind kind;

  union {
    struct {
      const Decl *decl;
      const Decl *original;
      NormalProtocolConformance *conformance;
    } polymorphic;

    struct {
      const AssociatedTypeDecl *assocType;
      TypeBase *type;
      NormalProtocolConformance *conformance;
      const void *location;
    } typeWitness;

    struct {
      NormalProtocolConformance *conformance;
      DeclContext *declContext;
      const void *location;
    } conformance;

    struct {
      const Decl *decl;
      DeclContext *declContext;
      TypeBase *type;
      const void *location;
    } entity;
  } storage;

  UnsafeUse(Kind kind) : kind(kind) { }

  static UnsafeUse forPolymorphic(
      Kind kind,
      const Decl *decl,
      const Decl *original,
      NormalProtocolConformance *conformance
  ) {
    assert(kind == Override || kind == Witness);

    UnsafeUse result(kind);
    result.storage.polymorphic.decl = decl;
    result.storage.polymorphic.original = original;
    result.storage.polymorphic.conformance = conformance;
    return result;
  }

  static UnsafeUse forReference(
      Kind kind,
      DeclContext *dc,
      const Decl *decl,
      Type type,
      SourceLoc location
  ) {
    assert(kind == UnownedUnsafe ||
           kind == NonisolatedUnsafe ||
           kind == ReferenceToUnsafe ||
           kind == CallToUnsafe);

    UnsafeUse result(kind);
    result.storage.entity.decl = decl;
    result.storage.entity.declContext = dc;
    result.storage.entity.type = type.getPointer();
    result.storage.entity.location = location.getOpaquePointerValue();
    return result;

  }

public:
  static UnsafeUse forOverride(const Decl *decl, const Decl *overridden) {
    return forPolymorphic(Override, decl, overridden, nullptr);
  }

  static UnsafeUse forWitness(const Decl *witness, const Decl *requirement,
                              NormalProtocolConformance *conformance) {
    return forPolymorphic(Witness, witness, requirement, conformance);
  }

  static UnsafeUse forTypeWitness(const AssociatedTypeDecl *assocType,
                                  Type typeWitness,
                                  NormalProtocolConformance *conformance,
                                  SourceLoc location) {
    UnsafeUse result(TypeWitness);
    result.storage.typeWitness.assocType = assocType;
    result.storage.typeWitness.type = typeWitness.getPointer();
    result.storage.typeWitness.conformance = conformance;
    result.storage.typeWitness.location = location.getOpaquePointerValue();
    return result;
  }

  static UnsafeUse forConformance(NormalProtocolConformance *conformance,
                                  SourceLoc location,
                                  DeclContext *dc) {
    UnsafeUse result(UnsafeConformance);
    result.storage.conformance.conformance = conformance;
    result.storage.conformance.declContext = dc;
    result.storage.conformance.location = location.getOpaquePointerValue();
    return result;
  }

  static UnsafeUse forUnownedUnsafe(const Decl *decl,
                                    SourceLoc location,
                                    DeclContext *dc) {
    return forReference(UnownedUnsafe, dc, decl, Type(), location);
  }

  static UnsafeUse forNonisolatedUnsafe(const Decl *decl,
                                        SourceLoc location,
                                        DeclContext *dc) {
    return forReference(NonisolatedUnsafe, dc, decl, Type(), location);
  }

  static UnsafeUse forReferenceToUnsafe(const Decl *decl,
                                        bool isCall,
                                        DeclContext *dc,
                                        Type type,
                                        SourceLoc location) {
    return forReference(isCall ? CallToUnsafe: ReferenceToUnsafe, dc,
                        decl, type, location);
  }

  Kind getKind() const { return kind; }

  /// The location at which this use will be diagnosed.
  SourceLoc getLocation() const {
    switch (getKind()) {
    case Override:
    case Witness:
      return getDecl()->getLoc();

    case UnsafeConformance:
      return getConformance()->getLoc();

    case TypeWitness:
      return SourceLoc(
          llvm::SMLoc::getFromPointer(
            (const char *)storage.typeWitness.location));

    case UnownedUnsafe:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case CallToUnsafe:
      return SourceLoc(
          llvm::SMLoc::getFromPointer((const char *)storage.entity.location));
    }
  }

  /// Get the main declaration, when there is one.
  const Decl *getDecl() const {
    switch (getKind()) {
    case Override:
    case Witness:
      return storage.polymorphic.decl;

    case TypeWitness:
      return storage.typeWitness.assocType;

    case UnownedUnsafe:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case CallToUnsafe:
      return storage.entity.decl;

    case UnsafeConformance:
      return nullptr;
    }
  }

  /// Get the associated type declaration for a type witness.
  const AssociatedTypeDecl *getAssociatedType() const {
    assert(getKind() == TypeWitness);
    return storage.typeWitness.assocType;
  }

  /// Retrieve the declaration context in which the reference occurs.
  DeclContext *getDeclContext() const {
    switch (getKind()) {
    case UnownedUnsafe:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case CallToUnsafe:
      return storage.entity.declContext;

    case Override:
      return getDecl()->getDeclContext();

    case Witness:
    case TypeWitness:
      return getConformance()->getDeclContext();

    case UnsafeConformance:
      return storage.conformance.declContext;
    }
  }

  /// Get the original declaration for an issue with a polymorphic
  /// implementation, e.g., an overridden declaration or a protocol
  /// requirement.
  const Decl *getOriginalDecl() const {
    switch (getKind()) {
    case Override:
    case Witness:
      return storage.polymorphic.original;

    case TypeWitness:
    case UnownedUnsafe:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case CallToUnsafe:
    case UnsafeConformance:
      return nullptr;
    }
  }

  /// Get the type of the reference, if there is one.
  Type getType() const {
    switch (getKind()) {
    case Override:
    case Witness:
    case UnsafeConformance:
      return nullptr;

    case TypeWitness:
      return storage.typeWitness.type;

    case UnownedUnsafe:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case CallToUnsafe:
      return storage.entity.type;
    }
  }

  /// Get the protocol conformance, if there is one.
  NormalProtocolConformance *getConformance() const {
    switch (getKind()) {
    case UnsafeConformance:
      return storage.conformance.conformance;

    case Witness:
      return storage.polymorphic.conformance;

    case TypeWitness:
      return storage.typeWitness.conformance;

    case Override:
    case UnownedUnsafe:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case CallToUnsafe:
      return nullptr;
    }
  }
};

} // end namespace swift

#endif // SWIFT_AST_UNSAFEUSE_H
