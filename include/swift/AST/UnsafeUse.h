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
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/Type.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Unreachable.h"

namespace swift {

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
    /// A reference to an @exclusivity(unchecked) entity.
    ExclusivityUnchecked,
    /// A reference to a nonisolated(unsafe) entity.
    NonisolatedUnsafe,
    /// A reference to an unsafe declaration.
    ReferenceToUnsafe,
    /// A reference to an unsafe storage.
    ReferenceToUnsafeStorage,
    /// A reference to a typealias that is not itself unsafe, but has
    /// an unsafe underlying type.
    ReferenceToUnsafeThroughTypealias,
    /// A call to an unsafe declaration.
    CallToUnsafe,
    /// A @preconcurrency import.
    PreconcurrencyImport,
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
      TypeBase *type;
      void *conformanceRef;
      const void *location;
    } conformance;

    struct {
      const Decl *decl;
      TypeBase *type;
      const void *location;
    } entity;

    const ImportDecl *importDecl;
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
      const Decl *decl,
      Type type,
      SourceLoc location
  ) {
    assert(kind == UnownedUnsafe ||
           kind == ExclusivityUnchecked ||
           kind == NonisolatedUnsafe ||
           kind == ReferenceToUnsafe ||
           kind == ReferenceToUnsafeStorage ||
           kind == ReferenceToUnsafeThroughTypealias ||
           kind == CallToUnsafe);

    UnsafeUse result(kind);
    result.storage.entity.decl = decl;
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

  static UnsafeUse forConformance(Type subjectType,
                                  ProtocolConformanceRef conformance,
                                  SourceLoc location) {
    assert(subjectType);
    UnsafeUse result(UnsafeConformance);
    result.storage.conformance.type = subjectType.getPointer();
    result.storage.conformance.conformanceRef = conformance.getOpaqueValue();
    result.storage.conformance.location = location.getOpaquePointerValue();
    return result;
  }

  static UnsafeUse forUnownedUnsafe(const Decl *decl, SourceLoc location) {
    return forReference(UnownedUnsafe, decl, Type(), location);
  }

  static UnsafeUse forExclusivityUnchecked(const Decl *decl,
                                           SourceLoc location) {
    return forReference(ExclusivityUnchecked, decl, Type(), location);
  }

  static UnsafeUse forNonisolatedUnsafe(const Decl *decl,
                                        SourceLoc location) {
    return forReference(NonisolatedUnsafe, decl, Type(), location);
  }

  static UnsafeUse forReferenceToUnsafe(const Decl *decl,
                                        bool isCall,
                                        Type type,
                                        SourceLoc location) {
    return forReference(isCall ? CallToUnsafe: ReferenceToUnsafe,
                        decl, type, location);
  }

  static UnsafeUse forReferenceToUnsafeStorage(const Decl *decl,
                                               Type type,
                                               SourceLoc location) {
    return forReference(ReferenceToUnsafeStorage, decl, type, location);
  }

  static UnsafeUse forReferenceToUnsafeThroughTypealias(const Decl *decl,
                                        Type type,
                                        SourceLoc location) {
    return forReference(ReferenceToUnsafeThroughTypealias,
                        decl, type, location);
  }

  static UnsafeUse forPreconcurrencyImport(const ImportDecl *importDecl) {
    UnsafeUse result(PreconcurrencyImport);
    result.storage.importDecl = importDecl;
    return result;
  }

  Kind getKind() const { return kind; }

  /// The location at which this use will be diagnosed.
  SourceLoc getLocation() const {
    switch (getKind()) {
    case Override:
    case Witness:
      return getDecl()->getLoc();

    case UnsafeConformance:
      return SourceLoc(
          llvm::SMLoc::getFromPointer(
            (const char *)storage.conformance.location));

    case TypeWitness:
      return SourceLoc(
          llvm::SMLoc::getFromPointer(
            (const char *)storage.typeWitness.location));

    case UnownedUnsafe:
    case ExclusivityUnchecked:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case ReferenceToUnsafeStorage:
    case ReferenceToUnsafeThroughTypealias:
    case CallToUnsafe:
      return SourceLoc(
          llvm::SMLoc::getFromPointer((const char *)storage.entity.location));

    case PreconcurrencyImport:
      return storage.importDecl->getLoc();
    }
  }

  /// Replace the location, if possible.
  void replaceLocation(SourceLoc loc) {
    switch (getKind()) {
    case Override:
    case Witness:
    case PreconcurrencyImport:
      // Cannot replace location.
      return;

    case UnsafeConformance:
      storage.conformance.location = loc.getOpaquePointerValue();
      break;

    case TypeWitness:
      storage.typeWitness.location = loc.getOpaquePointerValue();
      break;

    case UnownedUnsafe:
    case ExclusivityUnchecked:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case ReferenceToUnsafeStorage:
    case ReferenceToUnsafeThroughTypealias:
    case CallToUnsafe:
      storage.entity.location = loc.getOpaquePointerValue();
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
    case ExclusivityUnchecked:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case ReferenceToUnsafeStorage:
    case ReferenceToUnsafeThroughTypealias:
    case CallToUnsafe:
      return storage.entity.decl;

    case UnsafeConformance:
      return nullptr;

    case PreconcurrencyImport:
      return storage.importDecl;
    }
  }

  /// Get the associated type declaration for a type witness.
  const AssociatedTypeDecl *getAssociatedType() const {
    assert(getKind() == TypeWitness);
    return storage.typeWitness.assocType;
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
    case ExclusivityUnchecked:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case ReferenceToUnsafeThroughTypealias:
    case ReferenceToUnsafeStorage:
    case CallToUnsafe:
    case UnsafeConformance:
    case PreconcurrencyImport:
      return nullptr;
    }
  }

  /// Get the type of the reference, if there is one.
  Type getType() const {
    switch (getKind()) {
    case Override:
    case Witness:
    case PreconcurrencyImport:
      return nullptr;

    case UnsafeConformance:
      return storage.conformance.type;

    case TypeWitness:
      return storage.typeWitness.type;

    case UnownedUnsafe:
    case ExclusivityUnchecked:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case ReferenceToUnsafeStorage:
    case ReferenceToUnsafeThroughTypealias:
    case CallToUnsafe:
      return storage.entity.type;
    }
  }

  /// Get the protocol conformance, if there is one.
  ProtocolConformanceRef getConformance() const {
    switch (getKind()) {
    case UnsafeConformance:
      return ProtocolConformanceRef::getFromOpaqueValue(
          storage.conformance.conformanceRef);

    case Witness:
      return ProtocolConformanceRef(storage.polymorphic.conformance);

    case TypeWitness:
      return ProtocolConformanceRef(storage.typeWitness.conformance);

    case Override:
    case UnownedUnsafe:
    case ExclusivityUnchecked:
    case NonisolatedUnsafe:
    case ReferenceToUnsafe:
    case ReferenceToUnsafeStorage:
    case ReferenceToUnsafeThroughTypealias:
    case CallToUnsafe:
    case PreconcurrencyImport:
      return ProtocolConformanceRef::forInvalid();
    }
  }
};

} // end namespace swift

#endif // SWIFT_AST_UNSAFEUSE_H
