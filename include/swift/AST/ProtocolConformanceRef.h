//===--- ProtocolConformanceRef.h - AST Protocol Conformance ----*- C++ -*-===//
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
// This file defines the ProtocolConformanceRef type.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_PROTOCOLCONFORMANCEREF_H
#define SWIFT_AST_PROTOCOLCONFORMANCEREF_H

#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Debug.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include <optional>

namespace swift {
class AbstractConformance;
}

namespace llvm {
class raw_ostream;

template <>
struct PointerLikeTypeTraits<swift::AbstractConformance *> {
public:
  static inline void *getAsVoidPointer(swift::AbstractConformance *ptr) {
    return ptr;
  }
  static inline swift::AbstractConformance *getFromVoidPointer(void *ptr) {
    return (swift::AbstractConformance *)ptr;
  }
  enum { NumLowBitsAvailable = swift::TypeAlignInBits };
};
}

namespace swift {

class BuiltinProtocolConformance;
class ConcreteDeclRef;
class PackConformance;
class ProtocolConformance;
class Requirement;
enum class EffectKind : uint8_t;

/// A ProtocolConformanceRef is a handle to a protocol conformance which
/// may be either concrete or abstract.
///
/// A concrete conformance is derived from a specific protocol conformance
/// declaration.
///
/// An abstract conformance is derived from context: the conforming type
/// is either existential or opaque (i.e. an archetype), and while the
/// type-checker promises that the conformance exists, it is not known
/// statically which concrete conformance it refers to.
///
/// ProtocolConformanceRef allows the efficient recovery of the protocol
/// even when the conformance is abstract.
class ProtocolConformanceRef {
public:
  using UnionType = llvm::PointerUnion<AbstractConformance *,
                                       ProtocolConformance *,
                                       PackConformance *>;

private:
  UnionType Union;

  explicit ProtocolConformanceRef(UnionType value) : Union(value) {}

public:
  ProtocolConformanceRef() : Union() {}
  ProtocolConformanceRef(std::nullptr_t) : Union() {}

  /// Create an abstract protocol conformance reference.
  explicit ProtocolConformanceRef(AbstractConformance *abstract)
      : Union(abstract) {
    assert(abstract != nullptr &&
           "cannot construct ProtocolConformanceRef with null");
  }

  /// Create a concrete protocol conformance reference.
  explicit ProtocolConformanceRef(ProtocolConformance *conf) : Union(conf) {
    assert(conf != nullptr &&
           "cannot construct ProtocolConformanceRef with null");
  }

  /// Create a pack protocol conformance reference.
  explicit ProtocolConformanceRef(PackConformance *conf) : Union(conf) {
    assert(conf != nullptr &&
           "cannot construct ProtocolConformanceRef with null");
  }

  static ProtocolConformanceRef forInvalid() {
    return ProtocolConformanceRef();
  }

  /// Retrieve an invalid or missing conformance, as appropriate, when a
  /// legitimate conformance doesn't exist.
  static ProtocolConformanceRef forMissingOrInvalid(
      Type type, ProtocolDecl *proto);

  bool isInvalid() const;

  explicit operator bool() const { return !isInvalid(); }

  /// Create an abstract conformance for a type parameter or archetype.
  static ProtocolConformanceRef forAbstract(Type conformingType,
                                            ProtocolDecl *protocol);

  bool isConcrete() const {
    return !isInvalid() && isa<ProtocolConformance *>(Union);
  }
  ProtocolConformance *getConcrete() const {
    ASSERT(isConcrete());
    return cast<ProtocolConformance *>(Union);
  }

  bool isPack() const { return !isInvalid() && isa<PackConformance *>(Union); }
  PackConformance *getPack() const {
    ASSERT(isPack());
    return cast<PackConformance *>(Union);
  }

  bool isAbstract() const {
    return !isInvalid() && isa<AbstractConformance *>(Union);
  }

  AbstractConformance *getAbstract() const {
    ASSERT(isAbstract());
    return cast<AbstractConformance *>(Union);
  }

  /// Determine whether this conformance (or a conformance it depends on)
  /// involves an always-unavailable conformance.
  bool hasUnavailableConformance() const;

  /// Determine whether this conformance (or a conformance it depends on)
  /// involves a "missing" conformance anywhere. Such conformances
  /// cannot be depended on to always exist.
  bool hasMissingConformance() const;

  /// Enumerate the missing conformances in this conformance.
  ///
  /// Calls \c fn with each missing conformance found within this conformance,
  /// including this conformance or any conditional conformances it depends on.
  /// If the invocation of \c fn returns \c true, the traversal exits early
  /// and the overall function returns \c true.
  ///
  /// \returns \c true if any invocation of \c fn returned true,
  /// \c false otherwise.
  bool forEachMissingConformance(
      llvm::function_ref<bool(BuiltinProtocolConformance *missing)> fn) const;

  /// Enumerate all of the isolated conformances in the given conformance.
  ///
  /// The given `body` will be called on each isolated conformance. If it ever
  /// returns `true`, this function will abort the search and return `true`.
  bool forEachIsolatedConformance(
      llvm::function_ref<bool(ProtocolConformanceRef)> body
  ) const;

  using OpaqueValue = void*;
  OpaqueValue getOpaqueValue() const { return Union.getOpaqueValue(); }
  static ProtocolConformanceRef getFromOpaqueValue(OpaqueValue value) {
    return ProtocolConformanceRef(UnionType::getFromOpaqueValue(value));
  }

  /// Retrieve the conforming type.
  Type getType() const;

  /// Return the protocol requirement.
  ProtocolDecl *getProtocol() const;
  
  /// Apply a substitution to the conforming type.
  ProtocolConformanceRef subst(SubstitutionMap subMap,
                               SubstOptions options = std::nullopt) const;

  /// Apply a substitution to the conforming type.
  ProtocolConformanceRef subst(TypeSubstitutionFn subs,
                               LookupConformanceFn conformances,
                               SubstOptions options = std::nullopt) const;

  /// Apply a substitution to the conforming type.
  ///
  /// This function should generally not be used outside of the substitution
  /// subsystem.
  ProtocolConformanceRef subst(InFlightSubstitution &IFS) const;

  /// Map contextual types to interface types in the conformance.
  ProtocolConformanceRef mapConformanceOutOfContext() const;

  /// Look up the type witness for an associated type declaration in this
  /// conformance.
  Type getTypeWitness(AssociatedTypeDecl *assocType,
                      SubstOptions options = std::nullopt) const;

  /// Given a dependent type (expressed in terms of this conformance's
  /// protocol), follow it from the conforming type.
  Type getAssociatedType(Type dependentType) const;

  /// Given a dependent type (expressed in terms of this conformance's
  /// protocol) and conformance, follow it from the conforming type.
  ProtocolConformanceRef
  getAssociatedConformance(Type dependentType, ProtocolDecl *requirement) const;

  SWIFT_DEBUG_DUMP;
  void dump(llvm::raw_ostream &out, unsigned indent = 0,
            bool details = true) const;

  void print(llvm::raw_ostream &out) const;

  bool operator==(ProtocolConformanceRef other) const {
    return Union == other.Union;
  }
  bool operator!=(ProtocolConformanceRef other) const {
    return Union != other.Union;
  }

  friend llvm::hash_code hash_value(ProtocolConformanceRef conformance) {
    return llvm::hash_value(conformance.Union.getOpaqueValue());
  }

  Type getTypeWitnessByName(Identifier name) const;

  /// Find a particular named function witness for a type that conforms to
  /// the given protocol.
  ///
  /// \param type The conforming type.
  ///
  /// \param name The name of the requirement.
  ConcreteDeclRef getWitnessByName(DeclName name) const;

  /// Determine whether this conformance is canonical.
  bool isCanonical() const;

  /// Create a canonical conformance from the current one.
  ProtocolConformanceRef getCanonicalConformanceRef() const;

  /// Get any additional requirements that are required for this conformance to
  /// be satisfied.
  ArrayRef<Requirement> getConditionalRequirements() const;

  bool hasEffect(EffectKind kind) const;
};

void simple_display(llvm::raw_ostream &out, ProtocolConformanceRef conformanceRef);
SourceLoc extractNearestSourceLoc(const ProtocolConformanceRef conformanceRef);

} // end namespace swift

namespace llvm {
class raw_ostream;

template <>
struct PointerLikeTypeTraits<swift::ProtocolConformanceRef>
  : PointerLikeTypeTraits<swift::ProtocolConformanceRef::UnionType>
{
public:
  static inline void *getAsVoidPointer(swift::ProtocolConformanceRef ref) {
    return ref.getOpaqueValue();
  }
  static inline swift::ProtocolConformanceRef getFromVoidPointer(void *ptr) {
    return swift::ProtocolConformanceRef::getFromOpaqueValue(ptr);
  }
};

}

#endif // LLVM_SWIFT_AST_PROTOCOLCONFORMANCEREF_H
