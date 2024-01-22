//===--- TypeCheckProtocol.h - Constraint-based Type Checking ----*- C++ -*-===//
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
// This file provides the constraint-based type checker, anchored by the
// \c ConstraintSystem class, which provides type checking and type
// inference for expressions.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_PROTOCOL_H
#define SWIFT_SEMA_PROTOCOL_H

#include "TypeChecker.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/RequirementEnvironment.h"
#include "swift/AST/RequirementMatch.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/AST/Witness.h"
#include "swift/Basic/Debug.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class AssociatedTypeDecl;
class AvailabilityContext;
class DeclContext;
class FuncDecl;
class NormalProtocolConformance;
class ProtocolDecl;
class TypeRepr;
class ValueDecl;

class RequirementEnvironment;

/// Gather the value witnesses for the given requirement.
///
/// \param DC A nominal type or extension context where the conformance
/// was declared.
/// \param req A member of a protocol that DC conforms to.
/// \param ignoringNames If non-null and there are no value
/// witnesses with the correct full name, the results will reflect
/// lookup for just the base name and the pointee will be set to
/// \c true.
SmallVector<ValueDecl *, 4> lookupValueWitnesses(DeclContext *DC,
                                                 ValueDecl *req,
                                                 bool *ignoringNames);

struct RequirementCheck;

class WitnessChecker {
public:
  using RequirementEnvironmentCacheKey =
      std::pair<const GenericSignatureImpl *, const ClassDecl *>;
  using RequirementEnvironmentCache =
      llvm::DenseMap<RequirementEnvironmentCacheKey, RequirementEnvironment>;

protected:
  ASTContext &Context;
  ProtocolDecl *Proto;
  Type Adoptee;
  // The conforming context, either a nominal type or extension.
  DeclContext *DC;

  ASTContext &getASTContext() const { return Context; }

  // An auxiliary lookup table to be used for witnesses remapped via
  // @_implements(Protocol, DeclName)
  llvm::DenseMap<DeclName, llvm::TinyPtrVector<ValueDecl *>> ImplementsTable;

  RequirementEnvironmentCache ReqEnvironmentCache;

  WitnessChecker(ASTContext &ctx, ProtocolDecl *proto, Type adoptee,
                 DeclContext *dc);

  void lookupValueWitnessesViaImplementsAttr(ValueDecl *req,
                                             SmallVector<ValueDecl *, 4>
                                             &witnesses);

  bool findBestWitness(ValueDecl *requirement,
                       bool *ignoringNames,
                       NormalProtocolConformance *conformance,
                       SmallVectorImpl<RequirementMatch> &matches,
                       unsigned &numViable,
                       unsigned &bestIdx,
                       bool &doNotDiagnoseMatches);

  bool checkWitnessAccess(ValueDecl *requirement,
                          ValueDecl *witness,
                          bool *isSetter);

  bool checkWitnessAvailability(ValueDecl *requirement,
                                ValueDecl *witness,
                                AvailabilityContext *requirementInfo);

  RequirementCheck checkWitness(ValueDecl *requirement,
                                const RequirementMatch &match);
};

/// The result of attempting to resolve a witness.
enum class ResolveWitnessResult {
  /// The resolution succeeded.
  Success,
  /// There was an explicit witness available, but it failed some
  /// criteria.
  ExplicitFailed,
  /// There was no witness available.
  Missing
};

enum class MissingWitnessDiagnosisKind {
  FixItOnly,
  ErrorOnly,
  ErrorFixIt,
};

class AssociatedTypeInference;
class MultiConformanceChecker;

/// The protocol conformance checker.
///
/// This helper class handles most of the details of checking whether a
/// given type (\c Adoptee) conforms to a protocol (\c Proto).
class ConformanceChecker : public WitnessChecker {
public:
  /// Key that can be used to uniquely identify a particular Objective-C
  /// method.
  using ObjCMethodKey = std::pair<ObjCSelector, char>;

private:
  friend class MultiConformanceChecker;
  friend class AssociatedTypeInference;

  NormalProtocolConformance *Conformance;
  SourceLoc Loc;

  /// Witnesses that are currently being resolved.
  llvm::SmallPtrSet<ValueDecl *, 4> ResolvingWitnesses;

  /// Keep track of missing witnesses, either type or value, for later
  /// diagnosis emits. This may contain witnesses that are external to the
  /// protocol under checking.
  llvm::SetVector<ASTContext::MissingWitness> &GlobalMissingWitnesses;

  /// Keep track of the slice in GlobalMissingWitnesses that is local to
  /// this protocol under checking.
  unsigned LocalMissingWitnessesStartIndex;

  /// Whether we've already complained about problems with this conformance.
  bool AlreadyComplained = false;

  /// Mapping from Objective-C methods to the set of requirements within this
  /// protocol that have the same selector and instance/class designation.
  llvm::SmallDenseMap<ObjCMethodKey, TinyPtrVector<AbstractFunctionDecl *>, 4>
    objcMethodRequirements;

  /// Whether objcMethodRequirements has been computed.
  bool computedObjCMethodRequirements = false;

  /// Record a (non-type) witness for the given requirement.
  void recordWitness(ValueDecl *requirement, const RequirementMatch &match);

  /// Record that the given optional requirement has no witness.
  void recordOptionalWitness(ValueDecl *requirement);

  /// Record that the given requirement has no valid witness.
  void recordInvalidWitness(ValueDecl *requirement);

  /// Check for ill-formed uses of Objective-C generics in a type witness.
  bool checkObjCTypeErasedGenerics(AssociatedTypeDecl *assocType,
                                   Type type,
                                   TypeDecl *typeDecl);

  /// Check that the witness and requirement have compatible actor contexts.
  ///
  /// \returns the isolation that needs to be enforced to invoke the witness
  /// from the requirement, used when entering an actor-isolated synchronous
  /// witness from an asynchronous requirement.
  llvm::Optional<ActorIsolation> checkActorIsolation(ValueDecl *requirement,
                                                     ValueDecl *witness);

  /// Enforce restrictions on non-final classes witnessing requirements
  /// involving the protocol 'Self' type.
  void checkNonFinalClassWitness(ValueDecl *requirement,
                                 ValueDecl *witness);

  /// Resolve a (non-type) witness via name lookup.
  ResolveWitnessResult resolveWitnessViaLookup(ValueDecl *requirement);

  /// Resolve a (non-type) witness via derivation.
  ResolveWitnessResult resolveWitnessViaDerivation(ValueDecl *requirement);

  /// Resolve a (non-type) witness via default definition or optional.
  ResolveWitnessResult resolveWitnessViaDefault(ValueDecl *requirement);

  /// Resolve a (non-type) witness by trying each standard strategy until one
  /// of them produces a result.
  ResolveWitnessResult
  resolveWitnessTryingAllStrategies(ValueDecl *requirement);

  /// Attempt to resolve a type witness via member name lookup.
  ResolveWitnessResult resolveTypeWitnessViaLookup(
                         AssociatedTypeDecl *assocType);

  /// Check whether all of the protocol's generic requirements are satisfied by
  /// the chosen type witnesses.
  void ensureRequirementsAreSatisfied();

  ArrayRef<ASTContext::MissingWitness> getLocalMissingWitness() {
    return GlobalMissingWitnesses.getArrayRef().
      slice(LocalMissingWitnessesStartIndex,
            GlobalMissingWitnesses.size() - LocalMissingWitnessesStartIndex);
  }

  void clearGlobalMissingWitnesses() {
    GlobalMissingWitnesses.clear();
    LocalMissingWitnessesStartIndex = GlobalMissingWitnesses.size();
  }

public:
  /// Call this to diagnose currently known missing witnesses.
  ///
  /// \returns true if any witnesses were diagnosed.
  bool diagnoseMissingWitnesses(MissingWitnessDiagnosisKind Kind,
                                bool Delayed);

  /// Emit any diagnostics that have been delayed.
  void emitDelayedDiags();

  ConformanceChecker(ASTContext &ctx, NormalProtocolConformance *conformance,
                     llvm::SetVector<ASTContext::MissingWitness> &GlobalMissingWitnesses);

  ~ConformanceChecker();

  /// Resolve all of the type witnesses.
  void resolveTypeWitnesses();

  /// Resolve all of the non-type witnesses.
  void resolveValueWitnesses();

  /// Resolve the witness for the given non-type requirement as
  /// directly as possible, only resolving other witnesses if
  /// needed, e.g., to determine type witnesses used within the
  /// requirement.
  ///
  /// This entry point is designed to be used when the witness for a
  /// particular requirement and adoptee is required, before the
  /// conformance has been completed checked.
  void resolveSingleWitness(ValueDecl *requirement);

  /// Resolve the type witness for the given associated type as
  /// directly as possible.
  void resolveSingleTypeWitness(AssociatedTypeDecl *assocType);

  /// Check the entire protocol conformance, ensuring that all
  /// witnesses are resolved and emitting any diagnostics.
  void checkConformance(MissingWitnessDiagnosisKind Kind);

  /// Retrieve the Objective-C method key from the given function.
  ObjCMethodKey getObjCMethodKey(AbstractFunctionDecl *func);

  /// Retrieve the Objective-C requirements in this protocol that have the
  /// given Objective-C method key.
  ArrayRef<AbstractFunctionDecl *> getObjCRequirements(ObjCMethodKey key);

  /// @returns a non-null requirement if the given requirement is part of a
  /// group of ObjC requirements that share the same ObjC method key.
  /// The first such requirement that the predicate function returns true for
  /// is the requirement required by this function. Otherwise, nullptr is
  /// returned.
  ValueDecl *getObjCRequirementSibling(ValueDecl *requirement,
                    llvm::function_ref<bool(AbstractFunctionDecl *)>predicate);
};

/// A system for recording and probing the integrity of a type witness solution
/// for a set of unresolved associated type declarations.
///
/// Right now can reason only about abstract type witnesses, i.e., same-type
/// constraints, default type definitions, and bindings to generic parameters.
class TypeWitnessSystem final {
  /// Equivalence classes are used on demand to express equivalences between
  /// witness candidates and reflect changes to resolved types across their
  /// members.
  class EquivalenceClass final {
    /// The pointer:
    /// - The resolved type for witness candidates belonging to this equivalence
    ///   class. The resolved type may be a type parameter, but cannot directly
    ///   pertain to a name variable in the owning system; instead, witness
    ///   candidates that should resolve to the same type share an equivalence
    ///   class.
    /// The int:
    /// - A flag indicating whether the resolved type is ambiguous. When set,
    ///   the resolved type is null.
    llvm::PointerIntPair<Type, 1, bool> ResolvedTyAndIsAmbiguous;

  public:
    EquivalenceClass(Type ty) : ResolvedTyAndIsAmbiguous(ty, false) {}

    EquivalenceClass(const EquivalenceClass &) = delete;
    EquivalenceClass(EquivalenceClass &&) = delete;
    EquivalenceClass &operator=(const EquivalenceClass &) = delete;
    EquivalenceClass &operator=(EquivalenceClass &&) = delete;

    Type getResolvedType() const {
      return ResolvedTyAndIsAmbiguous.getPointer();
    }
    void setResolvedType(Type ty);

    bool isAmbiguous() const {
      return ResolvedTyAndIsAmbiguous.getInt();
    }
    void setAmbiguous() {
      ResolvedTyAndIsAmbiguous = {nullptr, true};
    }
  };

  /// A type witness candidate for a name variable.
  struct TypeWitnessCandidate final {
    /// The defaulted associated type declaration correlating with this
    /// candidate, if present.
    AssociatedTypeDecl *DefaultedAssocType;

    /// The equivalence class of this candidate.
    EquivalenceClass *EquivClass;
  };

  /// The set of equivalence classes in the system.
  llvm::SmallPtrSet<EquivalenceClass *, 4> EquivalenceClasses;

  /// The mapping from name variables (the names of unresolved associated
  /// type declarations) to their corresponding type witness candidates.
  llvm::SmallDenseMap<Identifier, TypeWitnessCandidate, 4> TypeWitnesses;

public:
  TypeWitnessSystem(ArrayRef<AssociatedTypeDecl *> assocTypes);
  ~TypeWitnessSystem();

  TypeWitnessSystem(const TypeWitnessSystem &) = delete;
  TypeWitnessSystem(TypeWitnessSystem &&) = delete;
  TypeWitnessSystem &operator=(const TypeWitnessSystem &) = delete;
  TypeWitnessSystem &operator=(TypeWitnessSystem &&) = delete;

  /// Get the resolved type witness for the associated type with the given name.
  Type getResolvedTypeWitness(Identifier name) const;
  bool hasResolvedTypeWitness(Identifier name) const;

  /// Get the defaulted associated type relating to the resolved type witness
  /// for the associated type with the given name, if present.
  AssociatedTypeDecl *getDefaultedAssocType(Identifier name) const;

  /// Record a type witness for the given associated type name.
  ///
  /// \note This need not lead to the resolution of a type witness, e.g.
  /// an associated type may be defaulted to another.
  void addTypeWitness(Identifier name, Type type);

  /// Record a default type witness.
  ///
  /// \param defaultedAssocType The specific associated type declaration that
  /// defines the given default type.
  ///
  /// \note This need not lead to the resolution of a type witness.
  void addDefaultTypeWitness(Type type, AssociatedTypeDecl *defaultedAssocType);

  /// Record the given same-type requirement, if regarded of interest to
  /// the system.
  ///
  /// \note This need not lead to the resolution of a type witness.
  void addSameTypeRequirement(const Requirement &req);

  void dump(llvm::raw_ostream &out,
            const NormalProtocolConformance *conformance) const;

private:
  /// Form an equivalence between the given name variables.
  void addEquivalence(Identifier name1, Identifier name2);

  /// Merge \p equivClass2 into \p equivClass1.
  ///
  /// \note This will delete \p equivClass2 after migrating its members to
  /// \p equivClass1.
  void mergeEquivalenceClasses(EquivalenceClass *equivClass1,
                               const EquivalenceClass *equivClass2);

  /// The result of comparing two resolved types targeting a single equivalence
  /// class, in terms of their relative impact on solving the system.
  enum class ResolvedTypeComparisonResult {
    /// The first resolved type is a better choice than the second one.
    Better,

    /// The first resolved type is an equivalent or worse choice than the
    /// second one.
    EquivalentOrWorse,

    /// Both resolved types are concrete and mutually exclusive.
    Ambiguity
  };

  /// Compare the given resolved types as targeting a single equivalence class,
  /// in terms of the their relative impact on solving the system.
  static ResolvedTypeComparisonResult compareResolvedTypes(Type ty1, Type ty2);
};

/// Match the given witness to the given requirement.
///
/// \returns the result of performing the match.
RequirementMatch matchWitness(
    DeclContext *dc, ValueDecl *req, ValueDecl *witness,
    llvm::function_ref<
        std::tuple<llvm::Optional<RequirementMatch>, Type, Type>(void)>
        setup,
    llvm::function_ref<llvm::Optional<RequirementMatch>(Type, Type)> matchTypes,
    llvm::function_ref<RequirementMatch(bool, ArrayRef<OptionalAdjustment>)>
        finalize);

RequirementMatch
matchWitness(WitnessChecker::RequirementEnvironmentCache &reqEnvCache,
             ProtocolDecl *proto, RootProtocolConformance *conformance,
             DeclContext *dc, ValueDecl *req, ValueDecl *witness);

/// If the given type is a direct reference to an associated type of
/// the given protocol, return the referenced associated type.
AssociatedTypeDecl *getReferencedAssocTypeOfProtocol(Type type,
                                                     ProtocolDecl *proto);

enum class TypeAdjustment : uint8_t {
  NoescapeToEscaping, NonsendableToSendable
};

/// Perform any necessary adjustments to the inferred associated type to
/// make it suitable for later use.
///
/// \param performed Will be set \c true if this operation performed
/// the adjustment, or \c false if the operation found a type that the
/// adjustment could have applied to but did not actually need to adjust it.
/// Unchanged otherwise.
Type adjustInferredAssociatedType(TypeAdjustment adjustment, Type type,
                                  bool &performed);

/// Find the @objc requirement that are witnessed by the given
/// declaration.
///
/// \param anySingleRequirement If true, returns at most a single requirement,
/// which might be any of the requirements that match.
///
/// \returns the set of requirements to which the given witness is a
/// witness.
llvm::TinyPtrVector<ValueDecl *> findWitnessedObjCRequirements(
                                     const ValueDecl *witness,
                                     bool anySingleRequirement = false);

void diagnoseConformanceFailure(Type T,
                                ProtocolDecl *Proto,
                                DeclContext *DC,
                                SourceLoc ComplainLoc);

Type getTupleConformanceTypeWitness(DeclContext *dc,
                                    AssociatedTypeDecl *assocType);

/// Find an associated type declaration that provides a default definition.
AssociatedTypeDecl *findDefaultedAssociatedType(
    DeclContext *dc, NominalTypeDecl *adoptee,
    AssociatedTypeDecl *assocType);

}

namespace llvm {

template<>
struct DenseMapInfo<swift::ASTContext::MissingWitness> {
  using MissingWitness = swift::ASTContext::MissingWitness;
  using RequirementPointerTraits = DenseMapInfo<swift::ValueDecl *>;

  static inline MissingWitness getEmptyKey() {
    return MissingWitness(RequirementPointerTraits::getEmptyKey(), {});
  }
  static inline MissingWitness getTombstoneKey() {
    return MissingWitness(RequirementPointerTraits::getTombstoneKey(), {});
  }
  static inline unsigned getHashValue(MissingWitness missing) {
    return RequirementPointerTraits::getHashValue(missing.requirement);
  }
  static bool isEqual(MissingWitness a, MissingWitness b) {
    return a.requirement == b.requirement;
  }
};

}
#endif // SWIFT_SEMA_PROTOCOL_H
