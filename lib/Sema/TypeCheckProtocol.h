//===--- ConstraintSystem.h - Constraint-based Type Checking ----*- C++ -*-===//
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

#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class AccessScope;
class AssociatedTypeDecl;
class AvailabilityContext;
class DeclContext;
class NormalProtocolConformance;
class ProtocolDecl;
class TypeChecker;
class ValueDecl;

/// A conflict between two inferred type witnesses for the same
/// associated type.
struct TypeWitnessConflict {
  /// The associated type.
  AssociatedTypeDecl *AssocType;

  /// The first type.
  Type FirstType;

  /// The requirement to which the first witness was matched.
  ValueDecl *FirstRequirement;

  /// The witness from which the first type witness was inferred.
  ValueDecl *FirstWitness;

  /// The second type.
  Type SecondType;

  /// The requirement to which the second witness was matched.
  ValueDecl *SecondRequirement;

  /// The witness from which the second type witness was inferred.
  ValueDecl *SecondWitness;
};

/// Describes the result of checking a type witness.
///
/// This class evaluates true if an error occurred.
class CheckTypeWitnessResult {
  Type Requirement;

public:
  CheckTypeWitnessResult() { }
  CheckTypeWitnessResult(Type reqt) : Requirement(reqt) {}

  Type getRequirement() const { return Requirement; }
  bool isConformanceRequirement() const {
    return Requirement->isExistentialType();
  }

  explicit operator bool() const { return !Requirement.isNull(); }
};

/// Check whether the given type witness can be used for the given
/// associated type.
///
/// \returns an empty result on success, or a description of the error.
CheckTypeWitnessResult checkTypeWitness(TypeChecker &tc, DeclContext *dc,
                                        ProtocolDecl *proto,
                                        AssociatedTypeDecl *assocType,
                                        Type type);

/// The set of associated types that have been inferred by matching
/// the given value witness to its corresponding requirement.
struct InferredAssociatedTypesByWitness {
  /// The witness we matched.
  ValueDecl *Witness = nullptr;

  /// The associated types inferred from matching this witness.
  SmallVector<std::pair<AssociatedTypeDecl *, Type>, 4> Inferred;

  /// Inferred associated types that don't meet the associated type
  /// requirements.
  SmallVector<std::tuple<AssociatedTypeDecl *, Type, CheckTypeWitnessResult>,
              2> NonViable;

  void dump(llvm::raw_ostream &out, unsigned indent) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const,
                            "only for use in the debugger");
};

/// The set of witnesses that were considered when attempting to
/// infer associated types.
typedef SmallVector<InferredAssociatedTypesByWitness, 2>
  InferredAssociatedTypesByWitnesses;

/// A mapping from requirements to the set of matches with witnesses.
typedef SmallVector<std::pair<ValueDecl *,
                              InferredAssociatedTypesByWitnesses>, 4>
  InferredAssociatedTypes;

/// A potential solution to the set of inferred type witnesses.
struct InferredTypeWitnessesSolution {
  /// The set of type witnesses inferred by this solution, along
  /// with the index into the value witnesses where the type was
  /// inferred.
  llvm::SmallDenseMap<AssociatedTypeDecl *, std::pair<Type, unsigned>, 4>
    TypeWitnesses;

  /// The value witnesses selected by this step of the solution.
  SmallVector<std::pair<ValueDecl *, ValueDecl *>, 4> ValueWitnesses;

  /// The number of value witnesses that occur in protocol
  /// extensions.
  unsigned NumValueWitnessesInProtocolExtensions;

#ifndef NDEBUG
  LLVM_ATTRIBUTE_USED
#endif
  void dump();
};

struct RequirementMatch;
struct RequirementCheck;

class WitnessChecker {
protected:
  TypeChecker &TC;
  ProtocolDecl *Proto;
  Type Adoptee;
  // The conforming context, either a nominal type or extension.
  DeclContext *DC;

  // An auxiliary lookup table to be used for witnesses remapped via
  // @_implements(Protocol, DeclName)
  llvm::DenseMap<DeclName, llvm::TinyPtrVector<ValueDecl *>> ImplementsTable;

  WitnessChecker(TypeChecker &tc, ProtocolDecl *proto,
                 Type adoptee, DeclContext *dc);

  /// Gather the value witnesses for the given requirement.
  ///
  /// \param ignoringNames If non-null and there are no value
  /// witnesses with the correct full name, the results will reflect
  /// lookup for just the base name and the pointee will be set to
  /// \c true.
  SmallVector<ValueDecl *, 4> lookupValueWitnesses(ValueDecl *req,
                                                   bool *ignoringNames);

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

  bool checkWitnessAccess(AccessScope &requiredAccessScope,
                          ValueDecl *requirement,
                          ValueDecl *witness,
                          bool *isSetter);

  bool checkWitnessAvailability(ValueDecl *requirement,
                                ValueDecl *witness,
                                AvailabilityContext *requirementInfo);

  RequirementCheck checkWitness(AccessScope requiredAccessScope,
                                ValueDecl *requirement,
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
  friend class MultiConformanceChecker;
  friend class AssociatedTypeInference;

  NormalProtocolConformance *Conformance;
  SourceLoc Loc;

  /// Witnesses that are currently being resolved.
  llvm::SmallPtrSet<ValueDecl *, 4> ResolvingWitnesses;

  /// Caches the set of associated types that are referenced in each
  /// requirement.
  llvm::DenseMap<ValueDecl *, llvm::SmallVector<AssociatedTypeDecl *, 2>>
    ReferencedAssociatedTypes;

  /// Keep track of missing witnesses, either type or value, for later
  /// diagnosis emits. This may contain witnesses that are external to the
  /// protocol under checking.
  llvm::SetVector<ValueDecl*> &GlobalMissingWitnesses;

  /// Keep track of the slice in GlobalMissingWitnesses that is local to
  /// this protocol under checking.
  unsigned LocalMissingWitnessesStartIndex;

  /// True if we shouldn't complain about problems with this conformance
  /// right now, i.e. if methods are being called outside
  /// checkConformance().
  bool SuppressDiagnostics;

  /// Whether we've already complained about problems with this conformance.
  bool AlreadyComplained = false;

  /// Whether we checked the requirement signature already.
  bool CheckedRequirementSignature = false;

  /// Retrieve the associated types that are referenced by the given
  /// requirement with a base of 'Self'.
  ArrayRef<AssociatedTypeDecl *> getReferencedAssociatedTypes(ValueDecl *req);

  /// Record a (non-type) witness for the given requirement.
  void recordWitness(ValueDecl *requirement, const RequirementMatch &match);

  /// Record that the given optional requirement has no witness.
  void recordOptionalWitness(ValueDecl *requirement);

  /// Record that the given requirement has no valid witness.
  void recordInvalidWitness(ValueDecl *requirement);

  /// Record a type witness.
  ///
  /// \param assocType The associated type whose witness is being recorded.
  ///
  /// \param type The witness type.
  ///
  /// \param typeDecl The decl the witness type came from; can be null.
  void recordTypeWitness(AssociatedTypeDecl *assocType, Type type,
                         TypeDecl *typeDecl, bool performRedeclarationCheck);

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

  /// Attempt to resolve a type witness via member name lookup.
  ResolveWitnessResult resolveTypeWitnessViaLookup(
                         AssociatedTypeDecl *assocType);

  /// Infer associated type witnesses for the given tentative
  /// requirement/witness match.
  InferredAssociatedTypesByWitness inferTypeWitnessesViaValueWitness(
                                     ValueDecl *req,
                                     ValueDecl *witness);

  /// Infer associated type witnesses for the given value requirement.
  InferredAssociatedTypesByWitnesses inferTypeWitnessesViaValueWitnesses(
                   const llvm::SetVector<AssociatedTypeDecl *> &allUnresolved,
                   ValueDecl *req);

  /// Infer associated type witnesses for all relevant value requirements.
  ///
  /// \param assocTypes The set of associated types we're interested in.
  InferredAssociatedTypes
  inferTypeWitnessesViaValueWitnesses(
    const llvm::SetVector<AssociatedTypeDecl *> &assocTypes);

  /// Diagnose or defer a diagnostic, as appropriate.
  ///
  /// \param requirement The requirement with which this diagnostic is
  /// associated, if any.
  ///
  /// \param isError Whether this diagnostic is an error.
  ///
  /// \param fn A function to call to emit the actual diagnostic. If
  /// diagnostics are being deferred,
  void diagnoseOrDefer(
         ValueDecl *requirement, bool isError,
         std::function<void(NormalProtocolConformance *)> fn);

  void
  addUsedConformances(ProtocolConformance *conformance,
                      llvm::SmallPtrSetImpl<ProtocolConformance *> &visited);
  void addUsedConformances(ProtocolConformance *conformance);

  ArrayRef<ValueDecl*> getLocalMissingWitness() {
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
  void diagnoseMissingWitnesses(MissingWitnessDiagnosisKind Kind);
  /// Emit any diagnostics that have been delayed.
  void emitDelayedDiags();

  ConformanceChecker(TypeChecker &tc, NormalProtocolConformance *conformance,
                     llvm::SetVector<ValueDecl*> &GlobalMissingWitnesses,
                     bool suppressDiagnostics = true);

  /// Resolve all of the type witnesses.
  void resolveTypeWitnesses();

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

  /// Check all of the protocols requirements are actually satisfied by a
  /// the chosen type witnesses.
  void ensureRequirementsAreSatisfied();

  /// Check the entire protocol conformance, ensuring that all
  /// witnesses are resolved and emitting any diagnostics.
  void checkConformance(MissingWitnessDiagnosisKind Kind);
};
/// Captures the state needed to infer associated types.
class AssociatedTypeInference {
  /// The type checker we'll need to validate declarations etc.
  TypeChecker &tc;

  /// The conformance for which we are inferring associated types.
  NormalProtocolConformance *conformance;

  /// The protocol for which we are inferring associated types.
  ProtocolDecl *proto;

  /// The declaration context in which conformance to the protocol is
  /// declared.
  DeclContext *dc;

  /// The type that is adopting the protocol.
  Type adoptee;

  /// The set of type witnesses inferred from value witnesses.
  InferredAssociatedTypes inferred;

  /// Hash table containing the type witnesses that we've inferred for
  /// each associated type, as well as an indication of how we inferred them.
  llvm::ScopedHashTable<AssociatedTypeDecl *, std::pair<Type, unsigned>>
    typeWitnesses;

  /// Information about a failed, defaulted associated type.
  AssociatedTypeDecl *failedDefaultedAssocType = nullptr;
  Type failedDefaultedWitness;
  CheckTypeWitnessResult failedDefaultedResult;

  /// Information about a failed, derived associated type.
  AssociatedTypeDecl *failedDerivedAssocType = nullptr;
  Type failedDerivedWitness;

  // Which type witness was missing?
  AssociatedTypeDecl *missingTypeWitness = nullptr;

  // Was there a conflict in type witness deduction?
  Optional<TypeWitnessConflict> typeWitnessConflict;
  unsigned numTypeWitnessesBeforeConflict = 0;

public:
  AssociatedTypeInference(TypeChecker &tc,
                          NormalProtocolConformance *conformance);

private:
  /// Compute the default type witness from an associated type default,
  /// if there is one.
  Type computeDefaultTypeWitness(AssociatedTypeDecl *assocType);

  /// Compute the "derived" type witness for an associated type that is
  /// known to the compiler.
  Type computeDerivedTypeWitness(AssociatedTypeDecl *assocType);

  /// Substitute the current type witnesses into the given interface type.
  Type substCurrentTypeWitnesses(Type type);

  /// Check whether the current set of type witnesses meets the
  /// requirements of the protocol.
  bool checkCurrentTypeWitnesses();

  /// Top-level operation to find solutions for the given unresolved
  /// associated types.
  void findSolutions(
                 ArrayRef<AssociatedTypeDecl *> unresolvedAssocTypes,
                 SmallVectorImpl<InferredTypeWitnessesSolution> &solutions);

  /// Explore the solution space to find both viable and non-viable solutions.
  void findSolutionsRec(
         ArrayRef<AssociatedTypeDecl *> unresolvedAssocTypes,
         SmallVectorImpl<InferredTypeWitnessesSolution> &solutions,
         SmallVectorImpl<InferredTypeWitnessesSolution> &nonViableSolutions,
         SmallVector<std::pair<ValueDecl *, ValueDecl *>, 4> &valueWitnesses,
         unsigned numTypeWitnesses,
         unsigned numValueWitnessesInProtocolExtensions,
         unsigned reqDepth);

  /// Determine whether the first solution is better than the second
  /// solution.
  bool isBetterSolution(const InferredTypeWitnessesSolution &first,
                        const InferredTypeWitnessesSolution &second);

  /// Find the best solution.
  ///
  /// \param solutions All of the solutions to consider. On success,
  /// this will contain only the best solution.
  ///
  /// \returns \c false if there was a single best solution,
  /// \c true if no single best solution exists.
  bool findBestSolution(
                SmallVectorImpl<InferredTypeWitnessesSolution> &solutions);

  /// Emit a diagnostic for the case where there are no solutions at all
  /// to consider.
  ///
  /// \returns true if a diagnostic was emitted, false otherwise.
  bool diagnoseNoSolutions(
                     ArrayRef<AssociatedTypeDecl *> unresolvedAssocTypes,
                     ConformanceChecker &checker);

  /// Emit a diagnostic when there are multiple solutions.
  ///
  /// \returns true if a diagnostic was emitted, false otherwise.
  bool diagnoseAmbiguousSolutions(
                ArrayRef<AssociatedTypeDecl *> unresolvedAssocTypes,
                ConformanceChecker &checker,
                SmallVectorImpl<InferredTypeWitnessesSolution> &solutions);

public:
  /// Describes a mapping from associated type declarations to their
  /// type witnesses (as interface types).
  typedef std::vector<std::pair<AssociatedTypeDecl *, Type>>
    InferredTypeWitnesses;

  /// Perform associated type inference.
  ///
  /// \returns \c true if an error occurred, \c false otherwise
  Optional<InferredTypeWitnesses> solve(ConformanceChecker &checker);
};

}

#endif // SWIFT_SEMA_PROTOCOL_H
