//===--- GenericSignatureBuilder.h - Generic signature builder --*- C++ -*-===//
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
// Support for collecting a set of generic requirements, whether they are
// explicitly stated, inferred from a type signature, or implied by other
// requirements, and computing the canonicalized, minimized generic signature
// from those requirements.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_GENERICSIGNATUREBUILDER_H
#define SWIFT_GENERICSIGNATUREBUILDER_H

#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TrailingObjects.h"
#include <functional>
#include <memory>

namespace swift {

class DeclContext;
class DependentMemberType;
class GenericParamList;
class GenericSignature;
class GenericSignatureBuilder;
class GenericTypeParamType;
class LazyResolver;
class ModuleDecl;
class Pattern;
class ProtocolConformance;
class Requirement;
class RequirementRepr;
class SILModule;
class SourceLoc;
class SubstitutionMap;
class Type;
class TypeRepr;
class ASTContext;
class DiagnosticEngine;

/// Determines how to resolve a dependent type to a potential archetype.
enum class ArchetypeResolutionKind {
  /// Always create a new potential archetype to describe this dependent type,
  /// which might be invalid and may not provide complete information.
  AlwaysPartial,

  /// Only create a potential archetype when it is well-formed (e.g., a nested
  /// type should exist) and make sure we have complete information about
  /// that potential archetype.
  CompleteWellFormed,

  /// Only create a new potential archetype to describe this dependent type
  /// if it is already known.
  AlreadyKnown,
};

/// \brief Collects a set of requirements of generic parameters, both explicitly
/// stated and inferred, and determines the set of archetypes for each of
/// the generic parameters.
class GenericSignatureBuilder {
public:
  /// Describes a potential archetype, which stands in for a generic parameter
  /// type or some type derived from it.
  class PotentialArchetype;

  using UnresolvedType = llvm::PointerUnion<PotentialArchetype *, Type>;
  struct ResolvedType;

  using RequirementRHS =
      llvm::PointerUnion3<Type, PotentialArchetype *, LayoutConstraint>;

  /// The location of a requirement as written somewhere in the source.
  typedef llvm::PointerUnion<const TypeRepr *, const RequirementRepr *>
    WrittenRequirementLoc;

  class RequirementSource;

  class FloatingRequirementSource;

  class DelayedRequirement;

  /// Describes a specific constraint on a potential archetype.
  template<typename T>
  struct Constraint {
    PotentialArchetype *archetype;
    T value;
    const RequirementSource *source;
  };

  /// Describes a concrete constraint on a potential archetype where, where the
  /// other parameter is a concrete type.
  typedef Constraint<Type> ConcreteConstraint;

  /// Describes an equivalence class of potential archetypes.
  struct EquivalenceClass {
    /// The list of protocols to which this equivalence class conforms.
    ///
    /// The keys form the (semantic) list of protocols to which this type
    /// conforms. The values are the conformance constraints as written on
    /// this equivalence class.
    llvm::MapVector<ProtocolDecl *, std::vector<Constraint<ProtocolDecl *>>>
      conformsTo;

    /// Same-type constraints between each potential archetype and any other
    /// archetype in its equivalence class.
    llvm::MapVector<PotentialArchetype *,
                    std::vector<Constraint<PotentialArchetype *>>>
      sameTypeConstraints;

    /// Concrete type to which this equivalence class is equal.
    ///
    /// This is the semantic concrete type; the constraints as written
    /// (or implied) are stored in \c concreteTypeConstraints;
    Type concreteType;

    /// The same-type-to-concrete constraints written within this
    /// equivalence class.
    std::vector<ConcreteConstraint> concreteTypeConstraints;

    /// Superclass constraint, which requires that the type fulfilling the
    /// requirements of this equivalence class to be the same as or a subtype
    /// of this superclass.
    Type superclass;

    /// Superclass constraints written within this equivalence class.
    std::vector<ConcreteConstraint> superclassConstraints;

    /// \The layout constraint for this equivalence class.
    LayoutConstraint layout;

    /// Layout constraints written within this equivalence class.
    std::vector<Constraint<LayoutConstraint>> layoutConstraints;

    /// The members of the equivalence class.
    TinyPtrVector<PotentialArchetype *> members;

    /// Describes a component within the graph of same-type constraints within
    /// the equivalence class that is held together by derived constraints.
    struct DerivedSameTypeComponent {
      /// The potential archetype that acts as the anchor for this component.
      PotentialArchetype *anchor;

      /// The (best) requirement source within the component that makes the
      /// potential archetypes in this component equivalent to the concrete
      /// type.
      const RequirementSource *concreteTypeSource;
    };

    /// The set of connected components within this equivalence class, using
    /// only the derived same-type constraints in the graph.
    std::vector<DerivedSameTypeComponent> derivedSameTypeComponents;

    /// Whether we have detected recursion during the substitution of
    /// the concrete type.
    unsigned recursiveConcreteType : 1;

    /// Whether we have detected recursion during the substitution of
    /// the superclass type.
    unsigned recursiveSuperclassType : 1;

    /// Construct a new equivalence class containing only the given
    /// potential archetype (which represents itself).
    EquivalenceClass(PotentialArchetype *representative);

    /// Find a source of the same-type constraint that maps a potential
    /// archetype in this equivalence class to a concrete type along with
    /// that concrete type as written.
    Optional<ConcreteConstraint>
    findAnyConcreteConstraintAsWritten(
                              PotentialArchetype *preferredPA = nullptr) const;

    /// Find a source of the superclass constraint in this equivalence class
    /// that has a type equivalence to \c superclass, along with that
    /// superclass type as written.
    Optional<ConcreteConstraint>
    findAnySuperclassConstraintAsWritten(
                              PotentialArchetype *preferredPA = nullptr) const;

    /// Determine whether conformance to the given protocol is satisfied by
    /// a superclass requirement.
    bool isConformanceSatisfiedBySuperclass(ProtocolDecl *proto) const;

    /// Dump a debugging representation of this equivalence class.
    void dump(llvm::raw_ostream &out) const;

    LLVM_ATTRIBUTE_DEPRECATED(void dump() const,
                              "only for use in the debugger");

    /// Caches.

    /// The cached archetype anchor.
    struct {
      /// The cached archetype anchor itself.
      PotentialArchetype *anchor = nullptr;

      /// The number of members of the equivalence class when the archetype
      /// anchor was cached.
      unsigned numMembers;
    } archetypeAnchorCache;
  };

  friend class RequirementSource;

  /// The result of introducing a new constraint.
  enum class ConstraintResult {
    /// The constraint was resolved and the relative potential archetypes
    /// have been updated.
    Resolved,

    /// The constraint was written directly on a concrete type.
    Concrete,

    /// The constraint conflicted with existing constraints in some way;
    /// the generic signature is ill-formed.
    Conflicting,

    /// The constraint could not be resolved immediately.
    Unresolved,
  };

  /// Enum used to indicate how we should handle a constraint that cannot be
  /// processed immediately for some reason.
  enum class UnresolvedHandlingKind : char {
    /// Generate a new, unresolved constraint and consider the constraint
    /// "resolved" at this point.
    GenerateConstraints = 0,

    /// Do not generate a new constraint; rather, return
    /// \c ConstraintResult::Unresolved and let the caller handle it.
    ReturnUnresolved = 1,
  };

private:
  class InferRequirementsWalker;
  friend class InferRequirementsWalker;
  friend class GenericSignature;

  ASTContext &Context;
  DiagnosticEngine &Diags;
  struct Implementation;
  std::unique_ptr<Implementation> Impl;

  GenericSignatureBuilder(const GenericSignatureBuilder &) = delete;
  GenericSignatureBuilder &operator=(const GenericSignatureBuilder &) = delete;

  /// Record that the given potential archetype is unresolved, so we know to
  /// resolve it later.
  void recordUnresolvedType(PotentialArchetype *unresolvedPA);

  /// When a particular requirement cannot be resolved due to, e.g., a
  /// currently-unresolvable or nested type, this routine should be
  /// called to cope with the unresolved requirement.
  ///
  /// \returns \c ConstraintResult::Resolved or ConstraintResult::Delayed,
  /// as appropriate based on \c unresolvedHandling.
  ConstraintResult handleUnresolvedRequirement(RequirementKind kind,
                                   UnresolvedType lhs,
                                   RequirementRHS rhs,
                                   FloatingRequirementSource source,
                                   UnresolvedHandlingKind unresolvedHandling);

  /// Retrieve the constraint source conformance for the superclass constraint
  /// of the given potential archetype (if present) to the given protocol.
  ///
  /// \param pa The potential archetype whose superclass constraint is being
  /// queried.
  ///
  /// \param proto The protocol to which we are establishing conformance.
  const RequirementSource *resolveSuperConformance(
                            GenericSignatureBuilder::PotentialArchetype *pa,
                            ProtocolDecl *proto);

  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype conforms to the given protocol.
  ConstraintResult addConformanceRequirement(PotentialArchetype *T,
                                             ProtocolDecl *Proto,
                                             const RequirementSource *Source);

  /// Try to resolve the given unresolved potential archetype.
  ConstraintResult resolveUnresolvedType(PotentialArchetype *pa);

public:
  /// \brief Add a new same-type requirement between two fully resolved types
  /// (output of \c GenericSignatureBuilder::resolve).
  ///
  /// If the types refer to two concrete types that are fundamentally
  /// incompatible (e.g. \c Foo<Bar<T>> and \c Foo<Baz>), \c diagnoseMismatch is
  /// called with the two types that don't match (\c Bar<T> and \c Baz for the
  /// previous example).
  ConstraintResult
  addSameTypeRequirementDirect(
                         ResolvedType paOrT1, ResolvedType paOrT2,
                         FloatingRequirementSource Source,
                         llvm::function_ref<void(Type, Type)> diagnoseMismatch);

  /// \brief Add a new same-type requirement between two fully resolved types
  /// (output of GenericSignatureBuilder::resolve).
  ///
  /// The two types must not be incompatible concrete types.
  ConstraintResult addSameTypeRequirementDirect(
                                            ResolvedType paOrT1,
                                            ResolvedType paOrT2,
                                            FloatingRequirementSource Source);

  /// \brief Add a new same-type requirement between two unresolved types.
  ///
  /// The types are resolved with \c GenericSignatureBuilder::resolve, and must
  /// not be incompatible concrete types.
  ConstraintResult addSameTypeRequirement(
                                    UnresolvedType paOrT1,
                                    UnresolvedType paOrT2,
                                    FloatingRequirementSource Source,
                                    UnresolvedHandlingKind unresolvedHandling);

  /// \brief Add a new same-type requirement between two unresolved types.
  ///
  /// The types are resolved with \c GenericSignatureBuilder::resolve. \c
  /// diagnoseMismatch is called if the two types refer to incompatible concrete
  /// types.
  ConstraintResult
  addSameTypeRequirement(UnresolvedType paOrT1, UnresolvedType paOrT2,
                         FloatingRequirementSource Source,
                         UnresolvedHandlingKind unresolvedHandling,
                         llvm::function_ref<void(Type, Type)> diagnoseMismatch);

  /// Update the superclass for the equivalence class of \c T.
  ///
  /// This assumes that the constraint has already been recorded.
  void updateSuperclass(PotentialArchetype *T,
                        Type superclass,
                        const RequirementSource *source);

private:
  /// \brief Add a new superclass requirement specifying that the given
  /// potential archetype has the given type as an ancestor.
  ConstraintResult addSuperclassRequirementDirect(
                                              PotentialArchetype *T,
                                              Type Superclass,
                                              const RequirementSource *Source);

  /// \brief Add a new type requirement specifying that the given
  /// type conforms-to or is a superclass of the second type.
  ConstraintResult addTypeRequirement(
                          UnresolvedType subject,
                          UnresolvedType constraint,
                          FloatingRequirementSource source,
                          UnresolvedHandlingKind unresolvedHandling);

  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetypes are equivalent.
  ConstraintResult addSameTypeRequirementBetweenArchetypes(
                                               PotentialArchetype *T1,
                                               PotentialArchetype *T2,
                                               const RequirementSource *Source);
  
  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype is bound to a concrete type.
  ConstraintResult addSameTypeRequirementToConcrete(PotentialArchetype *T,
                                        Type Concrete,
                                        const RequirementSource *Source);

  /// \brief Add a new same-type requirement specifying that the given two
  /// types should be the same.
  ///
  /// \param diagnoseMismatch Callback invoked when the types in the same-type
  /// requirement mismatch.
  ConstraintResult addSameTypeRequirementBetweenConcrete(
      Type T1, Type T2, FloatingRequirementSource Source,
      llvm::function_ref<void(Type, Type)> diagnoseMismatch);

  /// \brief Add a new layout requirement directly on the potential archetype.
  ///
  /// \returns true if this requirement makes the set of requirements
  /// inconsistent, in which case a diagnostic will have been issued.
  ConstraintResult addLayoutRequirementDirect(PotentialArchetype *PAT,
                                              LayoutConstraint Layout,
                                              const RequirementSource *Source);

  /// Add a new layout requirement to the subject.
  ConstraintResult addLayoutRequirement(
                                    UnresolvedType subject,
                                    LayoutConstraint layout,
                                    FloatingRequirementSource source,
                                    UnresolvedHandlingKind unresolvedHandling);

  /// Add the requirements placed on the given type parameter
  /// to the given potential archetype.
  ///
  /// \param inferForModule Infer additional requirements from the types
  /// relative to the given module.
  ConstraintResult addInheritedRequirements(
                                TypeDecl *decl,
                                UnresolvedType type,
                                const RequirementSource *parentSource,
                                ModuleDecl *inferForModule);

  /// Visit all of the potential archetypes.
  template<typename F>
  void visitPotentialArchetypes(F f);

  void markPotentialArchetypeRecursive(PotentialArchetype *pa,
                                       ProtocolDecl *proto,
                                       const RequirementSource *source);

public:
  /// Construct a new generic signature builder.
  ///
  /// \param lookupConformance Conformance-lookup routine that will be used
  /// to satisfy conformance requirements for concrete types.
  explicit GenericSignatureBuilder(ASTContext &ctx,
                            std::function<GenericFunction> lookupConformance);

  GenericSignatureBuilder(GenericSignatureBuilder &&);
  ~GenericSignatureBuilder();

  /// Retrieve the AST context.
  ASTContext &getASTContext() const { return Context; }

  /// Retrieve the conformance-lookup function used by this generic signature builder.
  std::function<GenericFunction> getLookupConformanceFn() const;

  /// Retrieve the lazy resolver, if there is one.
  LazyResolver *getLazyResolver() const;

  /// Enumerate the requirements that describe the signature of this
  /// generic signature builder.
  ///
  /// \param f A function object that will be passed each requirement
  /// and requirement source.
  void enumerateRequirements(llvm::function_ref<
                      void (RequirementKind kind,
                            PotentialArchetype *archetype,
                            RequirementRHS constraint,
                            const RequirementSource *source)> f);

public:
  /// \brief Add a new generic parameter for which there may be requirements.
  void addGenericParameter(GenericTypeParamDecl *GenericParam);

  /// Add the requirements placed on the given abstract type parameter
  /// to the given potential archetype.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool addGenericParameterRequirements(GenericTypeParamDecl *GenericParam);

  /// \brief Add a new generic parameter for which there may be requirements.
  void addGenericParameter(GenericTypeParamType *GenericParam);
  
  /// \brief Add a new requirement.
  ///
  /// \param inferForModule Infer additional requirements from the types
  /// relative to the given module.
  ///
  /// \returns true if this requirement makes the set of requirements
  /// inconsistent, in which case a diagnostic will have been issued.
  ConstraintResult addRequirement(const RequirementRepr *req,
                                  ModuleDecl *inferForModule);

  /// \brief Add a new requirement.
  ///
  /// \param inferForModule Infer additional requirements from the types
  /// relative to the given module.
  ///
  /// \returns true if this requirement makes the set of requirements
  /// inconsistent, in which case a diagnostic will have been issued.
  ConstraintResult addRequirement(const RequirementRepr *Req,
                                  FloatingRequirementSource source,
                                  const SubstitutionMap *subMap,
                                  ModuleDecl *inferForModule);

  /// \brief Add an already-checked requirement.
  ///
  /// Adding an already-checked requirement cannot fail. This is used to
  /// re-inject requirements from outer contexts.
  ///
  /// \param inferForModule Infer additional requirements from the types
  /// relative to the given module.
  ///
  /// \returns true if this requirement makes the set of requirements
  /// inconsistent, in which case a diagnostic will have been issued.
  ConstraintResult addRequirement(const Requirement &req,
                                  FloatingRequirementSource source,
                                  ModuleDecl *inferForModule,
                                  const SubstitutionMap *subMap = nullptr);

  /// \brief Add all of a generic signature's parameters and requirements.
  void addGenericSignature(GenericSignature *sig);

  /// \brief Build the generic signature.
  GenericSignature *getGenericSignature();

  /// Infer requirements from the given type, recursively.
  ///
  /// This routine infers requirements from a type that occurs within the
  /// signature of a generic function. For example, given:
  ///
  /// \code
  /// func f<K, V>(dict : Dictionary<K, V>) { ... }
  /// \endcode
  ///
  /// where \c Dictionary requires that its key type be \c Hashable,
  /// the requirement \c K : Hashable is inferred from the parameter type,
  /// because the type \c Dictionary<K,V> cannot be formed without it.
  void inferRequirements(ModuleDecl &module, TypeLoc type,
                         FloatingRequirementSource source);

  /// Infer requirements from the given pattern, recursively.
  ///
  /// This routine infers requirements from a type that occurs within the
  /// signature of a generic function. For example, given:
  ///
  /// \code
  /// func f<K, V>(dict : Dictionary<K, V>) { ... }
  /// \endcode
  ///
  /// where \c Dictionary requires that its key type be \c Hashable,
  /// the requirement \c K : Hashable is inferred from the parameter type,
  /// because the type \c Dictionary<K,V> cannot be formed without it.
  void inferRequirements(ModuleDecl &module, ParameterList *params,
                         GenericParamList *genericParams);

  /// \brief Finalize the set of requirements and compute the generic
  /// signature.
  ///
  /// After this point, one cannot introduce new requirements.
  GenericSignature *computeGenericSignature(
                      SourceLoc loc,
                      bool allowConcreteGenericParams = false);

  /// Finalize the set of requirements, performing any remaining checking
  /// required before generating archetypes.
  ///
  /// \param allowConcreteGenericParams If true, allow generic parameters to
  /// be made concrete.
  void finalize(SourceLoc loc,
                ArrayRef<GenericTypeParamType *> genericParams,
                bool allowConcreteGenericParams=false);

  /// Diagnose any remaining renames.
  ///
  /// \returns \c true if there were any remaining renames to diagnose.
  bool diagnoseRemainingRenames(SourceLoc loc,
                                ArrayRef<GenericTypeParamType *> genericParams);

  /// Process any delayed requirements that can be handled now.
  void processDelayedRequirements();

private:
  /// Describes the relationship between a given constraint and
  /// the canonical constraint of the equivalence class.
  enum class ConstraintRelation {
    /// The constraint is unrelated.
    ///
    /// This is a conservative result that can be used when, for example,
    /// we have incomplete information to make a determination.
    Unrelated,
    /// The constraint is redundant and can be removed without affecting the
    /// semantics.
    Redundant,
    /// The constraint conflicts, meaning that the signature is erroneous.
    Conflicting,
  };

  /// Check a list of constraints, removing self-derived constraints
  /// and diagnosing redundant constraints.
  ///
  /// \param isSuitableRepresentative Determines whether the given constraint
  /// is a suitable representative.
  ///
  /// \param checkConstraint Checks the given constraint against the
  /// canonical constraint to determine which diagnostics (if any) should be
  /// emitted.
  ///
  /// \returns the representative constraint.
  template<typename T>
  Constraint<T> checkConstraintList(
                           ArrayRef<GenericTypeParamType *> genericParams,
                           std::vector<Constraint<T>> &constraints,
                           llvm::function_ref<bool(const Constraint<T> &)>
                             isSuitableRepresentative,
                           llvm::function_ref<ConstraintRelation(const T&)>
                             checkConstraint,
                           Optional<Diag<unsigned, Type, T, T>>
                             conflictingDiag,
                           Diag<Type, T> redundancyDiag,
                           Diag<unsigned, Type, T> otherNoteDiag);

  /// Check a list of constraints, removing self-derived constraints
  /// and diagnosing redundant constraints.
  ///
  /// \param isSuitableRepresentative Determines whether the given constraint
  /// is a suitable representative.
  ///
  /// \param checkConstraint Checks the given constraint against the
  /// canonical constraint to determine which diagnostics (if any) should be
  /// emitted.
  ///
  /// \returns the representative constraint.
  template<typename T, typename DiagT>
  Constraint<T> checkConstraintList(
                           ArrayRef<GenericTypeParamType *> genericParams,
                           std::vector<Constraint<T>> &constraints,
                           llvm::function_ref<bool(const Constraint<T> &)>
                             isSuitableRepresentative,
                           llvm::function_ref<ConstraintRelation(const T&)>
                             checkConstraint,
                           Optional<Diag<unsigned, Type, DiagT, DiagT>>
                             conflictingDiag,
                           Diag<Type, DiagT> redundancyDiag,
                           Diag<unsigned, Type, DiagT> otherNoteDiag,
                           llvm::function_ref<DiagT(const T&)> diagValue,
                           bool removeSelfDerived);

  /// Check the concrete type constraints within the equivalence
  /// class of the given potential archetype.
  void checkConcreteTypeConstraints(
                            ArrayRef<GenericTypeParamType *> genericParams,
                            PotentialArchetype *pa);

  /// Check the superclass constraints within the equivalence
  /// class of the given potential archetype.
  void checkSuperclassConstraints(
                            ArrayRef<GenericTypeParamType *> genericParams,
                            PotentialArchetype *pa);

  /// Check conformance constraints within the equivalence class of the
  /// given potential archetype.
  void checkConformanceConstraints(
                            ArrayRef<GenericTypeParamType *> genericParams,
                            PotentialArchetype *pa);

  /// Check layout constraints within the equivalence class of the given
  /// potential archetype.
  void checkLayoutConstraints(ArrayRef<GenericTypeParamType *> genericParams,
                              PotentialArchetype *pa);

  /// Check same-type constraints within the equivalence class of the
  /// given potential archetype.
  void checkSameTypeConstraints(
                            ArrayRef<GenericTypeParamType *> genericParams,
                            PotentialArchetype *pa);

public:
  /// \brief Resolve the given type to the potential archetype it names.
  ///
  /// The \c resolutionKind parameter describes how resolution should be
  /// performed. If the potential archetype named by the given dependent type
  /// already exists, it will be always returned. If it doesn't exist yet,
  /// the \c resolutionKind dictates whether the potential archetype will
  /// be created or whether null will be returned.
  ///
  /// For any type that cannot refer to an archetype, this routine returns null.
  PotentialArchetype *resolveArchetype(Type type,
                                       ArchetypeResolutionKind resolutionKind);

  /// \brief Resolve the given type as far as this Builder knows how.
  ///
  /// If successful, this returns either a non-typealias potential archetype
  /// or a Type, if \c type is concrete.
  /// If the type cannot be resolved, e.g., because it is "too" recursive
  /// given the source, returns \c None.
  Optional<ResolvedType> resolve(UnresolvedType type,
                                 FloatingRequirementSource source);

  /// \brief Dump all of the requirements, both specified and inferred.
  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(),
      "only for use within the debugger");

  /// Dump all of the requirements to the given output stream.
  void dump(llvm::raw_ostream &out);
};

/// Describes how a generic signature determines a requirement, from its origin
/// in some requirement written in the source, inferred through a path of
/// other implications (e.g., introduced by a particular protocol).
///
/// Requirement sources are uniqued within a generic signature builder.
class GenericSignatureBuilder::RequirementSource final
  : public llvm::FoldingSetNode,
    private llvm::TrailingObjects<RequirementSource, ProtocolDecl *,
                                  WrittenRequirementLoc> {

  friend class FloatingRequirementSource;
  friend class GenericSignature;

public:
  enum Kind : uint8_t {
    /// A requirement stated explicitly, e.g., in a where clause or type
    /// parameter declaration.
    ///
    /// Explicitly-stated requirement can be tied to a specific requirement
    /// in a 'where' clause (which stores a \c RequirementRepr), a type in an
    /// 'inheritance' clause (which stores a \c TypeRepr), or can be 'abstract',
    /// , e.g., due to canonicalization, deserialization, or other
    /// source-independent formulation.
    ///
    /// This is a root requirement source.
    Explicit,

    /// A requirement inferred from part of the signature of a declaration,
    /// e.g., the type of a generic function. For example:
    ///
    /// func f<T>(_: Set<T>) { } // infers T: Hashable
    ///
    /// This is a root requirement source, which can be described by a
    /// \c TypeRepr.
    Inferred,

    /// A requirement for the creation of the requirement signature of a
    /// protocol.
    ///
    /// This is a root requirement source, which is described by the protocol
    /// whose requirement signature is being computed.
    RequirementSignatureSelf,

    /// The requirement came from two nested types of the equivalent types whose
    /// names match.
    ///
    /// This is a root requirement source.
    NestedTypeNameMatch,

    /// The requirement is a protocol requirement.
    ///
    /// This stores the protocol that introduced the requirement as well as the
    /// dependent type (relative to that protocol) to which the conformance
    /// appertains.
    ProtocolRequirement,

    /// The requirement is a protocol requirement that is inferred from
    /// some part of the protocol definition.
    ///
    /// This stores the protocol that introduced the requirement as well as the
    /// dependent type (relative to that protocol) to which the conformance
    /// appertains.
    InferredProtocolRequirement,

    /// A requirement that was resolved via a superclass requirement.
    ///
    /// This stores the \c ProtocolConformance* used to resolve the
    /// requirement.
    Superclass,

    /// A requirement that was resolved for a nested type via its parent
    /// type.
    Parent,

    /// A requirement that was resolved for a nested type via a same-type-to-
    /// concrete constraint.
    ///
    /// This stores the \c ProtocolConformance* used to resolve the
    /// requirement.
    Concrete,
  };

  /// The kind of requirement source.
  const Kind kind;

private:
  /// The kind of storage we have.
  enum class StorageKind : uint8_t {
    RootArchetype,
    StoredType,
    ProtocolConformance,
    AssociatedTypeDecl,
  };

  /// The kind of storage we have.
  const StorageKind storageKind;

  /// Whether there is a trailing written requirement location.
  const bool hasTrailingWrittenRequirementLoc;

  /// Whether a protocol requirement came from the requirement signature.
  const bool usesRequirementSignature;

  /// The actual storage, described by \c storageKind.
  union {
    /// The root archetype.
    PotentialArchetype *rootArchetype;

    /// The type to which a requirement applies.
    TypeBase *type;

    /// A protocol conformance used to satisfy the requirement.
    ProtocolConformance *conformance;

    /// An associated type to which a requirement is being applied.
    AssociatedTypeDecl *assocType;
  } storage;

  friend TrailingObjects;

  /// The trailing protocol declaration, if there is one.
  size_t numTrailingObjects(OverloadToken<ProtocolDecl *>) const {
    switch (kind) {
    case RequirementSignatureSelf:
    case ProtocolRequirement:
    case InferredProtocolRequirement:
      return 1;

    case Explicit:
    case Inferred:
    case NestedTypeNameMatch:
    case Superclass:
    case Parent:
    case Concrete:
      return 0;
    }

    llvm_unreachable("Unhandled RequirementSourceKind in switch.");
  }

  /// The trailing written requirement location, if there is one.
  size_t numTrailingObjects(OverloadToken<WrittenRequirementLoc>) const {
    return hasTrailingWrittenRequirementLoc ? 1 : 0;
  }

#ifndef NDEBUG
  /// Determines whether we have been provided with an acceptable storage kind
  /// for the given requirement source kind.
  static bool isAcceptableStorageKind(Kind kind, StorageKind storageKind);
#endif

  /// Retrieve the opaque storage as a single pointer, for use in uniquing.
  const void *getOpaqueStorage1() const;

  /// Retrieve the second opaque storage as a single pointer, for use in
  /// uniquing.
  const void *getOpaqueStorage2() const;

  /// Retrieve the third opaque storage as a single pointer, for use in
  /// uniquing.
  const void *getOpaqueStorage3() const;

  /// Whether this kind of requirement source is a root.
  static bool isRootKind(Kind kind) {
    switch (kind) {
    case Explicit:
    case Inferred:
    case RequirementSignatureSelf:
    case NestedTypeNameMatch:
      return true;

    case ProtocolRequirement:
    case InferredProtocolRequirement:
    case Superclass:
    case Parent:
    case Concrete:
      return false;
    }

    llvm_unreachable("Unhandled RequirementSourceKind in switch.");
  }

public:
  /// The "parent" of this requirement source.
  ///
  /// The chain of parent requirement sources will eventually terminate in a
  /// requirement source with one of the "root" kinds.
  const RequirementSource * const parent;

  RequirementSource(Kind kind, PotentialArchetype *rootArchetype,
                    ProtocolDecl *protocol,
                    WrittenRequirementLoc writtenReqLoc)
    : kind(kind), storageKind(StorageKind::RootArchetype),
      hasTrailingWrittenRequirementLoc(!writtenReqLoc.isNull()),
      usesRequirementSignature(false), parent(nullptr) {
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.rootArchetype = rootArchetype;
    if (kind == RequirementSignatureSelf)
      getTrailingObjects<ProtocolDecl *>()[0] = protocol;
    if (hasTrailingWrittenRequirementLoc)
      getTrailingObjects<WrittenRequirementLoc>()[0] = writtenReqLoc;
  }

  RequirementSource(Kind kind, const RequirementSource *parent,
                    Type type, ProtocolDecl *protocol,
                    WrittenRequirementLoc writtenReqLoc)
    : kind(kind), storageKind(StorageKind::StoredType),
      hasTrailingWrittenRequirementLoc(!writtenReqLoc.isNull()),
      usesRequirementSignature(protocol->isRequirementSignatureComputed()),
      parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.type = type.getPointer();
    if (isProtocolRequirement())
      getTrailingObjects<ProtocolDecl *>()[0] = protocol;
    if (hasTrailingWrittenRequirementLoc)
      getTrailingObjects<WrittenRequirementLoc>()[0] = writtenReqLoc;
  }

  RequirementSource(Kind kind, const RequirementSource *parent,
                    ProtocolConformance *conformance)
    : kind(kind), storageKind(StorageKind::ProtocolConformance),
      hasTrailingWrittenRequirementLoc(false),
      usesRequirementSignature(false), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.conformance = conformance;
  }

  RequirementSource(Kind kind, const RequirementSource *parent,
                    AssociatedTypeDecl *assocType)
    : kind(kind), storageKind(StorageKind::AssociatedTypeDecl),
      hasTrailingWrittenRequirementLoc(false),
      usesRequirementSignature(false), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.assocType = assocType;
  }

public:
  /// Retrieve an abstract requirement source.
  static const RequirementSource *forAbstract(PotentialArchetype *root);

  /// Retrieve a requirement source representing an explicit requirement
  /// stated in an 'inheritance' or 'where' clause.
  static const RequirementSource *forExplicit(PotentialArchetype *root,
                                              WrittenRequirementLoc writtenLoc);

  /// Retrieve a requirement source representing a requirement that is
  /// inferred from some part of a generic declaration's signature, e.g., the
  /// parameter or result type of a generic function.
  static const RequirementSource *forInferred(PotentialArchetype *root,
                                              const TypeRepr *typeRepr);

  /// Retrieve a requirement source representing the requirement signature
  /// computation for a protocol.
  static const RequirementSource *forRequirementSignature(
                                                      PotentialArchetype *root,
                                                      ProtocolDecl *protocol);

  /// Retrieve a requirement source for nested type name matches.
  static const RequirementSource *forNestedTypeNameMatch(
                                     PotentialArchetype *root);

private:
  /// A requirement source that describes that a requirement comes from a
  /// requirement of the given protocol described by the parent.
  const RequirementSource *viaProtocolRequirement(
                             GenericSignatureBuilder &builder,
                             Type dependentType,
                             ProtocolDecl *protocol,
                             bool inferred,
                             WrittenRequirementLoc writtenLoc =
                               WrittenRequirementLoc()) const;

public:
  /// A requirement source that describes that a requirement that is resolved
  /// via a superclass requirement.
  const RequirementSource *viaSuperclass(
                                        GenericSignatureBuilder &builder,
                                        ProtocolConformance *conformance) const;

  /// A requirement source that describes that a requirement that is resolved
  /// via a same-type-to-concrete requirement.
  const RequirementSource *viaConcrete(GenericSignatureBuilder &builder,
                                       ProtocolConformance *conformance) const;

  /// A constraint source that describes that a constraint that is resolved
  /// for a nested type via a constraint on its parent.
  ///
  /// \param assocType the associated type that
  const RequirementSource *viaParent(GenericSignatureBuilder &builder,
                                     AssociatedTypeDecl *assocType) const;

  /// Retrieve the root requirement source.
  const RequirementSource *getRoot() const;

  /// Retrieve the potential archetype at the root.
  PotentialArchetype *getRootPotentialArchetype() const;

  /// Retrieve the potential archetype to which this source refers.
  PotentialArchetype *getAffectedPotentialArchetype() const;

  /// Visit each of the potential archetypes along the path, from the root
  /// potential archetype to each potential archetype named via (e.g.) a
  /// protocol requirement or parent source.
  ///
  /// \param visitor Called with each potential archetype along the path along
  /// with the requirement source that is being applied on top of that
  /// potential archetype. Can return \c true to halt the search.
  ///
  /// \returns nullptr if any call to \c visitor returned true. Otherwise,
  /// returns the potential archetype to which the entire source refers.
  PotentialArchetype *visitPotentialArchetypesAlongPath(
           llvm::function_ref<bool(PotentialArchetype *,
                                   const RequirementSource *)> visitor) const;

  /// Whether this source is a requirement in a protocol.
  bool isProtocolRequirement() const {
    return kind == ProtocolRequirement || kind == InferredProtocolRequirement;
  }

  /// Whether the requirement is inferred or derived from an inferred
  /// requirement.
  bool isInferredRequirement() const;

  /// Classify the kind of this source for diagnostic purposes.
  unsigned classifyDiagKind() const;

  /// Whether the requirement can be derived from something in its path.
  ///
  /// Derived requirements will not be recorded in a minimized generic
  /// signature, because the information can be re-derived by following the
  /// path.
  bool isDerivedRequirement() const;

  /// Determine whether the given derived requirement \c source, when rooted at
  /// the potential archetype \c pa, is actually derived from the same
  /// requirement. Such "self-derived" requirements do not make the original
  /// requirement redundant, because without said original requirement, the
  /// derived requirement ceases to hold.
  bool isSelfDerivedSource(PotentialArchetype *pa,
                           bool &derivedViaConcrete) const;

  /// Determine whether a requirement \c pa: proto, when formed from this
  /// requirement source, is dependent on itself.
  bool isSelfDerivedConformance(PotentialArchetype *pa,
                                ProtocolDecl *proto,
                                bool &derivedViaConcrete) const;

  /// Retrieve a source location that corresponds to the requirement.
  SourceLoc getLoc() const;

  /// Compare two requirement sources to determine which has the more
  /// optimal path.
  ///
  /// \returns -1 if the \c this is better, 1 if the \c other is better, and 0
  /// if they are equivalent in length.
  int compare(const RequirementSource *other) const;

  /// Retrieve the type representation for this requirement, if there is one.
  const TypeRepr *getTypeRepr() const {
    if (!hasTrailingWrittenRequirementLoc) return nullptr;
    return getTrailingObjects<WrittenRequirementLoc>()[0]
             .dyn_cast<const TypeRepr *>();
  }

  /// Retrieve the requirement representation for this requirement, if there is
  /// one.
  const RequirementRepr *getRequirementRepr() const {
    if (!hasTrailingWrittenRequirementLoc) return nullptr;
    return getTrailingObjects<WrittenRequirementLoc>()[0]
             .dyn_cast<const RequirementRepr *>();
  }

  /// Retrieve the type stored in this requirement.
  Type getStoredType() const;

  /// Retrieve the protocol for this requirement, if there is one.
  ProtocolDecl *getProtocolDecl() const;

  /// Retrieve the protocol conformance for this requirement, if there is one.
  ProtocolConformance *getProtocolConformance() const {
    if (storageKind != StorageKind::ProtocolConformance) return nullptr;
    return storage.conformance;
  }

  /// Retrieve the associated type declaration for this requirement, if there
  /// is one.
  AssociatedTypeDecl *getAssociatedType() const {
    if (storageKind != StorageKind::AssociatedTypeDecl) return nullptr;
    return storage.assocType;
  }

  /// Profiling support for \c FoldingSet.
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, kind, parent, getOpaqueStorage1(), getOpaqueStorage2(),
            getOpaqueStorage3());
  }

  /// Profiling support for \c FoldingSet.
  static void Profile(llvm::FoldingSetNodeID &ID, Kind kind,
                      const RequirementSource *parent, const void *storage1,
                      const void *storage2, const void *storage3) {
    ID.AddInteger(kind);
    ID.AddPointer(parent);
    ID.AddPointer(storage1);
    ID.AddPointer(storage2);
    ID.AddPointer(storage3);
  }

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const,
      "only for use within the debugger");

  /// Dump the requirement source.
  void dump(llvm::raw_ostream &out, SourceManager *SrcMgr,
            unsigned indent) const;

  LLVM_ATTRIBUTE_DEPRECATED(
    void print() const,
    "only for use within the debugger");

  /// Print the requirement source (shorter form)
  void print(llvm::raw_ostream &out, SourceManager *SrcMgr) const;
};

/// A requirement source that potentially lacks a root \c PotentialArchetype.
/// The root will be supplied as soon as the appropriate dependent type is
/// resolved.
class GenericSignatureBuilder::FloatingRequirementSource {
  enum Kind {
    /// A fully-resolved requirement source, which does not need a root.
    Resolved,
    /// An explicit requirement source lacking a root.
    Explicit,
    /// An inferred requirement source lacking a root.
    Inferred,
    /// A requirement source augmented by an abstract protocol requirement
    AbstractProtocol,
    /// A requirement source for a nested-type-name match introduced by
    /// the given source.
    NestedTypeNameMatch,
  } kind;

  using Storage =
    llvm::PointerUnion3<const RequirementSource *, const TypeRepr *,
                        const RequirementRepr *>;

  Storage storage;

  // Additional storage for an abstract protocol requirement.
  union {
    struct {
      ProtocolDecl *protocol = nullptr;
      WrittenRequirementLoc written;
      bool inferred = false;
    } protocolReq;

    Identifier nestedName;
  };

  FloatingRequirementSource(Kind kind, Storage storage)
    : kind(kind), storage(storage) { }

public:
  /// Implicit conversion from a resolved requirement source.
  FloatingRequirementSource(const RequirementSource *source)
    : FloatingRequirementSource(Resolved, source) { }

  static FloatingRequirementSource forAbstract() {
    return { Explicit, Storage() };
  }

  static FloatingRequirementSource forExplicit(const TypeRepr *typeRepr) {
    return { Explicit, typeRepr };
  }

  static FloatingRequirementSource forExplicit(
                                     const RequirementRepr *requirementRepr) {
    return { Explicit, requirementRepr };
  }

  static FloatingRequirementSource forInferred(const TypeRepr *typeRepr) {
    return { Inferred, typeRepr };
  }

  static FloatingRequirementSource viaProtocolRequirement(
                                     const RequirementSource *base,
                                     ProtocolDecl *inProtocol,
                                     bool inferred) {
    FloatingRequirementSource result{ AbstractProtocol, base };
    result.protocolReq.protocol = inProtocol;
    result.protocolReq.inferred = inferred;
    return result;
  }

  static FloatingRequirementSource viaProtocolRequirement(
                                     const RequirementSource *base,
                                     ProtocolDecl *inProtocol,
                                     WrittenRequirementLoc written,
                                     bool inferred) {
    FloatingRequirementSource result{ AbstractProtocol, base };
    result.protocolReq.protocol = inProtocol;
    result.protocolReq.written = written;
    result.protocolReq.inferred = inferred;
    return result;
  }

  static FloatingRequirementSource forNestedTypeNameMatch(
                                     const RequirementSource *base,
                                     Identifier nestedName) {
    FloatingRequirementSource result{ NestedTypeNameMatch, base };
    result.nestedName = nestedName;
    return result;
  };

  /// Retrieve the complete requirement source rooted at the given potential
  /// archetype.
  const RequirementSource *getSource(PotentialArchetype *pa) const;

  /// Retrieve the source location for this requirement.
  SourceLoc getLoc() const;

  /// Whether this is an explicitly-stated requirement.
  bool isExplicit() const;

  /// Return the "inferred" version of this source, if it isn't already
  /// inferred.
  FloatingRequirementSource asInferred(const TypeRepr *typeRepr) const;

  /// Whether this requirement source is recursive when composed with
  /// the given type.
  bool isRecursive(Type rootType, GenericSignatureBuilder &builder) const;
};

class GenericSignatureBuilder::PotentialArchetype {
  /// The parent of this potential archetype (for a nested type) or the
  /// generic signature builder in which this root resides.
  llvm::PointerUnion<PotentialArchetype*, GenericSignatureBuilder*> parentOrBuilder;

  /// The identifier describing this particular archetype.
  ///
  /// \c parentOrBuilder determines whether we have a nested type vs. a root,
  /// while `isUnresolvedNestedType` determines whether we have an unresolved
  /// nested type (vs. a resolved one);
  union PAIdentifier {
    /// The name of an unresolved, nested type.
    Identifier name;

    /// The associated type or typealias for a resolved nested type.
    TypeDecl *assocTypeOrConcrete;

    /// The generic parameter key for a root.
    GenericParamKey genericParam;

    PAIdentifier(Identifier name) : name(name) { }

    PAIdentifier(AssociatedTypeDecl *assocType)
      : assocTypeOrConcrete(assocType) { }

    PAIdentifier(TypeDecl *concreteDecl)
      : assocTypeOrConcrete(concreteDecl) { }

    PAIdentifier(GenericParamKey genericParam) : genericParam(genericParam) { }
  } identifier;

  /// \brief The representative of the equivalence class of potential archetypes
  /// to which this potential archetype belongs, or (for the representative)
  /// the equivalence class itself.
  mutable llvm::PointerUnion<PotentialArchetype *, EquivalenceClass *>
    representativeOrEquivClass;

  /// A stored nested type.
  struct StoredNestedType {
    /// The potential archetypes describing this nested type, all of which
    /// are equivalent.
    llvm::TinyPtrVector<PotentialArchetype *> archetypes;

    typedef llvm::TinyPtrVector<PotentialArchetype *>::iterator iterator;
    iterator begin() { return archetypes.begin(); }
    iterator end() { return archetypes.end(); }

    typedef llvm::TinyPtrVector<PotentialArchetype *>::const_iterator
      const_iterator;
    const_iterator begin() const { return archetypes.begin(); }
    const_iterator end() const { return archetypes.end(); }

    PotentialArchetype *front() const { return archetypes.front(); }
    PotentialArchetype *back() const { return archetypes.back(); }

    unsigned size() const { return archetypes.size(); }
    bool empty() const { return archetypes.empty(); }

    void push_back(PotentialArchetype *pa) {
      archetypes.push_back(pa);
    }
  };

  /// \brief The set of nested types of this archetype.
  ///
  /// For a given nested type name, there may be multiple potential archetypes
  /// corresponding to different associated types (from different protocols)
  /// that share a name.
  llvm::MapVector<Identifier, StoredNestedType> NestedTypes;

  /// Tracks the number of conformances that
  unsigned numConformancesInNestedType = 0;

  /// Whether this is an unresolved nested type.
  unsigned isUnresolvedNestedType : 1;

  /// \brief Recursively conforms to itself.
  unsigned IsRecursive : 1;

  /// Whether this potential archetype is invalid, e.g., because it could not
  /// be resolved.
  unsigned Invalid : 1;

  /// Whether we have diagnosed a rename.
  unsigned DiagnosedRename : 1;

  /// If we have renamed this (nested) type due to typo correction,
  /// the old name.
  Identifier OrigName;

  /// \brief Construct a new potential archetype for an unresolved
  /// associated type.
  PotentialArchetype(PotentialArchetype *parent, Identifier name);

  /// \brief Construct a new potential archetype for an associated type.
  PotentialArchetype(PotentialArchetype *parent, AssociatedTypeDecl *assocType)
    : parentOrBuilder(parent), identifier(assocType),
      isUnresolvedNestedType(false), IsRecursive(false), Invalid(false),
      DiagnosedRename(false)
  {
    assert(parent != nullptr && "Not an associated type?");
  }

  /// \brief Construct a new potential archetype for a concrete declaration.
  PotentialArchetype(PotentialArchetype *parent, TypeDecl *concreteDecl)
    : parentOrBuilder(parent), identifier(concreteDecl),
      isUnresolvedNestedType(false),
      IsRecursive(false), Invalid(false),
      DiagnosedRename(false)
  {
    assert(parent != nullptr && "Not an associated type?");
  }

  /// \brief Construct a new potential archetype for a generic parameter.
  PotentialArchetype(GenericSignatureBuilder *builder, GenericParamKey genericParam)
    : parentOrBuilder(builder), identifier(genericParam),
      isUnresolvedNestedType(false),
      IsRecursive(false), Invalid(false),
      DiagnosedRename(false)
  {
  }

  /// \brief Retrieve the representative for this archetype, performing
  /// path compression on the way.
  PotentialArchetype *getRepresentative() const;

  /// Retrieve the generic signature builder with which this archetype is
  /// associated.
  GenericSignatureBuilder *getBuilder() const {
    const PotentialArchetype *pa = this;
    while (auto parent = pa->getParent())
      pa = parent;
    return pa->parentOrBuilder.get<GenericSignatureBuilder *>();
  }

  friend class GenericSignatureBuilder;
  friend class GenericSignature;

  /// \brief Retrieve the debug name of this potential archetype.
  std::string getDebugName() const;

public:
  ~PotentialArchetype();

  /// Retrieve the parent of this potential archetype, which will be non-null
  /// when this potential archetype is an associated type.
  PotentialArchetype *getParent() const { 
    return parentOrBuilder.dyn_cast<PotentialArchetype *>();
  }

  /// Retrieve the associated type to which this potential archetype
  /// has been resolved.
  AssociatedTypeDecl *getResolvedAssociatedType() const {
    assert(getParent() && "Not an associated type");
    if (isUnresolvedNestedType)
      return nullptr;

    return dyn_cast<AssociatedTypeDecl>(identifier.assocTypeOrConcrete);
  }

  /// Determine whether this PA is still unresolved.
  bool isUnresolved() const { return isUnresolvedNestedType; }

  /// Resolve the potential archetype to the given associated type.
  void resolveAssociatedType(AssociatedTypeDecl *assocType,
                             GenericSignatureBuilder &builder);

  /// Resolve the potential archetype to the given typealias.
  void resolveConcreteType(TypeDecl *concreteDecl,
                           GenericSignatureBuilder &builder);

  /// Determine whether this is a generic parameter.
  bool isGenericParam() const {
    return parentOrBuilder.is<GenericSignatureBuilder *>();
  }

  /// Retrieve the generic parameter key for a potential archetype that
  /// represents this potential archetype.
  ///
  /// \pre \c isGenericParam()
  GenericParamKey getGenericParamKey() const {
    assert(isGenericParam() && "Not a generic parameter");
    return identifier.genericParam;
  }

  /// Retrieve the generic parameter key for the generic parameter at the
  /// root of this potential archetype.
  GenericParamKey getRootGenericParamKey() const {
    if (auto parent = getParent())
      return parent->getRootGenericParamKey();

    return getGenericParamKey();
  }

  /// Retrieve the name of a nested potential archetype.
  Identifier getNestedName() const {
    assert(getParent() && "Not a nested type");
    if (isUnresolvedNestedType)
      return identifier.name;

    return identifier.assocTypeOrConcrete->getName();
  }

  /// Retrieve the concrete type declaration.
  TypeDecl *getConcreteTypeDecl() const {
    assert(getParent() && "not a nested type");
    if (isUnresolvedNestedType)
      return nullptr;

    if (isa<AssociatedTypeDecl>(identifier.assocTypeOrConcrete))
      return nullptr;

    return identifier.assocTypeOrConcrete;
  }

  /// Retrieve the set of protocols to which this potential archetype
  /// conforms.
  SmallVector<ProtocolDecl *, 4> getConformsTo() const {
    SmallVector<ProtocolDecl *, 4> result;

    if (auto equiv = getEquivalenceClassIfPresent()) {
      for (const auto &entry : equiv->conformsTo)
        result.push_back(entry.first);
    }

    return result;
  }

  /// Add a conformance to this potential archetype.
  ///
  /// \returns true if the conformance was new, false if it already existed.
  bool addConformance(ProtocolDecl *proto,
                      const RequirementSource *source,
                      GenericSignatureBuilder &builder);

  /// Retrieve the superclass of this archetype.
  Type getSuperclass() const {
    if (auto equiv = getEquivalenceClassIfPresent())
      return equiv->superclass;
    
    return nullptr;
  }

  /// Retrieve the layout constraint of this archetype.
  LayoutConstraint getLayout() const {
    if (auto equivClass = getEquivalenceClassIfPresent())
      return equivClass->layout;

    return LayoutConstraint();
  }

  /// Retrieve the set of nested types.
  const llvm::MapVector<Identifier, StoredNestedType> &getNestedTypes() const {
    return NestedTypes;
  }

  /// \brief Determine the nesting depth of this potential archetype, e.g.,
  /// the number of associated type references.
  unsigned getNestingDepth() const;

  /// Determine whether two potential archetypes are in the same equivalence
  /// class.
  bool isInSameEquivalenceClassAs(const PotentialArchetype *other) const {
    return getRepresentative() == other->getRepresentative();
  }

  /// Retrieve the equivalence class, if it's already present.
  ///
  /// Otherwise, return null.
  EquivalenceClass *getEquivalenceClassIfPresent() const {
    return getRepresentative()->representativeOrEquivClass
             .dyn_cast<EquivalenceClass *>();
  }

  /// Retrieve or create the equivalence class.
  EquivalenceClass *getOrCreateEquivalenceClass() const;

  /// Retrieve the equivalence class containing this potential archetype.
  TinyPtrVector<PotentialArchetype *> getEquivalenceClassMembers() const {
    if (auto equivClass = getEquivalenceClassIfPresent())
      return equivClass->members;

    return TinyPtrVector<PotentialArchetype *>(
                                       const_cast<PotentialArchetype *>(this));
  }

  /// \brief Retrieve the potential archetype to be used as the anchor for
  /// potential archetype computations.
  PotentialArchetype *getArchetypeAnchor(GenericSignatureBuilder &builder);

  /// Add a same-type constraint between this archetype and the given
  /// other archetype.
  void addSameTypeConstraint(PotentialArchetype *otherPA,
                             const RequirementSource *source);

  /// Retrieve the same-type constraints.
  ArrayRef<Constraint<PotentialArchetype *>> getSameTypeConstraints() const {
    if (auto equivClass = getEquivalenceClassIfPresent()) {
      auto known = equivClass->sameTypeConstraints.find(
                                       const_cast<PotentialArchetype *>(this));
      if (known == equivClass->sameTypeConstraints.end()) return { };
      return known->second;
    }

    return { };
  }

  /// \brief Retrieve (or create) a nested type with the given name.
  PotentialArchetype *getNestedType(Identifier Name,
                                    GenericSignatureBuilder &builder);

  /// \brief Retrieve (or create) a nested type with a known associated type.
  PotentialArchetype *getNestedType(AssociatedTypeDecl *assocType,
                                    GenericSignatureBuilder &builder);

  /// \brief Retrieve (or create) a nested type with a known concrete type
  /// declaration.
  PotentialArchetype *getNestedType(TypeDecl *concreteDecl,
                                    GenericSignatureBuilder &builder);

  /// Describes the kind of update that is performed.
  enum class NestedTypeUpdate {
    /// Resolve an existing potential archetype, but don't create a new
    /// one if not present.
    ResolveExisting,
    /// If this potential archetype is missing, create it.
    AddIfMissing,
    /// If this potential archetype is missing and would be a better anchor,
    /// create it.
    AddIfBetterAnchor,
  };

  /// \brief Retrieve (or create) a nested type that is the current best
  /// nested archetype anchor (locally) with the given name.
  ///
  /// When called on the archetype anchor, this will produce the named
  /// archetype anchor.
  PotentialArchetype *getNestedArchetypeAnchor(
                       Identifier name,
                       GenericSignatureBuilder &builder,
                       NestedTypeUpdate kind = NestedTypeUpdate::AddIfMissing);

  /// Update the named nested type when we know this type conforms to the given
  /// protocol.
  ///
  /// \returns the potential archetype associated with the associated
  /// type or typealias of the given protocol, unless the \c kind implies that
  /// a potential archetype should not be created if it's missing.
  PotentialArchetype *updateNestedTypeForConformance(
                      PointerUnion<AssociatedTypeDecl *, TypeDecl *> type,
                      NestedTypeUpdate kind);

  /// Update the named nested type when we know this type conforms to the given
  /// protocol.
  ///
  /// \returns the potential archetype associated with either an associated
  /// type or typealias of the given protocol, unless the \c kind implies that
  /// a potential archetype should not be created if it's missing.
  PotentialArchetype *updateNestedTypeForConformance(
                        Identifier name,
                        ProtocolDecl *protocol,
                        NestedTypeUpdate kind);

  /// \brief Retrieve (or build) the type corresponding to the potential
  /// archetype within the given generic environment.
  Type getTypeInContext(GenericSignatureBuilder &builder,
                        GenericEnvironment *genericEnv);

  /// Retrieve the dependent type that describes this potential
  /// archetype.
  ///
  /// \param genericParams The set of generic parameters to use in the resulting
  /// dependent type.
  ///
  /// \param allowUnresolved If true, allow the result to contain
  /// \c DependentMemberType types with a name but no specific associated
  /// type.
  Type getDependentType(ArrayRef<GenericTypeParamType *> genericParams,
                        bool allowUnresolved);

  /// True if the potential archetype has been bound by a concrete type
  /// constraint.
  bool isConcreteType() const {
    if (auto equivClass = getEquivalenceClassIfPresent())
      return static_cast<bool>(equivClass->concreteType);

    return false;
  }
  
  /// Get the concrete type this potential archetype is constrained to.
  Type getConcreteType() const {
    if (auto equivClass = getEquivalenceClassIfPresent())
      return equivClass->concreteType;

    return Type();
  }

  void setIsRecursive() { IsRecursive = true; }
  bool isRecursive() const { return IsRecursive; }

  bool isInvalid() const { return Invalid; }

  void setInvalid() { Invalid = true; }

  /// Determine whether this archetype was renamed due to typo
  /// correction. If so, \c getName() retrieves the new name.
  bool wasRenamed() const { return !OrigName.empty(); }

  /// Note that this potential archetype was is going to be renamed (due to typo
  /// correction), saving the old name.
  void saveNameForRenaming() {
    OrigName = getNestedName();
  }

  /// For a renamed potential archetype, retrieve the original name.
  Identifier getOriginalName() const {
    assert(wasRenamed());
    return OrigName;
  }

  /// Whether we already diagnosed this rename.
  bool alreadyDiagnosedRename() const { return DiagnosedRename; }

  /// Note that we already diagnosed this rename.
  void setAlreadyDiagnosedRename() { DiagnosedRename = true; }

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const,
      "only for use within the debugger");

  void dump(llvm::raw_ostream &Out, SourceManager *SrcMgr,
            unsigned Indent) const;

  friend class GenericSignatureBuilder;
};

/// Describes a requirement whose processing has been delayed for some reason.
class GenericSignatureBuilder::DelayedRequirement {
public:
  enum Kind {
    /// A type requirement, which may be a conformance or a superclass
    /// requirement.
    Type,

    /// A layout requirement.
    Layout,

    /// A same-type requirement.
    SameType,

    /// An unresolved potential archetype.
    Unresolved,
  };

  Kind kind;
  UnresolvedType lhs;
  RequirementRHS rhs;
  FloatingRequirementSource source;
};

/// Whether the given constraint result signals an error.
inline bool isErrorResult(GenericSignatureBuilder::ConstraintResult result) {
  switch (result) {
  case GenericSignatureBuilder::ConstraintResult::Concrete:
  case GenericSignatureBuilder::ConstraintResult::Conflicting:
    return true;

  case GenericSignatureBuilder::ConstraintResult::Resolved:
  case GenericSignatureBuilder::ConstraintResult::Unresolved:
    return false;
  }
}

} // end namespace swift

#endif
