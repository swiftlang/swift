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
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/ilist.h"
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
  /// Only create a potential archetype when it is well-formed (e.g., a nested
  /// type should exist) and make sure we have complete information about
  /// that potential archetype.
  CompleteWellFormed,

  /// Only create a new potential archetype to describe this dependent type
  /// if it is already known.
  AlreadyKnown,

  /// Only create a potential archetype when it is well-formed (i.e., we know
  /// that there is a nested type with that name), but (unlike \c AlreadyKnown)
  /// allow the creation of a new potential archetype.
  WellFormed,
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
  class ResolvedType;

  using UnresolvedRequirementRHS =
      llvm::PointerUnion3<Type, PotentialArchetype *, LayoutConstraint>;

  using RequirementRHS =
    llvm::PointerUnion3<Type, PotentialArchetype *, LayoutConstraint>;

  /// The location of a requirement as written somewhere in the source.
  typedef llvm::PointerUnion<const TypeRepr *, const RequirementRepr *>
    WrittenRequirementLoc;

  class RequirementSource;

  class FloatingRequirementSource;

  class DelayedRequirement;

  template<typename T> struct Constraint;

  /// Describes a concrete constraint on a potential archetype where, where the
  /// other parameter is a concrete type.
  typedef Constraint<Type> ConcreteConstraint;

  /// Describes an equivalence class of potential archetypes.
  struct EquivalenceClass : llvm::ilist_node<EquivalenceClass> {
    /// The list of protocols to which this equivalence class conforms.
    ///
    /// The keys form the (semantic) list of protocols to which this type
    /// conforms. The values are the conformance constraints as written on
    /// this equivalence class.
    llvm::MapVector<ProtocolDecl *, std::vector<Constraint<ProtocolDecl *>>>
      conformsTo;

    /// Same-type constraints within this equivalence class.
    std::vector<Constraint<Type>> sameTypeConstraints;

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
    ///
    /// This list of members is slightly ordered, in that the first
    /// element always has a depth no greater than the depth of any other
    /// member.
    TinyPtrVector<PotentialArchetype *> members;

    /// Describes a component within the graph of same-type constraints within
    /// the equivalence class that is held together by derived constraints.
    struct DerivedSameTypeComponent {
      /// The potential archetype that acts as the anchor for this component.
      UnresolvedType anchor;

      /// The (best) requirement source within the component that makes the
      /// potential archetypes in this component equivalent to the concrete
      /// type.
      const RequirementSource *concreteTypeSource;
    };

    /// The set of connected components within this equivalence class, using
    /// only the derived same-type constraints in the graph.
    std::vector<DerivedSameTypeComponent> derivedSameTypeComponents;

    /// Delayed requirements that could be resolved by a change to this
    /// equivalence class.
    std::vector<DelayedRequirement> delayedRequirements;

    /// Whether we have detected recursion during the substitution of
    /// the concrete type.
    unsigned recursiveConcreteType : 1;

    /// Whether we have an invalid concrete type.
    unsigned invalidConcreteType : 1;

    /// Whether we have detected recursion during the substitution of
    /// the superclass type.
    unsigned recursiveSuperclassType : 1;

    /// Construct a new equivalence class containing only the given
    /// potential archetype (which represents itself).
    EquivalenceClass(PotentialArchetype *representative);

    /// Note that this equivalence class has been modified.
    void modified(GenericSignatureBuilder &builder);

    EquivalenceClass(const EquivalenceClass &) = delete;
    EquivalenceClass(EquivalenceClass &&) = delete;
    EquivalenceClass &operator=(const EquivalenceClass &) = delete;
    EquivalenceClass &operator=(EquivalenceClass &&) = delete;

    /// Add a new member to this equivalence class.
    void addMember(PotentialArchetype *pa);

    /// Record the conformance of this equivalence class to the given
    /// protocol as found via the given requirement source.
    ///
    /// \returns true if this conformance is new to the equivalence class,
    /// and false otherwise.
    bool recordConformanceConstraint(GenericSignatureBuilder &builder,
                                     ResolvedType type,
                                     ProtocolDecl *proto,
                                     FloatingRequirementSource source);

    /// Find a source of the same-type constraint that maps a potential
    /// archetype in this equivalence class to a concrete type along with
    /// that concrete type as written.
    Optional<ConcreteConstraint>
    findAnyConcreteConstraintAsWritten(Type preferredType = Type()) const;

    /// Find a source of the superclass constraint in this equivalence class
    /// that has a type equivalence to \c superclass, along with that
    /// superclass type as written.
    Optional<ConcreteConstraint>
    findAnySuperclassConstraintAsWritten(Type preferredType = Type()) const;

    /// Determine whether conformance to the given protocol is satisfied by
    /// a superclass requirement.
    bool isConformanceSatisfiedBySuperclass(ProtocolDecl *proto) const;

    /// Lookup a nested type with the given name within this equivalence
    /// class.
    ///
    /// \param otherConcreteTypes If non-null, will be filled in the all of the
    /// concrete types we found (other than the result) with the same name.
    TypeDecl *lookupNestedType(
                   GenericSignatureBuilder &builder,
                   Identifier name,
                   SmallVectorImpl<TypeDecl *> *otherConcreteTypes = nullptr);

    /// Retrieve the "anchor" type that canonically describes this equivalence
    /// class, for use in the canonical type.
    Type getAnchor(GenericSignatureBuilder &builder,
                   TypeArrayView<GenericTypeParamType> genericParams);

    /// \brief Retrieve (or build) the contextual type corresponding to
    /// this equivalence class within the given generic environment.
    Type getTypeInContext(GenericSignatureBuilder &builder,
                          GenericEnvironment *genericEnv);

    /// Dump a debugging representation of this equivalence class,
    void dump(llvm::raw_ostream &out,
              GenericSignatureBuilder *builder = nullptr) const;

    LLVM_ATTRIBUTE_DEPRECATED(
                  void dump(GenericSignatureBuilder *builder = nullptr) const,
                  "only for use in the debugger");

    /// Caches.

    /// The cached archetype anchor.
    struct {
      /// The cached anchor itself.
      Type anchor;

      /// The generation at which the anchor was last computed.
      unsigned lastGeneration;
    } archetypeAnchorCache;

    /// Describes a cached nested type.
    struct CachedNestedType {
      unsigned numConformancesPresent;
      CanType superclassPresent;
      llvm::TinyPtrVector<TypeDecl *> types;
    };

    /// Cached nested-type information, which contains the best declaration
    /// for a given name.
    llvm::SmallDenseMap<Identifier, CachedNestedType> nestedTypeNameCache;
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

    /// Generate an unresolved constraint but still return
    /// \c ConstraintResult::Unresolved so the caller knows what happened.
    GenerateUnresolved = 1,
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

  /// When a particular requirement cannot be resolved due to, e.g., a
  /// currently-unresolvable or nested type, this routine should be
  /// called to cope with the unresolved requirement.
  ///
  /// \returns \c ConstraintResult::Resolved or ConstraintResult::Delayed,
  /// as appropriate based on \c unresolvedHandling.
  ConstraintResult handleUnresolvedRequirement(RequirementKind kind,
                                   UnresolvedType lhs,
                                   UnresolvedRequirementRHS rhs,
                                   FloatingRequirementSource source,
                                   EquivalenceClass *unresolvedEquivClass,
                                   UnresolvedHandlingKind unresolvedHandling);

  /// Resolve the conformance of the given type to the given protocol when the
  /// potential archetype is known to be equivalent to a concrete type.
  ///
  /// \returns the requirement source for the resolved conformance, or nullptr
  /// if the conformance could not be resolved.
  const RequirementSource *resolveConcreteConformance(ResolvedType type,
                                                      ProtocolDecl *proto);

  /// Retrieve the constraint source conformance for the superclass constraint
  /// of the given potential archetype (if present) to the given protocol.
  ///
  /// \param type The type whose superclass constraint is being queried.
  ///
  /// \param proto The protocol to which we are establishing conformance.
  const RequirementSource *resolveSuperConformance(ResolvedType type,
                                                   ProtocolDecl *proto);

public:
  /// \brief Add a new conformance requirement specifying that the given
  /// type conforms to the given protocol.
  ConstraintResult addConformanceRequirement(ResolvedType type,
                                             ProtocolDecl *proto,
                                             FloatingRequirementSource source);

  /// "Expand" the conformance of the given \c pa to the protocol \c proto,
  /// adding the requirements from its requirement signature, rooted at
  /// the given requirement \c source.
  ConstraintResult expandConformanceRequirement(
                                      ResolvedType selfType,
                                      ProtocolDecl *proto,
                                      const RequirementSource *source,
                                      bool onlySameTypeConstraints);

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
  ///
  /// \returns true if anything in the equivalence class changed, false
  /// otherwise.
  bool updateSuperclass(ResolvedType type,
                        Type superclass,
                        FloatingRequirementSource source);

private:
  /// \brief Add a new superclass requirement specifying that the given
  /// potential archetype has the given type as an ancestor.
  ConstraintResult addSuperclassRequirementDirect(
                                              ResolvedType type,
                                              Type superclass,
                                              FloatingRequirementSource source);

  /// \brief Add a new type requirement specifying that the given
  /// type conforms-to or is a superclass of the second type.
  ///
  /// \param inferForModule Infer additional requirements from the types
  /// relative to the given module.
  ConstraintResult addTypeRequirement(UnresolvedType subject,
                                      UnresolvedType constraint,
                                      FloatingRequirementSource source,
                                      UnresolvedHandlingKind unresolvedHandling,
                                      ModuleDecl *inferForModule);

  /// Note that we have added the nested type nestedPA
  void addedNestedType(PotentialArchetype *nestedPA);

  /// Add a rewrite rule from that makes the two types equivalent.
  ///
  /// \returns true if a new rewrite rule was added, and false otherwise.
  bool addSameTypeRewriteRule(CanType type1, CanType type2);

  /// \brief Add a same-type requirement between two types that are known to
  /// refer to type parameters.
  ConstraintResult addSameTypeRequirementBetweenTypeParameters(
                                         ResolvedType type1, ResolvedType type2,
                                         const RequirementSource *source);
  
  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype is bound to a concrete type.
  ConstraintResult addSameTypeRequirementToConcrete(ResolvedType type,
                                        Type concrete,
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
  ConstraintResult addLayoutRequirementDirect(ResolvedType type,
                                              LayoutConstraint layout,
                                              FloatingRequirementSource source);

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

public:
  /// Construct a new generic signature builder.
  explicit GenericSignatureBuilder(ASTContext &ctx);
  GenericSignatureBuilder(GenericSignatureBuilder &&);
  ~GenericSignatureBuilder();

  /// Retrieve the AST context.
  ASTContext &getASTContext() const { return Context; }

  /// Functor class suitable for use as a \c LookupConformanceFn to look up a
  /// conformance in a generic signature builder.
  class LookUpConformanceInBuilder {
    GenericSignatureBuilder *builder;
  public:
    explicit LookUpConformanceInBuilder(GenericSignatureBuilder *builder)
      : builder(builder) {}

    Optional<ProtocolConformanceRef>
    operator()(CanType dependentType,
               Type conformingReplacementType,
               ProtocolType *conformedProtocol) const {
      return builder->lookupConformance(dependentType,
                                        conformingReplacementType,
                                        conformedProtocol);
    }
  };

  /// Retrieve a function that can perform conformance lookup for this
  /// builder.
  LookUpConformanceInBuilder getLookupConformanceFn();

  /// Lookup a protocol conformance in a module-agnostic manner.
  Optional<ProtocolConformanceRef>
  lookupConformance(CanType dependentType, Type conformingReplacementType,
                    ProtocolType *conformedProtocol);


  /// Retrieve the lazy resolver, if there is one.
  LazyResolver *getLazyResolver() const;

  /// Enumerate the requirements that describe the signature of this
  /// generic signature builder.
  ///
  /// \param f A function object that will be passed each requirement
  /// and requirement source.
  void enumerateRequirements(
                    TypeArrayView<GenericTypeParamType> genericParams,
                    llvm::function_ref<
                      void (RequirementKind kind,
                            Type type,
                            RequirementRHS constraint,
                            const RequirementSource *source)> f);

  /// Retrieve the generic parameters used to describe the generic
  /// signature being built.
  TypeArrayView<GenericTypeParamType> getGenericParams() const;

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
                                  ModuleDecl *inferForModule);

  /// \brief Add all of a generic signature's parameters and requirements.
  void addGenericSignature(GenericSignature *sig);

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
  /// After this point, one cannot introduce new requirements, and the
  /// generic signature builder no longer has valid state.
  GenericSignature *computeGenericSignature(
                      SourceLoc loc,
                      bool allowConcreteGenericParams = false,
                      bool allowBuilderToMove = true) &&;

  /// Compute the requirement signature for the given protocol.
  static GenericSignature *computeRequirementSignature(ProtocolDecl *proto);

private:
  /// Finalize the set of requirements, performing any remaining checking
  /// required before generating archetypes.
  ///
  /// \param allowConcreteGenericParams If true, allow generic parameters to
  /// be made concrete.
  void finalize(SourceLoc loc,
                TypeArrayView<GenericTypeParamType> genericParams,
                bool allowConcreteGenericParams=false);

public:
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
                           TypeArrayView<GenericTypeParamType> genericParams,
                           std::vector<Constraint<T>> &constraints,
                           llvm::function_ref<bool(const Constraint<T> &)>
                             isSuitableRepresentative,
                           llvm::function_ref<
                             ConstraintRelation(const Constraint<T>&)>
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
                           TypeArrayView<GenericTypeParamType> genericParams,
                           std::vector<Constraint<T>> &constraints,
                           llvm::function_ref<bool(const Constraint<T> &)>
                             isSuitableRepresentative,
                           llvm::function_ref<
                             ConstraintRelation(const Constraint<T>&)>
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
                            TypeArrayView<GenericTypeParamType> genericParams,
                            EquivalenceClass *equivClass);

  /// Check the superclass constraints within the equivalence
  /// class of the given potential archetype.
  void checkSuperclassConstraints(
                            TypeArrayView<GenericTypeParamType> genericParams,
                            EquivalenceClass *equivClass);

  /// Check conformance constraints within the equivalence class of the
  /// given potential archetype.
  void checkConformanceConstraints(
                            TypeArrayView<GenericTypeParamType> genericParams,
                            EquivalenceClass *equivClass);

  /// Check layout constraints within the equivalence class of the given
  /// potential archetype.
  void checkLayoutConstraints(TypeArrayView<GenericTypeParamType> genericParams,
                              EquivalenceClass *equivClass);

  /// Check same-type constraints within the equivalence class of the
  /// given potential archetype.
  void checkSameTypeConstraints(
                            TypeArrayView<GenericTypeParamType> genericParams,
                            EquivalenceClass *equivClass);

  /// Realize a potential archetype for the given type.
  ///
  /// The resolved archetype will be written back into the unresolved type,
  /// to make the next resolution more efficient.
  PotentialArchetype *realizePotentialArchetype(UnresolvedType &type);

public:
  /// \brief Try to resolve the equivalence class of the given type.
  ///
  /// \param type The type to resolve.
  ///
  /// \param resolutionKind How to perform the resolution.
  ///
  /// \param wantExactPotentialArchetype Whether to return the precise
  /// potential archetype described by the type (vs. just the equivalance
  /// class and resolved type).
  ResolvedType maybeResolveEquivalenceClass(
                                      Type type,
                                      ArchetypeResolutionKind resolutionKind,
                                      bool wantExactPotentialArchetype);

  /// \brief Resolve the equivalence class for the given type parameter,
  /// which provides information about that type.
  ///
  /// The \c resolutionKind parameter describes how resolution should be
  /// performed. If the potential archetype named by the given dependent type
  /// already exists, it will be always returned. If it doesn't exist yet,
  /// the \c resolutionKind dictates whether the potential archetype will
  /// be created or whether null will be returned.
  ///
  /// For any type that cannot refer to an equivalence class, this routine
  /// returns null.
  EquivalenceClass *resolveEquivalenceClass(
                      Type type,
                      ArchetypeResolutionKind resolutionKind);

  /// \brief Resolve the given type as far as this Builder knows how.
  ///
  /// If successful, this returns either a non-typealias potential archetype
  /// or a Type, if \c type is concrete.
  /// If the type cannot be resolved, e.g., because it is "too" recursive
  /// given the source, returns an unresolved result containing the equivalence
  /// class that would need to change to resolve this type.
  ResolvedType resolve(UnresolvedType type, FloatingRequirementSource source);

  /// Determine whether the two given types are in the same equivalence class.
  bool areInSameEquivalenceClass(Type type1, Type type2);

  /// Simplify the given dependent type down to its canonical representation.
  Type getCanonicalTypeParameter(Type type);

  /// Verify the correctness of the given generic signature.
  ///
  /// This routine will test that the given generic signature is both minimal
  /// and canonical, emitting errors if it is not.
  static void verifyGenericSignature(ASTContext &context,
                                     GenericSignature *sig);

  /// Verify all of the generic sigantures in the given module.
  static void verifyGenericSignaturesInModule(ModuleDecl *module);

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

    /// The requirement is the implicit binding of a type to
    /// the interface type of the concrete type declaration it represents.
    ///
    /// This is a root requirement source.
    ConcreteTypeBinding,

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
    /// This stores the \c ProtocolConformanceRef used to resolve the
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

    /// A requirement that was resolved based on structural derivation from
    /// another requirement.
    Derived,

    /// A requirement that was provided for another type in the
    /// same equivalence class, but which we want to "re-root" on a new
    /// type.
    EquivalentType,
  };

  /// The kind of requirement source.
  const Kind kind;

private:
  /// The kind of storage we have.
  enum class StorageKind : uint8_t {
    None,
    StoredType,
    ProtocolConformance,
    AssociatedTypeDecl,
  };

  /// The kind of storage we have.
  const StorageKind storageKind;

  /// Whether there is a trailing written requirement location.
  const bool hasTrailingWrittenRequirementLoc;

public:
  /// Whether a protocol requirement came from the requirement signature.
  const bool usesRequirementSignature;

private:
  /// The actual storage, described by \c storageKind.
  union {
    /// The type to which a requirement applies.
    TypeBase *type;

    /// A protocol conformance used to satisfy the requirement.
    void *conformance;

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
    case ConcreteTypeBinding:
    case Superclass:
    case Parent:
    case Concrete:
    case Derived:
    case EquivalentType:
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
    case ConcreteTypeBinding:
      return true;

    case ProtocolRequirement:
    case InferredProtocolRequirement:
    case Superclass:
    case Parent:
    case Concrete:
    case Derived:
    case EquivalentType:
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

  RequirementSource(Kind kind, Type rootType,
                    ProtocolDecl *protocol,
                    WrittenRequirementLoc writtenReqLoc)
    : kind(kind), storageKind(StorageKind::StoredType),
      hasTrailingWrittenRequirementLoc(!writtenReqLoc.isNull()),
      usesRequirementSignature(false), parent(nullptr) {
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.type = rootType.getPointer();
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
                    ProtocolConformanceRef conformance)
    : kind(kind), storageKind(StorageKind::ProtocolConformance),
      hasTrailingWrittenRequirementLoc(false),
      usesRequirementSignature(false), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.conformance = conformance.getOpaqueValue();
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

  RequirementSource(Kind kind, const RequirementSource *parent)
    : kind(kind), storageKind(StorageKind::None),
      hasTrailingWrittenRequirementLoc(false),
      usesRequirementSignature(false), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");
  }

  RequirementSource(Kind kind, const RequirementSource *parent,
                    Type newType)
    : kind(kind), storageKind(StorageKind::StoredType),
      hasTrailingWrittenRequirementLoc(false),
      usesRequirementSignature(false), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");
    storage.type = newType.getPointer();
  }

public:
  /// Retrieve an abstract requirement source.
  static const RequirementSource *forAbstract(GenericSignatureBuilder &builder,
                                              Type rootType);

  /// Retrieve a requirement source representing an explicit requirement
  /// stated in an 'inheritance' or 'where' clause.
  static const RequirementSource *forExplicit(GenericSignatureBuilder &builder,
                                              Type rootType,
                                              WrittenRequirementLoc writtenLoc);

  /// Retrieve a requirement source representing a requirement that is
  /// inferred from some part of a generic declaration's signature, e.g., the
  /// parameter or result type of a generic function.
  static const RequirementSource *forInferred(GenericSignatureBuilder &builder,
                                              Type rootType,
                                              const TypeRepr *typeRepr);

  /// Retrieve a requirement source representing the requirement signature
  /// computation for a protocol.
  static const RequirementSource *forRequirementSignature(
                                              GenericSignatureBuilder &builder,
                                              Type rootType,
                                              ProtocolDecl *protocol);

  /// Retrieve a requirement source for nested type name matches.
  static const RequirementSource *forNestedTypeNameMatch(
                                      GenericSignatureBuilder &builder,
                                      Type rootType);

  /// Retrieve a requirement source describing when a concrete type
  /// declaration is used to define a potential archetype.
  static const RequirementSource *forConcreteTypeBinding(
                                     GenericSignatureBuilder &builder,
                                     Type rootType);

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
                                    ProtocolConformanceRef conformance) const;

  /// A requirement source that describes that a requirement that is resolved
  /// via a same-type-to-concrete requirement.
  const RequirementSource *viaConcrete(
                                     GenericSignatureBuilder &builder,
                                     ProtocolConformanceRef conformance) const;

  /// A constraint source that describes that a constraint that is resolved
  /// for a nested type via a constraint on its parent.
  ///
  /// \param assocType the associated type that
  const RequirementSource *viaParent(GenericSignatureBuilder &builder,
                                     AssociatedTypeDecl *assocType) const;

  /// A constraint source that describes a constraint that is structurally
  /// derived from another constraint but does not require further information.
  const RequirementSource *viaDerived(GenericSignatureBuilder &builder) const;

  /// A constraint source that describes a constraint that is structurally
  /// derived from another constraint but does not require further information.
  const RequirementSource *viaEquivalentType(GenericSignatureBuilder &builder,
                                             Type newType) const;

  /// Form a new requirement source without the subpath [start, end).
  ///
  /// Removes a redundant sub-path \c [start, end) from the requirement source,
  /// creating a new requirement source comprised on \c start followed by
  /// everything that follows \c end.
  /// It is the caller's responsibility to ensure that the path up to \c start
  /// and the path through \c start to \c end produce the same thing.
  const RequirementSource *withoutRedundantSubpath(
                                          GenericSignatureBuilder &builder,
                                          const RequirementSource *start,
                                          const RequirementSource *end) const;

  /// Retrieve the root requirement source.
  const RequirementSource *getRoot() const;

  /// Retrieve the type at the root.
  Type getRootType() const;

  /// Retrieve the type to which this source refers.
  Type getAffectedType() const;

  /// Visit each of the types along the path, from the root type
  /// each type named via (e.g.) a protocol requirement or parent source.
  ///
  /// \param visitor Called with each type along the path along
  /// with the requirement source that is being applied on top of that
  /// type. Can return \c true to halt the search.
  ///
  /// \returns a null type if any call to \c visitor returned true. Otherwise,
  /// returns the type to which the entire source refers.
  Type visitPotentialArchetypesAlongPath(
           llvm::function_ref<bool(Type,
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

  /// Whether we should diagnose a redundant constraint based on this
  /// requirement source.
  ///
  /// \param primary Whether this is the "primary" requirement source, on which
  /// a "redundant constraint" warning would be emitted vs. the requirement
  /// source that would be used for the accompanying note.
  bool shouldDiagnoseRedundancy(bool primary) const;

  /// Determine whether the given derived requirement \c source, when rooted at
  /// the potential archetype \c pa, is actually derived from the same
  /// requirement. Such "self-derived" requirements do not make the original
  /// requirement redundant, because without said original requirement, the
  /// derived requirement ceases to hold.
  bool isSelfDerivedSource(GenericSignatureBuilder &builder,
                           Type type,
                           bool &derivedViaConcrete) const;

  /// For a requirement source that describes the requirement \c type:proto,
  /// retrieve the minimal subpath of this requirement source that will
  /// compute that requirement.
  ///
  /// When the result is different from (i.e., a subpath of) \c this or is
  /// nullptr (indicating an embedded, distinct self-derived subpath), the
  /// conformance requirement is considered to be "self-derived".
  const RequirementSource *getMinimalConformanceSource(
                                            GenericSignatureBuilder &builder,
                                            Type type,
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

  /// Retrieve the written requirement location, if there is one.
  WrittenRequirementLoc getWrittenRequirementLoc() const {
    if (!hasTrailingWrittenRequirementLoc) return WrittenRequirementLoc();
    return getTrailingObjects<WrittenRequirementLoc>()[0];
  }

  /// Retrieve the type representation for this requirement, if there is one.
  const TypeRepr *getTypeRepr() const {
    return getWrittenRequirementLoc().dyn_cast<const TypeRepr *>();
  }

  /// Retrieve the requirement representation for this requirement, if there is
  /// one.
  const RequirementRepr *getRequirementRepr() const {
    return getWrittenRequirementLoc().dyn_cast<const RequirementRepr *>();
  }

  /// Retrieve the type stored in this requirement.
  Type getStoredType() const;

  /// Retrieve the protocol for this requirement, if there is one.
  ProtocolDecl *getProtocolDecl() const;

  /// Retrieve the protocol conformance for this requirement, if there is one.
  ProtocolConformanceRef getProtocolConformance() const {
    assert(storageKind == StorageKind::ProtocolConformance);
    return ProtocolConformanceRef::getFromOpaqueValue(storage.conformance);
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
    result.protocolReq.written = WrittenRequirementLoc();
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
                                     Identifier nestedName) {
    FloatingRequirementSource result{ NestedTypeNameMatch, Storage() };
    result.nestedName = nestedName;
    return result;
  };

  /// Retrieve the complete requirement source rooted at the given type.
  const RequirementSource *getSource(GenericSignatureBuilder &builder,
                                     Type type) const;

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

/// Describes a specific constraint on a particular type.
template<typename T>
struct GenericSignatureBuilder::Constraint {
  /// The specific subject of the constraint.
  ///
  /// This may either be a (resolved) dependent type or the potential
  /// archetype that it resolves to.
  mutable UnresolvedType subject;

  /// A value used to describe the constraint.
  T value;

  /// The requirement source used to derive this constraint.
  const RequirementSource *source;

  /// Retrieve the dependent type describing the subject of the constraint.
  Type getSubjectDependentType(
                       TypeArrayView<GenericTypeParamType> genericParams) const;

  /// Determine whether the subject is equivalence to the given type.
  bool isSubjectEqualTo(Type type) const;

  /// Determine whether the subject is equivalence to the given type.
  bool isSubjectEqualTo(const PotentialArchetype *pa) const;

  /// Determine whether this constraint has the same subject as the
  /// given constraint.
  bool hasSameSubjectAs(const Constraint<T> &other) const;
};

class GenericSignatureBuilder::PotentialArchetype {
  /// The parent of this potential archetype (for a nested type) or the
  /// ASTContext in which the potential archetype resides.
  llvm::PointerUnion<PotentialArchetype*, ASTContext*> parentOrContext;

  /// The identifier describing this particular archetype.
  ///
  /// \c parentOrBuilder determines whether we have a nested type vs. a root.
  union PAIdentifier {
    /// The associated type for a resolved nested type.
    AssociatedTypeDecl *assocType;

    /// The generic parameter key for a root.
    GenericParamKey genericParam;

    PAIdentifier(AssociatedTypeDecl *assocType) : assocType(assocType) {}

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

  /// \brief Construct a new potential archetype for a concrete declaration.
  PotentialArchetype(PotentialArchetype *parent, AssociatedTypeDecl *assocType)
      : parentOrContext(parent), identifier(assocType) {
    assert(parent != nullptr && "Not a nested type?");
    assert(assocType->getOverriddenDecls().empty());
  }

  /// \brief Construct a new potential archetype for a generic parameter.
  PotentialArchetype(ASTContext &ctx, GenericParamKey genericParam)
    : parentOrContext(&ctx), identifier(genericParam)
  {
  }

public:
  /// \brief Retrieve the representative for this archetype, performing
  /// path compression on the way.
  PotentialArchetype *getRepresentative() const;

  friend class GenericSignatureBuilder;
  friend class GenericSignature;

public:
  ~PotentialArchetype();

  /// \brief Retrieve the debug name of this potential archetype.
  std::string getDebugName() const;

  /// Retrieve the parent of this potential archetype, which will be non-null
  /// when this potential archetype is an associated type.
  PotentialArchetype *getParent() const { 
    return parentOrContext.dyn_cast<PotentialArchetype *>();
  }

  /// Retrieve the type declaration to which this nested type was resolved.
  AssociatedTypeDecl *getResolvedType() const {
    assert(getParent() && "Not an associated type");
    return identifier.assocType;
  }

  /// Determine whether this is a generic parameter.
  bool isGenericParam() const {
    return parentOrContext.is<ASTContext *>();
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
    return identifier.assocType->getName();
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
  EquivalenceClass *getOrCreateEquivalenceClass(
                                    GenericSignatureBuilder &builder) const;

  /// Retrieve the equivalence class containing this potential archetype.
  TinyPtrVector<PotentialArchetype *> getEquivalenceClassMembers() const {
    if (auto equivClass = getEquivalenceClassIfPresent())
      return equivClass->members;

    return TinyPtrVector<PotentialArchetype *>(
                                       const_cast<PotentialArchetype *>(this));
  }

  /// Update the named nested type when we know this type conforms to the given
  /// protocol.
  ///
  /// \returns the potential archetype associated with the associated
  /// type of the given protocol, unless the \c kind implies that
  /// a potential archetype should not be created if it's missing.
  PotentialArchetype *
  updateNestedTypeForConformance(GenericSignatureBuilder &builder,
                                 AssociatedTypeDecl *assocType,
                                 ArchetypeResolutionKind kind);

  /// Retrieve the dependent type that describes this potential
  /// archetype.
  ///
  /// \param genericParams The set of generic parameters to use in the resulting
  /// dependent type.
  Type getDependentType(TypeArrayView<GenericTypeParamType> genericParams)const;

  /// True if the potential archetype has been bound by a concrete type
  /// constraint.
  bool isConcreteType() const {
    if (auto equivClass = getEquivalenceClassIfPresent())
      return static_cast<bool>(equivClass->concreteType);

    return false;
  }

  /// Retrieve the AST context in which this potential archetype resides.
  ASTContext &getASTContext() const;

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
  };

  Kind kind;
  UnresolvedType lhs;
  UnresolvedRequirementRHS rhs;
  FloatingRequirementSource source;

  /// Dump a debugging representation of this delayed requirement class.
  void dump(llvm::raw_ostream &out) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const,
                            "only for use in the debugger");
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

/// Canonical ordering for dependent types.
int compareDependentTypes(Type type1, Type type2);

} // end namespace swift

#endif
