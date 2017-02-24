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
#include "swift/AST/Identifier.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/TinyPtrVector.h"
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
class Type;
class TypeRepr;
class ASTContext;
class DiagnosticEngine;

/// Describes how a generic signature determines a requirement, from its origin
/// in some requirement written in the source, inferred through a path of
/// other implications (e.g., introduced by a particular protocol).
///
/// Requirement sources are uniqued within a generic signature builder.
class RequirementSource : public llvm::FoldingSetNode {
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
    /// This stores the protocol that introduced the requirement.
    ProtocolRequirement,

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
    None,
    TypeRepr,
    RequirementRepr,
    ProtocolDecl,
    ProtocolConformance,
  };

  /// The kind of storage we have.
  const StorageKind storageKind;

  /// The actual storage, described by \c storageKind.
  union {
    /// The type representation descibing where the requirement came from.
    const TypeRepr *typeRepr;

    /// Where a requirement came from.
    const RequirementRepr *requirementRepr;

    /// The protocol being described.
    ProtocolDecl *protocol;

    /// A protocol conformance used to satisfy the requirement.
    ProtocolConformance *conformance;
  } storage;

  /// Determines whether we have been provided with an acceptable storage kind
  /// for the given requirement source kind.
  static bool isAcceptableStorageKind(Kind kind, StorageKind storageKind);

  /// Retrieve the opaque storage as a single pointer, for use in uniquing.
  const void *getOpaqueStorage() const;

  /// Whether this kind of requirement source is a root.
  static bool isRootKind(Kind kind) {
    switch (kind) {
    case Explicit:
    case Inferred:
    case RequirementSignatureSelf:
    case NestedTypeNameMatch:
      return true;

    case ProtocolRequirement:
    case Superclass:
    case Parent:
    case Concrete:
      return false;
    }
  }

public:
  /// The "parent" of this requirement source.
  ///
  /// The chain of parent requirement sources will eventually terminate in a
  /// requirement source with one of the "root" kinds.
  const RequirementSource * const parent;

  RequirementSource(Kind kind, const RequirementSource *parent)
    : kind(kind), storageKind(StorageKind::None), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    // Prevent uninitialized memory.
    storage.typeRepr = nullptr;
  }

  RequirementSource(Kind kind, const RequirementSource *parent,
                    const TypeRepr *typeRepr)
    : kind(kind), storageKind(StorageKind::TypeRepr), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.typeRepr = typeRepr;
  }

  RequirementSource(Kind kind, const RequirementSource *parent,
                    const RequirementRepr *requirementRepr)
    : kind(kind), storageKind(StorageKind::RequirementRepr), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.requirementRepr = requirementRepr;
  }

  RequirementSource(Kind kind, const RequirementSource *parent,
                   ProtocolDecl *protocol)
    : kind(kind), storageKind(StorageKind::ProtocolDecl), parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.protocol = protocol;
  }

  RequirementSource(Kind kind, const RequirementSource *parent,
                   ProtocolConformance *conformance)
    : kind(kind), storageKind(StorageKind::ProtocolConformance),
      parent(parent) {
    assert((static_cast<bool>(parent) != isRootKind(kind)) &&
           "Root RequirementSource should not have parent (or vice versa)");
    assert(isAcceptableStorageKind(kind, storageKind) &&
           "RequirementSource kind/storageKind mismatch");

    storage.conformance = conformance;
  }

public:
  /// Retrieve an abstract requirement source.
  static const RequirementSource *forAbstract(GenericSignatureBuilder &builder);

  /// Retrieve a requirement source representing an explicit requirement
  /// stated in an 'inheritance' clause.
  static const RequirementSource *forExplicit(GenericSignatureBuilder &builder,
                                              const TypeRepr *typeRepr);

  /// Retrieve a requirement source representing an explicit requirement
  /// stated in an 'where' clause.
  static const RequirementSource *forExplicit(GenericSignatureBuilder &builder,
                                        const RequirementRepr *requirementRepr);

  /// Retrieve a requirement source representing a requirement that is
  /// inferred from some part of a generic declaration's signature, e.g., the
  /// parameter or result type of a generic function.
  static const RequirementSource *forInferred(GenericSignatureBuilder &builder,
                                              const TypeRepr *typeRepr);

  /// Retrieve a requirement source representing the requirement signature
  /// computation for a protocol.
  static const RequirementSource *forRequirementSignature(
                                              GenericSignatureBuilder &builder,
                                              ProtocolDecl *protocol);

  /// Retrieve an requirement source for nested type name matches.
  static const RequirementSource *forNestedTypeNameMatch(
                                              GenericSignatureBuilder &builder);

  /// A requirement source that describes that a requirement comes from a
  /// requirement of the given protocol described by the parent.
  const RequirementSource *viaAbstractProtocolRequirement(
                             GenericSignatureBuilder &builder,
                             ProtocolDecl *protocol) const;

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
  const RequirementSource *viaParent(GenericSignatureBuilder &builder) const;

  /// Whether the requirement can be derived from something in its path.
  ///
  /// Derived requirements will not be recorded in a minimized generic
  /// signature, because the information can be re-derived by following the
  /// path.
  bool isDerivedRequirement() const;

  /// Whether the requirement is derived via some concrete conformance, e.g.,
  /// a concrete type's conformance to a protocol or a superclass's conformance
  /// to a protocol.
  bool isDerivedViaConcreteConformance() const;

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
    if (storageKind != StorageKind::TypeRepr) return nullptr;
    return storage.typeRepr;
  }

  /// Retrieve the requirement representation for this requirement, if there is
  /// one.
  const RequirementRepr *getRequirementRepr() const {
    if (storageKind != StorageKind::RequirementRepr) return nullptr;
    return storage.requirementRepr;
  }

  /// Retrieve the protocol for this requirement, if there is one.
  ProtocolDecl *getProtocolDecl() const;

  /// Retrieve the protocol conformance for this requirement, if there is one.
  ProtocolConformance *getProtocolConformance() const {
    if (storageKind != StorageKind::ProtocolConformance) return nullptr;
    return storage.conformance;
  }

  /// Profiling support for \c FoldingSet.
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, kind, parent, getOpaqueStorage());
  }

  /// Profiling support for \c FoldingSet.
  static void Profile(llvm::FoldingSetNodeID &ID, Kind kind,
                      const RequirementSource *parent, const void *storage) {
    ID.AddInteger(kind);
    ID.AddPointer(parent);
    ID.AddPointer(storage);
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

  /// Describes an equivalence class of potential archetypes.
  struct EquivalenceClass {
    /// Concrete type to which this equivalence class is equal.
    Type concreteType;

    /// The members of the equivalence class.
    TinyPtrVector<PotentialArchetype *> members;

    /// Construct a new equivalence class containing only the given
    /// potential archetype (which represents itself).
    EquivalenceClass(PotentialArchetype *representative);
  };

  friend class RequirementSource;

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

  /// Update an existing constraint source reference when another constraint
  /// source was found to produce the same constraint. Only the better
  /// constraint source will be kept.
  ///
  /// \returns true if the new constraint source was better, false otherwise.
  bool updateRequirementSource(const RequirementSource *&existingSource,
                               const RequirementSource *newSource);

  /// Retrieve the constraint source conformance for the superclass constraint
  /// of the given potential archetype (if present) to the given protocol.
  ///
  /// \param pa The potential archetype whose superclass constraint is being
  /// queried.
  ///
  /// \param proto The protocol to which we are establishing conformance.
  ///
  /// \param protoSource The requirement source for the conformance to the
  /// given protocol.
  const RequirementSource *resolveSuperConformance(
                            GenericSignatureBuilder::PotentialArchetype *pa,
                            ProtocolDecl *proto,
                            const RequirementSource *&protoSource);

  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype conforms to the given protocol.
  bool addConformanceRequirement(PotentialArchetype *T,
                                 ProtocolDecl *Proto,
                                 const RequirementSource *Source);

  bool addConformanceRequirement(PotentialArchetype *T,
                                 ProtocolDecl *Proto,
                                 const RequirementSource *Source,
                                llvm::SmallPtrSetImpl<ProtocolDecl *> &Visited);

public:
  /// \brief Add a new same-type requirement between two fully resolved types
  /// (output of \c GenericSignatureBuilder::resolve).
  ///
  /// If the types refer to two concrete types that are fundamentally
  /// incompatible (e.g. \c Foo<Bar<T>> and \c Foo<Baz>), \c diagnoseMismatch is
  /// called with the two types that don't match (\c Bar<T> and \c Baz for the
  /// previous example).
  bool
  addSameTypeRequirement(ResolvedType paOrT1, ResolvedType paOrT2,
                         const RequirementSource *Source,
                         llvm::function_ref<void(Type, Type)> diagnoseMismatch);

  /// \brief Add a new same-type requirement between two fully resolved types
  /// (output of GenericSignatureBuilder::resolve).
  ///
  /// The two types must not be incompatible concrete types.
  bool addSameTypeRequirement(ResolvedType paOrT1, ResolvedType paOrT2,
                              const RequirementSource *Source);

  /// \brief Add a new same-type requirement between two unresolved types.
  ///
  /// The types are resolved with \c GenericSignatureBuilder::resolve, and must
  /// not be incompatible concrete types.
  bool addSameTypeRequirement(UnresolvedType paOrT1, UnresolvedType paOrT2,
                              const RequirementSource *Source);

  /// \brief Add a new same-type requirement between two unresolved types.
  ///
  /// The types are resolved with \c GenericSignatureBuilder::resolve. \c
  /// diagnoseMismatch is called if the two types refer to incompatible concrete
  /// types.
  bool
  addSameTypeRequirement(UnresolvedType paOrT1, UnresolvedType paOrT2,
                         const RequirementSource *Source,
                         llvm::function_ref<void(Type, Type)> diagnoseMismatch);

private:
  /// \brief Add a new superclass requirement specifying that the given
  /// potential archetype has the given type as an ancestor.
  bool addSuperclassRequirement(PotentialArchetype *T,
                                Type Superclass,
                                const RequirementSource *Source);

  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetypes are equivalent.
  bool addSameTypeRequirementBetweenArchetypes(PotentialArchetype *T1,
                                               PotentialArchetype *T2,
                                               const RequirementSource *Source);
  
  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype is bound to a concrete type.
  bool addSameTypeRequirementToConcrete(PotentialArchetype *T,
                                        Type Concrete,
                                        const RequirementSource *Source);

  /// \brief Add a new same-type requirement specifying that the given two
  /// types should be the same.
  ///
  /// \param diagnoseMismatch Callback invoked when the types in the same-type
  /// requirement mismatch.
  bool addSameTypeRequirementBetweenConcrete(
      Type T1, Type T2, const RequirementSource *Source,
      llvm::function_ref<void(Type, Type)> diagnoseMismatch);

  /// Add the requirements placed on the given type parameter
  /// to the given potential archetype.
  bool addInheritedRequirements(TypeDecl *decl, PotentialArchetype *pa,
                                const RequirementSource *parentSource,
                                llvm::SmallPtrSetImpl<ProtocolDecl *> &visited);

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
  /// \returns true if this requirement makes the set of requirements
  /// inconsistent, in which case a diagnostic will have been issued.
  bool addRequirement(const RequirementRepr *Req);

  /// \brief Add an already-checked requirement.
  ///
  /// Adding an already-checked requirement cannot fail. This is used to
  /// re-inject requirements from outer contexts.
  ///
  /// \returns true if this requirement makes the set of requirements
  /// inconsistent, in which case a diagnostic will have been issued.
  bool addRequirement(const Requirement &req, const RequirementSource *source);

  bool addRequirement(const Requirement &req, const RequirementSource *source,
                      llvm::SmallPtrSetImpl<ProtocolDecl *> &Visited);

  bool addLayoutRequirement(PotentialArchetype *PAT,
                            LayoutConstraint Layout,
                            const RequirementSource *Source);

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
  void inferRequirements(TypeLoc type, unsigned minDepth, unsigned maxDepth);

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
  void inferRequirements(ParameterList *params,GenericParamList *genericParams);

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

private:
  /// Check for redundant concrete type constraints within the equivalence
  /// class of the given potential archetype.
  void checkRedundantConcreteTypeConstraints(
                            ArrayRef<GenericTypeParamType *> genericParams,
                            PotentialArchetype *pa);

public:
  /// \brief Resolve the given type to the potential archetype it names.
  ///
  /// This routine will synthesize nested types as required to refer to a
  /// potential archetype, even in cases where no requirement specifies the
  /// requirement for such an archetype. FIXME: The failure to include such a
  /// requirement will be diagnosed at some point later (when the types in the
  /// signature are fully resolved).
  ///
  /// For any type that cannot refer to an archetype, this routine returns null.
  PotentialArchetype *resolveArchetype(Type type);

  /// \brief Resolve the given type as far as this Builder knows how.
  ///
  /// This returns either a non-typealias potential archetype or a Type, if \c
  /// type is concrete.
  // FIXME: the hackTypeFromGenericTypeAlias is just temporarily patching over
  // problems with generic typealiases (see the comment on the ResolvedType
  // function)
  ResolvedType resolve(UnresolvedType type,
                       bool hackTypeFromGenericTypeAlias = false);

  /// \brief Dump all of the requirements, both specified and inferred.
  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(),
      "only for use within the debugger");

  /// Dump all of the requirements to the given output stream.
  void dump(llvm::raw_ostream &out);
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
    TypeDecl *assocTypeOrAlias;

    /// The generic parameter key for a root.
    GenericParamKey genericParam;

    PAIdentifier(Identifier name) : name(name) { }

    PAIdentifier(AssociatedTypeDecl *assocType)
      : assocTypeOrAlias(assocType) { }

    PAIdentifier(TypeAliasDecl *typeAlias)
      : assocTypeOrAlias(typeAlias) { }

    PAIdentifier(GenericParamKey genericParam) : genericParam(genericParam) { }
  } identifier;

  /// \brief The representative of the equivalence class of potential archetypes
  /// to which this potential archetype belongs, or (for the representative)
  /// the equivalence class itself.
  mutable llvm::PointerUnion<PotentialArchetype *, EquivalenceClass *>
    representativeOrEquivClass;

  /// Same-type constraints between this potential archetype and any other
  /// archetype in its equivalence class.
  llvm::MapVector<PotentialArchetype *, const RequirementSource *>
    SameTypeConstraints;

  /// \brief The superclass of this archetype, if specified.
  Type Superclass;

  /// The source of the superclass requirement.
  const RequirementSource *SuperclassSource = nullptr;

  /// \brief The list of protocols to which this archetype will conform.
  llvm::MapVector<ProtocolDecl *, const RequirementSource *> ConformsTo;

  /// \brief The layout constraint of this archetype, if specified.
  LayoutConstraint Layout;

  /// The source of the layout constraint requirement.
  const RequirementSource *LayoutSource = nullptr;

  /// \brief The set of nested types of this archetype.
  ///
  /// For a given nested type name, there may be multiple potential archetypes
  /// corresponding to different associated types (from different protocols)
  /// that share a name.
  llvm::MapVector<Identifier, llvm::TinyPtrVector<PotentialArchetype *>>
    NestedTypes;

  /// The concrete type to which a this potential archetype has been
  /// constrained.
  Type ConcreteType;

  /// The source of the concrete type requirement, if one was written
  /// on this potential archetype.
  const RequirementSource *ConcreteTypeSource = nullptr;

  /// Whether this is an unresolved nested type.
  unsigned isUnresolvedNestedType : 1;

  /// \brief Recursively conforms to itself.
  unsigned IsRecursive : 1;

  /// Whether this potential archetype is invalid, e.g., because it could not
  /// be resolved.
  unsigned Invalid : 1;

  /// Whether we have detected recursion during the substitution of
  /// the concrete type.
  unsigned RecursiveConcreteType : 1;

  /// Whether we have detected recursion during the substitution of
  /// the superclass type.
  unsigned RecursiveSuperclassType : 1;

  /// Whether we have diagnosed a rename.
  unsigned DiagnosedRename : 1;

  /// If we have renamed this (nested) type due to typo correction,
  /// the old name.
  Identifier OrigName;

  /// \brief Construct a new potential archetype for an unresolved
  /// associated type.
  PotentialArchetype(PotentialArchetype *parent, Identifier name)
    : parentOrBuilder(parent), identifier(name), isUnresolvedNestedType(true),
      IsRecursive(false), Invalid(false),
      RecursiveConcreteType(false), RecursiveSuperclassType(false),
      DiagnosedRename(false)
  { 
    assert(parent != nullptr && "Not an associated type?");
  }

  /// \brief Construct a new potential archetype for an associated type.
  PotentialArchetype(PotentialArchetype *parent, AssociatedTypeDecl *assocType)
    : parentOrBuilder(parent), identifier(assocType),
      isUnresolvedNestedType(false), IsRecursive(false), Invalid(false),
      RecursiveConcreteType(false),
      RecursiveSuperclassType(false), DiagnosedRename(false)
  {
    assert(parent != nullptr && "Not an associated type?");
  }

  /// \brief Construct a new potential archetype for a type alias.
  PotentialArchetype(PotentialArchetype *parent, TypeAliasDecl *typeAlias)
    : parentOrBuilder(parent), identifier(typeAlias),
      isUnresolvedNestedType(false),
      IsRecursive(false), Invalid(false),
      RecursiveConcreteType(false),
      RecursiveSuperclassType(false), DiagnosedRename(false)
  {
    assert(parent != nullptr && "Not an associated type?");
  }

  /// \brief Construct a new potential archetype for a generic parameter.
  PotentialArchetype(GenericSignatureBuilder *builder, GenericParamKey genericParam)
    : parentOrBuilder(builder), identifier(genericParam),
      isUnresolvedNestedType(false),
      IsRecursive(false), Invalid(false),
      RecursiveConcreteType(false), RecursiveSuperclassType(false),
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

    return dyn_cast<AssociatedTypeDecl>(identifier.assocTypeOrAlias);
  }

  /// Resolve the potential archetype to the given associated type.
  void resolveAssociatedType(AssociatedTypeDecl *assocType,
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

    return identifier.assocTypeOrAlias->getName();
  }

  /// Retrieve the type alias.
  TypeAliasDecl *getTypeAliasDecl() const {
    assert(getParent() && "not a nested type");
    if (isUnresolvedNestedType)
      return nullptr;

    return dyn_cast<TypeAliasDecl>(identifier.assocTypeOrAlias);
  }

  /// Retrieve the set of protocols to which this type conforms.
  llvm::MapVector<ProtocolDecl *, const RequirementSource *> &
  getConformsTo() {
    return ConformsTo;
  }

  /// Add a conformance to this potential archetype.
  ///
  /// \returns true if the conformance was new, false if it already existed.
  bool addConformance(ProtocolDecl *proto, bool updateExistingSource,
                      const RequirementSource *source,
                      GenericSignatureBuilder &builder);

  /// Retrieve the superclass of this archetype.
  Type getSuperclass() const { return Superclass; }

  /// Retrieve the requirement source for the superclass requirement.
  const RequirementSource *getSuperclassSource() const {
    return SuperclassSource;
  } 

  /// Retrieve the layout constraint of this archetype.
  LayoutConstraint getLayout() const { return Layout; }

  /// Retrieve the requirement source for the layout constraint requirement.
  const RequirementSource *getLayoutSource() const {
    return LayoutSource;
  }

  /// Retrieve the set of nested types.
  const llvm::MapVector<Identifier, llvm::TinyPtrVector<PotentialArchetype *>> &
  getNestedTypes() const{
    return NestedTypes;
  }

  /// \brief Determine the nesting depth of this potential archetype, e.g.,
  /// the number of associated type references.
  unsigned getNestingDepth() const;

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
  llvm::iterator_range<
    std::vector<std::pair<PotentialArchetype *, const RequirementSource *>>
       ::const_iterator>
  getSameTypeConstraints() const {
    return llvm::make_range(SameTypeConstraints.begin(),
                            SameTypeConstraints.end());
  }

  /// Retrieve the concrete type source as written on this potential archetype.
  const RequirementSource *getConcreteTypeSourceAsWritten() const {
    return ConcreteTypeSource;
  }

  /// Find a source of the same-type constraint that maps this potential
  /// archetype to a concrete type somewhere in the equivalence class of this
  /// type.
  const RequirementSource *findAnyConcreteTypeSourceAsWritten() const;

  /// \brief Retrieve (or create) a nested type with the given name.
  PotentialArchetype *getNestedType(Identifier Name,
                                    GenericSignatureBuilder &builder);

  /// \brief Retrieve (or create) a nested type with a known associated type.
  PotentialArchetype *getNestedType(AssociatedTypeDecl *assocType,
                                    GenericSignatureBuilder &builder);

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

} // end namespace swift

#endif
