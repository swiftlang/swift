//===--- ArchetypeBuilder.h - Generic Archetype Builder ---------*- C++ -*-===//
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
// Support for collecting a set of generic requirements, both explicitly stated
// and inferred, and computing the archetypes and required witness tables from
// those requirements.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ARCHETYPEBUILDER_H
#define SWIFT_ARCHETYPEBUILDER_H

#include "swift/AST/Identifier.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <functional>
#include <memory>

namespace swift {

class AbstractTypeParamDecl;
class ArchetypeType;
class AssociatedTypeDecl;
class DeclContext;
class DependentMemberType;
class GenericParamList;
class GenericSignature;
class GenericTypeParamDecl;
class GenericTypeParamType;
class LazyResolver;
class ModuleDecl;
class Pattern;
class ProtocolConformance;
class ProtocolDecl;
class Requirement;
class RequirementRepr;
class SILModule;
class SourceLoc;
class Type;
class TypeRepr;
class ASTContext;
class DiagnosticEngine;

/// Describes how a requirement was determined.
class RequirementSource {
public:
  enum Kind : unsigned char {
    /// The requirement was explicitly stated in the generic parameter
    /// clause.
    Explicit,
    /// The requirement was inferred from the function's parameter or
    /// result types.
    Inferred,

    /// The requirement was part of a protocol requirement on an
    /// associated type.
    ///
    /// These are dropped when building the GenericSignature.
    Protocol,

    /// The requirement is redundant with some other requirement.
    ///
    /// These are dropped when building the GenericSignature.
    Redundant,

    /// The requirement is redundant due to the superclass conforming to one
    /// of the protocols.
    ///
    /// These are dropped when building the GenericSignature.
    Inherited,
  };

  RequirementSource(Kind kind, SourceLoc loc) : StoredKind(kind), Loc(loc) { }

  /// Retrieve the kind of requirement source.
  Kind getKind() const { return StoredKind; }

  /// Set the kind of the requirement source.
  void setKind(Kind kind) { StoredKind = kind; }

  /// Retrieve the source location at which the requirement originated.
  SourceLoc getLoc() const { return Loc; }

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(SourceManager *srcMgr) const,
      "only for use within the debugger");

  /// Dump the requirement source.
  void dump(llvm::raw_ostream &out, SourceManager *srcMgr) const;

private:
  Kind StoredKind;
  SourceLoc Loc;
};

/// \brief Collects a set of requirements of generic parameters, both explicitly
/// stated and inferred, and determines the set of archetypes for each of
/// the generic parameters.
class ArchetypeBuilder {
public:
  /// Describes a potential archetype, which stands in for a generic parameter
  /// type or some type derived from it.
  class PotentialArchetype;

  using RequirementRHS =
      llvm::PointerUnion3<Type, PotentialArchetype *, LayoutConstraint>;

private:
  class InferRequirementsWalker;
  friend class InferRequirementsWalker;

  ASTContext &Context;
  DiagnosticEngine &Diags;
  struct Implementation;
  std::unique_ptr<Implementation> Impl;

  ArchetypeBuilder(const ArchetypeBuilder &) = delete;
  ArchetypeBuilder &operator=(const ArchetypeBuilder &) = delete;

  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype conforms to the given protocol.
  bool addConformanceRequirement(PotentialArchetype *T,
                                 ProtocolDecl *Proto,
                                 RequirementSource Source);

  bool addConformanceRequirement(PotentialArchetype *T,
                                 ProtocolDecl *Proto,
                                 RequirementSource Source,
                                llvm::SmallPtrSetImpl<ProtocolDecl *> &Visited);

public:
  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetypes are equivalent.
  bool addSameTypeRequirementBetweenArchetypes(PotentialArchetype *T1,
                                               PotentialArchetype *T2,
                                               RequirementSource Source);
  
  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype is bound to a concrete type.
  bool addSameTypeRequirementToConcrete(PotentialArchetype *T,
                                        Type Concrete,
                                        RequirementSource Source);

private:
  /// \brief Add a new superclass requirement specifying that the given
  /// potential archetype has the given type as an ancestor.
  bool addSuperclassRequirement(PotentialArchetype *T, 
                                Type Superclass,
                                RequirementSource Source);

  /// \brief Add a new same-type requirement specifying that the given potential
  /// archetypes should map to the equivalent archetype.
  bool addSameTypeRequirement(Type T1, Type T2, RequirementSource Source);

  /// Add the requirements placed on the given abstract type parameter
  /// to the given potential archetype.
  bool addAbstractTypeParamRequirements(
         AbstractTypeParamDecl *decl,
         PotentialArchetype *pa,
         RequirementSource::Kind kind,
         llvm::SmallPtrSetImpl<ProtocolDecl *> &visited);

  /// Visit all of the types that show up in the list of inherited
  /// types.
  ///
  /// \returns true if any of the invocations of \c visitor returned true.
  bool visitInherited(ArrayRef<TypeLoc> inheritedTypes,
                      llvm::function_ref<bool(Type, SourceLoc)> visitor);

  /// Visit all of the potential archetypes.
  template<typename F>
  void visitPotentialArchetypes(F f);

public:
  /// Construct a new archetype builder.
  ///
  /// \param lookupConformance Conformance-lookup routine that will be used
  /// to satisfy conformance requirements for concrete types.
  explicit ArchetypeBuilder(ASTContext &ctx,
                            std::function<GenericFunction> lookupConformance);

  ArchetypeBuilder(ArchetypeBuilder &&);
  ~ArchetypeBuilder();

  /// Retrieve the AST context.
  ASTContext &getASTContext() const { return Context; }

  /// Retrieve the conformance-lookup function used by this archetype builder.
  std::function<GenericFunction> getLookupConformanceFn() const;

  /// Retrieve the lazy resolver, if there is one.
  LazyResolver *getLazyResolver() const;

  /// Enumerate the requirements that describe the signature of this
  /// archetype builder.
  ///
  /// \param f A function object that will be passed each requirement
  /// and requirement source.
  void enumerateRequirements(llvm::function_ref<
                      void (RequirementKind kind,
                            PotentialArchetype *archetype,
                            RequirementRHS constraint,
                            RequirementSource source)> f);

private:
  PotentialArchetype *addGenericParameter(GenericTypeParamType *GenericParam,
                                          Identifier ParamName);

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
  bool addRequirement(const RequirementRepr &Req);

  /// \brief Add an already-checked requirement.
  ///
  /// Adding an already-checked requirement cannot fail. This is used to
  /// re-inject requirements from outer contexts.
  void addRequirement(const Requirement &req, RequirementSource source);

  bool addLayoutRequirement(PotentialArchetype *PAT,
                            LayoutConstraint Layout,
                            RequirementSource Source);

  /// \brief Add all of a generic signature's parameters and requirements.
  void addGenericSignature(GenericSignature *sig);

  /// \brief Build the generic signature.
  GenericSignature *getGenericSignature();

  /// \brief Build the generic environment.
  GenericEnvironment *getGenericEnvironment(GenericSignature *signature);

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
  void finalize(SourceLoc loc, bool allowConcreteGenericParams=false);

  /// Diagnose any remaining renames.
  ///
  /// \returns \c true if there were any remaining renames to diagnose.
  bool diagnoseRemainingRenames(SourceLoc loc);

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

  /// \brief Dump all of the requirements, both specified and inferred.
  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(),
      "only for use within the debugger");

  /// Dump all of the requirements to the given output stream.
  void dump(llvm::raw_ostream &out);
};

class ArchetypeBuilder::PotentialArchetype {
  /// Either the parent of this potential archetype (for an associated
  /// type) or the generic type parameter type to which this potential
  /// archetype corresponds.
  llvm::PointerUnion<PotentialArchetype*, GenericTypeParamType*> ParentOrParam;

  /// \brief The name of this potential archetype or, for an
  /// associated type, the declaration of the associated type to which
  /// this potential archetype has been resolved. Or, for a type alias,
  /// the type alias decl.
  llvm::PointerUnion3<Identifier, AssociatedTypeDecl *,
                      TypeAliasDecl *> NameOrAssociatedType;

  /// \brief The representative of the equivalent class of potential archetypes
  /// to which this potential archetype belongs.
  PotentialArchetype *Representative;

  /// \brief The source of a same-type requirement.
  Optional<RequirementSource> SameTypeSource;

  /// \brief The superclass of this archetype, if specified.
  Type Superclass;

  /// The source of the superclass requirement.
  Optional<RequirementSource> SuperclassSource;

  /// \brief The list of protocols to which this archetype will conform.
  llvm::MapVector<ProtocolDecl *, RequirementSource> ConformsTo;

  /// \brief The layout constraint of this archetype, if specified.
  LayoutConstraint Layout;

  /// The source of the layout constraint requirement.
  Optional<RequirementSource> LayoutSource;

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

  /// The equivalence class of this potential archetype.
  llvm::TinyPtrVector<PotentialArchetype *> EquivalenceClass;

  /// \brief Construct a new potential archetype for an unresolved
  /// associated type.
  PotentialArchetype(PotentialArchetype *Parent, Identifier Name)
    : ParentOrParam(Parent), NameOrAssociatedType(Name), Representative(this),
      IsRecursive(false), Invalid(false),
      RecursiveConcreteType(false), RecursiveSuperclassType(false),
      DiagnosedRename(false)
  { 
    assert(Parent != nullptr && "Not an associated type?");
    EquivalenceClass.push_back(this);
  }

  /// \brief Construct a new potential archetype for an associated type.
  PotentialArchetype(PotentialArchetype *Parent, AssociatedTypeDecl *AssocType)
    : ParentOrParam(Parent), NameOrAssociatedType(AssocType), 
      Representative(this), IsRecursive(false), Invalid(false),
      RecursiveConcreteType(false),
      RecursiveSuperclassType(false), DiagnosedRename(false)
  {
    assert(Parent != nullptr && "Not an associated type?");
    EquivalenceClass.push_back(this);
  }

  /// \brief Construct a new potential archetype for a type alias.
  PotentialArchetype(PotentialArchetype *Parent, TypeAliasDecl *TypeAlias)
    : ParentOrParam(Parent), NameOrAssociatedType(TypeAlias),
      Representative(this), IsRecursive(false), Invalid(false),
      RecursiveConcreteType(false),
      RecursiveSuperclassType(false), DiagnosedRename(false)
  {
    assert(Parent != nullptr && "Not an associated type?");
    EquivalenceClass.push_back(this);
  }

  /// \brief Construct a new potential archetype for a generic parameter.
  PotentialArchetype(GenericTypeParamType *GenericParam, 
                     Identifier Name)
    : ParentOrParam(GenericParam),
      NameOrAssociatedType(Name), Representative(this), IsRecursive(false),
      Invalid(false),
      RecursiveConcreteType(false), RecursiveSuperclassType(false),
      DiagnosedRename(false)
  {
    EquivalenceClass.push_back(this);
  }

  /// \brief Recursively build the full name.
  void buildFullName(bool forDebug, SmallVectorImpl<char> &result) const;

  /// Retrieve the generic parameter at the root of this potential archetype.
  GenericTypeParamType *getRootParam() const {
    if (auto parent = getParent())
      return parent->getRootParam();

    return getGenericParam();
  }

  /// \brief Retrieve the representative for this archetype, performing
  /// path compression on the way.
  PotentialArchetype *getRepresentative();

  friend class ArchetypeBuilder;
  friend class GenericSignature;

public:
  ~PotentialArchetype();

  /// \brief Retrieve the name of this potential archetype.
  Identifier getName() const;

  /// \brief Retrieve the full display name of this potential archetype.
  std::string getFullName() const;

  /// \brief Retrieve the debug name of this potential archetype.
  std::string getDebugName() const;

  /// Retrieve the parent of this potential archetype, which will be non-null
  /// when this potential archetype is an associated type.
  PotentialArchetype *getParent() const { 
    return ParentOrParam.dyn_cast<PotentialArchetype *>(); 
  }

  /// Retrieve the associated type to which this potential archetype
  /// has been resolved.
  AssociatedTypeDecl *getResolvedAssociatedType() const {
    assert(getParent() && "Not an associated type");
    return NameOrAssociatedType.dyn_cast<AssociatedTypeDecl *>();
  }

  /// Resolve the potential archetype to the given associated type.
  void resolveAssociatedType(AssociatedTypeDecl *assocType,
                             ArchetypeBuilder &builder);

  /// Retrieve the generic type parameter for this potential
  /// archetype, if it corresponds to a generic parameter.
  ///
  /// FIXME: We should weaken this to just a depth/index key.
  GenericTypeParamType *getGenericParam() const {
    return ParentOrParam.dyn_cast<GenericTypeParamType *>(); 
  }

  /// Determine whether this is a generic parameter.
  bool isGenericParam() const {
    return ParentOrParam.is<GenericTypeParamType *>();
  }

  /// Retrieve the generic parameter key for a potential archetype that
  /// represents this potential archetype.
  ///
  /// \pre \c isGenericParam()
  GenericParamKey getGenericParamKey() const {
    return getGenericParam();
  }

  /// Retrieve the generic parameter key for the generic parameter at the
  /// root of this potential archetype.
  GenericParamKey getRootGenericParamKey() const {
    return getRootParam();
  }

  /// Retrieve the type alias.
  TypeAliasDecl *getTypeAliasDecl() const {
    return NameOrAssociatedType.dyn_cast<TypeAliasDecl *>();
  }

  /// Retrieve the set of protocols to which this type conforms.
  const llvm::MapVector<ProtocolDecl *, RequirementSource> &
  getConformsTo() const {
    return ConformsTo;
  }

  /// Add a conformance to this potential archetype.
  ///
  /// \returns true if the conformance was new, false if it already existed.
  bool addConformance(ProtocolDecl *proto, bool updateExistingSource,
                      const RequirementSource &source,
                      ArchetypeBuilder &builder);

  /// Retrieve the superclass of this archetype.
  Type getSuperclass() const { return Superclass; }

  /// Retrieve the requirement source for the superclass requirement.
  const RequirementSource &getSuperclassSource() const {
    return *SuperclassSource;
  } 

  /// Retrieve the layout constraint of this archetype.
  LayoutConstraint getLayout() const { return Layout; }

  /// Retrieve the requirement source for the layout constraint requirement.
  const RequirementSource &getLayoutSource() const {
    return *LayoutSource;
  }

  /// Retrieve the set of nested types.
  const llvm::MapVector<Identifier, llvm::TinyPtrVector<PotentialArchetype *>> &
  getNestedTypes() const{
    return NestedTypes;
  }

  /// \brief Determine the nesting depth of this potential archetype, e.g.,
  /// the number of associated type references.
  unsigned getNestingDepth() const;

  /// Retrieve the equivalence class containing this potential archetype.
  ArrayRef<PotentialArchetype *> getEquivalenceClass() {
    return getRepresentative()->EquivalenceClass;
  }

  /// \brief Retrieve the potential archetype to be used as the anchor for
  /// potential archetype computations.
  PotentialArchetype *getArchetypeAnchor();

  /// Retrieve the source of the same-type constraint that applies to this
  /// potential archetype.
  const RequirementSource &getSameTypeSource() const {
    return *SameTypeSource;
  }

  /// \brief Retrieve (or create) a nested type with the given name.
  PotentialArchetype *getNestedType(Identifier Name,
                                    ArchetypeBuilder &builder);


  /// \brief Retrieve (or build) the type corresponding to the potential
  /// archetype within the given generic environment.
  Type getTypeInContext(ArchetypeBuilder &builder,
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
    if (Representative != this)
      return Representative->isConcreteType();

    return static_cast<bool>(ConcreteType);
  }
  
  /// Get the concrete type this potential archetype is constrained to.
  Type getConcreteType() const {
    if (Representative != this)
      return Representative->getConcreteType();
    return ConcreteType;
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
    OrigName = getName();
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

  /// Whether this potential archetype makes a better archetype anchor than
  /// the given archetype anchor.
  bool isBetterArchetypeAnchor(PotentialArchetype *other) const;

  void dump(llvm::raw_ostream &Out, SourceManager *SrcMgr,
            unsigned Indent);

  friend class ArchetypeBuilder;

private:
  bool hasConcreteTypeInPath() const;
};

} // end namespace swift

#endif
