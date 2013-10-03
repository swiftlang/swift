//===--- ArchetypeBuilder.h - Generic Archetype Builder -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Support for collecting a set of generic requirements, both explicitly stated
// and inferred, and computing the archetypes and required witness tables from
// those requirements.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Optional.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SetVector.h"
#include <functional>
#include <memory>

namespace swift {

class AbstractTypeParamDecl;
class ArchetypeType;
class AssociatedTypeDecl;
class Pattern;
class ProtocolDecl;
class RequirementRepr;
class SourceLoc;
class TranslationUnit;
class Type;
class TypeRepr;
class ASTContext;
class DiagnosticEngine;

/// \brief Collects a set of requirements of generic parameters, both explicitly
/// stated and inferred, and determines the set of archetypes for each of
/// the generic parameters.
class ArchetypeBuilder {
public:
  /// Describes a potential archetype, which stands in for a generic parameter
  /// type or some type derived from it.
  class PotentialArchetype;

private:
  class InferRequirementsWalker;
  friend class InferRequirementsWalker;

  TranslationUnit &TU;
  ASTContext &Context;
  DiagnosticEngine &Diags;
  struct Implementation;
  std::unique_ptr<Implementation> Impl;

  ArchetypeBuilder(const ArchetypeBuilder &) = delete;
  ArchetypeBuilder &operator=(const ArchetypeBuilder &) = delete;

  /// \brief Add a new conformance requirement specifying that the given
  /// potential archetype conforms to the given protocol.
  bool addConformanceRequirement(PotentialArchetype *T,
                                 ProtocolDecl *Proto);

  /// \brief Add a new superclass requirement specifying that the given
  /// potential archetype has the given type as an ancestor.
  bool addSuperclassRequirement(PotentialArchetype *T, SourceLoc ColonLoc,
                                Type Superclass);

  /// \brief Add a new same-type requirement specifying that the given potential
  /// archetypes should map to the equivalent archetype.
  bool addSameTypeRequirement(PotentialArchetype *T1,
                              SourceLoc EqualLoc,
                              PotentialArchetype *T2);

public:
  ArchetypeBuilder(TranslationUnit &tu, DiagnosticEngine &diags);

  /// Construct a new archtype builder.
  ///
  /// \param tu The translation unit in which the builder will create
  /// archetypes.
  ///
  /// \param diags The diagnostics entity to use.
  ///
  /// \param getInheritedProtocols A function that determines the set of
  /// protocols inherited from the given protocol. This produces the final
  /// results of ProtocolDecl::getProtocols().
  ///
  /// \param getConformsTo A function that determines the set of protocols
  /// to which the given type parameter conforms. The produces the final
  /// results of AbstractTypeParamDecl::getProtocols() for an associated type.
  ArchetypeBuilder(
    TranslationUnit &tu, DiagnosticEngine &diags,
    std::function<ArrayRef<ProtocolDecl *>(ProtocolDecl *)>
      getInheritedProtocols,
    std::function<ArrayRef<ProtocolDecl *>(AbstractTypeParamDecl *)>
      getConformsTo);
  ArchetypeBuilder(ArchetypeBuilder &&);
  ~ArchetypeBuilder();

  /// Retrieve the translation unit.
  TranslationUnit &getTranslationUnit() const { return TU; }

  /// \brief Add a new generic parameter for which there may be requirements.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool addGenericParameter(AbstractTypeParamDecl *GenericParam,
                           Optional<unsigned> Index = Nothing);

  /// \brief Add a new requirement.
  ///
  /// \returns true if this requirement makes the set of requirements
  /// inconsistent, in which case a diagnostic will have been issued.
  bool addRequirement(const RequirementRepr &Req);

  /// \brief Add a new, implicit conformance requirement for one of the
  /// parameters.
  bool addImplicitConformance(AbstractTypeParamDecl *Param,
                              ProtocolDecl *Proto);

  /// Infer requirements from the given type representation, recursively.
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
  ///
  /// \returns true if an error occurred, false otherwise.
  bool inferRequirements(TypeRepr *type);

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
  ///
  /// \returns true if an error occurred, false otherwise.
  bool inferRequirements(Pattern *pattern);

  /// \brief Resolve the given type to the potential archetype it names.
  ///
  /// This routine will synthesize nested types as required to refer to a
  /// potential archetype, even in cases where no requirement specifies the
  /// requirement for such an archetype. FIXME: The failure to include such a
  /// requirement will be diagnosed at some point later (when the types in the
  /// signature are fully resolved).
  ///
  /// For any type that cannot refer to an archetype, this routine returns null.
  PotentialArchetype *resolveType(Type type);

  /// \brief Assign archetypes to each of the generic parameters and all
  /// of their associated types, recursively.
  ///
  /// This operation should only be performed after all generic parameters and
  /// requirements have been added to the builder. It is non-reversible.
  void assignArchetypes();

  /// \brief Retrieve the archetype that corresponds to the given generic
  /// parameter.
  ArchetypeType *getArchetype(AbstractTypeParamDecl *GenericParam) const;

  /// \brief Retrieve the array of all of the archetypes produced during
  /// archetype assignment. The 'primary' archetypes will occur first in this
  /// list.
  ArrayRef<ArchetypeType *> getAllArchetypes();

  // FIXME: Compute the set of 'extra' witness tables needed to express this
  // requirement set.

  /// \brief Dump all of the requirements, both specified and inferred.
  void dump();
};

class ArchetypeBuilder::PotentialArchetype {
  /// \brief The parent of this potential archetype, which will be non-null
  /// when this potential archetype is an associated type.
  PotentialArchetype *Parent;

  /// \brief The name of this potential archetype.
  Identifier Name;

  /// \brief The index of the computed archetype.
  Optional<unsigned> Index;

  /// \brief The representative of the equivalent class of potential archetypes
  /// to which this potential archetype belongs.
  PotentialArchetype *Representative;

  /// \brief The superclass of this archetype, if specified.
  Type Superclass;

  /// \brief The list of protocols to which this archetype will conform.
  llvm::SetVector<ProtocolDecl *, SmallVector<ProtocolDecl *, 4>> ConformsTo;

  /// \brief The set of nested typed stores within this archetype.
  llvm::DenseMap<Identifier, PotentialArchetype *> NestedTypes;

  /// \brief The actual archetype, once it has been assigned.
  ArchetypeType *Archetype;

  /// \brief Construct a new potential archetype.
  PotentialArchetype(PotentialArchetype *Parent, Identifier Name,
                     Optional<unsigned> Index = Nothing)
    : Parent(Parent), Name(Name), Index(Index), Representative(this),
      Archetype(nullptr) { }

  /// \brief Recursively build the full name.
  void buildFullName(SmallVectorImpl<char> &Result) const;

public:
  ~PotentialArchetype();

  /// \brief Retrieve the name of this potential archetype.
  StringRef getName() const { return Name.str(); }

  /// \brief Retrieve the full display name of this potential archetype.
  std::string getFullName() const;

  /// Retrieve the parent of this potential archetype, which will be non-null
  /// when this potential archetype is an associated type.
  PotentialArchetype *getParent() const { return Parent; }

  /// Retrieve the set of protocols to which this type conforms.
  ArrayRef<ProtocolDecl *> getConformsTo() const {
    return llvm::makeArrayRef(ConformsTo.begin(), ConformsTo.end());
  }

  /// Retrieve the superclass of this archetype.
  Type getSuperclass() const { return Superclass; }

  /// Retrieve the set of nested types.
  const llvm::DenseMap<Identifier, PotentialArchetype*> &getNestedTypes() const{
    return NestedTypes;
  }

  /// \brief Determine the nesting depth of this potential archetype, e.g.,
  /// the number of associated type references.
  unsigned getNestingDepth() const;

  /// \brief Retrieve the representative for this archetype, performing
  /// path compression on the way.
  PotentialArchetype *getRepresentative();

  /// \brief Retrieve (or create) a nested type with the given name.
  PotentialArchetype *getNestedType(Identifier Name);

  /// \brief Retrieve (or build) the archetype corresponding to the potential
  /// archetype.
  ArchetypeType *getArchetype(AssociatedTypeDecl * /*nullable*/ rootAssocTy,
                              TranslationUnit &tu);

  /// Retrieve the associated type declaration for a given nested type.
  AssociatedTypeDecl *getAssociatedType(TranslationUnit &tu, Identifier name);

  void dump(llvm::raw_ostream &Out, unsigned Indent);

  friend class ArchetypeBuilder;
};

} // end namespace swift
