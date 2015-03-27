//===--- ProtocolConformance.cpp - AST Protocol Conformance -----*- C++ -*-===//
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
// This file implements the protocol conformance data structures.
//
//===----------------------------------------------------------------------===//
#include "swift/Basic/Fallthrough.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/Substitution.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeWalker.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

void *ProtocolConformance::operator new(size_t bytes, ASTContext &context,
                                        AllocationArena arena,
                                        unsigned alignment) {
  return context.Allocate(bytes, alignment, arena);

}

#define CONFORMANCE_SUBCLASS_DISPATCH(Method, Args)                          \
switch (getKind()) {                                                         \
  case ProtocolConformanceKind::Normal:                                      \
    static_assert(&ProtocolConformance::Method !=                            \
                    &NormalProtocolConformance::Method,                      \
                  "Must override NormalProtocolConformance::" #Method);      \
    return cast<NormalProtocolConformance>(this)->Method Args;               \
  case ProtocolConformanceKind::Specialized:                                 \
    static_assert(&ProtocolConformance::Method !=                            \
                    &SpecializedProtocolConformance::Method,                 \
                  "Must override SpecializedProtocolConformance::" #Method); \
    return cast<SpecializedProtocolConformance>(this)->Method Args;          \
  case ProtocolConformanceKind::Inherited:                                   \
    static_assert(&ProtocolConformance::Method !=                            \
                    &InheritedProtocolConformance::Method,                   \
                  "Must override InheritedProtocolConformance::" #Method);   \
    return cast<InheritedProtocolConformance>(this)->Method Args;            \
}                                                                            \
llvm_unreachable("bad ProtocolConformanceKind");

/// Get the protocol being conformed to.
ProtocolDecl *ProtocolConformance::getProtocol() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getProtocol, ())
}

DeclContext *ProtocolConformance::getDeclContext() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getDeclContext, ())
}

/// Retrieve the state of this conformance.
ProtocolConformanceState ProtocolConformance::getState() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getState, ())
}

bool
ProtocolConformance::hasTypeWitness(AssociatedTypeDecl *assocType,
                                    LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(hasTypeWitness, (assocType, resolver));
}

const Substitution &
ProtocolConformance::getTypeWitness(AssociatedTypeDecl *assocType, 
                                    LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getTypeWitness, (assocType, resolver))
}

Type
ProtocolConformance::getTypeWitnessByName(Type type,
                                          ProtocolConformance *conformance,
                                          Identifier name,
                                          LazyResolver *resolver) {
  // For an archetype, retrieve the nested type with the appropriate
  // name. There are no conformance tables.
  if (auto archetype = type->getAs<ArchetypeType>()) {
    return archetype->getNestedTypeValue(name);
  }

  // Find the named requirement.
  AssociatedTypeDecl *assocType = nullptr;
  auto members = conformance->getProtocol()->lookupDirect(name);
  for (auto member : members) {
    assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (assocType)
      break;
  }

  if (!assocType)
    return nullptr;
  
  assert(conformance && "Missing conformance information");
  if (!conformance->hasTypeWitness(assocType, resolver)) {
    return nullptr;
  }
  return conformance->getTypeWitness(assocType, resolver).getReplacement();
}

ConcreteDeclRef ProtocolConformance::getWitness(ValueDecl *requirement,
                                               LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getWitness, (requirement, resolver))
}

const InheritedConformanceMap &
ProtocolConformance::getInheritedConformances() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getInheritedConformances, ())
}

/// Determine whether the witness for the given requirement
/// is either the default definition or was otherwise deduced.
bool ProtocolConformance::usesDefaultDefinition(ValueDecl *requirement) const {
  CONFORMANCE_SUBCLASS_DISPATCH(usesDefaultDefinition, (requirement))
}

GenericParamList *ProtocolConformance::getGenericParams() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Inherited:
  case ProtocolConformanceKind::Normal:
    // If we have a normal or inherited protocol conformance, look for its
    // generic parameters.
    return getDeclContext()->getGenericParamsOfContext();

  case ProtocolConformanceKind::Specialized:
    // If we have a specialized protocol conformance, since we do not support
    // currently partial specialization, we know that it can not have any open
    // type variables.
    return nullptr;
  }
}

Type ProtocolConformance::getInterfaceType() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Normal:
  case ProtocolConformanceKind::Inherited:
    return getDeclContext()->getDeclaredInterfaceType();

  case ProtocolConformanceKind::Specialized:
    // Assume a specialized conformance is fully applied.
    return getType();
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

GenericSignature *ProtocolConformance::getGenericSignature() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Inherited:
  case ProtocolConformanceKind::Normal:
    // If we have a normal or inherited protocol conformance, look for its
    // generic signature.
    return getDeclContext()->getGenericSignatureOfContext();

  case ProtocolConformanceKind::Specialized:
    // If we have a specialized protocol conformance, since we do not support
    // currently partial specialization, we know that it can not have any open
    // type variables.
    return nullptr;
  }
}

bool NormalProtocolConformance::hasTypeWitness(AssociatedTypeDecl *assocType,
                                               LazyResolver *resolver) const {
  if (TypeWitnesses.find(assocType) != TypeWitnesses.end()) {
    return true;
  }
  if (resolver) {
    resolver->resolveTypeWitness(this, assocType);
    if (TypeWitnesses.find(assocType) != TypeWitnesses.end()) {
      return true;
    }
  }
  return false;
}

const Substitution &NormalProtocolConformance::getTypeWitness(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  auto known = TypeWitnesses.find(assocType);
  if (known == TypeWitnesses.end()) {
    assert(resolver && "Unable to resolve type witness");
    resolver->resolveTypeWitness(this, assocType);
    known = TypeWitnesses.find(assocType);
    assert(known != TypeWitnesses.end() && "Didn't resolve witness?");
  }

  return known->second;
}

void NormalProtocolConformance::setTypeWitness(
       AssociatedTypeDecl *assocType,
       const Substitution &substitution) const {
  assert(getProtocol() == cast<ProtocolDecl>(assocType->getDeclContext()) &&
         "associated type in wrong protocol");
  assert(TypeWitnesses.count(assocType) == 0 && "Type witness already known");
  assert(!isComplete() && "Conformance already complete?");
  TypeWitnesses[assocType] = substitution;
}

  /// Retrieve the value witness corresponding to the given requirement.
ConcreteDeclRef NormalProtocolConformance::getWitness(
                  ValueDecl *requirement, 
                  LazyResolver *resolver) const {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
  auto known = Mapping.find(requirement);
  if (known == Mapping.end()) {
    assert(resolver && "Unable to resolve witness without resolver");
    resolver->resolveWitness(this, requirement);
    known = Mapping.find(requirement);
  }
  if (known != Mapping.end()) {
    return known->second;
  } else {
    assert(!isComplete() && "Resolver did not resolve requirement");
    return ConcreteDeclRef();
  }
}

void NormalProtocolConformance::setWitness(ValueDecl *requirement,
                                           ConcreteDeclRef witness) const {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
  assert(getProtocol() == cast<ProtocolDecl>(requirement->getDeclContext()) &&
         "requirement in wrong protocol");
  assert(Mapping.count(requirement) == 0 && "Witness already known");
  assert(!isComplete() && "Conformance already complete?");
  Mapping[requirement] = witness;
}

SpecializedProtocolConformance::SpecializedProtocolConformance(
    Type conformingType,
    ProtocolConformance *genericConformance,
    ArrayRef<Substitution> substitutions)
  : ProtocolConformance(ProtocolConformanceKind::Specialized, conformingType),
    GenericConformance(genericConformance),
    GenericSubstitutions(substitutions)
{
  assert(genericConformance->getKind() != ProtocolConformanceKind::Specialized);
}

bool SpecializedProtocolConformance::hasTypeWitness(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  return TypeWitnesses.find(assocType) != TypeWitnesses.end() ||
         GenericConformance->hasTypeWitness(assocType, resolver);
}

const Substitution &SpecializedProtocolConformance::getTypeWitness(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  // If we've already created this type witness, return it.
  auto known = TypeWitnesses.find(assocType);
  if (known != TypeWitnesses.end()) {
    return known->second;
  }

  // Otherwise, perform substitutions to create this witness now.
  TypeSubstitutionMap substitutionMap = GenericConformance->getGenericParams()
    ->getSubstitutionMap(GenericSubstitutions);
  
  auto &genericWitness
    = GenericConformance->getTypeWitness(assocType, resolver);
  auto conformingDC = getDeclContext();
  auto conformingModule = conformingDC->getParentModule();
  auto specializedType
    = genericWitness.getReplacement().subst(conformingModule,
                                       substitutionMap,
                                       /*ignoreMissing=*/false,
                                       resolver);

  // If the type witness was unchanged, just copy it directly.
  if (specializedType.getPointer() == genericWitness.getReplacement().getPointer()) {
    TypeWitnesses[assocType] = genericWitness;
    return TypeWitnesses[assocType];
  }

  // Gather the conformances for the type witness. These should never fail.
  SmallVector<ProtocolConformance *, 4> conformances;
  auto archetype = genericWitness.getArchetype();
  for (auto proto : archetype->getConformsTo()) {
    auto conforms = conformingModule->lookupConformance(specializedType, proto,
                                                        resolver);
    assert((conforms.getInt() == ConformanceKind::Conforms ||
            specializedType->is<TypeVariableType>() ||
            specializedType->is<GenericTypeParamType>() ||
            specializedType->is<DependentMemberType>()) &&
           "Improperly checked substitution");
    conformances.push_back(conforms.getPointer());
  }

  // Form the substitution.
  auto &ctx = assocType->getASTContext();
  TypeWitnesses[assocType] = Substitution{archetype, specializedType,
                                          ctx.AllocateCopy(conformances)};
  return TypeWitnesses[assocType];
}

ConcreteDeclRef
SpecializedProtocolConformance::getWitness(ValueDecl *requirement,
                                           LazyResolver *resolver) const {
  // FIXME: Apply substitutions here!
  return GenericConformance->getWitness(requirement, resolver);
}

const NormalProtocolConformance *
ProtocolConformance::getRootNormalConformance() const {
  const ProtocolConformance *C = this;
  while (!isa<NormalProtocolConformance>(C)) {
    switch (C->getKind()) {
    case ProtocolConformanceKind::Normal:
      llvm_unreachable("should have broken out of loop");
    case ProtocolConformanceKind::Inherited:
      C = cast<InheritedProtocolConformance>(C)
          ->getInheritedConformance();
      break;
    case ProtocolConformanceKind::Specialized:
      C = cast<SpecializedProtocolConformance>(C)
        ->getGenericConformance();
      break;
    }
  }
  return cast<NormalProtocolConformance>(C);
}

ProtocolConformance *ProtocolConformance::subst(Module *module,
                                      Type substType,
                                      ArrayRef<Substitution> subs,
                                      TypeSubstitutionMap &subMap,
                                      ArchetypeConformanceMap &conformanceMap) {
  if (getType()->isEqual(substType))
    return this;
  
  switch (getKind()) {
  case ProtocolConformanceKind::Normal:
    if (substType->isSpecialized()) {
      assert(getType()->isSpecialized()
             && "substitution mapped non-specialized to specialized?!");
      assert(getType()->getNominalOrBoundGenericNominal()
               == substType->getNominalOrBoundGenericNominal()
             && "substitution mapped to different nominal?!");
      return module->getASTContext()
        .getSpecializedConformance(substType, this,
                           substType->gatherAllSubstitutions(module, nullptr));
    }
    assert(substType->isEqual(getType())
           && "substitution changed non-specialized type?!");
    return this;
      
  case ProtocolConformanceKind::Inherited: {
    // Substitute the base.
    auto inheritedConformance
      = cast<InheritedProtocolConformance>(this)->getInheritedConformance();
    ProtocolConformance *newBase;
    if (inheritedConformance->getType()->isSpecialized()) {
      newBase = inheritedConformance->subst(module, substType, subs, subMap,
                                            conformanceMap);
    } else {
      newBase = inheritedConformance;
    }

    return module->getASTContext()
      .getInheritedConformance(substType, newBase);
  }
  case ProtocolConformanceKind::Specialized: {
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    SmallVector<Substitution, 8> newSubs;
    newSubs.reserve(spec->getGenericSubstitutions().size());
    for (auto &sub : spec->getGenericSubstitutions())
      newSubs.push_back(sub.subst(module, subs, subMap, conformanceMap));
    
    auto ctxNewSubs = module->getASTContext().AllocateCopy(newSubs);
    
    return module->getASTContext()
      .getSpecializedConformance(substType, spec->getGenericConformance(),
                                 ctxNewSubs);
  }
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

ProtocolConformance *
ProtocolConformance::getInheritedConformance(ProtocolDecl *protocol) const {
  // Preserve specialization through this operation by peeling off the
  // substitutions from a specialized conformance so we can apply them later.
  const ProtocolConformance *unspecialized;
  ArrayRef<Substitution> subs;
  switch (getKind()) {
  case ProtocolConformanceKind::Specialized: {
    auto spec = cast<SpecializedProtocolConformance>(this);
    unspecialized = spec->getGenericConformance();
    subs = spec->getGenericSubstitutions();
    break;
  }
    
  case ProtocolConformanceKind::Normal:
  case ProtocolConformanceKind::Inherited:
    unspecialized = this;
    break;
  }
  
  
  ProtocolConformance *foundInherited;
  
  // Search for the inherited conformance among our immediate parents.
  auto &inherited = unspecialized->getInheritedConformances();
  auto known = inherited.find(protocol);
  if (known != inherited.end()) {
    foundInherited = known->second;
    goto found_inherited;
  }

  // If not there, the inherited conformance must be available through one of
  // our parents.
  for (auto &inheritedMapping : inherited)
    if (inheritedMapping.first->inheritsFrom(protocol)) {
      foundInherited = inheritedMapping.second->
        getInheritedConformance(protocol);
      goto found_inherited;
    }

  llvm_unreachable("Can't find the inherited conformance.");

found_inherited:
  
  // Specialize the inherited conformance, if necessary.
  if (!subs.empty()) {
    TypeSubstitutionMap subMap;
    ArchetypeConformanceMap conformanceMap;

    // Fill in the substitution and conformance maps.
    // FIXME: Unfortunate reliance on Substitution::Archetype here.
    for (auto sub : subs) {
      auto arch = sub.getArchetype();
      conformanceMap[arch] = sub.getConformances();
      if (arch->isPrimary())
        subMap[arch] = sub.getReplacement();
    }
    return foundInherited->subst(getDeclContext()->getParentModule(),
                                 getType(), subs, subMap, conformanceMap);
  }
  assert((getType()->isEqual(foundInherited->getType()) ||
          foundInherited->getType()->isSuperclassOf(getType(), nullptr))
         && "inherited conformance does not match type");
  return foundInherited;
}

#pragma mark Protocol conformance lookup

class swift::ConformanceLookupTable {
  /// Describes the stage at which a particular nominal type or
  /// extension's conformances has been processed.
  enum class ConformanceStage : unsigned char {
    /// The explicit conformances have been recorded in the lookup table.
    RecordedExplicit,

    /// Conformances from the superclass have been inherited.
    Inherited,

    /// The explicit conformances have been expanded out to include
    /// the conformances they imply.
    ExpandedImplied,

    /// The complete set of conformances have been fully resolved to
    /// assign conformances, diagnose conflicts, etc.
    Resolved,
  };

  /// The number of conformance stages.
  static const unsigned NumConformanceStages = 4;

  /// An entry in the last-processed list, which contains a pointer to
  /// the last extension that was processed at a particular stage (or
  /// nullptr if no extensions have been processed) and indicates
  /// whether the nominal type declaration itself has been processed
  /// at that stage.
  typedef llvm::PointerIntPair<ExtensionDecl *, 1, bool> LastProcessedEntry;

  /// Array indicating how far we have gotten in processing the
  /// nominal type and list of extensions for each stage of
  /// conformance checking.
  LastProcessedEntry LastProcessed[NumConformanceStages];

  struct ConformanceEntry;

  /// Describes the "source" of a conformance, indicating where the
  /// conformance came from.
  class ConformanceSource {
    llvm::PointerIntPair<void *, 2, ConformanceEntryKind> Storage;

    ConformanceSource(void *ptr, ConformanceEntryKind kind) 
      : Storage(ptr, kind) { }

  public:
    /// Create an inherited conformance.
    ///
    /// The given class will have an inherited conformance for the
    /// requested protocol.
    static ConformanceSource forInherited(ClassDecl *classDecl) {
      return ConformanceSource(classDecl, ConformanceEntryKind::Inherited);
    }

    /// Create an explicit conformance.
    ///
    /// The given declaration context (nominal type declaration or
    /// extension thereof) explicitly specifies conformance to the
    /// protocol.
    static ConformanceSource forExplicit(DeclContext *dc) {
      return ConformanceSource(dc, ConformanceEntryKind::Explicit);
    }

    /// Create an implied conformance.
    ///
    /// Conformance to the protocol is implied by the given
    /// conformance entry. The chain of conformance entries will
    /// eventually terminate in a non-implied conformance.
    static ConformanceSource forImplied(ConformanceEntry *entry) {
      return ConformanceSource(entry, ConformanceEntryKind::Implied);
    }

    /// Create a synthesized conformance.
    ///
    /// The given nominal type declaration will get a synthesized
    /// conformance to the requested protocol.
    static ConformanceSource forSynthesized(NominalTypeDecl *typeDecl) {
      return ConformanceSource(typeDecl, ConformanceEntryKind::Synthesized);
    }

    /// Retrieve the kind of conformance formed from this source.
    ConformanceEntryKind getKind() const { return Storage.getInt(); }

    /// For an inherited conformance, retrieve the class declaration
    /// for the inheriting class.
    ClassDecl *getInheritingClass() const {
      assert(getKind() == ConformanceEntryKind::Inherited);
      return static_cast<ClassDecl *>(Storage.getPointer());      
    }

    /// For an explicit conformance, retrieve the declaration context
    /// that specifies the conformance.
    DeclContext *getExplicitDeclContext() const {
      assert(getKind() == ConformanceEntryKind::Explicit);
      return static_cast<DeclContext *>(Storage.getPointer());      
    }

    /// For a synthesized conformance, retrieve the nominal type decl
    /// that will receive the conformance.
    ConformanceEntry *getImpliedSource() const {
      assert(getKind() == ConformanceEntryKind::Implied);
      return static_cast<ConformanceEntry *>(Storage.getPointer());
    }

    /// For a synthesized conformance, retrieve the nominal type decl
    /// that will receive the conformance.
    NominalTypeDecl *getSynthesizedDecl() const {
      assert(getKind() == ConformanceEntryKind::Synthesized);
      return static_cast<NominalTypeDecl *>(Storage.getPointer());
    }

    /// Get the declaration context that this conformance will be
    /// associated with.
    DeclContext *getDeclContext() const;
  };

  /// An entry in the conformance table.
  struct ConformanceEntry {
    /// The source location within the current context where the
    /// protocol conformance was specified.
    SourceLoc Loc;

    /// If this conformance entry has been superseded, the conformance
    /// that superseded it.
    ConformanceEntry *SupersededBy = nullptr;

    /// The source of this conformance entry , which is either a
    /// DeclContext (for an explicitly-specified conformance) or a
    /// link to the conformance that implied this conformance.
    ConformanceSource Source;

    /// Either the protocol to be resolved or the resolved protocol conformance.
    llvm::PointerUnion<ProtocolDecl *, ProtocolConformance *> Conformance;

    ConformanceEntry(SourceLoc loc, ProtocolDecl *protocol,
                     ConformanceSource source)
      : Loc(loc), Source(source), Conformance(protocol) { }

    /// Retrieve the location at which this conformance was declared
    /// or synthesized.
    SourceLoc getLoc() const { return Loc; }

    /// Whether this conformance is already "fixed" and cannot be superseded.
    bool isFixed() const {
      // If a conformance has been assigned, it cannot be superseded.
      if (getConformance())
        return true;

      // Otherwise, only inherited conformances are fixed.
      switch (getKind()) {
      case ConformanceEntryKind::Explicit:
      case ConformanceEntryKind::Implied:
      case ConformanceEntryKind::Synthesized:
        return false;

      case ConformanceEntryKind::Inherited:
        return true;
      }
    }

    /// Whether this protocol conformance was superseded by another
    /// conformance.
    bool isSuperseded() const { return SupersededBy != nullptr; }

    /// Retrieve the conformance entry that superseded this one.
    ConformanceEntry *getSupersededBy() const { return SupersededBy; }

    /// Note that this conformance entry was superseded by the given
    /// entry.
    void markSupersededBy(ConformanceLookupTable &table,
                          ConformanceEntry *entry,
                          bool diagnose);

    /// Determine the kind of conformance.
    ConformanceEntryKind getKind() const {
      return Source.getKind();
    }

    /// Retrieve the declaration context associated with this conformance.
    DeclContext *getDeclContext() const {
      return Source.getDeclContext();
    }

    /// Retrieve the protocol to which this conformance entry refers.
    ProtocolDecl *getProtocol() const {
      if (auto protocol = Conformance.dyn_cast<ProtocolDecl *>())
        return protocol;

      return Conformance.get<ProtocolConformance *>()->getProtocol();
    }

    /// Retrieve the conformance for this entry, if it has one.
    ProtocolConformance *getConformance() const {
      return Conformance.dyn_cast<ProtocolConformance *>();
    }

    /// Retrieve the conformance entry where the conformance was
    /// declared.
    const ConformanceEntry *getDeclaredConformance() const {
      if (Source.getKind() == ConformanceEntryKind::Implied)
        return Source.getImpliedSource()->getDeclaredConformance();

      return this;
    }

    /// Retrieve the source location of the place where the
    /// conformance was introduced, e.g., an explicit conformance or
    /// the point at which a subclass inherits a conformance from its
    /// superclass.
    SourceLoc getDeclaredLoc() const {
      if (Source.getKind() == ConformanceEntryKind::Implied)
        return Source.getImpliedSource()->getDeclaredLoc();

      return Loc;
    }

    // Only allow allocation of conformance entries using the
    // allocator in ASTContext.
    void *operator new(size_t Bytes, ASTContext &C,
                       unsigned Alignment = alignof(ConformanceEntry)) {
      return C.Allocate(Bytes, Alignment);
    }

    LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const LLVM_ATTRIBUTE_USED,
        "only for use within the debugger");
    void dump(raw_ostream &os, unsigned indent = 0) const;
  };

  /// The set of conformance entries for a given protocol.
  typedef llvm::TinyPtrVector<ConformanceEntry *> ConformanceEntries;

  /// The type of the internal conformance table.
  typedef llvm::DenseMap<ProtocolDecl *, ConformanceEntries> ConformanceTable;

  /// The conformance table.
  ConformanceTable Conformances;

  typedef llvm::SmallVector<ProtocolDecl *, 2> ProtocolList;

  /// List of all of the protocols to which a given context declares
  /// conformance, both explicitly and implicitly.
  llvm::DenseMap<DeclContext *, SmallVector<ConformanceEntry *, 4>>
    AllConformances;

  /// The complete set of diagnostics about erroneously superseded
  /// protocol conformances.
  llvm::SmallDenseMap<DeclContext *, std::vector<ConformanceEntry *> >
    AllSupersededDiagnostics;

  /// Indicates whether we are visiting the superclass.
  bool VisitingSuperclass = false;

  /// Add a protocol.
  bool addProtocol(NominalTypeDecl *nominal,
                   ProtocolDecl *protocol, SourceLoc loc,
                   ConformanceSource source);

  /// Add the protocols from the given list.
  void addProtocols(NominalTypeDecl *nominal, ArrayRef<TypeLoc> inherited,
                    ConformanceSource source, LazyResolver *resolver);

  /// Expand the implied conformances for the given DeclContext.
  void expandImpliedConformances(NominalTypeDecl *nominal, DeclContext *dc,
                                 LazyResolver *resolver);

  /// A three-way ordering
  enum class Ordering {
    Before,
    Equivalent,
    After,
  };

  /// Determine whether the first conformance entry supersedes the
  /// second when determining where to place the conformance.
  ///
  /// \param diagnoseSuperseded When one entry is better than another,
  /// whether to diagnose the problem as an error.
  Ordering compareConformances(ConformanceEntry *lhs, ConformanceEntry *rhs,
                               bool &diagnoseSuperseded);

  /// Resolve the set of conformances that will be generated for the
  /// given protocol.
  ///
  /// \returns true if any conformance entries were superseded by this
  /// operation.
  bool resolveConformances(NominalTypeDecl *nominal,
                           ProtocolDecl *protocol,
                           LazyResolver *resolver);

  /// Retrieve the declaration context that provides the
  /// (non-inherited) conformance described by the given conformance
  /// entry.
  DeclContext *getConformingContext(NominalTypeDecl *nominal,
                                    LazyResolver *resolver,
                                    ConformanceEntry *entry);

  /// Resolve the given conformance entry to an actual protocol conformance.
  ProtocolConformance *getConformance(NominalTypeDecl *nominal,
                                      LazyResolver *resolver,
                                      ConformanceEntry *entry);

  /// Enumerate each of the unhandled contexts (nominal type
  /// declaration or extension) within the given stage.
  ///
  /// \param stage The stage to process. Note that it is up to the
  /// caller to ensure that prior stages have already been handled.
  ///
  /// \param nominalFunc Function object to be invoked when the
  /// nominal type declaration itself needs to be processed. It takes
  /// the nominal type declaration and its result is ignored.
  ///
  /// \param extensionFunc Function object to be invoked with a given
  /// extension needs to be processed. It takes the extension as an
  /// argument and its result is ignored.
  template<typename NominalFunc, typename ExtensionFunc>
  void forEachInStage(ConformanceStage stage,
                      NominalTypeDecl *nominal,
                      LazyResolver *resolver,
                      NominalFunc nominalFunc,
                      ExtensionFunc extensionFunc);

  /// Inherit the conformances from the given superclass into the
  /// given nominal type.
  ///
  /// \param classDecl The class into which the conformances will be
  /// inherited.
  ///
  /// \param superclassDecl The superclass from which the conformances
  /// will be inherited.
  ///
  /// \param superclassExt If non-null, the superclass extension from
  /// which conformances will be inherited. If null, the conformances
  /// on the superclass declaration itself will be inherited.
  void inheritConformances(ClassDecl *classDecl, 
                           ClassDecl *superclassDecl,
                           ExtensionDecl *superclassExt,
                           LazyResolver *resolver);

  /// Update a lookup table with conformances from newly-added extensions.
  void updateLookupTable(NominalTypeDecl *nominal, ConformanceStage stage,
                         LazyResolver *resolver);

  /// Load all of the protocol conformances for the given (serialized)
  /// declaration context.
  void loadAllConformances(NominalTypeDecl *nominal,
                           DeclContext *dc,
                           ArrayRef<ProtocolConformance *> conformances);

public:
  /// Create a new conformance lookup table.
  ConformanceLookupTable(ASTContext &ctx, NominalTypeDecl *nominal,
                         LazyResolver *resolver);

  /// Destroy the conformance table.
  void destroy();

  /// Add a synthesized conformance to the lookup table.
  void addSynthesizedConformance(NominalTypeDecl *nominal,
                                 ProtocolDecl *protocol);

  /// Register an externally-supplied protocol conformance.
  void registerProtocolConformance(ProtocolConformance *conformance);

  /// Look for conformances to the given protocol.
  ///
  /// \param conformances Will be populated with the set of protocol
  /// conformances found for this protocol and nominal type.
  ///
  /// \returns true if any conformances were found. 
  bool lookupConformance(Module *module, 
                         NominalTypeDecl *nominal,
                         ProtocolDecl *protocol, 
                         LazyResolver *resolver,
                         SmallVectorImpl<ProtocolConformance *> &conformances);

  /// Look for all of the conformances within the given declaration context.
  void lookupConformances(NominalTypeDecl *nominal,
                          DeclContext *dc,
                          LazyResolver *resolver,
                          ConformanceLookupKind lookupKind,
                          SmallVectorImpl<ProtocolDecl *> *protocols,
                          SmallVectorImpl<ProtocolConformance *> *conformances,
                          SmallVectorImpl<ConformanceDiagnostic> *diagnostics);

  /// Retrieve the complete set of protocols to which this nominal
  /// type conforms.
  void getAllProtocols(NominalTypeDecl *nominal,
                       LazyResolver *resolver,
                       SmallVectorImpl<ProtocolDecl *> &scratch);

  /// Retrieve the complete set of protocol conformances for this
  /// nominal type.
  void getAllConformances(NominalTypeDecl *nominal,
                          LazyResolver *resolver,
                          SmallVectorImpl<ProtocolConformance *> &scratch);

  /// Retrieve the protocols that would be implicitly synthesized.
  /// FIXME: This is a hack, because it's the wrong question to ask. It
  /// skips over the possibility that there is an explicit conformance
  /// somewhere.
  void getImplicitProtocols(NominalTypeDecl *nominal,
                            SmallVectorImpl<ProtocolDecl *> &protocols);

  // Only allow allocation of conformance lookup tables using the
  // allocator in ASTContext or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(ConformanceLookupTable)) {
    return C.Allocate(Bytes, Alignment);
  }
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");
  void dump(raw_ostream &os) const;
};

DeclContext *ConformanceLookupTable::ConformanceSource::getDeclContext() const {
  switch (getKind()) {
  case ConformanceEntryKind::Inherited:
    return getInheritingClass();

  case ConformanceEntryKind::Explicit:
    return getExplicitDeclContext();

  case ConformanceEntryKind::Implied:
    return getImpliedSource()->Source.getDeclContext();

  case ConformanceEntryKind::Synthesized:
    return getSynthesizedDecl();
  }
}

void ConformanceLookupTable::ConformanceEntry::markSupersededBy(
       ConformanceLookupTable &table,
       ConformanceEntry *entry,
       bool diagnose) {
  assert(!isSuperseded() && "Already superseded");

  // Note that we've been superseded.
  SupersededBy = entry;

  if (diagnose) {
    // Record the problem in the conformance table. We'll
    // diagnose these in semantic analysis.
    table.AllSupersededDiagnostics[getDeclContext()].push_back(this);
  }
}

void ConformanceLookupTable::ConformanceEntry::dump() const {
  dump(llvm::errs());
}

void ConformanceLookupTable::ConformanceEntry::dump(raw_ostream &os,
                                                    unsigned indent) const {
  os.indent(indent) << "(conformance @" << static_cast<const void *>(this);

  os << " protocol=";
  getProtocol()->dumpRef(os);

  if (Loc.isValid()) {
    os << " loc=";
    Loc.dump(getProtocol()->getASTContext().SourceMgr);
  }

  switch (getKind()) {
  case ConformanceEntryKind::Implied:
    os << " implied_by=@" 
       << static_cast<const void *>(Source.getImpliedSource());
    break;

  case ConformanceEntryKind::Explicit:
    os << " explicit";
    break;

  case ConformanceEntryKind::Inherited:
    os << " inherited";
    break;

  case ConformanceEntryKind::Synthesized:
    os << " synthesized";
    break;
  }

  if (auto conf = getConformance()) {
    os << " fixed_conformance=@" << static_cast<const void *>(conf);
  }

  if (SupersededBy)
    os << " superseded_by=@" << static_cast<const void *>(SupersededBy);

  os << ")\n";
}

ConformanceLookupTable::ConformanceLookupTable(ASTContext &ctx, 
                                               NominalTypeDecl *nominal,
                                               LazyResolver *resolver) {
  // Register a cleanup with the ASTContext to call the conformance
  // table destructor.
  ctx.addCleanup([this]() {
    this->destroy();
  });
}

void ConformanceLookupTable::destroy() {
  this->~ConformanceLookupTable();
}

template<typename NominalFunc, typename ExtensionFunc>
void ConformanceLookupTable::forEachInStage(ConformanceStage stage,
                                            NominalTypeDecl *nominal,
                                            LazyResolver *resolver,
                                            NominalFunc nominalFunc,
                                            ExtensionFunc extensionFunc) {
  assert(static_cast<unsigned>(stage) < NumConformanceStages &&
         "NumConformanceStages has not been updated");
  LastProcessedEntry &lastProcessed
    = LastProcessed[static_cast<unsigned>(stage)];

  // Handle the nominal type.
  if (!lastProcessed.getInt()) {
    lastProcessed.setInt(true);

    if (resolver)
      resolver->resolveDeclSignature(nominal);

    // If we have conformances we can load, do so.
    // FIXME: This could be more lazy.
    auto loader = nominal->takeConformanceLoader();
    if (loader.first) {
      SmallVector<ProtocolConformance *, 2> conformances;
      loader.first->loadAllConformances(nominal, loader.second, conformances);
      loadAllConformances(nominal, nominal, conformances);
    }

    nominalFunc(nominal);
  }

  // Handle the extensions that we have not yet visited.
  nominal->prepareExtensions();
  for (auto next = lastProcessed.getPointer()
                     ? lastProcessed.getPointer()->NextExtension.getPointer()
                     : nominal->FirstExtension;
       next;
       next = next->NextExtension.getPointer()) {
    lastProcessed.setPointer(next);

    if (resolver)
      resolver->resolveExtension(next);

    // If we have conformances we can load, do so.
    // FIXME: This could be more lazy.
    auto loader = next->takeConformanceLoader();
    if (loader.first) {
      SmallVector<ProtocolConformance *, 2> conformances;
      loader.first->loadAllConformances(next, loader.second, conformances);
      loadAllConformances(nominal, next, conformances);
    }

    extensionFunc(next);
  }
}

void ConformanceLookupTable::inheritConformances(ClassDecl *classDecl, 
                                                 ClassDecl *superclassDecl,
                                                 ExtensionDecl *superclassExt,
                                                 LazyResolver *resolver) {
  // Local function to return the location of the superclass. This
  // takes a little digging, so compute on first use and cache it.
  SourceLoc superclassLoc;
  auto getSuperclassLoc = [&] {
    if (superclassLoc.isValid())
      return superclassLoc;

    for (const auto &inherited : classDecl->getInherited()) {
      if (auto inheritedType = inherited.getType()) {
        if (inheritedType->getClassOrBoundGenericClass()) {
          superclassLoc = inherited.getSourceRange().Start;
          return superclassLoc;
        }
      }
    }

    superclassLoc = superclassDecl->getLoc();
    return superclassLoc;
  };

  llvm::SmallPtrSet<ProtocolDecl *, 4> protocols;
  auto addInheritedConformance = [&](ConformanceEntry *entry) {
    auto protocol = entry->getProtocol();

    // Don't add redundant conformances here. This is merely an
    // optimization; resolveConformances() would zap the duplicates
    // anyway.
    if (!protocols.insert(protocol).second)
      return;
    
    // Add the inherited entry.
    (void)addProtocol(classDecl, protocol, getSuperclassLoc(), 
                      ConformanceSource::forInherited(classDecl));
  };

  // Add inherited conformances.
  DeclContext *superDC = superclassExt;
  if (!superclassExt)
    superDC = superclassDecl;

  for (auto entry : superclassDecl->ConformanceTable->AllConformances[superDC]){
    addInheritedConformance(entry);
  }
}

void ConformanceLookupTable::updateLookupTable(NominalTypeDecl *nominal,
                                               ConformanceStage stage,
                                               LazyResolver *resolver) {
  switch (stage) {
  case ConformanceStage::RecordedExplicit:
    // Record all of the explicit conformances.
    forEachInStage(stage, nominal, resolver,
                   [&](NominalTypeDecl *nominal) {
                     if (resolver)
                       resolver->resolveInheritanceClause(nominal);

                     addProtocols(nominal, nominal->getInherited(), 
                                  ConformanceSource::forExplicit(nominal),
                                  resolver);
                   },
                   [&](ExtensionDecl *ext) {
                     if (resolver)
                       resolver->resolveInheritanceClause(ext);

                     addProtocols(nominal, ext->getInherited(),
                                  ConformanceSource::forExplicit(ext),
                                  resolver);
                   });
    break;

  case ConformanceStage::Inherited:
    updateLookupTable(nominal, ConformanceStage::RecordedExplicit, resolver);

    // For classes, expand implied conformances of the superclass,
    // because an implied conformance in the superclass is considered
    // "fixed" in the subclass.
    if (auto classDecl = dyn_cast<ClassDecl>(nominal)) {
      if (resolver)
        resolver->resolveSuperclass(classDecl);

      if (Type superclass = classDecl->getSuperclass()) {
        if (auto superclassDecl 
              = superclass->getClassOrBoundGenericClass()) {
          // Break infinite recursion when visiting ill-formed classes
          // with circular inheritance.
          if (VisitingSuperclass)
            return;
          llvm::SaveAndRestore<bool> visiting(VisitingSuperclass, true);

          // Resolve the conformances of the superclass.
          superclassDecl->prepareConformanceTable(resolver);
          superclassDecl->ConformanceTable->updateLookupTable(
            superclassDecl,
            ConformanceStage::Resolved,
            resolver);
          
          // Expand inherited conformances.
          forEachInStage(stage, superclassDecl, resolver,
                         [&](NominalTypeDecl *superclass) {
                           inheritConformances(classDecl, superclassDecl,
                                               nullptr, resolver);
                         },
                         [&](ExtensionDecl *ext) {
                           inheritConformances(classDecl, superclassDecl, ext,
                                               resolver);
                         });
        }
      }
    }
    break;    

  case ConformanceStage::ExpandedImplied:
    // Record explicit conformances and import inherited conformances
    // before expanding.
    updateLookupTable(nominal, ConformanceStage::Inherited, resolver);

    // Expand inherited conformances.
    forEachInStage(stage, nominal, resolver,
                   [&](NominalTypeDecl *nominal) {
                     expandImpliedConformances(nominal, nominal, resolver);
                   },
                   [&](ExtensionDecl *ext) {
                     expandImpliedConformances(nominal, ext, resolver);
                   });
    break;

  case ConformanceStage::Resolved:
    // Expand inherited conformances so we have the complete set of
    // conformances.
    updateLookupTable(nominal, ConformanceStage::ExpandedImplied, resolver);
    
    /// Determine whether any extensions were added that might require
    /// us to compute conformances again.
    bool anyChanged = false;
    forEachInStage(stage, nominal, resolver,
                   [&](NominalTypeDecl *nominal) {
                     anyChanged = true;
                   },
                   [&](ExtensionDecl *ext) {
                     anyChanged = true;
                   });
                   
    if (anyChanged) {
      // Compute the conformances for each protocol.
      bool anySuperseded = false;
      for (const auto &entry : Conformances) {
        if (resolveConformances(nominal, entry.first, resolver))
          anySuperseded = true;
      }

      if (anySuperseded) {
        // Update the lists of all conformances to remove superseded
        // conformances.
        for (auto &conformances : AllConformances) {
          conformances.second.erase(
            std::remove_if(conformances.second.begin(),
                           conformances.second.end(),
                           [&](ConformanceEntry *entry) {
                             return entry->isSuperseded();
                           }),
            conformances.second.end());
        }
      }
    }
    break;
  }
}

void ConformanceLookupTable::loadAllConformances(
       NominalTypeDecl *nominal,
       DeclContext *dc,
       ArrayRef<ProtocolConformance*> conformances) {
  // If this declaration context came from source, there's nothing to
  // do here.
  if (dc->getParentSourceFile())
    return;

  // Add entries for each loaded conformance.
  for (auto conformance : conformances) {
    registerProtocolConformance(conformance);
  }
}

namespace {
  /// Visit the protocols referenced by the given type, which was
  /// uttered at the given location.
  template<typename AddProtocolFunc>
  void visitProtocols(Type type, SourceLoc loc, AddProtocolFunc addProtocol) {
    // Protocol types.
    if (auto protocol = type->getAs<ProtocolType>()) {
      addProtocol(protocol->getDecl(), loc);
      return;
    }

    // Protocol compositions.
    if (auto composition = type->getAs<ProtocolCompositionType>()) {
      for (auto protocol : composition->getProtocols())
        visitProtocols(protocol, loc, addProtocol);
      return;
    }
  }

  /// Visit the protocols referenced by the given type representation.
  /// Each protocol is provided to the callback along with the source
  /// location where that protocol was named.
  template<typename AddProtocolFunc>
  void visitProtocols(TypeRepr *typeRepr, LazyResolver *resolver,
                      AddProtocolFunc addProtocol) {
    // Look through attributed type representations.
    while (auto attr = dyn_cast<AttributedTypeRepr>(typeRepr))
      typeRepr = attr->getTypeRepr();

    // Handle identifier type representations.
    if (auto ident = dyn_cast<IdentTypeRepr>(typeRepr)) {
      ComponentIdentTypeRepr *component = ident->getComponentRange().back();

      // If the last component refers to a type, visit the protocols
      // in that type.
      if (component->isBoundType()) {
        visitProtocols(component->getBoundType(), component->getIdLoc(),
                       addProtocol);
        return;
      }

      // If the last component refers to a type declaration, visit the
      // protocols in that type.
      if (component->isBoundDecl()) {
        if (auto typeDecl = dyn_cast<TypeDecl>(component->getBoundDecl())) {
          if (resolver)
            resolver->resolveDeclSignature(typeDecl);

          visitProtocols(typeDecl->getDeclaredType(), component->getIdLoc(),
                         addProtocol);
        }
      }

      return;
    }

    // Recurse into protocol compositions.
    if (auto composition = dyn_cast<ProtocolCompositionTypeRepr>(typeRepr)) {
      for (auto ident : composition->getProtocols())
        visitProtocols(ident, resolver, addProtocol);
      return;
    }
  }
}

bool ConformanceLookupTable::addProtocol(NominalTypeDecl *nominal,
                                         ProtocolDecl *protocol, SourceLoc loc,
                                         ConformanceSource source) {
  DeclContext *dc = source.getDeclContext();
  ASTContext &ctx = dc->getASTContext();

  // Determine the kind of conformance.
  ConformanceEntryKind kind = source.getKind();

  // If this entry is synthesized or implied, scan to determine
  // whether there are any explicit better conformances that make this
  // conformance trivially superseded (and, therefore, no worth
  // recording).
  auto &conformanceEntries = Conformances[protocol];
  if (kind == ConformanceEntryKind::Implied ||
      kind == ConformanceEntryKind::Synthesized) {
    for (const auto *existingEntry : conformanceEntries) {
      switch (existingEntry->getKind()) {
      case ConformanceEntryKind::Explicit:
      case ConformanceEntryKind::Inherited:
        return false;

      case ConformanceEntryKind::Implied:
        // An implied conformance is better than a synthesized one.
        if (kind == ConformanceEntryKind::Synthesized)
          return false;
        break;

      case ConformanceEntryKind::Synthesized:
        break;
      }
    }
  }

  /// Build the conformance entry (if it hasn't been built before).
  ConformanceEntry *entry = new (ctx) ConformanceEntry(loc, protocol, source);
  conformanceEntries.push_back(entry);

  // Record this as a conformance within the given declaration
  // context.
  AllConformances[dc].push_back(entry);
  return true;
}

void ConformanceLookupTable::addProtocols(NominalTypeDecl *nominal,
                                          ArrayRef<TypeLoc> inherited,
                                          ConformanceSource source,
                                          LazyResolver *resolver) {
  // Visit each of the types in the inheritance list to find
  // protocols.
  for (const auto &entry : inherited) {
    if (auto typeRepr = entry.getTypeRepr()) {
      visitProtocols(typeRepr, resolver,
                     [&](ProtocolDecl *protocol, SourceLoc loc) {
                       addProtocol(nominal, protocol, loc, source);
                     });
    }
  }
}

void ConformanceLookupTable::expandImpliedConformances(NominalTypeDecl *nominal,
                                                       DeclContext *dc, 
                                                       LazyResolver *resolver) {
  // Note: recursive type-checking implies that that AllConformances
  // may be reallocated during this traversal, so pay the lookup cost
  // during each iteration.
  for (unsigned i = 0; i != AllConformances[dc].size(); ++i) {
    ConformanceEntry *conformanceEntry = AllConformances[dc][i];
    ProtocolDecl *conformingProtocol = conformanceEntry->getProtocol();

    // Visit the protocols inherited by this protocol, adding them as
    // implied conformances.
    if (resolver)
      resolver->resolveInheritanceClause(dc);
      
    // FIXME: It's odd that the inheritance clause isn't loaded at all during
    // deserialization, so we have this weird separate path.
    if (!conformingProtocol->getParentSourceFile()) {
      for (auto protocol : conformingProtocol->getInheritedProtocols(
                             resolver)) {
        addProtocol(nominal, protocol, SourceLoc(),
                    ConformanceSource::forImplied(conformanceEntry));
      }
      continue;
    }
    
    for (const auto &entry : conformingProtocol->getInherited()) {
      if (auto typeRepr = entry.getTypeRepr()) {
        visitProtocols(typeRepr, resolver,
                       [&](ProtocolDecl *protocol, SourceLoc loc) {
                         addProtocol(nominal, protocol, loc, 
                                     ConformanceSource::forImplied(
                                       conformanceEntry));
                       });
      }
    }
  }
}

/// Determine whether the given conformance entry kind can be replaced.
static bool isReplaceable(ConformanceEntryKind kind) {
  switch (kind) {
  case ConformanceEntryKind::Implied:
  case ConformanceEntryKind::Synthesized:
    return true;

  case ConformanceEntryKind::Explicit:
  case ConformanceEntryKind::Inherited:
    return false;
  }
}

ConformanceLookupTable::Ordering ConformanceLookupTable::compareConformances(
                                   ConformanceEntry *lhs,
                                   ConformanceEntry *rhs,
                                   bool &diagnoseSuperseded) {
  // If one entry is fixed and the other is not, we have our answer.
  if (lhs->isFixed() != rhs->isFixed()) {
    // If the non-fixed conformance is not replacable, we have a failure to
    // diagnose.
    diagnoseSuperseded = (lhs->isFixed() && !isReplaceable(rhs->getKind())) ||
                         (rhs->isFixed() && !isReplaceable(lhs->getKind()));
      
    return lhs->isFixed() ? Ordering::Before : Ordering::After;
  }

  ConformanceEntryKind lhsKind = lhs->getKind();
  ConformanceEntryKind rhsKind = rhs->getKind();

  if (lhsKind != ConformanceEntryKind::Implied ||
      rhsKind != ConformanceEntryKind::Implied) {
    // If both conformances are non-replaceable, diagnose the
    // superseded one.
    diagnoseSuperseded = !isReplaceable(lhsKind) && !isReplaceable(rhsKind) &&
      !(lhsKind == ConformanceEntryKind::Inherited &&
        rhsKind == ConformanceEntryKind::Inherited);

    // If we can order by kind, do so.
    if (lhsKind != rhsKind) {
      return (static_cast<unsigned>(lhsKind) < static_cast<unsigned>(rhsKind))
               ? Ordering::Before
               : Ordering::After;
    }

    // We shouldn't get two synthesized conformances. It's not harmful
    // per se, but it's indicative of redundant logic in the frontend.
    assert(lhsKind != ConformanceEntryKind::Synthesized &&
          "Shouldn't ever get two synthesized conformances");

    // FIXME: Deterministic ordering.
    return Ordering::Before;
  }

  // Both the left- and right-hand sides are implied, so use the
  // stated explicit conformances to determine where the conformance
  // should go.
  assert(lhsKind == ConformanceEntryKind::Implied &&
         "Expected implied conformance");
  assert(rhsKind == ConformanceEntryKind::Implied &&
         "Expected implied conformance");
  auto lhsExplicit = lhs->getDeclaredConformance();
  auto lhsExplicitProtocol = lhsExplicit->getProtocol();
  auto rhsExplicit = rhs->getDeclaredConformance();
  auto rhsExplicitProtocol = rhsExplicit->getProtocol();
  if (lhsExplicitProtocol != rhsExplicitProtocol) {
    // If the explicit protocol for the left-hand side is implied by
    // the explicit protocol for the right-hand side, the left-hand
    // side supersedes the right-hand side.
    for (auto rhsProtocol : rhsExplicitProtocol->getAllProtocols(nullptr)){
      if (rhsProtocol == lhsExplicitProtocol) {
        diagnoseSuperseded = false;
        return Ordering::Before;
      }
    }

    // If the explicit protocol for the right-hand side is implied by
    // the explicit protocol for the left-hand side, the right-hand
    // side supersedes the left-hand side.
    for (auto lhsProtocol : lhsExplicitProtocol->getAllProtocols(nullptr)){
      if (lhsProtocol == rhsExplicitProtocol) {
        diagnoseSuperseded = false;
        return Ordering::After;
      }
    }

    // There is no ordering between the two explicit conformances. If
    // they land in different contexts, diagnose the problem.
    diagnoseSuperseded = lhs->getDeclContext() != rhs->getDeclContext();
  }

  // If we get here when lhsExplicitProtocol == rhsExplicitProtocol,
  // supersede without diagnosing. Either it's well-formed because the
  // two implicit conformances come from exactly the same explicit
  // conformance (e.g., due to the protocol inherance graph being a
  // DAG rather than a tree) or we will already be diagnosing the
  // redundant explicit conformance, and don't want to introduce
  // redundant diagnostics.

  // FIXME: Deterministic ordering.
  return Ordering::Before;
}

bool ConformanceLookupTable::resolveConformances(NominalTypeDecl *nominal,
                                                 ProtocolDecl *protocol,
                                                 LazyResolver *resolver) {
  // Find any entries that are superseded by other entries.
  ConformanceEntries &entries = Conformances[protocol];
  llvm::SmallPtrSet<DeclContext *, 4> knownConformances;
  bool anySuperseded = false;
  for (auto entry : entries) {
    // If this entry has a conformance associated with it, note that.
    if (entry->getConformance())
      knownConformances.insert(entry->getDeclContext());

    // If this entry was superseded, move on.
    if (entry->isSuperseded()) {
      anySuperseded = true;
      continue;
    }

    // Determine whether this entry is superseded by (or supersedes)
    // some other entry.
    for (auto otherEntry : entries) {
      if (entry == otherEntry)
        continue;

      if (otherEntry->isSuperseded()) {
        anySuperseded = true;
        continue;
      }

      bool diagnoseSuperseded = false;
      bool doneWithEntry = false;
      switch (compareConformances(entry, otherEntry, diagnoseSuperseded)) {
      case Ordering::Equivalent:
        break;

      case Ordering::Before:
        otherEntry->markSupersededBy(*this, entry, diagnoseSuperseded);
        anySuperseded = true; 
        break;

      case Ordering::After:
        entry->markSupersededBy(*this, otherEntry, diagnoseSuperseded);
        anySuperseded = true;
        doneWithEntry = true;
        break;
      }
    
      if (doneWithEntry)
        break;
    }
  }

  // If any entries were superseded, remove them now.
  if (anySuperseded) {
    entries.erase(std::remove_if(entries.begin(), entries.end(),
                                 [&](ConformanceEntry *entry) {
                                   return entry->isSuperseded();
                                 }),
                  entries.end());
  }

  return anySuperseded;
}

DeclContext *ConformanceLookupTable::getConformingContext(
               NominalTypeDecl *nominal,
               LazyResolver *resolver,
               ConformanceEntry *entry) {
  ProtocolDecl *protocol = entry->getProtocol();

  // Dig through the inherited entries to find a non-inherited one.
  // Handle recursive inheritance.
  SmallPtrSet<ClassDecl *, 4> visited;
  while (entry->getKind() == ConformanceEntryKind::Inherited) {
    // Make sure we have an up-to-date conformance table for the
    // superclass.
    auto classDecl = cast<ClassDecl>(nominal);
    if (!visited.insert(classDecl).second)
      return nullptr;

    auto superclassDecl
      = classDecl->getSuperclass()->getClassOrBoundGenericClass();

    if (!classDecl->ConformanceTable->VisitingSuperclass) {
      llvm::SaveAndRestore<bool> visiting(
                                   classDecl->ConformanceTable
                                     ->VisitingSuperclass,
                                   true);

      superclassDecl->prepareConformanceTable(resolver);
      superclassDecl->ConformanceTable->resolveConformances(superclassDecl,
                                                            protocol,
                                                            resolver);
    }

    // Grab the superclass entry and continue searching for a
    // non-inherited conformance.
    // FIXME: Ambiguity detection and resolution.
    entry = superclassDecl->ConformanceTable->Conformances[protocol].front();
    nominal = superclassDecl;
  }

  return entry->getDeclContext();
}

ProtocolConformance *ConformanceLookupTable::getConformance(
                       NominalTypeDecl *nominal,
                       LazyResolver *resolver,
                       ConformanceEntry *entry) {
  // If we already have a conformance, we're done.
  if (auto conformance = entry->getConformance())
    return conformance;

  ProtocolDecl *protocol = entry->getProtocol();

  // Determine where the explicit conformance actually lives.
  // FIXME: This is a hack to ensure that inherited conformances are
  // always "single step", which is bad for resilience but is assumed
  // elsewhere in the compiler.
  DeclContext *conformingDC = getConformingContext(nominal, resolver, entry);
  if (!conformingDC)
    return nullptr;

  NominalTypeDecl *conformingNominal
    = conformingDC->isNominalTypeOrNominalTypeExtensionContext();

  // Form the conformance.
  Type type = entry->getDeclContext()->getDeclaredTypeInContext();
  ProtocolConformance *conformance;
    ASTContext &ctx = nominal->getASTContext();
  if (entry->getKind() == ConformanceEntryKind::Inherited) {
    // For an inherited conformance, the conforming nominal type will
    // be different from the nominal type.
    assert(conformingNominal != nominal && "Broken inherited conformance");

    // Find the superclass type that matches where the conformance was
    // declared.
    Type superclassTy = type->getSuperclass(resolver);
    while (superclassTy->getAnyNominal() != conformingNominal)
      superclassTy = superclassTy->getSuperclass(resolver);

    // Look up the inherited conformance.
    Module *module = entry->getDeclContext()->getParentModule();
    auto inheritedConformance = module->lookupConformance(superclassTy,
                                                          protocol,
                                                          resolver)
                                  .getPointer();

    // Form the inherited conformance.
    conformance = ctx.getInheritedConformance(type, inheritedConformance);
  } else {
    // Create or find the normal conformance.
    Type conformingType = conformingDC->getDeclaredTypeInContext();
    SourceLoc conformanceLoc
      = conformingNominal == conformingDC
          ? conformingNominal->getLoc()
          : cast<ExtensionDecl>(conformingDC)->getLoc();

    auto normal = ctx.getConformance(conformingType, protocol, conformanceLoc,
                                     conformingDC,
                                     ProtocolConformanceState::Incomplete);

    // FIXME: Fully check the conformance. We shouldn't need this.
    if (resolver) {
      resolver->checkConformance(normal);
    }

    conformance = normal;
  }

  // Record the conformance.
  entry->Conformance = conformance;
  return conformance;
}

void ConformanceLookupTable::addSynthesizedConformance(NominalTypeDecl *nominal,
                                                       ProtocolDecl *protocol) {
  addProtocol(nominal, protocol, nominal->getLoc(),
              ConformanceSource::forSynthesized(nominal));
}

void ConformanceLookupTable::registerProtocolConformance(
       ProtocolConformance *conformance) {
  auto protocol = conformance->getProtocol();
  auto dc = conformance->getDeclContext();
  auto nominal = dc->isNominalTypeOrNominalTypeExtensionContext();

  // If there is an entry to update, do so.
  auto &dcConformances = AllConformances[dc];
  for (auto entry : dcConformances) {
    if (entry->getProtocol() == protocol) {
      assert(!entry->getConformance() ||
             entry->getConformance() == conformance &&
             "Mismatched conformances");
      entry->Conformance = conformance;
      return;
    }
  }

  // Otherwise, add a new entry.
  auto inherited = dyn_cast<InheritedProtocolConformance>(conformance);
  ConformanceSource source
    = inherited ? ConformanceSource::forInherited(cast<ClassDecl>(nominal))
                : ConformanceSource::forExplicit(dc);

  ASTContext &ctx = nominal->getASTContext();
  ConformanceEntry *entry = new (ctx) ConformanceEntry(SourceLoc(),
                                                       protocol,
                                                       source);
  entry->Conformance = conformance;

  // Record that this type conforms to the given protocol.
  Conformances[protocol].push_back(entry);

  // Record this as a conformance within the given declaration
  // context.
  dcConformances.push_back(entry);
}

bool ConformanceLookupTable::lookupConformance(
       Module *module, 
       NominalTypeDecl *nominal,
       ProtocolDecl *protocol, 
       LazyResolver *resolver,
       SmallVectorImpl<ProtocolConformance *> &conformances) {
  // Update to record all explicit and inherited conformances.
  updateLookupTable(nominal, ConformanceStage::Inherited, resolver);

  // Look for conformances to this protocol.
  auto known = Conformances.find(protocol);
  if (known == Conformances.end()) {
    // If we didn't find anything, expand implied conformances.
    updateLookupTable(nominal, ConformanceStage::ExpandedImplied, resolver);
    known = Conformances.find(protocol);

    // We didn't find anything.
    if (known == Conformances.end())
      return false;
  }

  // Resolve the conformances for this protocol.
  resolveConformances(nominal, protocol, resolver);
  for (auto entry : Conformances[protocol]) {
    if (auto conformance = getConformance(nominal, resolver, entry)) {
      conformances.push_back(conformance);
    }
  }
  return !conformances.empty();
}

void ConformanceLookupTable::lookupConformances(
       NominalTypeDecl *nominal,
       DeclContext *dc,
       LazyResolver *resolver,
       ConformanceLookupKind lookupKind,
       SmallVectorImpl<ProtocolDecl *> *protocols,
       SmallVectorImpl<ProtocolConformance *> *conformances,
       SmallVectorImpl<ConformanceDiagnostic> *diagnostics) {
  // We need to expand all implied conformances before we can find
  // those conformances that pertain to this declaration context.
  updateLookupTable(nominal, ConformanceStage::ExpandedImplied, resolver);

  /// Resolve conformances for each of the protocols to which this
  /// declaration may provide a conformance. Only some of these will
  /// result in conformances that are attributed to this declaration
  /// context.
  auto &potentialConformances = AllConformances[dc]; 
  for (const auto &potential : potentialConformances) {
    resolveConformances(nominal, potential->getProtocol(), resolver);
  }

  // Remove any superseded conformances from AllConformances.
  potentialConformances.erase(
    std::remove_if(potentialConformances.begin(),
                   potentialConformances.end(),
                   [&](ConformanceEntry *entry) {
                     if (entry->isSuperseded())
                       return true;

                     if (lookupKind == ConformanceLookupKind::OnlyExplicit &&
                         entry->getKind() != ConformanceEntryKind::Explicit)
                       return true;

                     // Record the protocol.
                     if (protocols)
                       protocols->push_back(entry->getProtocol());

                     // Record the conformance.
                     if (conformances) {
                       if (auto conformance = getConformance(nominal, resolver,
                                                             entry))
                         conformances->push_back(conformance);
                     }
                     return false;
                   }),
    potentialConformances.end());

  // Gather any diagnostics we've produced.
  if (diagnostics) {
    auto knownDiags = AllSupersededDiagnostics.find(dc);
    if (knownDiags != AllSupersededDiagnostics.end()) {
      for (const auto *entry : knownDiags->second) {
        ConformanceEntry *supersededBy = entry->getSupersededBy();

        diagnostics->push_back({entry->getProtocol(), 
                                entry->getDeclaredLoc(),
                                entry->getKind(),
                                entry->getDeclaredConformance()->getProtocol(),
                                supersededBy->getDeclContext(),
                                supersededBy->getKind(),
                                supersededBy->getDeclaredConformance()
                                  ->getProtocol()});
      }

      // We have transferred these diagnostics; erase them.
      AllSupersededDiagnostics.erase(knownDiags);
    }
  }
}

void ConformanceLookupTable::getAllProtocols(
       NominalTypeDecl *nominal,
       LazyResolver *resolver,
       SmallVectorImpl<ProtocolDecl *> &scratch) {
  // We need to expand all implied conformances to find the complete
  // set of protocols to which this nominal type conforms.
  updateLookupTable(nominal, ConformanceStage::ExpandedImplied, resolver);

  // Gather all of the protocols.
  for (const auto &conformance : Conformances) {
    if (conformance.second.empty())
      continue;

    scratch.push_back(conformance.first);
  }

  // FIXME: sort the protocols in some canonical order?
}

void ConformanceLookupTable::getAllConformances(
       NominalTypeDecl *nominal,
       LazyResolver *resolver,
       SmallVectorImpl<ProtocolConformance *> &scratch) {
  // We need to expand and resolve all conformances to enumerate them.
  updateLookupTable(nominal, ConformanceStage::Resolved, resolver);

  // Gather all of the protocols.
  for (const auto &conformance : AllConformances) {
    for (auto entry : conformance.second) {
      if (auto conformance = getConformance(nominal, resolver, entry))
        scratch.push_back(conformance);
    }
  }

  // FIXME: sort the conformances in some canonical order?
}

void ConformanceLookupTable::getImplicitProtocols(
       NominalTypeDecl *nominal,
       SmallVectorImpl<ProtocolDecl *> &protocols) {
  for (auto conformance : AllConformances[nominal]) {
    if (conformance->getKind() == ConformanceEntryKind::Synthesized) {
      protocols.push_back(conformance->getProtocol());
    }
  }
}

void ConformanceLookupTable::dump() const {
  dump(llvm::errs());
}
  
void ConformanceLookupTable::dump(raw_ostream &os) const {
  for (const auto &dcEntries : AllConformances) {
    os << "Conformances in context:\n";
    dcEntries.first->printContext(os);
    for (auto entry : dcEntries.second) {
      entry->dump(os);
    }
  }
}

void NominalTypeDecl::prepareConformanceTable(LazyResolver *resolver) const {
  if (ConformanceTable)
    return;

  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  ASTContext &ctx = getASTContext();
  ConformanceTable = new (ctx) ConformanceLookupTable(ctx, mutableThis,
                                                      resolver);

  // If this type declaration was not parsed from source code or introduced
  // via the Clang importer, don't add any synthesized conformances.
  if (!getParentSourceFile() && !hasClangNode())
    return;

  // Add any synthesized conformances.
  if (isa<ClassDecl>(this)) {
    if (auto anyObject = getASTContext().getProtocol(
                           KnownProtocolKind::AnyObject)) {
      ConformanceTable->addSynthesizedConformance(mutableThis, anyObject);
    }
  } else if (auto theEnum = dyn_cast<EnumDecl>(mutableThis)) {
    if (theEnum->isSimpleEnum()) {
      // Simple enumerations conform to Equatable.
      if (auto equatable = ctx.getProtocol(KnownProtocolKind::Equatable)) {
        ConformanceTable->addSynthesizedConformance(mutableThis, equatable);
      }

      // Simple enumerations conform to Hashable.
      if (auto hashable = getASTContext().getProtocol(
                            KnownProtocolKind::Hashable)) {
        ConformanceTable->addSynthesizedConformance(mutableThis, hashable);
      }
    }

    // Enumerations with a raw type conform to RawRepresentable.
    if (resolver)
      resolver->resolveRawType(theEnum);
    if (theEnum->hasRawType()) {
      // Simple enumerations conform to Equatable and Hashable.
      if (auto rawRepresentable = getASTContext().getProtocol(
                                    KnownProtocolKind::RawRepresentable)) {
        ConformanceTable->addSynthesizedConformance(mutableThis,
                                                    rawRepresentable);
      }
    }
  }

  // Add protocols for any synthesized protocol attributes.
  for (auto attr : getAttrs()) {
    if (auto synthesizedProto = dyn_cast<SynthesizedProtocolAttr>(attr)) {
      if (auto proto = getASTContext().getProtocol(
                         synthesizedProto->getProtocolKind())) {
        ConformanceTable->addSynthesizedConformance(mutableThis, proto);
      }
    }
  }
}

bool NominalTypeDecl::lookupConformance(
       Module *module, ProtocolDecl *protocol,
       LazyResolver *resolver,
       SmallVectorImpl<ProtocolConformance *> &conformances) const {
  prepareConformanceTable(resolver);
  return ConformanceTable->lookupConformance(
           module,
           const_cast<NominalTypeDecl *>(this),
           protocol,
           resolver,
           conformances);
}

SmallVector<ProtocolDecl *, 2> NominalTypeDecl::getAllProtocols(
                                 LazyResolver *resolver) const {
  prepareConformanceTable(resolver);
  SmallVector<ProtocolDecl *, 2> result;
  ConformanceTable->getAllProtocols(const_cast<NominalTypeDecl *>(this),
                                    resolver,
                                    result);
  return result;
}

SmallVector<ProtocolConformance *, 2> NominalTypeDecl::getAllConformances(
                                        LazyResolver *resolver,
                                        bool sorted) const
{
  prepareConformanceTable(resolver);
  SmallVector<ProtocolConformance *, 2> result;
  ConformanceTable->getAllConformances(const_cast<NominalTypeDecl *>(this),
                                       resolver,
                                       result);

  if (sorted) {
    // If requested, sort the results.
    ASTContext &ctx = getASTContext();
    std::sort(result.begin(), result.end(), [&](ProtocolConformance *lhs,
                                                ProtocolConformance *rhs) {
      // If the two conformances are normal conformances with locations,
      // sort by location.
      if (auto lhsNormal = dyn_cast<NormalProtocolConformance>(lhs)) {
        if (auto rhsNormal = dyn_cast<NormalProtocolConformance>(rhs)) {
          if (lhsNormal->getLoc().isValid() && rhsNormal->getLoc().isValid()) {
            unsigned lhsBuffer
              = ctx.SourceMgr.findBufferContainingLoc(lhsNormal->getLoc());
            unsigned rhsBuffer
              = ctx.SourceMgr.findBufferContainingLoc(rhsNormal->getLoc());

            // If the buffers are the same, use source location ordering.
            if (lhsBuffer == rhsBuffer) {
              return ctx.SourceMgr.isBeforeInBuffer(lhsNormal->getLoc(),
                                                    rhsNormal->getLoc());
            }

            // Otherwise, order by buffer identifier.
            return StringRef(ctx.SourceMgr.getIdentifierForBuffer(lhsBuffer))
                   < StringRef(ctx.SourceMgr.getIdentifierForBuffer(rhsBuffer));
          }
        }
      }

      // Otherwise, sort by protocol.
      ProtocolDecl *lhsProto = lhs->getProtocol();
      ProtocolDecl *rhsProto = rhs->getProtocol();
      return ProtocolType::compareProtocols(&lhsProto, &rhsProto) < 0;
    });
  }
  return result;
}

void NominalTypeDecl::getImplicitProtocols(
       LazyResolver *resolver,
       SmallVectorImpl<ProtocolDecl *> &protocols) {
  prepareConformanceTable(resolver);
  ConformanceTable->getImplicitProtocols(this, protocols);
}

void NominalTypeDecl::registerProtocolConformance(
       ProtocolConformance *conformance) {
  prepareConformanceTable(/*FIXME:*/nullptr);
  ConformanceTable->registerProtocolConformance(conformance);
}

SmallVector<ProtocolDecl *, 2>
DeclContext::getLocalProtocols(
  LazyResolver *resolver,
  ConformanceLookupKind lookupKind,
  SmallVectorImpl<ConformanceDiagnostic> *diagnostics) const
{
  SmallVector<ProtocolDecl *, 2> result;

  // Dig out the nominal type.
  NominalTypeDecl *nominal = isNominalTypeOrNominalTypeExtensionContext();
  if (!nominal)
    return result;

  // Update to record all potential conformances.
  nominal->prepareConformanceTable(resolver);
  nominal->ConformanceTable->lookupConformances(
    nominal,
    const_cast<DeclContext *>(this),
    resolver,
    lookupKind,
    &result,
    nullptr,
    diagnostics);
  return result;
}

SmallVector<ProtocolConformance *, 2>
DeclContext::getLocalConformances(
  LazyResolver *resolver,
  ConformanceLookupKind lookupKind,
  SmallVectorImpl<ConformanceDiagnostic> *diagnostics) const
{
  SmallVector<ProtocolConformance *, 2> result;

  // Dig out the nominal type.
  NominalTypeDecl *nominal = isNominalTypeOrNominalTypeExtensionContext();
  if (!nominal)
    return result;

  // Update to record all potential conformances.
  nominal->prepareConformanceTable(resolver);
  nominal->ConformanceTable->lookupConformances(
    nominal,
    const_cast<DeclContext *>(this),
    resolver,
    lookupKind,
    nullptr,
    &result,
    diagnostics);
  return result;
}
