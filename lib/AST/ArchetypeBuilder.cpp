//===--- ArchetypeBuilder.cpp - Generic Requirement Builder ---------------===//
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

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

using namespace swift;
using llvm::DenseMap;

void RequirementSource::dump(SourceManager *srcMgr) const {
  dump(llvm::errs(), srcMgr);
}

void RequirementSource::dump(llvm::raw_ostream &out, 
                             SourceManager *srcMgr) const {
  switch (getKind()) {
  case Explicit:
    out << "explicit";
    break;

  case Redundant:
    out << "redundant";
    break;

  case Protocol:
    out << "protocol";
    break;

  case Inferred:
    out << "inferred";
    break;

  case Inherited:
    out << "inherited";
    break;

  case ProtocolRequirementSignatureSelf:
    out << "protocol_requirement_signature_self";
    break;
  }

  if (srcMgr && getLoc().isValid()) {
    out << " @ ";
    getLoc().dump(*srcMgr);
  }
}

/// Update the recorded requirement source when a new requirement
/// source provides the same requirement.
static void updateRequirementSource(RequirementSource &source,
                                    RequirementSource newSource) {
  // If the new source is less explicit than the existing source,
  // or if they have the same kind but we don't have source location
  // information yet, replace the existing source.
  if (source.getKind() < newSource.getKind() ||
      (source.getKind() == newSource.getKind() &&
       !source.getLoc().isValid()))
    source = newSource;
}

struct ArchetypeBuilder::Implementation {
  /// Function used to look up conformances.
  std::function<GenericFunction> LookupConformance;

  /// The generic parameters that this archetype builder is working with.
  SmallVector<GenericTypeParamType *, 4> GenericParams;

  /// The potential archetypes for the generic parameters in \c GenericParams.
  SmallVector<PotentialArchetype *, 4> PotentialArchetypes;

  /// The number of nested types that haven't yet been resolved to archetypes.
  /// Once all requirements have been added, this will be zero in well-formed
  /// code.
  unsigned NumUnresolvedNestedTypes = 0;

  /// The nested types that have been renamed.
  SmallVector<PotentialArchetype *, 4> RenamedNestedTypes;

#ifndef NDEBUG
  /// Whether we've already finalized the builder.
  bool finalized = false;
#endif
};

ArchetypeBuilder::PotentialArchetype::~PotentialArchetype() {
  for (const auto &nested : NestedTypes) {
    for (auto pa : nested.second) {
      if (pa != this)
        delete pa;
    }
  }
}

std::string ArchetypeBuilder::PotentialArchetype::getDebugName() const {
  llvm::SmallString<64> result;

  auto parent = getParent();
  if (!parent) {
    return GenericTypeParamType::get(getGenericParamKey().Depth,
                                     getGenericParamKey().Index,
                                     getBuilder()->getASTContext())->getName()
             .str();
  }

  // Nested types.
  result += parent->getDebugName();

  // When building the name for debugging purposes, include the
  // protocol into which the associated type was resolved.
  if (auto assocType = getResolvedAssociatedType()) {
    result.push_back('[');
    result.push_back('.');
    result.append(assocType->getProtocol()->getName().str().begin(),
                  assocType->getProtocol()->getName().str().end());
    result.push_back(']');
  }

  result.push_back('.');
  result.append(getNestedName().str().begin(), getNestedName().str().end());

  return result.str().str();
}

unsigned ArchetypeBuilder::PotentialArchetype::getNestingDepth() const {
  unsigned Depth = 0;
  for (auto P = getParent(); P; P = P->getParent())
    ++Depth;
  return Depth;
}

void ArchetypeBuilder::PotentialArchetype::resolveAssociatedType(
       AssociatedTypeDecl *assocType,
       ArchetypeBuilder &builder) {
  assert(!getResolvedAssociatedType() && "associated type is already resolved");
  isUnresolvedNestedType = false;
  identifier.assocTypeOrAlias = assocType;
  assert(assocType->getName() == getNestedName());
  assert(builder.Impl->NumUnresolvedNestedTypes > 0 &&
         "Mismatch in number of unresolved nested types");
  --builder.Impl->NumUnresolvedNestedTypes;
}

/// Retrieve the conformance for the superclass constraint of the given
/// potential archetype (if present) to the given protocol.
///
/// \param pa The potential archetype whose superclass constraint is being
/// queried.
///
/// \param proto The protocol to which we are establishing conformance.
///
/// \param conformsSource The requirement source for the conformance to the
/// given protocol.
///
/// \param builder The archetype builder in which the potential archetype
/// resides.
static ProtocolConformance *getSuperConformance(
                              ArchetypeBuilder::PotentialArchetype *pa,
                              ProtocolDecl *proto,
                              RequirementSource &conformsSource,
                              ArchetypeBuilder &builder) {
  // Get the superclass constraint.
  Type superclass = pa->getSuperclass();
  if (!superclass) return nullptr;

  // Lookup the conformance of the superclass to this protocol.
  auto conformance =
    builder.getLookupConformanceFn()(pa->getDependentType(
                                         { }, /*allowUnresolved=*/true)
                                       ->getCanonicalType(),
                                     superclass,
                                     proto->getDeclaredInterfaceType()
                                       ->castTo<ProtocolType>());
  if (!conformance) return nullptr;

  // Conformance to this protocol is redundant; update the requirement source
  // appropriately.
  updateRequirementSource(
    conformsSource,
    RequirementSource(RequirementSource::Inherited,
                      pa->getSuperclassSource().getLoc()));
  return conformance->getConcrete();
}

/// If there is a same-type requirement to be added for the given nested type
/// due to a superclass constraint on the parent type, add it now.
static void maybeAddSameTypeRequirementForNestedType(
              ArchetypeBuilder::PotentialArchetype *nestedPA,
              RequirementSource fromSource,
              ProtocolConformance *superConformance,
              ArchetypeBuilder &builder) {
  // If there's no super conformance, we're done.
  if (!superConformance) return;

  auto assocType = nestedPA->getResolvedAssociatedType();
  assert(assocType && "Not resolved to an associated type?");

  // Dig out the type witness.
  auto concreteType =
    superConformance->getTypeWitness(assocType, builder.getLazyResolver())
      .getReplacement();
  if (!concreteType) return;

  // Add the same-type constraint.
  concreteType = superConformance->getDeclContext()
      ->mapTypeOutOfContext(concreteType);
  if (auto otherPA = builder.resolveArchetype(concreteType))
    builder.addSameTypeRequirementBetweenArchetypes(
        nestedPA, otherPA, fromSource);
  else
    builder.addSameTypeRequirementToConcrete(
        nestedPA, concreteType, fromSource);
}

bool ArchetypeBuilder::PotentialArchetype::addConformance(
       ProtocolDecl *proto, 
       bool updateExistingSource,
       const RequirementSource &source,
       ArchetypeBuilder &builder) {
  auto rep = getRepresentative();
  if (rep != this)
    return rep->addConformance(proto, updateExistingSource, source, builder);

  // Check whether we already know about this conformance.
  auto known = ConformsTo.find(proto);
  if (known != ConformsTo.end()) {
    // We already have this requirement. Update the requirement source
    // appropriately.
    if (updateExistingSource)
      updateRequirementSource(known->second, source);
    return false;
  }

  // Add this conformance.
  auto inserted = ConformsTo.insert(std::make_pair(proto, source)).first;

  // Determine whether there is a superclass constraint where the
  // superclass conforms to this protocol.
  ProtocolConformance *superConformance = getSuperConformance(this, proto,
                                                              inserted->second,
                                                              builder);

  RequirementSource redundantSource(RequirementSource::Redundant,
                                    source.getLoc());

  // Check whether any associated types in this protocol resolve
  // nested types of this potential archetype.
  for (auto member : proto->getMembers()) {
    auto assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (!assocType)
      continue;

    auto known = NestedTypes.find(assocType->getName());
    if (known == NestedTypes.end())
      continue;

    // If the nested type was not already resolved, do so now.
    if (!known->second.front()->getResolvedAssociatedType()) {
      known->second.front()->resolveAssociatedType(assocType, builder);

      // If there's a superclass constraint that conforms to the protocol,
      // add the appropriate same-type relationship.
      maybeAddSameTypeRequirementForNestedType(known->second.front(),
                                               redundantSource,
                                               superConformance,
                                               builder);
      continue;
    }

    // Otherwise, create a new potential archetype for this associated type
    // and make it equivalent to the first potential archetype we encountered.
    auto otherPA = new PotentialArchetype(this, assocType);
    otherPA->addSameTypeConstraint(known->second.front(), redundantSource);

    // Update the equivalence class.
    auto frontRep = known->second.front()->getRepresentative();
    otherPA->Representative = frontRep;
    frontRep->EquivalenceClass.push_back(otherPA);

    known->second.push_back(otherPA);

    // If there's a superclass constraint that conforms to the protocol,
    // add the appropriate same-type relationship.
    maybeAddSameTypeRequirementForNestedType(otherPA, redundantSource,
                                             superConformance, builder);
  }

  return true;
}

auto ArchetypeBuilder::PotentialArchetype::getRepresentative()
                                             -> PotentialArchetype *{
  // Find the representative.
  PotentialArchetype *Result = Representative;
  while (Result != Result->Representative)
    Result = Result->Representative;

  // Perform (full) path compression.
  PotentialArchetype *FixUp = this;
  while (FixUp != FixUp->Representative) {
    PotentialArchetype *Next = FixUp->Representative;
    FixUp->Representative = Result;
    FixUp = Next;
  }

  return Result;
}

/// Canonical ordering for dependent types in generic signatures.
static int compareDependentTypes(
                             ArchetypeBuilder::PotentialArchetype * const* pa,
                             ArchetypeBuilder::PotentialArchetype * const* pb) {
  auto a = *pa, b = *pb;

  // Fast-path check for equality.
  if (a == b)
    return 0;

  // Ordering is as follows:
  // - Generic params
  if (a->isGenericParam() && b->isGenericParam())
    return a->getGenericParamKey() < b->getGenericParamKey() ? -1 : +1;

  // A generic parameter is always ordered before a nested type.
  if (a->isGenericParam() != b->isGenericParam())
    return a->isGenericParam() ? -1 : +1;

  // Typealiases must be ordered *after* everything else, to ensure they
  // don't become representatives in the case where a typealias is equated
  // with an associated type.
  if (!!a->getTypeAliasDecl() != !!b->getTypeAliasDecl())
    return a->getTypeAliasDecl() ? +1 : -1;

  // - Dependent members
  auto ppa = a->getParent();
  auto ppb = b->getParent();

  // - by base, so t_0_n.`P.T` < t_1_m.`P.T`
  if (int compareBases = compareDependentTypes(&ppa, &ppb))
    return compareBases;

  // - by name, so t_n_m.`P.T` < t_n_m.`P.U`
  if (int compareNames = a->getNestedName().str().compare(
                                                      b->getNestedName().str()))
    return compareNames;

  if (auto *aa = a->getResolvedAssociatedType()) {
    if (auto *ab = b->getResolvedAssociatedType()) {
      // - by protocol, so t_n_m.`P.T` < t_n_m.`Q.T` (given P < Q)
      auto protoa = aa->getProtocol();
      auto protob = ab->getProtocol();
      if (int compareProtocols
            = ProtocolType::compareProtocols(&protoa, &protob))
        return compareProtocols;

      // Error case: if we have two associated types with the same name in the
      // same protocol, just tie-break based on address.
      if (aa != ab)
        return aa < ab ? -1 : +1;
    } else {
      // A resolved archetype is always ordered before an unresolved one.
      return -1;
    }
  } else {
    // A resolved archetype is always ordered before an unresolved one.
    if (b->getResolvedAssociatedType())
      return +1;
  }

  // Make sure typealiases are properly ordered, to avoid crashers.
  // FIXME: Ideally we would eliminate typealiases earlier.
  if (auto *aa = a->getTypeAliasDecl()) {
    auto *ab = b->getTypeAliasDecl();
    assert(ab != nullptr && "Should have handled this case above");

    // - by protocol, so t_n_m.`P.T` < t_n_m.`Q.T` (given P < Q)
    auto protoa =
      aa->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
    auto protob =
      ab->getDeclContext()->getAsProtocolOrProtocolExtensionContext();

    if (int compareProtocols
          = ProtocolType::compareProtocols(&protoa, &protob))
      return compareProtocols;

    // FIXME: Arbitrarily break the result here.
    if (aa != ab)
      return aa < ab ? -1 : +1;
  }

  // Along the error path where one or both of the potential archetypes was
  // renamed due to typo correction,
  if (a->wasRenamed() || b->wasRenamed()) {
    if (a->wasRenamed() != b->wasRenamed())
      return a->wasRenamed() ? +1 : -1;

    if (int compareNames = a->getOriginalName().str().compare(
                                                    b->getOriginalName().str()))
      return compareNames;
  }

  llvm_unreachable("potential archetype total order failure");
}

/// Determine whether there is a concrete type anywhere in the path to the root.
static bool hasConcreteTypeInPath(
                               const ArchetypeBuilder::PotentialArchetype *pa) {
  for (; pa; pa = pa->getParent()) {
    if (pa->isConcreteType()) return true;
  }

  return false;
}

/// Whether this potential archetype makes a better archetype anchor than
/// the given archetype anchor.
static bool isBetterArchetypeAnchor(
                              const ArchetypeBuilder::PotentialArchetype *pa,
                              const ArchetypeBuilder::PotentialArchetype *pb) {
  // If one potential archetype has a concrete type in its path but the other
  // does not, prefer the one that does not.
  auto aConcrete = hasConcreteTypeInPath(pa);
  auto bConcrete = hasConcreteTypeInPath(pb);
  if (aConcrete != bConcrete)
    return bConcrete;

  auto mutablePA = const_cast<ArchetypeBuilder::PotentialArchetype *>(pa);
  auto mutablePB = const_cast<ArchetypeBuilder::PotentialArchetype *>(pb);
  return compareDependentTypes(&mutablePA, &mutablePB) < 0;
}

/// Rebuild the given potential archetype based on anchors.
static ArchetypeBuilder::PotentialArchetype*rebuildPotentialArchetypeAnchor(
                                    ArchetypeBuilder::PotentialArchetype *pa,
                                    ArchetypeBuilder &builder) {
  if (auto parent = pa->getParent()) {
    auto parentAnchor =
      rebuildPotentialArchetypeAnchor(parent->getArchetypeAnchor(builder),
                                      builder);
    if (parent == parentAnchor) return pa;

    if (auto assocType = pa->getResolvedAssociatedType())
      return parentAnchor->getNestedType(assocType, builder);

    return parentAnchor->getNestedType(pa->getNestedName(), builder);
  }

  return pa;
}

auto ArchetypeBuilder::PotentialArchetype::getArchetypeAnchor(
                                                      ArchetypeBuilder &builder)
       -> PotentialArchetype * {
  // Rebuild the potential archetype anchor for this type, so the equivalence
  // class will contain the anchor.
  (void)rebuildPotentialArchetypeAnchor(this, builder);

  // Find the best archetype within this equivalence class.
  PotentialArchetype *rep = getRepresentative();
  auto best = rep;
  for (auto pa : rep->getEquivalenceClass()) {
    if (isBetterArchetypeAnchor(pa, best))
      best = pa;
  }

#ifndef NDEBUG
  // Make sure that we did, in fact, get one that is better than all others.
  for (auto pa : rep->getEquivalenceClass()) {
    assert((pa == best || isBetterArchetypeAnchor(best, pa)) &&
           !isBetterArchetypeAnchor(pa, best) &&
           "archetype anchor isn't a total order");
  }
#endif

  return best;
}

// Give a nested type the appropriately resolved concrete type, based off a
// parent PA that has a concrete type.
static void concretizeNestedTypeFromConcreteParent(
    ArchetypeBuilder::PotentialArchetype *parent,
    ArchetypeBuilder::PotentialArchetype *nestedPA, ArchetypeBuilder &builder,
    llvm::function_ref<ProtocolConformanceRef(ProtocolDecl *)>
        lookupConformance) {
  auto concreteParent = parent->getConcreteType();
  assert(concreteParent &&
         "attempting to resolve concrete nested type of non-concrete PA");

  // These requirements are all redundant: they're implied by the concrete
  // parent.
  auto Source = RequirementSource(RequirementSource::Redundant,
                                  parent->getConcreteTypeSource().getLoc());

  auto assocType = nestedPA->getResolvedAssociatedType();

  if (auto *concreteArchetype = concreteParent->getAs<ArchetypeType>()) {
    Type witnessType =
        concreteArchetype->getNestedType(nestedPA->getNestedName());
    builder.addSameTypeRequirementToConcrete(nestedPA, witnessType, Source);
  } else if (assocType) {
    auto conformance = lookupConformance(assocType->getProtocol());

    Type witnessType;
    if (conformance.isConcrete()) {
      witnessType = conformance.getConcrete()
                        ->getTypeWitness(assocType, builder.getLazyResolver())
                        .getReplacement();
    } else {
      witnessType = DependentMemberType::get(concreteParent, assocType);
    }

    if (auto witnessPA = builder.resolveArchetype(witnessType)) {
      builder.addSameTypeRequirementBetweenArchetypes(nestedPA, witnessPA,
                                                      Source);
    } else {
      builder.addSameTypeRequirementToConcrete(nestedPA, witnessType, Source);
    }
  }
}

auto ArchetypeBuilder::PotentialArchetype::getNestedType(
       Identifier nestedName,
       ArchetypeBuilder &builder) -> PotentialArchetype * {
  // If we already have a nested type with this name, return it.
  if (!NestedTypes[nestedName].empty()) {
    return NestedTypes[nestedName].front();
  }

  // Find the same nested type within the representative (unless we are
  // the representative, of course!).
  PotentialArchetype *repNested = nullptr;
  auto rep = getRepresentative();
  if (rep != this)
    repNested = rep->getNestedType(nestedName, builder);

  RequirementSource redundantSource(RequirementSource::Redundant,
                                    SourceLoc());

  // Attempt to resolve this nested type to an associated type
  // of one of the protocols to which the parent potential
  // archetype conforms.
  SmallVector<std::pair<ProtocolDecl *, RequirementSource>, 4>
    conformsTo(rep->ConformsTo.begin(), rep->ConformsTo.end());
  for (auto &conforms : conformsTo) {
    for (auto member : conforms.first->lookupDirect(nestedName)) {
      PotentialArchetype *pa;
      
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        // Resolve this nested type to this associated type.
        pa = new PotentialArchetype(this, assocType);
      } else if (auto alias = dyn_cast<TypeAliasDecl>(member)) {
        // Resolve this nested type to this type alias.
        pa = new PotentialArchetype(this, alias);
        
        if (!alias->hasInterfaceType())
          builder.getLazyResolver()->resolveDeclSignature(alias);
        if (!alias->hasInterfaceType())
          continue;

        auto type = alias->getDeclaredInterfaceType();
        if (auto existingPA = builder.resolveArchetype(type)) {
          builder.addSameTypeRequirementBetweenArchetypes(pa, existingPA,
                                                          redundantSource);
        } else {
          builder.addSameTypeRequirementToConcrete(pa, type, redundantSource);
        }
      } else
        continue;

      // If we have resolved this nested type to more than one associated
      // type, create same-type constraints between them.
      llvm::TinyPtrVector<PotentialArchetype *> &nested =
          NestedTypes[nestedName];
      if (!nested.empty()) {
        auto existing = nested.front();
        if (existing->getTypeAliasDecl() && !pa->getTypeAliasDecl()) {
          // If we found a typealias first, and now have an associatedtype
          // with the same name, it was a Swift 2 style declaration of the
          // type an inherited associatedtype should be bound to. In such a
          // case we want to make sure the associatedtype is frontmost to
          // generate generics/witness lists correctly, and the alias
          // will be unused/useless for generic constraining anyway.
          nested.insert(nested.begin(), pa);
        } else {
          nested.push_back(pa);
        }

        // Produce a same-type constraint between the two same-named
        // potential archetypes.
        pa->addSameTypeConstraint(nested.front(), redundantSource);

        auto frontRep = nested.front()->getRepresentative();
        pa->Representative = frontRep;
        frontRep->EquivalenceClass.push_back(pa);
      } else {
        nested.push_back(pa);

        if (repNested) {
          builder.addSameTypeRequirementBetweenArchetypes(pa, repNested,
                                                          redundantSource);
        }
      }

      // If there's a superclass constraint that conforms to the protocol,
      // add the appropriate same-type relationship.
      ProtocolConformance *superConformance =
        getSuperConformance(this, conforms.first, conforms.second, builder);
      maybeAddSameTypeRequirementForNestedType(pa, redundantSource,
                                               superConformance, builder);
    }
  }

  // We couldn't resolve the nested type yet, so create an
  // unresolved associated type.
  llvm::TinyPtrVector<PotentialArchetype *> &nested = NestedTypes[nestedName];
  if (nested.empty()) {
    nested.push_back(new PotentialArchetype(this, nestedName));
    ++builder.Impl->NumUnresolvedNestedTypes;
  }

  auto nestedPA = nested.front();

  // We know something concrete about the parent PA, so we need to propagate
  // that information to this new archetype.
  if (isConcreteType()) {
    concretizeNestedTypeFromConcreteParent(
        this, nestedPA, builder,
        [&](ProtocolDecl *proto) -> ProtocolConformanceRef {
          auto depTy = nestedPA->getDependentType({}, /*allowUnresolved=*/true)
                           ->getCanonicalType();
          auto protocolTy =
              proto->getDeclaredInterfaceType()->castTo<ProtocolType>();
          auto conformance = builder.getLookupConformanceFn()(
              depTy, getConcreteType(), protocolTy);
          assert(conformance &&
                 "failed to find PA's conformance to known protocol");
          return *conformance;
        });
  }

  return nestedPA;
}

auto ArchetypeBuilder::PotentialArchetype::getNestedType(
                            AssociatedTypeDecl *assocType,
                            ArchetypeBuilder &builder) -> PotentialArchetype * {
  // Add the requirement that this type conform to the protocol of the
  // associated type. We treat this as "inferred" because it comes from the
  // structure of the type---there will be an explicit or implied requirement
  // somewhere else.
  bool failed = builder.addConformanceRequirement(
                  this, assocType->getProtocol(),
                  RequirementSource(RequirementSource::Inferred, SourceLoc()));
  (void)failed;

  // Trigger the construction of nested types with this name.
  auto fallback = getNestedType(assocType->getName(), builder);

  // Find the nested type that resolved to this associated type.
  for (const auto &nested : NestedTypes[assocType->getName()]) {
    if (nested->getResolvedAssociatedType() == assocType) return nested;
  }

  assert(failed && "unable to find nested type that we know is there");
  return fallback;
}

Type ArchetypeBuilder::PotentialArchetype::getTypeInContext(
                                               ArchetypeBuilder &builder,
                                               GenericEnvironment *genericEnv) {
  ArrayRef<GenericTypeParamType *> genericParams =
    genericEnv->getGenericParams();

  // Retrieve the archetype from the archetype anchor in this equivalence class.
  // The anchor must not have any concrete parents (otherwise we would just
  // use the representative).
  auto archetypeAnchor = getArchetypeAnchor(builder);
  if (archetypeAnchor != this)
    return archetypeAnchor->getTypeInContext(builder, genericEnv);

  auto representative = getRepresentative();
  ASTContext &ctx = genericEnv->getGenericSignature()->getASTContext();

  // Return a concrete type or archetype we've already resolved.
  if (Type concreteType = representative->getConcreteType()) {
    // Otherwise, substitute in the archetypes in the environment.
    // If this has a recursive type, return an error type.
    if (representative->RecursiveConcreteType) {
      return ErrorType::get(getDependentType(genericParams,
                                             /*allowUnresolved=*/true));
    }

    return genericEnv->mapTypeIntoContext(concreteType,
                                          builder.getLookupConformanceFn());
  }

  // Local function to check whether we have a generic parameter that has
  // already been recorded
  auto getAlreadyRecoveredGenericParam = [&]() -> Type {
    if (!isGenericParam()) return Type();

    auto type = genericEnv->getMappingIfPresent(getGenericParamKey());
    if (!type) return Type();

    // We already have a mapping for this generic parameter in the generic
    // environment. Return it.
    return *type;
  };

  AssociatedTypeDecl *assocType = nullptr;
  ArchetypeType *ParentArchetype = nullptr;
  if (auto parent = getParent()) {
    // For nested types, first substitute into the parent so we can form the
    // proper nested type.
    auto parentTy = parent->getTypeInContext(builder, genericEnv);
    if (!parentTy)
      return ErrorType::get(getDependentType(genericParams,
                                             /*allowUnresolved=*/true));

    ParentArchetype = parentTy->getAs<ArchetypeType>();
    if (!ParentArchetype) {
      LazyResolver *resolver = ctx.getLazyResolver();
      assert(resolver && "need a lazy resolver");
      (void) resolver;

      // Resolve the member type.
      auto type = getDependentType(genericParams, /*allowUnresolved=*/false);
      if (type->hasError())
        return type;

      auto depMemberType = type->castTo<DependentMemberType>();
      Type memberType =
        depMemberType->substBaseType(parentTy,
                                     builder.getLookupConformanceFn());

      // If the member type maps to an archetype, resolve that archetype.
      if (auto memberPA = builder.resolveArchetype(memberType)) {
        if (memberPA->getRepresentative() != representative) {
          return memberPA->getTypeInContext(builder, genericEnv);
        }

        llvm_unreachable("we have no parent archetype");
      }


      // Otherwise, it's a concrete type.
      return genericEnv->mapTypeIntoContext(memberType,
                                            builder.getLookupConformanceFn());
    }

    // Check whether the parent already has a nested type with this name. If
    // so, return it directly.
    if (auto nested = ParentArchetype->getNestedTypeIfKnown(getNestedName()))
      return *nested;

    // We will build the archetype below.
    assocType = getResolvedAssociatedType();
  } else if (auto result = getAlreadyRecoveredGenericParam()) {
    return result;
  }

  // Determine the superclass for the archetype. If it exists and involves
  // type parameters, substitute them.
  Type superclass = representative->getSuperclass();
  if (superclass && superclass->hasTypeParameter()) {
    if (representative->RecursiveSuperclassType) {
      superclass = ErrorType::get(superclass);
    } else {
      superclass = genericEnv->mapTypeIntoContext(
                                              superclass,
                                              builder.getLookupConformanceFn());

      // We might have recursively recorded the archetype; if so, return early.
      // FIXME: This should be detectable before we end up building archetypes.
      if (auto result = getAlreadyRecoveredGenericParam())
        return result;
    }
  }

  LayoutConstraint layout = representative->getLayout();

  // Build a new archetype.

  // Collect the protocol conformances for the archetype.
  SmallVector<ProtocolDecl *, 4> Protos;
  for (const auto &conforms : representative->getConformsTo()) {
    switch (conforms.second.getKind()) {
    case RequirementSource::Explicit:
    case RequirementSource::Inferred:
    case RequirementSource::Protocol:
    case RequirementSource::ProtocolRequirementSignatureSelf:
    case RequirementSource::Redundant:
      Protos.push_back(conforms.first);
      break;

    case RequirementSource::Inherited:
      // Inherited conformances are recoverable from the superclass
      // constraint.
      break;
    }
  }

  // Create the archetype.
  //
  // Note that we delay the computation of the superclass until after we
  // create the archetype, in case the superclass references the archetype
  // itself.
  ArchetypeType *arch;
  if (ParentArchetype) {
    // If we were unable to resolve this as an associated type, produce an
    // error type.
    if (!assocType) {
      return ErrorType::get(getDependentType(genericParams,
                                             /*allowUnresolved=*/true));
    }

    // Create a nested archetype.
    arch = ArchetypeType::getNew(ctx, ParentArchetype, assocType, Protos,
                                 superclass, layout);

    // Register this archetype with its parent.
    ParentArchetype->registerNestedType(getNestedName(), arch);
  } else {
    // Create a top-level archetype.
    Identifier name =
      genericParams[getGenericParamKey().findIndexIn(genericParams)]->getName();
    arch = ArchetypeType::getNew(ctx, genericEnv, name, Protos,
                                 superclass, layout);

    // Register the archetype with the generic environment.
    genericEnv->addMapping(getGenericParamKey(), arch);
  }

  return arch;
}

void ArchetypeType::resolveNestedType(
                                    std::pair<Identifier, Type> &nested) const {
  auto genericEnv = getGenericEnvironment();
  auto &builder = *genericEnv->getArchetypeBuilder();

  Type interfaceType =
    genericEnv->mapTypeOutOfContext(const_cast<ArchetypeType *>(this));
  auto parentPA = builder.resolveArchetype(interfaceType);
  auto memberPA = parentPA->getNestedType(nested.first, builder);
  auto result = memberPA->getTypeInContext(builder, genericEnv);
  assert(!nested.second ||
         nested.second->isEqual(result) ||
         (nested.second->hasError() && result->hasError()));
  nested.second = result;
}

Type ArchetypeBuilder::PotentialArchetype::getDependentType(
                                ArrayRef<GenericTypeParamType *> genericParams,
                                bool allowUnresolved) {
  if (auto parent = getParent()) {
    Type parentType = parent->getDependentType(genericParams,
                                               allowUnresolved);
    if (parentType->hasError())
      return parentType;

    // If we've resolved to an associated type, use it.
    if (auto assocType = getResolvedAssociatedType())
      return DependentMemberType::get(parentType, assocType);

    // If we don't allow unresolved dependent member types, fail.
    if (!allowUnresolved)
      return ErrorType::get(getDependentType(genericParams,
                                             /*allowUnresolved=*/true));

    return DependentMemberType::get(parentType, getNestedName());
  }
  
  assert(isGenericParam() && "Not a generic parameter?");

  unsigned index = getGenericParamKey().findIndexIn(genericParams);

  // FIXME: This is a temporary workaround.
  if (genericParams.empty()) {
    return getBuilder()->Impl->GenericParams[index];
  }

  return genericParams[index];
}

void ArchetypeBuilder::PotentialArchetype::dump(llvm::raw_ostream &Out,
                                                SourceManager *SrcMgr,
                                                unsigned Indent) {
  // Print name.
  if (Indent == 0 || isGenericParam())
    Out << getDebugName();
  else
    Out.indent(Indent) << getNestedName();

  // Print superclass.
  if (Superclass) {
    Out << " : ";
    Superclass.print(Out);
    Out << " [";
    SuperclassSource->dump(Out, SrcMgr);
    Out << "]";
  }

  // Print requirements.
  if (!ConformsTo.empty()) {
    Out << " : ";

    bool First = true;
    for (const auto &ProtoAndSource : ConformsTo) {
      if (First)
        First = false;
      else
        Out << " & ";

      Out << ProtoAndSource.first->getName().str() << " [";
      ProtoAndSource.second.dump(Out, SrcMgr);
      Out << "]";
    }
  }

  if (Representative != this) {
    Out << " [represented by " << getRepresentative()->getDebugName() << "]";
  }

  if (EquivalenceClass.size() > 1) {
    Out << " [equivalence class ";
    bool isFirst = true;
    for (auto equiv : EquivalenceClass) {
      if (equiv == this) continue;

      if (isFirst) isFirst = false;
      else Out << ", ";

      Out << equiv->getDebugName();
    }
    Out << "]";
  }

  Out << "\n";

  // Print nested types.
  for (const auto &nestedVec : NestedTypes) {
    for (auto nested : nestedVec.second) {
      nested->dump(Out, SrcMgr, Indent + 2);
    }
  }
}

ArchetypeBuilder::ArchetypeBuilder(
                               ASTContext &ctx,
                               std::function<GenericFunction> lookupConformance)
  : Context(ctx), Diags(Context.Diags), Impl(new Implementation) {
  Impl->LookupConformance = std::move(lookupConformance);
}

ArchetypeBuilder::ArchetypeBuilder(ArchetypeBuilder &&) = default;

ArchetypeBuilder::~ArchetypeBuilder() {
  if (!Impl)
    return;

  for (auto PA : Impl->PotentialArchetypes)
    delete PA;
}

std::function<GenericFunction>
ArchetypeBuilder::getLookupConformanceFn() const {
  return Impl->LookupConformance;
}

LazyResolver *ArchetypeBuilder::getLazyResolver() const { 
  return Context.getLazyResolver();
}

auto ArchetypeBuilder::resolveArchetype(Type type, PotentialArchetype *basePA)
    -> PotentialArchetype * {
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    if (basePA)
      return basePA;

    unsigned index = GenericParamKey(genericParam).findIndexIn(
                                                           Impl->GenericParams);
    if (index < Impl->GenericParams.size())
      return Impl->PotentialArchetypes[index];

    return nullptr;
  }

  if (auto dependentMember = type->getAs<DependentMemberType>()) {
    auto base = resolveArchetype(dependentMember->getBase(), basePA);
    if (!base)
      return nullptr;

    if (auto assocType = dependentMember->getAssocType())
      return base->getNestedType(assocType, *this);

    return base->getNestedType(dependentMember->getName(), *this);
  }

  return nullptr;
}

void ArchetypeBuilder::addGenericParameter(GenericTypeParamDecl *GenericParam) {
  addGenericParameter(
     GenericParam->getDeclaredInterfaceType()->castTo<GenericTypeParamType>());
}

bool ArchetypeBuilder::addGenericParameterRequirements(GenericTypeParamDecl *GenericParam) {
  GenericParamKey Key(GenericParam);
  auto PA = Impl->PotentialArchetypes[Key.findIndexIn(Impl->GenericParams)];
  
  // Add the requirements from the declaration.
  llvm::SmallPtrSet<ProtocolDecl *, 8> visited;
  return addAbstractTypeParamRequirements(GenericParam, PA,
                                          RequirementSource::Explicit,
                                          visited);
}

void ArchetypeBuilder::addGenericParameter(GenericTypeParamType *GenericParam) {
  GenericParamKey Key(GenericParam);
  assert(Impl->GenericParams.empty() ||
         ((Key.Depth == Impl->GenericParams.back()->getDepth() &&
           Key.Index == Impl->GenericParams.back()->getIndex() + 1) ||
          (Key.Depth > Impl->GenericParams.back()->getDepth() &&
           Key.Index == 0)));

  // Create a potential archetype for this type parameter.
  auto PA = new PotentialArchetype(this, GenericParam);
  Impl->GenericParams.push_back(GenericParam);
  Impl->PotentialArchetypes.push_back(PA);
}

bool ArchetypeBuilder::addConformanceRequirement(PotentialArchetype *PAT,
                                                 ProtocolDecl *Proto,
                                                 RequirementSource Source) {
  llvm::SmallPtrSet<ProtocolDecl *, 8> Visited;
  return addConformanceRequirement(PAT, Proto, Source, Visited);
}

bool ArchetypeBuilder::addConformanceRequirement(PotentialArchetype *PAT,
                                                 ProtocolDecl *Proto,
                                                 RequirementSource Source,
                               llvm::SmallPtrSetImpl<ProtocolDecl *> &Visited) {
  // Add the requirement to the representative.
  auto T = PAT->getRepresentative();

  // Add the requirement, if we haven't done so already.
  if (!T->addConformance(Proto, /*updateExistingSource=*/true, Source, *this))
    return false;

  bool inserted = Visited.insert(Proto).second;
  assert(inserted);
  (void) inserted;

  // Requirements introduced by the main protocol are explicit in a protocol
  // requirement signature, but inferred from this conformance otherwise.
  auto Kind =
      Source.getKind() == RequirementSource::ProtocolRequirementSignatureSelf
          ? RequirementSource::Explicit
          : RequirementSource::Protocol;

  // Use the requirement signature to avoid rewalking the entire protocol.  This
  // cannot compute the requirement signature directly, because that may be
  // infinitely recursive: this code is also used to construct it.
  if (Proto->isRequirementSignatureComputed()) {
    auto reqSig = Proto->getRequirementSignature();

    for (auto req : reqSig->getRequirements()) {
      RequirementSource InnerSource(Kind, Source.getLoc());
      addRequirement(req, InnerSource, T, Visited);
    }
  } else {
    // Conformances to inherit protocols are explicit in a protocol requirement
    // signature, but inferred from this conformance otherwise.
    auto InnerKind =
        Source.getKind() == RequirementSource::ProtocolRequirementSignatureSelf
            ? RequirementSource::Explicit
            : RequirementSource::Redundant;
    RequirementSource InnerSource(InnerKind, Source.getLoc());
    // Add all of the inherited protocol requirements, recursively.
    if (auto resolver = getLazyResolver())
      resolver->resolveInheritedProtocols(Proto);

    for (auto InheritedProto :
         Proto->getInheritedProtocols(getLazyResolver())) {
      if (Visited.count(InheritedProto)) {
        markPotentialArchetypeRecursive(T, InheritedProto, InnerSource);
        continue;
      }
      if (addConformanceRequirement(T, InheritedProto, InnerSource, Visited))
        return true;
    }

    // Add requirements for each of the associated types.
    for (auto Member : Proto->getMembers()) {
      if (auto AssocType = dyn_cast<AssociatedTypeDecl>(Member)) {
        // Add requirements placed directly on this associated type.
        auto AssocPA = T->getNestedType(AssocType, *this);

        if (AssocPA != T) {
          if (addAbstractTypeParamRequirements(AssocType, AssocPA, Kind,
                                               Visited))
            return true;
        }

        continue;
      }

      // FIXME: Requirement declarations.
    }
  }

  Visited.erase(Proto);
  return false;
}

bool ArchetypeBuilder::addLayoutRequirement(PotentialArchetype *PAT,
                                            LayoutConstraint Layout,
                                            RequirementSource Source) {
  // Add the requirement to the representative.
  auto T = PAT->getRepresentative();

  if (T->Layout) {
    if (T->Layout == Layout) {
      // Update the source.
      T->LayoutSource = Source;
      return false;
    }
    // There is an existing layout constraint for this archetype.
    Diags.diagnose(Source.getLoc(), diag::mutiple_layout_constraints,
                   Layout, T->Layout);
    Diags.diagnose(T->LayoutSource->getLoc(),
                   diag::previous_layout_constraint, T->Layout);
    T->setInvalid();

    return true;
  }

  T->Layout = Layout;
  T->LayoutSource = Source;

  return false;
}

bool ArchetypeBuilder::addSuperclassRequirement(PotentialArchetype *T,
                                                Type Superclass,
                                                RequirementSource Source) {
  T = T->getRepresentative();

  // Make sure the concrete type fulfills the superclass requirement
  // of the archetype.
  if (T->isConcreteType()) {
    Type concrete = T->getConcreteType();
    if (!Superclass->isExactSuperclassOf(concrete, getLazyResolver())) {
      Diags.diagnose(T->getConcreteTypeSource().getLoc(),
                     diag::type_does_not_inherit,
                     T->getDependentType(/*FIXME:*/{ },
                                         /*allowUnresolved=*/true),
                     concrete, Superclass)
        .highlight(Source.getLoc());
      return true;
    }

    return false;
  }

  // Local function to handle the update of superclass conformances
  // when the superclass constraint changes.
  auto updateSuperclassConformances = [&] {
    for (auto &conforms : T->ConformsTo) {
      if (auto superConformance = getSuperConformance(T, conforms.first,
                                                      conforms.second, *this)) {
        for (auto req : conforms.first->getMembers()) {
          auto assocType = dyn_cast<AssociatedTypeDecl>(req);
          if (!assocType) continue;

          const auto &nestedTypes = T->getNestedTypes();
          auto nested = nestedTypes.find(assocType->getName());
          if (nested == nestedTypes.end()) continue;

          RequirementSource redundantSource(RequirementSource::Inherited,
                                            Source.getLoc());

          for (auto nestedPA : nested->second) {
            if (nestedPA->getResolvedAssociatedType() == assocType)
              maybeAddSameTypeRequirementForNestedType(nestedPA,
                                                       redundantSource,
                                                       superConformance, *this);
          }
        }
      }
    }
  };

  // If T already has a superclass, make sure it's related.
  if (T->Superclass) {
    // TODO: In principle, this could be isBindableToSuperclassOf instead of
    // isExactSubclassOf. If you had:
    //
    //   class Foo<T>
    //   class Bar: Foo<Int>
    //
    //   func foo<T, U where U: Foo<T>, U: Bar>(...) { ... }
    //
    // then the second constraint should be allowed, constraining U to Bar
    // and secondarily imposing a T == Int constraint.
    if (T->Superclass->isExactSuperclassOf(Superclass, nullptr)) {
      T->Superclass = Superclass;

      // We've strengthened the bound, so update superclass conformances.
      updateSuperclassConformances();
    // TODO: Similar to the above, a more general isBindableToSuperclassOf
    // base class constraint could potentially introduce secondary constraints.
    // If you had:
    //
    //   class Foo<T>
    //   class Bar: Foo<Int>
    //
    //   func foo<T, U where U: Bar, U: Foo<T>>(...) { ... }
    //
    // then the second `U: Foo<T>` constraint introduces a `T == Int`
    // constraint.
    } else if (!Superclass->isExactSuperclassOf(T->Superclass, nullptr)) {
      Diags.diagnose(Source.getLoc(),
                     diag::requires_superclass_conflict,
                     T->getDependentType(/*FIXME: */{ }, true),
                     T->Superclass, Superclass)
        .highlight(T->SuperclassSource->getLoc());
      return true;
    }

    updateRequirementSource(*T->SuperclassSource, Source);
    return false;
  }

  // Set the superclass.
  T->Superclass = Superclass;
  T->SuperclassSource = Source;

  // Update based on these conformances.
  updateSuperclassConformances();
  return false;
}

void ArchetypeBuilder::PotentialArchetype::addSameTypeConstraint(
                                             PotentialArchetype *otherPA,
                                             const RequirementSource &source) {
  // If the types are the same, there's nothing to do.
  if (this == otherPA) return;

  // Update the same-type constraints of this PA to reference the other PA.
  auto insertedIntoThis = SameTypeConstraints.insert({otherPA, source});
  if (!insertedIntoThis.second)
    updateRequirementSource(insertedIntoThis.first->second, source);

  // Update the same-type constraints of the other PA to reference this PA.
  auto insertedIntoOther = otherPA->SameTypeConstraints.insert({this, source});
  if (!insertedIntoOther.second)
    updateRequirementSource(insertedIntoOther.first->second, source);
}

bool ArchetypeBuilder::addSameTypeRequirementBetweenArchetypes(
       PotentialArchetype *OrigT1,
       PotentialArchetype *OrigT2,
       RequirementSource Source) 
{
  // Record the same-type constraint.
  OrigT1->addSameTypeConstraint(OrigT2, Source);

  // Operate on the representatives
  auto T1 = OrigT1->getRepresentative();
  auto T2 = OrigT2->getRepresentative();

  // If the representatives are already the same, we're done.
  if (T1 == T2)
    return false;

  // Decide which potential archetype is to be considered the representative.
  // It doesn't specifically matter which we use, but it's a minor optimization
  // to prefer the canonical type.
  if (compareDependentTypes(&T2, &T1) < 0)
    std::swap(T1, T2);

  // Merge any concrete constraints.
  Type concrete1 = T1->getConcreteType();
  Type concrete2 = T2->getConcreteType();
  
  if (concrete1 && concrete2) {
    bool mismatch =
      addSameTypeRequirement(concrete1, concrete2, Source,
        [&](Type type1, Type type2) {
          Diags.diagnose(Source.getLoc(),
                         diag::requires_same_type_conflict,
                         T1->getDependentType(/*FIXME: */{ }, true), type1,
                         type2);

        });

    if (mismatch) return true;
  } else if (concrete1) {
    assert(!T2->ConcreteType
           && "already formed archetype for concrete-constrained parameter");
    T2->ConcreteType = concrete1;
    T2->ConcreteTypeSource = T1->ConcreteTypeSource;
  } else if (concrete2) {
    assert(!T1->ConcreteType
           && "already formed archetype for concrete-constrained parameter");
    T1->ConcreteType = concrete2;
    T1->ConcreteTypeSource = T2->ConcreteTypeSource;
  }

  // Don't mark requirements as redundant if they come from one of our
  // child archetypes. This is a targeted fix -- more general cases
  // continue to break. In general, we need to detect cycles in the
  // archetype graph and not propagate requirement source information
  // along back edges.
  bool creatingCycle = false;
  auto T2Parent = T2;
  while (T2Parent != nullptr) {
    if (T2Parent->getRepresentative() == T1)
      creatingCycle = true;
    T2Parent = T2Parent->getParent();
  }

  // Make T1 the representative of T2, merging the equivalence classes.
  T2->Representative = T1;
  for (auto equiv : T2->EquivalenceClass)
    T1->EquivalenceClass.push_back(equiv);

  // Superclass requirements.
  if (T2->Superclass) {
    addSuperclassRequirement(T1, T2->getSuperclass(),
                             T2->getSuperclassSource());
  }

  // Add all of the protocol conformance requirements of T2 to T1.
  for (auto conforms : T2->ConformsTo) {
    T1->addConformance(conforms.first, /*updateExistingSource=*/!creatingCycle,
                       conforms.second, *this);
  }

  // Recursively merge the associated types of T2 into T1.
  RequirementSource redundantSource(RequirementSource::Redundant, SourceLoc());
  for (auto equivT2 : T2->EquivalenceClass) {
    for (auto T2Nested : equivT2->NestedTypes) {
      auto T1Nested = T1->getNestedType(T2Nested.first, *this);
      if (addSameTypeRequirementBetweenArchetypes(T1Nested,
                                                  T2Nested.second.front(),
                                                  redundantSource))
        return true;
    }
  }

  return false;
}

bool ArchetypeBuilder::addSameTypeRequirementToConcrete(
       PotentialArchetype *T,
       Type Concrete,
       RequirementSource Source) {
  // Operate on the representative.
  T = T->getRepresentative();
  
  // If we've already been bound to a type, we're either done, or we have a
  // problem.
  if (auto oldConcrete = T->getConcreteType()) {
    bool mismatch =
      addSameTypeRequirement(oldConcrete, Concrete, Source,
        [&](Type type1, Type type2) {
          Diags.diagnose(Source.getLoc(),
                         diag::requires_same_type_conflict,
                         T->getDependentType(/*FIXME: */{ }, true), type1,
                         type2);

        });

    if (mismatch) return true;
    return false;
  }
  
  // Make sure the concrete type fulfills the requirements on the archetype.
  DenseMap<ProtocolDecl *, ProtocolConformanceRef> conformances;
  if (!Concrete->is<ArchetypeType>()) {
    CanType depTy = T->getDependentType({ }, /*allowUnresolved=*/true)
                      ->getCanonicalType();
    for (auto conforms : T->getConformsTo()) {
      auto protocol = conforms.first;
      auto conformance =
        getLookupConformanceFn()(depTy, Concrete,
                                 protocol->getDeclaredInterfaceType()
                                   ->castTo<ProtocolType>());
      if (!conformance) {
        Diags.diagnose(Source.getLoc(),
                       diag::requires_generic_param_same_type_does_not_conform,
                       Concrete, protocol->getName());
        return true;
      }

      conformances.insert({protocol, *conformance});
    }
  }
  
  // Record the requirement.
  T->ConcreteType = Concrete;
  T->ConcreteTypeSource = Source;

  // Make sure the concrete type fulfills the superclass requirement
  // of the archetype.
  RequirementSource redundantSource(RequirementSource::Redundant,
                                    Source.getLoc());
  if (T->Superclass) {
    if (!T->Superclass->isExactSuperclassOf(Concrete, getLazyResolver())) {
      Diags.diagnose(Source.getLoc(), diag::type_does_not_inherit,
                     T->getDependentType(/*FIXME: */{ },
                                         /*allowUnresolved=*/true),
                     Concrete, T->Superclass)
        .highlight(T->SuperclassSource->getLoc());
      return true;
    }

    // The superclass requirement is made redundant by the concrete type
    // assignment.
    updateRequirementSource(*T->SuperclassSource, redundantSource);
  }

  // Eagerly resolve any existing nested types to their concrete forms (others
  // will be "concretized" as they are constructed, in getNestedType).
  for (auto equivT : T->EquivalenceClass) {
    for (auto nested : equivT->getNestedTypes()) {
      concretizeNestedTypeFromConcreteParent(
          equivT, nested.second.front(), *this,
          [&](ProtocolDecl *proto) -> ProtocolConformanceRef {
            return conformances.find(proto)->second;
          });
    }
  }

  return false;
}

bool ArchetypeBuilder::addSameTypeRequirement(
                        Type Reqt1, Type Reqt2,
                        RequirementSource Source,
                        PotentialArchetype *basePA,
                        llvm::function_ref<void(Type, Type)> diagnoseMismatch) {
  // Find the potential archetypes.
  PotentialArchetype *T1 = resolveArchetype(Reqt1, basePA);
  PotentialArchetype *T2 = resolveArchetype(Reqt2, basePA);
  
  // If both sides of the requirement are type parameters, equate them.
  if (T1 && T2)
    return addSameTypeRequirementBetweenArchetypes(T1, T2, Source);
  
  // If just one side is a type parameter, map it to a concrete type.
  if (T1)
    return addSameTypeRequirementToConcrete(T1, Reqt2, Source);
  if (T2)
    return addSameTypeRequirementToConcrete(T2, Reqt1, Source);

  // Otherwise, the types should be equal.
  if (Reqt1->isEqual(Reqt2)) return false;

  diagnoseMismatch(Reqt1, Reqt2);
  return true;
}

// Local function to mark the given associated type as recursive,
// diagnosing it if this is the first such occurrence.
void ArchetypeBuilder::markPotentialArchetypeRecursive(
    PotentialArchetype *pa, ProtocolDecl *proto, RequirementSource source) {
  if (pa->isRecursive())
    return;
  pa->setIsRecursive();

  // FIXME: Drop this protocol.
  pa->addConformance(proto, /*updateExistingSource=*/true, source, *this);
  if (!pa->getParent())
    return;

  auto assocType = pa->getResolvedAssociatedType();
  if (!assocType || assocType->isInvalid())
    return;

  Diags.diagnose(assocType->getLoc(), diag::recursive_requirement_reference);

  // Silence downstream errors referencing this associated type.
  assocType->setInvalid();
}

bool ArchetypeBuilder::addAbstractTypeParamRequirements(
       AbstractTypeParamDecl *decl,
       PotentialArchetype *pa,
       RequirementSource::Kind kind,
       llvm::SmallPtrSetImpl<ProtocolDecl *> &visited) {
  if (isa<AssociatedTypeDecl>(decl) &&
      decl->hasInterfaceType() &&
      decl->getInterfaceType()->is<ErrorType>())
    return false;

  // Otherwise, walk the 'inherited' list to identify requirements.
  if (auto resolver = getLazyResolver())
    resolver->resolveInheritanceClause(decl);
  return visitInherited(decl->getInherited(), [&](Type inheritedType,
                                                  SourceLoc loc) -> bool {
    RequirementSource source(kind, loc);
    // Protocol requirement.
    if (auto protocolType = inheritedType->getAs<ProtocolType>()) {
      if (visited.count(protocolType->getDecl())) {
        markPotentialArchetypeRecursive(pa, protocolType->getDecl(), source);

        return true;
      }

      return addConformanceRequirement(pa, protocolType->getDecl(), source,
                                       visited);
    }

    // Superclass requirement.
    if (inheritedType->getClassOrBoundGenericClass()) {
      return addSuperclassRequirement(pa, inheritedType, source);
    }

    // Note: anything else is an error, to be diagnosed later.
    return false;
  });
}

bool ArchetypeBuilder::visitInherited(
       ArrayRef<TypeLoc> inheritedTypes,
       llvm::function_ref<bool(Type, SourceLoc)> visitor) {
  // Local function that (recursively) adds inherited types.
  bool isInvalid = false;
  std::function<void(Type, SourceLoc)> visitInherited;
  visitInherited = [&](Type inheritedType, SourceLoc loc) {
    // Decompose protocol compositions.
    if (auto compositionType
          = inheritedType->getAs<ProtocolCompositionType>()) {
      for (auto protoType : compositionType->getProtocols())
        visitInherited(protoType, loc);
      return;
    }

    isInvalid |= visitor(inheritedType, loc);
  };

  // Visit all of the inherited types.
  for (auto inherited : inheritedTypes) {
    visitInherited(inherited.getType(), inherited.getLoc());
  }

  return isInvalid;
}

bool ArchetypeBuilder::addRequirement(const RequirementRepr &Req) {
  switch (Req.getKind()) {
  case RequirementReprKind::LayoutConstraint: {
    // FIXME: Need to do something here.
    PotentialArchetype *PA = resolveArchetype(Req.getSubject());
    if (!PA) {
      // FIXME: Poor location information.
      // FIXME: Delay diagnostic until after type validation?
      Diags.diagnose(Req.getColonLoc(), diag::requires_not_suitable_archetype,
                     0, Req.getSubjectLoc(), 0);
      return true;
    }

    RequirementSource source(
        RequirementSource::Explicit,
        Req.getLayoutConstraintLoc().getSourceRange().Start);

    if (addLayoutRequirement(PA, Req.getLayoutConstraint(), source))
      return true;

    return false;
  }

  case RequirementReprKind::TypeConstraint: {
    PotentialArchetype *PA = resolveArchetype(Req.getSubject());
    if (!PA) {
      // FIXME: Poor location information.
      // FIXME: Delay diagnostic until after type validation?
      Diags.diagnose(Req.getColonLoc(), diag::requires_not_suitable_archetype,
                     0, Req.getSubjectLoc(), 0);
      return true;
    }

    // Check whether this is a supertype requirement.
    RequirementSource source(RequirementSource::Explicit,
                             Req.getConstraintLoc().getSourceRange().Start);
    if (Req.getConstraint()->getClassOrBoundGenericClass()) {
      return addSuperclassRequirement(PA, Req.getConstraint(), source);
    }

    SmallVector<ProtocolDecl *, 4> ConformsTo;
    if (!Req.getConstraint()->isExistentialType(ConformsTo)) {
      // FIXME: Diagnose this failure here, rather than over in type-checking.
      return true;
    }

    // Add each of the protocols.
    for (auto Proto : ConformsTo)
      if (addConformanceRequirement(PA, Proto, source))
        return true;

    return false;
  }

  case RequirementReprKind::SameType:
    // Require that at least one side of the requirement be a type parameter.
    if (!Req.getFirstType()->isTypeParameter() &&
        !Req.getSecondType()->isTypeParameter()) {
      Diags.diagnose(Req.getEqualLoc(), diag::requires_no_same_type_archetype)
        .highlight(Req.getFirstTypeLoc().getSourceRange())
        .highlight(Req.getSecondTypeLoc().getSourceRange());
      return true;
    }

    return addRequirement(Requirement(RequirementKind::SameType,
                                      Req.getFirstType(),
                                      Req.getSecondType()),
                          RequirementSource(RequirementSource::Explicit,
                                            Req.getEqualLoc()));
  }

  llvm_unreachable("Unhandled requirement?");
}

bool ArchetypeBuilder::addRequirement(const Requirement &req,
                                      RequirementSource source) {
  llvm::SmallPtrSet<ProtocolDecl *, 8> Visited;
  return addRequirement(req, source, nullptr, Visited);
}

bool ArchetypeBuilder::addRequirement(
    const Requirement &req, RequirementSource source,
    PotentialArchetype *basePA,
    llvm::SmallPtrSetImpl<ProtocolDecl *> &Visited) {
  switch (req.getKind()) {
  case RequirementKind::Superclass: {
    // FIXME: Diagnose this.
    PotentialArchetype *pa = resolveArchetype(req.getFirstType(), basePA);
    if (!pa) return false;

    assert(req.getSecondType()->getClassOrBoundGenericClass());
    return addSuperclassRequirement(pa, req.getSecondType(), source);
  }

  case RequirementKind::Layout: {
    // FIXME: Diagnose this.
    PotentialArchetype *pa = resolveArchetype(req.getFirstType(), basePA);
    if (!pa) return false;

    return addLayoutRequirement(pa, req.getLayoutConstraint(), source);
  }

  case RequirementKind::Conformance: {
    // FIXME: Diagnose this.
    PotentialArchetype *pa = resolveArchetype(req.getFirstType(), basePA);
    if (!pa) return false;

    SmallVector<ProtocolDecl *, 4> conformsTo;
    (void)req.getSecondType()->isExistentialType(conformsTo);

    // Add each of the protocols.
    for (auto proto : conformsTo) {
      if (Visited.count(proto)) {
        markPotentialArchetypeRecursive(pa, proto, source);
        continue;
      }
      if (addConformanceRequirement(pa, proto, source, Visited)) return true;
    }

    return false;
  }

  case RequirementKind::SameType:
    return addSameTypeRequirement(
             req.getFirstType(), req.getSecondType(), source, basePA,
             [&](Type type1, Type type2) {
               if (source.getLoc().isValid())
                 Diags.diagnose(source.getLoc(),
                                diag::requires_same_concrete_type, type1,
                                type2);
             });
  }

  llvm_unreachable("Unhandled requirement?");
}

/// AST walker that infers requirements from type representations.
class ArchetypeBuilder::InferRequirementsWalker : public TypeWalker {
  ArchetypeBuilder &Builder;
  SourceLoc Loc;
  unsigned MinDepth;
  unsigned MaxDepth;

  /// We cannot add requirements to archetypes from outer generic parameter
  /// lists.
  bool isOuterArchetype(PotentialArchetype *PA) {
    unsigned ParamDepth = PA->getRootGenericParamKey().Depth;
    assert(ParamDepth <= MaxDepth);
    return ParamDepth < MinDepth;
  }

public:
  InferRequirementsWalker(ArchetypeBuilder &builder,
                          SourceLoc loc,
                          unsigned MinDepth,
                          unsigned MaxDepth)
    : Builder(builder), Loc(loc), MinDepth(MinDepth), MaxDepth(MaxDepth) { }

  Action walkToTypePost(Type ty) override {
    auto boundGeneric = ty->getAs<BoundGenericType>();
    if (!boundGeneric)
      return Action::Continue; 

    auto genericSig = boundGeneric->getDecl()->getGenericSignature();
    if (!genericSig)
      return Action::Stop;

    /// Retrieves the type substitution.
    auto args = boundGeneric->getGenericArgs();
    auto genericSigDepth =
      genericSig->getInnermostGenericParams().front()->getDepth();
    auto getTypeSubstitution = [&](SubstitutableType *dependentType) -> Type {
      if (auto gp = dyn_cast<GenericTypeParamType>(dependentType)) {
        if (gp->getDepth() == genericSigDepth)
          return args[gp->getIndex()];

        return gp;
      }

      return dependentType;
    };

    // Handle the requirements.
    RequirementSource source(RequirementSource::Inferred, Loc);
    for (const auto &req : genericSig->getRequirements()) {
      switch (req.getKind()) {
      case RequirementKind::SameType: {
        auto firstType = req.getFirstType().subst(
                           getTypeSubstitution,
                           Builder.getLookupConformanceFn());
        if (!firstType)
          break;

        auto firstPA = Builder.resolveArchetype(firstType);

        if (firstPA && isOuterArchetype(firstPA))
          return Action::Continue;

        auto secondType = req.getSecondType().subst(
                           getTypeSubstitution,
                           Builder.getLookupConformanceFn());
        if (!secondType)
          break;
        auto secondPA = Builder.resolveArchetype(secondType);

        if (firstPA && secondPA) {
          if (Builder.addSameTypeRequirementBetweenArchetypes(firstPA, secondPA,
                                                              source)) {
            return Action::Stop;
          }
        } else if (firstPA || secondPA) {
          auto PA = firstPA ? firstPA : secondPA;
          auto concrete = firstPA ? secondType : firstType;
          if (Builder.addSameTypeRequirementToConcrete(PA, concrete, source)) {
            return Action::Stop;
          }
        }
        break;
      }

      case RequirementKind::Superclass:
      case RequirementKind::Layout:
      case RequirementKind::Conformance: {
        auto subjectType = req.getFirstType().subst(
                             getTypeSubstitution,
                             Builder.getLookupConformanceFn());
        if (!subjectType)
          break;

        auto subjectPA = Builder.resolveArchetype(subjectType);
        if (!subjectPA) {
          break;
        }

        if (isOuterArchetype(subjectPA))
          return Action::Continue;

        if (req.getKind() == RequirementKind::Conformance) {
          auto proto = req.getSecondType()->castTo<ProtocolType>();
          if (Builder.addConformanceRequirement(subjectPA, proto->getDecl(),
                                                source)) {
            return Action::Stop;
          }
        } else if (req.getKind() == RequirementKind::Layout) {
          if (Builder.addLayoutRequirement(subjectPA, req.getLayoutConstraint(),
                                           source)) {
            return Action::Stop;
          }
        } else {
          if (Builder.addSuperclassRequirement(subjectPA, req.getSecondType(),
                                               source)) {
            return Action::Stop;
          }
        }
        break;
      }
      }
    }

    return Action::Continue;
  }
};

void ArchetypeBuilder::inferRequirements(TypeLoc type,
                                         unsigned minDepth,
                                         unsigned maxDepth) {
  if (!type.getType())
    return;
  // FIXME: Crummy source-location information.
  InferRequirementsWalker walker(*this, type.getSourceRange().Start,
                                 minDepth, maxDepth);
  type.getType().walk(walker);
}

void ArchetypeBuilder::inferRequirements(ParameterList *params,
                                         GenericParamList *genericParams) {
  if (genericParams == nullptr)
    return;

  unsigned depth = genericParams->getDepth();
  for (auto P : *params)
    inferRequirements(P->getTypeLoc(),
                      /*minDepth=*/depth,
                      /*maxDepth=*/depth);
}

/// Perform typo correction on the given nested type, producing the
/// corrected name (if successful).
static Identifier typoCorrectNestedType(
                    ArchetypeBuilder::PotentialArchetype *pa) {
  StringRef name = pa->getNestedName().str();

  // Look through all of the associated types of all of the protocols
  // to which the parent conforms.
  llvm::SmallVector<Identifier, 2> bestMatches;
  unsigned bestEditDistance = 0;
  unsigned maxScore = (name.size() + 1) / 3;
  for (const auto &conforms : pa->getParent()->getConformsTo()) {
    auto proto = conforms.first;
    for (auto member : proto->getMembers()) {
      auto assocType = dyn_cast<AssociatedTypeDecl>(member);
      if (!assocType)
        continue;

      unsigned dist = name.edit_distance(assocType->getName().str(),
                                         /*AllowReplacements=*/true,
                                         maxScore);
      assert(dist > 0 && "nested type should have matched associated type");
      if (bestEditDistance == 0 || dist == bestEditDistance) {
        bestEditDistance = dist;
        maxScore = bestEditDistance;
        bestMatches.push_back(assocType->getName());
      } else if (dist < bestEditDistance) {
        bestEditDistance = dist;
        maxScore = bestEditDistance;
        bestMatches.clear();
        bestMatches.push_back(assocType->getName());
      }
    }
  }

  // FIXME: Look through the superclass.

  // If we didn't find any matches at all, fail.
  if (bestMatches.empty())
    return Identifier();

  // Make sure that we didn't find more than one match at the best
  // edit distance.
  for (auto other : llvm::makeArrayRef(bestMatches).slice(1)) {
    if (other != bestMatches.front())
      return Identifier();
  }

  return bestMatches.front();
}

void
ArchetypeBuilder::finalize(SourceLoc loc,
                           ArrayRef<GenericTypeParamType *> genericParams,
                           bool allowConcreteGenericParams) {
  assert(!Impl->finalized && "Already finalized builder");
#ifndef NDEBUG
  Impl->finalized = true;
#endif

  // Local function (+ cache) describing the set of potential archetypes
  // directly referenced by the concrete same-type constraint of the given
  // potential archetype. Both the inputs and results are the representatives
  // of their equivalence classes.
  llvm::DenseMap<PotentialArchetype *,
                 SmallPtrSet<PotentialArchetype *, 4>> concretePAs;
  auto getConcreteReferencedPAs
      = [&](PotentialArchetype *pa) -> SmallPtrSet<PotentialArchetype *, 4> {
    assert(pa == pa->getRepresentative() && "Only use with representatives");
    auto known = concretePAs.find(pa);
    if (known != concretePAs.end())
      return known->second;

    SmallPtrSet<PotentialArchetype *, 4> referencedPAs;
    if (!pa->isConcreteType() || !pa->getConcreteType()->hasTypeParameter())
      return referencedPAs;

    if (auto concreteType = pa->getConcreteType()) {
      if (concreteType->hasTypeParameter()) {
        concreteType.visit([&](Type type) {
          if (type->isTypeParameter()) {
            if (auto referencedPA = resolveArchetype(type)) {
              referencedPAs.insert(referencedPA->getRepresentative());
            }
          }
        });
      }
    }

    concretePAs[pa] = referencedPAs;
    return referencedPAs;
  };

  /// Check whether the given type references the archetype.
  auto isRecursiveConcreteType = [&](PotentialArchetype *archetype,
                                     bool isSuperclass) {
    SmallPtrSet<PotentialArchetype *, 4> visited;
    SmallVector<PotentialArchetype *, 4> stack;
    stack.push_back(archetype);
    visited.insert(archetype);

    // Check whether the specific type introduces recursion.
    auto checkTypeRecursion = [&](Type type) {
      if (!type->hasTypeParameter()) return false;

      return type.findIf([&](Type type) {
        if (type->isTypeParameter()) {
          if (auto referencedPA = resolveArchetype(type)) {
            referencedPA = referencedPA->getRepresentative();
            if (referencedPA == archetype) return true;

            if (visited.insert(referencedPA).second)
              stack.push_back(referencedPA);
          }
        }

        return false;
      });
    };

    while (!stack.empty()) {
      auto pa = stack.back();
      stack.pop_back();

      // If we're checking superclasses, do so now.
      if (isSuperclass) {
        if (auto superclass = pa->getSuperclass()) {
          if (checkTypeRecursion(superclass)) return true;
        }
      }

      // Otherwise, look for the potential archetypes referenced by
      // same-type constraints.
      for (auto referencedPA : getConcreteReferencedPAs(pa)) {
        // If we found a reference to the original archetype, it's recursive.
        if (referencedPA == archetype) return true;

        if (visited.insert(referencedPA).second)
          stack.push_back(referencedPA);
      }
    }

    return false;
  };

  // Check for recursive same-type bindings and superclass constraints.
  visitPotentialArchetypes([&](PotentialArchetype *archetype) {
    if (archetype != archetype->getRepresentative()) return;

    // Check for recursive same-type bindings.
    if (archetype->isConcreteType()) {
      if (isRecursiveConcreteType(archetype, /*isSuperclass=*/false)) {
        if (archetype->ConcreteTypeSource->getLoc().isValid())
          Diags.diagnose(archetype->ConcreteTypeSource->getLoc(),
                         diag::recursive_same_type_constraint,
                         archetype->getDependentType(genericParams,
                                                     /*allowUnresolved=*/true),
                         archetype->getConcreteType());

        archetype->RecursiveConcreteType = true;
      }
    }

    // Check for recursive superclass bindings.
    if (archetype->getSuperclass()) {
      if (isRecursiveConcreteType(archetype, /*isSuperclass=*/true)) {
        if (archetype->SuperclassSource->getLoc().isValid())
          Diags.diagnose(archetype->SuperclassSource->getLoc(),
                         diag::recursive_superclass_constraint,
                         archetype->getDependentType(genericParams,
                                                     /*allowUnresolved=*/true),
                         archetype->getSuperclass());

        archetype->RecursiveSuperclassType = true;
      }
    }
  });

  // Create anchors for all of the potential archetypes.
  // FIXME: This is because we might be missing some from the equivalence
  // classes. It is an egregious hack.
  visitPotentialArchetypes([&](PotentialArchetype *archetype) {
    (void)archetype->getArchetypeAnchor(*this);
  });

  SmallPtrSet<PotentialArchetype *, 4> visited;

  // Check for generic parameters which have been made concrete or equated
  // with each other.
  if (!allowConcreteGenericParams) {
    unsigned depth = 0;
    for (const auto &gp : Impl->GenericParams)
      depth = std::max(depth, gp->getDepth());

    for (const auto pa : Impl->PotentialArchetypes) {
      auto rep = pa->getRepresentative();

      if (pa->getRootGenericParamKey().Depth < depth)
        continue;

      if (!visited.insert(rep).second)
        continue;

      // Don't allow a generic parameter to be equivalent to a concrete type,
      // because then we don't actually have a parameter.
      if (rep->getConcreteType()) {
        auto &Source = rep->ConcreteTypeSource;

        // For auto-generated locations, we should have diagnosed the problem
        // elsewhere already.
        if (!Source->getLoc().isValid())
          continue;

        Diags.diagnose(Source->getLoc(),
                       diag::requires_generic_param_made_equal_to_concrete,
                       rep->getDependentType(genericParams,
                                             /*allowUnresolved=*/true));
        continue;
      }

      // Don't allow two generic parameters to be equivalent, because then we
      // don't actually have two parameters.
      for (auto other : rep->getEquivalenceClass()) {
        // If it isn't a generic parameter, skip it.
        if (other == pa || other->getParent() != nullptr) continue;

        SourceLoc constraintLoc;
        for (const auto &sameType : pa->getSameTypeConstraints()) {
          SourceLoc sameTypeLoc = sameType.second.getLoc();
          if (sameTypeLoc.isInvalid()) continue;

          if (sameType.first == other) {
            constraintLoc = sameTypeLoc;
            break;
          }

          if (constraintLoc.isInvalid())
            constraintLoc = sameTypeLoc;
        }

        if (constraintLoc.isInvalid())
          constraintLoc = loc;

        Diags.diagnose(constraintLoc,
                       diag::requires_generic_params_made_equal,
                       pa->getDependentType(genericParams, true),
                       other->getDependentType(genericParams, true));
        break;
      }
    }
  }

  // If any nested types remain unresolved, produce diagnostics.
  if (Impl->NumUnresolvedNestedTypes > 0) {
    visitPotentialArchetypes([&](PotentialArchetype *pa) {
      // We only care about nested types that haven't been resolved.
      if (pa->getParent() == nullptr || pa->getResolvedAssociatedType() ||
          pa->getTypeAliasDecl() ||
          /* FIXME: Should be able to handle this earlier */pa->getSuperclass())
        return;

      // Try to typo correct to a nested type name.
      Identifier correction = typoCorrectNestedType(pa);
      if (correction.empty()) {
        pa->setInvalid();
        return;
      }

      // Note that this is being renamed.
      pa->saveNameForRenaming();
      Impl->RenamedNestedTypes.push_back(pa);
      
      // Resolve the associated type and merge the potential archetypes.
      auto replacement = pa->getParent()->getNestedType(correction, *this);
      pa->resolveAssociatedType(replacement->getResolvedAssociatedType(),
                                *this);
      addSameTypeRequirementBetweenArchetypes(
        pa, replacement,
        RequirementSource(RequirementSource::Redundant, SourceLoc()));
    });
  }
}

bool ArchetypeBuilder::diagnoseRemainingRenames(
                              SourceLoc loc,
                              ArrayRef<GenericTypeParamType *> genericParams) {
  bool invalid = false;

  for (auto pa : Impl->RenamedNestedTypes) {
    if (pa->alreadyDiagnosedRename()) continue;

    Diags.diagnose(loc, diag::invalid_member_type_suggest,
                   pa->getParent()->getDependentType(genericParams,
                                                     /*allowUnresolved=*/true),
                   pa->getOriginalName(), pa->getNestedName());
    invalid = true;
  }

  return invalid;
}

template<typename F>
void ArchetypeBuilder::visitPotentialArchetypes(F f) {
  // Stack containing all of the potential archetypes to visit.
  SmallVector<PotentialArchetype *, 4> stack;
  llvm::SmallPtrSet<PotentialArchetype *, 4> visited;

  // Add top-level potential archetypes to the stack.
  for (const auto pa : Impl->PotentialArchetypes) {
    if (visited.insert(pa).second)
      stack.push_back(pa);
  }

  // Visit all of the potential archetypes.
  while (!stack.empty()) {
    PotentialArchetype *pa = stack.back();
    stack.pop_back();
    f(pa);

    // Visit nested potential archetypes.
    for (const auto &nested : pa->getNestedTypes()) {
      for (auto nestedPA : nested.second) {
        if (visited.insert(nestedPA).second) {
          stack.push_back(nestedPA);
        }
      }
    }
  }
}

namespace {
  using PotentialArchetype = ArchetypeBuilder::PotentialArchetype;
} // end anonymous namespace

/// Perform a depth-first search from the given potential archetype through
/// the *implicit* same-type constraints.
///
/// \param pa The potential archetype to visit.
/// \param visited The set of potential archetypes that have already been
/// seen.
/// \param found Used to record each potential archetype visited
static void sameTypeDFS(PotentialArchetype *pa,
                        SmallPtrSetImpl<PotentialArchetype *> &visited,
                        SmallVectorImpl<PotentialArchetype *> &found) {
  // If we've already visited this potential archetype, we're done.
  if (!visited.insert(pa).second) return;

  // Note that we've found this potential archetype.
  found.push_back(pa);

  // Visit its adjacent potential archetypes.
  for (const auto &sameType : pa->getSameTypeConstraints()) {
    switch (sameType.second.getKind()) {
    case RequirementSource::Explicit:
    case RequirementSource::Inferred:
    case RequirementSource::ProtocolRequirementSignatureSelf:
      // Skip explicit constraints.
      continue;

    case RequirementSource::Inherited:
    case RequirementSource::Protocol:
    case RequirementSource::Redundant:
      break;
    }

    sameTypeDFS(sameType.first, visited, found);
  }
}

/// Computes the ordered set of archetype anchors required to form a minimum
/// spanning tree among the connected components formed by only the implied
/// same-type requirements within the equivalence class of \c rep.
///
/// The equivalance class of the given representative potential archetype
/// (\c rep) contains all potential archetypes that are made equivalent by
/// the known set of same-type constraints, which includes both directly-
/// stated same-type constraints (e.g., \c T.A == T.B) as well as same-type
/// constraints that are implied either because the names coincide (e.g.,
/// \c T[.P1].A == T[.P2].A) or due to a requirement in a protocol.
///
/// The equivalance class of the given representative potential archetype
/// (\c rep) is formed from a graph whose vertices are the potential archetypes
/// and whose edges are the same-type constraints. These edges include both
/// directly-stated same-type constraints (e.g., \c T.A == T.B) as well as
/// same-type constraints that are implied either because the names coincide
/// (e.g., \c T[.P1].A == T[.P2].A) or due to a requirement in a protocol.
/// The equivalence class forms a single connected component.
///
/// Within that graph is a subgraph that includes only those edges that are
/// implied (and, therefore, excluding those edges that were explicitly stated).
/// The connected components within that subgraph describe the potential
/// archetypes that would be equivalence even with all of the (explicit)
/// same-type constraints removed.
///
/// The entire equivalence class can be restored by introducing edges between
/// the connected components. This function computes a minimal, canonicalized
/// set of edges (same-type constraints) needed to describe the equivalence
/// class, which is suitable for the generation of the canonical generic
/// signature.
///
/// The resulting set of "edges" is returned as a set of vertices, one per
/// connected component (of the subgraph). Each is the anchor for that
/// connected component (as determined by \c compareDependentTypes()), and the
/// set itself is ordered by \c compareDependentTypes(). The actual set of
/// canonical edges connects vertex i to vertex i+1 for i in 0..<size-1.
static SmallVector<PotentialArchetype *, 2> getSameTypeComponentAnchors(
                                                     PotentialArchetype *rep) {
  SmallPtrSet<PotentialArchetype *, 8> visited;
  SmallVector<PotentialArchetype *, 2> componentAnchors;
  for (auto pa : rep->getEquivalenceClass()) {
    // If we've already seen this potential archetype, there's nothing else to
    // do.
    if (visited.count(pa) != 0) continue;

    // Find all of the potential archetypes within this connected component.
    SmallVector<PotentialArchetype *, 2> component;
    sameTypeDFS(pa, visited, component);

    // Find the best anchor for this component.
    PotentialArchetype *anchor = component[0];
    for (auto componentPA : ArrayRef<PotentialArchetype *>(component).slice(1)){
      if (compareDependentTypes(&componentPA, &anchor) < 0)
        anchor = componentPA;
    }

    // Record the anchor.
    componentAnchors.push_back(anchor);
  }

  llvm::array_pod_sort(componentAnchors.begin(), componentAnchors.end(),
                       compareDependentTypes);

  return componentAnchors;
}

void ArchetypeBuilder::enumerateRequirements(llvm::function_ref<
                     void (RequirementKind kind,
                           PotentialArchetype *archetype,
                           ArchetypeBuilder::RequirementRHS constraint,
                           RequirementSource source)> f) {
  // Collect all archetypes.
  SmallVector<PotentialArchetype *, 8> archetypes;
  visitPotentialArchetypes([&](PotentialArchetype *archetype) {
    archetypes.push_back(archetype);
  });

  // Remove any invalid potential archetypes or archetypes whose parents are
  // concrete; they have no requirements.
  archetypes.erase(
    std::remove_if(archetypes.begin(), archetypes.end(),
      [&](PotentialArchetype *archetype) -> bool {
        // Invalid archetypes are never representatives in well-formed or
        // corrected signature, so we don't need to visit them.
        if (archetype->isInvalid())
          return true;

        // If there is a concrete type above us, there are no requirements to
        // emit.
        if (archetype->getParent() &&
            hasConcreteTypeInPath(archetype->getParent()))
          return true;

        // Keep it.
        return false;
      }),
    archetypes.end());

  // Sort the archetypes in canonical order.
  llvm::array_pod_sort(archetypes.begin(), archetypes.end(),
                       compareDependentTypes);

  // Track the anchors for each of the implied connected components within the
  // equivalance class of each representative.
  llvm::DenseMap<PotentialArchetype *, SmallVector<PotentialArchetype *, 2>>
    sameTypeComponentAnchors;
  auto getSameTypeComponentAnchors =
    [&](PotentialArchetype *rep) -> ArrayRef<PotentialArchetype *> {
      assert(rep->getRepresentative() == rep);
      auto known = sameTypeComponentAnchors.find(rep);
      if (known != sameTypeComponentAnchors.end())
        return known->second;

      return sameTypeComponentAnchors.insert(
               {rep, ::getSameTypeComponentAnchors(rep) }).first->second;
    };

  for (auto *archetype : archetypes) {
    // Check whether this archetype is one of the anchors within its
    // connected component. If so, we may need to emit a same-type constraint.
    //
    // FIXME: O(n) in the number of implied connected components within the
    // equivalence class. The equivalence class should be small, but...
    auto rep = archetype->getRepresentative();
    auto componentAnchors = getSameTypeComponentAnchors(rep);
    auto knownAnchor = std::find(componentAnchors.begin(),
                                 componentAnchors.end(),
                                 archetype);
    std::function<void()> deferredSameTypeRequirement;

    if (knownAnchor != componentAnchors.end()) {
      // If this equivalance class is bound to a concrete type, equate the
      // anchor with a concrete type.
      if (auto concreteType = rep->getConcreteType()) {
        f(RequirementKind::SameType, archetype, concreteType,
          rep->getConcreteTypeSource());
        continue;
      }

      // If we're at the last anchor in the component, do nothing;
      auto nextAnchor = knownAnchor;
      ++nextAnchor;
      if (nextAnchor != componentAnchors.end()) {
        // Form a same-type constraint from this anchor within the component
        // to the next.
        // FIXME: Distinguish between explicit and inferred here?
        auto otherPA = *nextAnchor;
        deferredSameTypeRequirement = [&f, archetype, otherPA] {
          f(RequirementKind::SameType, archetype, otherPA,
            RequirementSource(RequirementSource::Explicit, SourceLoc()));
        };
      }
    }
    SWIFT_DEFER {
      if (deferredSameTypeRequirement) deferredSameTypeRequirement();
    };

    // If this is not the archetype anchor, we're done.
    if (archetype != archetype->getArchetypeAnchor(*this))
      continue;

    // If we have a superclass, produce a superclass requirement
    if (Type superclass = rep->getSuperclass()) {
      f(RequirementKind::Superclass, archetype, superclass,
        rep->getSuperclassSource());
    }

    // If we have a layout constraint, produce a layout requirement.
    if (LayoutConstraint Layout = archetype->getLayout()) {
      f(RequirementKind::Layout, archetype, Layout,
        archetype->getLayoutSource());
    }

    // Enumerate conformance requirements.
    SmallVector<ProtocolDecl *, 4> protocols;
    DenseMap<ProtocolDecl *, RequirementSource> protocolSources;
    for (const auto &conforms : rep->getConformsTo()) {
      protocols.push_back(conforms.first);
      assert(protocolSources.count(conforms.first) == 0 && 
             "redundant protocol requirement?");
      protocolSources.insert({conforms.first, conforms.second});
    }

    // Sort the protocols in canonical order.
    llvm::array_pod_sort(protocols.begin(), protocols.end(), 
                         ProtocolType::compareProtocols);

    // Enumerate the conformance requirements.
    for (auto proto : protocols) {
      assert(protocolSources.count(proto) == 1 && "Missing conformance?");
      f(RequirementKind::Conformance, archetype, 
        proto->getDeclaredInterfaceType(),
        protocolSources.find(proto)->second);
    }
  };
}

void ArchetypeBuilder::dump() {
  dump(llvm::errs());
}

void ArchetypeBuilder::dump(llvm::raw_ostream &out) {
  out << "Requirements:";
  enumerateRequirements([&](RequirementKind kind,
                            PotentialArchetype *archetype,
                            ArchetypeBuilder::RequirementRHS constraint,
                            RequirementSource source) {
    switch (kind) {
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
      out << "\n  ";
      out << archetype->getDebugName() << " : " 
          << constraint.get<Type>().getString() << " [";
      source.dump(out, &Context.SourceMgr);
      out << "]";
      break;
    case RequirementKind::Layout:
      out << "\n  ";
      out << archetype->getDebugName() << " : "
          << constraint.get<LayoutConstraint>().getString() << " [";
      source.dump(out, &Context.SourceMgr);
      out << "]";
      break;
    case RequirementKind::SameType:
      out << "\n  ";
      out << archetype->getDebugName() << " == " ;
      if (auto secondType = constraint.dyn_cast<Type>()) {
        out << secondType.getString();
      } else {
        out << constraint.get<PotentialArchetype *>()->getDebugName();
      }
      out << " [";
      source.dump(out, &Context.SourceMgr);
      out << "]";
      break;
    }
  });
  out << "\n";

  out << "Potential archetypes:\n";
  for (auto pa : Impl->PotentialArchetypes) {
    pa->dump(out, &Context.SourceMgr, 2);
  }
  out << "\n";
}

void ArchetypeBuilder::addGenericSignature(GenericSignature *sig) {
  if (!sig) return;

  RequirementSource::Kind sourceKind = RequirementSource::Explicit;
  for (auto param : sig->getGenericParams())
    addGenericParameter(param);

  RequirementSource source(sourceKind, SourceLoc());
  for (auto &reqt : sig->getRequirements()) {
    addRequirement(reqt, source);
  }
}

/// Collect the set of requirements placed on the given generic parameters and
/// their associated types.
static void collectRequirements(ArchetypeBuilder &builder,
                                ArrayRef<GenericTypeParamType *> params,
                                SmallVectorImpl<Requirement> &requirements) {
  builder.enumerateRequirements([&](RequirementKind kind,
          ArchetypeBuilder::PotentialArchetype *archetype,
          ArchetypeBuilder::RequirementRHS type,
          RequirementSource source) {
    // Filter out redundant requirements.
    switch (source.getKind()) {
    case RequirementSource::Explicit:
    case RequirementSource::Inferred:
      // The requirement was explicit and required, keep it.
      break;

    case RequirementSource::Protocol:
    case RequirementSource::Redundant:
    case RequirementSource::Inherited:
    case RequirementSource::ProtocolRequirementSignatureSelf:
      // The requirement was redundant, drop it.
      return;
    }

    auto depTy = archetype->getDependentType(params,
                                             /*allowUnresolved=*/false);

    if (depTy->hasError())
      return;

    Type repTy;
    if (auto concreteTy = type.dyn_cast<Type>()) {
      // Maybe we were equated to a concrete type...
      repTy = concreteTy;

      // Drop requirements involving concrete types containing
      // unresolved associated types.
      if (repTy.findIf([](Type t) -> bool {
            if (auto *depTy = dyn_cast<DependentMemberType>(t.getPointer()))
              if (depTy->getAssocType() == nullptr)
                return true;
            return false;
          })) {
        return;
      }
    } else if (auto layoutConstraint = type.dyn_cast<LayoutConstraint>()) {
      requirements.push_back(Requirement(kind, depTy, layoutConstraint));
      return;
    } else {
      // ...or to a dependent type.
      repTy = type.get<ArchetypeBuilder::PotentialArchetype *>()
          ->getDependentType(params, /*allowUnresolved=*/false);
    }

    if (repTy->hasError())
      return;

    requirements.push_back(Requirement(kind, depTy, repTy));
  });
}

GenericSignature *ArchetypeBuilder::getGenericSignature() {
  assert(Impl->finalized && "Must finalize builder first");

  // Collect the requirements placed on the generic parameter types.
  SmallVector<Requirement, 4> requirements;
  collectRequirements(*this, Impl->GenericParams, requirements);

  auto sig = GenericSignature::get(Impl->GenericParams, requirements);
  return sig;
}

void ArchetypeBuilder::expandGenericEnvironment(GenericEnvironment *env) {

  assert(Impl->finalized && "Must finalize builder first");

  // Force the creation of all of the archetypes.
  // FIXME: This isn't a well-formed notion with recursive protocol constraints.
  auto signature = env->getGenericSignature();
  visitPotentialArchetypes([&](PotentialArchetype *pa) {
    if (auto archetype =
            env->mapTypeIntoContext(
              pa->getDependentType(signature->getGenericParams(),
                                  /*allowUnresolved=*/false),
             getLookupConformanceFn())
          ->getAs<ArchetypeType>())
      (void)archetype->getAllNestedTypes();
  });

#ifndef NDEBUG
  // FIXME: This property should be maintained when there are errors, too.
  if (!Diags.hadAnyError()) {
    auto genericParams = signature->getGenericParams();
    visitPotentialArchetypes([&](PotentialArchetype *pa) {
      if (pa->isConcreteType()) return;

      auto depTy = pa->getDependentType(genericParams,
                                        /*allowUnresolved=*/false);
      auto inContext = env->mapTypeIntoContext(depTy, getLookupConformanceFn());

      auto repDepTy = pa->getRepresentative()->getDependentType(
                                                    genericParams,
                                                    /*allowUnresolved=*/false);
      auto repInContext =
        env->mapTypeIntoContext(repDepTy, getLookupConformanceFn());
      assert((inContext->isEqual(repInContext) ||
              inContext->hasError() ||
              repInContext->hasError()) &&
             "Potential archetype mapping differs from representative!");
    });
  }
#endif
}

