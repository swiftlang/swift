//===--- ArchetypeBuilder.cpp - Generic Requirement Builder ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

using namespace swift;
using llvm::DenseMap;

using NestedType = ArchetypeType::NestedType;

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

  case OuterScope:
    out << "outer";
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

/// The identifying information for a generic parameter.

namespace {
struct GenericTypeParamKey {
  unsigned Depth : 16;
  unsigned Index : 16;
  
  static GenericTypeParamKey forDecl(GenericTypeParamDecl *d) {
    return {d->getDepth(), d->getIndex()};
  }
  
  static GenericTypeParamKey forType(GenericTypeParamType *t) {
    return {t->getDepth(), t->getIndex()};
  }
};
}

namespace llvm {

template<>
struct DenseMapInfo<GenericTypeParamKey> {
  static inline GenericTypeParamKey getEmptyKey() { return {0xFFFF, 0xFFFF}; }
  static inline GenericTypeParamKey getTombstoneKey() { return {0xFFFE, 0xFFFE}; }
  static inline unsigned getHashValue(GenericTypeParamKey k) {
    return DenseMapInfo<unsigned>::getHashValue(k.Depth << 16 | k.Index);
  }
  static bool isEqual(GenericTypeParamKey a, GenericTypeParamKey b) {
    return a.Depth == b.Depth && a.Index == b.Index;
  }
};
  
}

struct ArchetypeBuilder::Implementation {
  /// A mapping from generic parameters to the corresponding potential
  /// archetypes.
  llvm::MapVector<GenericTypeParamKey, PotentialArchetype*> PotentialArchetypes;

  /// The number of nested types that haven't yet been resolved to archetypes.
  /// Once all requirements have been added, this will be zero in well-formed
  /// code.
  unsigned NumUnresolvedNestedTypes = 0;
};

ArchetypeBuilder::PotentialArchetype::~PotentialArchetype() {
  for (const auto &nested : NestedTypes) {
    for (auto pa : nested.second) {
      if (pa != this)
        delete pa;
    }
  }
}

void ArchetypeBuilder::PotentialArchetype::buildFullName(
       bool forDebug,
       SmallVectorImpl<char> &result) const {
  if (auto parent = getParent()) {
    parent->buildFullName(forDebug, result);

    // When building the name for debugging purposes, include the
    // protocol into which the associated type was resolved.
    if (forDebug) {
      if (auto assocType = getResolvedAssociatedType()) {
        result.push_back('[');
        result.push_back('.');
        result.append(assocType->getProtocol()->getName().str().begin(), 
                      assocType->getProtocol()->getName().str().end());
        result.push_back(']');
      }
    }

    result.push_back('.');
  }
  result.append(getName().str().begin(), getName().str().end());
}

Identifier ArchetypeBuilder::PotentialArchetype::getName() const { 
  if (auto assocType = NameOrAssociatedType.dyn_cast<AssociatedTypeDecl *>())
    return assocType->getName();
  if (auto typeAlias = NameOrAssociatedType.dyn_cast<TypeAliasDecl *>())
    return typeAlias->getName();
  return NameOrAssociatedType.get<Identifier>();
}

std::string ArchetypeBuilder::PotentialArchetype::getFullName() const {
  llvm::SmallString<64> result;
  buildFullName(false, result);
  return result.str().str();
}

std::string ArchetypeBuilder::PotentialArchetype::getDebugName() const {
  llvm::SmallString<64> result;
  buildFullName(true, result);
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
  assert(!NameOrAssociatedType.is<AssociatedTypeDecl *>() &&
         "associated type is already resolved");
  NameOrAssociatedType = assocType;
  assert(assocType->getName() == getName());
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
    builder.getModule().lookupConformance(superclass, proto,
                                          builder.getLazyResolver());
  if (!conformance) return nullptr;

  // Conformance to this protocol is redundant; update the requirement source
  // appropriately.
  updateRequirementSource(
    conformsSource,
    RequirementSource(RequirementSource::Redundant,
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
  concreteType = ArchetypeBuilder::mapTypeOutOfContext(
                   superConformance->getDeclContext(), concreteType);
  if (auto otherPA = builder.resolveArchetype(concreteType))
    builder.addSameTypeRequirementBetweenArchetypes(
        nestedPA, otherPA, fromSource);
  else
    builder.addSameTypeRequirementToConcrete(
        nestedPA, concreteType, fromSource);
}

bool ArchetypeBuilder::PotentialArchetype::addConformance(
       ProtocolDecl *proto, 
       const RequirementSource &source,
       ArchetypeBuilder &builder) {
  auto rep = getRepresentative();
  if (rep != this)
    return rep->addConformance(proto, source, builder);

  // Check whether we already know about this conformance.
  auto known = ConformsTo.find(proto);
  if (known != ConformsTo.end()) {
    // We already have this requirement. Update the requirement source
    // appropriately.
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
    auto frontRep = known->second.front()->getRepresentative();
    otherPA->Representative = frontRep;
    frontRep->EquivalenceClass.push_back(otherPA);
    otherPA->SameTypeSource = redundantSource;
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

bool ArchetypeBuilder::PotentialArchetype::hasConcreteTypeInPath() const {
  for (auto pa = this; pa; pa = pa->getParent()) {
    if (pa->ArchetypeOrConcreteType.isConcreteType() &&
        !pa->ArchetypeOrConcreteType.getAsConcreteType()->is<ArchetypeType>())
      return true;
  }

  return false;
}

bool ArchetypeBuilder::PotentialArchetype::isBetterArchetypeAnchor(
       PotentialArchetype *other) const {
  auto concrete = hasConcreteTypeInPath();
  auto otherConcrete = other->hasConcreteTypeInPath();
  if (concrete != otherConcrete)
    return otherConcrete;

  // FIXME: Not a total order.
  return std::make_tuple(getRootParam()->getDepth(),
                         getRootParam()->getIndex(),
                         getNestingDepth())
    < std::make_tuple(other->getRootParam()->getDepth(),
                      other->getRootParam()->getIndex(),
                      other->getNestingDepth());
}

auto ArchetypeBuilder::PotentialArchetype::getArchetypeAnchor()
       -> PotentialArchetype * {

  // Default to the representative, unless we find something better.
  PotentialArchetype *best = getRepresentative();
  for (auto pa : best->getEquivalenceClass()) {
    if (pa->isBetterArchetypeAnchor(best))
      best = pa;
  }

  return best;
}

auto ArchetypeBuilder::PotentialArchetype::getNestedType(
       Identifier nestedName,
       ArchetypeBuilder &builder) -> PotentialArchetype * {
  // Retrieve the nested type from the representation of this set.
  if (Representative != this)
    return getRepresentative()->getNestedType(nestedName, builder);

    // If we already have a nested type with this name, return it.
  if (!NestedTypes[nestedName].empty()) {
    return NestedTypes[nestedName].front();
  }

  RequirementSource redundantSource(RequirementSource::Redundant,
                                    SourceLoc());

  // Attempt to resolve this nested type to an associated type
  // of one of the protocols to which the parent potential
  // archetype conforms.
  for (auto &conforms : ConformsTo) {
    for (auto member : conforms.first->lookupDirect(nestedName)) {
      PotentialArchetype *pa;
      
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        // Resolve this nested type to this associated type.
        pa = new PotentialArchetype(this, assocType);
      } else if (auto alias = dyn_cast<TypeAliasDecl>(member)) {
        // Resolve this nested type to this type alias.
        pa = new PotentialArchetype(this, alias);
        
        if (!alias->hasUnderlyingType())
          builder.getLazyResolver()->resolveDeclSignature(alias);
        if (!alias->hasUnderlyingType())
          continue;

        auto type = alias->getUnderlyingType();
        SmallVector<Identifier, 4> identifiers;
        
        if (auto archetype = type->getAs<ArchetypeType>()) {
          auto containingProtocol = dyn_cast<ProtocolDecl>(alias->getParent());
          if (!containingProtocol) continue;
          
          // Go up archetype parents until we find our containing protocol.
          while (archetype->getSelfProtocol() != containingProtocol) {
            identifiers.push_back(archetype->getName());
            archetype = archetype->getParent();
            if (!archetype)
              break;
          }
          if (!archetype)
            continue;
        } else if (auto dependent = type->getAs<DependentMemberType>()) {
          do {
            identifiers.push_back(dependent->getName());
            dependent = dependent->getBase()->getAs<DependentMemberType>();
          } while (dependent);
        }
        
        if (identifiers.size()) {
          // Go down our PAs until we find the referenced PA.
          auto existingPA = this;
          while (identifiers.size()) {
            auto identifier = identifiers.back();
            // If we end up looking for ourselves, don't recurse.
            if (existingPA == this && identifier == nestedName) {
              existingPA = pa;
            } else {
              existingPA = existingPA->getNestedType(identifier, builder);
              existingPA = existingPA->getRepresentative();
            }
            identifiers.pop_back();
          }
          if (pa != existingPA) {
            pa->Representative = existingPA;
            pa->Representative->EquivalenceClass.push_back(pa);
            pa->SameTypeSource = redundantSource;
          }
        } else if (type->hasArchetype()) {
          // This is a complex type involving other associatedtypes, we'll fail
          // to resolve and get a special diagnosis in finalize.
          continue;
        } else {
          pa->ArchetypeOrConcreteType = NestedType::forConcreteType(type);
          pa->SameTypeSource = redundantSource;
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
          for (auto existing : nested) {
            existing->Representative = pa;
            existing->Representative->EquivalenceClass.push_back(existing);
            existing->SameTypeSource = redundantSource;
          }
          nested.insert(nested.begin(), pa);
          NestedTypes[nestedName] = nested;
        } else {
          pa->Representative = existing->getRepresentative();
          pa->Representative->EquivalenceClass.push_back(pa);
          pa->SameTypeSource = redundantSource;
          nested.push_back(pa);
        }
      } else
        nested.push_back(pa);

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

  return nested.front();
}

/// Replace dependent types with their archetypes or concrete types.
static Type substConcreteTypesForDependentTypes(ArchetypeBuilder &builder,
                                                Type type) {
  return type.transform([&](Type type) -> Type {
      if (auto depMemTy = type->getAs<DependentMemberType>()) {
        auto newBase = substConcreteTypesForDependentTypes(builder,
                                                           depMemTy->getBase());
        return depMemTy->substBaseType(&builder.getModule(), newBase,
                                       builder.getLazyResolver());
      }

      if (auto typeParam = type->getAs<GenericTypeParamType>()) {
        auto potentialArchetype = builder.resolveArchetype(typeParam);
        return potentialArchetype->getType(builder).getValue();
      }

      return type;
  });
}

ArchetypeType::NestedType
ArchetypeBuilder::PotentialArchetype::getType(ArchetypeBuilder &builder) {

  auto representative = getRepresentative();

  // Retrieve the archetype from the archetype anchor in this equivalence class.
  // The anchor must not have any concrete parents (otherwise we would just
  // use the representative).
  auto archetypeAnchor = getArchetypeAnchor();
  if (archetypeAnchor != this)
    return archetypeAnchor->getType(builder);

  // Return a concrete type or archetype we've already resolved.
  if (representative->ArchetypeOrConcreteType) {
    // If the concrete type is dependent, substitute dependent types
    // for archetypes.
    if (auto concreteType
          = representative->ArchetypeOrConcreteType.getAsConcreteType()) {
      if (concreteType->hasTypeParameter()) {
        // If we already know the concrete type is recursive, just
        // return an error. It will be diagnosed elsewhere.
        if (representative->RecursiveConcreteType) {
          return NestedType::forConcreteType(
                   ErrorType::get(builder.getASTContext()));
        }

        // If we're already substituting a concrete type, mark this
        // potential archetype as having a recursive concrete type.
        if (representative->SubstitutingConcreteType) {
          representative->RecursiveConcreteType = true;
          return NestedType::forConcreteType(
                   ErrorType::get(builder.getASTContext()));
        }

        representative->SubstitutingConcreteType = true;
        NestedType result = NestedType::forConcreteType(
                              substConcreteTypesForDependentTypes(
                                builder,
                                concreteType));
        representative->SubstitutingConcreteType = false;

        // If all went well, we're done.
        if (!representative->RecursiveConcreteType)
          return result;

        // Otherwise, we found that the concrete type is recursive,
        // complain and return an error.
        builder.Diags.diagnose(SameTypeSource->getLoc(),
                               diag::recursive_same_type_constraint,
                               getDependentType(builder, false),
                               concreteType);

        return NestedType::forConcreteType(
                 ErrorType::get(builder.getASTContext()));
      }
    }

    return representative->ArchetypeOrConcreteType;
  }
  
  ArchetypeType::AssocTypeOrProtocolType assocTypeOrProto = RootProtocol;
  // Allocate a new archetype.
  ArchetypeType *ParentArchetype = nullptr;
  auto &mod = builder.getModule();
  if (auto parent = getParent()) {
    assert(assocTypeOrProto.isNull() &&
           "root protocol type given for non-root archetype");
    auto parentTy = parent->getType(builder);
    if (!parentTy)
      return NestedType::forConcreteType(
               ErrorType::get(builder.getASTContext()));

    ParentArchetype = parentTy.getAsArchetype();
    if (!ParentArchetype) {
      // We might have an outer archetype as a concrete type here; if so, just
      // return that.
      ParentArchetype = parentTy.getValue()->getAs<ArchetypeType>();
      if (ParentArchetype) {
        representative->ArchetypeOrConcreteType
          = NestedType::forConcreteType(
              ParentArchetype->getNestedTypeValue(getName()));
        return representative->ArchetypeOrConcreteType;
      }

      LazyResolver *resolver = builder.getLazyResolver();
      assert(resolver && "need a lazy resolver");
      (void) resolver;

      // Resolve the member type.
      auto type = getDependentType(builder, false);
      if (type->is<ErrorType>())
        return NestedType::forConcreteType(type);

      auto depMemberType = type->castTo<DependentMemberType>();
      Type memberType = depMemberType->substBaseType(
                          &mod,
                          parent->ArchetypeOrConcreteType.getAsConcreteType(),
                          builder.getLazyResolver());
      if (auto memberPA = builder.resolveArchetype(memberType)) {
        // If the member type maps to an archetype, resolve that archetype.
        if (memberPA->getRepresentative() != getRepresentative()) {
          representative->ArchetypeOrConcreteType = memberPA->getType(builder);
          return representative->ArchetypeOrConcreteType;
        }

        llvm_unreachable("we have no parent archetype");
      } else {
        // Otherwise, it's a concrete type.
        representative->ArchetypeOrConcreteType
          = NestedType::forConcreteType(
              builder.substDependentType(memberType));
        representative->SameTypeSource = parent->SameTypeSource;

        return representative->ArchetypeOrConcreteType;
      }
    }

    assocTypeOrProto = getResolvedAssociatedType();
  }

  // If we ended up building our parent archetype, then we'll have
  // already filled in our own archetype.
  if (auto arch = representative->ArchetypeOrConcreteType.getAsArchetype())
    return NestedType::forArchetype(arch);

  SmallVector<ProtocolDecl *, 4> Protos;
  for (const auto &conforms : ConformsTo) {
    Protos.push_back(conforms.first);
  }

  Type superclass;

  if (Superclass) {
    if (representative->RecursiveSuperclassType) {
      builder.Diags.diagnose(SuperclassSource->getLoc(),
                             diag::recursive_superclass_constraint,
                             Superclass);
    } else {
      representative->RecursiveSuperclassType = true;
      assert(!Superclass->hasArchetype() &&
             "superclass constraint must use interface types");
      superclass = builder.substDependentType(Superclass);
      representative->RecursiveSuperclassType = false;
    }
  }

  auto arch
    = ArchetypeType::getNew(builder.getASTContext(), ParentArchetype,
                            assocTypeOrProto, getName(), Protos,
                            superclass, isRecursive());

  representative->ArchetypeOrConcreteType = NestedType::forArchetype(arch);
  
  // Collect the set of nested types of this archetype, and put them into
  // the archetype itself.
  if (!NestedTypes.empty()) {
    builder.getASTContext().registerLazyArchetype(arch, builder, this);
    SmallVector<std::pair<Identifier, NestedType>, 4> FlatNestedTypes;
    for (auto Nested : NestedTypes) {
      // Skip type aliases, which are just shortcuts.
      if (Nested.second.front()->getTypeAliasDecl())
        continue;
      bool anyNotRenamed = false;
      for (auto NestedPA : Nested.second) {
        if (!NestedPA->wasRenamed()) {
          anyNotRenamed = true;
          break;
        }
      }

      if (!anyNotRenamed)
        continue;

      FlatNestedTypes.push_back({ Nested.first, NestedType() });
    }
    arch->setNestedTypes(builder.getASTContext(), FlatNestedTypes);

    // Force the resolution of the nested types.
    (void)arch->getNestedTypes();

    builder.getASTContext().unregisterLazyArchetype(arch);
  }

  return NestedType::forArchetype(arch);
}

Type ArchetypeBuilder::PotentialArchetype::getDependentType(
       ArchetypeBuilder &builder,
       bool allowUnresolved) {
  if (auto parent = getParent()) {
    Type parentType = parent->getDependentType(builder, allowUnresolved);
    if (parentType->is<ErrorType>())
      return parentType;

    // If we've resolved to an associated type, use it.
    if (auto assocType = getResolvedAssociatedType())
      return DependentMemberType::get(parentType, assocType, builder.Context);

    // If typo correction renamed this type, get the renamed type.
    if (wasRenamed())
      return parent->getNestedType(getName(), builder)
               ->getDependentType(builder, allowUnresolved);
    
    // If we don't allow unresolved dependent member types, fail.
    if (!allowUnresolved)
      return ErrorType::get(builder.Context);

    return DependentMemberType::get(parentType, getName(), builder.Context);
  }
  
  assert(getGenericParam() && "Not a generic parameter?");
  return getGenericParam();
}

void ArchetypeBuilder::PotentialArchetype::dump(llvm::raw_ostream &Out,
                                                SourceManager *SrcMgr,
                                                unsigned Indent) {
  // Print name.
  Out.indent(Indent) << getName();

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
    Out << " [represented by " << getRepresentative()->getFullName() << "]";
  }

  Out << "\n";

  // Print nested types.
  for (const auto &nestedVec : NestedTypes) {
    for (auto nested : nestedVec.second) {
      nested->dump(Out, SrcMgr, Indent + 2);
    }
  }
}

ArchetypeBuilder::ArchetypeBuilder(Module &mod, DiagnosticEngine &diags)
  : Mod(mod), Context(mod.getASTContext()), Diags(diags),
    Impl(new Implementation)
{
}

ArchetypeBuilder::ArchetypeBuilder(ArchetypeBuilder &&) = default;

ArchetypeBuilder::~ArchetypeBuilder() {
  if (!Impl)
    return;

  for (auto PA : Impl->PotentialArchetypes)
    delete PA.second;
}

LazyResolver *ArchetypeBuilder::getLazyResolver() const { 
  return Context.getLazyResolver();
}

auto ArchetypeBuilder::resolveArchetype(Type type) -> PotentialArchetype * {
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    auto known
      = Impl->PotentialArchetypes.find(GenericTypeParamKey::forType(genericParam));
    if (known == Impl->PotentialArchetypes.end())
      return nullptr;

    return known->second;
  }

  if (auto dependentMember = type->getAs<DependentMemberType>()) {
    auto base = resolveArchetype(dependentMember->getBase());
    if (!base)
      return nullptr;

    return base->getNestedType(dependentMember->getName(), *this);
  }

  return nullptr;
}

auto ArchetypeBuilder::addGenericParameter(GenericTypeParamType *GenericParam,
                                           ProtocolDecl *RootProtocol,
                                           Identifier ParamName)
       -> PotentialArchetype *
{
  GenericTypeParamKey Key{GenericParam->getDepth(), GenericParam->getIndex()};
  
  // Create a potential archetype for this type parameter.
  assert(!Impl->PotentialArchetypes[Key]);
  auto PA = new PotentialArchetype(GenericParam, RootProtocol, ParamName);

  Impl->PotentialArchetypes[Key] = PA;
  return PA;
}

void ArchetypeBuilder::addGenericParameter(GenericTypeParamDecl *GenericParam) {
  ProtocolDecl *RootProtocol = dyn_cast<ProtocolDecl>(GenericParam->getDeclContext());
  if (!RootProtocol) {
    if (auto Ext = dyn_cast<ExtensionDecl>(GenericParam->getDeclContext()))
      RootProtocol = dyn_cast_or_null<ProtocolDecl>(Ext->getExtendedType()->getAnyNominal());
  }
  addGenericParameter(
        GenericParam->getDeclaredType()->castTo<GenericTypeParamType>(),
        RootProtocol,
        GenericParam->getName());
}

bool ArchetypeBuilder::addGenericParameterRequirements(GenericTypeParamDecl *GenericParam) {
  GenericTypeParamKey Key{GenericParam->getDepth(), GenericParam->getIndex()};
  auto PA = Impl->PotentialArchetypes[Key];
  
  // Add the requirements from the declaration.
  llvm::SmallPtrSet<ProtocolDecl *, 8> visited;
  return addAbstractTypeParamRequirements(GenericParam, PA,
                                          RequirementSource::Explicit,
                                          visited);
}

void ArchetypeBuilder::addGenericParameter(GenericTypeParamType *GenericParam) {
  auto name = GenericParam->getName();
  // Trim '$' so that archetypes are more readily discernible from abstract
  // parameters.
  if (name.str().startswith("$"))
    name = Context.getIdentifier(name.str().slice(1, name.str().size()));
  
  addGenericParameter(GenericParam, nullptr, name);
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
  if (!T->addConformance(Proto, Source, *this))
    return false;

  RequirementSource InnerSource(RequirementSource::Redundant, Source.getLoc());
  
  bool inserted = Visited.insert(Proto).second;
  assert(inserted);
  (void) inserted;

  // Add all of the inherited protocol requirements, recursively.
  if (auto resolver = getLazyResolver())
    resolver->resolveInheritedProtocols(Proto);
  for (auto InheritedProto : Proto->getInheritedProtocols(getLazyResolver())) {
    if (Visited.count(InheritedProto))
      continue;
    if (addConformanceRequirement(T, InheritedProto, InnerSource, Visited))
      return true;
  }

  // Add requirements for each of the associated types.
  // FIXME: This should use the generic signature, not walk the members.
  for (auto Member : Proto->getMembers()) {
    if (auto AssocType = dyn_cast<AssociatedTypeDecl>(Member)) {
      // Add requirements placed directly on this associated type.
      auto AssocPA = T->getNestedType(AssocType->getName(), *this);
      if (AssocPA != T) {
        if (addAbstractTypeParamRequirements(AssocType, AssocPA,
                                             RequirementSource::Protocol,
                                             Visited))
          return true;
      }

      continue;
    }

    // FIXME: Requirement declarations.
  }
  
  Visited.erase(Proto);
  return false;
}

bool ArchetypeBuilder::addSuperclassRequirement(PotentialArchetype *T,
                                                Type Superclass,
                                                RequirementSource Source) {
  if (Superclass->hasArchetype()) {
    // Map contextual type to interface type.
    // FIXME: There might be a better way to do this.
    Superclass = Superclass.transform(
        [&](Type t) -> Type {
          if (t->is<ArchetypeType>()) {
            auto *pa = resolveArchetype(t);
            // Why does this happen?
            if (!pa)
              return ErrorType::get(Context);
            return pa->getDependentType(*this, false);
          }
          return t;
        });
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

          RequirementSource redundantSource(RequirementSource::Redundant,
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
                     diag::requires_superclass_conflict, T->getName(),
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

/// Canonical ordering for dependent types in generic signatures.
static int compareDependentTypes(ArchetypeBuilder::PotentialArchetype * const* pa,
                                 ArchetypeBuilder::PotentialArchetype * const* pb) {
  auto a = *pa, b = *pb;

  // Fast-path check for equality.
  if (a == b)
    return 0;

  // Ordering is as follows:
  // - Generic params
  if (auto gpa = a->getGenericParam()) {
    if (auto gpb = b->getGenericParam()) {
      // - by depth, so t_0_n < t_1_m
      if (int compareDepth = gpa->getDepth() - gpb->getDepth())
        return compareDepth;
      // - by index, so t_n_0 < t_n_1
      if (int compareIndex = gpa->getIndex() - gpb->getIndex())
        return compareIndex;
      llvm_unreachable("total order failure among generic parameters");
    }

    // A generic param is always ordered before a nested type.
    return -1;
  }

  if (b->getGenericParam())
    return +1;

  // - Dependent members
  auto ppa = a->getParent();
  auto ppb = b->getParent();

  // - by base, so t_0_n.`P.T` < t_1_m.`P.T`
  if (int compareBases = compareDependentTypes(&ppa, &ppb))
    return compareBases;

  // - by name, so t_n_m.`P.T` < t_n_m.`P.U`
  if (int compareNames = a->getName().str().compare(b->getName().str()))
    return compareNames;

  if (auto *aa = a->getResolvedAssociatedType()) {
    if (auto *ab = b->getResolvedAssociatedType()) {
      // - by protocol, so t_n_m.`P.T` < t_n_m.`Q.T` (given P < Q)
      auto protoa = aa->getProtocol();
      auto protob = ab->getProtocol();
      if (int compareProtocols
            = ProtocolType::compareProtocols(&protoa, &protob))
        return compareProtocols;

      // - if one is the representative, put it first.
      if ((a->getRepresentative() == a) !=
          (b->getRepresentative() == b))
        return a->getRepresentative() ? -1 : 1;

      // FIXME: Would be nice if this was a total order.
      return 0;
    }

    // A resolved archetype is always ordered before an unresolved one.
    return -1;
  }

  if (b->getResolvedAssociatedType())
    return +1;

  llvm_unreachable("potential archetype total order failure");
}

bool ArchetypeBuilder::addSameTypeRequirementBetweenArchetypes(
       PotentialArchetype *T1,
       PotentialArchetype *T2,
       RequirementSource Source) 
{
  // Operate on the representatives
  T1 = T1->getRepresentative();
  T2 = T2->getRepresentative();

  // If the representatives are already the same, we're done.
  if (T1 == T2)
    return false;

  // Decide which potential archetype is to be considered the representative.
  // We necessarily prefer potential archetypes rooted at parameters that come
  // from outer generic parameter lists, since those generic parameters will
  // have archetypes bound in the outer context.
  //
  // FIXME: The above comment is mostly obsolete, so why can't we just use
  // compareDependentTypes() here?
  auto T1Param = T1->getRootParam();
  auto T2Param = T2->getRootParam();
  unsigned T1Depth = T1->getNestingDepth();
  unsigned T2Depth = T2->getNestingDepth();
  auto T1Key = std::make_tuple(T1->wasRenamed(), T1Param->getDepth(),
                               T1Param->getIndex(), T1Depth);
  auto T2Key = std::make_tuple(T2->wasRenamed(), T2Param->getDepth(),
                               T2Param->getIndex(), T2Depth);
  if (T2Key < T1Key ||
      (T2Key == T1Key &&
       compareDependentTypes(&T2, &T1) < 0))
    std::swap(T1, T2);

  // Don't allow two generic parameters to be equivalent, because then we
  // don't actually have two parameters.
  // FIXME: Should we simply allow this?
  if (T1Depth == 0 && T2Depth == 0) {
    Diags.diagnose(Source.getLoc(), diag::requires_generic_params_made_equal,
                   T1->getName(), T2->getName());
    return true;
  }
  
  // Merge any concrete constraints.
  Type concrete1 = T1->ArchetypeOrConcreteType.getAsConcreteType();
  Type concrete2 = T2->ArchetypeOrConcreteType.getAsConcreteType();
  
  if (concrete1 && concrete2) {
    if (!concrete1->isEqual(concrete2)) {
      Diags.diagnose(Source.getLoc(), diag::requires_same_type_conflict,
                     T1->getName(), concrete1, concrete2);
      return true;
      
    }
  } else if (concrete1) {
    assert(!T2->ArchetypeOrConcreteType
           && "already formed archetype for concrete-constrained parameter");
    T2->ArchetypeOrConcreteType = NestedType::forConcreteType(concrete1);
    T2->SameTypeSource = T1->SameTypeSource;
  } else if (concrete2) {
    assert(!T1->ArchetypeOrConcreteType
           && "already formed archetype for concrete-constrained parameter");
    T1->ArchetypeOrConcreteType = NestedType::forConcreteType(concrete2);
    T1->SameTypeSource = T2->SameTypeSource;
  }
  
  // Make T1 the representative of T2, merging the equivalence classes.
  T2->Representative = T1;
  T2->SameTypeSource = Source;
  for (auto equiv : T2->EquivalenceClass)
    T1->EquivalenceClass.push_back(equiv);

  // FIXME: superclass requirements!

  // Add all of the protocol conformance requirements of T2 to T1.
  for (auto conforms : T2->ConformsTo) {
    T1->addConformance(conforms.first, conforms.second, *this);
  }

  // Recursively merge the associated types of T2 into T1.
  RequirementSource redundantSource(RequirementSource::Redundant, SourceLoc());
  for (auto T2Nested : T2->NestedTypes) {
    auto T1Nested = T1->getNestedType(T2Nested.first, *this);
    if (addSameTypeRequirementBetweenArchetypes(T1Nested,
                                                T2Nested.second.front(),
                                                redundantSource))
      return true;
  }

  return false;
}

bool ArchetypeBuilder::addSameTypeRequirementToConcrete(
       PotentialArchetype *T,
       Type Concrete,
       RequirementSource Source) {
  // Operate on the representative.
  T = T->getRepresentative();
  
  assert(!T->ArchetypeOrConcreteType.getAsArchetype()
         && "already formed archetype for concrete-constrained parameter");
  
  // If we've already been bound to a type, we're either done, or we have a
  // problem.
  if (auto oldConcrete = T->ArchetypeOrConcreteType.getAsConcreteType()) {
    if (!oldConcrete->isEqual(Concrete)) {
      Diags.diagnose(Source.getLoc(), diag::requires_same_type_conflict,
                     T->getName(), oldConcrete, Concrete);
      return true;
    }
    return false;
  }
  
  // Don't allow a generic parameter to be equivalent to a concrete type,
  // because then we don't actually have a parameter.
  // FIXME: Should we simply allow this?
  if (T->getNestingDepth() == 0) {
    Diags.diagnose(Source.getLoc(), 
                   diag::requires_generic_param_made_equal_to_concrete,
                   T->getName());
    return true;
  }
  
  // Make sure the concrete type fulfills the requirements on the archetype.
  DenseMap<ProtocolDecl *, ProtocolConformance*> conformances;
  if (!Concrete->is<ArchetypeType>()) {
    for (auto conforms : T->getConformsTo()) {
      auto protocol = conforms.first;
      auto conformance = Mod.lookupConformance(Concrete, protocol,
                                               getLazyResolver());
      if (!conformance) {
        Diags.diagnose(Source.getLoc(),
                       diag::requires_generic_param_same_type_does_not_conform,
                       Concrete, protocol->getName());
        return true;
      }

      assert(conformance->isConcrete() && "Abstract conformance?");
      conformances[protocol] = conformance->getConcrete();
    }
  }
  
  // Record the requirement.
  T->ArchetypeOrConcreteType = NestedType::forConcreteType(Concrete);
  T->SameTypeSource = Source;

  // Recursively resolve the associated types to their concrete types.
  for (auto nested : T->getNestedTypes()) {
    AssociatedTypeDecl *assocType
      = nested.second.front()->getResolvedAssociatedType();
    if (auto *concreteArchetype = Concrete->getAs<ArchetypeType>()) {
      ArchetypeType::NestedType witnessType =
          concreteArchetype->getNestedType(nested.first);
      addSameTypeRequirementToConcrete(nested.second.front(),
                                       witnessType.getValue(),
                                       Source);
    } else {
      assert(conformances.count(assocType->getProtocol()) > 0
             && "missing conformance?");
      auto witness = conformances[assocType->getProtocol()]
            ->getTypeWitness(assocType, getLazyResolver());
      auto witnessType = witness.getReplacement();
      if (auto witnessPA = resolveArchetype(witnessType)) {
        addSameTypeRequirementBetweenArchetypes(nested.second.front(),
                                                witnessPA,
                                                Source);
      } else {
        addSameTypeRequirementToConcrete(nested.second.front(),
                                         witnessType,
                                         Source);
      }
    }
  }
  
  return false;
}
                                                               
bool ArchetypeBuilder::addSameTypeRequirement(Type Reqt1, Type Reqt2,
                                              RequirementSource Source) {
  // Find the potential archetypes.
  PotentialArchetype *T1 = resolveArchetype(Reqt1);
  PotentialArchetype *T2 = resolveArchetype(Reqt2);
  
  // Require that at least one side of the requirement be a potential archetype.
  if (!T1 && !T2) {
    assert(Source.getLoc().isValid() && "reintroducing invalid requirement");
    Diags.diagnose(Source.getLoc(), diag::requires_no_same_type_archetype);
    return true;
  }
  
  // If both sides of the requirement are open archetypes, combine them.
  if (T1 && T2)
    return addSameTypeRequirementBetweenArchetypes(T1, T2, Source);
  
  // Otherwise, we're binding an open archetype.
  if (T1)
    return addSameTypeRequirementToConcrete(T1, Reqt2, Source);
  return addSameTypeRequirementToConcrete(T2, Reqt1, Source);
}

bool ArchetypeBuilder::addAbstractTypeParamRequirements(
       AbstractTypeParamDecl *decl,
       PotentialArchetype *pa,
       RequirementSource::Kind kind,
       llvm::SmallPtrSetImpl<ProtocolDecl *> &visited) {
  // Local function to mark the given associated type as recursive,
  // diagnosing it if this is the first such occurrence.
  auto markRecursive = [&](AssociatedTypeDecl *assocType,
                           ProtocolDecl *proto,
                           SourceLoc loc) {
    if (!pa->isRecursive() && !assocType->isRecursive()) {
      Diags.diagnose(assocType->getLoc(),
                     diag::recursive_requirement_reference);
        
      // Mark all associatedtypes in this protocol as recursive (and error-type)
      // to avoid later crashes dealing with this invalid protocol in other
      // contexts.
      auto containingProto =
        assocType->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
      for (auto member : containingProto->getMembers())
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(member))
          assocType->setIsRecursive();
    }
    pa->setIsRecursive();

    // Silence downstream errors referencing this associated type.
    assocType->setInvalid();

    // FIXME: Drop this protocol.
    pa->addConformance(proto, RequirementSource(kind, loc), *this);
  };

  // If the abstract type parameter already has an archetype assigned,
  // use that information.
  if (auto archetype = decl->getArchetype()) {
    SourceLoc loc = decl->getLoc();

    // Superclass requirement.
    if (auto superclass = archetype->getSuperclass()) {
      if (addSuperclassRequirement(pa, superclass,
                                   RequirementSource(kind, loc)))
        return true;
    }

    // Conformance requirements.
    for (auto proto : archetype->getConformsTo()) {
      if (visited.count(proto)) {
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(decl))
          markRecursive(assocType, proto, loc);

        continue;
      }

      if (addConformanceRequirement(pa, proto, RequirementSource(kind, loc),
                                    visited))
        return true;
    }

    return false;
  }

  // Otherwise, walk the 'inherited' list to identify requirements.
  if (auto resolver = getLazyResolver())
    resolver->resolveInheritanceClause(decl);
  return visitInherited(decl->getInherited(),
                        [&](Type inheritedType, SourceLoc loc) -> bool {
    // Protocol requirement.
    if (auto protocolType = inheritedType->getAs<ProtocolType>()) {
      if (visited.count(protocolType->getDecl())) {
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(decl))
          markRecursive(assocType, protocolType->getDecl(), loc);

        return true;
      }

      return addConformanceRequirement(pa, protocolType->getDecl(),
                                       RequirementSource(kind, loc),
                                       visited);
    }

    // Superclass requirement.
    if (inheritedType->getClassOrBoundGenericClass()) {
      return addSuperclassRequirement(pa, inheritedType,
                                      RequirementSource(kind, loc));
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
    return addSameTypeRequirement(Req.getFirstType(), 
                                  Req.getSecondType(),
                                  RequirementSource(RequirementSource::Explicit,
                                                    Req.getEqualLoc()));
  }

  llvm_unreachable("Unhandled requirement?");
}

void ArchetypeBuilder::addRequirement(const Requirement &req, 
                                      RequirementSource source) {
  switch (req.getKind()) {
  case RequirementKind::Superclass: {
    PotentialArchetype *pa = resolveArchetype(req.getFirstType());
    assert(pa && "Re-introducing invalid requirement");

    assert(req.getSecondType()->getClassOrBoundGenericClass());
    addSuperclassRequirement(pa, req.getSecondType(), source);
    return;
  }

  case RequirementKind::Conformance: {
    PotentialArchetype *pa = resolveArchetype(req.getFirstType());
    assert(pa && "Re-introducing invalid requirement");
    // FIXME: defensively return if assertions are disabled until we figure out
    // how this sitatuaion can occur and fix it properly.
    if (!pa)
      return;

    SmallVector<ProtocolDecl *, 4> conformsTo;
    bool existential = req.getSecondType()->isExistentialType(conformsTo);
    assert(existential && "Re-introducing invalid requirement");
    (void)existential;

    // Add each of the protocols.
    for (auto proto : conformsTo) {
      bool invalid = addConformanceRequirement(pa, proto, source);
      assert(!invalid && "Re-introducing invalid requirement");
      (void)invalid;
    }

    return;
  }

  case RequirementKind::SameType:
    addSameTypeRequirement(req.getFirstType(), req.getSecondType(), source);
    return;
    
  case RequirementKind::WitnessMarker:
    return;
  }

  llvm_unreachable("Unhandled requirement?");
}

/// AST walker that infers requirements from type representations.
class ArchetypeBuilder::InferRequirementsWalker : public TypeWalker {
  ArchetypeBuilder &Builder;
  SourceLoc Loc;
  bool HadError = false;
  unsigned Depth;

  /// We cannot add requirements to archetypes from outer generic parameter
  /// lists.
  bool isOuterArchetype(PotentialArchetype *PA) {
    unsigned ParamDepth = PA->getRootParam()->getDepth();
    assert(ParamDepth <= Depth);
    return ParamDepth < Depth;
  }

public:
  InferRequirementsWalker(ArchetypeBuilder &builder,
                          SourceLoc loc,
                          unsigned Depth)
    : Builder(builder), Loc(loc), Depth(Depth) { }

  bool hadError() const { return HadError; }

  virtual Action walkToTypePost(Type ty) override { 
    auto boundGeneric = ty->getAs<BoundGenericType>();
    if (!boundGeneric)
      return Action::Continue; 

    auto genericSig = boundGeneric->getDecl()->getGenericSignature();
    if (!genericSig)
      return Action::Stop;

    auto params = genericSig->getInnermostGenericParams();
    auto args = boundGeneric->getGenericArgs();

    // Produce substitutions from the generic parameters to the actual
    // arguments.
    TypeSubstitutionMap substitutions;
    for (unsigned i = 0, n = params.size(); i != n; ++i) {
      substitutions[params[i]->getCanonicalType()->castTo<SubstitutableType>()]
        = args[i];
    }

    // Handle the requirements.
    RequirementSource source(RequirementSource::Inferred, Loc);
    for (const auto &req : genericSig->getRequirements()) {
      switch (req.getKind()) {
      case RequirementKind::WitnessMarker:
        break;

      case RequirementKind::SameType: {
        auto firstType = req.getFirstType().subst(
                           &Builder.getModule(),
                           substitutions,
                           SubstFlags::IgnoreMissing);
        if (!firstType)
          break;

        auto firstPA = Builder.resolveArchetype(firstType);

        if (firstPA && isOuterArchetype(firstPA))
          return Action::Continue;

        auto secondType = req.getSecondType().subst(
                            &Builder.getModule(), 
                            substitutions,
                            SubstFlags::IgnoreMissing);
        if (!secondType)
          break;
        auto secondPA = Builder.resolveArchetype(secondType);

        if (firstPA && secondPA) {
          if (Builder.addSameTypeRequirementBetweenArchetypes(firstPA, secondPA,
                                                              source)) {
            HadError = true;
            return Action::Stop;
          }
        } else if (firstPA || secondPA) {
          auto PA = firstPA ? firstPA : secondPA;
          auto concrete = firstPA ? secondType : firstType;
          if (Builder.addSameTypeRequirementToConcrete(PA, concrete, source)) {
            HadError = true;
            return Action::Stop;
          }
        }
        break;
      }

      case RequirementKind::Superclass:
      case RequirementKind::Conformance: {
        auto subjectType = req.getFirstType().subst(
                             &Builder.getModule(),
                             substitutions,
                             SubstFlags::IgnoreMissing);
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
            HadError = true;
            return Action::Stop;
          }
        } else {
          if (Builder.addSuperclassRequirement(subjectPA, req.getSecondType(),
                                               source)) {
            HadError = true;
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

bool ArchetypeBuilder::inferRequirements(TypeLoc type,
                                         GenericParamList *genericParams) {
  if (!type.getType())
    return true;
  if (genericParams == nullptr)
    return false;
  // FIXME: Crummy source-location information.
  InferRequirementsWalker walker(*this, type.getSourceRange().Start,
                                 genericParams->getDepth());
  type.getType().walk(walker);
  return walker.hadError();
}

bool ArchetypeBuilder::inferRequirements(ParameterList *params,
                                         GenericParamList *genericParams) {
  if (genericParams == nullptr)
    return false;
  
  bool hadError = false;
  for (auto P : *params)
    hadError |= inferRequirements(P->getTypeLoc(), genericParams);
  return hadError;
}

/// Perform typo correction on the given nested type, producing the
/// corrected name (if successful).
static Identifier typoCorrectNestedType(
                    ArchetypeBuilder::PotentialArchetype *pa) {
  StringRef name = pa->getName().str();

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
                                         /*allowReplacements=*/true,
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

bool ArchetypeBuilder::finalize(SourceLoc loc) {
  bool invalid = false;

  // If any nested types remain unresolved, produce diagnostics.
  if (Impl->NumUnresolvedNestedTypes > 0) {
    visitPotentialArchetypes([&](PotentialArchetype *pa) {
      // We only care about nested types that haven't been resolved.
      if (pa->getParent() == nullptr || pa->getResolvedAssociatedType() ||
          /* FIXME: Should be able to handle this earlier */pa->getSuperclass())
        return;

      // If a typealias with this name exists in one of the parent protocols,
      // give a special diagnosis.
      auto parentConformances = pa->getParent()->getConformsTo();
      for (auto &conforms : parentConformances) {
        for (auto member : conforms.first->getMembers()) {
          auto typealias = dyn_cast<TypeAliasDecl>(member);
          if (!typealias || typealias->getName() != pa->getName())
            continue;

          Context.Diags.diagnose(loc, diag::invalid_member_type_alias,
                                 pa->getName());
          invalid = true;
          pa->setInvalid();
          return;
        }
      }
      
      // Try to typo correct to a nested type name.
      Identifier correction = typoCorrectNestedType(pa);
      if (correction.empty()) {
        invalid = true;
        pa->setInvalid();
        return;
      }

      // Set the typo-corrected name.
      pa->setRenamed(correction);
      
      // Resolve the associated type and merge the potential archetypes.
      auto replacement = pa->getParent()->getNestedType(correction, *this);
      addSameTypeRequirementBetweenArchetypes(
        pa, replacement,
        RequirementSource(RequirementSource::Redundant, SourceLoc()));
    });
  }

  return invalid;
}

ArchetypeType *
ArchetypeBuilder::getArchetype(GenericTypeParamDecl *GenericParam) {
  auto known = Impl->PotentialArchetypes.find(
                 GenericTypeParamKey::forDecl(GenericParam));
  if (known == Impl->PotentialArchetypes.end())
    return nullptr;

  return known->second->getType(*this).getAsArchetype();
}

template<typename F>
void ArchetypeBuilder::visitPotentialArchetypes(F f) {
  // Stack containing all of the potential archetypes to visit.
  SmallVector<PotentialArchetype *, 4> stack;
  llvm::SmallPtrSet<PotentialArchetype *, 4> visited;

  // Add top-level potential archetypes to the stack.
  for (const auto &pa : Impl->PotentialArchetypes) {
    if (visited.insert(pa.second).second)
      stack.push_back(pa.second);
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

void ArchetypeBuilder::enumerateRequirements(llvm::function_ref<
                     void (RequirementKind kind,
                           PotentialArchetype *archetype,
                           llvm::PointerUnion<Type, PotentialArchetype *> type,
                           RequirementSource source)> f) {
  // First, collect all archetypes, and sort them.
  SmallVector<PotentialArchetype *, 8> archetypes;
  visitPotentialArchetypes([&](PotentialArchetype *archetype) {
    archetypes.push_back(archetype);
  });

  llvm::array_pod_sort(archetypes.begin(), archetypes.end(),
                       compareDependentTypes);

  for (auto *archetype : archetypes) {
    // Invalid archetypes are never representatives in well-formed or corrected
    // signature, so we don't need to visit them.
    if (archetype->isInvalid())
      continue;

    // If this type is not the representative, or if it was made concrete,
    // we emit a same-type constraint.
    if (archetype->getRepresentative() != archetype ||
        archetype->isConcreteType()) {
      auto *first = archetype;
      auto *second = archetype->getRepresentative();

      if (second->isConcreteType()) {
        Type concreteType = second->getConcreteType();
        f(RequirementKind::SameType, first, concreteType,
          first->getSameTypeSource());
        continue;
      }

      assert(!first->isConcreteType());

      // Neither one is concrete. Put the shorter type first.
      if (compareDependentTypes(&first, &second) > 0)
        std::swap(first, second);

      f(RequirementKind::SameType, first, second,
        archetype->getSameTypeSource());
      continue;
    }

    // Add the witness marker.
    f(RequirementKind::WitnessMarker, archetype, Type(),
      RequirementSource(RequirementSource::Explicit, SourceLoc()));

    // If we have a superclass, produce a superclass requirement
    if (Type superclass = archetype->getSuperclass()) {
      f(RequirementKind::Superclass, archetype, superclass,
        archetype->getSuperclassSource());
    }

    // Enumerate conformance requirements.
    SmallVector<ProtocolDecl *, 4> protocols;
    DenseMap<ProtocolDecl *, RequirementSource> protocolSources;
    for (const auto &conforms : archetype->getConformsTo()) {
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
                            llvm::PointerUnion<Type, PotentialArchetype *> type,
                            RequirementSource source) {
    switch (kind) {
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
      out << "\n  ";
      out << archetype->getDebugName() << " : " 
          << type.get<Type>().getString() << " [";
      source.dump(out, &Context.SourceMgr);
      out << "]";
      break;

    case RequirementKind::SameType:
      out << "\n  ";
      out << archetype->getDebugName() << " == " ;
      if (auto secondType = type.dyn_cast<Type>()) {
        out << secondType.getString();
      } else {
        out << type.get<PotentialArchetype *>()->getDebugName();
      }
      out << " [";
      source.dump(out, &Context.SourceMgr);
      out << "]";
      break;

    case RequirementKind::WitnessMarker:
      out << "\n  " << archetype->getDebugName() << " witness marker";
      break;
    }
  });
  out << "\n";
}

Type ArchetypeBuilder::mapTypeIntoContext(const DeclContext *dc, Type type) {
  return mapTypeIntoContext(dc->getParentModule(),
                            dc->getGenericEnvironmentOfContext(),
                            type);
}

Type ArchetypeBuilder::mapTypeIntoContext(ModuleDecl *M,
                                          GenericEnvironment *env,
                                          Type type) {
  auto canType = type->getCanonicalType();
  assert(!canType->hasArchetype() && "already have a contextual type");
  if (!canType->hasTypeParameter())
    return type;

  assert(env && "dependent type in non-generic context");

  return env->mapTypeIntoContext(M, type);
}

Type
ArchetypeBuilder::mapTypeOutOfContext(const DeclContext *dc, Type type) {
  return mapTypeOutOfContext(dc->getParentModule(),
                             dc->getGenericEnvironmentOfContext(),
                             type);
}

Type
ArchetypeBuilder::mapTypeOutOfContext(ModuleDecl *M,
                                      GenericEnvironment *env,
                                      Type type) {
  auto canType = type->getCanonicalType();
  assert(!canType->hasTypeParameter() && "already have an interface type");
  if (!canType->hasArchetype())
    return type;

  assert(env && "dependent type in non-generic context");

  return env->mapTypeOutOfContext(M, type);
}

void ArchetypeBuilder::addGenericSignature(GenericSignature *sig,
                                           GenericEnvironment *env,
                                           bool treatRequirementsAsExplicit) {
  if (!sig) return;
  
  RequirementSource::Kind sourceKind = treatRequirementsAsExplicit
    ? RequirementSource::Explicit
    : RequirementSource::OuterScope;
  
  for (auto param : sig->getGenericParams()) {
    addGenericParameter(param);

    if (env) {
      // If this generic parameter has an archetype, use it as the concrete
      // type.
      auto contextTy = env->mapTypeIntoContext(&Mod, param);
      if (auto archetype = contextTy->getAs<ArchetypeType>()) {
        auto key = GenericTypeParamKey::forType(param);
        assert(Impl->PotentialArchetypes.count(key) && "Missing parameter?");
        auto *pa = Impl->PotentialArchetypes[key];
        assert(pa == pa->getRepresentative() && "Not the representative");
        pa->ArchetypeOrConcreteType = NestedType::forConcreteType(archetype);
        pa->SameTypeSource = RequirementSource(sourceKind, SourceLoc());
      }
    }
  }

  RequirementSource source(sourceKind, SourceLoc());
  for (auto &reqt : sig->getRequirements()) {
    addRequirement(reqt, source);
  }
}

Type ArchetypeBuilder::substDependentType(Type type) {
  return substConcreteTypesForDependentTypes(*this, type);
}

/// Collect the set of requirements placed on the given generic parameters and
/// their associated types.
static void collectRequirements(ArchetypeBuilder &builder,
                                ArrayRef<GenericTypeParamType *> params,
                                SmallVectorImpl<Requirement> &requirements) {
  builder.enumerateRequirements([&](RequirementKind kind,
          ArchetypeBuilder::PotentialArchetype *archetype,
          llvm::PointerUnion<Type, ArchetypeBuilder::PotentialArchetype *> type,
          RequirementSource source) {
    // Filter out redundant requirements.
    switch (source.getKind()) {
    case RequirementSource::Explicit:
    case RequirementSource::Inferred:
    case RequirementSource::OuterScope:
      // The requirement was explicit and required, keep it.
      break;

    case RequirementSource::Protocol:
    case RequirementSource::Redundant:
      // The requirement was redundant, drop it.
      return;
    }

    auto depTy = archetype->getDependentType(builder, false);

    if (depTy->is<ErrorType>())
      return;

    if (kind == RequirementKind::WitnessMarker) {
      requirements.push_back(Requirement(kind, depTy, Type()));
      return;
    }

    Type repTy;
    if (auto concreteTy = type.dyn_cast<Type>()) {
      // Maybe we were equated to a concrete type...
      repTy = concreteTy;
    } else {
      // ...or to a dependent type.
      repTy = type.get<ArchetypeBuilder::PotentialArchetype *>()
          ->getDependentType(builder, false);
    }

    if (repTy->is<ErrorType>())
      return;

    requirements.push_back(Requirement(kind, depTy, repTy));
  });
}

GenericSignature *ArchetypeBuilder::getGenericSignature(
    ArrayRef<GenericTypeParamType *> genericParamTypes) {
  // Collect the requirements placed on the generic parameter types.
  SmallVector<Requirement, 4> requirements;
  collectRequirements(*this, genericParamTypes, requirements);

  auto sig = GenericSignature::get(genericParamTypes, requirements);
  return sig;
}

GenericEnvironment *ArchetypeBuilder::getGenericEnvironment(
    ArrayRef<GenericTypeParamType *> genericParamTypes) {
  TypeSubstitutionMap interfaceToArchetypeMap;

  for (auto paramTy : genericParamTypes) {
    auto known = Impl->PotentialArchetypes.find(
                   GenericTypeParamKey::forType(paramTy));
    assert(known != Impl->PotentialArchetypes.end());

    auto archetypeTy = known->second->getType(*this).getAsArchetype();
    auto concreteTy = known->second->getType(*this).getAsConcreteType();
    if (archetypeTy)
      interfaceToArchetypeMap[paramTy] = archetypeTy;
    else if (concreteTy)
      interfaceToArchetypeMap[paramTy] = concreteTy;
    else
      llvm_unreachable("broken generic parameter");
  }

  return GenericEnvironment::get(Context, interfaceToArchetypeMap);
}

