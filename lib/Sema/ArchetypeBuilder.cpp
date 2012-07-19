//===--- ArchetypeBuilder.cpp - Generic Requirement Builder -------------===//
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
#include "ArchetypeBuilder.h"
#include "TypeChecker.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using llvm::DenseMap;

/// \brief A type that will be mapped down to some archetype, which gathers
/// all of the requirements and nested types of that archetype.
struct ArchetypeBuilder::PotentialArchetype {
  PotentialArchetype(StringRef DisplayName, Optional<unsigned> Index = Nothing)
    : DisplayName(DisplayName.str()), Index(Index), Archetype(nullptr) { }

  /// \brief The name used to describe this archetype.
  std::string DisplayName;

  /// \brief The index of the computed archetype.
  Optional<unsigned> Index;

  /// \brief The list of protocols to which this archetype will conform.
  llvm::SetVector<ProtocolDecl *> ConformsTo;

  /// \brief The set of nested typed stored within this protocol.
  DenseMap<Identifier, PotentialArchetype *> NestedTypes;

  /// \brief The actual archetype, once it has been assigned.
  ArchetypeType *Archetype;

  /// \brief Retrieve (or create) a nested type with the given name.
  PotentialArchetype *getNestedType(Identifier Name) {
    PotentialArchetype *&Result = NestedTypes[Name];
    if (!Result) {
      // FIXME: The 'This' hack is pretty ugly.
      if (Name.str() == "This")
        Result = this;
      else
        Result = new PotentialArchetype(DisplayName + "." + Name.get());
    }

    return Result;
  }

  /// \brief Retrieve (or build) the archetype corresponding to the potential
  /// archetype.
  ArchetypeType *getArchetype(TypeChecker &TC) {
    if (!Archetype) {
      // Allocate a new archetype.
      SmallVector<ProtocolDecl *, 4> Protos(ConformsTo.begin(),
                                            ConformsTo.end());
      Archetype = ArchetypeType::getNew(TC.Context, DisplayName, Protos, Index);

      // For each of the protocols we conform to, find the appropriate nested
      // types and add archetype mappings for them.
      // FIXME: This hideousness is caused by the hideousness inherent in
      // the AssociatedTypeMap anti-design. Fix that, and this becomes less
      // horrible.
      for (auto Proto : ConformsTo) {
        for (auto Member : Proto->getMembers()) {
          auto AssocType = dyn_cast<TypeAliasDecl>(Member);
          if (!AssocType)
            continue;

          ArchetypeType *AssocArchetype
            = AssocType->getDeclaredType()->castTo<ArchetypeType>();

          TC.Context.AssociatedTypeMap[Archetype][AssocArchetype]
            = getNestedType(AssocType->getName())->getArchetype(TC);
        }
      }
    }

    return Archetype;
  }

  void dump(llvm::raw_ostream &Out, unsigned Indent) {
    // Print name.
    StringRef Name = DisplayName;
    std::size_t DotPos = Name.rfind('.');
    if (DotPos != StringRef::npos)
      Name = Name.substr(DotPos+1);
    Out.indent(Indent) << Name;

    // Print requirements.
    if (!ConformsTo.empty()) {
      Out << " : ";
      if (ConformsTo.size() != 1)
        Out << "protocol<";

      bool First = true;
      for (auto Proto : ConformsTo) {
        if (First)
          First = false;
        else
          Out << ", ";

        Out << Proto->getName().str();
      }

      if (ConformsTo.size() != 1)
        Out << ">";
    }
    Out << "\n";

    // Print nested types.
    for (const auto &Nested : NestedTypes) {
      Nested.second->dump(Out, Indent + 2);
    }
  }
};

struct ArchetypeBuilder::Implementation {
  SmallVector<TypeAliasDecl *, 4> GenericParams;
  DenseMap<TypeAliasDecl *, PotentialArchetype *> PotentialArchetypes;
};

ArchetypeBuilder::ArchetypeBuilder(TypeChecker &TC)
  : TC(TC), Impl(new Implementation)
{
}

ArchetypeBuilder::~ArchetypeBuilder() {}

auto ArchetypeBuilder::resolveType(Type T) -> PotentialArchetype * {
  auto IdType = dyn_cast<IdentifierType>(T);
  if (!IdType)
    return nullptr;

  PotentialArchetype *Current = nullptr;
  if (!IdType->Components[0].Value.is<ValueDecl *>())
    return nullptr;

  // The first type needs to be known as a potential archetype, e.g., a
  // generic parameter.
  TypeAliasDecl *FirstType
    = dyn_cast<TypeAliasDecl>(IdType->Components[0].Value.get<ValueDecl *>());
  if (!FirstType)
    return nullptr;

  DenseMap<TypeAliasDecl *, PotentialArchetype *>::iterator Known
    = Impl->PotentialArchetypes.find(FirstType);
  if (Known == Impl->PotentialArchetypes.end())
    return nullptr;

  // Resolve nested types.
  Current = Known->second;
  for (unsigned I = 1, N = IdType->Components.size(); I != N; ++I) {
    Current = Current->getNestedType(IdType->Components[I].Id);
  }

  return Current;
}

bool ArchetypeBuilder::addGenericParameter(TypeAliasDecl *GenericParam,
                                           Optional<unsigned> Index) {
  Impl->GenericParams.push_back(GenericParam);

  // Create a potential archetype for this generic parameter.
  assert(!Impl->PotentialArchetypes[GenericParam]);
  auto PA = new PotentialArchetype(GenericParam->getName().str(), Index);
  Impl->PotentialArchetypes[GenericParam] = PA;

  // Add each of the requirements placed on this generic parameter.
  for (auto Inherited : GenericParam->getInherited()) {
    SmallVector<ProtocolDecl *, 4> ConformsTo;
    if (Inherited->isExistentialType(ConformsTo)) {
      for (auto Proto : ConformsTo)
        if (addConformanceRequirement(PA, Proto))
          return true;
    }
  }

  return false;
}

bool ArchetypeBuilder::addConformanceRequirement(PotentialArchetype *T,
                                                ProtocolDecl *Proto){
  // If we've already added this requirement, we're done.
  if (!T->ConformsTo.insert(Proto))
    return false;

  // Add all of the inherited protocol requirements, recursively.
  for (auto Inherited : Proto->getInherited()) {
    SmallVector<ProtocolDecl *, 4> InheritedProtos;
    if (Inherited->isExistentialType(InheritedProtos)) {
      for (auto InheritedProto : InheritedProtos) {
        if (addConformanceRequirement(T, InheritedProto))
          return true;
      }
    }
  }

  // Add requirements for each of the associated types.
  for (auto Member : Proto->getMembers()) {
    if (auto AssocType = dyn_cast<TypeAliasDecl>(Member)) {
      // FIXME: Another 'This' hack.
      if (AssocType->getName().str() == "This")
        continue;

      // Add requirements placed directly on this associated type.
      auto AssocPA = T->getNestedType(AssocType->getName());
      for (auto Inherited : AssocType->getInherited()) {
        SmallVector<ProtocolDecl *, 4> InheritedProtos;
        if (Inherited->isExistentialType(InheritedProtos)) {
          for (auto InheritedProto : InheritedProtos) {
            if (addConformanceRequirement(AssocPA, InheritedProto))
              return true;
          }
        }
      }

      continue;
    }

    // FIXME: Requirement declarations.
  }

  return false;
}

bool ArchetypeBuilder::addRequirement(const Requirement &Req) {
  switch (Req.getKind()) {
  case RequirementKind::Conformance: {
    PotentialArchetype *PA = resolveType(Req.getSubject());
    if (!PA) {
      // FIXME: Diagnose this failure.
      return true;
    }

    SmallVector<ProtocolDecl *, 4> ConformsTo;
    if (!Req.getProtocol()->isExistentialType(ConformsTo)) {
      // FIXME: Diagnose this failure here, rather than over in type-checking.
      return true;
    }

    // Add each of the protocols.
    for (auto Proto : ConformsTo)
      if (addConformanceRequirement(PA, Proto))
        return true;

    return false;
  }

  case RequirementKind::SameType: {
    // FIXME: Implement same-type constraints.
    return false;
  }
  }

  llvm_unreachable("Unhandled requirement?");
}

llvm::DenseMap<TypeAliasDecl *, ArchetypeType *>
ArchetypeBuilder::assignArchetypes() {
  llvm::DenseMap<TypeAliasDecl *, ArchetypeType *> Archetypes;
  for (const auto& PA : Impl->PotentialArchetypes) {
    Archetypes[PA.first] = PA.second->getArchetype(TC);
  }
  return std::move(Archetypes);
}

void ArchetypeBuilder::dump() {
  llvm::errs() << "Archetypes to build:\n";
  for (const auto& PA : Impl->PotentialArchetypes) {
    PA.second->dump(llvm::errs(), 2);
  }
}
