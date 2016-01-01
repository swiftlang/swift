//===--- Fulfillment.cpp - Static metadata search  ------------------------===//
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
//  This file implements routines for searching for ways to find metadata
//  from other metadata.
//
//===----------------------------------------------------------------------===//

#include "Fulfillment.h"

#include "swift/AST/Decl.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace irgen;

/// Is metadata for the given type kind a "leaf", or does it possibly
/// store any other type metadata that we can statically extract?
///
/// It's okay to conservatively answer "no".  It's more important for this
/// to be quick than for it to be accurate; don't recurse.
static bool isLeafTypeMetadata(CanType type) {
  switch (type->getKind()) {
#define SUGARED_TYPE(ID, SUPER) \
  case TypeKind::ID:
#define UNCHECKED_TYPE(ID, SUPER) \
  case TypeKind::ID:
#define TYPE(ID, SUPER)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("kind is invalid for a canonical type");

#define ARTIFICIAL_TYPE(ID, SUPER) \
  case TypeKind::ID:
#define TYPE(ID, SUPER)
#include "swift/AST/TypeNodes.def"
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::DynamicSelf:
    llvm_unreachable("these types do not have metadata");

  // All the builtin types are leaves.
#define BUILTIN_TYPE(ID, SUPER) \
  case TypeKind::ID:
#define TYPE(ID, SUPER)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Module:
    return true;

  // Type parameters are statically opaque.
  case TypeKind::Archetype:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    return true;

  // Only the empty tuple is a leaf.
  case TypeKind::Tuple:
    return cast<TupleType>(type)->getNumElements() == 0;

  // Nominal types might have parents.
  case TypeKind::Class:
  case TypeKind::Enum:
  case TypeKind::Protocol:
  case TypeKind::Struct:
    return !cast<NominalType>(type)->getParent();

  // Bound generic types have type arguments.
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
    return false;

  // Functions have component types.
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction:  // included for future-proofing
    return false;

  // Protocol compositions have component types.
  case TypeKind::ProtocolComposition:
    return false;

  // Metatypes have instance types.
  case TypeKind::Metatype:
  case TypeKind::ExistentialMetatype:
    return false;
  }
  llvm_unreachable("bad type kind");
}

/// Given that we have a source for metadata of the given type, check
/// to see if it fulfills anything.
///
/// \param isExact - true if the metadata is known to be exactly the
///   metadata for the given type, false if it might be a subtype
bool FulfillmentMap::searchTypeMetadata(ModuleDecl &M, CanType type,
                                        IsExact_t isExact,
                                        unsigned source, MetadataPath &&path,
                                        const InterestingKeysCallback &keys) {

  // If this is an exact source, and it's an interesting type, add this
  // as a fulfillment.
  if (isExact && keys.isInterestingType(type)) {
    // If the type isn't a leaf type, also check it as an inexact match.
    bool hadFulfillment = false;
    if (!isLeafTypeMetadata(type)) {
      hadFulfillment |= searchTypeMetadata(M, type, IsInexact, source,
                                           MetadataPath(path), keys);
    }

    // Add the fulfillment.
    hadFulfillment |= addFulfillment({type, nullptr}, source, std::move(path));
    return hadFulfillment;
  }

  // Inexact metadata will be a problem if we ever try to use this
  // to remember that we already have the metadata for something.
  if (auto nomTy = dyn_cast<NominalType>(type)) {
    return searchNominalTypeMetadata(M, nomTy, source, std::move(path), keys);
  }
  if (auto boundTy = dyn_cast<BoundGenericType>(type)) {
    return searchBoundGenericTypeMetadata(M, boundTy, source, std::move(path),
                                          keys);
  }

  // TODO: tuples
  // TODO: functions
  // TODO: metatypes

  return false;
}

/// Given that we have a source for a witness table that the given type
/// conforms to the given protocol, check to see if it fulfills anything.
bool FulfillmentMap::searchWitnessTable(ModuleDecl &M,
                                        CanType type, ProtocolDecl *protocol,
                                        unsigned source, MetadataPath &&path,
                                        const InterestingKeysCallback &keys) {
  llvm::SmallPtrSet<ProtocolDecl*, 4> interestingConformancesBuffer;
  llvm::SmallPtrSetImpl<ProtocolDecl*> *interestingConformances = nullptr;

  // If the interesting-keys set is limiting the set of interesting
  // conformances, collect that filter.
  if (keys.isInterestingType(type) &&
      keys.hasLimitedInterestingConformances(type)) {
    // Bail out immediately if the set is empty.
    // This only makes sense because we're not trying to fulfill
    // associated types this way.
    auto requiredConformances = keys.getInterestingConformances(type);
    if (requiredConformances.empty()) return false;

    interestingConformancesBuffer.insert(requiredConformances.begin(),
                                         requiredConformances.end());
    interestingConformances = &interestingConformancesBuffer;
  }

  return searchWitnessTable(M, type, protocol, source, std::move(path), keys,
                            interestingConformances);
}

bool FulfillmentMap::searchWitnessTable(ModuleDecl &M,
                                        CanType type, ProtocolDecl *protocol,
                                        unsigned source, MetadataPath &&path,
                                        const InterestingKeysCallback &keys,
                                  const llvm::SmallPtrSetImpl<ProtocolDecl*> *
                                          interestingConformances) {
  assert(Lowering::TypeConverter::protocolRequiresWitnessTable(protocol));

  bool hadFulfillment = false;

  auto nextInheritedIndex = 0;
  for (auto inherited : protocol->getInheritedProtocols(nullptr)) {
    auto index = nextInheritedIndex++;

    // Ignore protocols that don't have witness tables.
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(inherited))
      continue;

    MetadataPath inheritedPath = path;
    inheritedPath.addInheritedProtocolComponent(index);
    hadFulfillment |= searchWitnessTable(M, type, inherited,
                                         source, std::move(inheritedPath),
                                         keys, interestingConformances);
  }

  // If we're not limited the set of interesting conformances, or if
  // this is an interesting conformance, record it.
  if (!interestingConformances || interestingConformances->count(protocol)) {
    hadFulfillment |= addFulfillment({type, protocol}, source, std::move(path));
  }

  return hadFulfillment;
}


bool FulfillmentMap::searchParentTypeMetadata(ModuleDecl &M, CanType parent,
                                              unsigned source,
                                              MetadataPath &&path,
                                        const InterestingKeysCallback &keys) {
  // We might not have a parent type.
  if (!parent) return false;

  // If we do, it has to be nominal one way or another.
  path.addNominalParentComponent();
  return searchTypeMetadata(M, parent, IsExact, source, std::move(path), keys);
}

bool FulfillmentMap::searchNominalTypeMetadata(ModuleDecl &M,
                                               CanNominalType type,
                                               unsigned source,
                                               MetadataPath &&path,
                                         const InterestingKeysCallback &keys) {
  // Nominal types add no generic arguments themselves, but they
  // may have the arguments of their parents.
  return searchParentTypeMetadata(M, type.getParent(),
                                  source, std::move(path), keys);
}

bool FulfillmentMap::searchBoundGenericTypeMetadata(ModuleDecl &M,
                                                    CanBoundGenericType type,
                                                    unsigned source,
                                                    MetadataPath &&path,
                                         const InterestingKeysCallback &keys) {
  auto params = type->getDecl()->getGenericParams()->getAllArchetypes();
  auto substitutions = type->getSubstitutions(&M, nullptr);
  assert(params.size() >= substitutions.size() &&
         "generic decl archetypes should parallel generic type subs");

  bool hadFulfillment = false;

  for (unsigned i = 0, e = substitutions.size(); i != e; ++i) {
    auto sub = substitutions[i];
    CanType arg = sub.getReplacement()->getCanonicalType();

    // Skip uninteresting type arguments.
    if (!keys.hasInterestingType(arg))
      continue;

    // If the argument is a type parameter, fulfill conformances for it.
    if (keys.isInterestingType(arg)) {
      hadFulfillment |=
        searchTypeArgConformances(M, arg, params[i], source, path, i, keys);
    }

    // Refine the path.
    MetadataPath argPath = path;
    argPath.addNominalTypeArgumentComponent(i);
    hadFulfillment |=
      searchTypeMetadata(M, arg, IsExact, source, std::move(argPath), keys);
  }

  // Also match against the parent.  The polymorphic type
  // will start with any arguments from the parent.
  hadFulfillment |= searchParentTypeMetadata(M, type.getParent(),
                                             source, std::move(path), keys);
  return hadFulfillment;
}

bool FulfillmentMap::searchTypeArgConformances(ModuleDecl &M, CanType arg,
                                               ArchetypeType *param,
                                               unsigned source,
                                               const MetadataPath &path,
                                               unsigned argIndex,
                                         const InterestingKeysCallback &keys) {
  // Our sources are the protocol conformances that are recorded in
  // the generic metadata.
  auto storedConformances = param->getConformsTo();
  if (storedConformances.empty()) return false;

  llvm::SmallPtrSet<ProtocolDecl*, 4> interestingConformancesBuffer;
  llvm::SmallPtrSetImpl<ProtocolDecl*> *interestingConformances = nullptr;

  // If the interesting-keys set is limiting the set of interesting
  // conformances, collect that filter.
  if (keys.hasLimitedInterestingConformances(arg)) {
    // Bail out immediately if the set is empty.
    auto requiredConformances = keys.getInterestingConformances(arg);
    if (requiredConformances.empty()) return false;

    interestingConformancesBuffer.insert(requiredConformances.begin(),
                                         requiredConformances.end());
    interestingConformances = &interestingConformancesBuffer;
  }

  bool hadFulfillment = false;

  for (size_t confIndex : indices(storedConformances)) {
    auto storedProtocol = storedConformances[confIndex];
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(storedProtocol))
      continue;

    MetadataPath confPath = path;
    confPath.addNominalTypeArgumentConformanceComponent(argIndex, confIndex);
    hadFulfillment |=
      searchWitnessTable(M, arg, storedProtocol, source, std::move(confPath),
                         keys, interestingConformances);
  }

  return hadFulfillment;
}

/// Testify that there's a fulfillment at the given path.
bool FulfillmentMap::addFulfillment(FulfillmentKey key,
                                    unsigned source, MetadataPath &&path) {
  // Only add a fulfillment if we don't have any previous
  // fulfillment for that value or if it 's cheaper than the existing
  // fulfillment.
  auto it = Fulfillments.find(key);
  if (it != Fulfillments.end()) {
    if (path.cost() >= it->second.Path.cost()) {
      return false;
    }

    it->second.SourceIndex = source;
    it->second.Path = std::move(path);
    return true;
  } else {
    Fulfillments.insert({ key, Fulfillment(source, std::move(path)) });
    return true;
  }
}
