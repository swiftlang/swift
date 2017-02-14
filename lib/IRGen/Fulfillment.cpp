//===--- Fulfillment.cpp - Static metadata search  ------------------------===//
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
//  This file implements routines for searching for ways to find metadata
//  from other metadata.
//
//===----------------------------------------------------------------------===//

#include "Fulfillment.h"
#include "IRGenModule.h"

#include "swift/AST/Decl.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/TypeLowering.h"
#include "GenericRequirement.h"

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
  case TypeKind::Error:
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
bool FulfillmentMap::searchTypeMetadata(IRGenModule &IGM, CanType type,
                                        IsExact_t isExact,
                                        unsigned source, MetadataPath &&path,
                                        const InterestingKeysCallback &keys) {

  // If this is an exact source, and it's an interesting type, add this
  // as a fulfillment.
  if (isExact && keys.isInterestingType(type)) {
    // If the type isn't a leaf type, also check it as an inexact match.
    bool hadFulfillment = false;
    if (!isLeafTypeMetadata(type)) {
      hadFulfillment |= searchTypeMetadata(IGM, type, IsInexact, source,
                                           MetadataPath(path), keys);
    }

    // Add the fulfillment.
    hadFulfillment |= addFulfillment({type, nullptr}, source, std::move(path));
    return hadFulfillment;
  }

  // Inexact metadata will be a problem if we ever try to use this
  // to remember that we already have the metadata for something.
  if (auto nomTy = dyn_cast<NominalType>(type)) {
    return searchNominalTypeMetadata(IGM, nomTy, source, std::move(path), keys);
  }
  if (auto boundTy = dyn_cast<BoundGenericType>(type)) {
    return searchBoundGenericTypeMetadata(IGM, boundTy, source,
                                          std::move(path), keys);
  }

  // TODO: tuples
  // TODO: functions
  // TODO: metatypes

  return false;
}

/// Given that we have a source for a witness table that the given type
/// conforms to the given protocol, check to see if it fulfills anything.
bool FulfillmentMap::searchWitnessTable(IRGenModule &IGM,
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

  return searchWitnessTable(IGM, type, protocol, source, std::move(path), keys,
                            interestingConformances);
}

bool FulfillmentMap::searchWitnessTable(IRGenModule &IGM,
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
    hadFulfillment |= searchWitnessTable(IGM, type, inherited,
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


bool FulfillmentMap::searchParentTypeMetadata(IRGenModule &IGM,
                                              NominalTypeDecl *decl,
                                              CanType parent,
                                              unsigned source,
                                              MetadataPath &&path,
                                        const InterestingKeysCallback &keys) {
  // We might not have a parent type.
  if (!parent) return false;

  // If we do, it has to be nominal one way or another.
  path.addNominalParentComponent();
  return searchTypeMetadata(IGM, parent, IsExact, source, std::move(path),keys);
}

bool FulfillmentMap::searchNominalTypeMetadata(IRGenModule &IGM,
                                               CanNominalType type,
                                               unsigned source,
                                               MetadataPath &&path,
                                         const InterestingKeysCallback &keys) {
  // Nominal types add no generic arguments themselves, but they
  // may have the arguments of their parents.
  return searchParentTypeMetadata(IGM, type->getDecl(), type.getParent(),
                                  source, std::move(path), keys);
}

bool FulfillmentMap::searchBoundGenericTypeMetadata(IRGenModule &IGM,
                                                    CanBoundGenericType type,
                                                    unsigned source,
                                                    MetadataPath &&path,
                                         const InterestingKeysCallback &keys) {
  // Objective-C generics don't preserve their generic parameters at runtime,
  // so they aren't able to fulfill type metadata requirements.
  if (type->getDecl()->hasClangNode()) {
    return false;
  }
  
  bool hadFulfillment = false;

  GenericTypeRequirements requirements(IGM, type->getDecl());
  requirements.enumerateFulfillments(
      IGM, type->getContextSubstitutionMap(IGM.getSwiftModule(), type->getDecl()),
      [&](unsigned reqtIndex, CanType arg,
          Optional<ProtocolConformanceRef> conf) {
    // Skip uninteresting type arguments.
    if (!keys.hasInterestingType(arg))
      return;

    // If the fulfilled value is type metadata, refine the path.
    if (!conf) {
      MetadataPath argPath = path;
      argPath.addNominalTypeArgumentComponent(reqtIndex);
      hadFulfillment |=
        searchTypeMetadata(IGM, arg, IsExact, source, std::move(argPath), keys);
      return;
    }

    // Otherwise, it's a conformance.

    // Ignore it unless the type itself is interesting.
    if (!keys.isInterestingType(arg))
      return;

    // Refine the path.
    MetadataPath argPath = path;
    argPath.addNominalTypeArgumentConformanceComponent(reqtIndex);

    llvm::SmallPtrSet<ProtocolDecl*, 4> interestingConformancesBuffer;
    llvm::SmallPtrSetImpl<ProtocolDecl*> *interestingConformances = nullptr;

    // If the interesting-keys set is limiting the set of interesting
    // conformances, collect that filter.
    if (keys.hasLimitedInterestingConformances(arg)) {
      // Bail out immediately if the set is empty.
      auto requiredConformances = keys.getInterestingConformances(arg);
      if (requiredConformances.empty()) return;

      interestingConformancesBuffer.insert(requiredConformances.begin(),
                                           requiredConformances.end());
      interestingConformances = &interestingConformancesBuffer;
    }

    hadFulfillment |=
      searchWitnessTable(IGM, arg, conf->getRequirement(), source,
                         std::move(argPath), keys, interestingConformances);
  });

  // Also match against the parent.  The polymorphic type
  // will start with any arguments from the parent.
  hadFulfillment |= searchParentTypeMetadata(IGM, type->getDecl(),
                                             type.getParent(),
                                             source, std::move(path), keys);
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

bool FulfillmentMap::Everything::isInterestingType(CanType type) const {
  return true;
}
bool FulfillmentMap::Everything::hasInterestingType(CanType type) const {
  return true;
}
bool FulfillmentMap::Everything
                   ::hasLimitedInterestingConformances(CanType type) const {
  return false;
}
GenericSignature::ConformsToArray
FulfillmentMap::Everything::getInterestingConformances(CanType type) const{
  return {};
}

void FulfillmentMap::dump() const {
  auto &out = llvm::errs();
  for (auto &entry : Fulfillments) {
    out << "(" << entry.first.first;
    if (auto proto = entry.first.second) {
      out << ", " << proto->getNameStr();
    }
    out << ") => sources[" << entry.second.SourceIndex
        << "]." << entry.second.Path << "\n";
  }
}
