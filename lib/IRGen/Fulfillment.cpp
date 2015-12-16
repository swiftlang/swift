//===--- Fulfillment.cpp - Static metadata search  ------------------------===//
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
//  This file implements routines for searching for ways to find metadata
//  from other metadata.
//
//===----------------------------------------------------------------------===//

#include "Fulfillment.h"

#include "swift/AST/Decl.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace irgen;

/// Given that we have a source for metadata of the given type, check
/// to see if it fulfills anything.
///
/// \param isExact - true if the metadata is known to be exactly the
///   metadata for the given type, false if it might be a subtype
bool FulfillmentMap::searchTypeMetadata(ModuleDecl &M, CanType type,
                                        IsExact_t isExact,
                                        unsigned source, MetadataPath &&path,
                                        const InterestingKeysCallback *keys) {

  // Type parameters.  Inexact metadata are useless here.
  if (isExact && type->isTypeParameter()) {
    return addFulfillment({type, nullptr}, source, std::move(path));
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

bool FulfillmentMap::searchParentTypeMetadata(ModuleDecl &M, CanType parent,
                                              unsigned source,
                                              MetadataPath &&path,
                                        const InterestingKeysCallback *keys) {
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
                                         const InterestingKeysCallback *keys) {
  // Nominal types add no generic arguments themselves, but they
  // may have the arguments of their parents.
  return searchParentTypeMetadata(M, type.getParent(),
                                  source, std::move(path), keys);
}

bool FulfillmentMap::searchBoundGenericTypeMetadata(ModuleDecl &M,
                                                    CanBoundGenericType type,
                                                    unsigned source,
                                                    MetadataPath &&path,
                                         const InterestingKeysCallback *keys) {
  auto params = type->getDecl()->getGenericParams()->getAllArchetypes();
  auto substitutions = type->getSubstitutions(&M, nullptr);
  assert(params.size() >= substitutions.size() &&
         "generic decl archetypes should parallel generic type subs");

  bool hadFulfillment = false;

  for (unsigned i = 0, e = substitutions.size(); i != e; ++i) {
    auto sub = substitutions[i];
    CanType arg = sub.getReplacement()->getCanonicalType();

    if (keys && !keys->isInterestingType(arg))
      continue;

    // If the argument is a type parameter, fulfill conformances for it.
    if (arg->isTypeParameter()) {
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
                                         const InterestingKeysCallback *keys) {
  // Our sources are the protocol conformances that are recorded in
  // the generic metadata.
  auto storedConformances = param->getConformsTo();
  if (storedConformances.empty()) return false;

  bool hadFulfillment = false;

  // If we don't have an interesting-keys callback, add fulfillments
  // for all of the stored conformances.
  if (!keys) {
    // Check each of the stored conformances.
    for (size_t confIndex : indices(storedConformances)) {
      MetadataPath confPath = path;
      confPath.addNominalTypeArgumentConformanceComponent(argIndex,
                                                          confIndex);
      hadFulfillment |=
        addFulfillment({arg, storedConformances[confIndex]},
                       source, std::move(confPath));
    }

    return hadFulfillment;
  }

  // Otherwise, our targets are the interesting conformances for the type
  // argument.
  auto requiredConformances = keys->getInterestingConformances(arg);
  if (requiredConformances.empty()) return false;

  for (auto target : requiredConformances) {
    // Ignore trivial protocols.
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(target))
      continue;

    // Check each of the stored conformances.
    for (size_t confIndex : indices(storedConformances)) {
      // TODO: maybe this should consider indirect conformance.
      // But that should be part of the metadata path.
      if (target == storedConformances[confIndex]) {
        MetadataPath confPath = path;
        confPath.addNominalTypeArgumentConformanceComponent(argIndex,
                                                            confIndex);
        hadFulfillment |=
          addFulfillment({arg, target}, source, std::move(confPath));
      }
    }
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
