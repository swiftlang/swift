//===--- LocalTypeData.cpp - Local type data search -----------------------===//
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
//  This file implements routines for finding and caching local type data
//  for a search.
//
//===----------------------------------------------------------------------===//

#include "LocalTypeData.h"
#include "Fulfillment.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "swift/SIL/SILModule.h"

using namespace swift;
using namespace irgen;

LocalTypeDataCache &IRGenFunction::getOrCreateLocalTypeData() {
  // Lazily allocate it.
  if (LocalTypeData) return *LocalTypeData;
  LocalTypeData = new LocalTypeDataCache();
  return *LocalTypeData;
}

void IRGenFunction::destroyLocalTypeData() {
  delete LocalTypeData;
}

unsigned LocalTypeDataCache::CacheEntry::cost() const {
  switch (getKind()) {
  case Kind::Concrete:
    return static_cast<const ConcreteCacheEntry*>(this)->cost();
  case Kind::Abstract:
    return static_cast<const AbstractCacheEntry*>(this)->cost();
  }
  llvm_unreachable("bad cache entry kind");
}

void LocalTypeDataCache::CacheEntry::erase() const {
  switch (getKind()) {
  case Kind::Concrete:
    delete static_cast<const ConcreteCacheEntry*>(this);
    return;
  case Kind::Abstract:
    delete static_cast<const AbstractCacheEntry*>(this);
    return;
  }
  llvm_unreachable("bad cache entry kind");
}

llvm::Value *IRGenFunction::getLocalTypeData(CanType type,
                                             LocalTypeDataKind kind) {
  assert(LocalTypeData);
  return LocalTypeData->get(*this, LocalTypeDataCache::getKey(type, kind));
}

llvm::Value *IRGenFunction::tryGetLocalTypeData(CanType type,
                                                LocalTypeDataKind kind) {
  if (!LocalTypeData) return nullptr;
  return LocalTypeData->tryGet(*this, LocalTypeDataCache::getKey(type, kind));
}

llvm::Value *LocalTypeDataCache::tryGet(IRGenFunction &IGF, Key key) {
  auto it = Map.find(key);
  if (it == Map.end()) return nullptr;
  auto &chain = it->second;

  CacheEntry *best = nullptr, *bestPrev = nullptr;
  Optional<unsigned> bestCost;

  CacheEntry *next = chain.Root, *nextPrev = nullptr;
  do {
    assert(next);
    CacheEntry *cur = next, *curPrev = nextPrev;
    nextPrev = cur;
    next = cur->getNext();

    // Ignore unacceptable entries.
    if (!IGF.isActiveDominancePointDominatedBy(cur->DefinitionPoint))
      continue;

    // If there's a collision, compare by cost, ignoring higher-cost entries.
    if (best) {
      // Compute the cost of the best entry if we haven't done so already.
      // If that's zero, go ahead and short-circuit out.
      if (!bestCost) {
        bestCost = best->cost();
        if (*bestCost == 0) break;
      }

      auto curCost = cur->cost();
      if (curCost >= *bestCost) continue;

      // Replace the best cost and fall through.
      bestCost = curCost;
    }
    best = cur;
    bestPrev = curPrev;
  } while (next);

  // If we didn't find anything, we're done.
  if (!best) return nullptr;

  // Okay, we've found the best entry available.
  switch (best->getKind()) {

  // For concrete caches, this is easy.
  case CacheEntry::Kind::Concrete:
    return static_cast<ConcreteCacheEntry*>(best)->Value;

  // For abstract caches, we need to follow a path.
  case CacheEntry::Kind::Abstract: {
    auto entry = static_cast<AbstractCacheEntry*>(best);

    // Follow the path.
    auto &source = AbstractSources[entry->SourceIndex];
    auto result = entry->follow(IGF, source);

    // If we can't cache at the current insertion point, we're done.
    if (!IGF.hasCacheableDominancePoint())
      return result;

    // Make a new concrete entry at the active definition point.
    auto newEntry =
      new ConcreteCacheEntry(IGF.getActiveDominancePoint(), result);

    // If the active definition point is the same as the old entry's
    // definition point, delete the old entry.
    if (best->DefinitionPoint == IGF.getActiveDominancePoint()) {
      chain.eraseEntry(bestPrev, best);
    }

    // Add the new entry to the front of the chain.
    chain.push_front(newEntry);

    return result;
  }

  }
  llvm_unreachable("bad cache entry kind");
}

llvm::Value *
LocalTypeDataCache::AbstractCacheEntry::follow(IRGenFunction &IGF,
                                               AbstractSource &source) const {
  switch (source.getKind()) {
  case AbstractSource::Kind::TypeMetadata:
    return Path.followFromTypeMetadata(IGF, source.getType(),
                                       source.getValue(), &source.getCache());  

  case AbstractSource::Kind::WitnessTable:
    return Path.followFromWitnessTable(IGF, source.getProtocol(),
                                       source.getValue(), &source.getCache());  
  }
  llvm_unreachable("bad source kind");
}

void IRGenFunction::setScopedLocalTypeData(CanType type, LocalTypeDataKind kind,
                                           llvm::Value *data) {
  if (!hasCacheableDominancePoint())
    return;

  getOrCreateLocalTypeData().addConcrete(getActiveDominancePoint(),
                                         LocalTypeDataCache::getKey(type, kind),
                                         data);
}

void IRGenFunction::setUnscopedLocalTypeData(CanType type,
                                             LocalTypeDataKind kind,
                                             llvm::Value *data) {
  getOrCreateLocalTypeData()
    .addConcrete(DominancePoint::universal(),
                 LocalTypeDataCache::getKey(type, kind), data);
}

void LocalTypeDataCache::addAbstractForTypeMetadata(IRGenFunction &IGF,
                                                    CanType type,
                                                    llvm::Value *metadata) {
  // If we shouldn't cache at the current dominance point, short-circuit.
  if (!IGF.hasCacheableDominancePoint())
    return;

  // Look for anything at all that's fulfilled by this.  If we don't find
  // anything, stop.
  FulfillmentMap fulfillments;
  if (!fulfillments.searchTypeMetadata(*IGF.IGM.SILMod->getSwiftModule(),
                                       type, FulfillmentMap::IsExact,
                                       /*source*/ 0, MetadataPath(),
                                       FulfillmentMap::Everything())) {
    return;
  }

  addAbstractForFullfillments(IGF, std::move(fulfillments),
                              [&]() -> AbstractSource {
    return AbstractSource(AbstractSource::Kind::TypeMetadata, type, metadata);
  });
}

void LocalTypeDataCache::
addAbstractForFullfillments(IRGenFunction &IGF, FulfillmentMap &&fulfillments,
                            llvm::function_ref<AbstractSource()> createSource) {
  // Add the source lazily.
  Optional<unsigned> sourceIndex;
  auto getSourceIndex = [&]() -> unsigned {
    if (!sourceIndex) {
      AbstractSources.emplace_back(createSource());
      sourceIndex = AbstractSources.size() - 1;
    }
    return *sourceIndex;
  };

  for (auto &fulfillment : fulfillments) {
    CanType type = CanType(fulfillment.first.first);
    LocalTypeDataKind localDataKind;

    // For now, ignore witness-table fulfillments when they're not for
    // archetypes.
    if (ProtocolDecl *protocol = fulfillment.first.second) {
      if (auto archetype = dyn_cast<ArchetypeType>(type)) {
        auto conformsTo = archetype->getConformsTo();
        auto it = std::find(conformsTo.begin(), conformsTo.end(), protocol);
        if (it == conformsTo.end()) continue;
        localDataKind =LocalTypeDataKind::forArchetypeProtocolWitnessTable(*it);
      } else {
        continue;
      }
    } else {
      localDataKind = LocalTypeDataKind::forMetatype();
    }

    // Find the chain for the key.
    auto key = getKey(type, localDataKind);
    auto &chain = Map[key];

    // Check whether there's already an entr that's at least as good as the
    // fulfillment.
    Optional<unsigned> fulfillmentCost;
    auto getFulfillmentCost = [&]() -> unsigned {
      if (!fulfillmentCost)
        fulfillmentCost = fulfillment.second.Path.cost();
      return *fulfillmentCost;
    };

    bool foundBetter = false;
    for (CacheEntry *cur = chain.Root, *last = nullptr; cur;
         last = cur, cur = cur->getNext()) {
      // Ensure the entry is acceptable.
      if (!IGF.isActiveDominancePointDominatedBy(cur->DefinitionPoint))
        continue;

      // Ensure that the entry isn't better than the fulfillment.
      auto curCost = cur->cost();
      if (curCost == 0 || curCost <= getFulfillmentCost()) {
        foundBetter = true;
        break;
      }

      // If the entry is defined at the current point, (1) we know there
      // won't be a better entry and (2) we should remove it.
      if (cur->DefinitionPoint == IGF.getActiveDominancePoint()) {
        // Splice it out of the chain.
        chain.eraseEntry(last, cur);
        break;
      }
    }
    if (foundBetter) continue;

    // Okay, make a new entry.
    auto newEntry = new AbstractCacheEntry(IGF.getActiveDominancePoint(),
                                           getSourceIndex(),
                                           std::move(fulfillment.second.Path));

    // Add it to the front of the chain.
    chain.push_front(newEntry);
  }
}
