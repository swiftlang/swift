//===--- LocalTypeData.cpp - Local type data search -----------------------===//
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
//  This file implements routines for finding and caching local type data
//  for a search.
//
//===----------------------------------------------------------------------===//

#include "LocalTypeData.h"
#include "Fulfillment.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/SILModule.h"

using namespace swift;
using namespace irgen;

LocalTypeDataKey LocalTypeDataKey::getCachingKey() const {
  return { Type, Kind.getCachingKind() };
}

LocalTypeDataKind LocalTypeDataKind::getCachingKind() const {
  // Most local type data kinds are already canonical.
  if (!isConcreteProtocolConformance()) return *this;

  // Map protocol conformances to their root normal conformance.
  auto conformance = getConcreteProtocolConformance();
  return forConcreteProtocolWitnessTable(
                                     conformance->getRootNormalConformance());
}

LocalTypeDataCache &IRGenFunction::getOrCreateLocalTypeData() {
  // Lazily allocate it.
  if (LocalTypeData) return *LocalTypeData;
  LocalTypeData = new LocalTypeDataCache();
  return *LocalTypeData;
}

void IRGenFunction::destroyLocalTypeData() {
  delete LocalTypeData;
}

OperationCost LocalTypeDataCache::CacheEntry::cost() const {
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

llvm::Value *IRGenFunction::tryGetConcreteLocalTypeData(LocalTypeDataKey key) {
  if (!LocalTypeData) return nullptr;
  return LocalTypeData->tryGet(*this, key, /*allow abstract*/ false);
}

llvm::Value *IRGenFunction::tryGetLocalTypeData(LocalTypeDataKey key) {
  if (!LocalTypeData) return nullptr;
  return LocalTypeData->tryGet(*this, key);
}

llvm::Value *LocalTypeDataCache::tryGet(IRGenFunction &IGF, Key key,
                                        bool allowAbstract) {
  auto it = Map.find(key);
  if (it == Map.end()) return nullptr;
  auto &chain = it->second;

  CacheEntry *best = nullptr;
  Optional<OperationCost> bestCost;

  CacheEntry *next = chain.Root;
  while (next) {
    CacheEntry *cur = next;
    next = cur->getNext();

    // Ignore abstract entries if so requested.
    if (!allowAbstract && cur->getKind() != CacheEntry::Kind::Concrete)
      continue;

    // Ignore unacceptable entries.
    if (!IGF.isActiveDominancePointDominatedBy(cur->DefinitionPoint))
      continue;

    // If there's a collision, compare by cost, ignoring higher-cost entries.
    if (best) {
      // Compute the cost of the best entry if we haven't done so already.
      // If that's zero, go ahead and short-circuit out.
      if (!bestCost) {
        bestCost = best->cost();
        if (*bestCost == OperationCost::Free) break;
      }

      auto curCost = cur->cost();
      if (curCost >= *bestCost) continue;

      // Replace the best cost and fall through.
      bestCost = curCost;
    }
    best = cur;
  }

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

    // Following the path automatically caches at every point along it,
    // including the end.
    assert(chain.Root->DefinitionPoint == IGF.getActiveDominancePoint());
    assert(chain.Root->getKind() == CacheEntry::Kind::Concrete);

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
                                       source.getValue(), nullptr);

  case AbstractSource::Kind::ProtocolWitnessTable:
    return Path.followFromWitnessTable(IGF, source.getType(),
                                       source.getProtocolConformance(),
                                       source.getValue(), nullptr);
  }
  llvm_unreachable("bad source kind");
}

static void maybeEmitDebugInfoForLocalTypeData(IRGenFunction &IGF,
                                               LocalTypeDataKey key,
                                               llvm::Value *data) {
  // Only if debug info is enabled.
  if (!IGF.IGM.DebugInfo) return;

  // Only for type metadata.
  if (key.Kind != LocalTypeDataKind::forTypeMetadata()) return;

  // Only for archetypes, and not for opened archetypes.
  auto type = dyn_cast<ArchetypeType>(key.Type);
  if (!type) return;
  if (type->getOpenedExistentialType()) return;

  // At -O0, create an alloca to keep the type alive.
  auto name = type->getFullName();
  if (!IGF.IGM.IRGen.Opts.Optimize) {
    auto temp = IGF.createAlloca(data->getType(), IGF.IGM.getPointerAlignment(),
                                 name);
    IGF.Builder.CreateStore(data, temp);
    data = temp.getAddress();
  }

  // Emit debug info for the metadata.
  IGF.IGM.DebugInfo->emitTypeMetadata(IGF, data, name);
}

void IRGenFunction::setScopedLocalTypeData(LocalTypeDataKey key,
                                           llvm::Value *data) {
  maybeEmitDebugInfoForLocalTypeData(*this, key, data);

  // Register with the active ConditionalDominanceScope if necessary.
  bool isConditional = isConditionalDominancePoint();
  if (isConditional) {
    registerConditionalLocalTypeDataKey(key);
  }

  getOrCreateLocalTypeData().addConcrete(getActiveDominancePoint(),
                                         isConditional, key, data);
}

void IRGenFunction::setUnscopedLocalTypeData(LocalTypeDataKey key,
                                             llvm::Value *data) {
  maybeEmitDebugInfoForLocalTypeData(*this, key, data);

  // This is supportable, but it would require ensuring that we add the
  // entry after any conditional entries; otherwise the stack discipline
  // will get messed up.
  assert(!isConditionalDominancePoint() &&
         "adding unscoped local type data while in conditional scope");
  getOrCreateLocalTypeData().addConcrete(DominancePoint::universal(),
                                         /*conditional*/ false, key, data);
}

void IRGenFunction::bindLocalTypeDataFromTypeMetadata(CanType type,
                                                      IsExact_t isExact,
                                                      llvm::Value *metadata) {
  // Remember that we have this type metadata concretely.
  if (isExact) {
    if (!metadata->hasName()) setTypeMetadataName(IGM, metadata, type);
    setScopedLocalTypeData(type, LocalTypeDataKind::forTypeMetadata(), metadata);
  }

  // Don't bother adding abstract fulfillments at a conditional dominance
  // point; we're too likely to throw them all away.
  if (isConditionalDominancePoint())
    return;

  getOrCreateLocalTypeData()
    .addAbstractForTypeMetadata(*this, type, isExact, metadata);
}

void LocalTypeDataCache::addAbstractForTypeMetadata(IRGenFunction &IGF,
                                                    CanType type,
                                                    IsExact_t isExact,
                                                    llvm::Value *metadata) {
  // Look for anything at all that's fulfilled by this.  If we don't find
  // anything, stop.
  FulfillmentMap fulfillments;
  if (!fulfillments.searchTypeMetadata(IGF.IGM, type, isExact,
                                       /*source*/ 0, MetadataPath(),
                                       FulfillmentMap::Everything())) {
    return;
  }

  addAbstractForFulfillments(IGF, std::move(fulfillments),
                             [&]() -> AbstractSource {
    return AbstractSource(type, metadata);
  });
}

void LocalTypeDataCache::
addAbstractForFulfillments(IRGenFunction &IGF, FulfillmentMap &&fulfillments,
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
        localDataKind = LocalTypeDataKind::forAbstractProtocolWitnessTable(*it);
      } else {
        continue;
      }

    } else {
      // Ignore type metadata fulfillments for non-dependent types that
      // we can produce very cheaply.  We don't want to end up emitting
      // the type metadata for Int by chasing through N layers of metadata
      // just because that path happens to be in the cache.
      if (!type->hasArchetype() &&
          isTypeMetadataAccessTrivial(IGF.IGM, type)) {
        continue;
      }

      localDataKind = LocalTypeDataKind::forTypeMetadata();
    }

    // Find the chain for the key.
    auto key = getKey(type, localDataKind);
    auto &chain = Map[key];

    // Check whether there's already an entry that's at least as good as the
    // fulfillment.
    Optional<OperationCost> fulfillmentCost;
    auto getFulfillmentCost = [&]() -> OperationCost {
      if (!fulfillmentCost)
        fulfillmentCost = fulfillment.second.Path.cost();
      return *fulfillmentCost;
    };

    bool isConditional = IGF.isConditionalDominancePoint();

    bool foundBetter = false;
    for (CacheEntry *cur = chain.Root, *last = nullptr; cur;
         last = cur, cur = cur->getNext()) {
      // Ensure the entry is acceptable.
      if (!IGF.isActiveDominancePointDominatedBy(cur->DefinitionPoint))
        continue;

      // Ensure that the entry isn't better than the fulfillment.
      auto curCost = cur->cost();
      if (curCost == OperationCost::Free || curCost <= getFulfillmentCost()) {
        foundBetter = true;
        break;
      }

      // If the entry is defined at the current point, (1) we know there
      // won't be a better entry and (2) we should remove it.
      if (cur->DefinitionPoint == IGF.getActiveDominancePoint() &&
          !isConditional) {
        // Splice it out of the chain.
        assert(!cur->isConditional());
        chain.eraseEntry(last, cur);
        break;
      }
    }
    if (foundBetter) continue;

    // Okay, make a new entry.

    // Register with the conditional dominance scope if necessary.
    if (isConditional) {
      IGF.registerConditionalLocalTypeDataKey(key);
    }

    // Allocate the new entry.
    auto newEntry = new AbstractCacheEntry(IGF.getActiveDominancePoint(),
                                           isConditional,
                                           getSourceIndex(),
                                           std::move(fulfillment.second.Path));

    // Add it to the front of the chain.
    chain.push_front(newEntry);
  }
}
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
LLVM_DUMP_METHOD void LocalTypeDataCache::dump() const {
  auto &out = llvm::errs();

  if (Map.empty()) {
    out << "(empty)\n";
    return;
  }

  for (auto &mapEntry : Map) {
    mapEntry.first.print(out);
    out << " => [";

    if (mapEntry.second.Root) out << "\n";
    for (auto cur = mapEntry.second.Root; cur; cur = cur->getNext()) {
      out << "  (";
      if (cur->DefinitionPoint.isUniversal()) out << "universal";
      else out << cur->DefinitionPoint.as<void>();
      out << ") ";

      if (cur->isConditional()) out << "conditional ";

      switch (cur->getKind()) {
      case CacheEntry::Kind::Concrete: {
        auto entry = static_cast<const ConcreteCacheEntry*>(cur);
        out << "concrete: " << entry->Value << "\n  ";
        if (!isa<llvm::Instruction>(entry->Value)) out << "  ";
        entry->Value->dump();
        break;
      }

      case CacheEntry::Kind::Abstract: {
        auto entry = static_cast<const AbstractCacheEntry*>(cur);
        out << "abstract: source=" << entry->SourceIndex << "\n";
        break;
      }
      }
    }
    out << "]\n";
  }
}
#endif

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
LLVM_DUMP_METHOD void LocalTypeDataKey::dump() const {
  print(llvm::errs());
}
#endif

void LocalTypeDataKey::print(llvm::raw_ostream &out) const {
  out << "(" << Type.getPointer()
      << " (" << Type << "), ";
  Kind.print(out);
  out << ")";
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
LLVM_DUMP_METHOD void LocalTypeDataKind::dump() const {
  print(llvm::errs());
}
#endif

void LocalTypeDataKind::print(llvm::raw_ostream &out) const {
  if (isConcreteProtocolConformance()) {
    out << "ConcreteConformance(";
    getConcreteProtocolConformance()->printName(out);
    out << ")";
  } else if (isAbstractProtocolConformance()) {
    out << "AbstractConformance("
        << getAbstractProtocolConformance()->getName()
        << ")";
  } else if (Value == TypeMetadata) {
    out << "TypeMetadata";
  } else if (Value == ValueWitnessTable) {
    out << "ValueWitnessTable";
  } else {
    assert(isSingletonKind());
    ValueWitness witness = ValueWitness(Value - ValueWitnessBase);
    out << getValueWitnessName(witness);
  }
}

IRGenFunction::ConditionalDominanceScope::~ConditionalDominanceScope() {
  IGF.ConditionalDominance = OldScope;

  // Remove any conditional entries from the chains that were added in this
  // scope.
  for (auto &key : RegisteredKeys) {
    IGF.LocalTypeData->eraseConditional(key);
  }
}

void LocalTypeDataCache::eraseConditional(ArrayRef<LocalTypeDataKey> keys) {
  for (auto &key : keys) {
    auto &chain = Map[key];

    // Our ability to simply delete the front of the chain relies on an
    // assumption that (1) conditional additions always go to the front of
    // the chain and (2) we never add something unconditionally while in
    // an unconditional scope.
    assert(chain.Root);
    assert(chain.Root->isConditional());
    chain.eraseEntry(nullptr, chain.Root);
  }
}
