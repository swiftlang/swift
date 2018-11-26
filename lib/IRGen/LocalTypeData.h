//===--- LocalTypeData.h - Dominance-scoped type data -----------*- C++ -*-===//
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
//  This file defines types relating to the local caching of type data,
//  such as type metadata, value witness tables, and protocol witness tables.
//
//  Type data may be cached concretely, meaning that it was already fully
//  computed, or abstractly, meaning that we remember how to recreate it
//  but haven't actually done so yet.
//
//  Type data may be cached at different points within a function.
//  Some of these points may not dominate all possible use sites.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LOCALTYPEDATA_H
#define SWIFT_IRGEN_LOCALTYPEDATA_H

#include "LocalTypeDataKind.h"
#include "DominancePoint.h"
#include "MetadataPath.h"
#include "MetadataRequest.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/STLExtras.h"
#include <utility>

namespace swift {
  class TypeBase;

namespace irgen {
  class DynamicMetadataRequest;
  class MetadataResponse;
  class FulfillmentMap;
  enum IsExact_t : bool;

/// A cache of local type data.
///
/// Basic design considerations:
///
///   - We want to be able to efficiently look up a key and find something.
///     Generally this will find something from the entry block.  We shouldn't
///     have to scan all the dominating points first.
///
///   - We don't expect to have multiple definitions for a key very often.
///     Therefore, given a collision, it should be okay to scan a list and
///     ask whether each is acceptable.
class LocalTypeDataCache {
public:
  static LocalTypeDataKey getKey(CanType type, LocalTypeDataKind index) {
    return LocalTypeDataKey{ type, index };
  }

private:
  struct CacheEntry {
    enum class Kind {
      Concrete, Abstract
    };

    DominancePoint DefinitionPoint;

  private:
    enum { KindMask = 0x1, ConditionalMask = 0x2 };
    llvm::PointerIntPair<CacheEntry*, 2, unsigned> NextAndFlags;

  public:
    Kind getKind() const {
      return Kind(NextAndFlags.getInt() & KindMask);
    }
    CacheEntry *getNext() const { return NextAndFlags.getPointer(); }
    void setNext(CacheEntry *next) { NextAndFlags.setPointer(next); }

    /// Return the abstract cost of evaluating this cache entry.
    OperationCost cost() const;

    /// Return the abstract cost of evaluating this cache entry.
    OperationCost costForRequest(LocalTypeDataKey key,
                                 DynamicMetadataRequest request) const;

    /// Return whether following the metadata path immediately produces
    /// a value that's statically known to satisfy the given request, or
    /// whether a separate dynamic check is required.
    bool immediatelySatisfies(LocalTypeDataKey key,
                              DynamicMetadataRequest request) const;

    /// Destruct and deallocate this cache entry.
    void erase() const;

    bool isConditional() const {
      return NextAndFlags.getInt() & ConditionalMask;
    }

  protected:
    CacheEntry(Kind kind, DominancePoint point, bool isConditional)
        : DefinitionPoint(point),
          NextAndFlags(nullptr,
                       unsigned(kind) | (isConditional ? ConditionalMask : 0)) {
    }
    ~CacheEntry() = default;
  };

  /// A concrete entry in the cache, which directly stores the desired value.
  struct ConcreteCacheEntry : CacheEntry {
    MetadataResponse Value;

    ConcreteCacheEntry(DominancePoint point, bool isConditional,
                       MetadataResponse value)
      : CacheEntry(Kind::Concrete, point, isConditional), Value(value) {}

    OperationCost cost() const { return OperationCost::Free; }
    OperationCost costForRequest(LocalTypeDataKey key,
                                 DynamicMetadataRequest request) const;

    bool immediatelySatisfies(LocalTypeDataKey key,
                              DynamicMetadataRequest request) const;

    static bool classof(const CacheEntry *entry) {
      return entry->getKind() == Kind::Concrete;
    }
  };

  /// A source of concrete data from which abstract cache entries can be
  /// derived.
  class AbstractSource {
  public:
    enum class Kind {
      TypeMetadata,
      ProtocolWitnessTable,
    };
  private:
    CanType Type;
    void *Conformance;
    MetadataResponse Value;

  public:
    explicit AbstractSource(CanType type, ProtocolConformanceRef conformance,
                            llvm::Value *value)
      : Type(type), Conformance(conformance.getOpaqueValue()),
        Value(MetadataResponse::forComplete(value)) {
      assert(Conformance != nullptr);
    }
    explicit AbstractSource(CanType type, MetadataResponse value)
      : Type(type), Conformance(nullptr), Value(value) {}

    Kind getKind() const {
      return (Conformance ? Kind::ProtocolWitnessTable : Kind::TypeMetadata);
    }

    CanType getType() const {
      return Type;
    }
    ProtocolConformanceRef getProtocolConformance() const {
      assert(Conformance && "not a protocol conformance");
      return ProtocolConformanceRef::getFromOpaqueValue(Conformance);
    }
    MetadataResponse getValue() const {
      return Value;
    }
  };

  /// An abstract entry in the cache, which requires some amount of
  /// non-trivial evaluation to derive the desired value.
  struct AbstractCacheEntry : CacheEntry {
    unsigned SourceIndex;
    unsigned State;
    MetadataPath Path;

    AbstractCacheEntry(DominancePoint point, bool isConditional,
                       unsigned sourceIndex, MetadataPath &&path,
                       MetadataState state)
      : CacheEntry(Kind::Abstract, point, isConditional),
        SourceIndex(sourceIndex), State(unsigned(state)),
        Path(std::move(path)) {}

    MetadataResponse follow(IRGenFunction &IGF, AbstractSource &source,
                            DynamicMetadataRequest request) const;

    /// Return the natural state of the metadata that would be produced
    /// by just following the path.  If the path ends in calling a type
    /// metadata accessor, it's okay to use MetadataState::Complete here
    /// because it's just as cheap to produce that as anything else.
    /// But if the path ends in just loading type metadata from somewhere,
    /// this should be the presumed state of that type metadata so that
    /// the costing accurately includes the cost of dynamically checking
    /// the state of that metadata.
    MetadataState getState() const {
      return MetadataState(State);
    }

    OperationCost cost() const { return Path.cost(); }
    OperationCost costForRequest(LocalTypeDataKey key,
                                 DynamicMetadataRequest request) const;

    bool immediatelySatisfies(LocalTypeDataKey key,
                              DynamicMetadataRequest request) const;

    static bool classof(const CacheEntry *entry) {
      return entry->getKind() == Kind::Abstract;
    }
  };

  /// The linked list of cache entries corresponding to a particular key.
  struct CacheEntryChain {
    CacheEntry *Root;

    explicit CacheEntryChain(CacheEntry *root = nullptr) : Root(root) {}

    CacheEntryChain(const CacheEntryChain &other) = delete;
    CacheEntryChain &operator=(const CacheEntryChain &other) = delete;

    CacheEntryChain(CacheEntryChain &&other) : Root(other.Root) {
      other.Root = nullptr;
    }
    CacheEntryChain &operator=(CacheEntryChain &&other) {
      Root = other.Root;
      other.Root = nullptr;
      return *this;
    }

    void push_front(CacheEntry *entry) {
      entry->setNext(Root);
      Root = entry;
    }

    void eraseEntry(CacheEntry *prev, CacheEntry *entry) {
      if (prev) {
        assert(prev->getNext() == entry);
        prev->setNext(entry->getNext());
      } else {
        assert(Root == entry);
        Root = entry->getNext();
      }
      entry->erase();
    }

    /// Delete the linked list.
    ~CacheEntryChain() {
      auto next = Root;
      while (next) {
        auto cur = next;
        next = cur->getNext();
        cur->erase();
      }
    }
  };

  llvm::DenseMap<LocalTypeDataKey, CacheEntryChain> Map;

  std::vector<AbstractSource> AbstractSources;

  void addAbstractForFulfillments(IRGenFunction &IGF,
                                  FulfillmentMap &&fulfillments,
                            llvm::function_ref<AbstractSource()> createSource);


public:
  LocalTypeDataCache() = default;
  LocalTypeDataCache(const LocalTypeDataCache &other) = delete;
  LocalTypeDataCache &operator=(const LocalTypeDataCache &other) = delete;

  /// Load the value from cache if possible.  This may require emitting
  /// code if the value is cached abstractly.
  MetadataResponse tryGet(IRGenFunction &IGF, LocalTypeDataKey key,
                          bool allowAbstract, DynamicMetadataRequest request);

  /// Add a new concrete entry to the cache at the given definition point.
  void addConcrete(DominancePoint point, bool isConditional,
                   LocalTypeDataKey key, MetadataResponse value) {
    assert((key.Kind.isAnyTypeMetadata() ||
            value.isStaticallyKnownComplete()) &&
           "only type metadata can be added in a non-complete state");
    auto newEntry = new ConcreteCacheEntry(point, isConditional, value);
    Map[key].push_front(newEntry);
  }

  /// Add entries based on what can be fulfilled from the given type metadata.
  void addAbstractForTypeMetadata(IRGenFunction &IGF, CanType type,
                                  IsExact_t isExact, MetadataResponse value);

  void dump() const;

  // Private details for ConditionalDominanceScope.
  void eraseConditional(ArrayRef<LocalTypeDataKey> keys);
};

}
}

#endif
