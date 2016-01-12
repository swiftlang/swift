//===--- LocalTypeData.h - Dominance-scoped type data -----------*- C++ -*-===//
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
#include "swift/AST/Type.h"
#include "llvm/ADT/STLExtras.h"
#include <utility>

namespace swift {
  class TypeBase;

namespace irgen {
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
  using Key = LocalTypeDataKey;

  static Key getKey(CanType type, LocalTypeDataKind index) {
    return { type, index };
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
    unsigned cost() const;

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
    llvm::Value *Value;

    ConcreteCacheEntry(DominancePoint point, bool isConditional,
                       llvm::Value *value)
      : CacheEntry(Kind::Concrete, point, isConditional), Value(value) {}

    unsigned cost() const { return 0; }
  };

  /// A source of concrete data from which abstract cache entries can be
  /// derived.
  class AbstractSource {
  public:
    enum class Kind {
      /// Type metadata.  The payload is a CanType.
      TypeMetadata,

      /// A protocol witness table.  The payload is a ProtocolDecl*.
      WitnessTable,
    };

  private:
    uintptr_t Payload;
    MetadataPath::Map<llvm::Value*> Cache;
    llvm::Value *Value;

    enum : uintptr_t { KindMask = 0x3 };

    explicit AbstractSource(Kind kind, void *ptr, llvm::Value *value)
      : Payload(uintptr_t(ptr) | unsigned(kind)), Value(value) {}

  public:
    explicit AbstractSource(Kind kind, CanType type, llvm::Value *metadata)
      : AbstractSource(kind, type.getPointer(), metadata) {}
    explicit AbstractSource(Kind kind, ProtocolDecl *protocol,
                            llvm::Value *witnessTable)
      : AbstractSource(kind, (void*) protocol, witnessTable) {}

    Kind getKind() const {
      return Kind(Payload & KindMask);
    }

    CanType getType() const {
      assert(getKind() == Kind::TypeMetadata);
      return CanType(reinterpret_cast<TypeBase*>(Payload & ~KindMask));
    }
    ProtocolDecl *getProtocol() const {
      assert(getKind() == Kind::WitnessTable);
      return reinterpret_cast<ProtocolDecl*>(Payload & ~KindMask);
    }

    llvm::Value *getValue() const {
      return Value;
    }

    MetadataPath::Map<llvm::Value*> &getCache() {
      return Cache;
    }
  };

  /// An abstract entry in the cache, which requires some amount of
  /// non-trivial evaluation to derive the desired value.
  struct AbstractCacheEntry : CacheEntry {
    unsigned SourceIndex;
    MetadataPath Path;

    AbstractCacheEntry(DominancePoint point, bool isConditional,
                       unsigned sourceIndex, MetadataPath &&path)
      : CacheEntry(Kind::Abstract, point, isConditional),
        SourceIndex(sourceIndex), Path(std::move(path)) {}

    llvm::Value *follow(IRGenFunction &IGF, AbstractSource &source) const;

    unsigned cost() const { return Path.cost(); }
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

  llvm::DenseMap<Key, CacheEntryChain> Map;

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
  llvm::Value *tryGet(IRGenFunction &IGF, Key key);

  /// Load the value from cache, asserting its presence.
  llvm::Value *get(IRGenFunction &IGF, Key key) {
    auto result = tryGet(IGF, key);
    assert(result && "get() on unmapped entry?");
    return result;
  }

  /// Add a new concrete entry to the cache at the given definition point.
  void addConcrete(DominancePoint point, bool isConditional,
                   Key key, llvm::Value *value) {
    auto newEntry = new ConcreteCacheEntry(point, isConditional, value);
    Map[key].push_front(newEntry);
  }

  /// Add entries based on what can be fulfilled from the given type metadata.
  void addAbstractForTypeMetadata(IRGenFunction &IGF, CanType type,
                                  IsExact_t isExact, llvm::Value *metadata);

  void eraseConditional(ArrayRef<LocalTypeDataKey> keys);
};

}
}

#endif
