//===-- LocalTypeDataCache.h - Dominance-scoped type data cache -*- C++ -*-===//
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
//  This file defines the LocalTypeDataCache type, which is used by
//  IRGenFunction to cache the information available for types in a local
//  context.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LOCALTYPEDATACACHE_H
#define SWIFT_IRGEN_LOCALTYPEDATACACHE_H

#include "swift/AST/Type.h"
#include <utility>

namespace swift {
class TypeBase;

namespace irgen {
enum class ValueWitness : unsigned;

/// A nonce value for storing some sort of locally-known information
/// about a type.
class LocalTypeData {
public:
  using RawType = unsigned;
private:
  RawType Value;
  
  explicit LocalTypeData(unsigned Value) : Value(Value) {}
  
  /// Magic values for special kinds of index.
  enum : RawType {
    Metatype = ~0U,
    ValueWitnessTable = ~1U,

    ValueWitnessBase = 0xFFFFFF00U,
  };
  
public:
  LocalTypeData() = default;
  
  // The magic values are all in the "negative" range and so do
  // not collide with reasonable index values.
  
  /// A reference to the type metadata.
  static LocalTypeData forMetatype() { return LocalTypeData(Metatype); }
  /// A reference to the value witness table.
  static LocalTypeData forValueWitnessTable() {
    return LocalTypeData(ValueWitnessTable);
  }

  /// A reference to a specific value witness.
  static LocalTypeData forValueWitness(ValueWitness witness) {
    return LocalTypeData((unsigned)witness + ValueWitnessBase);
  }
  
  /// A reference to a protocol witness table for an archetype.
  static LocalTypeData forArchetypeProtocolWitness(unsigned index) {
    return LocalTypeData(index);
  }
  
  RawType getRawValue() const {
    return Value;
  }
};

class LocalTypeDataCache {
public:
  using Key = std::pair<TypeBase*, LocalTypeData::RawType>;

  static Key getKey(CanType type, LocalTypeData index) {
    return Key(type.getPointer(), index.getRawValue());
  }

  /// An opaque class for storing keys for the dominance callback.  The
  /// key is assumed to be something like a pointer, and a null pointer is
  /// assumed to mean a non-dominating point.
  class DominanceKey {
    void *Value;
  public:
    explicit DominanceKey(void *value = nullptr) : Value(value) {}
    template <class T> T* as() const { return reinterpret_cast<T*>(Value); }
    explicit operator bool() const { return Value != nullptr; }
  };

  /// A RAII object for managing a dominance scope.
  class DominanceScope {
    LocalTypeDataCache &Cache;
    DominanceKey OldDefinitionPoint;
  public:
    explicit DominanceScope(LocalTypeDataCache &cache, DominanceKey newPoint)
        : Cache(cache), OldDefinitionPoint(cache.ActiveDefinitionPoint) {
      cache.ActiveDefinitionPoint = newPoint;
      assert(!newPoint || cache.Callback);
    }

    DominanceScope(const DominanceScope &other) = delete;
    DominanceScope &operator=(const DominanceScope &other) = delete;

    ~DominanceScope() {
      Cache.ActiveDefinitionPoint = OldDefinitionPoint;
    }
  };

  using DominanceCallback = bool(IRGenFunction &IGF, DominanceKey key);

private:
  /// The dominance callback.  This can be set at most once; when it's not
  /// set, the cache must never have a non-null active definition point.
  DominanceCallback *Callback = nullptr;
  DominanceKey ActiveDefinitionPoint;

  struct CacheEntryBase {
    DominanceKey DefinitionPoint;

  };

  struct Source {
    llvm::Value *Value;
    MetadataPath::Map<llvm::Value*> Path;
  };

  std::vector<

public:
};

}
}

#endif
