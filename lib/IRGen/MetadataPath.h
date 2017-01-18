//===--- MetadataPath.h - Path for lazily finding type metadata -*- C++ -*-===//
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
//  This file defines the MetadataPath type, which efficiently records the
//  path to a metadata object.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_METADATAPATH_H
#define SWIFT_IRGEN_METADATAPATH_H

#include "swift/Basic/EncodedSequence.h"
#include "swift/Reflection/MetadataSource.h"

namespace llvm {
  class Value;
}

namespace swift {
  class ProtocolDecl;
  class CanType;
  class Decl;

namespace irgen {
  class IRGenFunction;
  class LocalTypeDataKey;

/// A path from one source metadata --- either Swift type metadata or a Swift
/// protocol conformance --- to another.
class MetadataPath {
  class Component {
  public:
    enum class Kind {
      // Some components carry indices.
      // P means the primary index.

      /// Base protocol P of a protocol.
      InheritedProtocol,

      /// Witness table at requirement index P of a generic nominal type.
      NominalTypeArgumentConformance,

      /// Type metadata at requirement index P of a generic nominal type.
      NominalTypeArgument,
      LastWithPrimaryIndex = NominalTypeArgument,

      // Everything past this point has no index.

      /// The parent metadata of a nominal type.
      NominalParent,

      /// An impossible path.
      Impossible,
    };

  private:
    unsigned Primary;
    enum {
      KindMask = 0xF,
      IndexShift = 4,
    };
    static bool hasPrimaryIndex(Kind kind) {
      return kind <= Kind::LastWithPrimaryIndex;
    }

    explicit Component(unsigned primary)
        : Primary(primary) {}
  public:
    explicit Component(Kind kind) 
        : Primary(unsigned(kind)) {
      assert(!hasPrimaryIndex(kind));
    }
    explicit Component(Kind kind, unsigned primaryIndex)
        : Primary(unsigned(kind) | (primaryIndex << IndexShift)) {
      assert(hasPrimaryIndex(kind));
    }

    Kind getKind() const { return Kind(Primary & KindMask); }
    unsigned getPrimaryIndex() const {
      assert(hasPrimaryIndex(getKind()));
      return (Primary >> IndexShift);
    }

    /// Return an abstract measurement of the cost of this component.
    unsigned cost() const {
      // Right now, all components cost the same: they take one load.
      // In the future, maybe some components will be cheaper (no loads,
      // like loading from a superclass's metadata) or more expensive
      // (multiple loads, or even a call).
      return 1;
    }

    static Component decode(const EncodedSequenceBase::Chunk *&ptr) {
      unsigned primary = EncodedSequenceBase::decodeIndex(ptr);
      return Component(primary);
    }

    void encode(EncodedSequenceBase::Chunk *&ptr) const {
      EncodedSequenceBase::encodeIndex(Primary, ptr);
    }

    unsigned getEncodedSize() const {
      auto size = EncodedSequenceBase::getEncodedIndexSize(Primary);
      return size;
    }
  };
  EncodedSequence<Component> Path;

public:
  MetadataPath() {}

  using iterator = EncodedSequence<Component>::iterator;

  template <class ValueType>
  using Map = EncodedSequence<Component>::Map<ValueType>;

  /// Add a step to this path which will cause a dynamic assertion if
  /// it's followed.
  void addImpossibleComponent() {
    Path.push_back(Component(Component::Kind::Impossible));
  }

  /// Add a step to this path which gets the parent metadata.
  void addNominalParentComponent() {
    Path.push_back(Component(Component::Kind::NominalParent));
  }

  /// Add a step to this path which gets the type metadata stored at
  /// requirement index n in a generic type metadata.
  void addNominalTypeArgumentComponent(unsigned index) {
    Path.push_back(Component(Component::Kind::NominalTypeArgument, index));
  }

  /// Add a step to this path which gets the protocol witness table
  /// stored at requirement index n in a generic type metadata.
  void addNominalTypeArgumentConformanceComponent(unsigned index) {
    Path.push_back(Component(Component::Kind::NominalTypeArgumentConformance,
                             index));
  }

  /// Add a step to this path which gets the kth inherited protocol from a
  /// witness table.
  ///
  /// k is computed including protocols which do not have witness tables.
  void addInheritedProtocolComponent(unsigned index) {
    Path.push_back(Component(Component::Kind::InheritedProtocol, index));
  }

  /// Return an abstract measurement of the cost of this path.
  unsigned cost() const {
    unsigned cost = 0;
    for (const Component &component : Path)
      cost += component.cost();
    return cost;
  }

  /// Given a pointer to type metadata, follow a path from it.
  llvm::Value *followFromTypeMetadata(IRGenFunction &IGF,
                                      CanType sourceType,
                                      llvm::Value *source,
                                      Map<llvm::Value*> *cache) const;

  /// Given a pointer to a protocol witness table, follow a path from it.
  llvm::Value *followFromWitnessTable(IRGenFunction &IGF,
                                      CanType conformingType,
                                      ProtocolConformanceRef conformance,
                                      llvm::Value *source,
                                      Map<llvm::Value*> *cache) const;

  template <typename Allocator>
  const reflection::MetadataSource *
  getMetadataSource(Allocator &A,
                    const reflection::MetadataSource *Root) const {
    if (Root == nullptr)
      return nullptr;

    for (auto C : Path) {
      switch (C.getKind()) {
      case Component::Kind::NominalParent:
        Root = A.createParent(Root);
        continue;
      case Component::Kind::NominalTypeArgument:
        Root = A.createGenericArgument(C.getPrimaryIndex(), Root);
        continue;
      default:
        return nullptr;
      }
    }
    return Root;
  }

  void dump() const;
  void print(llvm::raw_ostream &out) const;
  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                       const MetadataPath &path) {
    path.print(out);
    return out;
  }

private:
  static llvm::Value *follow(IRGenFunction &IGF,
                             LocalTypeDataKey key,
                             llvm::Value *source,
                             MetadataPath::iterator begin,
                             MetadataPath::iterator end,
                             Map<llvm::Value*> *cache);

  /// Follow a single component of a metadata path.
  static llvm::Value *followComponent(IRGenFunction &IGF,
                                      LocalTypeDataKey &key,
                                      llvm::Value *source,
                                      Component component);
};

} // end namespace irgen
} // end namespace swift

#endif
