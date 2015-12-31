//===--- MetadataPath.h - Path for lazily finding type metadata -*- C++ -*-===//
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
//  This file defines the MetadataPath type, which efficiently records the
//  path to a metadata object.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_METADATAPATH_H
#define SWIFT_IRGEN_METADATAPATH_H

#include "swift/Basic/EncodedSequence.h"

namespace llvm {
  class Value;
}

namespace swift {
  class CanType;

namespace irgen {
  class IRGenFunction;

/// A path from one source metadata --- either Swift type metadata or a Swift
/// protocol conformance --- to another.
class MetadataPath {
  class Component {
  public:
    enum class Kind {
      // Some components carry indices.
      // P means the primary index.
      // S means the secondary index.

      /// Protocol conformance S of type argument P of a generic nominal type.
      NominalTypeArgumentConformance,
      LastWithSecondaryIndex = NominalTypeArgumentConformance,

      // Everything past this point has at most one index.

      /// Base protocol P of a protocol.
      InheritedProtocol,

      /// Type argument P of a generic nominal type.
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
    unsigned Secondary;
    enum {
      KindMask = 0xF,
      IndexShift = 4,
    };
    static bool hasPrimaryIndex(Kind kind) {
      return kind <= Kind::LastWithPrimaryIndex;
    }
    static bool hasSecondaryIndex(Kind kind) {
      return kind <= Kind::LastWithSecondaryIndex;
    }

    explicit Component(unsigned primary, unsigned secondary)
        : Primary(primary), Secondary(secondary) {}
  public:
    explicit Component(Kind kind) 
        : Primary(unsigned(kind)), Secondary(0) {
      assert(!hasPrimaryIndex(kind));
    }
    explicit Component(Kind kind, unsigned primaryIndex)
        : Primary(unsigned(kind) | (primaryIndex << IndexShift)),
          Secondary(0) {
      assert(hasPrimaryIndex(kind));
      assert(!hasSecondaryIndex(kind));
    }
    explicit Component(Kind kind, unsigned primaryIndex,
                       unsigned secondaryIndex)
        : Primary(unsigned(kind) | (primaryIndex << IndexShift)),
          Secondary(secondaryIndex) {
      assert(hasSecondaryIndex(kind));
    }

    Kind getKind() const { return Kind(Primary & KindMask); }
    unsigned getPrimaryIndex() const {
      assert(hasPrimaryIndex(getKind()));
      return (Primary >> IndexShift);
    }
    unsigned getSecondaryIndex() const {
      assert(hasSecondaryIndex(getKind()));
      return (Secondary);
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
      unsigned secondary =
        (hasSecondaryIndex(Kind(primary & KindMask))
            ? EncodedSequenceBase::decodeIndex(ptr) : 0);
      return Component(primary, secondary);
    }

    void encode(EncodedSequenceBase::Chunk *&ptr) const {
      EncodedSequenceBase::encodeIndex(Primary, ptr);
      if (hasSecondaryIndex(getKind()))
        EncodedSequenceBase::encodeIndex(Secondary, ptr);
    }

    unsigned getEncodedSize() const {
      auto size = EncodedSequenceBase::getEncodedIndexSize(Primary);
      if (hasSecondaryIndex(getKind()))
        size += EncodedSequenceBase::getEncodedIndexSize(Secondary);
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

  /// Add a step to this path which gets the nth type argument of a generic
  /// type metadata.
  void addNominalTypeArgumentComponent(unsigned index) {
    Path.push_back(Component(Component::Kind::NominalTypeArgument, index));
  }

  /// Add a step to this path which gets the kth protocol conformance of
  /// the nth type argument of a generic type metadata.
  void addNominalTypeArgumentConformanceComponent(unsigned argIndex,
                                                  unsigned conformanceIndex) {
    Path.push_back(Component(Component::Kind::NominalTypeArgumentConformance,
                             argIndex, conformanceIndex));
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
                                      ProtocolDecl *sourceDecl,
                                      llvm::Value *source,
                                      Map<llvm::Value*> *cache) const;

private:
  static llvm::Value *follow(IRGenFunction &IGF,
                             CanType sourceType,
                             Decl *sourceDecl,
                             llvm::Value *source,
                             MetadataPath::iterator begin,
                             MetadataPath::iterator end,
                             Map<llvm::Value*> *cache);

  static llvm::Value *followComponent(IRGenFunction &IGF,
                                      CanType &sourceType,
                                      Decl *&sourceDecl,
                                      llvm::Value *source,
                                      Component component);
};

} // end namespace irgen
} // end namespace swift

#endif
