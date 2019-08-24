//===--- Fulfillment.h - Deriving type/conformance metadata -----*- C++ -*-===//
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
// This file defines interfaces for deriving type metadata and protocol
// witness tables from various sources.
// 
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_FULFILLMENT_H
#define SWIFT_IRGEN_FULFILLMENT_H

#include "llvm/ADT/DenseMap.h"
#include "swift/AST/Types.h"
#include "swift/AST/GenericSignature.h"
#include "MetadataPath.h"

namespace swift {
namespace irgen {
  class IRGenModule;
  enum IsExact_t : bool;

/// The metadata value can be fulfilled by following the given metadata
/// path from the given source.
struct Fulfillment {
  Fulfillment() = default;
  Fulfillment(unsigned sourceIndex, MetadataPath &&path, MetadataState state)
    : SourceIndex(sourceIndex), State(unsigned(state)), Path(std::move(path)) {}

  /// The source index.
  unsigned SourceIndex : 30;

  /// The state of the metadata at the fulfillment.
  unsigned State : 2;

  /// The path from the source metadata.
  MetadataPath Path;

  MetadataState getState() const { return MetadataState(State); }
};

class FulfillmentMap {
  using FulfillmentKey = std::pair<Type, ProtocolDecl*>;

  llvm::DenseMap<FulfillmentKey, Fulfillment> Fulfillments;

public:
  struct InterestingKeysCallback {
    /// Is the given type something that we should add fulfillments for?
    virtual bool isInterestingType(CanType type) const = 0;

    /// Is the given type expressed in terms of types that we should add
    /// fulfillments for?
    ///
    /// It's okay to conservatively return true here.
    virtual bool hasInterestingType(CanType type) const = 0;

    /// Are we only interested in a subset of the conformances for a
    /// given type?
    virtual bool hasLimitedInterestingConformances(CanType type) const = 0;

    /// Return the limited interesting conformances for an interesting type.
    virtual GenericSignature::ConformsToArray
      getInterestingConformances(CanType type) const = 0;

    /// Return the limited interesting conformances for an interesting type.
    virtual CanType getSuperclassBound(CanType type) const = 0;

    virtual ~InterestingKeysCallback() = default;
  };

  FulfillmentMap() = default;

  using iterator = decltype(Fulfillments)::iterator;
  iterator begin() { return Fulfillments.begin(); }
  iterator end() { return Fulfillments.end(); }

  /// Is it even theoretically possible that we might find a fulfillment
  /// in the given type?
  static bool isInterestingTypeForFulfillments(CanType type) {
    // Some day, if we ever record fulfillments for concrete types, this
    // optimization will probably no longer be useful.
    return type->hasTypeParameter();
  }

  /// Search the given type metadata for useful fulfillments.
  ///
  /// \return true if any fulfillments were added by this search.
  bool searchTypeMetadata(IRGenModule &IGM, CanType type, IsExact_t isExact,
                          MetadataState metadataState,
                          unsigned sourceIndex, MetadataPath &&path,
                          const InterestingKeysCallback &interestingKeys);

  bool searchConformance(IRGenModule &IGM,
                         const ProtocolConformance *conformance,
                         unsigned sourceIndex, MetadataPath &&path,
                         const InterestingKeysCallback &interestingKeys);

  /// Search the given witness table for useful fulfillments.
  ///
  /// \return true if any fulfillments were added by this search.
  bool searchWitnessTable(IRGenModule &IGM, CanType type, ProtocolDecl *protocol,
                          unsigned sourceIndex, MetadataPath &&path,
                          const InterestingKeysCallback &interestingKeys);

  /// Register a fulfillment for the given key.
  ///
  /// \return true if the fulfillment was added, which won't happen if there's
  ///   already a fulfillment that was at least as good
  bool addFulfillment(FulfillmentKey key, unsigned source,
                      MetadataPath &&path, MetadataState state);

  const Fulfillment *getTypeMetadata(CanType type) const {
    auto it = Fulfillments.find({type, nullptr});
    if (it != Fulfillments.end()) {
      return &it->second;
    } else {
      return nullptr;
    }
  }

  const Fulfillment *getWitnessTable(CanType type, ProtocolDecl *proto) const {
    auto it = Fulfillments.find({type, proto});
    if (it != Fulfillments.end()) {
      return &it->second;
    } else {
      return nullptr;
    }
  }

  void dump() const;
  void print(llvm::raw_ostream &out) const;
  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                       const FulfillmentMap &map) {
    map.print(out);
    return out;
  }

private:
  bool searchNominalTypeMetadata(IRGenModule &IGM, CanType type,
                                 MetadataState metadataState, unsigned source,
                                 MetadataPath &&path,
                                 const InterestingKeysCallback &keys);

  /// Search the given witness table for useful fulfillments.
  ///
  /// \return true if any fulfillments were added by this search.
  bool searchWitnessTable(
      IRGenModule &IGM, CanType type, ProtocolDecl *protocol, unsigned source,
      MetadataPath &&path, const InterestingKeysCallback &keys,
      llvm::SmallPtrSetImpl<ProtocolDecl *> *interestingConformances);
};

}
}

#endif
