//===--- Location.h ---------------------------------------------------- -===//
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
// This file defines the class Location. A Location is an abstraction of an
// object field in program. It consists of a base that is the tracked SILValue
// and a projection path to the represented field.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOCATION_H
#define SWIFT_LOCATION_H

#include "swift/SIL/Projection.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

namespace swift {

//===----------------------------------------------------------------------===//
//                                  Location
//===----------------------------------------------------------------------===//

/// Forward declaration.
class Location;
using LocationList = llvm::SmallVector<Location, 8>;

class Location {
public:
  enum KeyKind : uint8_t { EmptyKey = 0, TombstoneKey, NormalKey };

private:
  /// Empty key, tombstone key or normal key.
  KeyKind Kind;
  /// The base of the object.
  SILValue Base;
  /// The path to reach the accessed field of the object.
  Optional<ProjectionPath> Path;

public:
  /// Constructors.
  Location() : Base(), Kind(NormalKey) {}
  Location(SILValue B) : Base(B), Kind(NormalKey) { initialize(B); }
  Location(SILValue B, ProjectionPath &P, KeyKind Kind = NormalKey)
      : Base(B), Path(std::move(P)), Kind(Kind) {}

  /// Copy constructor.
  Location(const Location &RHS) {
    Base = RHS.Base;
    Path.reset();
    Kind = RHS.Kind;
    if (!RHS.Path.hasValue())
      return;
    ProjectionPath X;
    X.append(RHS.Path.getValue());
    Path = std::move(X);
  }

  Location &operator=(const Location &RHS) {
    Base = RHS.Base;
    Path.reset();
    Kind = RHS.Kind;
    if (!RHS.Path.hasValue())
      return *this;
    ProjectionPath X;
    X.append(RHS.Path.getValue());
    Path = std::move(X);
    return *this;
  }

  /// Getters and setters for Location.
  KeyKind getKind() const { return Kind; }
  void setKind(KeyKind K) { Kind = K; }
  SILValue getBase() const { return Base; }
  Optional<ProjectionPath> &getPath() { return Path; }

  /// Returns the hashcode for the location.
  llvm::hash_code getHashCode() const {
    llvm::hash_code HC = llvm::hash_combine(Base.getDef(),
                                            Base.getResultNumber(),
                                            Base.getType());
    if (!Path.hasValue())
      return HC;
    HC = llvm::hash_combine(HC, hash_value(Path.getValue()));
    return HC;
  }

  /// Returns the type of the Location.
  SILType getType() const {
    if (Path.getValue().empty())
      return Base.getType();
    return Path.getValue().front().getType();
  }

  void subtractPaths(Optional<ProjectionPath> &P) {
    if (!P.hasValue())
      return;
    ProjectionPath::subtractPaths(Path.getValue(), P.getValue());
  }

  /// Return false if one projection path is a prefix of another. false
  /// otherwise.
  bool hasNonEmptySymmetricPathDifference(const Location &RHS) const {
    const ProjectionPath &P = RHS.Path.getValue();
    return Path.getValue().hasNonEmptySymmetricDifference(P);
  }

  /// Return true if the 2 locations have identical projection paths.
  /// If both locations have empty paths, they are treated as having
  /// identical projection path.
  bool hasIdenticalProjectionPath(const Location &RHS) const;

  /// Comparisons.
  bool operator!=(const Location &RHS) const { return !(*this == RHS); }
  bool operator==(const Location &RHS) const {
    // If type is not the same, then locations different.
    if (Kind != RHS.Kind)
      return false;
    // If Base is different, then locations different.
    if (Base != RHS.Base)
      return false;

    // If the projection paths are different, then locations are different.
    if (!hasIdenticalProjectionPath(RHS))
      return false;

    // These locations represent the same memory location.
    return true;
  }

  /// Trace the given SILValue till the base of the accessed object. Also
  /// construct the projection path to the field accessed.
  void initialize(SILValue val);

  /// Expand this location to all individual fields it contains.
  ///
  /// In SIL, we can have a store to an aggregate and loads from its individual
  /// fields. Therefore, we expand all the operations on aggregates onto
  /// individual fields.
  void expand(SILModule *Mod, LocationList &F);

  /// Enumerate the given Mem Location.
  static void enumerateLocation(SILModule *M, SILValue Mem,
                                std::vector<Location> &LocationVault,
                                llvm::DenseMap<Location, unsigned> &LocToBit);

  /// Enumerate all the locations in the function.
  static void enumerateLocations(SILFunction &F,
                                 std::vector<Location> &LocationVault,
                                 llvm::DenseMap<Location, unsigned> &LocToBit);
};

static inline llvm::hash_code hash_value(const Location &L) {
  return llvm::hash_combine(L.getBase().getDef(), L.getBase().getResultNumber(),
                            L.getBase().getType());
}


} // end swift namespace


/// Location is used in DenseMap, define functions required by DenseMap.
namespace llvm {

using swift::Location;

template <> struct DenseMapInfo<Location> {
  static inline Location getEmptyKey() {
    Location L;
    L.setKind(Location::EmptyKey);
    return L;
  }
  static inline Location getTombstoneKey() {
    Location L;
    L.setKind(Location::TombstoneKey);
    return L;
  }
  static unsigned getHashValue(const Location &Loc) {
    return hash_value(Loc);
  }
  static bool isEqual(const Location &LHS, const Location &RHS) {
    if (LHS.getKind() == Location::EmptyKey &&
        RHS.getKind() == Location::EmptyKey)
      return true;
    if (LHS.getKind() == Location::TombstoneKey &&
        RHS.getKind() == Location::TombstoneKey)
      return true;
    return LHS == RHS;
  }
};

} // namespace llvm



#endif  // SWIFT_LOCATION_H
