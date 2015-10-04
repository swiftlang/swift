//===--- Location.cpp ---------------------------------------------------===//
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

#define DEBUG_TYPE "sil-location"
#include "swift/SIL/Location.h"

using namespace swift;


//===----------------------------------------------------------------------===//
//                                  Location
//===----------------------------------------------------------------------===//

void Location::initialize(SILValue Dest) {
  Base = getUnderlyingObject(Dest);
  Path = ProjectionPath::getAddrProjectionPath(Base, Dest);
}

bool Location::hasIdenticalProjectionPath(const Location &RHS) const {
  // If both Paths have no value, then locations are different.
  if (!Path.hasValue() && !RHS.Path.hasValue())
    return false;
  // If 1 Path has value while the other does not, then the 2 locations
  // are different.
  if (Path.hasValue() != RHS.Path.hasValue())
    return false;
  // If both Paths are empty, then the 2 locations are the same.
  if (Path.getValue().empty() && RHS.Path.getValue().empty())
    return true;
  // If 2 Paths have different values, then the 2 locations are different.
  if (Path.getValue() != RHS.Path.getValue())
    return false;
  return true;
}

void Location::expand(SILModule *Mod, LocationList &Locs) {
  // Expands the given type into locations each of which contains 1 field from
  // the type.
  LocationList Worklist;
  llvm::SmallVector<Projection, 8> Projections;

  Worklist.push_back(*this);
  while (!Worklist.empty()) {
    // Get the next level projections based on current location's type.
    Location L = Worklist.pop_back_val();
    Projections.clear();
    Projection::getFirstLevelProjections(L.getType(), *Mod, Projections);

    // Reached the end of the projection tree, this field can not be expanded
    // anymore.
    if (Projections.empty()) {
      Locs.push_back(L);
      continue;
    }

    // Keep expanding the location.
    for (auto &P : Projections) {
      ProjectionPath X;
      X.append(P);
      X.append(L.Path.getValue());
      Location LL(Base, X);
      Worklist.push_back(LL);
    }
  }
}

bool Location::isMayAliasLocation(const Location &RHS, AliasAnalysis *AA) {
  // If the bases do not alias, then the locations can not alias.
  if (AA->isNoAlias(Base, RHS.getBase()))
    return false;
  // If one projection path is a prefix of another, then the locations
  // could alias.
  if (hasNonEmptySymmetricPathDifference(RHS))
    return false;
  return true;
}

bool Location::isMustAliasLocation(const Location &RHS, AliasAnalysis *AA) {
  // If the bases are not must-alias, the locations may not alias.
  if (!AA->isMustAlias(Base, RHS.getBase()))
    return false;
  // If projection paths are different, then the locations can not alias.
  if (!hasIdenticalProjectionPath(RHS))
    return false;
  return true;
}

void Location::enumerateLocation(SILModule *M, SILValue Mem,
                                 std::vector<Location> &LV,
                                 llvm::DenseMap<Location, unsigned> &BM) {
  // Construct a Location to represent the memory written by this instruction.
  Location L(Mem);

  // If we cant figure out the Base or Projection Path for the memory location,
  // simply ignore it for now.
  if (!L.getBase() || !L.getPath().hasValue())
    return;

  // Expand the given Mem into individual fields and add them to the
  // locationvault.
  LocationList Locs;
  L.expand(M, Locs);
  for (auto &Loc : Locs) {
    BM[Loc] = LV.size();
    LV.push_back(Loc);
  }
}

void Location::enumerateLocations(SILFunction &F,
                                  std::vector<Location> &LV,
                                  llvm::DenseMap<Location, unsigned> &BM) {
  // Enumerate all locations accessed by the loads or stores.
  //
  // TODO: process more instructions as we process more instructions in
  // processInstruction.
  //
  SILValue Op;
  for (auto &B : F) {
    for (auto &I : B) {
      if (auto *LI = dyn_cast<LoadInst>(&I)) {
        enumerateLocation(&I.getModule(), LI->getOperand(), LV, BM);
      } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
        enumerateLocation(&I.getModule(), SI->getDest(), LV, BM);
      }
    }
  }
}
