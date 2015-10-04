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
