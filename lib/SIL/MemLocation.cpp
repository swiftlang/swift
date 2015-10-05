//===------------------------- MemLocation.cpp ----------------------------===//
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

#define DEBUG_TYPE "sil-memlocation"
#include "swift/SIL/MemLocation.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Memory Location
//===----------------------------------------------------------------------===//

void MemLocation::initialize(SILValue Dest) {
  Base = getUnderlyingObject(Dest);
  Path = ProjectionPath::getAddrProjectionPath(Base, Dest);
}

bool MemLocation::hasIdenticalProjectionPath(const MemLocation &RHS) const {
  // If both Paths have no value, then the 2 locations are different.
  if (!Path.hasValue() && !RHS.Path.hasValue())
    return false;
  // If 1 Path has value while the other does not, then the 2 locations
  // are different.
  if (Path.hasValue() != RHS.Path.hasValue())
    return false;
  // If both Paths are empty, then the 2 locations are the same.
  if (Path.getValue().empty() && RHS.Path.getValue().empty())
    return true;
  // If both Paths have different values, then the 2 locations are different.
  if (Path.getValue() != RHS.Path.getValue())
    return false;
  return true;
}

void MemLocation::expand(SILModule *Mod, MemLocationList &Locs,
                         bool OnlyLeafNode) {
  // Perform a BFS to expand the given type into locations each of which
  // contains 1 field from the type.
  MemLocationList Worklist;
  llvm::SmallVector<Projection, 8> Projections;

  if (!OnlyLeafNode)
    Locs.push_back(*this);

  Worklist.push_back(*this);
  while (!Worklist.empty()) {
    // Get the next level projections based on current location's type.
    MemLocation L = Worklist.pop_back_val();
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
      MemLocation LL(Base, X);
      Worklist.push_back(LL);

      // Keep the intermediate nodes as well.
      if (!OnlyLeafNode)
        Locs.push_back(LL);
    }
  }
}

bool MemLocation::isMayAliasMemLocation(const MemLocation &RHS,
                                        AliasAnalysis *AA) {
  // If the bases do not alias, then the locations can not alias.
  if (AA->isNoAlias(Base, RHS.getBase()))
    return false;
  // If one projection path is a prefix of another, then the locations
  // could alias.
  if (hasNonEmptySymmetricPathDifference(RHS))
    return false;
  return true;
}

void MemLocation::getFirstLevelMemLocations(MemLocationList &Locs,
                                            SILModule *Mod) {
  SILType Ty = getType();
  llvm::SmallVector<Projection, 8> Out;
  Projection::getFirstLevelProjections(Ty, *Mod, Out);
  for (auto &X : Out) {
    ProjectionPath P;
    P.append(X);
    P.append(Path.getValue());
    Locs.push_back(MemLocation(Base, P));
  }
}

void MemLocation::mergeMemLocations(llvm::DenseSet<MemLocation> &Locs,
                                    MemLocation &M,
                                    SILModule *Mod) {
  // Nothing to merge.
  if (Locs.empty())
    return;

  // Get all the nodes in the projection tree, then go from leaf nodes to their
  // parents. This guarantees that at the point the parent is processed, its 
  // children have been processed already.
  MemLocationList AllLocs;
  M.expand(Mod, AllLocs, false);
  for (auto I = AllLocs.rbegin(), E = AllLocs.rend(); I != E; ++I) {
    MemLocationList FirstLevel;
    I->getFirstLevelMemLocations(FirstLevel, Mod);

    if (FirstLevel.empty())
      continue;

    bool Alive = true;
    for (auto &X : FirstLevel) {
      if (Locs.find(X) != Locs.end())
        continue;
      Alive = false;
    }

    // All first level locations are alive, create the new aggregated location.
    if (Alive) {
      for (auto &X : FirstLevel)
        Locs.erase(X);
      Locs.insert(*I);
    }
  }
}

bool MemLocation::isMustAliasMemLocation(const MemLocation &RHS,
                                         AliasAnalysis *AA) {
  // If the bases are not must-alias, the locations may not alias.
  if (!AA->isMustAlias(Base, RHS.getBase()))
    return false;
  // If projection paths are different, then the locations can not alias.
  if (!hasIdenticalProjectionPath(RHS))
    return false;
  return true;
}

void
MemLocation::enumerateMemLocation(SILModule *M, SILValue Mem,
                                  std::vector<MemLocation> &LV,
                                  llvm::DenseMap<MemLocation, unsigned> &BM) {
  // Construct a Location to represent the memory written by this instruction.
  MemLocation L(Mem);

  // If we cant figure out the Base or Projection Path for the memory location,
  // simply ignore it for now.
  if (!L.getBase() || !L.getPath().hasValue())
    return;

  // Expand the given Mem into individual fields and add them to the
  // locationvault.
  MemLocationList Locs;
  L.expand(M, Locs);
  for (auto &Loc : Locs) {
    BM[Loc] = LV.size();
    LV.push_back(Loc);
  }
}

void
MemLocation::enumerateMemLocations(SILFunction &F,
                                   std::vector<MemLocation> &LV,
                                   llvm::DenseMap<MemLocation, unsigned> &BM) {
  // Enumerate all locations accessed by the loads or stores.
  //
  // TODO: process more instructions as we process more instructions in
  // processInstruction.
  //
  SILValue Op;
  for (auto &B : F) {
    for (auto &I : B) {
      if (auto *LI = dyn_cast<LoadInst>(&I)) {
        enumerateMemLocation(&I.getModule(), LI->getOperand(), LV, BM);
      } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
        enumerateMemLocation(&I.getModule(), SI->getDest(), LV, BM);
      }
    }
  }
}


