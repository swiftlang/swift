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
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Memory Location
//===----------------------------------------------------------------------===//

// The base here will point to the actual object this inst is accessing,
// not this particular field.
//
// e.g. %1 = alloc_stack $S
//      %2 = struct_element_addr %1, #a
//      store %3 to %2 : $*Int
//
// Base will point to %1, but not %2. Projection path will indicate which
// field is accessed.
//
// Creating a canonicalized view on memory locations will make comparison
// between locations easier and also eases the implementation of intersection
// operator in the data flow. e.g. if MemLocation are available from multiple
// paths in the CFG and they are accessed in different ways
void MemLocation::initialize(SILValue Dest) {
  Base = getUnderlyingObject(Dest);
  Path = ProjectionPath::getAddrProjectionPath(Base, Dest);
}

void MemLocation::print() const { llvm::outs() << *this; }

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

MemLocation MemLocation::createMemLocation(SILValue Base,
                                           ProjectionPath &P1,
                                           ProjectionPath &P2) {
  ProjectionPath T;
  T.append(P1);
  T.append(P2);
  return MemLocation(Base, T);
}

void MemLocation::getFirstLevelMemLocations(MemLocationList &Locs,
                                            SILModule *Mod) {
  SILType Ty = getType();
  llvm::SmallVector<Projection, 8> Out;
  Projection::getFirstLevelAddrProjections(Ty, *Mod, Out);
  for (auto &X : Out) {
    ProjectionPath P;
    P.append(X);
    P.append(Path.getValue());
    Locs.push_back(MemLocation(Base, P));
  }
}

void MemLocation::expand(MemLocation &Base, SILModule *Mod,
                         MemLocationList &Locs) {
  // To expand a memory location to its indivisible parts, we first get the
  // address projection paths from the accessed type to each indivisible field,
  // i.e. leaf nodes, then we append these projection paths to the Base.
  //
  // NOTE: we get the address projection because the Base memory location is
  // initialized with address projection paths. By keeping it consistent makes
  // it easier to implement the getType function for MemLocation.
  //
  ProjectionPathList Paths;
  ProjectionPath::BreadthFirstEnumTypeProjection(Base.getType(), Mod, Paths,
                                                 true);

  // Construct the MemLocation by appending the projection path from the
  // accessed node to the leaf nodes.
  for (auto &X : Paths) {
    Locs.push_back(MemLocation::createMemLocation(Base.getBase(), X.getValue(),
                                                  Base.getPath().getValue()));
  }
}

void MemLocation::reduce(MemLocation &Base, SILModule *Mod,
                         MemLocationSet &Locs) {
  // Get all the nodes in the projection tree, then go from leaf nodes to their
  // parents. This guarantees that at the point the parent is processed, its 
  // children have been processed already.
  MemLocationList ALocs;
  ProjectionPathList Paths;
  ProjectionPath::BreadthFirstEnumTypeProjection(Base.getType(), Mod, Paths,
                                                 false);

  // Construct the MemLocation by appending the projection path from the
  // accessed node to the leaf nodes.
  for (auto &X : Paths) {
    ALocs.push_back(MemLocation::createMemLocation(Base.getBase(), X.getValue(),
                                                   Base.getPath().getValue()));
  }

  for (auto I = ALocs.rbegin(), E = ALocs.rend(); I != E; ++I) {
    MemLocationList FirstLevel;
    I->getFirstLevelMemLocations(FirstLevel, Mod);
    // Reached the end of the projection tree, this field can not be expanded
    // anymore.
    if (FirstLevel.empty())
      continue;

    // If this is a class reference type, we have reached end of the type tree.
    if (I->getType().getClassOrBoundGenericClass())
      continue;

    // This is NOT a leaf node, check whether all its first level children are
    // alive.
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

void
MemLocation::enumerateMemLocation(SILModule *M, SILValue Mem,
                                  std::vector<MemLocation> &LV,
                                  llvm::DenseMap<MemLocation, unsigned> &BM) {
  // Construct a Location to represent the memory written by this instruction.
  MemLocation L(Mem);

  // If we cant figure out the Base or Projection Path for the memory location,
  // simply ignore it for now.
  if (!L.isValid())
    return;

  // Expand the given Mem into individual fields and add them to the
  // locationvault.
  MemLocationList Locs;
  MemLocation::expand(L, M, Locs);
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
