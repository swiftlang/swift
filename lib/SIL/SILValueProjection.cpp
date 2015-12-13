//===------------------------- SILValueProjection.cpp ---------------------===//
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

#define DEBUG_TYPE "sil-value-projection"
#include "swift/SIL/SILValueProjection.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                              Utility Functions
//===----------------------------------------------------------------------===//

static inline void removeLSLocations(LSLocationValueMap &Values,
                                     LSLocationList &FirstLevel) {
  for (auto &X : FirstLevel)
    Values.erase(X);
}

//===----------------------------------------------------------------------===//
//                              SILValue Projection
//===----------------------------------------------------------------------===//

void SILValueProjection::print() const {
  llvm::outs() << Base;
  llvm::outs() << Path.getValue();
}

SILValue SILValueProjection::createExtract(SILValue Base,
                                           Optional<ProjectionPath> &Path,
                                           SILInstruction *Inst,
                                           bool IsValExt) {
  // If we found a projection path, but there are no projections, then the two
  // loads must be the same, return PrevLI.
  if (!Path || Path->empty())
    return Base;

  // Ok, at this point we know that we can construct our aggregate projections
  // from our list of address projections.
  SILValue LastExtract = Base;
  SILBuilder Builder(Inst);

  // Construct the path!
  for (auto PI = Path->rbegin(), PE = Path->rend(); PI != PE; ++PI) {
    if (IsValExt) {
      LastExtract =
          PI->createValueProjection(Builder, Inst->getLoc(), LastExtract).get();
      continue;
    }
    LastExtract =
        PI->createAddrProjection(Builder, Inst->getLoc(), LastExtract).get();
  }

  // Return the last extract we created.
  return LastExtract;
}

//===----------------------------------------------------------------------===//
//                              Load Store Value
//===----------------------------------------------------------------------===//

void LSValue::expand(SILValue Base, SILModule *M, LSValueList &Vals,
                     TypeExpansionAnalysis *TE) {
  // To expand a LSValue to its indivisible parts, we first get the
  // address projection paths from the accessed type to each indivisible field,
  // i.e. leaf nodes, then we append these projection paths to the Base.
  for (const auto &P :
       TE->getTypeExpansionProjectionPaths(Base.getType(), M, TEKind::TELeaf)) {
    Vals.push_back(LSValue(Base, P.getValue()));
  }
}

SILValue LSValue::reduce(LSLocation &Base, SILModule *M,
                         LSLocationValueMap &Values,
                         SILInstruction *InsertPt,
                         TypeExpansionAnalysis *TE) {
  // Walk bottom up the projection tree, try to reason about how to construct
  // a single SILValue out of all the available values for all the memory
  // locations.
  //
  // First, get a list of all the leaf nodes and intermediate nodes for the
  // Base memory location.
  LSLocationList ALocs;
  ProjectionPath &BasePath = Base.getPath().getValue();
  for (const auto &P :
       TE->getTypeExpansionProjectionPaths(Base.getType(), M, TEKind::TENode)) {
    ALocs.push_back(LSLocation(Base.getBase(), P.getValue(), BasePath));
  }

  // Second, go from leaf nodes to their parents. This guarantees that at the
  // point the parent is processed, its children have been processed already.
  for (auto I = ALocs.rbegin(), E = ALocs.rend(); I != E; ++I) {
    // This is a leaf node, we have a value for it.
    //
    // Reached the end of the projection tree, this is a leaf node.
    LSLocationList FirstLevel;
    I->getFirstLevelLSLocations(FirstLevel, M);
    if (FirstLevel.empty())
      continue;

    // If this is a class reference type, we have reached end of the type tree.
    if (I->getType().getClassOrBoundGenericClass())
      continue;

    // This is NOT a leaf node, we need to construct a value for it.
    //
    // If there are more than 1 children and all the children nodes have
    // LSValues with the same base. we can get away by not extracting
    // value
    // for every single field.
    //
    // Simply create a new node with all the aggregated base value, i.e.
    // stripping off the last level projection.
    //
    bool HasIdenticalValueBase = true;
    auto Iter = FirstLevel.begin();
    LSValue &FirstVal = Values[*Iter];
    SILValue FirstBase = FirstVal.getBase();
    Iter = std::next(Iter);
    for (auto EndIter = FirstLevel.end(); Iter != EndIter; ++Iter) {
      LSValue &V = Values[*Iter];
      HasIdenticalValueBase &= (FirstBase == V.getBase());
    }

    if (HasIdenticalValueBase &&
        (FirstLevel.size() > 1 || !FirstVal.hasEmptyProjectionPath())) {
      Values[*I] = FirstVal.stripLastLevelProjection();
      // We have a value for the parent, remove all the values for children.
      removeLSLocations(Values, FirstLevel);
      continue;
    }

    // In 2 cases do we need aggregation.
    //
    // 1. If there is only 1 child and we can not strip off any projections,
    // that means we need to create an aggregation.
    //
    // 2. Children have values from different bases, We need to create
    // extractions and aggregation in this case.
    //
    llvm::SmallVector<SILValue, 8> Vals;
    for (auto &X : FirstLevel) {
      Vals.push_back(Values[X].materialize(InsertPt));
    }
    SILBuilder Builder(InsertPt);
    NullablePtr<swift::SILInstruction> AI =
        Projection::createAggFromFirstLevelProjections(
            Builder, InsertPt->getLoc(), I->getType(), Vals);
    // This is the Value for the current node.
    ProjectionPath P;
    Values[*I] = LSValue(SILValue(AI.get()), P);
    removeLSLocations(Values, FirstLevel);

    // Keep iterating until we have reach the top-most level of the projection
    // tree.
    // i.e. the memory location represented by the Base.
  }

  assert(Values.size() == 1 && "Should have a single location this point");

  // Finally materialize and return the forwarding SILValue.
  return Values.begin()->second.materialize(InsertPt);
}


void LSValue::enumerateLSValue(SILModule *M, SILValue Val,
                               std::vector<LSValue> &Vault,
                               LSValueIndexMap &ValToBit,
                               TypeExpansionAnalysis *TE) {
  // Expand the given Mem into individual fields and add them to the
  // locationvault.
  LSValueList Vals;
  LSValue::expand(Val, M, Vals, TE);
  for (auto &Val : Vals) {
    ValToBit[Val] = Vault.size();
    Vault.push_back(Val);
  }
}

void LSValue::enumerateLSValues(SILFunction &F, std::vector<LSValue> &Vault,
                                LSValueIndexMap &ValToBit,
                                TypeExpansionAnalysis *TE) {
  // Enumerate all LSValues created or used by the loads or stores.
  //
  // TODO: process more instructions as we process more instructions in
  // processInstruction.
  //
  SILValue Op;
  for (auto &B : F) {
    for (auto &I : B) {
      if (auto *LI = dyn_cast<LoadInst>(&I)) {
        enumerateLSValue(&I.getModule(), SILValue(LI), Vault, ValToBit, TE);
      } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
        enumerateLSValue(&I.getModule(), SI->getSrc(), Vault, ValToBit, TE);
      }
    }
  }

  // Lastly, push in the covering value LSValue.
  ValToBit[LSValue(true)] = Vault.size();
  Vault.push_back(LSValue(true));
}

//===----------------------------------------------------------------------===//
//                                  Memory Location
//===----------------------------------------------------------------------===//

void LSLocation::initialize(SILValue Dest) {
  Base = getUnderlyingObject(Dest);
  Path = ProjectionPath::getAddrProjectionPath(Base, Dest);
}

bool LSLocation::isMustAliasLSLocation(const LSLocation &RHS,
                                       AliasAnalysis *AA) {
  // If the bases are not must-alias, the locations may not alias.
  if (!AA->isMustAlias(Base, RHS.getBase()))
    return false;
  // If projection paths are different, then the locations can not alias.
  if (!hasIdenticalProjectionPath(RHS))
    return false;
  return true;
}

bool LSLocation::isMayAliasLSLocation(const LSLocation &RHS,
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

void LSLocation::getFirstLevelLSLocations(LSLocationList &Locs,
                                          SILModule *Mod) {
  SILType Ty = getType();
  llvm::SmallVector<Projection, 8> Out;
  Projection::getFirstLevelAddrProjections(Ty, *Mod, Out);
  for (auto &X : Out) {
    ProjectionPath P;
    P.append(X);
    P.append(Path.getValue());
    Locs.push_back(LSLocation(Base, P));
  }
}

void LSLocation::expand(LSLocation &Base, SILModule *M, LSLocationList &Locs,
                        TypeExpansionAnalysis *TE) {
  // To expand a memory location to its indivisible parts, we first get the
  // address projection paths from the accessed type to each indivisible field,
  // i.e. leaf nodes, then we append these projection paths to the Base.
  //
  // Construct the LSLocation by appending the projection path from the
  // accessed node to the leaf nodes.
  ProjectionPath &BasePath = Base.getPath().getValue();
  for (const auto &P :
       TE->getTypeExpansionProjectionPaths(Base.getType(), M, TEKind::TELeaf)) {
    Locs.push_back(LSLocation(Base.getBase(), P.getValue(), BasePath));
  }
}

void LSLocation::reduce(LSLocation &Base, SILModule *M, LSLocationSet &Locs,
                        TypeExpansionAnalysis *TE) {
  // First, construct the LSLocation by appending the projection path from the
  // accessed node to the leaf nodes.
  LSLocationList Nodes;
  ProjectionPath &BasePath = Base.getPath().getValue();
  for (const auto &P :
       TE->getTypeExpansionProjectionPaths(Base.getType(), M, TEKind::TENode)) {
    Nodes.push_back(LSLocation(Base.getBase(), P.getValue(), BasePath));
  }

  // Second, go from leaf nodes to their parents. This guarantees that at the
  // point the parent is processed, its children have been processed already.
  for (auto I = Nodes.rbegin(), E = Nodes.rend(); I != E; ++I) {
    LSLocationList FirstLevel;
    I->getFirstLevelLSLocations(FirstLevel, M);
    // Reached the end of the projection tree, this is a leaf node.
    if (FirstLevel.empty())
      continue;

    // If this is a class reference type, we have reached end of the type tree.
    if (I->getType().getClassOrBoundGenericClass())
      continue;

    // This is NOT a leaf node, check whether all its first level children are
    // alive.
    bool Alive = true;
    for (auto &X : FirstLevel) {
      Alive &= Locs.find(X) != Locs.end();
    }

    // All first level locations are alive, create the new aggregated location.
    if (Alive) {
      for (auto &X : FirstLevel)
        Locs.erase(X);
      Locs.insert(*I);
    }
  }
}


void LSLocation::enumerateLSLocation(SILModule *M, SILValue Mem,
                                     std::vector<LSLocation> &LV,
                                     LSLocationIndexMap &BM,
                                     TypeExpansionAnalysis *TE) {
  // Construct a Location to represent the memory written by this instruction.
  LSLocation L(Mem);

  // If we cant figure out the Base or Projection Path for the memory location,
  // simply ignore it for now.
  if (!L.isValid())
    return;

  // Expand the given Mem into individual fields and add them to the
  // locationvault.
  LSLocationList Locs;
  LSLocation::expand(L, M, Locs, TE);
  for (auto &Loc : Locs) {
    BM[Loc] = LV.size();
    LV.push_back(Loc);
  }
}

void LSLocation::enumerateLSLocations(SILFunction &F,
                                      std::vector<LSLocation> &LV,
                                      LSLocationIndexMap &BM,
                                      TypeExpansionAnalysis *TE) {
  // Enumerate all locations accessed by the loads or stores.
  //
  // TODO: process more instructions as we process more instructions in
  // processInstruction.
  //
  SILValue Op;
  for (auto &B : F) {
    for (auto &I : B) {
      if (auto *LI = dyn_cast<LoadInst>(&I)) {
        enumerateLSLocation(&I.getModule(), LI->getOperand(), LV, BM, TE);
      } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
        enumerateLSLocation(&I.getModule(), SI->getDest(), LV, BM, TE);
      }
    }
  }
}
