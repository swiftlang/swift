//===--- SILDebugVariable.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILDebugVariable.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

Optional<SILDebugVariable>
SILDebugVariable::createFromAllocation(const AllocationInst *ai) {
  Optional<SILDebugVariable> varInfo;
  if (const auto *asi = dyn_cast_or_null<AllocStackInst>(ai))
    varInfo = asi->getVarInfo();
  // TODO: Support AllocBoxInst

  if (!varInfo)
    return varInfo;

  // Copy everything but the DIExpr
  Builder b(ai->getModule(), *varInfo);
  b.withDIExpr({});

  // Coalesce the debug loc attached on AI into VarInfo
  SILType type = ai->getType();
  SILLocation instLoc = ai->getLoc();
  const SILDebugScope *instDS = ai->getDebugScope();
  if (!varInfo->getType())
    b.withType(type);
  if (!varInfo->getLoc())
    b.withLoc(instLoc);
  if (!varInfo->getScope())
    b.withScope(instDS);

  return std::move(b).finalize();
}

void SILDebugVariable::Storage::Profile(
    llvm::FoldingSetNodeID &id, StringRef name, bool constant, unsigned argNo,
    bool isImplicit, Optional<SILType> type, Optional<SILLocation> declLoc,
    const SILDebugScope *declScope, ArrayRef<SILDIExprElement> exprElements) {
  id.AddString(name);
  llvm::dbgs() << "Name Hash1: " << id.ComputeHash() << '\n';
  id.AddBoolean(constant);
  llvm::dbgs() << "Constant Hash2: " << id.ComputeHash() << '\n';
  id.AddInteger(argNo);
  llvm::dbgs() << "ArgNo Hash3: " << id.ComputeHash() << '\n';
  id.AddBoolean(isImplicit);
  llvm::dbgs() << "IsImplicit Hash4: " << id.ComputeHash() << '\n';
  auto ty = type ? *type : SILType();
  id.AddPointer(
      llvm::PointerLikeTypeTraits<SILType>::getAsVoidPointer(ty));
  llvm::dbgs() << "Type Hash5: " << id.ComputeHash() << '\n';
  auto loc = declLoc ? *declLoc : SILLocation::invalid();
  id.Add(loc);
  llvm::dbgs() << "Loc Hash6: " << id.ComputeHash() << '\n';
  id.AddPointer(declScope);
  llvm::dbgs() << "Scope Hash7: " << id.ComputeHash() << '\n';
  id.AddInteger(exprElements.size());
  llvm::dbgs() << "Expr Element Size Hash8: " << id.ComputeHash() << '\n';
  for (auto &expr : exprElements) {
    id.Add(expr);
    llvm::dbgs() << "Expr Element HashN: " << id.ComputeHash() << '\n';
  }
}

void SILDebugVariable::Storage::Profile(llvm::FoldingSetNodeID &id) {
  Profile(id, Name, ArgNo, Constant, Implicit, Type, Loc, Scope,
          DIExpr.getElementArray());
}

void SILDebugVariable::Storage::dump() {
  llvm::dbgs() << "SILDebugVariable!\n";
  llvm::dbgs() << "Name: " << Name << '\n';
  llvm::dbgs() << "ArgNo: " << ArgNo << '\n';
  llvm::dbgs() << "Constant: " << Constant << '\n';
  llvm::dbgs() << "Implicit: " << Implicit << '\n';
  llvm::dbgs() << "Type: " << Type << '\n';
  llvm::dbgs() << "Loc: "; if (Loc) Loc->dump(); llvm::dbgs() << '\n';
  llvm::dbgs() << "Scope: " << Scope << '\n';
  llvm::dbgs() << "DIExpr Num Elts: " << DIExpr.getElementArray().size()
               << '\n';
  for (auto &elt : DIExpr.getElementArray()) {
    llvm::dbgs() << "Elt: " << elt.getKind() << '\n'; 
  }
}

SILDebugVariable::Storage *SILDebugVariable::create(
    SILModule &mod, StringRef name, bool constant, unsigned argNo,
    bool isImplicit, Optional<SILType> auxType, Optional<SILLocation> declLoc,
    const SILDebugScope *declScope,
    llvm::ArrayRef<SILDIExprElement> exprElements) {
  void *mem = mod.allocate(sizeof(SILDebugVariable::Storage),
                           alignof(SILDebugVariable::Storage));
  return ::new (mem)
      SILDebugVariable::Storage(name, constant, argNo, isImplicit, auxType,
                                declLoc, declScope, exprElements);
}

SILDebugVariable
SILDebugVariable::get(SILModule &mod, StringRef name, bool constant,
                      unsigned argNo, bool isImplicit,
                      Optional<SILType> auxType, Optional<SILLocation> declLoc,
                      const SILDebugScope *declScope,
                      llvm::ArrayRef<SILDIExprElement> exprElements) {
  llvm::dbgs() << "At top of SILDebugVariable::get!\n";
  SWIFT_DEFER {   llvm::dbgs() << "Exiting SILDebugVariable::get!\n"; };

  llvm::dbgs() << "Input Values:\n";
  llvm::dbgs() << "Name: " << name << '\n';
  llvm::dbgs() << "ArgNo: " << argNo << '\n';
  llvm::dbgs() << "Constant: " << constant << '\n';
  llvm::dbgs() << "Implicit: " << isImplicit << '\n';
  llvm::dbgs() << "Type: " << auxType << '\n';
  llvm::dbgs() << "Loc: "; if (declLoc) declLoc->dump(); llvm::dbgs() << '\n';
  llvm::dbgs() << "Scope: " << declScope << '\n';
  llvm::dbgs() << "DIExpr Num Elts: " << exprElements.size()
               << '\n';
  for (auto &elt : exprElements) {
    llvm::dbgs() << "Elt: " << elt.getKind() << '\n'; 
  }

  llvm::FoldingSetNodeID id;
  SILDebugVariable::Storage::Profile(id, name, constant, argNo, isImplicit,
                                     auxType, declLoc, declScope, exprElements);
  llvm::dbgs() << "Original Hash: " << id.ComputeHash() << '\n';
  llvm::dbgs() << "After first profile.\n";
  void *insertPos;
  auto *existing =
      mod.SILDebugVariableStorageSet.FindNodeOrInsertPos(id, insertPos);
  if (existing) {
    fprintf(stderr, "Matched to existing storage: %p\n", existing);
    llvm::FoldingSetNodeID id2;
    existing->Profile(id2);
    llvm::dbgs() << "Existing Hash: " << id2.ComputeHash() << '\n';
    existing->dump();
    assert(name == existing->Name);
    assert(constant == existing->Constant);
    assert(argNo == existing->ArgNo);
    assert(isImplicit == existing->Implicit);
    assert(auxType == existing->Type);
    assert(declLoc == existing->Loc);
    assert(declScope == existing->Scope);
    return SILDebugVariable(existing);
  }

  auto *newStorage =
      SILDebugVariable::create(mod, name, constant, argNo, isImplicit, auxType,
                               declLoc, declScope, exprElements);
  fprintf(stderr, "Just created new storage: %p\n", newStorage);
  mod.SILDebugVariableStorageSet.InsertNode(newStorage, insertPos);
  fprintf(stderr, "After inserting storage!\n");
  newStorage->dump();
  llvm::FoldingSetNodeID id2;
  newStorage->Profile(id2);
  llvm::dbgs() << "New Hash: " << id2.ComputeHash() << '\n';
  return SILDebugVariable(newStorage);
}
