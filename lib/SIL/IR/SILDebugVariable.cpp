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
  id.AddInteger(argNo);
  id.AddBoolean(constant);
  id.AddBoolean(isImplicit);
  auto ty = type ? *type : SILType();
  id.AddPointer(
      llvm::PointerLikeTypeTraits<decltype(ty)>::getAsVoidPointer(ty));
  // Add loc or nullptr.
  if (declLoc)
    id.Add(*declLoc);
  else
    id.AddPointer(nullptr);
  id.AddPointer(declScope);
  for (auto &expr : exprElements)
    id.Add(expr);
}

void SILDebugVariable::Storage::Profile(llvm::FoldingSetNodeID &id) {
  Profile(id, Name, ArgNo, Constant, Implicit, Type, Loc, Scope,
          DIExpr.getElementArray());
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
  llvm::FoldingSetNodeID id;
  SILDebugVariable::Storage::Profile(id, name, constant, argNo, isImplicit,
                                     auxType, declLoc, declScope, exprElements);

  void *insertPos;
  auto existing =
      mod.SILDebugVariableStorageSet.FindNodeOrInsertPos(id, insertPos);
  if (existing)
    return SILDebugVariable(existing);

  auto *newStorage =
      SILDebugVariable::create(mod, name, constant, argNo, isImplicit, auxType,
                               declLoc, declScope, exprElements);
  mod.SILDebugVariableStorageSet.InsertNode(newStorage, insertPos);
  return SILDebugVariable(newStorage);
}
