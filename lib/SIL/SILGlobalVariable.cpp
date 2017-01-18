//===--- SILGlobalVariable.cpp - Defines SILGlobalVariable structure ------===//
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

#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILGlobalVariable *SILGlobalVariable::create(SILModule &M, SILLinkage linkage,
                                             bool IsFragile,
                                             StringRef name,
                                             SILType loweredType,
                                             Optional<SILLocation> loc,
                                             VarDecl *Decl) {
  // Get a StringMapEntry for the variable.  As a sop to error cases,
  // allow the name to have an empty string.
  llvm::StringMapEntry<SILGlobalVariable*> *entry = nullptr;
  if (!name.empty()) {
    entry = &*M.GlobalVariableMap.insert(std::make_pair(name, nullptr)).first;
    assert(!entry->getValue() && "global variable already exists");
    name = entry->getKey();
  }

  auto var = new (M) SILGlobalVariable(M, linkage, IsFragile, name,
                                       loweredType, loc, Decl);

  if (entry) entry->setValue(var);
  return var;
}


SILGlobalVariable::SILGlobalVariable(SILModule &Module, SILLinkage Linkage,
                                     bool IsFragile,
                                     StringRef Name, SILType LoweredType,
                                     Optional<SILLocation> Loc, VarDecl *Decl)
  : Module(Module),
    Name(Name),
    LoweredType(LoweredType),
    Location(Loc),
    Linkage(unsigned(Linkage)),
    Fragile(IsFragile),
    VDecl(Decl) {
  IsDeclaration = isAvailableExternally(Linkage);
  setLet(Decl ? Decl->isLet() : false);
  InitializerF = nullptr;
  Module.silGlobals.push_back(this);
}

void SILGlobalVariable::setInitializer(SILFunction *InitF) {
  if (InitializerF)
    InitializerF->decrementRefCount();
  // Increment the ref count to make sure it will not be eliminated.
  InitF->incrementRefCount();
  InitializerF = InitF;
}

SILGlobalVariable::~SILGlobalVariable() {
  getModule().GlobalVariableMap.erase(Name);
}

// FIXME

static bool analyzeStaticInitializer(SILFunction *F, SILInstruction *&Val,
                                     SILGlobalVariable *&GVar) {
  Val = nullptr;
  GVar = nullptr;
  // We only handle a single SILBasicBlock for now.
  if (F->size() != 1)
    return false;

  SILBasicBlock *BB = &F->front();
  GlobalAddrInst *SGA = nullptr;
  bool HasStore = false;
  for (auto &I : *BB) {
    // Make sure we have a single GlobalAddrInst and a single StoreInst.
    // And the StoreInst writes to the GlobalAddrInst.
    if (isa<AllocGlobalInst>(&I)) {
      continue;
    } else if (auto *sga = dyn_cast<GlobalAddrInst>(&I)) {
      if (SGA)
        return false;
      SGA = sga;
      GVar = SGA->getReferencedGlobal();
    } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
      if (HasStore || SI->getDest() != SGA)
        return false;
      HasStore = true;
      Val = dyn_cast<SILInstruction>(SI->getSrc());

      // We only handle StructInst and TupleInst being stored to a
      // global variable for now.
      if (!isa<StructInst>(Val) && !isa<TupleInst>(Val))
        return false;
    } else {

      if (auto *bi = dyn_cast<BuiltinInst>(&I)) {
        switch (bi->getBuiltinInfo().ID) {
        case BuiltinValueKind::FPTrunc:
          if (isa<LiteralInst>(bi->getArguments()[0]))
            continue;
          break;
        default:
          return false;
        }
      }

      // Objective-C selector string literals cannot be used in static
      // initializers.
      if (auto *stringLit = dyn_cast<StringLiteralInst>(&I)) {
        switch (stringLit->getEncoding()) {
        case StringLiteralInst::Encoding::UTF8:
        case StringLiteralInst::Encoding::UTF16:
          continue;

        case StringLiteralInst::Encoding::ObjCSelector:
          return false;
        }
      }

      if (I.getKind() != ValueKind::ReturnInst &&
          I.getKind() != ValueKind::StructInst &&
          I.getKind() != ValueKind::TupleInst &&
          I.getKind() != ValueKind::DebugValueInst &&
          I.getKind() != ValueKind::IntegerLiteralInst &&
          I.getKind() != ValueKind::FloatLiteralInst)
        return false;
    }
  }
  return true;
}

bool SILGlobalVariable::canBeStaticInitializer(SILFunction *F) {
  SILInstruction *dummySI;
  SILGlobalVariable *dummyGV;
  return analyzeStaticInitializer(F, dummySI, dummyGV);
}

/// Check if a given SILFunction can be a static initializer. If yes, return
/// the SILGlobalVariable that it writes to.
SILGlobalVariable *SILGlobalVariable::getVariableOfStaticInitializer(
                     SILFunction *F) {
  SILInstruction *dummySI;
  SILGlobalVariable *GV;
  if (analyzeStaticInitializer(F, dummySI, GV))
    return GV;
  return nullptr;
}

/// Return the value that is written into the global variable.
SILInstruction *SILGlobalVariable::getValueOfStaticInitializer() {
  if (!InitializerF)
    return nullptr;

  SILInstruction *SI;
  SILGlobalVariable *dummyGV;
  if (analyzeStaticInitializer(InitializerF, SI, dummyGV))
    return SI;
  return nullptr;
}
