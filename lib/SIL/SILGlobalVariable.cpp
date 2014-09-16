//===--- SILGlobalVariable.cpp - Defines SILGlobalVariable structure ------===//
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

#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILGlobalVariable *SILGlobalVariable::create(SILModule &M, SILLinkage linkage,
                                             StringRef name,
                                             SILType loweredType,
                                             Optional<SILLocation> loc,
                                             VarDecl *Decl) {
  // Get a StringMapEntry for the variable.  As a sop to error cases,
  // allow the name to have an empty string.
  llvm::StringMapEntry<SILGlobalVariable*> *entry = nullptr;
  if (!name.empty()) {
    entry = &M.GlobalVariableTable.GetOrCreateValue(name);
    assert(!entry->getValue() && "global variable already exists");
    name = entry->getKey();
  }

  auto var = new (M) SILGlobalVariable(M, linkage, name, loweredType, loc,
                                       Decl);

  if (entry) entry->setValue(var);
  return var;
}


SILGlobalVariable::SILGlobalVariable(SILModule &Module, SILLinkage Linkage,
                                     StringRef Name, SILType LoweredType,
                                     Optional<SILLocation> Loc, VarDecl *Decl)
  : Module(Module),
    Name(Name),
    LoweredType(LoweredType),
    Location(Loc),
    Linkage(unsigned(Linkage)), VDecl(Decl) {
  IsDeclaration = isAvailableExternally(Linkage);
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
  getModule().GlobalVariableTable.erase(Name);
}
