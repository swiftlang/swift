//===--- SerializedSILLoader.cpp - A loader for SIL sections --------------===//
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

#include "DeserializeSIL.h"
#include "ModuleFile.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SerializedSILLoader::SerializedSILLoader(ASTContext &Ctx,
                                         SILModule *SILMod,
                                         Callback *callback) {

  // Get a list of SerializedModules from ASTContext.
  for (auto &Entry : Ctx.LoadedModules) {
    for (auto File : Entry.getValue()->getFiles()) {
      if (auto LoadedAST = dyn_cast<SerializedASTFile>(File)) {
        auto Des = new SILDeserializer(&LoadedAST->File, *SILMod, Ctx,
                                       callback);
        LoadedSILSections.emplace_back(Des);
      }
    }
  }
}

SerializedSILLoader::~SerializedSILLoader() {}

SILFunction *SerializedSILLoader::lookupSILFunction(SILFunction *Callee) {
  for (auto &Des : LoadedSILSections) {
    if (auto Func = Des->lookupSILFunction(Callee))
      return Func;
  }
  return nullptr;
}

SILVTable *SerializedSILLoader::lookupVTable(Identifier Name) {
  for (auto &Des : LoadedSILSections) {
    if (auto VT = Des->lookupVTable(Name))
      return VT;
  }
  return nullptr;
}

/// Deserialize all VTables in all SILModules.
void SerializedSILLoader::getAllVTables() {
  for (auto &Des : LoadedSILSections)
    Des->getAllVTables();
}

// Anchor the SerializedSILLoader v-table.
void SerializedSILLoader::Callback::_anchor() {}
