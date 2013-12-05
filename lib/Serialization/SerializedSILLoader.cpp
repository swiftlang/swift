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
                                         SILModule *SILMod) {
  // Get a list of SerializedModules from ASTContext.
  for (auto &CtxM : Ctx.LoadedModules) {
    if (auto TU = dyn_cast<TranslationUnit>(CtxM.getValue())) {
      for (auto File : TU->getFiles()) {
        if (auto LoadedAST = dyn_cast<SerializedASTFile>(File)) {
          auto Des = new SILDeserializer(&LoadedAST->File, *SILMod, Ctx);
          LoadedSILSections.emplace_back(Des);
        }
      }
    }
  }
} 

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
