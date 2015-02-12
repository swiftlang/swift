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

#define DEBUG_TYPE "serialized-sil-loader"
#include "swift/Serialization/SerializedSILLoader.h"
#include "DeserializeSIL.h"
#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/Debug.h"

using namespace swift;

SerializedSILLoader::SerializedSILLoader(ASTContext &Ctx,
                                         SILModule *SILMod,
                                         Callback *callback) {

  // Get a list of SerializedModules from ASTContext.
  // FIXME: Iterating over LoadedModules is not a good way to do this.
  for (auto &Entry : Ctx.LoadedModules) {
    for (auto File : Entry.second->getFiles()) {
      if (auto LoadedAST = dyn_cast<SerializedASTFile>(File)) {
        auto Des = new SILDeserializer(&LoadedAST->File, *SILMod, callback);
#ifndef NDEBUG
        SILMod->verify();
#endif
        LoadedSILSections.emplace_back(Des);
      }
    }
  }
}

SerializedSILLoader::~SerializedSILLoader() {}

SILFunction *SerializedSILLoader::lookupSILFunction(SILFunction *Callee) {
  // It is possible that one module has a declaration of a SILFunction, while
  // another has the full definition.
  SILFunction *retVal = nullptr;
  for (auto &Des : LoadedSILSections) {
    if (auto Func = Des->lookupSILFunction(Callee)) {
      DEBUG(llvm::dbgs() << "Deserialized " << Func->getName() << " from "
            << Des->getModuleIdentifier().str() << "\n");
      if (!Func->empty())
        return Func;
      retVal = Func;
    }
  }
  return retVal;
}

SILVTable *SerializedSILLoader::lookupVTable(Identifier Name) {
  for (auto &Des : LoadedSILSections) {
    if (auto VT = Des->lookupVTable(Name))
      return VT;
  }
  return nullptr;
}

SILWitnessTable *SerializedSILLoader::lookupWitnessTable(SILWitnessTable *WT) {
  for (auto &Des : LoadedSILSections)
    if (auto wT = Des->lookupWitnessTable(WT))
      return wT;
  return nullptr;
}

void SerializedSILLoader::invalidateCaches() {
  for (auto &Des : LoadedSILSections)
    Des->invalidateFunctionCache();
}

void SerializedSILLoader::getAll() {
  for (auto &Des : LoadedSILSections)
    Des->getAll();
}

void SerializedSILLoader::getAllSILFunctions() {
  for (auto &Des : LoadedSILSections)
    Des->getAllSILFunctions();
}

/// Deserialize all VTables in all SILModules.
void SerializedSILLoader::getAllVTables() {
  for (auto &Des : LoadedSILSections)
    Des->getAllVTables();
}

/// Deserialize all WitnessTables in all SILModules.
void SerializedSILLoader::getAllWitnessTables() {
  for (auto &Des : LoadedSILSections)
    Des->getAllWitnessTables();
}

// Anchor the SerializedSILLoader v-table.
void SerializedSILLoader::Callback::_anchor() {}
