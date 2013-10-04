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
    if (SerializedModule *LM = dyn_cast<SerializedModule>(CtxM.getValue())) {
      auto Des = new SILDeserializer(&LM->File, *SILMod, Ctx);
      LoadedSILSections.emplace_back(std::unique_ptr<SILDeserializer>(Des));
    }
  }
} 

SerializedSILLoader::~SerializedSILLoader() = default;

SILFunction *SerializedSILLoader::lookupSILFunction(SILFunction *Callee) {
  for (auto &Des : LoadedSILSections) {
    if (auto Func = Des->lookupSILFunction(Callee))
      return Func;
  }
  return nullptr;
}
