//===--- SILCloner.h - Defines the SILCloner class --------------*- C++ -*-===//
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
//
// This file defines the SILCloner class, used for cloning SIL instructions.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunctionBuilder.h"

namespace swift {

std::unique_ptr<SILModule> cloneModule(SILModule &originalModule) {
  // This implementation is based on llvm::CloneModule which can be found here:
  // https://llvm.org/doxygen/namespacellvm.html#ab371d6b308eb9772bdec63cf7a041407

  assert((originalModule.getStage() != SILStage::Canonical ||
          originalModule.getStage() != SILStage::Lowered) &&
         "cloneModule doesn't support SILStage::Lowered.");

  // Create a new module to copy the contents of \p originalModule into.
  llvm::PointerUnion<FileUnit *, ModuleDecl *> context{};
  context = (ModuleDecl *)originalModule.getSwiftModule();
  auto newModule = SILModule::createEmptyModule(context, originalModule.Types,
                                                originalModule.getOptions());

  // initialize the new \c SILGlobalVariable s
  for (const auto &originalGlobalVar : originalModule.getSILGlobals()) {
    SILGlobalVariable::create(
        *newModule, originalGlobalVar.getLinkage(),
        originalGlobalVar.isSerialized(), originalGlobalVar.getName(),
        originalGlobalVar.getLoweredType(), originalGlobalVar.getLocation(),
        originalGlobalVar.getDecl());
  }

  {
    // initialize the new \c SILFunction s
    SILFunctionBuilder functionBuilder{*newModule};
    for (const auto &originalFunction : originalModule) {
      // copy \p originalFunction into \p newModule
      functionBuilder.createFunction(
          originalFunction.getLinkage(), originalFunction.getName(),
          originalFunction.getLoweredFunctionType(),
          originalFunction.getGenericEnvironment(),
          originalFunction.getLocation(), originalFunction.isBare(),
          originalFunction.isTransparent(), originalFunction.isSerialized(),
          originalFunction.isDynamicallyReplaceable(),
          originalFunction.getEntryCount(), originalFunction.isThunk(),
          originalFunction.getClassSubclassScope(),
          originalFunction.getInlineStrategy(),
          originalFunction.getEffectsKind(),
          /*insertBefore=*/nullptr, originalFunction.getDebugScope());
    }
  }

  // copy the \c SILGlobalVariable initializers
  // Is this even needed? I'm unsure how to generate a program that uses
  // a global variables static initalizer. I would assume `let x = 1` would
  // do the trick, but it doesn't look like it does.
  // https://github.com/apple/swift/blob/main/docs/OwnershipManifesto.md#non-copyable-types
  // https://forums.swift.org/t/initializers-of-global-variables/34637
  {
    auto originalGlobal = originalModule.sil_global_begin();
    auto newGlobal = newModule->sil_global_begin();
    for (; originalGlobal != originalModule.sil_global_end() &&
           newGlobal != newModule->sil_global_end();
         ++originalGlobal, ++newGlobal) {
      assert(originalGlobal->begin() == originalGlobal->end() &&
             "Global static initalizer lists are not supported.");
    }
    assert(originalGlobal == originalModule.sil_global_end() &&
           newGlobal == newModule->sil_global_end() &&
           "`originalModule` and `newModule` sil global iterators are not of "
           "the same legnth.");
  }

  // copy the bodies of the \c SILFunction s
  {
    auto originalFunction = originalModule.begin();
    auto newFunction = newModule->begin();
    for (; originalFunction != originalModule.end() &&
           newFunction != newModule->end();
         ++originalFunction, ++newFunction) {
      if (originalFunction->begin() != originalFunction->end()) {
        SILFunctionCloner funcCloner(&*newFunction);
        funcCloner.cloneFunction(&*originalFunction);
      }
    }
    assert(originalFunction == originalModule.end() &&
           newFunction == newModule->end() &&
           "`originalModule` and `newModule` function iterators are not of the "
           "same legnth.");
  }

  return newModule;
}

} // namespace swift