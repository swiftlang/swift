//===--- InMemoryFrontend.cpp - Frontend operations, in memory --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "libInMemoryFrontend/InMemoryFrontend.h"

#include "swift/SIL/SILModule.h"
#include "swift/Subsystems.h"

namespace swift {
namespace inmemoryfrontend {

bool compileSwiftModule(CompilerInstance &CI,
                        std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
                        std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer) {
  CI.performSema();
  if (CI.getDiags().hadAnyError())
    return true;

  auto SILMod = performSILGeneration(CI.getMainModule(), CI.getSILTypes(),
                                     CI.getSILOptions());
  if (!SILMod)
    return true;

  SerializationOptions SerOpts;
  SILMod->setSerializeSILAction([&]() {
    serializeToMemory(CI.getMainModule(), SerOpts, moduleBuffer,
                      moduleDocBuffer, SILMod.get());
  });

  if (CI.performSILProcessing(SILMod.get()))
    return true;

  return false;
}

} // end namespace inmemoryfrontend
} // end namespace swift
