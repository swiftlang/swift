//===--- SerializeSILPass.cpp ---------------------------------------------===//
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

#define DEBUG_TYPE "serialize-sil"
#include "swift/Strings.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

/// A utility pass to serialize a SILModule at any place inside the optimization
/// pipeline.
class SerializeSILPass : public SILModuleTransform {
  /// Removes [serialized] from all functions. This allows for more
  /// optimizations and for a better dead function elimination.
  void removeSerializedFlagFromAllFunctions(SILModule &M) {
    for (auto &F : M) {
      F.setSerialized(IsNotSerialized);
    }

    for (auto &WT : M.getWitnessTables()) {
      WT.setSerialized(IsNotSerialized);
    }

    for (auto &VT : M.getVTables()) {
      VT.setSerialized(IsNotSerialized);
    }
  }

public:
  SerializeSILPass() {}
  void run() override {
    auto &M = *getModule();
    // Nothing to do if the module was serialized already.
    if (M.isSerialized())
      return;

    // Mark all reachable functions as "anchors" so that they are not
    // removed later by the dead function elimination pass. This
    // is required, because clients may reference any of the
    // serialized functions or anything referenced from them. Therefore,
    // to avoid linker errors, the object file of the current module should
    // contain all the symbols which were alive at the time of serialization.
    LLVM_DEBUG(llvm::dbgs() << "Serializing SILModule in SerializeSILPass\n");
    M.serialize();

    // If we are not optimizing, do not strip the [serialized] flag. We *could*
    // do this since after serializing [serialized] is irrelevent. But this
    // would incur an unnecessary compile time cost since if we are not
    // optimizing we are not going to perform any sort of DFE.
    if (!getOptions().shouldOptimize())
      return;
    removeSerializedFlagFromAllFunctions(M);
  }
};

SILTransform *swift::createSerializeSILPass() {
  return new SerializeSILPass();
}
