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
public:
  SerializeSILPass() {}
  void run() override {
    auto &M = *getModule();
    auto *SerializeSILAction = M.getSerializeSILAction();
    if (SerializeSILAction && SerializeSILAction->canBeExecuted()) {
      // This piece of code will be executed only once, because
      // the serialization action is an execute once action.

      // Mark all reachable functions as "anchors" so that they are not
      // removed later by the dead function elimination pass. This
      // is required, because clients may reference any of the
      // serialized functions or anything referenced from them. Therefore,
      // to avoid linker errors, the object file of the current module should
      // contain all the symbols which were alive at the time of serialization.
      markAllReachableFunctionsAsAnchors(&M);
      DEBUG(llvm::dbgs() << "Serializing SILModule in SerializeSILPass\n");
      SerializeSILAction->run();
    }
  }
};

SILTransform *swift::createSerializeSILPass() {
  return new SerializeSILPass();
}
