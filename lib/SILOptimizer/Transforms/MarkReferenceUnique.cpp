//===---- MarkReferenceUnique.cpp - Mark references as unique -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {

//===----------------------------------------------------------------------===//
//                            MarkReferenceUnique
//===----------------------------------------------------------------------===//

struct MarkReferenceUnique : public SILFunctionTransform {
  MarkReferenceUnique() = default;

  /// The entry point to the transformation.
  void run() override {
    auto *func = getFunction();

    bool madeChange = false;
    for (auto &block : *func) {
      for (auto &inst : block) {
        if (auto *ref = dyn_cast<AllocRefInst>(&inst)) {
          // No point in doing exra work.
          if (ref->isUniqueReference())
            continue;

          // The result of isReferenceUnique is an error message. If there's no
          // message, then this is a unique reference.
          bool isUnique =
              isReferenceUnique(ref, /*verbose*/ false).hasValue() == false;
          if (isUnique) {
            madeChange = true;
            ref->setUniqueReference();
          }
        }
      }
    }

    if (madeChange)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace

SILTransform *swift::createMarkReferenceUnique() {
  return new MarkReferenceUnique();
}
