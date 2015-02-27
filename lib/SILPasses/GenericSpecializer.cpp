//===-- Specializer.cpp ------ Performs Generic Specialization ------------===//
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

#define DEBUG_TYPE "specialization"

#include "swift/SILPasses/Utils/Generics.h"
#include "swift/SILPasses/Passes.h"

#include "swift/AST/ASTContext.h"

#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/SmallString.h"
using namespace swift;

namespace {
class SILGenericSpecializerTransform : public SILModuleTransform {
public:
  SILGenericSpecializerTransform() {}

  void run() override {
    CallGraphAnalysis* CGA = PM->getAnalysis<CallGraphAnalysis>();

    // Collect a call-graph bottom-up list of functions and specialize the
    // functions in reverse order.
    auto &CG = CGA->getCallGraph();
    bool Changed = GenericSpecializer(getModule()).
      specialize(CG.getBottomUpFunctionOrder());

    if (Changed) {
      // Schedule another iteration of the transformation pipe.
      PM->scheduleAnotherIteration();

      // Invalidate the call graph.
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
    }
  }

  StringRef getName() override { return "Generic Specialization"; }
};
} // end anonymous namespace


SILTransform *swift::createGenericSpecializer() {
  return new SILGenericSpecializerTransform();
}
