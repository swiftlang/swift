//===--- IsSelfRecursiveAnalysis.cpp --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Analysis/IsSelfRecursiveAnalysis.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

// Force the compiler to generate the destructor in this C++ file.
// Otherwise it can happen that it is generated in a SwiftCompilerSources module
// and that results in unresolved-symbols linker errors.
IsSelfRecursive::~IsSelfRecursive() = default;

void IsSelfRecursive::compute() {
  isSelfRecursive = false;

  for (auto &BB : *getFunction()) {
    for (auto &I : BB) {
      if (auto Apply = FullApplySite::isa(&I)) {
        if (Apply.getReferencedFunctionOrNull() == f) {
          isSelfRecursive = true;
          return;
        }
      }
    }
  }
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

SILAnalysis *swift::createIsSelfRecursiveAnalysis(SILModule *) {
  return new IsSelfRecursiveAnalysis();
}
