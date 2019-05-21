//===--- FrontendObserverTests.cpp ------------------------------------------------===//
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
///
/// This file exists to make sure methods of FrontendObserver are not removed
/// because external clients may be using them.
///
/// Successful compilation -> successful test.
///
//===----------------------------------------------------------------------===//

#include "gtest/gtest.h"
#include "swift/SIL/SILModule.h"
#include "swift/Frontend/Frontend.h"
#include "swift/FrontendTool/FrontendTool.h"

using namespace swift;

namespace {

class TestObserver : public testing::Test, public FrontendObserver {

public:

  void parsedArgs(CompilerInvocation &invocation) override { }

  void configuredCompiler(CompilerInstance &instance) override { }

  void performedSemanticAnalysis(CompilerInstance &instance) override { }

  void performedSILGeneration(SILModule &module) override { }

  void performedSILProcessing(SILModule &module) override { }

};

TEST_F(TestObserver, MethodsExistTest) {
  // If this file compiles, then the "test" is successful.
}

} // end anonymous namespace
