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
/// This test exists to make sure methods of FrontendObserver are not removed
/// because external clients may be using them.
///
//===----------------------------------------------------------------------===//

#include "gtest/gtest.h"
#include "swift/SIL/SILModule.h"
#include "swift/Frontend/Frontend.h"
#include "swift/AST/Module.h"
#include "swift/FrontendTool/FrontendTool.h"

using namespace swift;

namespace {

class TestObserver : public testing::Test, public FrontendObserver {

public:

  // convenient fields are grabbed from the given object at each method
  // to assert that the passed object is valid and usable

  // fields to assert
  std::string parsedArgsTest = "";
  std::string configuredCompilerTest = "";
  std::string performedSemanticAnalysisTest = "";
  bool performedSILGenerationTest = true;
  bool performedSILProcessingTest = true;

  void parsedArgs(CompilerInvocation &invocation) override {
    parsedArgsTest = invocation.getModuleName();
  }

  void configuredCompiler(CompilerInstance &instance) override {
    configuredCompilerTest = instance.getMainModule()->getName().str();
  }

  void performedSemanticAnalysis(CompilerInstance &instance) override {
    performedSemanticAnalysisTest = instance.getMainModule()->getName().str();
  }

  void performedSILGeneration(SILModule &module) override {
    performedSILGenerationTest = module.isSerialized();
  }

  void performedSILProcessing(SILModule &module) override {
    performedSILProcessingTest = module.hasFunction("random");
  }

};

TEST_F(TestObserver, MethodsExistTest) {
  std::string testName = "testName";

  // set up CompilerInvocation
  CompilerInvocation TestCompilerInovcation;
  TestCompilerInovcation.setModuleName(testName);

  // set up CompilerInstance
  CompilerInstance TestCompilerInstance;
  if (TestCompilerInstance.setup(TestCompilerInovcation))
    FAIL() << "Failed to setup CompilerInstance.";
  TestCompilerInstance.performSema();
  TestCompilerInstance.setSILModule(SILModule::createEmptyModule(
      TestCompilerInstance.getMainModule(),
      TestCompilerInstance.getSILOptions()));

  // perform calls, then assert that appropriate TestObserver property has been
  // set from given object, for each method

  parsedArgs(TestCompilerInovcation);
  ASSERT_EQ(parsedArgsTest, testName);

  configuredCompiler(TestCompilerInstance);
  ASSERT_EQ(configuredCompilerTest, testName);

  performedSemanticAnalysis(TestCompilerInstance);
  ASSERT_EQ(performedSemanticAnalysisTest, testName);

  performedSILGeneration(*TestCompilerInstance.getSILModule());
  ASSERT_EQ(performedSILGenerationTest, false);

  performedSILProcessing(*TestCompilerInstance.getSILModule());
  ASSERT_EQ(performedSILProcessingTest, false);

}

} // end anonymous namespace
