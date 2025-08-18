//===--- SILFunctionTest.cpp - Tests for SIL functions -------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/SIL/SILFunction.h"
#include "swiftc/SIL/SILInstruction.h"
#include "swiftc/SIL/SILType.h"
#include <gtest/gtest.h>

using namespace swiftc;

class SILFunctionTest : public ::testing::Test {
protected:
  void SetUp() override {}
};

TEST_F(SILFunctionTest, BasicFunctionCreation) {
  SILType intType("Int");
  std::vector<SILType> paramTypes = {intType, intType};
  
  SILFunction function("add", paramTypes, intType);
  
  EXPECT_EQ(function.getName(), "add");
  EXPECT_EQ(function.getParameterTypes().size(), 2u);
  EXPECT_EQ(function.getReturnType().getName(), "Int");
  EXPECT_TRUE(function.getBasicBlocks().empty());
}

TEST_F(SILFunctionTest, BasicBlockManagement) {
  SILType voidType("Void");
  SILFunction function("test", {}, voidType);
  
  auto* entryBlock = function.createBasicBlock();
  EXPECT_NE(entryBlock, nullptr);
  EXPECT_EQ(function.getBasicBlocks().size(), 1u);
  EXPECT_EQ(function.getEntryBlock(), entryBlock);
  
  auto* secondBlock = function.createBasicBlock();
  EXPECT_NE(secondBlock, nullptr);
  EXPECT_EQ(function.getBasicBlocks().size(), 2u);
  EXPECT_NE(secondBlock, entryBlock);
}

TEST_F(SILFunctionTest, FunctionTypes) {
  SILType stringType("String");
  SILType boolType("Bool");
  SILType intType("Int");
  
  std::vector<SILType> params = {stringType, intType};
  SILFunction function("complex", params, boolType);
  
  EXPECT_EQ(function.getParameterTypes()[0].getName(), "String");
  EXPECT_EQ(function.getParameterTypes()[1].getName(), "Int");
  EXPECT_EQ(function.getReturnType().getName(), "Bool");
}

TEST_F(SILFunctionTest, EmptyFunction) {
  SILType voidType("Void");
  SILFunction function("empty", {}, voidType);
  
  EXPECT_EQ(function.getName(), "empty");
  EXPECT_TRUE(function.getParameterTypes().empty());
  EXPECT_EQ(function.getReturnType().getName(), "Void");
  EXPECT_TRUE(function.getBasicBlocks().empty());
}

TEST_F(SILFunctionTest, FunctionWithManyParameters) {
  SILType intType("Int");
  std::vector<SILType> manyParams(10, intType);
  
  SILFunction function("manyParams", manyParams, intType);
  
  EXPECT_EQ(function.getParameterTypes().size(), 10u);
  for (const auto& param : function.getParameterTypes()) {
    EXPECT_EQ(param.getName(), "Int");
  }
}