//===--- SILAutoDiffIndices.cpp - Tests SILAutoDiffIndices ----------------===//
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
// SWIFT_ENABLE_TENSORFLOW

#include "TestContext.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/AST/TensorFlow.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

class TypeContainsTensorFlowValueTest : public ::testing::Test {
protected:
  TestContext testContext;
  ASTContext& Ctx;
  Type floatType;
  BoundGenericClassType *tensorHandleType;
  tf::TypeContainsTensorFlowValue tctf;

  TypeContainsTensorFlowValueTest()
      : Ctx(testContext.Ctx), floatType(createFloatType()),
        tensorHandleType(createTensorHandleType(floatType)) {}

  Type createStructWithField(const char *name, Type fieldType) {
    auto *structDecl = testContext.makeNominal<StructDecl>(name);
    structDecl->computeType();
    auto *structType = StructType::get(structDecl, Type(), Ctx);
    auto *varDecl = new (Ctx) VarDecl(
        /*IsStatic*/ false, VarDecl::Specifier::Var,
        /*IsCaptureList*/ false, SourceLoc(), Ctx.getIdentifier("field"),
        structDecl);
    varDecl->setInterfaceType(fieldType);
    structDecl->addMember(varDecl);
    return structType;
  }

  bool containsTensorFlowValue(Type t) {
    // TODO: Add tests with CheckHigherOrderFunctions set to true.
    return tctf.containsTensorFlowValue(t, /*CheckHigherOrderFunctions*/false);
  }


private:
  Type createFloatType() {
    // Float type.
    auto *floatDecl = testContext.makeNominal<StructDecl>("Float");
    return StructType::get(floatDecl, Type(), Ctx);
  }

  BoundGenericClassType *createTensorHandleType(Type genericType) {
    auto *tensorDecl = testContext.makeNominal<ClassDecl>("TensorHandle");

    // Generic parameter list.
    auto *floatGenericParamDecl = new (Ctx)
        GenericTypeParamDecl(tensorDecl->getDeclContext(),
                             Ctx.getIdentifier("Scalar"), SourceLoc(), 0, 0);
    auto *paramList = GenericParamList::create(
        Ctx, SourceLoc(), {floatGenericParamDecl}, SourceLoc());
    tensorDecl->setGenericParams(paramList);

    return BoundGenericClassType::get(tensorDecl, Type(), {genericType});
  }
};

TEST_F(TypeContainsTensorFlowValueTest, ClassifiesCorrectly) {
  EXPECT_TRUE(containsTensorFlowValue(tensorHandleType));
  EXPECT_TRUE(containsTensorFlowValue(
      createStructWithField("StructWithTensor", tensorHandleType)));
  EXPECT_FALSE(containsTensorFlowValue(
      createStructWithField("StructWithNoTensor", floatType)));
}

TEST_F(TypeContainsTensorFlowValueTest, WorksForRecursiveTypes) {
  // Creates a recursive type for testing purposes. Note that this is not a
  // valid swift type, but should suffice for the purposes of this unit test.
  //
  auto *recursiveDecl = testContext.makeNominal<StructDecl>("RecursiveStruct");
  recursiveDecl->computeType();
  auto *recursiveType = StructType::get(recursiveDecl, Type(), Ctx);
  // Add a field of the recursiveType.
  // (This is not possible in swift, but ok for tests.)
  auto *varDecl = new (Ctx) VarDecl(
      /*IsStatic*/ false, VarDecl::Specifier::Var,
      /*IsCaptureList*/ false, SourceLoc(), Ctx.getIdentifier("someField"),
      recursiveDecl);
  varDecl->setInterfaceType(recursiveType);
  recursiveDecl->addMember(varDecl);
  EXPECT_FALSE(containsTensorFlowValue(recursiveType));
}
