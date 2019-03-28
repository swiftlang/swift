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

#include "swift/AST/TensorFlow.h"
#include "TestContext.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "llvm/Support/FormatVariadic.h"
#include "gmock/gmock.h"

using namespace swift;
using namespace swift::unittest;

class TensorTypeUtilsTest : public ::testing::Test {
protected:
  TestContext testContext;
  ASTContext& Ctx;
  Type floatType;
  BoundGenericClassType *tensorHandleType;
  Type resourceHandleType;
  Type variantHandleType;
  tf::TypeContainsTensorFlowValue tctf;

  TensorTypeUtilsTest()
      : Ctx(testContext.Ctx), floatType(createFloatType()),
        tensorHandleType(createTensorHandleType(floatType)),
        resourceHandleType(createHandleType("ResourceHandle")),
        variantHandleType(createHandleType("VariantHandle")) {}

  Type createStructWithFields(const char *name, Type fieldType,
                              int numFields = 1) {
    auto *structDecl = testContext.makeNominal<StructDecl>(name);
    structDecl->computeType();
    auto *structType = StructType::get(structDecl, Type(), Ctx);
    for (int fieldId = 0; fieldId < numFields; ++fieldId) {
      std::string fieldName = llvm::formatv("field{0}", fieldId);
      auto *varDecl = new (Ctx) VarDecl(
          /*IsStatic*/ false, VarDecl::Specifier::Var,
          /*IsCaptureList*/ false, SourceLoc(), Ctx.getIdentifier(fieldName),
          structDecl);
      varDecl->setInterfaceType(fieldType);
      structDecl->addMember(varDecl);
    }
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

  Type createHandleType(StringRef typeName) {
    auto *resourceHandleDecl = testContext.makeNominal<ClassDecl>(typeName);
    return resourceHandleDecl->getDeclaredInterfaceType();
  }
};

// Given that Type objects are not directly comparable we define our own Matcher
MATCHER_P(IsSameTypeAs, refType,
          "is same type as " + ::testing::PrintToString(refType)) {
  return arg->matches(refType, TypeMatchOptions());
}

TEST_F(TensorTypeUtilsTest, ClassifiesCorrectly) {
  EXPECT_TRUE(containsTensorFlowValue(tensorHandleType));
  EXPECT_TRUE(containsTensorFlowValue(
      createStructWithFields("StructWithTensor", tensorHandleType)));
  EXPECT_FALSE(containsTensorFlowValue(
      createStructWithFields("StructWithNoTensor", floatType)));
}

TEST_F(TensorTypeUtilsTest, WorksForRecursiveTypes) {
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

TEST_F(TensorTypeUtilsTest, IsTensorHandleWorks) {
  EXPECT_TRUE(tf::isTensorHandle(tensorHandleType));
  EXPECT_FALSE(tf::isTensorHandle(floatType));
}

TEST_F(TensorTypeUtilsTest, ExtractsNullTypeOfNonTensorHandle) {
  EXPECT_TRUE(tf::getTensorHandleElementType(floatType).isNull());
}

TEST_F(TensorTypeUtilsTest, ExtractsGenericTypeOfTensorHandle) {
  Type elementTy = tf::getTensorHandleElementType(tensorHandleType);
  EXPECT_FALSE(elementTy.isNull());
  EXPECT_THAT(elementTy, IsSameTypeAs(floatType));
}

TEST_F(TensorTypeUtilsTest, ClassifiesTensorFlowTypes) {
  EXPECT_EQ(tf::TFValueKind::TensorHandle,
            tf::classifyTensorFlowValue(tensorHandleType));

  EXPECT_EQ(tf::TFValueKind::ResourceHandle,
            tf::classifyTensorFlowValue(resourceHandleType));

  EXPECT_EQ(tf::TFValueKind::VariantHandle,
            tf::classifyTensorFlowValue(variantHandleType));

  EXPECT_EQ(tf::TFValueKind::Nope, tf::classifyTensorFlowValue(floatType));

  auto *classDecl = testContext.makeNominal<ClassDecl>("SomeClass");
  Type classType = classDecl->getDeclaredInterfaceType();
  EXPECT_EQ(tf::TFValueKind::Nope, tf::classifyTensorFlowValue(classType));
}

TEST_F(TensorTypeUtilsTest, IsOpaqueHandleWorks) {
  auto *classDecl = testContext.makeNominal<ClassDecl>("SomeClass");
  Type classType = classDecl->getDeclaredInterfaceType();
  EXPECT_TRUE(tf::isOpaqueHandle(resourceHandleType) &&
              tf::isOpaqueHandle(variantHandleType));
  EXPECT_FALSE(tf::isOpaqueHandle(classType));
}

TEST_F(TensorTypeUtilsTest, IsTensorFlowValueWorks) {
  EXPECT_TRUE(tf::isTensorFlowValue(tensorHandleType) &&
              tf::isTensorFlowValue(resourceHandleType) &&
              tf::isTensorFlowValue(variantHandleType));
  EXPECT_FALSE(tf::isTensorFlowValue(floatType));
}

TEST_F(TensorTypeUtilsTest, FlattenTensorFlowValueAggregateWorks) {
  SmallVector<Type, 4> flattenedTensorFlowTypes;

  auto *anyDecl = testContext.makeNominal<ClassDecl>("AnyHandle");
  EXPECT_FALSE(tf::flattenTensorFlowValueAggregate(
      anyDecl->getDeclaredInterfaceType(), flattenedTensorFlowTypes));
  EXPECT_THAT(flattenedTensorFlowTypes, ::testing::IsEmpty());

  EXPECT_TRUE(
      tf::flattenTensorFlowValueAggregate(floatType, flattenedTensorFlowTypes));
  EXPECT_THAT(flattenedTensorFlowTypes, ::testing::IsEmpty());

  EXPECT_TRUE(tf::flattenTensorFlowValueAggregate(tensorHandleType,
                                                  flattenedTensorFlowTypes));
  EXPECT_EQ(1ul, flattenedTensorFlowTypes.size());
  EXPECT_THAT(flattenedTensorFlowTypes,
              ::testing::Each(IsSameTypeAs(tensorHandleType)));
  flattenedTensorFlowTypes.clear();

  Type twoFieldStructType = createStructWithFields(
      "TwoFieldStruct", tensorHandleType, /*numFields*/ 2);
  EXPECT_TRUE(tf::flattenTensorFlowValueAggregate(twoFieldStructType,
                                                  flattenedTensorFlowTypes));
  EXPECT_EQ(2ul, flattenedTensorFlowTypes.size());
  EXPECT_THAT(flattenedTensorFlowTypes,
              ::testing::Each(IsSameTypeAs(tensorHandleType)));
  flattenedTensorFlowTypes.clear();

  EXPECT_TRUE(tf::flattenTensorFlowValueAggregate(
      TupleType::get({twoFieldStructType, twoFieldStructType}, testContext.Ctx),
      flattenedTensorFlowTypes));
  EXPECT_EQ(4ul, flattenedTensorFlowTypes.size());
  EXPECT_THAT(flattenedTensorFlowTypes,
              ::testing::Each(IsSameTypeAs(tensorHandleType)));
}
