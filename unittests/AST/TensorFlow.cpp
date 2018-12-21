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

TEST(TensorFlow, ContainsTensorValueWorks) {
  TestContext C;
  auto *structDecl = C.makeNominal<StructDecl>("PairedStructure");
  structDecl->dump();
  auto *structType = StructType::get(structDecl, Type(), C.Ctx);
  structType->dump();
  auto *structType2 = StructType::get(structDecl, structType, C.Ctx);
  structType2->dump();
  // auto* blah = ArraySliceType::get(structType);
  // blah->dump();
  // auto *arrayDecl = C.makeNominal<StructDecl>("Array", Type(), {structDecl});
  // arrayDecl->dump();
  // auto
  EXPECT_TRUE(1 == 1);
}
