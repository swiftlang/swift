//===--- TypeDifferenceVisitorTests.cpp -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/TypeDifferenceVisitor.h"
#include "swift/AST/Types.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

namespace {
struct DifferenceChecker : CanTypeDifferenceVisitor<DifferenceChecker> {};
}

TEST(TypeDifferenceVisitor, NominalParentTypes) {
  TestContext C;

  auto *outerStruct = C.makeNominal<StructDecl>("Outer");
  auto *innerStruct = C.makeNominal<StructDecl>("Inner");

  Type outerInt = NominalType::get(outerStruct, Type(),
                                   {C.Ctx.TheRawPointerType}, C.Ctx);
  Type outerBool = NominalType::get(outerStruct, Type(),
                                    {C.Ctx.TheEmptyTupleType}, C.Ctx);

  Type innerInt1 = NominalType::get(innerStruct, outerInt, C.Ctx);
  Type innerInt2 = NominalType::get(innerStruct, outerInt, C.Ctx);
  Type innerBool = NominalType::get(innerStruct, outerBool, C.Ctx);
  Type innerNoParent = NominalType::get(innerStruct, Type(), C.Ctx);

  DifferenceChecker checker;

  // Identical types have no difference.
  EXPECT_FALSE(checker.visit(innerInt1->getCanonicalType(),
                             innerInt2->getCanonicalType()));

  // Differing parent types must be detected as different.
  EXPECT_TRUE(checker.visit(innerInt1->getCanonicalType(),
                            innerBool->getCanonicalType()));

  // Differing presence of parent type must be detected as different.
  EXPECT_TRUE(checker.visit(innerInt1->getCanonicalType(),
                            innerNoParent->getCanonicalType()));
  EXPECT_TRUE(checker.visit(innerNoParent->getCanonicalType(),
                            innerInt1->getCanonicalType()));
}
