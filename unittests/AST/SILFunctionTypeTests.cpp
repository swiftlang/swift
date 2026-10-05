//===--- SILFunctionTypeTests.cpp - SIL function type tests --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Types.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

TEST(SILFunctionType, COMMethod) {
  TestContext C;
  auto makeType = [&](SILFunctionTypeRepresentation representation) {
    auto extInfo =
        SILExtInfoBuilder().withRepresentation(representation).build();
    SILParameterInfo self(C.Ctx.TheRawPointerType,
                          ParameterConvention::Direct_Unowned);
    return SILFunctionType::get(nullptr, extInfo, SILCoroutineKind::None,
                                ParameterConvention::Direct_Unowned, {self}, {},
                                {}, std::nullopt, {}, {}, C.Ctx);
  };

  auto method = makeType(SILFunctionTypeRepresentation::COMMethod);
  EXPECT_TRUE(method->hasSelfParam());
  EXPECT_FALSE(method->getExtInfo().hasContext());
  EXPECT_EQ(method->getLanguage(), SILFunctionLanguage::C);
  EXPECT_FALSE(shouldStoreClangType(method->getRepresentation()));

  // Although both are foreign function pointers, the COM method's implicit
  // self parameter requires a distinct type and mangling.
  auto function = makeType(SILFunctionTypeRepresentation::CFunctionPointer);
  EXPECT_NE(method, function);
  EXPECT_FALSE(function->hasSelfParam());

  Mangle::ASTMangler mangler(C.Ctx);
  EXPECT_EQ(mangler.mangleTypeForTypeName(method), "BpIetVy_");
  EXPECT_EQ(mangler.mangleTypeForTypeName(function), "BpIetCy_");
}
