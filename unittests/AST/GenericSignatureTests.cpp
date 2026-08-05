//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Types.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

TEST(GenericSignature, MetatypeRequirementQueries) {
  TestContext C;
  ASTContext &Context = C.Ctx;

  ProtocolDecl *protocol = C.makeProtocol("P");
  GenericTypeParamType *T = GenericTypeParamType::getType(0, 0, Context);
  Type metatype = MetatypeType::get(T);

  auto signature =
      buildGenericSignature(Context, nullptr, {T},
                            {Requirement(RequirementKind::Conformance, metatype,
                                         protocol->getDeclaredInterfaceType())},
                            DefaultRequirementOptions());

  EXPECT_TRUE(signature->requiresProtocol(metatype, protocol));

  auto protocols = signature->getRequiredProtocols(metatype);
  ASSERT_EQ(protocols.size(), 1u);
  ASSERT_EQ(protocols.front(), protocol);

  auto requirements = signature->getLocalRequirements(metatype);
  ASSERT_EQ(requirements.protos.size(), 1u);
  EXPECT_EQ(requirements.protos.front(), protocol);
  EXPECT_FALSE(requirements.superclass);
  EXPECT_FALSE(requirements.layout);
  EXPECT_FALSE(requirements.packShape);
}
