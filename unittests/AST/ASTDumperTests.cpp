//===--- ASTDumperTests.cpp - Tests for ASTDumper -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"
#include <string>

using namespace swift;
using namespace swift::unittest;

TEST(ASTDumper, ArchetypeType) {
  TestContext C;
  auto &ctx = C.Ctx;

  auto sig = buildGenericSignature(ctx, nullptr, {ctx.TheSelfType}, {},
                                   /*allowInverses=*/true);

  TypeBase *archetype = nullptr;
  {
    llvm::SmallVector<ProtocolDecl *> protocols;
    archetype = PrimaryArchetypeType::getNew(ctx, sig.getGenericEnvironment(),
                                             ctx.TheSelfType, protocols, Type(),
                                             nullptr);
  }

  std::string fullStr;
  {
    llvm::raw_string_ostream os(fullStr);
    archetype->dump(os);
  }

  llvm::StringRef str(fullStr);
  EXPECT_TRUE(str.consume_front("(primary_archetype_type address=0x"));
  {
    intptr_t integer;
    EXPECT_FALSE(str.consumeInteger(16, integer));
  }

  EXPECT_EQ(str,
            " name=\"τ_0_0\"\n"
            "  (interface_type=generic_type_param_type depth=0 index=0 param_kind=type))\n");
}
