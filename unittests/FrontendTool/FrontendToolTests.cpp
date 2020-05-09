//===--- FrontendTests.cpp ------------------------------------------------===//
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

#include "gtest/gtest.h"
#include "swift/FrontendTool/FrontendTool.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

namespace {

TEST(FrontendTests, MakefileQuotingTest) {
  using namespace frontend::utils;

  llvm::SmallString<256> buffer;
  EXPECT_EQ(escapeForMake("S:\\b\\llvm", buffer), "S:\\b\\llvm");
  EXPECT_EQ(escapeForMake("S:\\b\\swift\\$sBoWV", buffer),
            "S:\\b\\swift\\$$sBoWV");
  EXPECT_EQ(escapeForMake("S:\\b\\swift\\#hashtag", buffer),
            "S:\\b\\swift\\\\#hashtag");

}

} // end anonymous namespace

