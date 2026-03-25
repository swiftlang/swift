//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ScanFixture.h"
#include "swift-c/DependencyScan/DependencyScan.h"
#include <string>
#include <unordered_set>
#include "gtest/gtest.h"

using namespace swift::unittest;

TEST_F(ScanTest, TestHasArgumentQuery) {
  auto supported_args_set = swiftscan_compiler_supported_arguments_query();
  ASSERT_TRUE(supported_args_set);
  ASSERT_GT(supported_args_set->count, static_cast<size_t>(0));

  std::unordered_set<std::string> optionSet;
  for (size_t i = 0; i < supported_args_set->count; ++i) {
    swiftscan_string_ref_t option = supported_args_set->strings[i];
    const char* data = static_cast<const char*>(option.data);
    optionSet.insert(std::string(data, option.length));
  }
  swiftscan_string_set_dispose(supported_args_set);

  // Verify well-known frontend options are present
  EXPECT_TRUE(optionSet.count("target")) << "Missing: target";
  EXPECT_TRUE(optionSet.count("sdk")) << "Missing: sdk";
  EXPECT_TRUE(optionSet.count("I")) << "Missing: I";
  EXPECT_TRUE(optionSet.count("module-name")) << "Missing: module-name";
  EXPECT_TRUE(optionSet.count("emit-module")) << "Missing: emit-module";
  EXPECT_TRUE(optionSet.count("emit-object")) << "Missing: emit-object";
  EXPECT_TRUE(optionSet.count("parse")) << "Missing: parse";
  EXPECT_TRUE(optionSet.count("typecheck")) << "Missing: typecheck";
  EXPECT_TRUE(optionSet.count("O")) << "Missing: O";
  EXPECT_TRUE(optionSet.count("g")) << "Missing: g";
}

TEST_F(ScanTest, TestDoesNotHaveArgumentQuery) {
  auto supported_args_set = swiftscan_compiler_supported_arguments_query();
  std::unordered_set<std::string> optionSet;
  for (size_t i = 0; i < supported_args_set->count; ++i) {
    swiftscan_string_ref_t option = supported_args_set->strings[i];
    const char* data = static_cast<const char*>(option.data);
    optionSet.insert(std::string(data, option.length));
  }
  swiftscan_string_set_dispose(supported_args_set);
  bool hasOption;
  hasOption = optionSet.find("-clearly-not-a-compiler-flag") != optionSet.end();
  EXPECT_EQ(hasOption, false);
  hasOption = optionSet.find("-emit-modul") != optionSet.end();
  EXPECT_EQ(hasOption, false);
}
