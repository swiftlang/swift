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
#include "swift/Basic/LLVM.h"
#include "swift/Option/Options.h"
#include "llvm/ADT/StringRef.h"
#include <vector>
#include <unordered_set>
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

static void
testHasOption(llvm::opt::OptTable &table, options::ID id,
              const std::unordered_set<std::string> &optionSet) {
  if (table.getOption(id).hasFlag(swift::options::FrontendOption)) {
    auto name = table.getOptionName(id);
    if (!name.empty() && name[0] != '\0') {
      auto nameStr = std::string(name);
      bool setContainsOption = optionSet.find(nameStr) != optionSet.end();
      EXPECT_EQ(setContainsOption, true) << "Missing Option: " << nameStr;
    }
  }
}

TEST_F(ScanTest, TestHasArgumentQuery) {
  std::unique_ptr<llvm::opt::OptTable> table = swift::createSwiftOptTable();
  auto supported_args_set = swiftscan_compiler_supported_arguments_query();
  std::unordered_set<std::string> optionSet;
  for (size_t i = 0; i < supported_args_set->count; ++i) {
    swiftscan_string_ref_t option = supported_args_set->strings[i];
    const char* data = static_cast<const char*>(option.data);
    optionSet.insert(std::string(data, option.length));
  }
  swiftscan_string_set_dispose(supported_args_set);
#define OPTION(...)                                                            \
  testHasOption(*table, swift::options::LLVM_MAKE_OPT_ID(__VA_ARGS__),         \
                optionSet);
#include "swift/Option/Options.inc"
#undef OPTION
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
