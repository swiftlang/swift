//===--- ClangImporterOptionsTest.cpp -------------------------------------===//
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
#include "swift/Basic/LangOptions.h"
#include "llvm/ADT/StringRef.h"
#include "gtest/gtest.h"

static std::string remap(llvm::StringRef path) { return "remapped"; }

TEST(ClangImporterOptions, nonPathsSkipped) {
  std::vector<std::string> args = {"-unmapped", "-another=unmapped"};
  swift::ClangImporterOptions options;
  options.ExtraArgs = args;

  EXPECT_EQ(options.getRemappedExtraArgs(remap), args);
}

TEST(ClangImporterOptions, optionPairs) {
  std::vector<std::string> args = {"-unmapped",    "-another=unmapped",
                                   "-I",           "some/path",
                                   "-ivfsoverlay", "another/path"};
  swift::ClangImporterOptions options;
  options.ExtraArgs = args;

  std::vector<std::string> expected = {"-unmapped",    "-another=unmapped",
                                       "-I",           "remapped",
                                       "-ivfsoverlay", "remapped"};
  EXPECT_EQ(options.getRemappedExtraArgs(remap), expected);
}

TEST(ClangImporterOptions, joinedPaths) {
  std::vector<std::string> args = {"-unmapped", "-another=unmapped",
                                   "-Isome/path",
                                   "-working-directory=another/path"};
  swift::ClangImporterOptions options;
  options.ExtraArgs = args;

  std::vector<std::string> expected = {"-unmapped", "-another=unmapped",
                                       "-Iremapped",
                                       "-working-directory=remapped"};
  EXPECT_EQ(options.getRemappedExtraArgs(remap), expected);
}
