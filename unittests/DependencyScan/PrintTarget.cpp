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
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Path.h"
#include "gtest/gtest.h"
#include <string>
#include <vector>

using namespace swift;
using namespace swift::unittest;

TEST_F(ScanTest, TestTargetInfoQuery) {
  std::vector<std::string> CommandStrArr = {
      std::string("-print-target-info"), std::string("-target"),
      std::string("x86_64-apple-macosx12.0")};
  // On Windows we need to add an extra escape for path separator characters because otherwise
  // the command line tokenizer will treat them as escape characters.
  for (size_t i = 0; i < CommandStrArr.size(); ++i) {
    std::replace(CommandStrArr[i].begin(), CommandStrArr[i].end(), '\\', '/');
  }
  std::vector<const char *> Compilation;
  for (auto &str : CommandStrArr)
    Compilation.push_back(str.c_str());

  llvm::SmallString<128> pathRoot("base");
  llvm::SmallString<128> compilerPath(pathRoot);
  llvm::sys::path::append(compilerPath, "foo", "bar", "bin", "swift-frontend");
  llvm::SmallString<128> relativeLibPath(pathRoot);
  llvm::sys::path::append(relativeLibPath, "foo", "bar", "lib", "swift");

  auto invocation = swiftscan_scan_invocation_create();
  swiftscan_scan_invocation_set_working_directory(invocation, "");
  swiftscan_scan_invocation_set_argv(invocation,
                                     static_cast<int>(Compilation.size()),
                                     (const char **)Compilation.data());
  auto targetInfo =
      swiftscan_compiler_target_info_query_v2(invocation, compilerPath.c_str());
  swiftscan_scan_invocation_dispose(invocation);

  auto targetInfoStr = std::string(static_cast<const char *>(targetInfo.data),
                                   targetInfo.length);
  ASSERT_FALSE(targetInfoStr.empty());

  EXPECT_NE(targetInfoStr.find("\"triple\": \"x86_64-apple-macosx12.0\""), std::string::npos);
  EXPECT_NE(targetInfoStr.find("\"librariesRequireRPath\": true"), std::string::npos);

  std::string expectedRuntimeResourcePath = "\"runtimeResourcePath\": \"" + relativeLibPath.str().str() + "\"";
  // On windows, need to normalize the path back to "\\" separators
  size_t start_pos = 0;
  while((start_pos = expectedRuntimeResourcePath.find("\\", start_pos)) != std::string::npos) {
    expectedRuntimeResourcePath.replace(start_pos, 1, "\\\\");
    start_pos += 2;
  }
  llvm::dbgs() << "Expected Runtime Resource Path: \n" << expectedRuntimeResourcePath << "\n";
  llvm::dbgs() << "Result Target Info: \n" << targetInfoStr << "\n";
  EXPECT_NE(targetInfoStr.find(expectedRuntimeResourcePath.c_str()), std::string::npos);
}
