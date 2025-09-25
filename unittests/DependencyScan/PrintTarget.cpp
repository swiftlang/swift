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
#include "swift/DependencyScan/StringUtils.h"
#include "swift/Basic/LLVM.h"
#include "swift/Option/Options.h"
#include "llvm/ADT/StringRef.h"
#include <vector>
#include <unordered_set>
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

TEST_F(ScanTest, TestTargetInfoQuery) {
  std::vector<std::string> CommandStrArr = {
    std::string("-print-target-info"),
    std::string("-target"), std::string("x86_64-apple-macosx12.0")};
  // On Windows we need to add an extra escape for path separator characters because otherwise
  // the command line tokenizer will treat them as escape characters.
  for (size_t i = 0; i < CommandStrArr.size(); ++i) {
    std::replace(CommandStrArr[i].begin(), CommandStrArr[i].end(), '\\', '/');
  }
  std::vector<const char *> Compilation;
  for (auto &str : CommandStrArr)
    Compilation.push_back(str.c_str());

  SmallString<128> pathRoot("base");
  SmallString<128> compilerPath(pathRoot);
  llvm::sys::path::append(compilerPath, "foo", "bar", "bin", "swift-frontend");
  SmallString<128> relativeLibPath(pathRoot);
  llvm::sys::path::append(relativeLibPath, "foo", "bar", "lib", "swift");;

  auto targetInfo = swift::dependencies::getTargetInfo(Compilation, compilerPath.c_str());
  if (targetInfo.getError()) {
    llvm::errs() << "For compilation: ";
    for (auto &str : Compilation)
      llvm::errs() << str << " ";
    llvm::errs() << "\nERROR:" << targetInfo.getError().message() << "\n";
  }

  auto targetInfoStr = std::string(swift::c_string_utils::get_C_string(targetInfo.get()));
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
