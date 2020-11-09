//===--- DefToYAMLConverterTests.cpp -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "LocalizationTest.h"
#include "swift/Localization/LocalizationFormat.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"
#include <cstdlib>
#include <random>
#include <string>
#include <system_error>

using namespace swift;
using namespace swift::diag;
using namespace swift::unittests;

static std::string getMainExecutablePath() {
  llvm::StringRef libPath = llvm::sys::path::parent_path(SWIFTLIB_DIR);
  llvm::SmallString<128> MainExecutablePath(libPath);
  llvm::sys::path::remove_filename(MainExecutablePath); // Remove /lib
  llvm::sys::path::remove_filename(MainExecutablePath); // Remove /.
  return std::string(MainExecutablePath);
}

static std::string getDefaultLocalizationPath() {
  llvm::SmallString<128> DefaultDiagnosticMessagesDir(getMainExecutablePath());
  llvm::sys::path::append(DefaultDiagnosticMessagesDir, "share", "swift",
                          "diagnostics");
  return std::string(DefaultDiagnosticMessagesDir);
}

TEST_F(LocalizationTest, MissingLocalizationFiles) {
  ASSERT_TRUE(llvm::sys::fs::exists(getDefaultLocalizationPath()));
  llvm::SmallString<128> EnglishLocalization(getDefaultLocalizationPath());
  llvm::sys::path::append(EnglishLocalization, "en");
  llvm::sys::path::replace_extension(EnglishLocalization, ".yaml");
  ASSERT_TRUE(llvm::sys::fs::exists(EnglishLocalization));
  llvm::sys::path::replace_extension(EnglishLocalization, ".db");
  ASSERT_TRUE(llvm::sys::fs::exists(EnglishLocalization));
}

TEST_F(LocalizationTest, ConverterTestMatchDiagnosticMessagesSequentially) {
  YAMLLocalizationProducer yaml(YAMLPath);
  yaml.forEachAvailable([](swift::DiagID id, llvm::StringRef translation) {
    llvm::StringRef msg = diagnosticMessages[static_cast<uint32_t>(id)];
    ASSERT_EQ(msg, translation);
  });
}

TEST_F(LocalizationTest, ConverterTestMatchDiagnosticMessagesRandomly) {
  YAMLLocalizationProducer yaml(YAMLPath);

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> distr(50, LocalDiagID::NumDiags);
  unsigned numberOfQueries = distr(gen);
  while (numberOfQueries--) {
    unsigned randomNum = RandNumber(LocalDiagID::NumDiags);
    DiagID randomId = static_cast<DiagID>(randomNum);
    llvm::StringRef msg = diagnosticMessages[randomNum];
    llvm::StringRef translation = yaml.getMessageOr(randomId, "");
    ASSERT_EQ(msg, translation);
  }
}
