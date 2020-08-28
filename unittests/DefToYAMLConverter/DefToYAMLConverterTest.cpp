//===--- DefToYAMLConverterTest.cpp --------------------------------------===//
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

#include "swift/Localization/LocalizationFormat.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::diag;

static constexpr const char *const diagnosticMessages[] = {
#define DIAG(KIND, ID, Options, Text, Signature) Text,
#include "swift/AST/DiagnosticsAll.def"
};

static std::string getDefaultLocalizationPath() {
  std::string libPath = llvm::sys::path::parent_path(SWIFTLIB_DIR);
  llvm::SmallString<128> DefaultDiagnosticMessagesDir(libPath);
  llvm::sys::path::remove_filename(DefaultDiagnosticMessagesDir); // Remove /lib
  llvm::sys::path::remove_filename(DefaultDiagnosticMessagesDir); // Remove /.
  llvm::sys::path::append(DefaultDiagnosticMessagesDir, "share", "swift",
                          "diagnostics");
  return std::string(DefaultDiagnosticMessagesDir.str());
}

TEST(DefToYAMLConverterTest, missingLocalizationFiles) {
  ASSERT_TRUE(llvm::sys::fs::exists(getDefaultLocalizationPath()));
  llvm::SmallString<128> EnglishLocalization(getDefaultLocalizationPath());
  llvm::sys::path::append(EnglishLocalization, "en");
  llvm::sys::path::replace_extension(EnglishLocalization, ".yaml");
  ASSERT_TRUE(llvm::sys::fs::exists(EnglishLocalization));
  llvm::sys::path::replace_extension(EnglishLocalization, ".db");
  ASSERT_TRUE(llvm::sys::fs::exists(EnglishLocalization));
}

TEST(DefToYAMLConverterTest, matchDiagnosticMessages) {
  llvm::SmallString<128> EnglishLocalization(getDefaultLocalizationPath());
  llvm::sys::path::append(EnglishLocalization, "en");
  llvm::sys::path::replace_extension(EnglishLocalization, ".yaml");
  YAMLLocalizationProducer yaml(EnglishLocalization.str());

  yaml.forEachAvailable([](swift::DiagID id, llvm::StringRef translation) {
    ASSERT_TRUE(diagnosticMessages[static_cast<uint32_t>(id)] == translation);
  });
}
