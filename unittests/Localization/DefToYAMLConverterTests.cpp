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

#include "swift/Localization/LocalizationFormat.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"
#include <cstdlib>
#include <random>
#include <string>
#include <system_error>

using namespace swift;
using namespace swift::diag;

enum LocalDiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) ID,
#include "swift/AST/DiagnosticsAll.def"
  NumDiags
};

static constexpr const char *const diagnosticID[] = {
#define DIAG(KIND, ID, Options, Text, Signature) #ID,
#include "swift/AST/DiagnosticsAll.def"
};

static constexpr const char *const diagnosticMessages[] = {
#define DIAG(KIND, ID, Options, Text, Signature) Text,
#include "swift/AST/DiagnosticsAll.def"
};

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

struct DefToYAMLConverterTest : public ::testing::Test {
  std::string YAMLPath;

public:
  DefToYAMLConverterTest() {
    llvm::SmallString<128> tempFilename;
    std::error_code error =
        llvm::sys::fs::createTemporaryFile("en", "yaml", tempFilename);
    assert(!error);

    YAMLPath = std::string(tempFilename);
  }

  void SetUp() override {
    bool failed = convertDefIntoYAML(YAMLPath);
    assert(!failed && "failed to generate a YAML file");
  }

  void TearDown() override { llvm::sys::fs::remove(YAMLPath); }

  /// Random number in [0,n)
  unsigned RandNumber(unsigned n) { return unsigned(rand()) % n; }

protected:
  static bool convertDefIntoYAML(std::string outputPath) {
    std::error_code error;
    llvm::raw_fd_ostream OS(outputPath, error, llvm::sys::fs::F_None);
    if (OS.has_error() || error)
      return true;

    llvm::ArrayRef<const char *> ids(diagnosticID, LocalDiagID::NumDiags);
    llvm::ArrayRef<const char *> messages(diagnosticMessages,
                                          LocalDiagID::NumDiags);

    DefToYAMLConverter converter(ids, messages);
    converter.convert(OS);

    OS.flush();

    return OS.has_error();
  }
};

TEST_F(DefToYAMLConverterTest, MissingLocalizationFiles) {
  ASSERT_TRUE(llvm::sys::fs::exists(getDefaultLocalizationPath()));
  llvm::SmallString<128> EnglishLocalization(getDefaultLocalizationPath());
  llvm::sys::path::append(EnglishLocalization, "en");
  llvm::sys::path::replace_extension(EnglishLocalization, ".yaml");
  ASSERT_TRUE(llvm::sys::fs::exists(EnglishLocalization));
  llvm::sys::path::replace_extension(EnglishLocalization, ".db");
  ASSERT_TRUE(llvm::sys::fs::exists(EnglishLocalization));
}

TEST_F(DefToYAMLConverterTest, MatchDiagnosticMessagesSequentially) {
  YAMLLocalizationProducer yaml(YAMLPath);
  yaml.forEachAvailable([](swift::DiagID id, llvm::StringRef translation) {
    llvm::StringRef msg = diagnosticMessages[static_cast<uint32_t>(id)];
    ASSERT_EQ(msg, translation);
  });
}

TEST_F(DefToYAMLConverterTest, MatchDiagnosticMessagesRandomly) {
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
