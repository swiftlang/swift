//===--- LocalizationTest.h - Helper for setting up locale tests -*- C++-*-===//
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

#ifndef LOCALIZATION_TEST_H
#define LOCALIZATION_TEST_H

#include "swift/Localization/LocalizationFormat.h"
#include "swift/Basic/Compiler.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"
#include <random>
#include <string>
#include <system_error>

using namespace swift::diag;

namespace swift {
namespace unittests {

enum LocalDiagID : uint32_t {
#define DIAG(KIND, ID, Group, Options, Text, Signature) ID,
#include "swift/AST/DiagnosticsAll.def"
  NumDiags
};

static constexpr const char *const diagnosticID[] = {
#define DIAG(KIND, ID, Group, Options, Text, Signature) #ID,
#include "swift/AST/DiagnosticsAll.def"
};

static constexpr const char *const diagnosticMessages[] = {
#define DIAG(KIND, ID, Group, Options, Text, Signature) Text,
#include "swift/AST/DiagnosticsAll.def"
};

struct LocalizationTest : public ::testing::Test {
  llvm::SmallVector<std::string, 4> TempFiles;

public:
  std::string DiagsPath;

  LocalizationTest() {
    DiagsPath = std::string(createTemporaryFile("en", "strings"));
  }

  void SetUp() override {
    bool failed = convertDefIntoStrings(DiagsPath);
    assert(!failed && "failed to generate a `.strings` file");
  }

  void TearDown() override {
    for (auto &tmp : TempFiles)
      llvm::sys::fs::remove(tmp);
  }

  std::string createTemporaryFile(std::string prefix, std::string suffix) {
    llvm::SmallString<128> tempFile;
    std::error_code error =
        llvm::sys::fs::createTemporaryFile(prefix, suffix, tempFile);
    assert(!error);
    // Can't use llvm::sys::RemoveFileOnSignal(tempFile) because
    // signals are not available on Windows.
    auto tmp = std::string(tempFile);
    TempFiles.push_back(tmp);
    return tmp;
  }

  /// Random number in [0,n)
  unsigned RandNumber(unsigned n) { return unsigned(rand()) % n; }

protected:
  static bool convertDefIntoStrings(std::string outputPath) {
    std::error_code error;
    llvm::raw_fd_ostream OS(outputPath, error, llvm::sys::fs::OF_None);
    if (OS.has_error() || error)
      return true;

    llvm::ArrayRef<const char *> ids(diagnosticID, LocalDiagID::NumDiags);
    llvm::ArrayRef<const char *> messages(diagnosticMessages,
                                          LocalDiagID::NumDiags);

    DefToStringsConverter converter(ids, messages);
    converter.convert(OS);

    OS.flush();

    return OS.has_error();
  }
};

} // end namespace unittests
} // end namespace swift

#endif
