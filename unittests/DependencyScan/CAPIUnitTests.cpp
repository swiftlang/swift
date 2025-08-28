//===---------------------- CAPIUnitTests.cpp -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift-c/DependencyScan/DependencyScan.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/LLVM.h"
#include "swift/DependencyScan/StringUtils.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

using namespace swift;

static std::string createFilename(StringRef base, StringRef name) {
  SmallString<256> path = base;
  llvm::sys::path::append(path, name);
  return llvm::Twine(path).str();
}

static bool emitFileWithContents(StringRef path, StringRef contents,
                                 std::string *pathOut = nullptr) {
  int FD;
  if (llvm::sys::fs::openFileForWrite(path, FD))
    return true;
  if (pathOut)
    *pathOut = path.str();
  llvm::raw_fd_ostream file(FD, /*shouldClose=*/true);
  file << contents;
  return false;
}

static bool emitFileWithContents(StringRef base, StringRef name,
                                 StringRef contents,
                                 std::string *pathOut = nullptr) {
  return emitFileWithContents(createFilename(base, name), contents, pathOut);
}

TEST(CAPIUnitTests, ClangScannerFSCacheOutOfDate) {
  SmallString<256> tempDir;
  ASSERT_FALSE(
      llvm::sys::fs::createUniqueDirectory("ScanTest.TestCAPI", tempDir));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(tempDir); };

  // Create test input file
  std::string TestPathStr = createFilename(tempDir, "foo.swift");
  ASSERT_FALSE(emitFileWithContents(tempDir, "foo.swift", "import A\n"));

  // Create includes
  std::string IncludeDirPath = createFilename(tempDir, "include");
  ASSERT_FALSE(llvm::sys::fs::create_directory(IncludeDirPath));
  std::string CHeadersDirPath = createFilename(IncludeDirPath, "CHeaders");
  ASSERT_FALSE(llvm::sys::fs::create_directory(CHeadersDirPath));
  std::string MissingDirPath = createFilename(IncludeDirPath, "Missing.dir");

  // Emit C headers and modulemap
  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "A.h", "void funcA(void);"));
  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "module.modulemap", "module A {\n\
header \"A.h\"\n\
export *\n\
}"));

  // Build command line arguments.
  std::vector<std::string> CommandStrArr = {
      TestPathStr, std::string("-I ") + CHeadersDirPath,
      std::string("-I ") + MissingDirPath};

  for (size_t i = 0; i < CommandStrArr.size(); ++i) {
    std::replace(CommandStrArr[i].begin(), CommandStrArr[i].end(), '\\', '/');
  }

  std::vector<const char *> Command;
  for (auto &command : CommandStrArr) {
    Command.push_back(command.c_str());
  }

  // Start scanning.
  auto scanner = swiftscan_scanner_create();
  auto invocation = swiftscan_scan_invocation_create();

  swiftscan_scan_invocation_set_working_directory(invocation, tempDir.c_str());
  swiftscan_scan_invocation_set_argv(invocation, Command.size(),
                                     Command.data());
  auto graph = swiftscan_dependency_graph_create(scanner, invocation);
  ASSERT_TRUE(graph);

  auto deps = swiftscan_dependency_graph_get_dependencies(graph);
  ASSERT_TRUE(deps);

  // Changing files that are cached on the file system.
  ASSERT_FALSE(
      emitFileWithContents(CHeadersDirPath, "module.modulemap", "module A {\n\
header \"A.h\"\n\
export *\n\
}\n\
module B {\n\
header \"B.h\"\n\
export *\n\
}"));

  ASSERT_FALSE(llvm::sys::fs::create_directory(MissingDirPath));

  auto outOfDateEntrySet =
      swiftscan_scanner_get_out_of_date_file_system_entry_set(scanner);
  auto numEntries =
      swiftscan_scanner_out_of_date_file_system_entry_set_get_size(
          outOfDateEntrySet);
  EXPECT_EQ(numEntries, 2u);

  bool CheckedNegativelyCached = false;
  bool CheckedSizeChanged = false;

  for (size_t i = 0; i < numEntries; i++) {
    swiftscan_scanner_out_of_date_file_system_entry_t entry =
        swiftscan_scanner_out_of_date_file_system_entry_set_get_entry(
            outOfDateEntrySet, i);

    swiftscan_scanner_out_of_date_file_system_entry_kind kind =
        swiftscan_scanner_out_of_date_file_system_entry_get_kind(entry);

    swiftscan_string_ref_t path =
        swiftscan_scanner_out_of_date_file_system_entry_get_path(entry);

    switch (kind) {
    case NegativelyCached:
      CheckedNegativelyCached = true;
      EXPECT_STREQ(c_string_utils::get_C_string(path), MissingDirPath.c_str());
      break;
    case SizeChanged:
      CheckedSizeChanged = true;
      uint64_t cachedSize =
          swiftscan_scanner_out_of_date_file_system_entry_get_cached_size(
              entry);
      uint64_t actualSize =
          swiftscan_scanner_out_of_date_file_system_entry_get_actual_size(
              entry);
      EXPECT_EQ(cachedSize, 34u);
      EXPECT_EQ(actualSize, 69u);
      break;
    }
  }

  EXPECT_TRUE(CheckedNegativelyCached);
  EXPECT_TRUE(CheckedSizeChanged);

  swiftscan_scanner_out_of_date_file_system_entry_set_dispose(
      outOfDateEntrySet);
  swiftscan_scan_invocation_dispose(invocation);
  swiftscan_scanner_dispose(scanner);
}
