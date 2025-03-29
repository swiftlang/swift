//===--- FileSystemTest.cpp - for swift/Basic/FileSystem.h ----------------===//
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

#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "gtest/gtest.h"

#include <string>

#define ASSERT_NO_ERROR(x)                                                     \
  do if (std::error_code ASSERT_NO_ERROR_ec = x) {                             \
    llvm::errs() << #x ": did not return errc::success.\n"                     \
      << "error number: " << ASSERT_NO_ERROR_ec.value() << "\n"                \
      << "error message: " << ASSERT_NO_ERROR_ec.message() << "\n";            \
    FAIL();                                                                    \
  } while (0)

using namespace llvm::sys;
using namespace swift;

namespace {

std::string getFileContents(llvm::StringRef path) {
  auto fs = llvm::vfs::getRealFileSystem();
  auto file = fs->openFileForRead(path);
  if (!file)
    return "";

  auto buffer = (*file)->getBuffer("");
  if (!buffer)
    return "";

  return (*buffer)->getBuffer().str();
}

TEST(FileSystem, MoveFileIfDifferentEmpty) {
  // Create unique temporary directory for these tests
  llvm::SmallString<128> dirPath;
  ASSERT_NO_ERROR(fs::createUniqueDirectory("FileSystem-test", dirPath));

  llvm::SmallString<128> sourceFile = dirPath;
  path::append(sourceFile, "source.txt");

  llvm::SmallString<128> destFile = dirPath;
  path::append(destFile, "dest.txt");

  // Test 1: Move empty over nonexistent.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream emptyOut(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
    }
    ASSERT_FALSE(fs::exists(destFile));

    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, destFile));
    ASSERT_TRUE(fs::exists(destFile));
    ASSERT_FALSE(fs::exists(sourceFile));

    ASSERT_NO_ERROR(fs::remove(destFile, false));
  }

  // Test 2: Move empty over empty.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream emptySource(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);

      llvm::raw_fd_ostream emptyDest(destFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
    }

    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, destFile));
    ASSERT_TRUE(fs::exists(destFile));
    ASSERT_FALSE(fs::exists(sourceFile));

    ASSERT_NO_ERROR(fs::remove(destFile, false));
  }

  // Test 3: Move empty over non-empty.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream emptySource(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);

      llvm::raw_fd_ostream dest(destFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
      dest << "a";
    }

    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, destFile));
    ASSERT_TRUE(fs::exists(destFile));
    ASSERT_FALSE(fs::exists(sourceFile));

    ASSERT_EQ(getFileContents(destFile), "");

    ASSERT_NO_ERROR(fs::remove(destFile, false));
  }

  // Test 4: Move over self.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream emptySource(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
    }


    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, sourceFile));
    ASSERT_TRUE(fs::exists(sourceFile));

    ASSERT_NO_ERROR(fs::remove(sourceFile, false));
  }

  // Clean up.
  ASSERT_NO_ERROR(fs::remove(dirPath, false));
}

TEST(FileSystem, MoveFileIfDifferentNonEmpty) {
  // Create unique temporary directory for these tests
  llvm::SmallString<128> dirPath;
  ASSERT_NO_ERROR(fs::createUniqueDirectory("FileSystem-test", dirPath));

  llvm::SmallString<128> sourceFile = dirPath;
  path::append(sourceFile, "source.txt");

  llvm::SmallString<128> destFile = dirPath;
  path::append(destFile, "dest.txt");

  // Test 1: Move source over nonexistent.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream sourceOut(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
      sourceOut << "original";
    }
    ASSERT_FALSE(fs::exists(destFile));

    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, destFile));
    ASSERT_TRUE(fs::exists(destFile));
    ASSERT_FALSE(fs::exists(sourceFile));

    ASSERT_EQ(getFileContents(destFile), "original");

    ASSERT_NO_ERROR(fs::remove(destFile, false));
  }

  // Test 2: Move source over empty.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream sourceOut(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
      sourceOut << "original";

      llvm::raw_fd_ostream destOut(destFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
    }

    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, destFile));
    ASSERT_TRUE(fs::exists(destFile));
    ASSERT_FALSE(fs::exists(sourceFile));

    ASSERT_EQ(getFileContents(destFile), "original");

    ASSERT_NO_ERROR(fs::remove(destFile, false));
  }

  // Test 3: Move source over non-empty-but-different.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream sourceOut(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
      sourceOut << "original";

      llvm::raw_fd_ostream destOut(destFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
      destOut << "different";
    }

    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, destFile));
    ASSERT_TRUE(fs::exists(destFile));
    ASSERT_FALSE(fs::exists(sourceFile));

    ASSERT_EQ(getFileContents(destFile), "original");

    ASSERT_NO_ERROR(fs::remove(destFile, false));
  }

  // Test 4: Move source over identical.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream sourceOut(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
      sourceOut << "original";

      llvm::raw_fd_ostream destOut(destFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
      destOut << "original";
    }

    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, destFile));
    ASSERT_TRUE(fs::exists(destFile));
    ASSERT_FALSE(fs::exists(sourceFile));

    ASSERT_NO_ERROR(fs::remove(destFile, false));
  }

  // Test 5: Move source over self.
  {
    {
      std::error_code error;
      llvm::raw_fd_ostream sourceOut(sourceFile, error, fs::OF_None);
      ASSERT_NO_ERROR(error);
      sourceOut << "original";
    }

    ASSERT_NO_ERROR(moveFileIfDifferent(sourceFile, sourceFile));
    ASSERT_TRUE(fs::exists(sourceFile));

    ASSERT_NO_ERROR(fs::remove(sourceFile, false));
  }

  // Clean up.
  ASSERT_NO_ERROR(fs::remove(dirPath, false));
}

TEST(FileSystem, MoveFileIfDifferentNonExistent) {
  // Create unique temporary directory for these tests
  llvm::SmallString<128> dirPath;
  ASSERT_NO_ERROR(fs::createUniqueDirectory("FileSystem-test", dirPath));

  llvm::SmallString<128> sourceFile = dirPath;
  path::append(sourceFile, "source.txt");
  llvm::SmallString<128> destFile = dirPath;
  path::append(destFile, "dest.txt");

  // Test 1: Nonexistent -> nonexistent.
  ASSERT_TRUE((bool)moveFileIfDifferent(sourceFile, destFile));

  {
    std::error_code error;
    llvm::raw_fd_ostream emptyOut(destFile, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
  }

  // Test 2: Nonexistent -> present.
  ASSERT_TRUE((bool)moveFileIfDifferent(sourceFile, destFile));

  // Clean up.
  ASSERT_NO_ERROR(fs::remove(destFile));
  ASSERT_NO_ERROR(fs::remove(dirPath));
}

TEST(FileSystem, MoveFileIfDifferentInvalid) {
  // Create unique temporary directory for these tests
  llvm::SmallString<128> dirPath;
  ASSERT_NO_ERROR(fs::createUniqueDirectory("FileSystem-test", dirPath));

  llvm::SmallString<128> sourceFile = dirPath;
  path::append(sourceFile, "source.txt");
  {
    std::error_code error;
    llvm::raw_fd_ostream emptyOut(sourceFile, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
  }

  // Test 1: Move a file over its parent directory.
  ASSERT_TRUE((bool)moveFileIfDifferent(sourceFile, dirPath));

  // Test 2: Move a file into a nonexistent directory.
  llvm::SmallString<128> destFile = dirPath;
  path::append(destFile, "nonexistent", "dest.txt");
  ASSERT_TRUE((bool)moveFileIfDifferent(sourceFile, destFile));

  // Clean up.
  ASSERT_NO_ERROR(fs::remove(sourceFile, false));
  ASSERT_NO_ERROR(fs::remove(dirPath, false));
}
} // anonymous namespace
