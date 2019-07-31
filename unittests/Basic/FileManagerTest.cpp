//===--- FileManagerTest.cpp - for swift/Basic/FileManager.h --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/FileManager.h"
#include "swift/Basic/LLVM.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {
/// An llvm::vfs::FileSystem that tracks the number of times status() and
/// openFileForRead() are called.
class StatTrackingFileSystem : public llvm::vfs::ProxyFileSystem {
  llvm::StringMap<unsigned> statCallCounter;
  llvm::StringMap<unsigned> openFileCounter;
public:
  explicit StatTrackingFileSystem(
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fs)
      : llvm::vfs::ProxyFileSystem(fs) {}

  llvm::ErrorOr<llvm::vfs::Status> status(const llvm::Twine &path) override {
    statCallCounter[path.str()] += 1;
    return ProxyFileSystem::status(path);
  }
  llvm::ErrorOr<std::unique_ptr<llvm::vfs::File>>
  openFileForRead(const llvm::Twine &path) override {
    openFileCounter[path.str()] += 1;
    return ProxyFileSystem::openFileForRead(path);
  }

  unsigned numberOfStatCalls(const StringRef path) {
    return statCallCounter[path];
  }

  unsigned numberOfOpens(const StringRef path) {
    return openFileCounter[path];
  }
};
}

TEST(FileManager, Caching) {
  llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem>
  inMemoryFS(new llvm::vfs::InMemoryFileSystem());

  llvm::IntrusiveRefCntPtr<StatTrackingFileSystem>
  statTrackingFS(new StatTrackingFileSystem(inMemoryFS));

  FileManager fileManager(statTrackingFS);

  // Fill the in-memory file system with some files.

  StringRef fileAContent = "contents of fileA";
  StringRef fileBContent = "contents of fileB";
  inMemoryFS->addFile("fileA.txt", /*mtime*/42,
                      llvm::MemoryBuffer::getMemBuffer(fileAContent));
  inMemoryFS->addFile("fileB.txt", /*mtime*/9001,
                      llvm::MemoryBuffer::getMemBuffer(fileBContent));

  // Open and stat the files multiple times.

  for (int i = 0; i < 5; ++i) {
    // Call status() before opening the file, which means we will stat twice.

    auto statA = fileManager.status("fileA.txt");
    ASSERT_TRUE(statA);
    EXPECT_EQ(statA->getLastModificationTime().time_since_epoch().count(), 42);
    EXPECT_EQ(statA->getSize(), fileAContent.size());

    auto openA = fileManager.getBufferForFile("fileA.txt");
    ASSERT_TRUE(openA);
    EXPECT_EQ((*openA)->getBuffer(), fileAContent);

  }

  for (int i = 0; i < 5; ++i) {
    // Open the file and then call status(), which means we should use the
    // cached status.
    auto openB = fileManager.getBufferForFile("fileB.txt");
    ASSERT_TRUE(openB);
    EXPECT_EQ((*openB)->getBuffer(), fileBContent);

    auto statB = fileManager.status("fileB.txt");
    ASSERT_TRUE(statB);
    EXPECT_EQ(statB->getLastModificationTime().time_since_epoch().count(),
              9001);
    EXPECT_EQ(statB->getSize(), fileBContent.size());
  }

  // We called status() once.
  // FIXME: This test currently fails, because FileManager doesn't keep track
  //        of llvm::vfs::Status values in its FileEntry struct.
  EXPECT_EQ(statTrackingFS->numberOfStatCalls("fileA.txt"), 1);

  // We should have retained our status from calling openFileForRead().
  // FIXME: This test currently fails, because FileManager doesn't keep track
  //        of llvm::vfs::Status values in its FileEntry struct.
  EXPECT_EQ(statTrackingFS->numberOfStatCalls("fileB.txt"), 0);

  // We should have only opened fileA once.
  EXPECT_EQ(statTrackingFS->numberOfOpens("fileA.txt"), 1);

  // We should have only opened fileB once.
  EXPECT_EQ(statTrackingFS->numberOfOpens("fileB.txt"), 1);
}
