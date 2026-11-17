//===-- CompileJobCacheKeyTest.cpp ------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/CompileJobCacheKey.h"
#include "swift/Frontend/CachingUtils.h"
#include "clang/CAS/IncludeTree.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Testing/Support/Error.h"
#include "gtest/gtest.h"

using namespace swift;

static llvm::Expected<llvm::cas::ObjectRef>
createCacheKey(llvm::cas::ObjectStore &CAS, ArrayRef<const char *> Args) {
  auto BaseKey = createCompileJobBaseCacheKey(CAS, Args);
  if (!BaseKey)
    return BaseKey.takeError();
  return createCompileJobCacheKeyForOutput(CAS, *BaseKey, 0);
}

TEST(CompileJobCacheKey, CreateCASFileSystemFromCacheKey) {
  std::unique_ptr<llvm::cas::ObjectStore> CAS = llvm::cas::createInMemoryCAS();
  StringRef Content = "func a() {}";
  auto ContentRef = CAS->storeFromString({}, Content);
  ASSERT_THAT_EXPECTED(ContentRef, llvm::Succeeded());
  auto File =
      clang::cas::IncludeTree::File::create(*CAS, "/tmp/a.swift", *ContentRef);
  ASSERT_THAT_EXPECTED(File, llvm::Succeeded());
  auto FileList = clang::cas::IncludeTree::FileList::create(
      *CAS,
      {{File->getRef(),
        static_cast<clang::cas::IncludeTree::FileList::FileSizeTy>(
            Content.size())}},
      {});
  ASSERT_THAT_EXPECTED(FileList, llvm::Succeeded());
  std::string FileListID = FileList->getID().toString();

  const char *Args[] = {"-frontend",
                        "-c",
                        "-module-name",
                        "Test",
                        "-clang-include-tree-filelist",
                        FileListID.c_str(),
                        "-primary-file",
                        "/tmp/a.swift"};
  auto Key = createCacheKey(*CAS, Args);
  ASSERT_THAT_EXPECTED(Key, llvm::Succeeded());

  auto FS = createCASFileSystemFromCacheKey(*CAS, *Key);
  ASSERT_THAT_EXPECTED(FS, llvm::Succeeded());
  auto Buffer = (*FS)->getBufferForFile("/tmp/a.swift");
  ASSERT_TRUE(Buffer);
  EXPECT_EQ((*Buffer)->getBuffer(), Content);
}

TEST(CompileJobCacheKey, CreateCASFileSystemFromCacheKeyWithoutIncludeTree) {
  std::unique_ptr<llvm::cas::ObjectStore> CAS = llvm::cas::createInMemoryCAS();
  const char *Args[] = {"-frontend",     "-c",          "-module-name", "Test",
                        "-primary-file", "/tmp/a.swift"};
  auto Key = createCacheKey(*CAS, Args);
  ASSERT_THAT_EXPECTED(Key, llvm::Succeeded());

  EXPECT_THAT_EXPECTED(
      createCASFileSystemFromCacheKey(*CAS, *Key),
      llvm::FailedWithMessage("no clang include tree in cache key"));
}
