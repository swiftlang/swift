//===------- BlocklistTest.cpp --------------------------------------------===//
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

#include "gtest/gtest.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/BlockList.h"
#include "swift/Basic/SourceManager.h"

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

TEST(BlocklistTest, testYamlParsing) {
  SmallString<256> temp;
  ASSERT_FALSE(llvm::sys::fs::createUniqueDirectory(
      "BlocklistTest.testYamlParsing", temp));
  SWIFT_DEFER { llvm::sys::fs::remove_directories(temp); };
  SourceManager sm;
  BlockListStore store(sm);
  std::string path1, path2;
  ASSERT_FALSE(emitFileWithContents(temp, "block1.yaml",
                                    "---\n"
                                    "ShouldUseBinaryModule:\n"
                                      "  ModuleName:\n"
                                        "    - M1 #rdar12345\n"
                                        "    - M2 #rdar12345\n"
                                        "    - M3\n"
                                        "    - M4\n"
                                      "  ProjectName:\n"
                                        "    - P1\n"
                                        "    - P2\n"
                                        "    - P3 #rdar12344\n"
                                        "    - P4\n"
                                    "---\n"
                                    "ShouldUseTextualModule:\n"
                                      "  ModuleName:\n"
                                        "    - M1_2 #rdar12345\n"
                                        "    - M2_2 #rdar12345\n"
                                        "    - M3_2\n"
                                        "    - M4_2\n"
                                      "  ProjectName:\n"
                                        "    - P1_2\n"
                                        "    - P2_2\n"
                                        "    - P3_2 #rdar12344\n"
                                        "    - P4_2\n",
                                    &path1));
  ASSERT_FALSE(emitFileWithContents(temp, "block2.yml",
                                    "---\n"
                                    "ShouldUseBinaryModule:\n"
                                    "  ModuleName:\n"
                                    "    - M1_block2 #rdar12345\n"
                                    "  ProjectName:\n"
                                    "    - P1_block2\n"
                                    "---\n"
                                    "ShouldUseTextualModule:\n"
                                    "  ModuleName:\n"
                                    "    - M1_2_block2 #rdar12345\n"
                                    "  ProjectName:\n"
                                    "    - P1_2_block2\n",
                                    &path2));
  store.addConfigureFilePath(path1);
  store.addConfigureFilePath(path2);
  ASSERT_TRUE(store.hasBlockListAction("M1", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_TRUE(store.hasBlockListAction("M2", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_TRUE(store.hasBlockListAction("P1", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_TRUE(store.hasBlockListAction("P2", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseBinaryModule));

  ASSERT_TRUE(store.hasBlockListAction("M1_2", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseTextualModule));
  ASSERT_TRUE(store.hasBlockListAction("M2_2", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseTextualModule));
  ASSERT_TRUE(store.hasBlockListAction("P1_2", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseTextualModule));
  ASSERT_TRUE(store.hasBlockListAction("P2_2", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseTextualModule));

  ASSERT_FALSE(store.hasBlockListAction("P1", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_FALSE(store.hasBlockListAction("P2", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_FALSE(store.hasBlockListAction("M1", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_FALSE(store.hasBlockListAction("M2", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseBinaryModule));

  ASSERT_TRUE(store.hasBlockListAction("M1_block2", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_TRUE(store.hasBlockListAction("P1_block2", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_TRUE(store.hasBlockListAction("M1_2_block2", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseTextualModule));
  ASSERT_TRUE(store.hasBlockListAction("P1_2_block2", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseTextualModule));

  ASSERT_FALSE(store.hasBlockListAction("M1_block2", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_FALSE(store.hasBlockListAction("P1_block2", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseBinaryModule));
  ASSERT_FALSE(store.hasBlockListAction("M1_2_block2", BlockListKeyKind::ProjectName, BlockListAction::ShouldUseTextualModule));
  ASSERT_FALSE(store.hasBlockListAction("P1_2_block2", BlockListKeyKind::ModuleName, BlockListAction::ShouldUseTextualModule));
}
