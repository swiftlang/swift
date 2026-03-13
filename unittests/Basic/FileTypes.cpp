//===--- FileTypes.cpp - for swift/Basic/FileTypes.h ----------------------===//
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

#include "swift/Basic/FileTypes.h"
#include "gtest/gtest.h"

namespace {
using namespace swift;
using namespace swift::file_types;

static const std::vector<std::pair<std::string, ID>> ExtIDs = {
#define TYPE(NAME, ID, EXTENSION, FLAGS) {EXTENSION, TY_##ID},
#include "swift/Basic/FileTypes.def"
};

TEST(FileSystem, lookupTypeFromFilename) {
  for (auto &Entry: ExtIDs) {
    // no extension, skip.
    if (Entry.first.empty())
      continue;
    // raw-sil, raw-sib, lowered-sil, and raw-llvm-ir do not have unique
    // extensions.
    if (Entry.second == TY_RawSIL || Entry.second == TY_RawSIB ||
        Entry.second == TY_LoweredSIL || Entry.second == TY_RawLLVM_IR)
      continue;

    std::string Filename = "Myfile." + Entry.first;
    ID Type = lookupTypeFromFilename(Filename);
    ASSERT_EQ(getTypeName(Type), getTypeName(Entry.second));
  }

  ASSERT_EQ(lookupTypeFromFilename(""), TY_INVALID);
  ASSERT_EQ(lookupTypeFromFilename("."), TY_INVALID);
  ASSERT_EQ(lookupTypeFromFilename(".."), TY_INVALID);
}

} // namespace
