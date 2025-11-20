//===--- Options.cpp ------------------------------------------------------===//
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

#include "swift/Option/Options.h"
#include "llvm/Option/Option.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::options;

TEST(Options, outputPathCacheInvariant) {
  auto optTable = createSwiftOptTable();
  // 0 is OP_INVALD
  for (auto id = 1; id < LastOption; ++id) {
    auto opt = optTable->getOption(id);
    // Only check the flag accepted by swift-frontend.
    if (!opt.hasFlag(SwiftFlags::FrontendOption))
      continue;

    // The following two options are only accepted by migrator.
    if (id == OPT_emit_migrated_file_path || id == OPT_emit_remap_file_path)
      continue;

    auto name = opt.getName();
    // Check that if a flag matches the convention `-emit-*-path{=}`, it should
    // be a path to an output file and should be marked as cache invariant.
    if (name.starts_with("emit") &&
        (name.ends_with("-path") || name.ends_with("-path=")))
      ASSERT_TRUE(opt.hasFlag(SwiftFlags::CacheInvariant));
  }
}
