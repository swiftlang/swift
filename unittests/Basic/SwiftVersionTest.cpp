//===--- SwiftVersionTest.cpp ---------------------------------------------===//
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

#include "swift/Basic/Version.h"
#include "gtest/gtest.h"

namespace {

TEST(SwiftVersion, Interop) {
  ASSERT_EQ((llvm::VersionTuple)swift::version::Version(),
            llvm::VersionTuple());
  ASSERT_EQ((llvm::VersionTuple)swift::version::Version({1}),
            llvm::VersionTuple(1));
  ASSERT_EQ((llvm::VersionTuple)swift::version::Version({1, 2}),
            llvm::VersionTuple(1, 2));
  ASSERT_EQ((llvm::VersionTuple)swift::version::Version({1, 2, 3}),
            llvm::VersionTuple(1, 2, 3));
  ASSERT_EQ((llvm::VersionTuple)swift::version::Version({1, 2, 3, 4}),
            llvm::VersionTuple(1, 2, 3, 4));
  ASSERT_EQ((llvm::VersionTuple)swift::version::Version({1, 2, 3, 4, 5}),
            llvm::VersionTuple(1, 2, 3, 4, 5));

  ASSERT_NE((llvm::VersionTuple)swift::version::Version({1, 2, 3, 4, 5}),
            llvm::VersionTuple(1, 2, 3, 4, 6));
  ASSERT_NE((llvm::VersionTuple)swift::version::Version({1, 2, 3, 4, 5}),
            llvm::VersionTuple(1, 2, 3, 4));
}

} // namespace
