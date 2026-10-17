//===--- PlatformTest.cpp - Tests for target platform helpers ------------===//
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

#include "swift/Basic/Platform.h"
#include "llvm/TargetParser/Triple.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(PlatformTest, DeploymentTargetVersion) {
  auto macOSVersion = getDeploymentTargetVersionForTriple(
      llvm::Triple("arm64-apple-macosx15.2"));
  ASSERT_TRUE(macOSVersion.has_value());
  EXPECT_EQ(*macOSVersion, llvm::VersionTuple(15, 2));

  auto unspecifiedMacOSVersion =
      getDeploymentTargetVersionForTriple(llvm::Triple("arm64-apple-macosx"));
  EXPECT_FALSE(unspecifiedMacOSVersion.has_value());

  auto unspecifiedIOSVersion =
      getDeploymentTargetVersionForTriple(llvm::Triple("arm64-apple-ios"));
  EXPECT_FALSE(unspecifiedIOSVersion.has_value());

  auto linuxVersion = getDeploymentTargetVersionForTriple(
      llvm::Triple("x86_64-unknown-linux-gnu"));
  EXPECT_FALSE(linuxVersion.has_value());

  auto unspecifiedWindowsVersion = getDeploymentTargetVersionForTriple(
      llvm::Triple("x86_64-pc-windows-msvc"));
  EXPECT_FALSE(unspecifiedWindowsVersion.has_value());

  auto windowsVersion = getDeploymentTargetVersionForTriple(
      llvm::Triple("x86_64-pc-windows10.0.19041-msvc"));
  ASSERT_TRUE(windowsVersion.has_value());
  EXPECT_EQ(*windowsVersion, llvm::VersionTuple(10, 0, 19041));

  auto androidVersion = getDeploymentTargetVersionForTriple(
      llvm::Triple("aarch64-unknown-linux-android28"));
  ASSERT_TRUE(androidVersion.has_value());
  EXPECT_EQ(*androidVersion, llvm::VersionTuple(28));

  auto unspecifiedAndroidVersion = getDeploymentTargetVersionForTriple(
      llvm::Triple("aarch64-unknown-linux-android"));
  EXPECT_FALSE(unspecifiedAndroidVersion.has_value());
}
