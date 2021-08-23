//===-------- ScanFixture.h - Dependency scanning tests -*- C++ ---------*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift-c/DependencyScan/DependencyScan.h"
#include "swift/DependencyScan/DependencyScanningTool.h"
#include "gtest/gtest.h"
#include <string>

using namespace swift::dependencies;

namespace swift {
namespace unittest {

class ScanTest : public ::testing::Test {
public:
  ScanTest();
  ~ScanTest();

protected:
  // The tool used to execute tests' scanning queries
  DependencyScanningTool ScannerTool;

  // Test workspace directory
  llvm::SmallString<256> TemporaryTestWorkspace;

  // Path to where the Swift standard library can be found
  llvm::SmallString<128> StdLibDir;
};

} // end namespace unittest
} // end namespace swift
