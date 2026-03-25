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
#include "gtest/gtest.h"
#include <string>

namespace swift {
namespace unittest {

class ScanTest : public ::testing::Test {
public:
  ScanTest();
  ~ScanTest();

protected:
  // The scanner instance used to execute tests' scanning queries
  swiftscan_scanner_t Scanner;

  // Test workspace directory
  llvm::SmallString<256> TemporaryTestWorkspace;

  // Path to where the Swift standard library can be found
  llvm::SmallString<128> StdLibDir;

  // Helper method to perform a scan
  swiftscan_dependency_graph_t
  performScan(const std::vector<const char *> &Command);
};

} // end namespace unittest
} // end namespace swift
