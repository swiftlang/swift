//===-------- ScanFixture.cpp - Dependency scanning tests -*- C++ -------*-===//
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

#include "ScanFixture.h"
#include "swift/Basic/LLVMInitialize.h"
#include "llvm/Support/FileSystem.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

ScanTest::ScanTest() : StdLibDir(SWIFTLIB_DIR) {
  INITIALIZE_LLVM();
  Scanner = swiftscan_scanner_create();
  // Create a temporary filesystem workspace for this test.
  llvm::sys::fs::createUniqueDirectory("ScanTest.Workspace",
                                       TemporaryTestWorkspace);
}

ScanTest::~ScanTest() {
  swiftscan_scanner_dispose(Scanner);
  llvm::sys::fs::remove_directories(TemporaryTestWorkspace);
}

swiftscan_dependency_graph_t
ScanTest::performScan(const std::vector<const char *> &Command) {
  auto invocation = swiftscan_scan_invocation_create();
  swiftscan_scan_invocation_set_argv(invocation,
                                     static_cast<int>(Command.size()),
                                     (const char **)Command.data());
  swiftscan_scan_invocation_set_working_directory(
      invocation, TemporaryTestWorkspace.c_str());
  auto result = swiftscan_dependency_graph_create(Scanner, invocation);
  swiftscan_scan_invocation_dispose(invocation);
  return result;
}
