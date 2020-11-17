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
using namespace swift::dependencies;

ScanTest::ScanTest() : ScannerTool(), StdLibDir(SWIFTLIB_DIR) {
  INITIALIZE_LLVM();
  // Create a temporary filesystem workspace for this test.
  llvm::sys::fs::createUniqueDirectory("ScanTest.Workspace",
                                       TemporaryTestWorkspace);
}

ScanTest::~ScanTest() {
  llvm::sys::fs::remove_directories(TemporaryTestWorkspace);
}
