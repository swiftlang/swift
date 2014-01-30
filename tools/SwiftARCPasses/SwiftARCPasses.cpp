//===- SwiftARCPasses.cpp - hooks to test the Swift passes with opt -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/OptimizeARC/Passes.h"

using namespace llvm;

static RegisterPass<swift::SwiftAliasAnalysis>
  AA("swift-aa", "Swift Alias Analysis", false, true);
static RegisterPass<swift::SwiftARCOpt>
  AO("swift-arc-optimize", "Swift ARC optimization");
static RegisterPass<swift::SwiftARCExpandPass>
  AE("swift-arc-expand", "Swift ARC expansion");
