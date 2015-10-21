//===- SwiftLLVMPasses.cpp - hooks to test the Swift passes with opt ------===//
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

#include "swift/LLVMPasses/Passes.h"

using namespace llvm;

static RegisterPass<swift::SwiftAAWrapperPass>
  AA("swift-aa", "Swift Alias Analysis", false, true);
static RegisterPass<swift::SwiftRCIdentity>
  RC("swift-rc-identity", "Swift RC Identity Analysis", false, true);
static RegisterPass<swift::SwiftARCOpt>
  AO("swift-arc-optimize", "Swift ARC optimization");
static RegisterPass<swift::SwiftARCContract>
  AE("swift-arc-contract", "Swift ARC contract");
