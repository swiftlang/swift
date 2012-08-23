//===--- BasicBlock.cpp - Basic blocks for high-level CFGs -------*- C++ -*-==//
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
//
// This file defines the high-level BasicBlocks used for Swift CFGs.
//
//===----------------------------------------------------------------------===//

#include "swift/CFG/BasicBlock.h"
#include "swift/CFG/CFG.h"
using namespace swift;

BasicBlock::BasicBlock(CFG *C) : cfg(C) { cfg->blocks.push_back(this); }
BasicBlock::~BasicBlock() {}
