//===--- Common.cpp - Automatic differentiation common utils --*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Automatic differentiation common utilities.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SILOptimizer/Utils/Differentiation/Common.h"

namespace swift {
namespace autodiff {

raw_ostream &getADDebugStream() { return llvm::dbgs() << "[AD] "; }

} // end namespace autodiff
} // end namespace swift
