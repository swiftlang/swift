//===--- SerializeIncremental.cpp - Write incremental swiftdeps -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "serialize-incremental"
#include "Serialization.h"
#include "swift/AST/FineGrainedDependencyFormat.h"

#include <type_traits>

using namespace swift;
using namespace swift::serialization;
using namespace llvm::support;
using llvm::BCBlockRAII;

void Serializer::writeIncrementalInfo(
    const fine_grained_dependencies::SourceFileDepGraph *DepGraph) {
  if (!DepGraph)
    return;

  {
    BCBlockRAII restoreBlock(Out, INCREMENTAL_INFORMATION_BLOCK_ID, 5);
    swift::fine_grained_dependencies::writeFineGrainedDependencyGraph(
        Out, *DepGraph);
  }
}
