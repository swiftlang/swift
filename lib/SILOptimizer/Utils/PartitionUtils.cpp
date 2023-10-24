//===--- PartitionUtils.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/PartitionUtils.h"
#include "llvm/Support/CommandLine.h"

#ifndef NDEBUG

bool swift::PartitionPrimitives::REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING;

static llvm::cl::opt<bool, true> // The parser
    RegionBasedIsolationVerboseLog(
        "sil-regionbasedisolation-verbose-log",
        llvm::cl::desc("Enable verbose logging for SIL region based isolation "
                       "diagnostics"),
        llvm::cl::Hidden,
        llvm::cl::location(swift::PartitionPrimitives::
                               REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING));

#endif
