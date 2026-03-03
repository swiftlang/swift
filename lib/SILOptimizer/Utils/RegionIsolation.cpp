//===--- RegionIsolation.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/RegionIsolation.h"

#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace regionisolation;

//===----------------------------------------------------------------------===//
//                               MARK: Logging
//===----------------------------------------------------------------------===//

LoggingFlag swift::regionisolation::ENABLE_LOGGING;

static llvm::cl::opt<LoggingFlag, true> // The parser
    RegionBasedIsolationLog(
        "sil-regionbasedisolation-log",
        llvm::cl::desc("Enable logging for SIL region-based isolation "
                       "diagnostics"),
        llvm::cl::Hidden,
        llvm::cl::values(
            clEnumValN(LoggingFlag::Off, "none", "no logging"),
            clEnumValN(LoggingFlag::Normal, "on", "non verbose logging"),
            clEnumValN(LoggingFlag::Verbose, "verbose", "verbose logging")),
        llvm::cl::location(swift::regionisolation::ENABLE_LOGGING));
