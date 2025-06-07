//===--- RegionIsolation.h ------------------------------------------------===//
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
///
/// \file This file just declares some command line options that are used for
/// the various parts of region analysis based diagnostics.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_REGIONANALYSIS_H
#define SWIFT_SILOPTIMIZER_UTILS_REGIONANALYSIS_H

namespace swift {

namespace regionisolation {

enum class LoggingFlag {
  Off = 0,
  Normal = 0x1,
  Verbose = 0x3,
};

extern LoggingFlag ENABLE_LOGGING;

#ifdef REGIONBASEDISOLATION_LOG
#error "REGIONBASEDISOLATION_LOG already defined?!"
#endif

#ifdef REGIONBASEDISOLATION_VERBOSE_LOG
#error "REGIONBASEDISOLATION_VERBOSE_LOG already defined?!"
#endif

#define REGIONBASEDISOLATION_LOG(...)                                          \
  do {                                                                         \
    if (swift::regionisolation::ENABLE_LOGGING !=                              \
        swift::regionisolation::LoggingFlag::Off) {                            \
      __VA_ARGS__;                                                             \
    }                                                                          \
  } while (0);

#define REGIONBASEDISOLATION_VERBOSE_LOG(...)                                  \
  do {                                                                         \
    if (swift::regionisolation::ENABLE_LOGGING ==                              \
        swift::regionisolation::LoggingFlag::Verbose) {                        \
      __VA_ARGS__;                                                             \
    }                                                                          \
  } while (0);

} // namespace regionisolation

} // namespace swift

#endif
