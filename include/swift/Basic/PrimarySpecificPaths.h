//===--- PrimarySpecificPaths.h ---------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PRIMARYSPECIFICPATHS_H
#define SWIFT_BASIC_PRIMARYSPECIFICPATHS_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SupplementaryOutputPaths.h"

#include <string>

namespace swift {
class PrimarySpecificPaths {
public:
  std::string OutputFilename;
  SupplementaryOutputPaths SupplementaryOutputs;

  /// The name of the "main" input file, used by the debug info.
  std::string MainInputFilenameForDebugInfo;

  PrimarySpecificPaths(
      std::string OutputFilename = std::string(),
      std::string MainInputFilenameForDebugInfo = std::string(),
      SupplementaryOutputPaths SupplementaryOutputs =
          SupplementaryOutputPaths())
      : OutputFilename(OutputFilename),
        SupplementaryOutputs(SupplementaryOutputs),
        MainInputFilenameForDebugInfo(MainInputFilenameForDebugInfo) {}

  bool haveModuleOrModuleDocOutputPaths() const {
    return !SupplementaryOutputs.ModuleOutputPath.empty() ||
           !SupplementaryOutputs.ModuleDocOutputPath.empty();
  }
};
} // namespace swift

#endif /* SWIFT_BASIC_PRIMARYSPECIFICPATHS_H */
