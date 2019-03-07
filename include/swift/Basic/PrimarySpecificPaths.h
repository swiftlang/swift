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
#include "llvm/ADT/StringRef.h"

#include <string>

namespace swift {

/// Holds all of the output paths, and debugging-info path that are
/// specific to which primary file is being compiled at the moment.

class PrimarySpecificPaths {
public:
  /// The name of the main output file,
  /// that is, the .o file for this input (or a file specified by -o).
  /// If there is no such file, contains an empty string. If the output
  /// is to be written to stdout, contains "-".
  std::string OutputFilename;

  SupplementaryOutputPaths SupplementaryOutputs;

  /// The name of the "main" input file, used by the debug info.
  std::string MainInputFilenameForDebugInfo;

  PrimarySpecificPaths(StringRef OutputFilename = StringRef(),
                       StringRef MainInputFilenameForDebugInfo = StringRef(),
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
