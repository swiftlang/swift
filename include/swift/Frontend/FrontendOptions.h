//===--- FrontendOptions.h --------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_FRONTENDOPTIONS_H
#define SWIFT_FRONTEND_FRONTENDOPTIONS_H

#include <string>
#include <vector>

namespace swift {

/// Options for controlling the behavior of the frontend.
class FrontendOptions {
public:
  /// The names of input files to the frontend.
  std::vector<std::string> InputFilenames;

  /// The name of the primary output file which should be created
  /// by the frontend.
  std::string OutputFilename;

  /// The name of the module which the frontend is building.
  std::string ModuleName;

  /// Indicates that the frontend should emit "verbose" SIL
  /// (if asked to emit SIL).
  bool EmitVerboseSIL;

  /// Path to a file which should contain serialized diagnostics for this
  /// frontend invocation.
  std::string SerializedDiagnosticsPath;

  FrontendOptions() : InputFilenames(), OutputFilename(), ModuleName(),
                      EmitVerboseSIL(false), SerializedDiagnosticsPath() {};
};

}

#endif
