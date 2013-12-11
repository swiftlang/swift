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
  std::vector<std::string> InputFilenames;
  std::string OutputFilename;

  std::string ModuleName;

  std::string SerializedDiagnosticsPath;
};

}

#endif
