//===--- MacroExpansionOptions.h --------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_MACROEXPANSIONOPTIONS_H
#define SWIFT_BASIC_MACROEXPANSIONOPTIONS_H

#include <string>

namespace swift {

/// Describes how macro expansion buffers should be emitted.
struct MacroExpansionOptions {
  /// Whether to emit macro expansion buffers into separate, temporary files.
  bool EmitFiles = false;

  /// The path to the directory into which macro expansion files should be
  /// written.
  ///
  /// If empty, a subdirectory of the system temp directory will be used.
  std::string Path = "";
};

} // end namespace swift

#endif // SWIFT_BASIC_MACROEXPANSIONOPTIONS_H
