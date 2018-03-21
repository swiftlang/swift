//===--- SupplementaryOutputPaths.h ----------------------------*- C++ -*-===*//
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

#ifndef SWIFT_FRONTEND_SUPPLEMENTARYOUTPUTPATHS_H
#define SWIFT_FRONTEND_SUPPLEMENTARYOUTPUTPATHS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"

#include <string>

namespace swift {
struct SupplementaryOutputPaths {
  /// The path to which we should emit an Objective-C header for the module.
  /// Currently only makes sense when the compiler has whole module knowledge.
  /// The modes for which it makes sense incuide both WMO and the "merge
  /// modules" job that happens after the normal compilation jobs. That's where
  /// the header is emitted in single-file mode, since it needs whole-module
  /// information.

  std::string ObjCHeaderOutputPath;

  /// The path to which we should emit a serialized module.
  /// It is valid whenever there are any inputs.
  std::string ModuleOutputPath;

  /// The path to which we should emit a module documentation file.
  /// It is valid whenever there are any inputs.
  std::string ModuleDocOutputPath;

  /// The path to which we should output a Make-style dependencies file.
  /// It is valid whenever there are any inputs.
  std::string DependenciesFilePath;

  /// The path to which we should output a Swift reference dependencies file.
  /// It is valid whenever there are any inputs.
  std::string ReferenceDependenciesFilePath;

  /// Path to a file which should contain serialized diagnostics for this
  /// frontend invocation.
  std::string SerializedDiagnosticsPath;

  /// The path to which we should output fix-its as source edits.
  std::string FixItsOutputPath;

  /// The path to which we should output a loaded module trace file.
  /// It is currently only used with WMO, but could be generalized.
  std::string LoadedModuleTracePath;

  /// The path to which we should output a TBD file.
  /// It is currently only used with WMO, but could be generalized.
  std::string TBDPath;

  SupplementaryOutputPaths() = default;
  SupplementaryOutputPaths(const SupplementaryOutputPaths &) = default;

  bool empty() const {
    return ObjCHeaderOutputPath.empty() && ModuleOutputPath.empty() &&
           ModuleDocOutputPath.empty() && DependenciesFilePath.empty() &&
           ReferenceDependenciesFilePath.empty() &&
           SerializedDiagnosticsPath.empty() && LoadedModuleTracePath.empty() &&
           TBDPath.empty();
  }
};
} // namespace swift

#endif /* SWIFT_FRONTEND_SUPPLEMENTARYOUTPUTPATHS_H */
