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
  ///
  /// Currently only makes sense when the compiler has whole module knowledge.
  /// The modes for which it makes sense incuide both WMO and the "merge
  /// modules" job that happens after the normal compilation jobs. That's where
  /// the header is emitted in single-file mode, since it needs whole-module
  /// information.
  ///
  /// \sa swift::printAsObjC
  std::string ObjCHeaderOutputPath;

  /// The path to which we should emit a serialized module.
  /// It is valid whenever there are any inputs.
  ///
  /// This binary format is used to describe the interface of a module when
  /// imported by client source code. The swiftmodule format is described in
  /// docs/Serialization.rst.
  ///
  /// \sa swift::serialize
  std::string ModuleOutputPath;

  /// The path to which we should emit a module source information file.
  /// It is valid whenever there are any inputs.
  ///
  /// This binary format stores source locations and other information about the
  /// declarations in a module.
  ///
  /// \sa swift::serialize
  std::string ModuleSourceInfoOutputPath;

  /// The path to which we should emit a module documentation file.
  /// It is valid whenever there are any inputs.
  ///
  /// This binary format stores doc comments and other information about the
  /// declarations in a module.
  ///
  /// \sa swift::serialize
  std::string ModuleDocOutputPath;

  /// The path to which we should output a Make-style dependencies file.
  /// It is valid whenever there are any inputs.
  ///
  /// Swift's compilation model means that Make-style dependencies aren't
  /// well-suited to model fine-grained dependencies. See docs/Driver.md for
  /// more information.
  ///
  /// \sa ReferenceDependenciesFilePath
  std::string DependenciesFilePath;

  /// The path to which we should output a Swift "reference dependencies" file.
  /// It is valid whenever there are any inputs.
  ///
  /// "Reference dependencies" track dependencies on a more fine-grained level
  /// than just "this file depends on that file". With Swift's "implicit
  /// visibility" within a module, that becomes very important for any sort of
  /// incremental build. These files are consumed by the Swift driver to decide
  /// whether a source file needs to be recompiled during a build. See
  /// docs/DependencyAnalysis.rst for more information.
  ///
  /// \sa swift::emitReferenceDependencies
  /// \sa DependencyGraph
  std::string ReferenceDependenciesFilePath;

  /// Path to a file which should contain serialized diagnostics for this
  /// frontend invocation.
  ///
  /// This uses the same serialized diagnostics format as Clang, for tools that
  /// want machine-parseable diagnostics. There's a bit more information on
  /// how clients might use this in docs/Driver.md.
  ///
  /// \sa swift::serialized_diagnostics::createConsumer
  std::string SerializedDiagnosticsPath;

  /// The path to which we should output fix-its as source edits.
  ///
  /// This is a JSON-based format that is used by the migrator, but is not
  /// really vetted for anything else.
  ///
  /// \sa swift::writeEditsInJson
  std::string FixItsOutputPath;

  /// The path to which we should output a loaded module trace file.
  /// It is valid whenever there are any inputs.
  ///
  /// The file is appended to, and consists of line-delimited JSON objects,
  /// where each line is of the form `{ "name": NAME, "target": TARGET,
  /// "swiftmodules": [PATH, PATH, ...] }`, representing the (real-path) PATHs
  /// to each .swiftmodule that was loaded while building module NAME for target
  /// TARGET. This format is subject to arbitrary change, however.
  std::string LoadedModuleTracePath;

  /// The path to which we should output a TBD file.
  ///
  /// "TBD" stands for "text-based dylib". It's a YAML-based format that
  /// describes the public ABI of a library, which clients can link against
  /// without having an actual dynamic library binary.
  ///
  /// Only makes sense when the compiler has whole-module knowledge.
  ///
  /// \sa swift::writeTBDFile
  std::string TBDPath;

  /// The path to which we should emit a module interface, which can
  /// be used by a client source file to import this module.
  ///
  /// This format is similar to the binary format used for #ModuleOutputPath,
  /// but is intended to be stable across compiler versions.
  ///
  /// Currently only makes sense when the compiler has whole-module knowledge.
  ///
  /// \sa swift::emitSwiftInterface
  std::string ModuleInterfaceOutputPath;

  SupplementaryOutputPaths() = default;
  SupplementaryOutputPaths(const SupplementaryOutputPaths &) = default;

  bool empty() const {
    return ObjCHeaderOutputPath.empty() && ModuleOutputPath.empty() &&
           ModuleDocOutputPath.empty() && DependenciesFilePath.empty() &&
           ReferenceDependenciesFilePath.empty() &&
           SerializedDiagnosticsPath.empty() && LoadedModuleTracePath.empty() &&
           TBDPath.empty() && ModuleInterfaceOutputPath.empty() &&
           ModuleSourceInfoOutputPath.empty();
  }
};
} // namespace swift

#endif // SWIFT_FRONTEND_SUPPLEMENTARYOUTPUTPATHS_H
