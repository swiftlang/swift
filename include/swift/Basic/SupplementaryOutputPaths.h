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

#include "swift/Basic/FileTypes.h"
#include "swift/Basic/LLVM.h"
#include "llvm/IR/Function.h"

#include <string>

namespace swift {
struct SupplementaryOutputPaths {
  /// The path to which we should emit a header file that exposes the Swift
  /// declarations to C, Objective-C and C++ clients for the module.
  ///
  /// Currently only makes sense when the compiler has whole module knowledge.
  /// The modes for which it makes sense incuide both WMO and the "merge
  /// modules" job that happens after the normal compilation jobs. That's where
  /// the header is emitted in single-file mode, since it needs whole-module
  /// information.
  ///
  /// \sa swift::printAsClangHeader
  std::string ClangHeaderOutputPath;

  /// The path to which we should emit a serialized module.
  /// It is valid whenever there are any inputs.
  ///
  /// This binary format is used to describe the interface of a module when
  /// imported by client source code. The swiftmodule format is described in
  /// docs/Serialization.md.
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
  /// docs/DependencyAnalysis.md for more information.
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

  /// The path to which we should emit a private module interface.
  ///
  /// The private module interface contains all SPI decls and attributes.
  ///
  /// \sa ModuleInterfaceOutputPath
  std::string PrivateModuleInterfaceOutputPath;

  /// The path to which we should emit module summary file.
  std::string ModuleSummaryOutputPath;

  /// The output path to generate ABI baseline.
  std::string ABIDescriptorOutputPath;

  /// The output path for extracted compile-time-known value information
  std::string ConstValuesOutputPath;

  /// The output path of Swift semantic info for this module.
  std::string ModuleSemanticInfoOutputPath;

  /// The output path for YAML optimization record file.
  std::string YAMLOptRecordPath;

  /// The output path for bitstream optimization record file.
  std::string BitstreamOptRecordPath;

  SupplementaryOutputPaths() = default;

  /// Apply a given function for each existing (non-empty string) supplementary output
  void forEachSetOutput(llvm::function_ref<void(const std::string&)> fn) const {
    if (!ClangHeaderOutputPath.empty())
      fn(ClangHeaderOutputPath);
    if (!ModuleOutputPath.empty())
      fn(ModuleOutputPath); 
    if (!ModuleSourceInfoOutputPath.empty())
      fn(ModuleSourceInfoOutputPath); 
    if (!ModuleDocOutputPath.empty())
      fn(ModuleDocOutputPath); 
    if (!DependenciesFilePath.empty())
      fn(DependenciesFilePath); 
    if (!ReferenceDependenciesFilePath.empty())
      fn(ReferenceDependenciesFilePath); 
    if (!SerializedDiagnosticsPath.empty())
      fn(SerializedDiagnosticsPath); 
    if (!FixItsOutputPath.empty())
      fn(FixItsOutputPath); 
    if (!LoadedModuleTracePath.empty())
      fn(LoadedModuleTracePath); 
    if (!TBDPath.empty())
      fn(TBDPath); 
    if (!ModuleInterfaceOutputPath.empty())
      fn(ModuleInterfaceOutputPath); 
    if (!PrivateModuleInterfaceOutputPath.empty())
      fn(PrivateModuleInterfaceOutputPath); 
    if (!ModuleSummaryOutputPath.empty())
      fn(ModuleSummaryOutputPath);
    if (!ABIDescriptorOutputPath.empty())
      fn(ABIDescriptorOutputPath);
    if (!ConstValuesOutputPath.empty())
      fn(ConstValuesOutputPath);
    if (!YAMLOptRecordPath.empty())
      fn(YAMLOptRecordPath);
    if (!BitstreamOptRecordPath.empty())
      fn(BitstreamOptRecordPath);
    if (!ModuleSemanticInfoOutputPath.empty())
      fn(ModuleSemanticInfoOutputPath);
  }

  void forEachSetOutputAndType(
      llvm::function_ref<void(const std::string &, file_types::ID)> fn) const {
    if (!ClangHeaderOutputPath.empty())
      fn(ClangHeaderOutputPath, file_types::ID::TY_ClangHeader);
    if (!ModuleOutputPath.empty())
      fn(ModuleOutputPath, file_types::ID::TY_SwiftModuleFile);
    if (!ModuleSourceInfoOutputPath.empty())
      fn(ModuleSourceInfoOutputPath, file_types::ID::TY_SwiftSourceInfoFile);
    if (!ModuleDocOutputPath.empty())
      fn(ModuleDocOutputPath, file_types::ID::TY_SwiftModuleDocFile);
    if (!DependenciesFilePath.empty())
      fn(DependenciesFilePath, file_types::ID::TY_Dependencies);
    if (!ReferenceDependenciesFilePath.empty())
      fn(ReferenceDependenciesFilePath, file_types::ID::TY_SwiftDeps);
    if (!SerializedDiagnosticsPath.empty())
      fn(SerializedDiagnosticsPath, file_types::ID::TY_SerializedDiagnostics);
    if (!FixItsOutputPath.empty())
      fn(FixItsOutputPath, file_types::ID::TY_SwiftFixIt);
    if (!LoadedModuleTracePath.empty())
      fn(LoadedModuleTracePath, file_types::ID::TY_ModuleTrace);
    if (!TBDPath.empty())
      fn(TBDPath, file_types::ID::TY_TBD);
    if (!ModuleInterfaceOutputPath.empty())
      fn(ModuleInterfaceOutputPath,
         file_types::ID::TY_SwiftModuleInterfaceFile);
    if (!PrivateModuleInterfaceOutputPath.empty())
      fn(PrivateModuleInterfaceOutputPath,
         file_types::ID::TY_PrivateSwiftModuleInterfaceFile);
    if (!ModuleSummaryOutputPath.empty())
      fn(ModuleSummaryOutputPath, file_types::ID::TY_SwiftModuleSummaryFile);
    if (!ABIDescriptorOutputPath.empty())
      fn(ABIDescriptorOutputPath, file_types::ID::TY_SwiftABIDescriptor);
    if (!ConstValuesOutputPath.empty())
      fn(ConstValuesOutputPath, file_types::ID::TY_ConstValues);
    if (!YAMLOptRecordPath.empty())
      fn(YAMLOptRecordPath, file_types::ID::TY_YAMLOptRecord);
    if (!BitstreamOptRecordPath.empty())
      fn(BitstreamOptRecordPath, file_types::ID::TY_BitstreamOptRecord);
    if (!ModuleSemanticInfoOutputPath.empty())
      fn(ModuleSemanticInfoOutputPath, file_types::ID::TY_ModuleSemanticInfo);
  }

  bool empty() const {
    return ClangHeaderOutputPath.empty() && ModuleOutputPath.empty() &&
           ModuleDocOutputPath.empty() && DependenciesFilePath.empty() &&
           ReferenceDependenciesFilePath.empty() &&
           SerializedDiagnosticsPath.empty() && LoadedModuleTracePath.empty() &&
           TBDPath.empty() && ModuleInterfaceOutputPath.empty() &&
           ModuleSourceInfoOutputPath.empty() &&
           ABIDescriptorOutputPath.empty() &&
           ConstValuesOutputPath.empty() &&
           ModuleSemanticInfoOutputPath.empty() && YAMLOptRecordPath.empty() &&
           BitstreamOptRecordPath.empty();
  }
};
} // namespace swift

#endif // SWIFT_FRONTEND_SUPPLEMENTARYOUTPUTPATHS_H
