//===--- ClangImporterOptions.h ---------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CLANGIMPORTER_CLANGIMPORTEROPTIONS_H
#define SWIFT_CLANGIMPORTER_CLANGIMPORTEROPTIONS_H

#include "llvm/ADT/Hashing.h"

#include <string>
#include <vector>

namespace swift {

/// Options for controlling the behavior of the Clang importer.
class ClangImporterOptions {
public:
  /// The module cache path which the Clang importer should use.
  std::string ModuleCachePath;

  /// Extra arguments which should be passed to the Clang importer.
  std::vector<std::string> ExtraArgs;

  /// A directory for overriding Clang's resource directory.
  std::string OverrideResourceDir;

  /// The target CPU to compile for.
  ///
  /// Equivalent to Clang's -mcpu=.
  std::string TargetCPU;

  /// The path to which we should store indexing data, if any.
  std::string IndexStorePath;

  /// The bridging header or PCH that will be imported.
  std::string BridgingHeader;

  /// When automatically generating a precompiled header from the bridging
  /// header, place it in this directory.
  std::string PrecompiledHeaderOutputDir;

  /// The optimizaton setting.  This doesn't typically matter for
  /// import, but it can affect Clang's IR generation of static functions.
  std::string Optimization;

  /// Disable validating the persistent PCH.
  bool PCHDisableValidation = false;

  /// \see Mode
  enum class Modes : uint8_t {
    /// Set up Clang for importing modules into Swift and generating IR from
    /// Swift code.
    Normal,
    /// Set up Clang for backend compilation only.
    EmbedBitcode
  };

  /// Controls how Clang is initially set up.
  Modes Mode = Modes::Normal;

  /// When set, preserves more information during import.
  ///
  /// Also \em disables some information that is only needed for object file
  /// generation.
  bool DetailedPreprocessingRecord = false;

  /// If true, Clang diagnostics will be dumped to stderr using Clang's
  /// diagnostic printer as well as being passed to Swift's diagnostic engine.
  bool DumpClangDiagnostics = false;

  /// If true, forward declarations will be imported using unavailable types
  /// instead of dropped altogether when possible.
  bool ImportForwardDeclarations = false;

  /// Whether to use the import as member inference system
  ///
  /// When importing a global, try to infer whether we can import it as a
  /// member of some type instead. This includes inits, computed properties,
  /// and methods.
  bool InferImportAsMember = false;

  /// If true ignore the swift bridged attribute.
  bool DisableSwiftBridgeAttr = false;

  /// When set, don't look for or load overlays.
  bool DisableOverlayModules = false;

  /// When set, don't enforce warnings with -Werror.
  bool DebuggerSupport = false;

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    using llvm::hash_combine;
    using llvm::hash_combine_range;

    return hash_combine(ModuleCachePath,
                        hash_combine_range(ExtraArgs.begin(), ExtraArgs.end()),
                        OverrideResourceDir,
                        TargetCPU,
                        BridgingHeader,
                        PrecompiledHeaderOutputDir,
                        static_cast<uint8_t>(Mode),
                        DetailedPreprocessingRecord,
                        ImportForwardDeclarations,
                        InferImportAsMember,
                        DisableSwiftBridgeAttr,
                        DisableOverlayModules);
  }
};

} // end namespace swift

#endif
