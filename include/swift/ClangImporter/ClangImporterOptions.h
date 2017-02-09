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

  // The bridging header or PCH that will be imported.
  std::string BridgingHeader;

  /// \see Mode
  enum class Modes {
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

  /// When set, don't validate module system headers. If a header is modified
  /// and this is not set, clang will rebuild the module.
  bool DisableModulesValidateSystemHeaders = false;
};

} // end namespace swift

#endif
