//===-- ClangImporterOptions.h ---------------------------------*- C++ -*--===//
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

  /// If true, matched getter-like and setter-like methods will be imported as
  /// properties.
  bool InferImplicitProperties = false;

  /// If true, Clang diagnostics will be dumped to stderr using Clang's
  /// diagnostic printer as well as being passed to Swift's diagnostic engine.
  bool DumpClangDiagnostics = false;

  /// If true, forward declarations will be imported using unavailable types
  /// instead of dropped altogether when possible.
  bool ImportForwardDeclarations = false;

  /// If true, function and method declarations with error
  /// out-parameters should be imported as "throws".
  bool ErrorHandling = true;
};

} // end namespace swift

#endif
