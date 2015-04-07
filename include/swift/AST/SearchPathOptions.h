//===--- SearchPathOptions.h ------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_SEARCHPATHOPTIONS_H
#define SWIFT_AST_SEARCHPATHOPTIONS_H

#include <string>
#include <vector>

namespace swift {

/// Options for controlling search path behavior.
class SearchPathOptions {
public:
  /// Path to the SDK which is being built against.
  std::string SDKPath;

  /// Path(s) which should be searched for modules.
  ///
  /// Do not add values to this directly. Instead, use
  /// \c ASTContext::addSearchPath.
  std::vector<std::string> ImportSearchPaths;

  /// Path(s) which should be searched for frameworks.
  ///
  /// Do not add values to this directly. Instead, use
  /// \c ASTContext::addSearchPath.
  std::vector<std::string> FrameworkSearchPaths;

  /// Path(s) which should be searched for libraries.
  ///
  /// This is used in immediate modes. It is safe to add paths to this directly.
  std::vector<std::string> LibrarySearchPaths;

  /// Path to search for compiler-relative header files.
  std::string RuntimeResourcePath;

  /// Path to search for compiler-relative stdlib dylibs.
  std::string RuntimeLibraryPath;

  /// Path to search for compiler-relative stdlib modules.
  std::string RuntimeLibraryImportPath;

  /// Don't look in for compiler-provided modules.
  bool SkipRuntimeLibraryImportPath = false;
};

}

#endif
