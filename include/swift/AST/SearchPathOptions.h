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
  std::vector<std::string> ImportSearchPaths;

  /// Path(s) which should be searched for frameworks.
  std::vector<std::string> FrameworkSearchPaths;

  /// Path which includes the swift runtime.
  std::string RuntimeIncludePath;
};

}

#endif
