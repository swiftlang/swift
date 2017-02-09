//===--- ASTSectionImporter.h - Import AST Section Modules ------*- C++ -*-===//
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
//
// This file implements support for loading modules serialized into a
// Mach-O AST section into Swift.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_ASTSECTION_IMPORTER_H
#define SWIFT_ASTSECTION_IMPORTER_H

#include "swift/Basic/LLVM.h"
#include <string>

namespace swift {
  class SerializedModuleLoader;

  /// \brief Provided a memory buffer with an entire Mach-O __apple_ast
  /// section, this function makes memory buffer copies of all swift
  /// modules found in it and registers them using
  /// registerMemoryBuffer() so they can be found by loadModule(). The
  /// access path of all modules found in the section is appended to
  /// the vector foundModules.
  /// \return true if successful.
  bool parseASTSection(SerializedModuleLoader* SML, StringRef Data,
                       SmallVectorImpl<std::string> &foundModules);


}
#endif
