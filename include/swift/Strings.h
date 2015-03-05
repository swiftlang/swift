//===--- Strings.h - Shared string constants across components --*- C++ -*-===//
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

#ifndef SWIFT_STRINGS_H
#define SWIFT_STRINGS_H

namespace swift {
  /// The extension for serialized modules.
  static const char SERIALIZED_MODULE_EXTENSION[] = "swiftmodule";
  /// The extension for serialized documentation comments.
  static const char SERIALIZED_MODULE_DOC_EXTENSION[] = "swiftdoc";
  /// The extension for SIL files.
  static const char SIL_EXTENSION[] = "sil";
  /// The extension for SIB files.
  static const char SIB_EXTENSION[] = "sib";
  /// The extension for LLVM IR files.
  static const char LLVM_BC_EXTENSION[] = "bc";
  static const char LLVM_IR_EXTENSION[] = "ll";
  /// The name of the standard library, which is a reserved module name.
  static const char STDLIB_NAME[] = "Swift";
  /// The name of the ObjectiveC module.
  static const char OBJC_MODULE_NAME[] = "ObjectiveC";
  /// The name of the Foundation module.
  static const char FOUNDATION_MODULE_NAME[] = "Foundation";
  /// The prefix of module names used by LLDB to capture Swift expressions
  static const char LLDB_EXPRESSIONS_MODULE_NAME_PREFIX[] = "__lldb_expr_";
} // end namespace swift

#endif
