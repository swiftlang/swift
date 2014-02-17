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
  /// The extension for SIL files.
  static const char SIL_EXTENSION[] = "sil";
  /// The name of the standard library, which is a reserved module name.

  static const char STDLIB_NAME[] = "Swift";
  /// The name of the ObjectiveC module.
  static const char OBJC_MODULE_NAME[] = "ObjectiveC";
  /// The name of the Foundation module.
  static const char FOUNDATION_MODULE_NAME[] = "Foundation";
} // end namespace swift

#endif
