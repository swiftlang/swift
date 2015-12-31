//===--- DefaultArgumentKind.h - Default Argument Kind Enum -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the DefaultArgumentKind enumeration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEFAULTARGUMENTKIND_H
#define SWIFT_DEFAULTARGUMENTKIND_H

namespace swift {

/// Describes the kind of default argument a tuple pattern element has.
enum class DefaultArgumentKind {
  /// No default argument.
  None,
  /// A normal default argument.
  Normal,
  /// The default argument is inherited from the corresponding argument of the
  /// overridden declaration.
  Inherited,
  /// The __FILE__ default argument, which is expanded at the call site.
  File,
  /// The __LINE__ default argument, which is expanded at the call site.
  Line,
  /// The __COLUMN__ default argument, which is expanded at the call site.
  Column,
  /// The __FUNCTION__ default argument, which is expanded at the call site.
  Function,
  /// The __DSO_HANDLE__ default argument, which is expanded at the call site.
  DSOHandle,
};

} // end namespace swift

#endif // LLVM_SWIFT_DEFAULTARGUMENTKIND_H

