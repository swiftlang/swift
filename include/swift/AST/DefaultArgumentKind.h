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

namespace llvm {
class StringRef;
}

namespace swift {

class Expr;

/// Describes the kind of default argument a tuple pattern element has.
enum class DefaultArgumentKind : unsigned {
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
  /// The "nil" literal.
  Nil,
  /// An empty array literal.
  EmptyArray,
  /// An empty dictionary literal.
  EmptyDictionary,
};

/// Retrieve the spelling of this default argument in source code, or
/// an empty string if it has none.
llvm::StringRef getDefaultArgumentSpelling(DefaultArgumentKind kind);

/// Infer a default argument kind from an expression, if the
/// expression is the canonical way to spell that default argument.
DefaultArgumentKind inferDefaultArgumentKind(Expr *expr);

} // end namespace swift

#endif // LLVM_SWIFT_DEFAULTARGUMENTKIND_H

