//===--- DefaultArgumentKind.h - Default Argument Kind Enum -----*- C++ -*-===//
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
  /// The #file default argument, which is expanded at the call site.
  File,
  /// The #line default argument, which is expanded at the call site.
  Line,
  /// The #column default argument, which is expanded at the call site.
  Column,
  /// The #function default argument, which is expanded at the call site.
  Function,
  /// The #dsohandle default argument, which is expanded at the call site.
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

