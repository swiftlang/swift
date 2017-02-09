//===--- FunctionRefKind.h - Function reference kind ------------*- C++ -*-===//
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
// This file defines the FunctionRefKind enum, which is used to describe how
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_FUNCTION_REF_KIND_H
#define SWIFT_AST_FUNCTION_REF_KIND_H

namespace swift {

/// Describes how a function is referenced within an expression node,
/// which dictates whether argument labels are part of the resulting
/// function type or not.
///
/// How a function is referenced comes down to how it was spelled in
/// the source code, e.g., was it called in the source code and was it
/// spelled as a compound name.
enum class FunctionRefKind : unsigned {
  /// The function was referenced using a bare function name (e.g.,
  /// 'f') and not directly called.
  Unapplied,
  /// The function was referenced using a bare function name and was
  /// directly applied once, e.g., "f(a: 1, b: 2)".
  SingleApply,
  /// The function was referenced using a bare function name and was
  /// directly applied two or more times, e.g., "g(x)(y)".
  DoubleApply,
  /// The function was referenced using a compound function name,
  /// e.g., "f(a:b:)".
  Compound,
};

/// Produce a string describing a function reference kind, for
/// debugging purposes.
StringRef getFunctionRefKindStr(FunctionRefKind refKind);

}

#endif // SWIFT_AST_FUNCTION_REF_KIND_H
