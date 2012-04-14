//===--- Builtins.h - Swift Builtin Functions -------------------*- C++ -*-===//
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
//
// This file defines the interface to builtin functions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_BUILTINS_H
#define SWIFT_AST_BUILTINS_H

#include "swift/AST/LLVM.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {
  class ASTContext;
  class Identifier;
  class Type;
  class ValueDecl;

/// Get the builtin type for the given name.
///
/// Returns a null type if the name is not a known builtin type name.
Type getBuiltinType(ASTContext &Context, StringRef Name);

/// OverloadedBuiltinKind - Whether and how a builtin is overloaded.
enum class OverloadedBuiltinKind : unsigned char {
  /// The builtin is not overloaded.
  None,

  /// The builtin is overloaded over all integer types.
  Integer,

  /// The builtin is overloaded over all floating-point types.
  Float,

  /// The builtin is overloaded over all integer and floating-point types.
  Arithmetic
};


/// BuiltinValueKind - The set of (possibly overloaded) builtin functions.
enum class BuiltinValueKind {
  None,

#define BUILTIN(Id, Name) Id,
#include "swift/AST/Builtins.def"
};

/// isBuiltinValue - Finds the builtin value with this name.
///
/// \param Type - Set to the overloaded type parameter of the builtin.
BuiltinValueKind isBuiltinValue(ASTContext &C, StringRef Name,
                                Type &Ty1, Type &Ty2);

/// getBuiltinValue - Finds the builtin value with the given name.
///
/// Returns null if the name does not identifier a known builtin value.
ValueDecl *getBuiltinValue(ASTContext &Context, Identifier Name);

}

#endif
