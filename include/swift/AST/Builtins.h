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

#include "swift/Basic/LLVM.h"
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

  /// The builtin is overloaded over all integer types and vectors of integers.
  IntegerOrVector,

  /// The builtin is overloaded over all integer types and the raw pointer type.
  IntegerOrRawPointer,

  /// The builtin is overloaded over all integer types, the raw pointer type,
  /// and vectors of integers.
  IntegerOrRawPointerOrVector,

  /// The builtin is overloaded over all floating-point types.
  Float,

  /// The builtin is overloaded over all floating-point types and vectors of
  /// floating-point types.
  FloatOrVector,

  /// The builtin is has custom processing.
  Special
};


/// getBuiltinBaseName - Decode the type list of a builtin (e.g. mul_Int32) and
/// return the base name (e.g. "mul").
StringRef getBuiltinBaseName(ASTContext &C, StringRef Name,
                             SmallVectorImpl<Type> &Types);

/// getLLVMIntrinsicID - Given an LLVM IR intrinsic name with argument types
/// remove (e.g. like "bswap") return the LLVM IR IntrinsicID for the intrinsic
/// or 0 if the intrinsic name doesn't match anything.
unsigned getLLVMIntrinsicID(StringRef Name, bool HasArgTypes);
  
/// getBuiltinValue - Finds the builtin value with the given name.
///
/// Returns null if the name does not identifier a known builtin value.
ValueDecl *getBuiltinValue(ASTContext &Context, Identifier Name);

}

#endif
