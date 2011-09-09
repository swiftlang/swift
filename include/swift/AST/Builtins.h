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

/// BuiltinTypeKind - The set of builtin types.
enum class BuiltinTypeKind {
  None,
  int1,
  int8,
  int16,
  int32,
  int64,
  float32,
  float64
};

/// Determine whether a name corresponds to a builtin type.
BuiltinTypeKind isBuiltinType(StringRef Name);

/// Get the builtin type for the given name.
///
/// Returns a null type if the type is BuiltinType::None.
Type getBuiltinType(ASTContext &Context, BuiltinTypeKind T);

/// Get the builtin type for the given name.
///
/// Returns a null type if the name is not a known builtin type name.
Type getBuiltinType(ASTContext &Context, Identifier Name);

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

/// Determines if a builtin type falls within the given category.
/// The category cannot be OverloadedBuiltinKind::None.
inline bool isBuiltinTypeOverloaded(BuiltinTypeKind T,
                                    OverloadedBuiltinKind OK) {
  assert(T != BuiltinTypeKind::None);
  switch (OK) {
  case OverloadedBuiltinKind::None:       break; // invalid 
  case OverloadedBuiltinKind::Integer:    return T <= BuiltinTypeKind::int64;
  case OverloadedBuiltinKind::Float:      return BuiltinTypeKind::float32 <= T;
  case OverloadedBuiltinKind::Arithmetic: return true;
  }
  llvm_unreachable("bad overloaded builtin kind");
}

/// BuiltinValueKind - The set of (possibly overloaded) builtin functions.
enum class BuiltinValueKind {
  None,

#define BUILTIN(Id, Name) Id,
#include "swift/AST/Builtins.def"
};

/// isBuiltinValue - Finds the builtin value with this name.
///
/// \param Type - Set to the overloaded type parameter of the builtin.
BuiltinValueKind isBuiltinValue(StringRef Name, BuiltinTypeKind &Type);

/// getBuiltinValue - Finds the builtin value with the given name.
///
/// Returns null if the name does not identifier a known builtin value.
ValueDecl *getBuiltinValue(ASTContext &Context, Identifier Name);

}

#endif
