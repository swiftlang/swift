//===--- TypeCheckConcurrency.h - Concurrency -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides type checking support for macros.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPECHECKMACROS_H
#define SWIFT_SEMA_TYPECHECKMACROS_H

#include "swift/AST/Type.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class Expr;
class TypeRepr;

#if SWIFT_SWIFT_PARSER

struct ASTGenMacroRAII {
private:
  void *opaqueMacro;
  TypeRepr *signature;
  GenericParamList *genericSignature;

  ASTGenMacroRAII(void *macro, TypeRepr *sig, GenericParamList *genericSig)
  : opaqueMacro(macro), signature(sig),
    genericSignature(genericSig) {}

public:
  static llvm::Optional<ASTGenMacroRAII> lookup(StringRef macroName,
                                                void *sourceFile,
                                                DeclContext *DC,
                                                ASTContext &ctx);

  TypeRepr *getSignature() const { return signature; }
  GenericParamList *getGenericSignature() const { return genericSignature; }

  ~ASTGenMacroRAII();
};

#endif

/// Expands the given macro expression and type-check the result with
/// the given expanded type.
///
/// \returns the type-checked replacement expression, or NULL if the
// macro could not be expanded.
Expr *expandMacroExpr(
    DeclContext *dc, Expr *expr, StringRef macroName, Type expandedType);

} // end namespace swift

#endif /* SWIFT_SEMA_TYPECHECKMACROS_H */

