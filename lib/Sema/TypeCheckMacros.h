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

namespace macro_context {
/// Retrieves the evaluation context of a macro with the given name.
///
/// The macro evaluation context is a user-defined generic signature and return
/// type that serves as the "interface type" of references to the macro. The
/// current implementation takes those pieces of syntax from the macro itself,
/// then inserts them into a Swift struct that looks like
///
/// \code
/// struct __MacroEvaluationContext\(macro.genericSignature) {
///   typealias SignatureType = \(macro.signature)
/// }
/// \endcode
///
/// So that we can use all of Swift's native name lookup and type resolution
/// facilities to map the parsed signature type back into a semantic \c Type
/// AST and a set of requiremnets.
///
/// \param macroName The name of the macro to look up.
/// \param useDC The decl context of the use of this macro.
StructDecl *lookup(StringRef macroName, DeclContext *useDC);
}

/// Expands the given macro expression and type-check the result with
/// the given expanded type.
///
/// \returns the type-checked replacement expression, or NULL if the
// macro could not be expanded.
Expr *expandMacroExpr(
    DeclContext *dc, Expr *expr, StringRef macroName, Type expandedType);

} // end namespace swift

#endif // SWIFT_SWIFT_PARSER
#endif /* SWIFT_SEMA_TYPECHECKMACROS_H */

