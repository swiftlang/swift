//===--- InternalMacro.h - Compiler-Internal Macros -------------*- C++ -*-===//
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
// This provides the abstract base class for macros that are implemented
// directly within the compiler, rather than by an external macro plugin.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_INTERNAL_MACRO_H
#define SWIFT_AST_INTERNAL_MACRO_H

#include <string>

namespace swift {

class ASTContext;
class CustomAttr;
class Decl;

/// The implementation of a macro that is provided directly by the compiler.
///
/// Unlike external macros, an internal macro is never spelled by the user and
/// is never resolved from a parsed macro declaration. Instead, the compiler
/// synthesizes a \c MacroDecl referencing an instance of this class and
/// attaches the corresponding custom attribute itself.
///
/// Since the implementation lives in the compiler it has direct access to
/// the AST of the declaration it is attached to, instead of receiving the
/// surrounding source as strings. The expansion is still produced as source
/// text, which is parsed into a macro-expansion buffer exactly as it is for
/// external macros. This allows the compiler to synthesize declarations while
/// also emitting proper debug locations and source text.
class InternalMacro {
public:
  virtual ~InternalMacro() = default;

  /// Expand this attached macro with direct access to the AST, returning the
  /// expansion source text to be parsed.
  ///
  /// \param ctx The AST context in which the expansion occurs.
  /// \param attachedTo The declaration the macro is attached to.
  virtual std::string expandAttached(ASTContext &ctx,
                                     Decl *attachedTo) const = 0;
};

} // end namespace swift

#endif // SWIFT_AST_INTERNAL_MACRO_H
