//===--- PrintOptions.h - Swift Language Declaration ASTs -------*- C++ -*-===//
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
// This file defines the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_PRINTOPTIONS_H
#define SWIFT_AST_PRINTOPTIONS_H

namespace swift {
  struct PrintOptions {
    /// \brief The indentation width.
    unsigned Indent = 2;
    
    /// \brief Whether to print function definitions.
    bool FunctionDefinitions = false;

    /// \brief Whether to print type definitions.
    bool TypeDefinitions = false;

    /// \brief Whether to print variable initializers.
    bool VarInitializers = false;

    /// \brief Whether to prefer printing TypeReprs instead of Types,
    /// if a TypeRepr is available.  This allows us to print the original
    /// spelling of the type name.
    ///
    /// \note This should be \c true when printing AST with the intention show
    /// it to the user.
    bool PreferTypeRepr = true;

    /// \brief Whether to print fully qualified Types.
    bool FullyQualifiedTypes = false;

    /// \brief Print fully qualified types if our heuristics say that a certain
    /// type might be ambiguous.
    bool FullyQualifiedTypesIfAmbiguous = false;

    /// \brief Print swift.Slice and swift.Optional with sugared syntax
    /// ([] and ?), even if there are no sugar type nodes.
    bool SynthesizeSugarOnTypes = false;

    /// \brief If true, the printer will explode a pattern like this:
    /// \code
    ///   var (a, b) = f()
    /// \endcode
    /// into multiple variable declarations.
    ///
    /// For this option to work correctly, \c VarInitializers should be
    /// \c false.
    bool ExplodePatternBindingDecls = false;

    /// \brief Whether to print implicit parts of the AST.
    bool SkipImplicit = false;

    /// \brief Retrieve the set of options that prints everything.
    static PrintOptions printEverything() {
      PrintOptions result;
      result.FunctionDefinitions = true;
      result.TypeDefinitions = true;
      result.VarInitializers = true;
      result.SkipImplicit = false;
      return result;
    }
  };
}

#endif // LLVM_SWIFT_AST_PRINTOPTIONS_H
