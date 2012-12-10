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

    /// \brief Retrieve the set of options that prints everything.
    static PrintOptions printEverything() {
      PrintOptions result;
      result.FunctionDefinitions = true;
      result.TypeDefinitions = true;
      result.VarInitializers = true;
      return result;
    }
  };
}

#endif // LLVM_SWIFT_AST_PRINTOPTIONS_H
