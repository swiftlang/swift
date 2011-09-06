//===--- Subsystems.h - Swift Compiler Subsystem Entrypoints ----*- C++ -*-===//
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
//  This file declares the main entrypoints to the various subsystems.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SUBSYSTEMS_H
#define SWIFT_SUBSYSTEMS_H

namespace swift {
  class TranslationUnit;
  class ASTContext;

  namespace irgen {
    class Options;
  }
  
  /// parseTranslationUnit - Parse a single buffer as a translation unit and
  /// return the decl.
  TranslationUnit *parseTranslationUnit(unsigned BufferID, ASTContext &Ctx);
  

  /// performNameBinding - Once parsing is complete, this walks the AST to
  /// resolve names and do other top-level validation.
  void performNameBinding(TranslationUnit *TU, ASTContext &Ctx);
  
  /// performTypeChecking - Once parsing and namebinding are complete, this
  /// walks the AST to resolve types and diagnose problems therein.
  ///
  void performTypeChecking(TranslationUnit *TU, ASTContext &Ctx);

  /// performIRGeneration - Turn the given translation unit into
  /// either LLVM IR or native code.
  void performIRGeneration(TranslationUnit *TU, ASTContext &Ctx,
                           irgen::Options &Opts);
  
} // end namespace swift

#endif
