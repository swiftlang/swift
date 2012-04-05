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

namespace llvm {
  class Module;
}

namespace swift {
  class TranslationUnit;
  class ASTContext;
  class Component;

  namespace irgen {
    class Options;
  }

  /// verify - Check that the translation unit is well formed (i.e. following
  /// the invariants of the AST, not that the code written by the user makes
  /// sense), aborting and spewing errors if not.
  void verify(TranslationUnit *TUnit);

  /// parseTranslationUnit - Parse a single buffer as a translation unit
  /// in the given component and return the decl.
  TranslationUnit *parseTranslationUnit(unsigned BufferID, Component *Comp,
                                        ASTContext &Ctx, bool IsMainModule);

  /// performNameBinding - Once parsing is complete, this walks the AST to
  /// resolve names and do other top-level validation.
  void performNameBinding(TranslationUnit *TU);
  
  /// performTypeChecking - Once parsing and namebinding are complete, this
  /// walks the AST to resolve types and diagnose problems therein.
  ///
  void performTypeChecking(TranslationUnit *TU);

  /// performCaptureAnalysis - Analyse the AST and mark local declarations
  /// and expressions which can capture them so they can be emitted more
  /// efficiently.
  void performCaptureAnalysis(TranslationUnit *TU);

  /// performIRGeneration - Turn the given translation unit into
  /// either LLVM IR or native code.
  void performIRGeneration(TranslationUnit *TU, irgen::Options &Opts);

  /// performIRGenerationIntoModule - Alternate entry point for IR generation
  /// for users which need the resulting module in memory.
  void performIRGenerationIntoModule(TranslationUnit *TU, irgen::Options &Opts,
                                     llvm::Module &Module);

} // end namespace swift

#endif
