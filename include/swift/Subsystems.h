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
  class FunctionPass;
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

  /// parseIntoTranslationUnit - Parse a single buffer into the given
  /// taranslation unit.  If the translation unit is the main module, stop
  /// parsing after the next stmt-brace-item with side-effects.  Returns
  /// the number of bytes parsed from the given buffer.
  bool parseIntoTranslationUnit(TranslationUnit *TU, unsigned BufferID,
                                unsigned *BufferOffset = 0,
                                unsigned BufferEndOffset = 0);

  /// performNameBinding - Once parsing is complete, this walks the AST to
  /// resolve names and do other top-level validation.  StartElem indicates
  /// where to start for incremental name binding in the main module.
  void performNameBinding(TranslationUnit *TU, unsigned StartElem = 0);
  
  /// performTypeChecking - Once parsing and namebinding are complete, this
  /// walks the AST to resolve types and diagnose problems therein. StartElem
  /// indicates where to start for incremental type checking in the
  /// main module.
  void performTypeChecking(TranslationUnit *TU, unsigned StartElem = 0);

  /// performCaptureAnalysis - Analyse the AST and mark local declarations
  /// and expressions which can capture them so they can be emitted more
  /// efficiently.  StartElem indicates where to start for incremental capture
  /// analysis in the main module.
  void performCaptureAnalysis(TranslationUnit *TU, unsigned StartElem = 0);

  /// performIRGeneration - Turn the given translation unit into
  /// either LLVM IR or native code.  StartElem indicates where to start for
  /// incremental IRGen in the main module.
  void performIRGeneration(irgen::Options &Opts, llvm::Module *Module,
                           TranslationUnit *TU, unsigned StartElem = 0);

  // Optimization passes.
  llvm::FunctionPass *createSwiftARCOptPass();
} // end namespace swift

#endif
