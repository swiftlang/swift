//===--- IRGenModule.h - Swift Global IR Generation Module ------*- C++ -*-===//
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
// This file defines the interface used 
// the AST into LLVM IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENMODULE_H
#define SWIFT_IRGEN_IRGENMODULE_H

namespace llvm {
  class Module;
  class TargetData;
}

namespace swift {
  class ASTContext;
  class TranslationUnitDecl;

namespace irgen {
  class Options;

/// IRGenModule - Primary class for emitting IR for global declarations.
/// 
class IRGenModule {
public:
  ASTContext &Context;
  Options &Opts;
  llvm::Module &Module;
  const llvm::TargetData &TargetData;

  IRGenModule(ASTContext &Context, Options &Opts, llvm::Module &Module,
              const llvm::TargetData &TargetData)
    : Context(Context), Opts(Opts), Module(Module),
      TargetData(TargetData) {}

  void emitTranslationUnit(TranslationUnitDecl *TU);
};

} // end namespace irgen
} // end namespace swift

#endif
