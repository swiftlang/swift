//===--- IRGen.h - Swift Language IR Generation -----------------*- C++ -*-===//
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
// This file defines the interface invoked by the frontend to turn
// the AST into LLVM IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGEN_H
#define SWIFT_IRGEN_IRGEN_H

namespace llvm {
  class Module;
}

namespace swift {
  class ASTContext;
  class TranslationUnitDecl;

namespace irgen {
  class Options;
}

/// performIRGeneration - Emits the given translation unit into the
/// given LLVM module, returning true if an error occurred.
void performIRGeneration(TranslationUnitDecl *TU, ASTContext &Context,
                         irgen::Options &Opts);

} // end namespace swift

#endif
