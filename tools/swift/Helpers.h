//===-- Helpers.h - frontend utility methods -----------------*- C++ -*----===//
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
// This file contains declarations of utility methods for parsing and
// performing semantic on modules.
//
//===----------------------------------------------------------------------===//

#ifndef HELPERS_H
#define HELPERS_H

#include "swift/Basic/LLVM.h"

namespace llvm {
  class MemoryBuffer;
}

namespace swift {
  class REPLContext;
  class SILModule;
  class SourceFile;

  bool appendToREPLTranslationUnit(SourceFile &SF,
                                   REPLContext &RC,
                                   llvm::MemoryBuffer *Buffer);

  bool runSILDiagnosticPasses(SILModule &Module);

  /// Run all the SIL performance optimization passes on Module.
  void runSILOptimizationPasses(SILModule &Module);

} // namespace swift

#endif
