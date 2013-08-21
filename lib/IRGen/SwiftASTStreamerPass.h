//===--- SwiftASTStreamerPass.h - DWARF SwiftAST Streamer -------*- C++ -*-===//
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
// This file implements the SwiftASTStreamerPass class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_SWIFTASTSTREAMERPASS
#define SWIFT_IRGEN_SWIFTASTSTREAMERPASS

#include "llvm/Pass.h"

namespace llvm {
  class MCStreamer;
  class MCContext;
}

namespace swift {

  class TranslationUnit;

  namespace irgen {
    class IRGenDebugInfo;

    /// This pass builds a swiftmodule that contains all the decls
    /// referenced by DWARF and emit it into a special __apple_swiftast
    /// section in the .o file.
    class SwiftASTStreamerPass : public llvm::ModulePass {
      static char ID;
      llvm::MCStreamer &AsmStreamer;
      llvm::MCContext &Context;
      TranslationUnit *TU;
      IRGenDebugInfo *DebugInfo;
    public:
      SwiftASTStreamerPass(llvm::MCStreamer &S, llvm::MCContext &C,
                           TranslationUnit *TU,
                           IRGenDebugInfo *DI)
        : ModulePass(ID), AsmStreamer(S), Context(C), TU(TU), DebugInfo(DI) {}
      virtual const char *getPassName() const { return "SwiftASTStreamerPass"; }
      virtual bool runOnModule(llvm::Module &M);
    };

  } // namespace irgen
} // namespace swift

#endif
