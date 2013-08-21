//===--- SwiftTargetMachine.h ------------------------------------*- C++ -*-===//
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
// This file implements the SwiftTargetMachine class.
//
// FIXME (LLVM-Swift-Branch): Remove this file and put the
// appropriate hooks into LLVM.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TARGET_MACHINE_H
#define SWIFT_TARGET_MACHINE_H

#include "swift/AST/AST.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/MC/MCStreamer.h"

namespace llvm {
  class MCStreamer;
  class MCContext;
}

namespace swift {
namespace irgen {

class IRGenDebugInfo;

/// SwiftTargetMachine - This class exists only to inject
/// SwiftASTStreamerPass into the PassManager. We have to do it this
/// way because we need that pass to have to access to both the
/// MCContext and the MCStreamer.
///
class TargetMachine : public llvm::LLVMTargetMachine {
  llvm::LLVMTargetMachine *Impl;
  llvm::OwningPtr<llvm::MCStreamer> AsmStreamer;
  llvm::MCContext *Context;
  TranslationUnit *TU;
  IRGenDebugInfo *DebugInfo;
public:
  TargetMachine(llvm::TargetMachine *TM,
                TranslationUnit *TU,
                IRGenDebugInfo *DI) :
    llvm::LLVMTargetMachine(TM->getTarget(), TM->getTargetTriple(),
                            TM->getTargetCPU(), TM->getTargetFeatureString(),
                            TM->Options, TM->getRelocationModel(),
                            TM->getCodeModel(), TM->getOptLevel()),
    Impl(static_cast<llvm::LLVMTargetMachine*>(TM)),
    TU(TU),
    DebugInfo(DI) { }
  virtual bool 
  addPassesToEmitFile(llvm::PassManagerBase &PM,
                      llvm::formatted_raw_ostream &Out,
                      llvm::TargetMachine::CodeGenFileType FileType,
                      bool DisableVerify = true,
                      llvm::AnalysisID StartAfter = 0,
                      llvm::AnalysisID StopAfter = 0);
};

}; // End irgen namespace.
}; // End swift namespace.

#endif
