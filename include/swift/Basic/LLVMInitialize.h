//===--- LLVMInitialize.h ---------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A file that declares macros for initializing all parts of LLVM that various
// binaries in swift use. Please call PROGRAM_START in the main routine of all
// binaries, and INITIALIZE_LLVM in anything that uses Clang or LLVM IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_LLVMINITIALIZE_H
#define SWIFT_BASIC_LLVMINITIALIZE_H

#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"

#define PROGRAM_START(argc, argv) \
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]); \
  llvm::PrettyStackTraceProgram _INITIALIZE_LLVM_STACKTRACE(argc, argv); \
  llvm::llvm_shutdown_obj _INITIALIZE_LLVM_SHUTDOWN_OBJ

#define INITIALIZE_LLVM() \
  do { \
    llvm::InitializeAllTargets(); \
    llvm::InitializeAllTargetMCs(); \
    llvm::InitializeAllAsmPrinters(); \
    llvm::InitializeAllAsmParsers(); \
    llvm::InitializeAllDisassemblers(); \
    llvm::InitializeAllTargetInfos(); \
  } while (0)

#endif // SWIFT_BASIC_LLVMINITIALIZE_H
