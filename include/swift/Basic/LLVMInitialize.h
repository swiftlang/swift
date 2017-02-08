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
// A file that declares a macro for initializing all parts of LLVM that various
// binaries in swift use. Please call the macro in the main routine of all
// binaries.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_LLVMINITIALIZE_H
#define SWIFT_BASIC_LLVMINITIALIZE_H

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

#define INITIALIZE_LLVM(argc, argv) \
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]); \
  llvm::PrettyStackTraceProgram _INITIALIZE_LLVM_STACKTRACE(argc, argv); \
  llvm::llvm_shutdown_obj _INITIALIZE_LLVM_SHUTDOWN_OBJ; \
  llvm::InitializeAllTargets(); \
  llvm::InitializeAllTargetMCs(); \
  llvm::InitializeAllAsmPrinters(); \
  llvm::InitializeAllAsmParsers(); \
  llvm::InitializeAllDisassemblers(); \
  llvm::InitializeAllTargetInfos();

#endif // SWIFT_BASIC_LLVMINITIALIZE_H
