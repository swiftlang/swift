//===--- FrontendTool.h - Frontend control ----------------------*- C++ -*-===//
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
// This file provides a high-level API for interacting with the basic
// frontend tool operation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTENDTOOL_H
#define SWIFT_FRONTENDTOOL_H

#include "swift/Basic/LLVM.h"

namespace llvm {
class Module;
}

namespace swift {
class CompilerInvocation;
class CompilerInstance;
class SILModule;

/// A simple observer of frontend activity.
///
/// Don't let this interface block enhancements to the frontend pipeline.
class FrontendObserver {
public:
  FrontendObserver() = default;
  virtual ~FrontendObserver() = default;

  /// The frontend has parsed the command line.
  virtual void parsedArgs(CompilerInvocation &invocation);

  /// The frontend has configured the compiler instance.
  virtual void configuredCompiler(CompilerInstance &instance);
};

/// Perform all the operations of the frontend, exactly as if invoked
/// with -frontend.
///
/// \param args the arguments to use as the arguments to the frontend
/// \param argv0 the name used as the frontend executable
/// \param mainAddr an address from the main executable
///
/// \return the exit value of the frontend: 0 or 1 on success unless
///   the frontend executes in immediate mode, in which case this will be
///   the exit value of the script, assuming it exits normally
int performFrontend(ArrayRef<const char *> args,
                    const char *argv0,
                    void *mainAddr,
                    FrontendObserver *observer = nullptr);


} // namespace swift

#endif
