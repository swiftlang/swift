//===--- FrontendUtil.h - Driver Utilities for Frontend ---------*- C++ -*-===//
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

#ifndef SWIFT_DRIVER_FRONTENDUTIL_H
#define SWIFT_DRIVER_FRONTENDUTIL_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/StringSaver.h"

#include <memory>

namespace swift {

class DiagnosticEngine;

namespace driver {
/// Expand response files in the argument list with retrying.
/// This function is a wrapper of lvm::cl::ExpandResponseFiles. It will
/// retry calling the function if the previous expansion failed.
void ExpandResponseFilesWithRetry(llvm::StringSaver &Saver,
                                  llvm::SmallVectorImpl<const char *> &Args);

/// Generates the list of arguments that would be passed to the compiler
/// frontend from the given driver arguments.
///
/// \param DriverPath The path to 'swiftc'.
/// \param ArgList The driver arguments (i.e. normal arguments for \c swiftc).
/// \param Diags The DiagnosticEngine used to report any errors parsing the
/// arguments.
/// \param Action Called with the list of frontend arguments if there were no
/// errors in processing \p ArgList. This is a callback rather than a return
/// value to avoid copying the arguments more than necessary.
/// \param ForceNoOutputs If true, override the output mode to "-typecheck" and
/// produce no outputs. For example, this disables "-emit-module" and "-c" and
/// prevents the creation of temporary files.
///
/// \returns True on error, or if \p Action returns true.
///
/// \note This function is not intended to create invocations which are
/// suitable for use in REPL or immediate modes.
bool getSingleFrontendInvocationFromDriverArguments(
    StringRef DriverPath, ArrayRef<const char *> ArgList,
    DiagnosticEngine &Diags,
    llvm::function_ref<bool(ArrayRef<const char *> FrontendArgs)> Action,
    bool ForceNoOutputs = false);

} // end namespace driver
} // end namespace swift

#endif
