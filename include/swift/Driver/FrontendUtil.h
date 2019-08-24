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

#include <memory>

namespace swift {

class DiagnosticEngine;

namespace driver {

/// Generates the list of arguments that would be passed to the compiler
/// frontend from the given driver arguments.
///
/// \param ArgList The driver arguments (i.e. normal arguments for \c swiftc).
/// \param Diags The DiagnosticEngine used to report any errors parsing the
/// arguments.
/// \param Action Called with the list of frontend arguments if there were no
/// errors in processing \p ArgList. This is a callback rather than a return
/// value to avoid copying the arguments more than necessary.
///
/// \returns True on error, or if \p Action returns true.
///
/// \note This function is not intended to create invocations which are
/// suitable for use in REPL or immediate modes.
bool getSingleFrontendInvocationFromDriverArguments(
    ArrayRef<const char *> ArgList, DiagnosticEngine &Diags,
    llvm::function_ref<bool(ArrayRef<const char *> FrontendArgs)> Action);

} // end namespace driver
} // end namespace swift

#endif
