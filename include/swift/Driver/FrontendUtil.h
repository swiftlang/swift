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

#include <memory>

namespace swift {

class CompilerInvocation;
class DiagnosticEngine;

namespace driver {

/// \brief Creates a CompilerInvocation from the given driver arguments.
///
/// \param ArgList The driver arguments for which a CompilerInvocation
/// should be created.
/// \param Diags The DiagnosticEngine which should be used for parsing arguments
///
/// \returns A fully-formed CompilerInvocation, or nullptr if one couldn't be
/// created.
///
/// \note This function is not intended to create CompilerInvocation instances
/// which are suitable for use in REPL or immediate modes, since it will have
/// the effect of overriding the frontend's requested action to
/// FrontendOptions::ActionType::Parse.
std::unique_ptr<CompilerInvocation> createCompilerInvocation(
    ArrayRef<const char *> ArgList, DiagnosticEngine &Diags);

} // end namespace driver
} // end namespace swift

#endif
