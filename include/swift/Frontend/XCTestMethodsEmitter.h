//===--- ParseableInterfaceSupport.h - swiftinterface files -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_XCTESTMETHODEMITTER_H
#define SWIFT_FRONTEND_XCTESTMETHODEMITTER_H

#include "swift/Basic/LLVM.h"

namespace swift {

class ModuleDecl;

/// Emit a JSON containing XCTest test methods for \p M.
///
/// This is useful for build systems to generate entry point for corelibs-xctest
/// that is used on Linux. Eventually, this can be removed once we have runtime
/// reflection support on Linux.
///
///
/// \return true if an error occurred
bool emitXCTestMethods(raw_ostream &out, ModuleDecl *M);

} // end namespace swift

#endif
