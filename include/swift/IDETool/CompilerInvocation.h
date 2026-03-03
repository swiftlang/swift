//===--- CompilerInvocation.h ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_COMPILERINVOCATION_H
#define SWIFT_IDE_COMPILERINVOCATION_H

#include "swift/Frontend/Frontend.h"

namespace swift {

class CompilerInvocation;

namespace ide {

bool initCompilerInvocation(
    CompilerInvocation &Invocation, ArrayRef<const char *> OrigArgs,
    FrontendOptions::ActionType Action, DiagnosticEngine &Diags,
    StringRef UnresolvedPrimaryFile,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    const std::string &swiftExecutablePath,
    const std::string &runtimeResourcePath, time_t sessionTimestamp,
    std::string &Error);

bool initInvocationByClangArguments(ArrayRef<const char *> ArgList,
                                    CompilerInvocation &Invok,
                                    std::string &Error);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_COMPILERINVOCATION_H
