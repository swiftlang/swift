//===--- ClangIncludePaths.h ------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CLANG_INCLUDE_PATHS_H
#define SWIFT_CLANG_INCLUDE_PATHS_H

#include "swift/AST/ASTContext.h"

namespace swift {

/// On Linux, some platform libraries (glibc, libstdc++) are not modularized.
/// We inject modulemaps for those libraries into their include directories
/// to allow using them from Swift.
SmallVector<std::pair<std::string, std::string>, 2>
getClangInvocationFileMapping(ASTContext &ctx);

Optional<SmallString<128>> getCxxShimModuleMapPath(SearchPathOptions &opts,
                                                   const llvm::Triple &triple);

} // namespace swift

#endif // SWIFT_CLANG_INCLUDE_PATHS_H
