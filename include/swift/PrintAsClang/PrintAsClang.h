//===--- PrintAsClang.h - Emit a header file for a Swift AST ----*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_H
#define SWIFT_PRINTASCLANG_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/Identifier.h"

namespace clang {
class HeaderSearch;
}

namespace swift {
class FrontendOptions;
class IRGenOptions;
class ModuleDecl;
class ValueDecl;

/// Print the exposed declarations in a module into a Clang header.
///
/// The Objective-C compatible declarations are printed into a block that
/// ensures that those declarations are only usable when the header is
/// compiled in Objective-C mode.
/// The C++ compatible declarations are printed into a block that ensures
/// that those declarations are only usable when the header is compiled in
/// C++ mode.
///
/// Returns true on error.
bool printAsClangHeader(raw_ostream &out, ModuleDecl *M,
                        StringRef bridgingHeader,
                        const FrontendOptions &frontendOpts,
                        const IRGenOptions &irGenOpts,
                        clang::HeaderSearch &headerSearchInfo);
}

#endif
