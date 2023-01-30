//===--- PrintSwiftToClangCoreScaffold.h - Print core decls -----*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_PRINTSWIFTTOCLANGCORESCAFFOLD_H
#define SWIFT_PRINTASCLANG_PRINTSWIFTTOCLANGCORESCAFFOLD_H

#include "llvm/Support/raw_ostream.h"

namespace swift {

class ASTContext;
class PrimitiveTypeMapping;
class SwiftToClangInteropContext;

/// Print out the core declarations required by C/C++ that are part of the core
/// Swift stdlib code.
void printSwiftToClangCoreScaffold(SwiftToClangInteropContext &ctx,
                                   ASTContext &astContext,
                                   PrimitiveTypeMapping &typeMapping,
                                   llvm::raw_ostream &os);

} // end namespace swift

#endif
