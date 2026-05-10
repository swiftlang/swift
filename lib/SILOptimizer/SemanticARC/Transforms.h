//===--- Transforms.h -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Top level transforms for SemanticARCOpts
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_SEMANTICARCOPT_TRANSFORMS_H
#define SWIFT_SILOPTIMIZER_SEMANTICARCOPT_TRANSFORMS_H

#include "llvm/Support/Compiler.h"

namespace swift {
namespace semanticarc {

struct Context;

/// Given the current map of owned phi arguments to consumed incoming values in
/// ctx, attempt to convert these owned phi arguments to guaranteed phi
/// arguments if the phi arguments are the only thing that kept us from
/// converting these incoming values to be guaranteed.
///
/// \returns true if we converted atleast one phi from owned -> guaranteed and
/// eliminated ARC traffic as a result.
LLVM_LIBRARY_VISIBILITY bool tryConvertOwnedPhisToGuaranteedPhis(Context &ctx);

} // namespace semanticarc
} // namespace swift

#endif
