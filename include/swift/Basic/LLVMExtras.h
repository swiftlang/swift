//===--- LLVMExtras.h -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides additional functionality on top of LLVM types
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_LLVMEXTRAS_H
#define SWIFT_BASIC_LLVMEXTRAS_H

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

/// A SetVector that does no allocations under the specified size
///
/// swift::SmallSetVector provides the old SmallSetVector semantics that allow
/// storing types that don't have `operator==`.
template <typename T, unsigned N>
using SmallSetVector = llvm::SetVector<T, llvm::SmallVector<T, N>,
      llvm::SmallDenseSet<T, N, llvm::DenseMapInfo<T>>>;

} // namespace swift

#endif // SWIFT_BASIC_LLVMEXTRAS_H
