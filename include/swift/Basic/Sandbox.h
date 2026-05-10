//===--- Sandbox.h ----------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_SANDBOX_H
#define SWIFT_BASIC_SANDBOX_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"

namespace swift {
namespace Sandbox {

/// Applies a sandbox invocation to the given command line (if the platform
/// supports it), and returns the modified command line. On platforms that don't
/// support sandboxing, the command line is returned unmodified.
///
/// - Parameters:
///   - command: The command line to sandbox (including executable as first
///              argument)
///   - strictness: The basic strictness level of the standbox.
///   - writableDirectories: Paths under which writing should be allowed, even
///     if they would otherwise be read-only based on the strictness or paths in
///     `readOnlyDirectories`.
///   - readOnlyDirectories: Paths under which writing should be denied, even if
///     they would have otherwise been allowed by the rules implied by the
///     strictness level.
bool apply(llvm::SmallVectorImpl<llvm::StringRef> &command,
           llvm::BumpPtrAllocator &Alloc);

} // namespace Sandbox
} // namespace swift

#endif // SWIFT_BASIC_SANDBOX_H
