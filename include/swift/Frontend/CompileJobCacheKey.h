//===--- CompileJobCacheKey.h - compile cache key methods -------*- C++ -*-===//
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
//
// This file contains declarations of utility methods for creating cache keys
// for compilation jobs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_COMPILEJOBCACHEKEY_H
#define SWIFT_COMPILEJOBCACHEKEY_H

#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/FileTypes.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Support/Error.h"

namespace swift {

/// Compute CompileJobBaseKey from swift-frontend command-line arguments.
/// CompileJobBaseKey represents the core inputs and arguments, and is used as a
/// base to compute keys for each compiler outputs.
// TODO: switch to create key from CompilerInvocation after we can canonicalize
// arguments.
llvm::Expected<llvm::cas::ObjectRef>
createCompileJobBaseCacheKey(llvm::cas::ObjectStore &CAS,
                             ArrayRef<const char *> Args);

/// Compute CompileJobKey for the compiler outputs. The key for the output
/// is computed from the base key for the compilation, the output kind and the
/// input file path that is associated with this specific output.
llvm::Expected<llvm::cas::ObjectRef>
createCompileJobCacheKeyForOutput(llvm::cas::ObjectStore &CAS,
                                  llvm::cas::ObjectRef BaseKey,
                                  StringRef ProducingInput,
                                  file_types::ID OutputType);
}

#endif
