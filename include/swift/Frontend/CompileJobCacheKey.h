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

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/CAS/CASReference.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

/// The indices of the references at the fixed positions in CompileJobBaseKey.
enum class CompileJobBaseKeyRef : unsigned {
  /// The compiler version.
  Version,
  /// The command-line arguments that are stable across the jobs in a module.
  CommandLine,
  /// The clang arguments (-Xcc).
  ClangArguments,
  /// The clang include tree root, or an empty blob if not used.
  IncludeTreeRoot,
  /// The clang include tree file list, or an empty blob if not used.
  IncludeTreeFileList,
  /// The number of the references at the fixed positions.
  NumFixedRefs,
};

/// Compute CompileJobBaseKey from swift-frontend command-line arguments.
/// CompileJobBaseKey represents the core inputs and arguments, and is used as a
/// base to compute keys for each compiler outputs. The CAS IDs passed by the
/// options labeled as ArgumentIsCASID are added to the key as references, and
/// it is an error if the referenced object is not in the CAS.
// TODO: switch to create key from CompilerInvocation after we can canonicalize
// arguments.
llvm::Expected<llvm::cas::ObjectRef>
createCompileJobBaseCacheKey(llvm::cas::ObjectStore &CAS,
                             ArrayRef<const char *> Args);

/// Compute CompileJobKey for the compiler outputs. The key for the output
/// is computed from the base key for the compilation and the input file index
/// which is the index for the input among all the input files (not just the
/// output producing inputs).
llvm::Expected<llvm::cas::ObjectRef>
createCompileJobCacheKeyForOutput(llvm::cas::ObjectStore &CAS,
                                  llvm::cas::ObjectRef BaseKey,
                                  unsigned InputIndex);

/// Print the CompileJobKey for debugging purpose.
llvm::Error printCompileJobCacheKey(llvm::cas::ObjectStore &CAS,
                                    llvm::cas::ObjectRef Key,
                                    llvm::raw_ostream &os);

} // namespace swift

#endif
