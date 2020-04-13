//===--- InMemoryFrontend.h - Frontend operations, in memory ----*- C++ -*-===//
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

#ifndef SWIFT_LIBINMEMORYFRONTEND_INMEMORYFRONTEND_H
#define SWIFT_LIBINMEMORYFRONTEND_INMEMORYFRONTEND_H

#include "swift/Frontend/Frontend.h"
#include "llvm/Support/MemoryBuffer.h"

namespace swift {
namespace inmemoryfrontend {

/// Given a fully setup CompilerInstance, configured to emit one module, runs
/// the compilation and emits the module to a memory buffer, without writing to
/// the filesystem. Emits error information to the CompilerInstance's
/// DiagnosticEngine.
///
/// \param moduleBuffer will be set to a pointer to the serialized module
///                     buffer. nullptr is allowed, in which case the module
///                     will not be serialized.
/// \param moduleDocBuffer will be set to a pointer to the serialized module
///                        doc buffer. nullptr is allowed, in which case the
///                        module doc will not be serialized.
/// \return true on error.
bool compileSwiftModule(CompilerInstance &CI,
                        std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
                        std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer);

} // end namespace inmemoryfrontend
} // end namespace swift

#endif
