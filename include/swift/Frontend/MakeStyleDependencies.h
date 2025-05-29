//===--- MakeStyleDependencies.h -- header for emitting dependency files --===//
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
//
//  This file defines interface for emitting Make Style Dependency Files.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_MAKESTYLEDEPENDENCIES_H
#define SWIFT_FRONTEND_MAKESTYLEDEPENDENCIES_H

#include "llvm/ADT/StringRef.h"

namespace llvm {
class raw_ostream;
} // namespace llvm

namespace swift {

class CompilerInstance;
class DiagnosticEngine;
class FrontendOptions;
class InputFile;

/// Emit make style dependency file from compiler instance if needed.
bool emitMakeDependenciesIfNeeded(CompilerInstance &instance,
                                  const InputFile &input);

/// Emit make style dependency file from a serialized buffer.
bool emitMakeDependenciesFromSerializedBuffer(llvm::StringRef buffer,
                                              llvm::raw_ostream &os,
                                              const FrontendOptions &opts,
                                              const InputFile &input,
                                              DiagnosticEngine &diags);

} // end namespace swift

#endif
