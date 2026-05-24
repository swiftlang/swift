//===--- ModuleImportExtractor.h - Extract imports from .swiftmodule ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_MODULEIMPORTEXTRACTOR_H
#define SWIFT_SERIALIZATION_MODULEIMPORTEXTRACTOR_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

/// Load a .swiftmodule and extract its import dependencies as text.
/// Emits a valid Swift source file containing equivalent import statements.
/// Returns 0 on success, 1 on failure (with error printed to errs()).
int extractModuleImports(StringRef modulePath, llvm::raw_ostream &out);

} // end namespace swift

#endif // SWIFT_SERIALIZATION_MODULEIMPORTEXTRACTOR_H
