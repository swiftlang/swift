//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKITDINPROC_INTERNAL_H
#define LLVM_SOURCEKITDINPROC_INTERNAL_H

#include <string>

namespace sourcekitdInProc {
std::string getRuntimeLibPath();
std::string getSwiftExecutablePath();
} // namespace sourcekitdInProc

#endif
