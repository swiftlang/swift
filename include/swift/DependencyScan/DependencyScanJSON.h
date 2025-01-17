//===-------------- DependencyScanImpl.h - Swift Compiler -----------------===//
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
// Implementation details of the dependency scanning C API
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_DEPENDENCY_SCAN_JSON_H
#define SWIFT_DEPENDENCY_SCAN_JSON_H

#include "swift-c/DependencyScan/DependencyScan.h"
#include "llvm/Support/raw_ostream.h"

namespace swift::dependencies {

void writePrescanJSON(llvm::raw_ostream &out,
                      swiftscan_import_set_t importSet);
void writeJSON(llvm::raw_ostream &out,
               swiftscan_dependency_graph_t fullDependencies);
} // namespace swift::dependencies

#endif // SWIFT_DEPENDENCY_SCAN_JSON_H
