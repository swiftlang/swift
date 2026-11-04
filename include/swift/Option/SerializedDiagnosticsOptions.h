//===--- SerializedDiagnosticsOptions.h - Diagnostics formats ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_OPTIONS_SERIALIZED_DIAGNOSTICS_OPTIONS_H
#define SWIFT_OPTIONS_SERIALIZED_DIAGNOSTICS_OPTIONS_H

#include "swift/Basic/DiagnosticOptions.h"
#include "llvm/ADT/STLFunctionalExtras.h"
#include <optional>

namespace llvm {
namespace opt {
class Arg;
class ArgList;
} // end namespace opt
} // end namespace llvm

namespace swift {

/// The format requested by the last of -serialize-diagnostics, which means
/// 'dia', and -serialize-diagnostics=<format>, or \c std::nullopt if neither
/// is given. Each unknown format is passed to \p onUnknownFormat and skipped.
std::optional<DiagnosticOptions::SerializedFormat>
diagnosticsSerializationFormat(
    const llvm::opt::ArgList &Args,
    llvm::function_ref<void(const llvm::opt::Arg &)> onUnknownFormat);

} // end namespace swift

#endif // SWIFT_OPTIONS_SERIALIZED_DIAGNOSTICS_OPTIONS_H
