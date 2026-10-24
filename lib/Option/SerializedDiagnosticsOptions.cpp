//===--- SerializedDiagnosticsOptions.cpp - Diagnostics formats -----------===//
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

#include "swift/Option/SerializedDiagnosticsOptions.h"
#include "swift/Option/Options.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"

using namespace swift;
using namespace llvm::opt;

std::optional<DiagnosticOptions::SerializedFormat>
swift::diagnosticsSerializationFormat(
    const ArgList &Args,
    llvm::function_ref<void(const Arg &)> onUnknownFormat) {
  std::optional<DiagnosticOptions::SerializedFormat> format;
  for (const Arg *A : Args.filtered(options::OPT_serialize_diagnostics,
                                    options::OPT_serialize_diagnostics_EQ)) {
    if (A->getOption().matches(options::OPT_serialize_diagnostics))
      format = DiagnosticOptions::SerializedFormat::LLVMBitcode;
    else if (auto named =
                 DiagnosticOptions::serializedFormatFromString(A->getValue()))
      format = named;
    else
      onUnknownFormat(*A);
  }
  return format;
}
