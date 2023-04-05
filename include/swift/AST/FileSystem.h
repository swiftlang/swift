//===--- FileSystem.h - File helpers that interact with Diags ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_FILESYSTEM_H
#define SWIFT_AST_FILESYSTEM_H

#include "swift/Basic/FileSystem.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "llvm/Support/VirtualOutputBackend.h"
#include "llvm/Support/VirtualOutputConfig.h"

namespace swift {
/// A wrapper around llvm::vfs::OutputBackend to handle diagnosing any file
/// system errors during output creation.
///
/// \returns true if there were any errors, either from the filesystem
/// operations or from \p action returning true.
inline bool
withOutputPath(DiagnosticEngine &diags, llvm::vfs::OutputBackend &Backend,
               StringRef outputPath,
               llvm::function_ref<bool(llvm::raw_pwrite_stream &)> action) {
  assert(!outputPath.empty());
  llvm::vfs::OutputConfig config;
  config.setAtomicWrite().setOnlyIfDifferent();

  auto outputFile = Backend.createFile(outputPath, config);
  if (!outputFile) {
    diags.diagnose(SourceLoc(), diag::error_opening_output, outputPath,
                   toString(outputFile.takeError()));
    return true;
  }

  bool failed = action(*outputFile);
  // If there is an error, discard output. Otherwise keep the output file.
  if (auto error = failed ? outputFile->discard() : outputFile->keep()) {
    // Don't diagnose discard error.
    if (failed)
      consumeError(std::move(error));
    else
      diags.diagnose(SourceLoc(), diag::error_closing_output, outputPath,
                     toString(std::move(error)));
    return true;
  }
  return failed;
}
} // end namespace swift

#endif // SWIFT_AST_FILESYSTEM_H
