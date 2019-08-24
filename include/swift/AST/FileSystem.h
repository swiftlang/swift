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

namespace swift {

/// A wrapper around swift::atomicallyWritingToFile that handles diagnosing any
/// filesystem errors and asserts the output path is nonempty.
///
/// \returns true if there were any errors, either from the filesystem
/// operations or from \p action returning true.
inline bool
withOutputFile(DiagnosticEngine &diags, StringRef outputPath,
               llvm::function_ref<bool(llvm::raw_pwrite_stream &)> action) {
  assert(!outputPath.empty());

  bool actionFailed = false;
  std::error_code EC = swift::atomicallyWritingToFile(
      outputPath,
      [&](llvm::raw_pwrite_stream &out) { actionFailed = action(out); });
  if (EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output, outputPath,
                   EC.message());
    return true;
  }
  return actionFailed;
}
} // end namespace swift

#endif // SWIFT_AST_FILESYSTEM_H
