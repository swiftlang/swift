//===--- Replacement.h - Textual Migrator Replacement -----------*- C++ -*-===//
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

#ifndef SWIFT_MIGRATOR_REPLACEMENT_H
#define SWIFT_MIGRATOR_REPLACEMENT_H

#include "swift/AST/DiagnosticConsumer.h"

namespace swift {
namespace migrator {

class Replacement {
  static void printReplacements(llvm::raw_ostream &OS,
                                ArrayRef<Replacement> Replacements);
public:
  /// The filename to which this textual replacement applies.
  const StringRef Filename;

  /// The original offset in the file to start the replacement
  const size_t OrigOffset;

  /// How many bytes from the original offset to replace.
  const size_t OrigLength;

  /// The text we are putting in as a replacement.
  const std::string ReplacementText;

  /// A diagnostic ID that suggested this replacement, if applicable.
  const llvm::Optional<DiagID> ID;

  Replacement(const StringRef Filename,
              const size_t OrigOffset,
              const size_t OrigLength,
              const std::string &ReplacementText,
              const llvm::Optional<DiagID> ID)
    : Filename(Filename),
      OrigOffset(OrigOffset),
      OrigLength(OrigLength),
      ReplacementText(ReplacementText),
      ID(ID) {}

  /// Print a JSON object describing this replacement.
  void printJSON(llvm::raw_ostream &OS) const;

  /// Emit the replacement map for all of the replacements provided
  /// to OutFilename.
  ///
  /// Returns true if failed.
  static bool emitRemap(StringRef OutFilename,
                        ArrayRef<Replacement> Replacements);
};

} // end namespace swift
} // end namespace migrator

#endif // SWIFT_MIGRATOR_REPLACEMENT_H
