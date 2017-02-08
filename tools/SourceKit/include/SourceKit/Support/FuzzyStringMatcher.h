//===--- FuzzyStringMatcher.h - ---------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_LIB_SUPPORT_FUZZYSTRINGMATCHER_H
#define LLVM_SOURCEKIT_LIB_SUPPORT_FUZZYSTRINGMATCHER_H

#include "SourceKit/Core/LLVM.h"
#include "llvm/ADT/BitVector.h"
#include <string>

namespace SourceKit {

/// FuzzyStringMatcher compares candidate strings against a pattern
/// string using a fuzzy matching algorithm and provides a numerical
/// score for the match quality.
///
/// The inputs should be UTF8 strings, but the implementation is not currently
/// unicode-correct in that no normalization or non-ASCII upper/lower casing is
/// supported.  Non-ASCII bytes in the input are treated as opaque.
class FuzzyStringMatcher {
  std::string pattern;
  std::string lowercasePattern;
  double maxScore; ///< The maximum possible raw score for this pattern.
  /// If (and only if) c is in pattern, charactersInPattern[c] == 1
  llvm::BitVector charactersInPattern;

public:
  bool normalize = false; ///< Whether to normalize scores to [0, 1].

public:
  FuzzyStringMatcher(StringRef pattern);

  /// Whether \p candidate matches the pattern.
  ///
  /// This operation is much simpler/faster than calculating
  /// the candidate's score.
  bool matchesCandidate(StringRef candidate) const;

  /// Calculates the numerical score for \p candidate.
  double scoreCandidate(StringRef candidate) const;
};

} // end namespace SourceKit

#endif // LLVM_SOURCEKIT_LIB_SUPPORT_FUZZYSTRINGMATCHER_H
