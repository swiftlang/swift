//===--- CodeCompletionContext.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CodeCompletionContext.h"

using namespace swift;
using namespace swift::ide;

std::vector<CodeCompletionResult *>
CodeCompletionContext::sortCompletionResults(
    ArrayRef<CodeCompletionResult *> Results) {
  std::vector<CodeCompletionResult *> SortedResults(Results.begin(),
                                                    Results.end());

  std::sort(SortedResults.begin(), SortedResults.end(),
            [](const auto &LHS, const auto &RHS) {
              int Result = StringRef(LHS->getFilterName())
                               .compare_insensitive(RHS->getFilterName());
              // If the case insensitive comparison is equal, then secondary
              // sort order should be case sensitive.
              if (Result == 0)
                Result = LHS->getFilterName().compare(RHS->getFilterName());
              return Result < 0;
            });

  return SortedResults;
}
