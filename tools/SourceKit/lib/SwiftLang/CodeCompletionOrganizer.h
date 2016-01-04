//===--- CodeCompletionOrganizer.h - ----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKIT_LIB_SWIFTLANG_CODECOMPLETIONORGANIZER_H
#define LLVM_SOURCEKIT_LIB_SWIFTLANG_CODECOMPLETIONORGANIZER_H

#include "CodeCompletion.h"
#include "SourceKit/Core/LangSupport.h"
#include "llvm/ADT/StringMap.h"

namespace swift {
class CompilerInvocation;
}

namespace SourceKit {
namespace CodeCompletion {

struct Options {
  bool sortByName = false;
  bool useImportDepth = true;
  bool groupOverloads = false;
  bool groupStems = false;
  bool includeExactMatch = false;
  bool addInnerResults = false;
  bool addInnerOperators = true;
  bool addInitsToTopLevel = false;
  bool hideUnderscores = true;
  bool reallyHideAllUnderscores = false;
  bool hideLowPriority = true;
  bool hideByNameStyle = true;
  bool fuzzyMatching = true;
  unsigned minFuzzyLength = 2;

  // Options for combining priorities. The defaults are chosen so that a fuzzy
  // match just breaks ties within a semantic context.  If semanticContextWeight
  // isn't modified, a fuzzyMatchWeight of N means that a perfect match is worth
  // the same as the worst possible match N/10 "contexts" ahead of it.
  unsigned semanticContextWeight = 10 * Completion::numSemanticContexts;
  unsigned fuzzyMatchWeight = 9;
  unsigned popularityBonus = 9;
};

struct SwiftCompletionInfo {
  swift::ASTContext *swiftASTContext = nullptr;
  swift::CompilerInvocation *invocation = nullptr;
  swift::ide::CodeCompletionContext *completionContext = nullptr;
};

typedef llvm::StringMap<PopularityFactor> NameToPopularityMap;

std::vector<Completion *>
extendCompletions(ArrayRef<SwiftResult *> swiftResults, CompletionSink &sink,
                  SwiftCompletionInfo &info,
                  const NameToPopularityMap *nameToPopularity,
                  const Options &options, Completion *prefix = nullptr,
                  Optional<SemanticContextKind> overrideContext = None,
                  Optional<SemanticContextKind> overrideOperatorContext = None);

bool addCustomCompletions(CompletionSink &sink,
                          std::vector<Completion *> &completions,
                          ArrayRef<CustomCompletionInfo> customCompletions,
                          CompletionKind completionKind);

class CodeCompletionOrganizer {
  class Impl;
  Impl &impl;
  const Options &options;
public:
  CodeCompletionOrganizer(const Options &options, CompletionKind kind);
  ~CodeCompletionOrganizer();

  static void
  preSortCompletions(llvm::MutableArrayRef<Completion *> completions);

  /// Add \p completions to the organizer, removing any results that don't match
  /// \p filterText and returning \p exactMatch if there is an exact match.
  ///
  /// Precondition: \p completions should be sorted with preSortCompletions().
  void addCompletionsWithFilter(ArrayRef<Completion *> completions,
                                StringRef filterText, Completion *&exactMatch);

  void groupAndSort(const Options &options);

  /// Finishes the results and returns them.
  /// For convenience, this returns a shared_ptr, but it is uniquely referenced.
  CodeCompletionViewRef takeResultsView();
};

} // end namespace CodeCompletion
} // end namespace SourceKit

#endif // LLVM_SOURCEKIT_LIB_SWIFTLANG_CODECOMPLETIONORGANIZER_H
