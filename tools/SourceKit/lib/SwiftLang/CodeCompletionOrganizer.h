//===--- CodeCompletionOrganizer.h - ----------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_LIB_SWIFTLANG_CODECOMPLETIONORGANIZER_H
#define LLVM_SOURCEKIT_LIB_SWIFTLANG_CODECOMPLETIONORGANIZER_H

#include "CodeCompletion.h"
#include "SourceKit/Core/LangSupport.h"
#include "llvm/ADT/StringMap.h"

namespace swift {
class CompilerInvocation;
}

namespace SourceKit {

  using TypeContextKind = swift::ide::CodeCompletionContext::TypeContextKind;

namespace CodeCompletion {

struct Options {
  bool sortByName = false;
  bool useImportDepth = true;
  bool groupOverloads = false;
  bool groupStems = false;
  bool includeExactMatch = true;
  bool addInnerResults = false;
  bool addInnerOperators = true;
  bool addInitsToTopLevel = false;
  bool callPatternHeuristics = true;
  bool hideUnderscores = true;
  bool reallyHideAllUnderscores = false;
  bool hideLowPriority = true;
  bool hideByNameStyle = true;
  bool fuzzyMatching = true;
  bool reuseASTContextIfPossible = false;
  unsigned minFuzzyLength = 2;
  unsigned showTopNonLiteralResults = 3;

  // Options for combining priorities.
  unsigned semanticContextWeight = 15;
  unsigned fuzzyMatchWeight = 10;
  unsigned popularityBonus = 2;
};

struct SwiftCompletionInfo {
  swift::ASTContext *swiftASTContext = nullptr;
  const swift::CompilerInvocation *invocation = nullptr;
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
  CodeCompletionOrganizer(const Options &options, CompletionKind kind,
                          TypeContextKind typeContextKind);
  ~CodeCompletionOrganizer();

  static void
  preSortCompletions(llvm::MutableArrayRef<Completion *> completions);

  /// Add \p completions to the organizer, removing any results that don't match
  /// \p filterText and returning \p exactMatch if there is an exact match.
  ///
  /// Precondition: \p completions should be sorted with preSortCompletions().
  void addCompletionsWithFilter(ArrayRef<Completion *> completions,
                                StringRef filterText, const FilterRules &rules,
                                Completion *&exactMatch);

  void groupAndSort(const Options &options);

  /// Finishes the results and returns them.
  /// For convenience, this returns a shared_ptr, but it is uniquely referenced.
  CodeCompletionViewRef takeResultsView();
};

} // end namespace CodeCompletion
} // end namespace SourceKit

#endif // LLVM_SOURCEKIT_LIB_SWIFTLANG_CODECOMPLETIONORGANIZER_H
