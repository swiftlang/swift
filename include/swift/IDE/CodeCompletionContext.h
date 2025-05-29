//===--- CodeCompletionContext.h ------------------------------------------===//
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

#ifndef SWIFT_IDE_CODECOMPLETIONCONTEXT
#define SWIFT_IDE_CODECOMPLETIONCONTEXT

#include "swift/IDE/CodeCompletionResult.h"
#include "swift/IDE/CodeCompletionResultSink.h"

namespace swift {
namespace ide {

class CodeCompletionCache;
struct RequestedCachedModule;

class CodeCompletionContext {
  friend class CodeCompletionResultBuilder;

  /// A set of current completion results.
  CodeCompletionResultSink CurrentResults;

public:
  CodeCompletionCache &Cache;
  CompletionKind CodeCompletionKind = CompletionKind::None;

  /// Module qualified nominal type decl names
  SmallVector<NullTerminatedStringRef, 2> LookedupNominalTypeNames;

  enum class TypeContextKind {
    /// There is no known contextual type. All types are equally good.
    None,

    /// There is a contextual type from e.g a single-expression closure/function
    /// body, where the return is implied. The context is a hint, and enables
    /// unresolved member completion, but should not hide any results.
    Implied,

    /// There are known contextual types, or there aren't but a nonvoid type is
    /// expected.
    Required,
  };

  TypeContextKind typeContextKind = TypeContextKind::None;

  /// Whether there may be members that can use implicit member syntax,
  /// e.g. `x = .foo`.
  bool MayUseImplicitMemberExpr = false;

  /// Flag to indicate that the completion is happening reusing ASTContext
  /// from the previous completion.
  /// NOTE: Do not use this to change the behavior. This is only for debugging.
  bool ReusingASTContext = false;

  CodeCompletionContext(CodeCompletionCache &Cache) : Cache(Cache) {}

  void setAnnotateResult(bool flag) { CurrentResults.annotateResult = flag; }
  bool getAnnotateResult() const { return CurrentResults.annotateResult; }

  void setIncludeObjectLiterals(bool flag) {
    CurrentResults.includeObjectLiterals = flag;
  }
  bool includeObjectLiterals() const {
    return CurrentResults.includeObjectLiterals;
  }

  void setAddInitsToTopLevel(bool flag) {
    CurrentResults.addInitsToTopLevel = flag;
  }
  bool getAddInitsToTopLevel() const {
    return CurrentResults.addInitsToTopLevel;
  }

  void setAddCallWithNoDefaultArgs(bool flag) {
    CurrentResults.addCallWithNoDefaultArgs = flag;
  }
  bool addCallWithNoDefaultArgs() const {
    return CurrentResults.addCallWithNoDefaultArgs;
  }

  /// Allocate a string owned by the code completion context.
  StringRef copyString(StringRef Str) {
    return Str.copy(*CurrentResults.Allocator);
  }

  /// Sort code completion results in an implementation-defined order
  /// in place.
  static std::vector<CodeCompletionResult *>
  sortCompletionResults(ArrayRef<CodeCompletionResult *> Results);

  CodeCompletionResultSink &getResultSink() { return CurrentResults; }

  /// Add code completion results from the given requested modules to this
  /// context.
  void addResultsFromModules(ArrayRef<RequestedCachedModule> RequestedModules,
                             const ExpectedTypeContext &TypeContext,
                             const DeclContext *DC,
                             bool CanCurrDeclContextHandleAsync);
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONCONTEXT
