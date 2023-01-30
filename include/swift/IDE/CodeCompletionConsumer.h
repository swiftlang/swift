//===--- CodeCompletionConsumer.h -----------------------------------------===//
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

#ifndef SWIFT_IDE_CODECOMPLETIONCONSUMER
#define SWIFT_IDE_CODECOMPLETIONCONSUMER

#include "swift/IDE/CodeCompletionContext.h"
#include "swift/Parse/IDEInspectionCallbacks.h"

namespace swift {
namespace ide {

struct RequestedCachedModule;

/// An abstract base class for consumers of code completion results.
/// \see \c SimpleCachingCodeCompletionConsumer.
class CodeCompletionConsumer {
public:
  virtual ~CodeCompletionConsumer() {}
  virtual void
  handleResultsAndModules(CodeCompletionContext &context,
                          ArrayRef<RequestedCachedModule> requestedModules,
                          const ExpectedTypeContext *TypeContext,
                          const DeclContext *DC,
                          bool CanCurrDeclContextHandleAsync) = 0;
};

/// A simplified code completion consumer interface that clients can use to get
/// CodeCompletionResults with automatic caching of top-level completions from
/// imported modules.
struct SimpleCachingCodeCompletionConsumer : public CodeCompletionConsumer {

  // Implement the CodeCompletionConsumer interface.
  void handleResultsAndModules(CodeCompletionContext &context,
                               ArrayRef<RequestedCachedModule> requestedModules,
                               const ExpectedTypeContext *TypeContext,
                               const DeclContext *DCForModules,
                               bool CanCurrDeclContextHandleAsync) override;

  /// Clients should override this method to receive \p Results.
  virtual void handleResults(CodeCompletionContext &context) = 0;
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONCONSUMER
