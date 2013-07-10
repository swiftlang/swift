//===- CodeCompletion.h - Routines for code completion --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CODE_COMPLETION_H
#define SWIFT_IDE_CODE_COMPLETION_H

#include "swift/Basic/LLVM.h"
#include "llvm/Support/Allocator.h"
#include <string>

namespace swift {
class CodeCompletionCallbacksFactory;

namespace code_completion {

/// \brief A routine to remove code completion tokens from code completion
/// tests.
///
/// \code
/// code-completion-token:
///     '#^' identifier '^#'
/// \endcode
///
/// \param Input test source code
/// \param TokenName names the token which position should be returned in
/// \p CompletionOffset.
/// \param CompletionOffset set to ~0U on error, or to a 0-based byte offset on
/// success
///
/// \returns test source code without any code completion tokens.
std::string removeCodeCompletionTokens(StringRef Input,
                                       StringRef TokenName,
                                       unsigned *CompletionOffset);

class CodeCompletionContext {
  llvm::BumpPtrAllocator Allocator;

public:
  /// \brief Allocate a string owned by the code completion context.
  StringRef CopyString(StringRef String);
};

/// \brief A structured representation of a code completion string.
class CodeCompletionString {
  struct Chunk {
    enum ChunkKind {
      TypedText,
      Optional,
      LeftParen,
      RightParen
    };

    unsigned Kind : 8;

    ChunkKind getKind() {
      return ChunkKind(Kind);
    }

    union {
      const char *Text;
      CodeCompletionString *SubString;
    };
  };
};

/// \brief A single code completion result.
class CodeCompletionResult {
  enum ResultKind {
    Declaration,
    Keyword,
    Pattern
  };
  CodeCompletionString CompletionString;

public:
  const CodeCompletionString &getCompletionString() const {
    return CompletionString;
  }
};

/// \brief An abstract base class for consumers of code completion results.
class CodeCompletionConsumer {
public:
  virtual ~CodeCompletionConsumer() {}

  virtual void handleResults(ArrayRef<CodeCompletionResult> Results) = 0;
};

/// \brief A code completion result consumer that prints the results to a
/// \c raw_ostream.
class PrintingCodeCompletionConsumer : public CodeCompletionConsumer {
  llvm::raw_ostream &OS;

public:
  PrintingCodeCompletionConsumer(llvm::raw_ostream &OS)
      : OS(OS) {
  }

  void handleResults(ArrayRef<CodeCompletionResult> Results) override;
};

/// \brief Create a factory for code completion callbacks.
CodeCompletionCallbacksFactory *
makeCodeCompletionCallbacksFactory(CodeCompletionContext &CompletionContext,
                                   CodeCompletionConsumer &Consumer);

} // namespace code_completion
} // namespace swift

#endif // SWIFT_IDE_CODE_COMPLETION_H

