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
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"
#include <string>
#include <vector>

namespace clang {
  class Decl;
} // namespace clang

namespace swift {
class CodeCompletionCallbacksFactory;
class Decl;

namespace code_completion {

class CodeCompletionContext;
class CodeCompletionResultBuilder;

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

/// \brief A structured representation of a code completion string.
class CodeCompletionString {
  friend class CodeCompletionResultBuilder;

  class Chunk {
    friend class CodeCompletionResultBuilder;

  public:
    enum class ChunkKind {
      /// Normal text chunk.
      Text,

      /// The first chunk of an optional substring that continues until
      /// \c NestingLevel decreases.
      OptionalBegin,

      // Punctuation.
      LeftParen,
      RightParen,
      Dot,
      Comma,

      /// The first chunk of a substring that describes the parameter for a
      /// function call.
      CallParameterBegin,
      /// Function call parameter name.  Can be omitted in the editor buffer.
      CallParameterName,
      /// A colon between parameter name and value.
      CallParameterColon,
      /// Required parameter type.
      CallParameterType,

      /// Specifies the type of the whole entity that is returned in this code
      /// completion result.  For example, for variable references it is the
      /// variable type, for function calls it is the return type.
      ///
      /// This chunk should not be inserted into the editor buffer.
      TypeAnnotation
    };

    static bool chunkHasText(ChunkKind Kind) {
      return Kind == ChunkKind::Text ||
             Kind == ChunkKind::LeftParen ||
             Kind == ChunkKind::RightParen ||
             Kind == ChunkKind::Dot ||
             Kind == ChunkKind::Comma ||
             Kind == ChunkKind::CallParameterName ||
             Kind == ChunkKind::CallParameterColon ||
             Kind == ChunkKind::CallParameterType ||
             Kind == ChunkKind::TypeAnnotation;
    }

  private:
    unsigned Kind : 8;
    unsigned NestingLevel : 8;

    StringRef Text;

    Chunk(ChunkKind Kind, unsigned NestingLevel, StringRef Text)
        : Kind(unsigned(Kind)), NestingLevel(NestingLevel), Text(Text) {
      assert(chunkHasText(Kind));
    }

    Chunk(ChunkKind Kind, unsigned NestingLevel)
        : Kind(unsigned(Kind)), NestingLevel(NestingLevel) {
      assert(!chunkHasText(Kind));
    }

public:
    ChunkKind getKind() const {
      return ChunkKind(Kind);
    }

    unsigned getNestingLevel() const {
      return NestingLevel;
    }

    StringRef getText() const {
      assert(chunkHasText(getKind()));
      return Text;
    }

    static Chunk createWithText(ChunkKind Kind, unsigned NestingLevel,
                                StringRef Text) {
      return Chunk(Kind, NestingLevel, Text);
    }

    static Chunk createSimple(ChunkKind Kind, unsigned NestingLevel) {
      return Chunk(Kind, NestingLevel);
    }
  };

  unsigned NumChunks : 16;

  CodeCompletionString(ArrayRef<Chunk> Chunks);

public:
  ArrayRef<Chunk> getChunks() const {
    return llvm::makeArrayRef(reinterpret_cast<const Chunk *>(this + 1),
                              NumChunks);
  }

  std::string getAsString() const;
};

/// \brief A single code completion result.
class CodeCompletionResult {
  friend class CodeCompletionResultBuilder;

public:
  enum ResultKind {
    SwiftDeclaration,
    ClangDeclaration,
    Keyword,
    Pattern
  };

private:
  const ResultKind Kind;
  CodeCompletionString *const CompletionString;
  llvm::PointerUnion<const Decl *, const clang::Decl *> AssociatedDecl;

  CodeCompletionResult(ResultKind Kind,
                       CodeCompletionString *CompletionString)
    : Kind(Kind), CompletionString(CompletionString) {
  }

  CodeCompletionResult(CodeCompletionString *CompletionString,
                       const Decl *AssociatedSwiftDecl)
    : CodeCompletionResult(ResultKind::SwiftDeclaration, CompletionString) {
    assert(AssociatedSwiftDecl && "should have a decl");
    AssociatedDecl = AssociatedSwiftDecl;
  }

  CodeCompletionResult(CodeCompletionString *CompletionString,
                       const clang::Decl *AssociatedClangDecl)
    : CodeCompletionResult(ResultKind::ClangDeclaration, CompletionString) {
    assert(AssociatedClangDecl && "should have a decl");
    AssociatedDecl = AssociatedClangDecl;
  }

public:
  const CodeCompletionString *getCompletionString() const {
    return CompletionString;
  }

  const Decl *getAssociatedSwiftDecl() const {
    return AssociatedDecl.get<const Decl *>();
  }

  const clang::Decl *getAssociatedClangDecl() const {
    return AssociatedDecl.get<const clang::Decl *>();
  }

  std::string getAsString() const;
};

class CodeCompletionResultBuilder {
  CodeCompletionContext &Context;
  CodeCompletionResult::ResultKind Kind;
  llvm::PointerUnion<const Decl *, const clang::Decl *> AssociatedDecl;
  unsigned CurrentNestingLevel = 0;
  SmallVector<CodeCompletionString::Chunk, 4> Chunks;

  void addChunkWithText(CodeCompletionString::Chunk::ChunkKind Kind,
                        StringRef Text);

  void addSimpleChunk(CodeCompletionString::Chunk::ChunkKind Kind) {
    Chunks.push_back(
        CodeCompletionString::Chunk::createSimple(Kind,
                                                  CurrentNestingLevel));
  }

  CodeCompletionResult *takeResult();
  void finishResult();

public:
  CodeCompletionResultBuilder(CodeCompletionContext &Context,
                              CodeCompletionResult::ResultKind Kind)
      : Context(Context), Kind(Kind) {
  }

  ~CodeCompletionResultBuilder() {
    finishResult();
  }

  void setAssociatedSwiftDecl(const Decl *D) {
    assert(Kind == CodeCompletionResult::ResultKind::SwiftDeclaration);
    AssociatedDecl = D;
  }

  void setAssociatedClangDecl(const clang::Decl *D) {
    assert(Kind == CodeCompletionResult::ResultKind::ClangDeclaration);
    AssociatedDecl = D;
  }

  void addTextChunk(StringRef Text) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Text, Text);
  }

  void addLeftParen() {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::LeftParen, "(");
  }

  void addRightParen() {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::RightParen, ")");
  }

  void addDot() {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Dot, ".");
  }

  void addComma(StringRef Text) {
    if (Text.empty())
      Text = ",";
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Comma, Text);
  }

  void addCallParameter(StringRef Name, StringRef Type) {
    CurrentNestingLevel++;
    addSimpleChunk(CodeCompletionString::Chunk::ChunkKind::CallParameterBegin);
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::CallParameterName, Name);
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::CallParameterColon, ": ");
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::CallParameterType, Type);
    CurrentNestingLevel--;
  }

  void addTypeAnnotation(StringRef Type) {
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::TypeAnnotation, Type);
  }
};

class CodeCompletionContext {
  friend class CodeCompletionResultBuilder;
  llvm::BumpPtrAllocator Allocator;
  std::vector<CodeCompletionResult *> CurrentCompletionResults;

public:
  /// \brief Allocate a string owned by the code completion context.
  StringRef copyString(StringRef String);

  ArrayRef<CodeCompletionResult *> takeResults();
};

/// \brief An abstract base class for consumers of code completion results.
class CodeCompletionConsumer {
public:
  virtual ~CodeCompletionConsumer() {}

  virtual void handleResults(ArrayRef<CodeCompletionResult *> Results) = 0;
};

/// \brief A code completion result consumer that prints the results to a
/// \c raw_ostream.
class PrintingCodeCompletionConsumer : public CodeCompletionConsumer {
  llvm::raw_ostream &OS;

public:
  PrintingCodeCompletionConsumer(llvm::raw_ostream &OS)
      : OS(OS) {
  }

  void handleResults(ArrayRef<CodeCompletionResult *> Results) override;
};

/// \brief Create a factory for code completion callbacks.
CodeCompletionCallbacksFactory *
makeCodeCompletionCallbacksFactory(CodeCompletionContext &CompletionContext,
                                   CodeCompletionConsumer &Consumer);

} // namespace code_completion
} // namespace swift

#endif // SWIFT_IDE_CODE_COMPLETION_H

