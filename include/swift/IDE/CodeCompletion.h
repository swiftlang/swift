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

#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"
#include <memory>
#include <string>
#include <vector>

namespace swift {
class CodeCompletionCallbacksFactory;
class Decl;

namespace ide {

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

public:
  class Chunk {
    friend class CodeCompletionResultBuilder;

  public:
    enum class ChunkKind {
      /// "internal", "private" or "public".
      AccessControlKeyword,

      /// such as @"availability"
      DeclAttrKeyword,

      /// such as "unavailable" etc. for @availability
      DeclAttrParamKeyword,

      /// The "override" keyword.
      OverrideKeyword,

      /// The keyword part of a declaration before the name, like "func".
      DeclIntroducer,

      /// Normal text chunk.
      Text,

      /// The first chunk of an optional substring that continues until
      /// \c NestingLevel decreases.
      OptionalBegin,

      // Punctuation.
      LeftParen,
      RightParen,
      LeftBracket,
      RightBracket,
      LeftAngle,
      RightAngle,
      Dot,
      Ellipsis,
      Comma,
      ExclamationMark,
      QuestionMark,
      Ampersand,

      /// The first chunk of a substring that describes the parameter for a
      /// generic type.
      GenericParameterBegin,
      /// Generic type parameter name.
      GenericParameterName,

      /// The first chunk of a substring that describes the parameter for a
      /// function call.
      CallParameterBegin,
      /// Function call parameter name.
      CallParameterName,
      /// Function call parameter internal / local name. If the parameter has no
      /// formal API name, it can still have a local name which can be useful
      /// for display purposes.
      ///
      /// This chunk should not be inserted into the editor buffer.
      CallParameterInternalName,
      /// A colon between parameter name and value.  Should be inserted in the
      /// editor buffer if the preceding CallParameterName was inserted.
      CallParameterColon,

      /// A equal sign between parameter name and value. Used in decl attribute.
      DeclAttrParamEqual,

      /// Required parameter type.
      CallParameterType,
      /// Desugared closure parameter type. This can be used to get the
      /// closure type if CallParameterType is a NameAliasType.
      CallParameterClosureType,

      /// A placeholder for \c ! or \c ? in a call to a method found by dynamic
      /// lookup.
      ///
      /// The default spelling is \c !, but clients may render it as \c ? if
      /// desired.
      DynamicLookupMethodCallTail,

      /// A placeholder for \c ! or \c ? in a call to an optional method.
      ///
      /// The default spelling is \c !, but clients may render it as \c ? if
      /// desired.
      OptionalMethodCallTail,

      /// Specifies the type of the whole entity that is returned in this code
      /// completion result.  For example, for variable references it is the
      /// variable type, for function calls it is the return type.
      ///
      /// This chunk should not be inserted into the editor buffer.
      TypeAnnotation,

      /// A brace statement -- left brace and right brace.  The preferred
      /// position to put the cursor after the completion result is inserted
      /// into the editor buffer is between the braces.
      ///
      /// The spelling as always "{}", but clients may choose to insert newline
      /// and indentation in between.
      BraceStmtWithCursor,
    };

    static bool chunkStartsNestedGroup(ChunkKind Kind) {
      return Kind == ChunkKind::CallParameterBegin ||
             Kind == ChunkKind::GenericParameterBegin ||
             Kind == ChunkKind::OptionalBegin;
    }

    static bool chunkHasText(ChunkKind Kind) {
      return Kind == ChunkKind::AccessControlKeyword ||
             Kind == ChunkKind::OverrideKeyword ||
             Kind == ChunkKind::DeclAttrKeyword ||
             Kind == ChunkKind::DeclIntroducer ||
             Kind == ChunkKind::Text ||
             Kind == ChunkKind::LeftParen ||
             Kind == ChunkKind::RightParen ||
             Kind == ChunkKind::LeftBracket ||
             Kind == ChunkKind::RightBracket ||
             Kind == ChunkKind::LeftAngle ||
             Kind == ChunkKind::RightAngle ||
             Kind == ChunkKind::Dot ||
             Kind == ChunkKind::Ellipsis ||
             Kind == ChunkKind::Comma ||
             Kind == ChunkKind::ExclamationMark ||
             Kind == ChunkKind::QuestionMark ||
             Kind == ChunkKind::Ampersand ||
             Kind == ChunkKind::CallParameterName ||
             Kind == ChunkKind::CallParameterInternalName ||
             Kind == ChunkKind::CallParameterColon ||
             Kind == ChunkKind::DeclAttrParamEqual ||
             Kind == ChunkKind::DeclAttrParamKeyword ||
             Kind == ChunkKind::CallParameterType ||
             Kind == ChunkKind::CallParameterClosureType ||
             Kind == ChunkKind::GenericParameterName ||
             Kind == ChunkKind::DynamicLookupMethodCallTail ||
             Kind == ChunkKind::OptionalMethodCallTail ||
             Kind == ChunkKind::TypeAnnotation ||
             Kind == ChunkKind::BraceStmtWithCursor;
    }

  private:
    unsigned Kind : 8;
    unsigned NestingLevel : 8;

    /// \brief If true, then this chunk is an annotation that is included only
    /// for exposition and may not be inserted in the editor buffer.
    unsigned IsAnnotation : 1;

    StringRef Text;

    Chunk(ChunkKind Kind, unsigned NestingLevel, StringRef Text)
        : Kind(unsigned(Kind)), NestingLevel(NestingLevel), IsAnnotation(0),
          Text(Text) {
      assert(chunkHasText(Kind));
    }

    Chunk(ChunkKind Kind, unsigned NestingLevel)
        : Kind(unsigned(Kind)), NestingLevel(NestingLevel), IsAnnotation(0) {
      assert(!chunkHasText(Kind));
    }

    void setIsAnnotation() {
      IsAnnotation = 1;
    }

  public:
    ChunkKind getKind() const {
      return ChunkKind(Kind);
    }

    bool is(ChunkKind K) const { return getKind() == K; }

    unsigned getNestingLevel() const {
      return NestingLevel;
    }

    bool isAnnotation() const {
      return IsAnnotation;
    }

    bool hasText() const { return chunkHasText(getKind()); }

    StringRef getText() const {
      assert(hasText());
      return Text;
    }

    bool endsPreviousNestedGroup(unsigned GroupNestingLevel) const {
      return NestingLevel < GroupNestingLevel ||
       (NestingLevel == GroupNestingLevel && chunkStartsNestedGroup(getKind()));
    }

    static Chunk createWithText(ChunkKind Kind, unsigned NestingLevel,
                                StringRef Text) {
      return Chunk(Kind, NestingLevel, Text);
    }

    static Chunk createSimple(ChunkKind Kind, unsigned NestingLevel) {
      return Chunk(Kind, NestingLevel);
    }
  };

private:
  unsigned NumChunks : 16;

  CodeCompletionString(ArrayRef<Chunk> Chunks);

public:
  ArrayRef<Chunk> getChunks() const {
    return llvm::makeArrayRef(reinterpret_cast<const Chunk *>(this + 1),
                              NumChunks);
  }

  StringRef getFirstTextChunk() const;
  Optional<unsigned> getFirstTextChunkIndex() const;

  /// Concatenates all text chunks considered part of the name to \p OS.
  void getName(raw_ostream &OS) const;

  /// Print a debug representation of the code completion string to \p OS.
  void print(raw_ostream &OS) const;
  void dump() const;
};

/// \brief Describes the origin of the code completion result.
///
/// This enum is ordered from the contexts that are "nearest" to the code
/// completion point to "outside" contexts.
enum class SemanticContextKind {
  /// Used in cases when the concept of semantic context is not applicable.
  None,

  /// \brief This is a highly-likely expression-context-specific completion
  /// result.  This description is intentionally vague: this is a catch-all
  /// category for all heuristics for highly-likely results.
  ///
  /// For example, the name of an overridden superclass member inside a nominal
  /// member function has ExpressionSpecific context:
  /// \code
  ///   class Base {
  ///     init() {}
  ///     init(a: Int) {}
  ///     func foo() {}
  ///     func bar() {}
  ///   }
  ///   class Derived {
  ///     init() {
  ///       super. // init() -- ExpressionSpecific
  ///              // init(a: Int) -- Super
  ///     }
  ///
  ///     func foo() {
  ///       super. // foo() -- ExpressionSpecific
  ///              // bar() -- Super
  ///     }
  ///   }
  /// \endcode
  ///
  /// In C-style for loop headers the iteration variable has ExpressionSpecific
  /// context:
  /// \code
  ///   for var foo = 0; #^A^# // foo -- ExpressionSpecific
  /// \endcode
  ExpressionSpecific,

  /// A declaration from the same function.
  Local,

  /// A declaration found in the immediately enclosing nominal decl.
  CurrentNominal,

  /// A declaration found in the superclass of the immediately enclosing
  /// nominal decl.
  Super,

  /// A declaration found in the non-immediately enclosing nominal decl.
  ///
  /// For example, 'Foo' is visible at (1) because of this.
  /// \code
  ///   struct A {
  ///     typealias Foo = Int
  ///     struct B {
  ///       func foo() {
  ///         // (1)
  ///       }
  ///     }
  ///   }
  /// \endcode
  OutsideNominal,

  /// A declaration from the current module.
  CurrentModule,

  /// A declaration imported from other module.
  OtherModule,
};

/// The declaration kind of a code completion result, if it is a declaration.
enum class CodeCompletionDeclKind {
  Class,
  Struct,
  Enum,
  EnumElement,
  Protocol,
  TypeAlias,
  GenericTypeParam,
  Constructor,
  Destructor,
  Subscript,
  StaticMethod,
  InstanceMethod,
  OperatorFunction,
  FreeFunction,
  StaticVar,
  InstanceVar,
  LocalVar,
  GlobalVar,
};

/// \brief A single code completion result.
class CodeCompletionResult {
  friend class CodeCompletionResultBuilder;

public:
  enum ResultKind {
    Declaration,
    Keyword,
    Pattern
  };

private:
  unsigned Kind : 2;
  unsigned AssociatedDeclKind : 8;
  unsigned SemanticContext : 3;
  unsigned NotRecommended : 1;

  /// The number of bytes to the left of the code completion point that
  /// should be erased first if this completion string is inserted in the
  /// editor buffer.
  unsigned NumBytesToErase : 7;

public:
  static const unsigned MaxNumBytesToErase = 127;

private:
  CodeCompletionString *const CompletionString;
  StringRef BriefDocComment;
  ArrayRef<StringRef> AssociatedUSRs;

  CodeCompletionResult(ResultKind Kind,
                       SemanticContextKind SemanticContext,
                       unsigned NumBytesToErase,
                       CodeCompletionString *CompletionString)
      : Kind(Kind), SemanticContext(unsigned(SemanticContext)),
        NotRecommended(false), NumBytesToErase(NumBytesToErase),
        CompletionString(CompletionString) {
    assert(Kind != Declaration && "use the other constructor");
    assert(CompletionString);
  }

  CodeCompletionResult(SemanticContextKind SemanticContext,
                       unsigned NumBytesToErase,
                       CodeCompletionString *CompletionString,
                       const Decl *AssociatedDecl,
                       bool NotRecommended,
                       StringRef BriefDocComment,
                       ArrayRef<StringRef> AssociatedUSRs)
      : Kind(ResultKind::Declaration),
        SemanticContext(unsigned(SemanticContext)),
        NotRecommended(NotRecommended),
        NumBytesToErase(NumBytesToErase),
        CompletionString(CompletionString),
        BriefDocComment(BriefDocComment),
        AssociatedUSRs(AssociatedUSRs) {
    assert(AssociatedDecl && "should have a decl");
    AssociatedDeclKind = unsigned(getCodeCompletionDeclKind(AssociatedDecl));
    assert(CompletionString);
  }

public:
  ResultKind getKind() const { return static_cast<ResultKind>(Kind); }

  CodeCompletionDeclKind getAssociatedDeclKind() const {
    assert(getKind() == Declaration);
    return static_cast<CodeCompletionDeclKind>(AssociatedDeclKind);
  }

  SemanticContextKind getSemanticContext() const {
    return static_cast<SemanticContextKind>(SemanticContext);
  }

  bool isNotRecommended() const {
    return NotRecommended;
  }

  unsigned getNumBytesToErase() const {
    return NumBytesToErase;
  }

  const CodeCompletionString *getCompletionString() const {
    return CompletionString;
  }

  StringRef getBriefDocComment() const {
    return BriefDocComment;
  }

  ArrayRef<StringRef> getAssociatedUSRs() const {
    return AssociatedUSRs;
  }

  /// Print a debug representation of the code completion result to \p OS.
  void print(raw_ostream &OS) const;
  void dump() const;

  static CodeCompletionDeclKind getCodeCompletionDeclKind(const Decl *D);
};

struct CodeCompletionResultSink {
  using AllocatorPtr = std::shared_ptr<llvm::BumpPtrAllocator>;

  /// The allocator used to allocate results "native" to this sink.
  AllocatorPtr Allocator;

  /// Allocators that keep alive "foreign" results imported into this sink from
  /// other sinks.
  std::vector<AllocatorPtr> ForeignAllocators;

  std::vector<CodeCompletionResult *> Results;

  CodeCompletionResultSink()
      : Allocator(std::make_shared<llvm::BumpPtrAllocator>()) {}
};

struct CodeCompletionCacheImpl;

/// \brief Per-module code completion result cache.
///
/// These results persist between multiple code completion requests and can be
/// used with different ASTContexts.
struct CodeCompletionCache {
  std::unique_ptr<CodeCompletionCacheImpl> Impl;

  CodeCompletionCache();
  ~CodeCompletionCache();
};

class CodeCompletionContext {
  friend class CodeCompletionResultBuilder;

  /// \brief A set of current completion results, not yet delivered to the
  /// consumer.
  CodeCompletionResultSink CurrentResults;

public:
  CodeCompletionCache &Cache;

  CodeCompletionContext(CodeCompletionCache &Cache)
      : Cache(Cache) {}

  /// \brief Allocate a string owned by the code completion context.
  StringRef copyString(StringRef Str);

  /// \brief Return current code completion results.
  MutableArrayRef<CodeCompletionResult *> takeResults();

  /// \brief Sort code completion results in an implementetion-defined order
  /// in place.
  static void sortCompletionResults(
      MutableArrayRef<CodeCompletionResult *> Results);

  CodeCompletionResultSink &getResultSink() {
    return CurrentResults;
  }
};

/// \brief An abstract base class for consumers of code completion results.
class CodeCompletionConsumer {
public:
  virtual ~CodeCompletionConsumer() {}

  virtual void handleResults(
      MutableArrayRef<CodeCompletionResult *> Results) = 0;
};

/// \brief A code completion result consumer that prints the results to a
/// \c raw_ostream.
class PrintingCodeCompletionConsumer : public CodeCompletionConsumer {
  llvm::raw_ostream &OS;
  bool IncludeKeywords;

public:
  PrintingCodeCompletionConsumer(llvm::raw_ostream &OS, bool IncludeKeywords = true)
      : OS(OS), IncludeKeywords(IncludeKeywords) {
  }

  void handleResults(
      MutableArrayRef<CodeCompletionResult *> Results) override;
};

/// \brief Create a factory for code completion callbacks.
CodeCompletionCallbacksFactory *
makeCodeCompletionCallbacksFactory(CodeCompletionContext &CompletionContext,
                                   CodeCompletionConsumer &Consumer);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CODE_COMPLETION_H

