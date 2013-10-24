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
#include "swift/AST/Decl.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"
#include <functional>
#include <string>
#include <vector>

namespace swift {
class CodeCompletionCallbacksFactory;
class ClangModule;

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
      Comma,
      ExclamationMark,
      QuestionMark,

      /// The first chunk of a substring that describes the parameter for a
      /// generic type.
      GenericParameterBegin,
      /// Generic type parameter name.
      GenericParameterName,

      /// The first chunk of a substring that describes the parameter for a
      /// function call.
      CallParameterBegin,
      /// Function call parameter name.  Can be omitted in the editor buffer.
      CallParameterName,
      /// A colon between parameter name and value.  Should be inserted in the
      /// editor buffer if the preceding CallParameterName was inserted.
      CallParameterColon,
      /// Required parameter type.
      CallParameterType,

      /// A placeholder for \c ! or \c ? in a call to a method found by dynamic
      /// lookup.
      ///
      /// Note that the IDE should not insert any of these characters by
      /// default.
      DynamicLookupMethodCallTail,

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
             Kind == ChunkKind::LeftBracket ||
             Kind == ChunkKind::RightBracket ||
             Kind == ChunkKind::LeftAngle ||
             Kind == ChunkKind::RightAngle ||
             Kind == ChunkKind::Dot ||
             Kind == ChunkKind::Comma ||
             Kind == ChunkKind::ExclamationMark ||
             Kind == ChunkKind::QuestionMark ||
             Kind == ChunkKind::CallParameterName ||
             Kind == ChunkKind::CallParameterColon ||
             Kind == ChunkKind::CallParameterType ||
             Kind == ChunkKind::GenericParameterName ||
             Kind == ChunkKind::DynamicLookupMethodCallTail ||
             Kind == ChunkKind::TypeAnnotation;
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
  GlobalVar
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
  CodeCompletionString *const CompletionString;

  CodeCompletionResult(ResultKind Kind,
                       SemanticContextKind SemanticContext,
                       CodeCompletionString *CompletionString)
      : Kind(Kind), SemanticContext(unsigned(SemanticContext)),
        CompletionString(CompletionString) {
  }

  CodeCompletionResult(SemanticContextKind SemanticContext,
                       CodeCompletionString *CompletionString,
                       const Decl *AssociatedDecl)
      : CodeCompletionResult(ResultKind::Declaration, SemanticContext,
                             CompletionString) {
    assert(AssociatedDecl && "should have a decl");
    AssociatedDeclKind = unsigned(getCodeCompletionDeclKind(AssociatedDecl));
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

  const CodeCompletionString *getCompletionString() const {
    return CompletionString;
  }

  /// Print a debug representation of the code completion result to \p OS.
  void print(raw_ostream &OS) const;
  void dump() const;

private:
  static CodeCompletionDeclKind getCodeCompletionDeclKind(const Decl *D);
};

struct CodeCompletionResultSink {
  llvm::BumpPtrAllocator &Allocator;
  std::vector<CodeCompletionResult *> &Results;

  CodeCompletionResultSink(llvm::BumpPtrAllocator &Allocator,
                           std::vector<CodeCompletionResult *> &Results)
      : Allocator(Allocator), Results(Results) {}
};

class CodeCompletionCache {
  struct Implementation;

  /// \brief Per-module code completion result cache.
  ///
  /// These results persist between multiple code completion requests.
  std::unique_ptr<Implementation> Impl;

public:
  CodeCompletionCache();
  ~CodeCompletionCache();

  /// \brief Cache key.
  struct Key {
    std::string Filename;
    std::string ModuleName;
    std::vector<std::string> AccessPath;
    bool ResultsHaveLeadingDot;

    friend bool operator==(const Key &LHS, const Key &RHS) {
      return LHS.Filename == RHS.Filename &&
             LHS.ModuleName == RHS.ModuleName &&
             LHS.AccessPath == RHS.AccessPath &&
             LHS.ResultsHaveLeadingDot == RHS.ResultsHaveLeadingDot;
    }
  };

  void getResults(
      const Key &K, CodeCompletionResultSink Sink, bool OnlyTypes,
      std::function<void(CodeCompletionCache &, Key)> FillCacheCallback);

  CodeCompletionResultSink getResultSinkFor(const Key &K);
};

class CodeCompletionContext {
  friend class CodeCompletionResultBuilder;
  llvm::BumpPtrAllocator Allocator;

  /// \brief A set of current completion results, not yet delivered to the
  /// consumer.
  std::vector<CodeCompletionResult *> CurrentCompletionResults;

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

  CodeCompletionResultSink getResultSink() {
    return CodeCompletionResultSink(Allocator, CurrentCompletionResults);
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

public:
  PrintingCodeCompletionConsumer(llvm::raw_ostream &OS)
      : OS(OS) {
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

namespace llvm {
template<>
struct DenseMapInfo<swift::ide::CodeCompletionCache::Key> {
  static inline swift::ide::CodeCompletionCache::Key getEmptyKey() {
    return swift::ide::CodeCompletionCache::Key{"", "", {}, false};
  }
  static inline swift::ide::CodeCompletionCache::Key getTombstoneKey() {
    auto Result = getEmptyKey();
    Result.Filename = "x";
    return Result;
  }
  static unsigned
  getHashValue(const swift::ide::CodeCompletionCache::Key &Val) {
    size_t H = 0;
    H ^= std::hash<std::string>()(Val.Filename);
    H ^= std::hash<std::string>()(Val.ModuleName);
    for (auto Piece : Val.AccessPath)
      H ^= std::hash<std::string>()(Piece);
    H ^= std::hash<bool>()(Val.ResultsHaveLeadingDot);
    return H;
  }
  static bool isEqual(const swift::ide::CodeCompletionCache::Key &LHS,
                      const swift::ide::CodeCompletionCache::Key &RHS) {
    return LHS == RHS;
  }
};
} // namespace llvm

namespace std {
template <> struct hash<swift::ide::CodeCompletionCache::Key> {
  std::size_t
  operator()(const swift::ide::CodeCompletionCache::Key &K) const {
    return llvm::DenseMapInfo<
        swift::ide::CodeCompletionCache::Key>::getHashValue(K);
  }
};
} // namespace std
#endif // SWIFT_IDE_CODE_COMPLETION_H

