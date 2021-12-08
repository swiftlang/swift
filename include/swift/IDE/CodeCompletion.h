//===--- CodeCompletion.h - Routines for code completion --------*- C++ -*-===//
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

#ifndef SWIFT_IDE_CODECOMPLETION_H
#define SWIFT_IDE_CODECOMPLETION_H

#include "swift/AST/ASTDemangler.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Frontend/Frontend.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/TrailingObjects.h"
#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace swift {
class CodeCompletionCallbacksFactory;
class Decl;
class DeclContext;
class FrontendOptions;
class ModuleDecl;
class SourceFile;

namespace ide {

class CodeCompletionCache;
class CodeCompletionContext;
class CodeCompletionResultBuilder;
struct CodeCompletionResultSink;
struct RequestedCachedModule;

/// A routine to remove code completion tokens from code completion
/// tests.
///
/// \code
/// code-completion-token:
///     '#^' identifier '^#'
/// \endcode
///
/// \param Input test source code.
/// \param TokenName names the token whose position should be returned in
/// \p CompletionOffset.
/// \param CompletionOffset set to ~0U on error, or to a 0-based byte offset on
/// success.
///
/// \returns test source code without any code completion tokens.
std::string removeCodeCompletionTokens(StringRef Input,
                                       StringRef TokenName,
                                       unsigned *CompletionOffset);

StringRef copyString(llvm::BumpPtrAllocator &Allocator,
                     StringRef Str);

const char *copyCString(llvm::BumpPtrAllocator &Allocator,
                        StringRef Str);

template <typename T>
ArrayRef<T> copyArray(llvm::BumpPtrAllocator &Allocator,
                            ArrayRef<T> Arr) {
  T *Buffer = Allocator.Allocate<T>(Arr.size());
  std::copy(Arr.begin(), Arr.end(), Buffer);
  return llvm::makeArrayRef(Buffer, Arr.size());
}

namespace detail {
class CodeCompletionStringChunk {
  friend class swift::ide::CodeCompletionResultBuilder;

public:
  enum class ChunkKind {
    /// "open", "public", "internal", "fileprivate", or "private".
    AccessControlKeyword,

    /// such as @"available".
    DeclAttrKeyword,

    /// such as "unavailable" etc. for @available.
    DeclAttrParamKeyword,

    /// The "override" keyword.
    OverrideKeyword,

    /// The "throws", "rethrows" and "async" keyword.
    EffectsSpecifierKeyword,

    /// The keyword part of a declaration before the name, like "func".
    DeclIntroducer,

    /// Other generic keyword.
    Keyword,

    /// Other generic attributes.
    Attribute,

    /// Normal text chunk.
    Text,

    /// Base name of the result.
    BaseName,

    /// The first chunk of an optional substring that continues until
    /// \c NestingLevel decreases.
    OptionalBegin,

    /// Punctuation.
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
    Equal,
    Whitespace,

    /// The first chunk of a whole generic parameter clause.
    /// E.g '<T, C: Collection>'
    GenericParameterClauseBegin,

    /// The first chunk of a generic quirement clause.
    /// E.g. 'where T: Collection, T.Element == Int'
    GenericRequirementClauseBegin,

    /// The first chunk of a substring that describes the parameter for a
    /// generic type.
    GenericParameterBegin,
    /// Generic type parameter name.
    GenericParameterName,

    /// The first chunk of a substring that describes the argument for a
    /// function call.
    CallArgumentBegin,

    /// Function call argument label.
    CallArgumentName,

    /// Function parameter internal / local name for an call argument. If the
    /// parameter has no formal API name, it can still have a local name which
    /// can be useful for display purposes.
    ///
    /// This chunk should not be inserted into the editor buffer.
    CallArgumentInternalName,

    /// A colon between argument name and value.  Should be inserted in the
    /// editor buffer if the preceding CallArgumentName was inserted.
    CallArgumentColon,

    /// A colon between parameter name and value. Used in decl attribute.
    DeclAttrParamColon,

    /// Required argument type.
    CallArgumentType,

    /// Argument type tag for annotated results.
    CallArgumentTypeBegin,

    /// System type name.
    TypeIdSystem,

    /// Non-system type name.
    TypeIdUser,

    /// Desugared closure argument type. This can be used to get the
    /// closure type if CallArgumentType is a TypeAliasType.
    CallArgumentClosureType,

    /// An expanded closure expression for the value of an argument, including
    /// the left and right braces and possible signature. The preferred
    /// position to put the cursor after the completion result is inserted
    /// into the editor buffer is between the braces.
    CallArgumentClosureExpr,

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

    /// The first chunk of a substring that describes the single parameter
    /// declaration for a parameter clause.
    ParameterDeclBegin,

    ParameterDeclExternalName,

    ParameterDeclLocalName,

    ParameterDeclColon,

    ParameterDeclTypeBegin,

    /// Default argument clause for parameter declarations.
    DefaultArgumentClauseBegin,

    /// First chunk for effect specifiers. i.e. 'async' and 'throws'.
    EffectsSpecifierClauseBegin,

    /// First chunk for result type clause i.e. ' -> ResultTy' or ': ResultTy'.
    DeclResultTypeClauseBegin,

    /// First chunk for attribute and modifier list i.e. 'override public'
    AttributeAndModifierListBegin,

    /// Specifies the type of the whole entity that is returned in this code
    /// completion result.  For example, for variable references it is the
    /// variable type, for function calls it is the return type.
    ///
    /// This chunk should not be inserted into the editor buffer.
    TypeAnnotation,

    /// Structured group version of 'TypeAnnotation'.
    /// This grouped chunks should not be inserted into the editor buffer.
    TypeAnnotationBegin,

    /// A brace statement -- left brace and right brace.  The preferred
    /// position to put the cursor after the completion result is inserted
    /// into the editor buffer is between the braces.
    ///
    /// The spelling is always "{}", but clients may choose to insert newline
    /// and indentation in between.
    BraceStmtWithCursor,
  };

  static bool chunkStartsNestedGroup(ChunkKind Kind) {
    return Kind == ChunkKind::CallArgumentBegin ||
           Kind == ChunkKind::GenericParameterBegin ||
           Kind == ChunkKind::OptionalBegin ||
           Kind == ChunkKind::CallArgumentTypeBegin ||
           Kind == ChunkKind::TypeAnnotationBegin ||
           Kind == ChunkKind::ParameterDeclBegin ||
           Kind == ChunkKind::ParameterDeclTypeBegin ||
           Kind == ChunkKind::DefaultArgumentClauseBegin ||
           Kind == ChunkKind::GenericParameterClauseBegin ||
           Kind == ChunkKind::EffectsSpecifierClauseBegin ||
           Kind == ChunkKind::DeclResultTypeClauseBegin ||
           Kind == ChunkKind::GenericRequirementClauseBegin ||
           Kind == ChunkKind::AttributeAndModifierListBegin;
  }

  static bool chunkHasText(ChunkKind Kind) {
    return Kind == ChunkKind::AccessControlKeyword ||
           Kind == ChunkKind::OverrideKeyword ||
           Kind == ChunkKind::EffectsSpecifierKeyword ||
           Kind == ChunkKind::DeclAttrKeyword ||
           Kind == ChunkKind::DeclIntroducer ||
           Kind == ChunkKind::Keyword ||
           Kind == ChunkKind::Attribute ||
           Kind == ChunkKind::BaseName ||
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
           Kind == ChunkKind::Equal ||
           Kind == ChunkKind::Whitespace ||
           Kind == ChunkKind::CallArgumentName ||
           Kind == ChunkKind::CallArgumentInternalName ||
           Kind == ChunkKind::CallArgumentColon ||
           Kind == ChunkKind::CallArgumentType ||
           Kind == ChunkKind::CallArgumentClosureType ||
           Kind == ChunkKind::CallArgumentClosureExpr ||
           Kind == ChunkKind::ParameterDeclExternalName ||
           Kind == ChunkKind::ParameterDeclLocalName ||
           Kind == ChunkKind::ParameterDeclColon ||
           Kind == ChunkKind::DeclAttrParamColon ||
           Kind == ChunkKind::DeclAttrParamKeyword ||
           Kind == ChunkKind::GenericParameterName ||
           Kind == ChunkKind::DynamicLookupMethodCallTail ||
           Kind == ChunkKind::OptionalMethodCallTail ||
           Kind == ChunkKind::TypeAnnotation ||
           Kind == ChunkKind::BraceStmtWithCursor ||
           Kind == ChunkKind::TypeIdSystem ||
           Kind == ChunkKind::TypeIdUser;
  }

private:
  unsigned Kind : 8;
  unsigned NestingLevel : 8;

  /// If true, then this chunk is an annotation that is included only
  /// for exposition and may not be inserted in the editor buffer.
  unsigned IsAnnotation : 1;

  StringRef Text;

  CodeCompletionStringChunk(ChunkKind Kind, unsigned NestingLevel, StringRef Text,
                            bool isAnnotation)
      : Kind(unsigned(Kind)), NestingLevel(NestingLevel),
        IsAnnotation(isAnnotation), Text(Text) {
    assert(chunkHasText(Kind));
  }

  CodeCompletionStringChunk(ChunkKind Kind, unsigned NestingLevel,
                            bool isAnnotation)
      : Kind(unsigned(Kind)), NestingLevel(NestingLevel),
        IsAnnotation(isAnnotation) {
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

  static CodeCompletionStringChunk createWithText(ChunkKind Kind,
                                                  unsigned NestingLevel,
                                                  StringRef Text,
                                                  bool isAnnotation = false) {
    return CodeCompletionStringChunk(Kind, NestingLevel, Text, isAnnotation);
  }

  static CodeCompletionStringChunk createSimple(ChunkKind Kind,
                                                unsigned NestingLevel,
                                                bool isAnnotation = false) {
    return CodeCompletionStringChunk(Kind, NestingLevel, isAnnotation);
  }
};

} // end namespace detail

/// A structured representation of a code completion string.
class alignas(detail::CodeCompletionStringChunk) CodeCompletionString final :
    private llvm::TrailingObjects<CodeCompletionString,
                                  detail::CodeCompletionStringChunk> {
  friend class CodeCompletionResultBuilder;
  friend TrailingObjects;

public:
  using Chunk = detail::CodeCompletionStringChunk;

private:
  unsigned NumChunks : 16;

  CodeCompletionString(ArrayRef<Chunk> Chunks);

public:
  /// Creates a \c CodeCompletionString from a list of \c Chunks.
  ///
  /// \note The caller must ensure any text inside \c Chunks will outlive this
  /// object, typically by storing them inside a \c CodeCompletionResultSink.
  static CodeCompletionString *create(llvm::BumpPtrAllocator &Allocator,
                                      ArrayRef<Chunk> Chunks);

  ArrayRef<Chunk> getChunks() const {
    return {getTrailingObjects<Chunk>(), NumChunks};
  }

  StringRef getFirstTextChunk(bool includeLeadingPunctuation = false) const;
  Optional<unsigned>
  getFirstTextChunkIndex(bool includeLeadingPunctuation = false) const;

  /// Print a debug representation of the code completion string to \p OS.
  void print(raw_ostream &OS) const;
  SWIFT_DEBUG_DUMP;
};

/// Describes the origin of the code completion result.
///
/// This enum is ordered from the contexts that are "nearest" to the code
/// completion point to "outside" contexts.
enum class SemanticContextKind : uint8_t {
  /// Used in cases when the concept of semantic context is not applicable.
  None,

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

  MAX_VALUE = OtherModule
};

enum class CodeCompletionFlairBit: uint8_t {
  /// **Deprecated**. Old style catch-all prioritization.
  ExpressionSpecific = 1 << 0,

  /// E.g. override func foo() { super.foo() ...
  SuperChain = 1 << 1,

  /// Argument label and type. i.e. 'label: <#Ty#>'.
  ArgumentLabels = 1 << 2,

  /// E.g. decl introducer or modifiers ('enum', 'protocol', 'public', etc.) at
  /// top-level.
  CommonKeywordAtCurrentPosition = 1 << 3,

  /// E.g. type decl introducer ('enum', 'class', etc.) in a function body.
  RareKeywordAtCurrentPosition = 1 << 4,

  /// E.g. protocol names at an expression position.
  RareTypeAtCurrentPosition = 1 << 5,

  /// E.g. referencing a type, function, etc… at top level position in a non
  /// script/main.swift file
  ExpressionAtNonScriptOrMainFileScope = 1 << 6,
};

using CodeCompletionFlair = OptionSet<CodeCompletionFlairBit>;

/// The declaration kind of a code completion result, if it is a declaration.
enum class CodeCompletionDeclKind : uint8_t {
  Module,
  Class,
  Struct,
  Enum,
  EnumElement,
  Protocol,
  AssociatedType,
  TypeAlias,
  GenericTypeParam,
  Constructor,
  Destructor,
  Subscript,
  StaticMethod,
  InstanceMethod,
  PrefixOperatorFunction,
  PostfixOperatorFunction,
  InfixOperatorFunction,
  FreeFunction,
  StaticVar,
  InstanceVar,
  LocalVar,
  GlobalVar,
  PrecedenceGroup,
};

enum class CodeCompletionLiteralKind : uint8_t {
  ArrayLiteral,
  BooleanLiteral,
  ColorLiteral,
  DictionaryLiteral,
  IntegerLiteral,
  ImageLiteral,
  NilLiteral,
  StringLiteral,
  Tuple,
};

enum class CodeCompletionOperatorKind : uint8_t {
  None,
  Unknown,
  Bang,       // !
  NotEq,      // !=
  NotEqEq,    // !==
  Modulo,     // %
  ModuloEq,   // %=
  Amp,        // &
  AmpAmp,     // &&
  AmpStar,    // &*
  AmpPlus,    // &+
  AmpMinus,   // &-
  AmpEq,      // &=
  LParen,     // ( -- not really an operator, but treated as one in some cases.
  Star,       // *
  StarEq,     // *=
  Plus,       // +
  PlusEq,     // +=
  Minus,      // -
  MinusEq,    // -=
  Dot,        // .
  DotDotDot,  // ...
  DotDotLess, // ..<
  Slash,      // /
  SlashEq,    // /=
  Less,       // <
  LessLess,   // <<
  LessLessEq, // <<=
  LessEq,     // <=
  Eq,         // =
  EqEq,       // ==
  EqEqEq,     // ===
  Greater,    // >
  GreaterEq,  // >=
  GreaterGreater,   // >>
  GreaterGreaterEq, // >>=
  QuestionDot,      // ?.
  Caret,            // ^
  CaretEq,          // ^=
  Pipe,             // |
  PipeEq,           // |=
  PipePipe,         // ||
  TildeEq,          // ~=

  MAX_VALUE = TildeEq
};

enum class CodeCompletionKeywordKind : uint8_t {
  None,
#define KEYWORD(X) kw_##X,
#define POUND_KEYWORD(X) pound_##X,
#include "swift/Syntax/TokenKinds.def"
};

enum class CompletionKind : uint8_t {
  None,
  Import,
  UnresolvedMember,
  DotExpr,
  StmtOrExpr,
  PostfixExprBeginning,
  PostfixExpr,
  PostfixExprParen,
  KeyPathExprObjC,
  KeyPathExprSwift,
  TypeDeclResultBeginning,
  TypeSimpleBeginning,
  TypeIdentifierWithDot,
  TypeIdentifierWithoutDot,
  CaseStmtKeyword,
  CaseStmtBeginning,
  NominalMemberBeginning,
  AccessorBeginning,
  AttributeBegin,
  AttributeDeclParen,
  EffectsSpecifier,
  PoundAvailablePlatform,
  CallArg,
  LabeledTrailingClosure,
  ReturnStmtExpr,
  YieldStmtExpr,
  ForEachSequence,
  AfterPoundExpr,
  AfterPoundDirective,
  PlatformConditon,
  AfterIfStmtElse,
  GenericRequirement,
  PrecedenceGroup,
  StmtLabel,
  ForEachPatternBeginning,
  TypeAttrBeginning,
};

enum class CodeCompletionDiagnosticSeverity : uint8_t {
  None,
  Error,
  Warning,
  Remark,
  Note,

  MAX_VALUE = Note
};

/// Reasons why a code completion item might not be recommended in a certain
/// context.
/// This enum is split into two subsets: \c ContextFreeNotRecommendedReason and
/// \c ContextualNotRecommendedReason. When adding a case to this enum also add
/// it to either of those.
/// Context-free diagnostics are independent of the context they are used in.
/// The not-recommended reason can thus be cached as part of a context free
/// code completion result.
/// Contextual not recommended reasons depend on the context they are used in.
/// E.g. \c InvalidAsyncContext depends on whether the usage context is async or
/// not.
enum class NotRecommendedReason : uint8_t {
  None = 0,                    // both contextual and context-free
  RedundantImport,             // contextual
  RedundantImportIndirect,     // contextual
  Deprecated,                  // context-free
  SoftDeprecated,              // context-free
  InvalidAsyncContext,         // contextual
  CrossActorReference,         // contextual
  VariableUsedInOwnDefinition, // contextual

  MAX_VALUE = VariableUsedInOwnDefinition
};

/// TODO: We consider deprecation warnings as context free although they don't
/// produce deprecation warnings insdide context that have the same or a
/// stronger deprecation attribute.
/// E.g.
/// In SDK:
/// \code
/// @available(iOS, deprecated: 12.0)
/// func deprecatedFunc()
/// \endcode
///
/// User code:
/// \code
/// @available(iOS, deprecated: 12.0)
/// func insiderUserDeprecatedContext() {
///   #^COMPLETE^#
/// }
/// \endcode
/// suggests \c deprecatedFunc as deprecated although it doesn't produce a
/// diagnostic during compilation. But this allows us to show deprecation
/// warnings for cached results.
enum class ContextFreeNotRecommendedReason : uint8_t {
  None = 0,
  Deprecated,
  SoftDeprecated,
  MAX_VALUE = SoftDeprecated
};

enum class ContextualNotRecommendedReason : uint8_t {
  None = 0,
  RedundantImport,
  RedundantImportIndirect,
  InvalidAsyncContext,
  CrossActorReference,
  VariableUsedInOwnDefinition,
  MAX_VALUE = VariableUsedInOwnDefinition
};

enum class CodeCompletionResultKind : uint8_t {
  Declaration,
  Keyword,
  Pattern,
  Literal,
  BuiltinOperator,

  MAX_VALUE = BuiltinOperator
};

class CodeCompletionResultType;

/// An arena in which \c CodeCompletionResultType are allocated. Two
/// \c CodeCompletionResultType with the same USR are represented by the same
/// object within one arena.
class CodeCompletionResultTypeArena
    : public llvm::ThreadSafeRefCountedBase<CodeCompletionResultTypeArena> {
  friend class CodeCompletionResultType;

  llvm::BumpPtrAllocator Allocator;

  /// Maps USRs to \c CodeCompletionResultTypes that have already been
  /// constructed.
  llvm::StringMap<CodeCompletionResultType *> USRCache;

  /// Maps Types to \c CodeCompletionResultTypes that have already been
  /// constructed.
  llvm::DenseMap<Type, CodeCompletionResultType *> TypeCache;
};

using CodeCompletionResultTypeArenaRef =
    llvm::IntrusiveRefCntPtr<CodeCompletionResultTypeArena>;

/// Compute all the supertypes that \p ResultType as seen from \p ResultType's
/// module. This includes all superclasses and protocols that \p ResultType
/// conforms to within its own module. Retroactive conformances in downstream
/// modules are not considered because they might not be present when using the
/// type in a different module.
SmallVector<CodeCompletionResultType *, 2>
computeSupertypes(Type ResultType,
                  const CodeCompletionResultTypeArenaRef &ResultTypeArena);

/// The type returned by a code completion item. It is canonicalized and can be
/// instantiated in different \c ASTContexts.
class CodeCompletionResultType {

  /// The USR of the type. Used to re-instantiate the type in different \c
  /// ASTContexts.
  std::string USR;

  /// If a type has already been computed from the USR, it is cached here.
  Type Ty;

  /// The \c ASTContext in which \c Ty lives. Must be stored separately from
  /// \c Ty because the \c ASTContext in which \c Ty lives might get destroyed,
  /// thus making the pointer inside \c Ty invalid.
  ASTContext *ContextOfTy;

  /// See \c computeSupertypes.
  std::vector<CodeCompletionResultType *> Supertypes;

  CodeCompletionResultType(StringRef USR,
                           ArrayRef<CodeCompletionResultType *> Supertypes)
      : USR(USR), Supertypes(Supertypes) {}

public:
  /// Create an \c CodeCompletionResultType from an \c ASTContext's \c Type.
  static CodeCompletionResultType *
  fromType(Type Ty, const CodeCompletionResultTypeArenaRef &Arena) {
    /// Canonicalize the type. We don't consider metatypes and instance types
    /// different for code completion purposes, normalize to instance types.
    if (Ty) {
      Ty = Ty->getCanonicalType();
    }
    auto ResFromType = Arena->TypeCache.find(Ty);
    if (ResFromType != Arena->TypeCache.end()) {
      return ResFromType->second;
    }

    auto Supertypes = computeSupertypes(Ty, Arena);

    CodeCompletionResultType *Res;
    // FIXME: We can't compute USRs for archetypes. If the type contains an
    // archetype, create a new CodeCompletionResultType so that multiple
    // archetypes aren't unified to the same ASTContextIndependent type.
    // Once we round-trip via the USR, this will produce a null type.
    if (Ty && !Ty->hasArchetype()) {
      SmallString<32> USR;
      llvm::raw_svector_ostream OS(USR);
      printTypeUSR(Ty, OS);
      Res = CodeCompletionResultType::fromUSR(USR, Supertypes, Arena);
    } else {
      Res = new (Arena->Allocator) CodeCompletionResultType("", Supertypes);
    }
    // We already know the Type. Cache it so it doesn't have to be recomputed.
    Res->Ty = Ty;
    Res->ContextOfTy = Ty ? &Ty->getASTContext() : nullptr;

    Arena->TypeCache[Ty] = Res;

    return Res;
  }

  /// Create a type from a USR. The USR will only get demangled once a \c Type
  /// inside an \c ASTContext is requested using \c getType.
  /// \p Supertypes are supertypes of the type with the given \p USR as computed
  /// by \c computeSupertypes.
  static CodeCompletionResultType *
  fromUSR(StringRef USR, ArrayRef<CodeCompletionResultType *> Supertypes,
          const CodeCompletionResultTypeArenaRef &Arena) {
    auto ResFromUSR = Arena->USRCache.find(USR);
    if (ResFromUSR != Arena->USRCache.end()) {
      return ResFromUSR->second;
    }
    auto Res = new (Arena->Allocator) CodeCompletionResultType(USR, Supertypes);

    Arena->USRCache[USR] = Res;
    return Res;
  }

  /// Return the \c Type within the given \p Ctx.
  Type getType(ASTContext &Ctx) {
    if (!Ty || ContextOfTy != &Ctx) {
      if (USR.empty()) {
        Ty = Type();
      } else {
        Ty = Demangle::getTypeForMangling(Ctx, USR);
        assert(!Ty ||
               Ty->isCanonical() &&
                   "Types in CodeCompletionResultType must be canonical.");
      }
    }
    return Ty;
  }

  ArrayRef<CodeCompletionResultType *> getSupertypes() const {
    return Supertypes;
  }

  /// Return the USR of this type, which identifies the type independent of the
  /// \c ASTContext.
  StringRef getUSR() const { return USR; }
};

/// The parts of a \c CodeCompletionResult that are not dependent on the context
/// it appears in and can thus be cached.
class ContextFreeCodeCompletionResult {
  CodeCompletionResultKind Kind : 3;
  uint8_t AssociatedKind;
  CodeCompletionOperatorKind KnownOperatorKind : 6;
  bool IsSystem : 1;
  CodeCompletionString *CompletionString;
  StringRef ModuleName;
  StringRef BriefDocComment;
  ArrayRef<StringRef> AssociatedUSRs;
  /// The types that can be produced by the expression. This is not a single
  /// unique type because for code completion we consider e.g. \c Int as
  /// producing both an \c Int metatype and an \c Int instance type.
  /// Can
  ///  - be empty if the completion result doesn't produce something that's
  ///    valid inside an expression, e.g. a keyword.
  ///  - contain a null type if the completion result produces something that's
  ///    valid inside an expression but the result type isn't known.
  ///  - contain proper types if the type produced by this completion result is
  ///    known.
  llvm::SmallVector<CodeCompletionResultType *, 2> ResultTypes;

  ContextFreeNotRecommendedReason NotRecommended;
  CodeCompletionDiagnosticSeverity DiagnosticSeverity : 3;
  StringRef DiagnosticMessage;

  static_assert(int(CodeCompletionResultKind::MAX_VALUE) < 1 << 3, "");
  static_assert(int(CodeCompletionOperatorKind::MAX_VALUE) < 1 << 6, "");
  static_assert(int(CodeCompletionDiagnosticSeverity::MAX_VALUE) < 1 << 3, "");

public:
  /// Memberwise initializer. \p AssociatedKInd is opaque and will be
  /// interpreted based on \p Kind. If \p KnownOperatorKind is \c None and the
  /// completion item is an operator, it will be determined based on the
  /// compleiton string.
  ///
  /// \note The caller must ensure that the \p CompleitonString and all the
  /// \c Ref types outlive this result, typically by storing them in the same
  /// \c CodeCompletionResultSink as the result itself.
  ContextFreeCodeCompletionResult(
      CodeCompletionResultKind Kind, uint8_t AssociatedKind,
      CodeCompletionOperatorKind KnownOperatorKind, bool IsSystem,
      CodeCompletionString *CompletionString, StringRef ModuleName,
      StringRef BriefDocComment, ArrayRef<StringRef> AssociatedUSRs,
      ArrayRef<CodeCompletionResultType *> ResultTypes,
      ContextFreeNotRecommendedReason NotRecommended,
      CodeCompletionDiagnosticSeverity DiagnosticSeverity,
      StringRef DiagnosticMessage)
      : Kind(Kind), AssociatedKind(AssociatedKind),
        KnownOperatorKind(KnownOperatorKind), IsSystem(IsSystem),
        CompletionString(CompletionString), ModuleName(ModuleName),
        BriefDocComment(BriefDocComment), AssociatedUSRs(AssociatedUSRs),
        ResultTypes(ResultTypes.begin(), ResultTypes.end()),
        NotRecommended(NotRecommended), DiagnosticSeverity(DiagnosticSeverity),
        DiagnosticMessage(DiagnosticMessage) {
    assert((NotRecommended == ContextFreeNotRecommendedReason::None) ==
               (DiagnosticSeverity == CodeCompletionDiagnosticSeverity::None) &&
           "Result should be not recommended iff it has a diagnostic");
    assert((DiagnosticSeverity == CodeCompletionDiagnosticSeverity::None) ==
               DiagnosticMessage.empty() &&
           "Completion item should have diagnostic message iff the diagnostics "
           "severity is not none");
    assert(CompletionString && "Result should have a completion string");
    if (isOperator() && KnownOperatorKind == CodeCompletionOperatorKind::None) {
      this->KnownOperatorKind = getCodeCompletionOperatorKind(CompletionString);
    }
    assert(!isOperator() ||
           getKnownOperatorKind() != CodeCompletionOperatorKind::None &&
               "isOperator implies operator kind != None");
  }

  /// Constructs a \c Pattern, \c Keyword or \c BuiltinOperator result.
  ///
  /// \note The caller must ensure that the \p CompletionString and all the
  /// \c StringRefs outlive this result, typically by storing them in the same
  /// \c CodeCompletionResultSink as the result itself.
  ContextFreeCodeCompletionResult(
      CodeCompletionResultKind Kind, CodeCompletionString *CompletionString,
      CodeCompletionOperatorKind KnownOperatorKind, StringRef BriefDocComment,
      ArrayRef<CodeCompletionResultType *> ResultTypes,
      ContextFreeNotRecommendedReason NotRecommended,
      CodeCompletionDiagnosticSeverity DiagnosticSeverity,
      StringRef DiagnosticMessage)
      : ContextFreeCodeCompletionResult(
            Kind, /*AssociatedKind=*/0, KnownOperatorKind,
            /*IsSystem=*/false, CompletionString, /*ModuleName=*/"",
            BriefDocComment, /*AssociatedUSRs=*/{}, ResultTypes, NotRecommended,
            DiagnosticSeverity, DiagnosticMessage) {}

  /// Constructs a \c Keyword result.
  ///
  /// \note The caller must ensure that the \p CompletionString and
  /// \p BriefDocComment outlive this result, typically by storing them in the
  /// same \c CodeCompletionResultSink as the result itself.
  ContextFreeCodeCompletionResult(
      CodeCompletionKeywordKind Kind, CodeCompletionString *CompletionString,
      StringRef BriefDocComment,
      ArrayRef<CodeCompletionResultType *> ResultType)
      : ContextFreeCodeCompletionResult(
            CodeCompletionResultKind::Keyword, static_cast<uint8_t>(Kind),
            CodeCompletionOperatorKind::None, /*IsSystem=*/false,
            CompletionString, /*ModuleName=*/"", BriefDocComment,
            /*AssociatedUSRs=*/{}, ResultType,
            ContextFreeNotRecommendedReason::None,
            CodeCompletionDiagnosticSeverity::None, /*DiagnosticMessage=*/"") {}

  /// Constructs a \c Literal result.
  ///
  /// \note The caller must ensure that the \p CompletionString outlives this
  /// result, typically by storing them in the same \c CodeCompletionResultSink
  /// as the result itself.
  ContextFreeCodeCompletionResult(
      CodeCompletionLiteralKind LiteralKind,
      CodeCompletionString *CompletionString,
      ArrayRef<CodeCompletionResultType *> ResultTypes)
      : ContextFreeCodeCompletionResult(
            CodeCompletionResultKind::Literal,
            static_cast<uint8_t>(LiteralKind), CodeCompletionOperatorKind::None,
            /*IsSystem=*/false, CompletionString, /*ModuleName=*/"",
            /*BriefDocComment=*/"",
            /*AssociatedUSRs=*/{}, ResultTypes,
            ContextFreeNotRecommendedReason::None,
            CodeCompletionDiagnosticSeverity::None, /*DiagnosticMessage=*/"") {}

  /// Constructs a \c Declaration result.
  ///
  /// \note The caller must ensure that the \p CompletionString and all the
  /// \c StringRefs outlive this result, typically by storing them in the same
  /// \c CodeCompletionResultSink as the result itself.
  ContextFreeCodeCompletionResult(
      CodeCompletionString *CompletionString, const Decl *AssociatedDecl,
      StringRef ModuleName, StringRef BriefDocComment,
      ArrayRef<StringRef> AssociatedUSRs,
      ArrayRef<CodeCompletionResultType *> ResultTypes,
      ContextFreeNotRecommendedReason NotRecommended,
      CodeCompletionDiagnosticSeverity DiagnosticSeverity,
      StringRef DiagnosticMessage)
      : ContextFreeCodeCompletionResult(
            CodeCompletionResultKind::Declaration,
            static_cast<uint8_t>(getCodeCompletionDeclKind(AssociatedDecl)),
            CodeCompletionOperatorKind::None, getDeclIsSystem(AssociatedDecl),
            CompletionString, ModuleName, BriefDocComment, AssociatedUSRs,
            ResultTypes, NotRecommended, DiagnosticSeverity,
            DiagnosticMessage) {
    assert(AssociatedDecl && "should have a decl");
  }

  CodeCompletionResultKind getKind() const { return Kind; }

  CodeCompletionDeclKind getAssociatedDeclKind() const {
    assert(getKind() == CodeCompletionResultKind::Declaration);
    return static_cast<CodeCompletionDeclKind>(AssociatedKind);
  }
  CodeCompletionLiteralKind getLiteralKind() const {
    assert(getKind() == CodeCompletionResultKind::Literal);
    return static_cast<CodeCompletionLiteralKind>(AssociatedKind);
  }
  CodeCompletionKeywordKind getKeywordKind() const {
    assert(getKind() == CodeCompletionResultKind::Keyword);
    return static_cast<CodeCompletionKeywordKind>(AssociatedKind);
  }

  CodeCompletionOperatorKind getKnownOperatorKind() const {
    assert(isOperator());
    return KnownOperatorKind;
  }

  bool isSystem() const { return IsSystem; };

  CodeCompletionString *getCompletionString() const { return CompletionString; }

  StringRef getModuleName() const { return ModuleName; }

  StringRef getBriefDocComment() const { return BriefDocComment; }

  ArrayRef<StringRef> getAssociatedUSRs() const { return AssociatedUSRs; }

  ArrayRef<CodeCompletionResultType *> getResultTypes() const {
    return ResultTypes;
  }

  ContextFreeNotRecommendedReason getNotRecommendedReason() const {
    return NotRecommended;
  }

  CodeCompletionDiagnosticSeverity getDiagnosticSeverity() const {
    return DiagnosticSeverity;
  }
  StringRef getDiagnosticMessage() const { return DiagnosticMessage; };

  bool isOperator() const {
    if (getKind() != CodeCompletionResultKind::Declaration)
      return getKind() == CodeCompletionResultKind::BuiltinOperator;
    switch (getAssociatedDeclKind()) {
    case CodeCompletionDeclKind::PrefixOperatorFunction:
    case CodeCompletionDeclKind::PostfixOperatorFunction:
    case CodeCompletionDeclKind::InfixOperatorFunction:
      return true;
    default:
      return false;
    }
  }

  static CodeCompletionOperatorKind
  getCodeCompletionOperatorKind(StringRef name);
  static CodeCompletionOperatorKind
  getCodeCompletionOperatorKind(CodeCompletionString *str);
  static CodeCompletionDeclKind getCodeCompletionDeclKind(const Decl *D);
  static bool getDeclIsSystem(const Decl *D);
};

/// The expected contextual type(s) for code-completion.
struct ExpectedTypeContext {
  /// Possible types of the code completion expression.
  llvm::SmallVector<Type, 4> possibleTypes;

  /// Pre typechecked type of the expression at the completion position.
  Type idealType;

  /// Whether the `ExpectedTypes` comes from a single-expression body, e.g.
  /// `foo({ here })`.
  ///
  /// Since the input may be incomplete, we take into account that the types are
  /// only a hint.
  bool isImplicitSingleExpressionReturn = false;
  bool preferNonVoid = false;

  bool empty() const { return possibleTypes.empty(); }
  bool requiresNonVoid() const {
    if (isImplicitSingleExpressionReturn)
      return false;
    if (preferNonVoid)
      return true;
    if (possibleTypes.empty())
      return false;
    return std::all_of(possibleTypes.begin(), possibleTypes.end(),
                       [](Type Ty) { return !Ty->isVoid(); });
  }

  ExpectedTypeContext() = default;
  ExpectedTypeContext(ArrayRef<Type> types, bool isImplicitSingleExprReturn)
      : possibleTypes(types.begin(), types.end()),
        isImplicitSingleExpressionReturn(isImplicitSingleExprReturn) {}
};

/// A single code completion result enriched with information that depend on
/// the completion's usage context.
class CodeCompletionResult {
public:
  /// Describes the relationship between the type of the completion results and
  /// the expected type at the code completion position.
  enum class ExpectedTypeRelation : uint8_t {
    /// The result does not have a type (e.g. keyword).
    NotApplicable,

    /// The type relation have not been calculated.
    Unknown,

    /// The relationship of the result's type to the expected type is not
    /// invalid, not convertible, and not identical.
    Unrelated,

    /// The result's type is invalid at the expected position.
    Invalid,

    /// The result's type is convertible to the type of the expected.
    Convertible,

    /// The result's type is identical to the type of the expected.
    Identical,

    MAX_VALUE = Identical
  };


private:
  ContextFreeCodeCompletionResult ContextFree;
  SemanticContextKind SemanticContext : 3;
  unsigned char Flair : 8;

  /// Contextual diagnostics. If the contextual not recommended reason is
  /// \c None, then the context free diagnostic will be shown to the user,
  /// otherwise the contextual diagnostic stored in this result is shown.
  ContextualNotRecommendedReason NotRecommended : 4;
  CodeCompletionDiagnosticSeverity DiagnosticSeverity : 3;
  StringRef DiagnosticMessage;

  /// The number of bytes to the left of the code completion point that
  /// should be erased first if this completion string is inserted in the
  /// editor buffer.
  unsigned NumBytesToErase : 7;

public:
  static const unsigned MaxNumBytesToErase = 127;

private:
  ExpectedTypeRelation TypeDistance : 3;

  // Assertions for limiting max values of enums.
  static_assert(int(SemanticContextKind::MAX_VALUE) < 1 << 3, "");
  static_assert(int(ContextualNotRecommendedReason::MAX_VALUE) < 1 << 4, "");
  static_assert(int(ExpectedTypeRelation::MAX_VALUE) < 1 << 3, "");

public:
  /// Memberwise initializer
  CodeCompletionResult(ContextFreeCodeCompletionResult ContextFree,
                       SemanticContextKind SemanticContext,
                       CodeCompletionFlair Flair, uint8_t NumBytesToErase,
                       ExpectedTypeRelation TypeDistance,
                       ContextualNotRecommendedReason NotRecommended,
                       CodeCompletionDiagnosticSeverity DiagnosticSeverity,
                       StringRef DiagnosticMessage)
      : ContextFree(ContextFree), SemanticContext(SemanticContext),
        Flair(Flair.toRaw()), NotRecommended(NotRecommended),
        DiagnosticSeverity(DiagnosticSeverity),
        DiagnosticMessage(DiagnosticMessage), NumBytesToErase(NumBytesToErase),
        TypeDistance(TypeDistance) {}

  /// Enrich a \c ContextFreeCodeCompletionResult with the following contextual
  /// information.
  /// This computes the type relation between the completion item and its
  /// expected type context.
  CodeCompletionResult(ContextFreeCodeCompletionResult ContextFree,
                       SemanticContextKind SemanticContext,
                       CodeCompletionFlair Flair, uint8_t NumBytesToErase,
                       const ExpectedTypeContext &TypeContext,
                       const DeclContext *DC,
                       ContextualNotRecommendedReason NotRecommended,
                       CodeCompletionDiagnosticSeverity DiagnosticSeverity,
                       StringRef DiagnosticMessage);

  const ContextFreeCodeCompletionResult &getContextFreeResult() const {
    return ContextFree;
  }

  /// Return a pointer to the data structure storing the context free
  /// properties.
  /// The pointer is valid as long as this result is alive.
  ContextFreeCodeCompletionResult *getContextFreeResultPtr() {
    return &ContextFree;
  }

  /// Copy this result to \p Sink with \p newFlair . Note that this does NOT
  /// copy the value of \c CompletionString , \c AssociatedUSRs etc. it only
  /// copies the pointers to them.
  CodeCompletionResult *withFlair(CodeCompletionFlair newFlair,
                                  CodeCompletionResultSink &Sink);

  CodeCompletionResultKind getKind() const {
    return getContextFreeResult().getKind();
  }

  CodeCompletionDeclKind getAssociatedDeclKind() const {
    return getContextFreeResult().getAssociatedDeclKind();
  }

  CodeCompletionLiteralKind getLiteralKind() const {
    return getContextFreeResult().getLiteralKind();
  }

  CodeCompletionKeywordKind getKeywordKind() const {
    return getContextFreeResult().getKeywordKind();
  }

  bool isOperator() const { return getContextFreeResult().isOperator(); }

  CodeCompletionOperatorKind getOperatorKind() const {
    return getContextFreeResult().getKnownOperatorKind();
  }

  bool isSystem() const { return getContextFreeResult().isSystem(); }

  ExpectedTypeRelation getExpectedTypeRelation() const { return TypeDistance; }

  /// Get the contextual not-recommended reason. This disregards context-free
  /// not recommended reasons.
  ContextualNotRecommendedReason getContextualNotRecommendedReason() const {
    return NotRecommended;
  }

  /// Return the contextual not recommended reason if there is one. If there is
  /// no contextual not recommended reason, return the context-free not
  /// recommended reason.
  NotRecommendedReason getNotRecommendedReason() const {
    switch (NotRecommended) {
    case ContextualNotRecommendedReason::None:
      switch (getContextFreeResult().getNotRecommendedReason()) {
      case ContextFreeNotRecommendedReason::None:
        return NotRecommendedReason::None;
      case ContextFreeNotRecommendedReason::Deprecated:
        return NotRecommendedReason::Deprecated;
      case ContextFreeNotRecommendedReason::SoftDeprecated:
        return NotRecommendedReason::SoftDeprecated;
      }
    case ContextualNotRecommendedReason::RedundantImport:
      return NotRecommendedReason::RedundantImport;
    case ContextualNotRecommendedReason::RedundantImportIndirect:
      return NotRecommendedReason::RedundantImportIndirect;
    case ContextualNotRecommendedReason::InvalidAsyncContext:
      return NotRecommendedReason::InvalidAsyncContext;
    case ContextualNotRecommendedReason::CrossActorReference:
      return NotRecommendedReason::CrossActorReference;
    case ContextualNotRecommendedReason::VariableUsedInOwnDefinition:
      return NotRecommendedReason::VariableUsedInOwnDefinition;
    }
  }

  SemanticContextKind getSemanticContext() const { return SemanticContext; }

  CodeCompletionFlair getFlair() const { return CodeCompletionFlair(Flair); }

  /// Modify "flair" of this result *in place*.
  void setFlair(CodeCompletionFlair flair) { Flair = flair.toRaw(); }

  bool isNotRecommended() const {
    return getNotRecommendedReason() != NotRecommendedReason::None;
  }

  unsigned getNumBytesToErase() const {
    return NumBytesToErase;
  }

  CodeCompletionString *getCompletionString() const {
    return getContextFreeResult().getCompletionString();
  }

  StringRef getModuleName() const {
    return getContextFreeResult().getModuleName();
  }

  StringRef getBriefDocComment() const {
    return getContextFreeResult().getBriefDocComment();
  }

  ArrayRef<StringRef> getAssociatedUSRs() const {
    return getContextFreeResult().getAssociatedUSRs();
  }

  /// Get the contextual diagnostic severity. This disregards context-free
  /// diagnostics.
  CodeCompletionDiagnosticSeverity getContextualDiagnosticSeverity() const {
    return DiagnosticSeverity;
  }

  /// Get the contextual diagnostic message. This disregards context-free
  /// diagnostics.
  StringRef getContextualDiagnosticMessage() const { return DiagnosticMessage; }

  /// Return the contextual diagnostic severity if there was a contextual
  /// diagnostic. If there is no contextual diagnostic, return the context-free
  /// diagnostic severity.
  CodeCompletionDiagnosticSeverity getDiagnosticSeverity() const {
    if (NotRecommended != ContextualNotRecommendedReason::None) {
      return DiagnosticSeverity;
    } else {
      return getContextFreeResult().getDiagnosticSeverity();
    }
  }

  /// Return the contextual diagnostic message if there was a contextual
  /// diagnostic. If there is no contextual diagnostic, return the context-free
  /// diagnostic message.
  StringRef getDiagnosticMessage() const {
    if (NotRecommended != ContextualNotRecommendedReason::None) {
      return DiagnosticMessage;
    } else {
      return getContextFreeResult().getDiagnosticMessage();
    }
  }

  /// Print a debug representation of the code completion result to \p OS.
  void printPrefix(raw_ostream &OS) const;
  SWIFT_DEBUG_DUMP;
};

struct CodeCompletionResultSink {
  using AllocatorPtr = std::shared_ptr<llvm::BumpPtrAllocator>;

  /// The allocator used to allocate results "native" to this sink.
  AllocatorPtr Allocator;

  /// Allocators that keep alive "foreign" results imported into this sink from
  /// other sinks.
  std::vector<AllocatorPtr> ForeignAllocators;

  /// The arena in which the \c CodeCompletionResultTypes for \c Results are
  /// stored.
  CodeCompletionResultTypeArenaRef ResultTypeArena;

  /// Whether to annotate the results with XML.
  bool annotateResult = false;

  /// Whether to emit object literals if desired.
  bool includeObjectLiterals = true;

  /// Whether to emit type initializers in addition to type names in expression
  /// position.
  bool addInitsToTopLevel = false;

  /// Whether to perform "call pettern heuristics".
  bool enableCallPatternHeuristics = false;

  std::vector<CodeCompletionResult *> Results;

  /// A single-element cache for module names stored in Allocator, keyed by a
  /// clang::Module * or swift::ModuleDecl *.
  std::pair<void *, StringRef> LastModule;

  CodeCompletionResultSink(
      const CodeCompletionResultTypeArenaRef &ResultTypeArena)
      : Allocator(std::make_shared<llvm::BumpPtrAllocator>()),
        ResultTypeArena(ResultTypeArena) {}
};

/// A utility for calculating the import depth of a given module. Direct imports
/// have depth 1, imports of those modules have depth 2, etc.
///
/// Special modules such as Playground auxiliary sources are considered depth
/// 0.
class ImportDepth {
  llvm::StringMap<uint8_t> depths;

public:
  ImportDepth() = default;
  ImportDepth(ASTContext &context, const FrontendOptions &frontendOptions);

  Optional<uint8_t> lookup(StringRef module) {
    auto I = depths.find(module);
    if (I == depths.end())
      return None;
    return I->getValue();
  }
};

class CodeCompletionContext {
  friend class CodeCompletionResultBuilder;

  /// A set of current completion results, not yet delivered to the
  /// consumer.
  /// Has the same ResultTypeArena as \c Cache.
  CodeCompletionResultSink CurrentResults;

public:
  /// Has the same ResultTypeArena as \c CurrentResults.
  CodeCompletionCache &Cache;
  CompletionKind CodeCompletionKind = CompletionKind::None;

  enum class TypeContextKind {
    /// There is no known contextual type. All types are equally good.
    None,

    /// There is a contextual type from a single-expression closure/function
    /// body. The context is a hint, and enables unresolved member completion,
    /// but should not hide any results.
    SingleExpressionBody,

    /// There are known contextual types, or there aren't but a nonvoid type is expected.
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

  CodeCompletionContext(CodeCompletionCache &Cache);

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

  void setCallPatternHeuristics(bool flag) {
    CurrentResults.enableCallPatternHeuristics = flag;
  }
  bool getCallPatternHeuristics() const {
    return CurrentResults.enableCallPatternHeuristics;
  }

  /// Allocate a string owned by the code completion context.
  StringRef copyString(StringRef Str);

  /// Return current code completion results.
  MutableArrayRef<CodeCompletionResult *> takeResults();

  /// Sort code completion results in an implementation-defined order
  /// in place.
  static void sortCompletionResults(
      MutableArrayRef<CodeCompletionResult *> Results);

  CodeCompletionResultSink &getResultSink() {
    return CurrentResults;
  }
};

struct SwiftCompletionInfo {
  swift::ASTContext *swiftASTContext = nullptr;
  const swift::CompilerInvocation *invocation = nullptr;
  CodeCompletionContext *completionContext = nullptr;
};

/// An abstract base class for consumers of code completion results.
/// \see \c SimpleCachingCodeCompletionConsumer.
class CodeCompletionConsumer {
public:
  virtual ~CodeCompletionConsumer() {}
  virtual void
  handleResultsAndModules(CodeCompletionContext &context,
                          ArrayRef<RequestedCachedModule> requestedModules,
                          DeclContext *DC,
                          const ExpectedTypeContext &TypeContext) = 0;
};

/// A simplified code completion consumer interface that clients can use to get
/// CodeCompletionResults with automatic caching of top-level completions from
/// imported modules.
struct SimpleCachingCodeCompletionConsumer : public CodeCompletionConsumer {

  // Implement the CodeCompletionConsumer interface.
  void handleResultsAndModules(CodeCompletionContext &context,
                               ArrayRef<RequestedCachedModule> requestedModules,
                               DeclContext *DCForModules,
                               const ExpectedTypeContext &TypeContext) override;

  /// Clients should override this method to receive \p Results.
  virtual void handleResults(CodeCompletionContext &context) = 0;
};

/// Create a factory for code completion callbacks.
CodeCompletionCallbacksFactory *
makeCodeCompletionCallbacksFactory(CodeCompletionContext &CompletionContext,
                                   CodeCompletionConsumer &Consumer);

/// Lookup the top-level code completions from \p module and store them in
/// \p targetSink.
///
/// Results are looked up as if in \p currDeclContext, which may be null.
void lookupCodeCompletionResultsFromModule(CodeCompletionResultSink &targetSink,
                                           const ModuleDecl *module,
                                           ArrayRef<std::string> accessPath,
                                           bool needLeadingDot,
                                           const SourceFile *SF);

} // end namespace ide
} // end namespace swift

namespace llvm {
template <> struct DenseMapInfo<swift::ide::CodeCompletionKeywordKind> {
  using Kind = swift::ide::CodeCompletionKeywordKind;
  static Kind getEmptyKey() { return Kind(~0u); }
  static Kind getTombstoneKey() { return Kind(~1u); }
  static unsigned getHashValue(const Kind &Val) { return unsigned(Val); }
  static bool isEqual(const Kind &LHS, const Kind &RHS) { return LHS == RHS; }
};
template <> struct DenseMapInfo<swift::ide::CodeCompletionLiteralKind> {
  using Kind = swift::ide::CodeCompletionLiteralKind;
  static Kind getEmptyKey() { return Kind(~0u); }
  static Kind getTombstoneKey() { return Kind(~1u); }
  static unsigned getHashValue(const Kind &Val) { return unsigned(Val); }
  static bool isEqual(const Kind &LHS, const Kind &RHS) { return LHS == RHS; }
};
template <> struct DenseMapInfo<swift::ide::CodeCompletionDeclKind> {
  using Kind = swift::ide::CodeCompletionDeclKind;
  static Kind getEmptyKey() { return Kind(~0u); }
  static Kind getTombstoneKey() { return Kind(~1u); }
  static unsigned getHashValue(const Kind &Val) { return unsigned(Val); }
  static bool isEqual(const Kind &LHS, const Kind &RHS) { return LHS == RHS; }
};
}

#endif // SWIFT_IDE_CODECOMPLETION_H
