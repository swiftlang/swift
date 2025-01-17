//===--- LangSupport.h - ----------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_CORE_LANGSUPPORT_H
#define LLVM_SOURCEKIT_CORE_LANGSUPPORT_H

#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/CancellationToken.h"
#include "SourceKit/Support/UIdent.h"
#include "swift/AST/Type.h"
#include "swift/IDE/CancellableResult.h"
#include "swift/IDE/CodeCompletionResult.h"
#include "swift/Refactoring/RenameLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/VersionTuple.h"
#include "llvm/Support/VirtualFileSystem.h"
#include <functional>
#include <memory>
#include <optional>
#include <unordered_set>
#include <variant>

namespace llvm {
  class MemoryBuffer;
}

namespace SourceKit {
class GlobalConfig;
using swift::ide::CancellableResult;
using swift::ide::RenameLoc;

struct EntityInfo {
  UIdent Kind;
  StringRef Name;
  StringRef USR;
  StringRef Group;
  StringRef ReceiverUSR;
  bool IsDynamic = false;
  bool IsTestCandidate = false;
  bool IsImplicit = false;
  unsigned Line = 0;
  unsigned Column = 0;
  ArrayRef<UIdent> Attrs;
  std::optional<UIdent> EffectiveAccess;

  EntityInfo() = default;
};

class IndexingConsumer {
  virtual void anchor();

public:
  virtual ~IndexingConsumer() { }

  virtual void failed(StringRef ErrDescription) = 0;

  virtual bool startDependency(UIdent Kind,
                               StringRef Name,
                               StringRef Path,
                               bool IsSystem) = 0;

  virtual bool finishDependency(UIdent Kind) = 0;

  virtual bool startSourceEntity(const EntityInfo &Info) = 0;

  virtual bool recordRelatedEntity(const EntityInfo &Info) = 0;

  virtual bool finishSourceEntity(UIdent Kind) = 0;
};

struct CodeCompletionInfo {
  UIdent Kind;
  // We need a separate field to passthrough custom kinds that originally came
  // from the client, because we can't safely construct a UIdent for them.
  void *CustomKind = nullptr;
  StringRef Name;
  StringRef Description;
  StringRef SourceText;
  StringRef TypeName;
  StringRef ModuleName;
  StringRef DocBrief;
  StringRef AssocUSRs;
  UIdent SemanticContext;
  UIdent TypeRelation;
  std::optional<uint8_t> ModuleImportDepth;
  bool NotRecommended;
  bool IsSystem;
  unsigned NumBytesToErase;

  struct IndexRange {
    unsigned begin = 0;
    unsigned end = 0;
    IndexRange() = default;
    IndexRange(unsigned begin, unsigned end) : begin(begin), end(end) {}
    unsigned length() const { return end - begin; }
    bool empty() const { return end == begin; }
  };

  struct ParameterStructure {
    IndexRange name;
    IndexRange afterColon;
    bool isLocalName = false;
    IndexRange range() const {
      if (!name.empty()) // if we have both, name comes before afterColon.
        return {name.begin, afterColon.empty() ? name.end : afterColon.end};
      return afterColon;
    }
  };

  struct DescriptionStructure {
    IndexRange baseName;
    IndexRange parameterRange;
  };

  std::optional<DescriptionStructure> descriptionStructure;
  std::optional<ArrayRef<ParameterStructure>> parametersStructure;
};

struct ExpressionType {
  unsigned ExprOffset;
  unsigned ExprLength;
  unsigned TypeOffset;
  std::vector<unsigned> ProtocolOffsets;
};

struct ExpressionTypesInFile {
  std::vector<ExpressionType> Results;
  StringRef TypeBuffer;
};

struct VariableType {
  /// The variable identifier's offset in the file.
  unsigned VarOffset;
  /// The variable identifier's length.
  unsigned VarLength;
  /// The offset of the type's string representation inside
  /// `VariableTypesInFile.TypeBuffer`.
  unsigned TypeOffset;
  /// Whether the variable declaration has an explicit type annotation.
  bool HasExplicitType;
};

struct VariableTypesInFile {
  /// The typed variable declarations in the file.
  std::vector<VariableType> Results;
  /// A String containing the printed representation of all types in
  /// `Results`. Entries in `Results` refer to their types by using
  /// an offset into this string.
  StringRef TypeBuffer;
};

class CodeCompletionConsumer {
  virtual void anchor();

public:
  virtual ~CodeCompletionConsumer() { }

  virtual void failed(StringRef ErrDescription) = 0;
  virtual void cancelled() = 0;

  virtual void setCompletionKind(UIdent kind) {};
  virtual void setReusingASTContext(bool) = 0;
  virtual void setAnnotatedTypename(bool) = 0;
  virtual bool handleResult(const CodeCompletionInfo &Info) = 0;
};

class GroupedCodeCompletionConsumer : public CodeCompletionConsumer {
public:
  virtual void startGroup(UIdent kind, StringRef name) = 0;
  virtual void endGroup() = 0;
  virtual void setNextRequestStart(unsigned offset) = 0;
};

struct CustomCompletionInfo {
  std::string Name;
  void *Kind;

  enum Context {
    Stmt = 1 << 0,
    Expr = 1 << 1,
    Type = 1 << 2,
    ForEachSequence = 1 << 3,
  };
  swift::OptionSet<Context> Contexts;
};

struct FilterRule {
  enum Kind {
    Everything,
    Module,
    Keyword,
    Literal,
    CustomCompletion,
    Identifier,
    Description,
  };
  Kind kind;
  bool hide;
  std::vector<StringRef> names; ///< Must be null-terminated.
  std::vector<UIdent> uids;
};

enum class DiagnosticSeverityKind {
  Warning,
  Error
};

enum class DiagnosticCategory {
  Deprecation,
  NoUsage
};

struct RawCharSourceRange {
  unsigned Offset;
  unsigned Length;
};

enum class MacroRoleBits: uint8_t {
#define MACRO_ROLE(Name, Description) Name,
#include "swift/Basic/MacroRoles.def"
};

/// The context in which a macro can be used, which determines the syntax it
/// uses.
enum class MacroRole: uint32_t {
#define MACRO_ROLE(Name, Description) \
    Name = 1 << static_cast<uint8_t>(MacroRoleBits::Name),
#include "swift/Basic/MacroRoles.def"
};
using MacroRoles = swift::OptionSet<MacroRole>;

struct MacroExpansionInfo {
  // See swift::ExternalMacroReference.
  struct ExternalMacroReference {
    std::string moduleName;
    std::string typeName;

    ExternalMacroReference(StringRef moduleName, StringRef typeName)
        : moduleName(moduleName), typeName(typeName){};
  };
  // See swift::ExpandedMacroDefinition.
  struct ExpandedMacroDefinition {
    // 'Replacement.range' references some part of code in 'expansionText'.
    // 'expansionText' will be replaced by the 'parameterIndex'-th argument of
    // the macro.
    struct Replacement {
      RawCharSourceRange range;
      unsigned parameterIndex;
      Replacement(RawCharSourceRange range, unsigned parameterIndex)
          : range(range), parameterIndex(parameterIndex) {}
    };
    std::string expansionText;
    std::vector<Replacement> replacements;
    std::vector<Replacement> genericReplacements;

    ExpandedMacroDefinition(StringRef expansionText)
        : expansionText(expansionText), replacements(), genericReplacements() {};
  };

  // Offset of the macro expansion syntax (i.e. attribute or #<macro name>) from
  // the start of the source file.
  unsigned offset;

  // Macro roles.
  MacroRoles roles;

  // Tagged union of macro definition.
  std::variant<ExternalMacroReference, ExpandedMacroDefinition> macroDefinition;

  MacroExpansionInfo(unsigned offset, MacroRoles roles,
                     ExternalMacroReference macroRef)
      : offset(offset), roles(roles), macroDefinition(macroRef) {}
  MacroExpansionInfo(unsigned offset, MacroRoles roles,
                     ExpandedMacroDefinition definition)
      : offset(offset), roles(roles), macroDefinition(definition) {}
};

/// Stores information about a given buffer, including its name and, if
/// generated, its source text and original location.
struct BufferInfo {
  struct OriginalLocation {
    std::shared_ptr<const BufferInfo> OrigBufferInfo;
    unsigned OrigBufferID;
    RawCharSourceRange Range;

    OriginalLocation(std::shared_ptr<const BufferInfo> OrigBufferInfo,
                     unsigned OrigBufferID, RawCharSourceRange Range)
        : OrigBufferInfo(std::move(OrigBufferInfo)), OrigBufferID(OrigBufferID),
          Range(Range) {}
  };

  std::string BufferName;
  std::optional<std::string> Contents;
  std::optional<OriginalLocation> OrigLocation;

  BufferInfo(std::string BufferName, std::optional<std::string> Contents,
             std::optional<OriginalLocation> OrigLocation)
      : BufferName(std::move(BufferName)), Contents(std::move(Contents)),
        OrigLocation(std::move(OrigLocation)) {}
};
using BufferInfoSharedPtr = std::shared_ptr<const BufferInfo>;

struct DiagnosticEntryInfoBase {
  struct Fixit {
    RawCharSourceRange Range;
    std::string Text;

    Fixit(RawCharSourceRange Range, std::string Text)
        : Range(Range), Text(std::move(Text)) {}
  };

  std::string ID;
  std::string Description;
  unsigned Offset = 0;
  unsigned Line = 0;
  unsigned Column = 0;
  BufferInfoSharedPtr FileInfo;
  SmallVector<DiagnosticCategory, 1> Categories;
  SmallVector<RawCharSourceRange, 2> Ranges;
  SmallVector<Fixit, 2> Fixits;
  SmallVector<std::string, 1> EducationalNotePaths;
};

struct DiagnosticEntryInfo : DiagnosticEntryInfoBase {
  DiagnosticSeverityKind Severity = DiagnosticSeverityKind::Error;
  SmallVector<DiagnosticEntryInfoBase, 1> Notes;
};

struct SwiftSemanticToken {
  unsigned ByteOffset;
  unsigned Length : 24;
  // The code-completion kinds are a good match for the semantic kinds we want.
  // FIXME: Maybe rename CodeCompletionDeclKind to a more general concept ?
  swift::ide::CodeCompletionDeclKind Kind : 6;
  unsigned IsRef : 1;
  unsigned IsSystem : 1;

  SwiftSemanticToken(swift::ide::CodeCompletionDeclKind Kind,
                     unsigned ByteOffset, unsigned Length, bool IsRef,
                     bool IsSystem)
      : ByteOffset(ByteOffset), Length(Length), Kind(Kind), IsRef(IsRef),
        IsSystem(IsSystem) {}

  bool getIsRef() const { return static_cast<bool>(IsRef); }

  bool getIsSystem() const { return static_cast<bool>(IsSystem); }

  UIdent getUIdentForKind();
};

#if !defined(_MSC_VER)
static_assert(sizeof(SwiftSemanticToken) == 8, "Too big");
// FIXME: MSVC doesn't pack bitfields with different underlying types.
// Giving up to check this in MSVC for now, because static_assert is only for
// keeping low memory usage.
#endif

struct SourceFileRange {
  /// The byte offset at which the range begins
  uintptr_t Start;
  /// The byte offset at which the end ends
  uintptr_t End;
};

class EditorConsumer {
  virtual void anchor();
public:
  virtual ~EditorConsumer() { }

  virtual bool needsSemanticInfo() { return true; }

  virtual void handleRequestError(const char *Description) = 0;

  virtual bool syntaxMapEnabled() = 0;
  virtual void handleSyntaxMap(unsigned Offset, unsigned Length,
                               UIdent Kind) = 0;

  virtual bool documentStructureEnabled() = 0;

  virtual void handleSemanticAnnotation(unsigned Offset, unsigned Length,
                                        UIdent Kind, bool isSystem) = 0;

  virtual void handleDeclaration(unsigned Offset, unsigned Length, UIdent Kind,
                                 StringRef USR) = 0;

  virtual void beginDocumentSubStructure(unsigned Offset, unsigned Length,
                                         UIdent Kind, UIdent AccessLevel,
                                         UIdent SetterAccessLevel,
                                         unsigned NameOffset,
                                         unsigned NameLength,
                                         unsigned BodyOffset,
                                         unsigned BodyLength,
                                         unsigned DocOffset,
                                         unsigned DocLength,
                                         StringRef DisplayName,
                                         StringRef TypeName,
                                         StringRef RuntimeName,
                                         StringRef SelectorName,
                                         ArrayRef<StringRef> InheritedTypes,
                                         ArrayRef<std::tuple<UIdent, unsigned, unsigned>> Attrs) = 0;

  virtual void endDocumentSubStructure() = 0;

  virtual void handleDocumentSubStructureElement(UIdent Kind, unsigned Offset,
                                                 unsigned Length) = 0;

  virtual void recordAffectedRange(unsigned Offset, unsigned Length) = 0;

  virtual void recordAffectedLineRange(unsigned Line, unsigned Length) = 0;

  virtual void recordFormattedText(StringRef Text) = 0;

  virtual bool diagnosticsEnabled() = 0;

  virtual void handleDiagnostics(ArrayRef<DiagnosticEntryInfo> DiagInfos,
                                 UIdent DiagStage) = 0;

  virtual void handleSourceText(StringRef Text) = 0;

  virtual void finished() {}
};

class OptionsDictionary {
  virtual void anchor();
public:
  virtual ~OptionsDictionary() {}

  virtual bool valueForOption(UIdent Key, unsigned &Val) = 0;
  virtual bool valueForOption(UIdent Key, bool &Val) = 0;
  virtual bool valueForOption(UIdent Key, StringRef &Val) = 0;
  virtual bool forEach(UIdent key, llvm::function_ref<bool(OptionsDictionary &)> applier) = 0;
};

struct Statistic;
typedef std::function<void(ArrayRef<Statistic *> stats)> StatisticsReceiver;

/// Options for configuring a virtual file system provider.
struct VFSOptions {
  /// The name of the virtual file system to use.
  std::string name;

  /// Arguments for the virtual file system provider (may be null).
  // FIXME: the lifetime is actually limited by the RequestDict.
  std::unique_ptr<OptionsDictionary> options;
};

/// Used to wrap the result of a request. There are three possibilities:
/// - The request succeeded (`value` is valid)
/// - The request was cancelled
/// - The request failed (with an `error`)
///
/// NOTE: This type does not own its `value` or `error`. Therefore, it's not
/// safe to store this type, nor is it safe to store its `value` or `error`.
/// Instead, any needed information should be fetched and stored (e.g. reading
/// properties from `value` or getting a `std::string` from `error`).
template <typename T>
class RequestResult {
  enum Type {
    Value,
    Error,
    Cancelled
  };
  union {
    const T *data;
    StringRef error;
  };
  RequestResult::Type type;

  RequestResult(const T &V): data(&V), type(Value) {}
  RequestResult(StringRef E): error(E), type(Error) {}
  RequestResult(): type(Cancelled) {}

public:
  static RequestResult fromResult(const T &value) {
    return RequestResult(value);
  }
  static RequestResult fromError(StringRef error) {
    return RequestResult(error);
  }
  static RequestResult cancelled() {
    return RequestResult();
  }

  bool isValue() const { return type == Value; }
  const T &value() const {
    assert(type == Value);
    return *data;
  }
  bool isError() const {
    return type == Error;
  }
  StringRef getError() const {
    assert(type == Error);
    return error;
  }
  bool isCancelled() const {
    return type == Cancelled;
  }
};

struct RefactoringInfo {
  UIdent Kind;
  StringRef KindName;
  StringRef UnavailableReason;

  RefactoringInfo(UIdent Kind, StringRef KindName, StringRef UnavailableReason)
      : Kind(Kind), KindName(KindName), UnavailableReason(UnavailableReason) {}

  void print(llvm::raw_ostream &OS, std::string Indentation) const {
    OS << Indentation << "RefactoringInfo" << '\n';
    OS << Indentation << "  Kind: " << Kind.getName() << '\n';
    OS << Indentation << "  KindName: " << KindName << '\n';
    OS << Indentation << "  UnavailableReason: " << UnavailableReason << '\n';
  }

  SWIFT_DEBUG_DUMP { print(llvm::errs(), ""); }
};

struct ParentInfo {
  StringRef Title;
  StringRef KindName;
  StringRef USR;

  ParentInfo(StringRef Title, StringRef KindName, StringRef USR)
      : Title(Title), KindName(KindName), USR(USR) {}

  void print(llvm::raw_ostream &OS, std::string Indentation) const {
    OS << Indentation << "ParentInfo" << '\n';
    OS << Indentation << "  Title: " << Title << '\n';
    OS << Indentation << "  KindName: " << KindName << '\n';
    OS << Indentation << "  USR: " << USR << '\n';
  }

  SWIFT_DEBUG_DUMP { print(llvm::errs(), ""); }
};

struct ReferencedDeclInfo {
  StringRef USR;
  UIdent DeclarationLang;
  StringRef AccessLevel;
  StringRef FilePath;
  StringRef ModuleName;
  bool IsSystem;
  bool IsSPI;
  ArrayRef<ParentInfo> ParentContexts;

  ReferencedDeclInfo(StringRef USR, UIdent DeclLang, StringRef AccessLevel,
                     StringRef FilePath, StringRef ModuleName, bool System,
                     bool SPI, ArrayRef<ParentInfo> Parents)
      : USR(USR), DeclarationLang(DeclLang), AccessLevel(AccessLevel),
        FilePath(FilePath), ModuleName(ModuleName), IsSystem(System),
        IsSPI(SPI), ParentContexts(Parents) {}

  void print(llvm::raw_ostream &OS, std::string Indentation) const {
    OS << Indentation << "ReferencedDeclInfo" << '\n';
    OS << Indentation << "  USR: " << USR << '\n';
    OS << Indentation << "  DeclarationLang: " << DeclarationLang.getName()
       << '\n';
    OS << Indentation << "  AccessLevel: " << AccessLevel << '\n';
    OS << Indentation << "  FilePath: " << FilePath << '\n';
    OS << Indentation << "  ModuleName: " << ModuleName << '\n';
    OS << Indentation << "  IsSystem: " << IsSystem << '\n';
    OS << Indentation << "  IsSPI: " << IsSPI << '\n';
    OS << Indentation << "  ParentContexts:" << '\n';
    for (auto ParentCtx : ParentContexts) {
      ParentCtx.print(OS, Indentation + "   ");
    }
  }

  SWIFT_DEBUG_DUMP { print(llvm::errs(), ""); }
};

struct LocationInfo {
  StringRef Filename;
  unsigned Offset = 0;
  unsigned Length = 0;
  unsigned Line = 0;
  unsigned Column = 0;

  void print(llvm::raw_ostream &OS, std::string Indentation) const {
    OS << Indentation << "LocationInfo" << '\n';
    OS << Indentation << "  Filename: " << Filename << '\n';
    OS << Indentation << "  Offset: " << Offset << '\n';
    OS << Indentation << "  Length: " << Length << '\n';
    OS << Indentation << "  Line: " << Line << '\n';
    OS << Indentation << "  Column: " << Column << '\n';
  }

  SWIFT_DEBUG_DUMP { print(llvm::errs(), ""); }
};

struct CursorSymbolInfo {
  UIdent Kind;
  UIdent DeclarationLang;
  StringRef Name;
  StringRef USR;
  StringRef TypeName;
  StringRef TypeUSR;
  StringRef ContainerTypeUSR;
  StringRef DocComment;
  StringRef DocCommentAsXML;
  StringRef GroupName;
  /// A key for documentation comment localization, if it exists in the doc
  /// comment for the declaration.
  StringRef LocalizationKey;
  /// Annotated XML pretty printed declaration.
  StringRef AnnotatedDeclaration;
  /// Fully annotated XML pretty printed declaration.
  /// FIXME: this should eventually replace \c AnnotatedDeclaration.
  StringRef FullyAnnotatedDeclaration;
  /// The SymbolGraph JSON for this declaration.
  StringRef SymbolGraph;
  /// Non-empty if the symbol was imported from a clang module.
  StringRef ModuleName;
  /// Non-empty if a generated interface editor document has previously been
  /// opened for the module the symbol came from.
  StringRef ModuleInterfaceName;
  /// Filename is non-empty if there's a source location.
  LocationInfo Location;
  /// For methods this lists the USRs of the overrides in the class hierarchy.
  ArrayRef<StringRef> OverrideUSRs;
  /// Related declarations, overloaded functions etc., in annotated XML form.
  ArrayRef<StringRef> AnnotatedRelatedDeclarations;
  /// All groups of the module name under cursor.
  ArrayRef<StringRef> ModuleGroupArray;
  /// Stores the Symbol Graph title, kind, and USR of the parent contexts of the
  /// symbol under the cursor.
  ArrayRef<ParentInfo> ParentContexts;
  /// The set of decls referenced in the symbol graph declaration fragments.
  ArrayRef<ReferencedDeclInfo> ReferencedSymbols;
  /// For calls this lists the USRs of the receiver types (multiple only in the
  /// case that the base is a protocol composition).
  ArrayRef<StringRef> ReceiverUSRs;

  bool IsSystem = false;
  bool IsDynamic = false;
  bool IsSynthesized = false;

  std::optional<unsigned> ParentNameOffset;

  void print(llvm::raw_ostream &OS, std::string Indentation) const {
    OS << Indentation << "CursorSymbolInfo" << '\n';
    OS << Indentation << "  Kind: " << Kind.getName() << '\n';
    OS << Indentation << "  DeclarationLang: " << DeclarationLang.getName()
       << '\n';
    OS << Indentation << "  Name: " << Name << '\n';
    OS << Indentation << "  USR: " << USR << '\n';
    OS << Indentation << "  TypeName: " << TypeName << '\n';
    OS << Indentation << "  TypeUSR: " << TypeUSR << '\n';
    OS << Indentation << "  ContainerTypeUSR: " << ContainerTypeUSR << '\n';
    OS << Indentation << "  DocComment: " << DocComment << '\n';
    OS << Indentation << "  DocCommentAsXML: " << DocCommentAsXML << '\n';
    OS << Indentation << "  GroupName: " << GroupName << '\n';
    OS << Indentation << "  LocalizationKey: " << LocalizationKey << '\n';
    OS << Indentation << "  AnnotatedDeclaration: " << AnnotatedDeclaration
       << '\n';
    OS << Indentation
       << "  FullyAnnotatedDeclaration: " << FullyAnnotatedDeclaration << '\n';
    OS << Indentation << "  SymbolGraph: " << SymbolGraph << '\n';
    OS << Indentation << "  ModuleName: " << ModuleName << '\n';
    OS << Indentation << "  ModuleInterfaceName: " << ModuleInterfaceName
       << '\n';
    Location.print(OS, Indentation + "  ");
    OS << Indentation << "  OverrideUSRs:" << '\n';
    for (auto OverrideUSR : OverrideUSRs) {
      OS << Indentation << "    " << OverrideUSR << '\n';
    }
    OS << Indentation << "  AnnotatedRelatedDeclarations:" << '\n';
    for (auto AnnotatedRelatedDeclaration : AnnotatedRelatedDeclarations) {
      OS << Indentation << "    " << AnnotatedRelatedDeclaration << '\n';
    }
    OS << Indentation << "  ModuleGroupArray:" << '\n';
    for (auto ModuleGroup : ModuleGroupArray) {
      OS << Indentation << "    " << ModuleGroup << '\n';
    }
    OS << Indentation << "  ParentContexts:" << '\n';
    for (auto ParentContext : ParentContexts) {
      ParentContext.print(OS, Indentation + "    ");
    }

    llvm::SmallVector<ReferencedDeclInfo> SortedReferencedSymbols(
        ReferencedSymbols.begin(), ReferencedSymbols.end());
    std::sort(SortedReferencedSymbols.begin(), SortedReferencedSymbols.end(),
              [](const ReferencedDeclInfo &LHS, const ReferencedDeclInfo &RHS) {
                return LHS.USR < RHS.USR;
              });
    OS << Indentation << "ReferencedSymbols:" << '\n';
    for (auto ReferencedSymbol : SortedReferencedSymbols) {
      ReferencedSymbol.print(OS, Indentation + "    ");
    }
    OS << Indentation << "ReceiverUSRs:" << '\n';
    for (auto ReceiverUSR : ReceiverUSRs) {
      OS << Indentation << "    " << ReceiverUSR << '\n';
    }
    OS << Indentation << "IsSystem: " << IsSystem << '\n';
    OS << Indentation << "IsDynamic: " << IsDynamic << '\n';
    OS << Indentation << "IsSynthesized: " << IsSynthesized << '\n';
    OS << Indentation << "ParentNameOffset: " << ParentNameOffset << '\n';
  }

  SWIFT_DEBUG_DUMP { print(llvm::errs(), ""); }
};

struct CursorInfoData {
  // If nonempty, a proper Info could not be resolved (and the rest of the Info
  // will be empty). Clients can potentially use this to show a diagnostic
  // message to the user in lieu of using the empty response.
  StringRef InternalDiagnostic;
  llvm::SmallVector<CursorSymbolInfo, 1> Symbols;
  /// All available actions on the code under cursor.
  llvm::SmallVector<RefactoringInfo, 8> AvailableActions;
  /// Whether the ASTContext was reused for this cursor info.
  bool DidReuseAST = false;
  /// An allocator that can be used to allocate data that is referenced by this
  /// \c CursorInfoData.
  llvm::BumpPtrAllocator Allocator;

  bool isEmpty() const {
    return InternalDiagnostic.empty() && Symbols.empty() &&
           AvailableActions.empty();
  }

  void print(llvm::raw_ostream &OS, std::string Indentation) const {
    OS << Indentation << "CursorInfoData" << '\n';
    OS << Indentation << "  Symbols:" << '\n';
    for (auto Symbol : Symbols) {
      Symbol.print(OS, Indentation + "    ");
    }
    OS << Indentation << "  AvailableActions:" << '\n';
    for (auto AvailableAction : AvailableActions) {
      AvailableAction.print(OS, Indentation + "    ");
    }
    OS << Indentation << "DidReuseAST: " << DidReuseAST << '\n';
  }

  SWIFT_DEBUG_DUMP { print(llvm::errs(), ""); }
};

/// The result type of `LangSupport::getDiagnostics`
typedef ArrayRef<DiagnosticEntryInfo> DiagnosticsResult;

/// The result of `LangSupport::getSemanticTokens`.
typedef std::vector<SwiftSemanticToken> SemanticTokensResult;

struct RangeInfo {
  UIdent RangeKind;
  StringRef ExprType;
  StringRef RangeContent;
};

struct NameTranslatingInfo {
  // If nonempty, a proper Info could not be resolved (and the rest of the Info
  // will be empty). Clients can potentially use this to show a diagnostic
  // message to the user in lieu of using the empty response.
  StringRef InternalDiagnostic;

  UIdent NameKind;
  StringRef BaseName;
  std::vector<StringRef> ArgNames;
  bool IsZeroArgSelector = false;
};

enum class SemanticRefactoringKind {
  None,
#define SEMANTIC_REFACTORING(KIND, NAME, ID) KIND,
#include "swift/Refactoring/RefactoringKinds.def"
};

struct SemanticRefactoringInfo {
  SemanticRefactoringKind Kind;
  // The name of the input buffer to start the refactoring in. This must either
  // be empty (in which case the primary file for the AST is used), or exactly
  // match the buffer identifier stored in the source manager.
  StringRef InputBufferName;
  unsigned Line;
  unsigned Column;
  unsigned Length;
  StringRef PreferredName;
};

struct RelatedIdentInfo {
  unsigned Offset;
  unsigned Length;
  swift::ide::RenameLocUsage Usage;
};

/// Result of `findRelatedIdentifiersInFile`.
struct RelatedIdentsResult {
  SmallVector<RelatedIdentInfo, 8> RelatedIdents;
  std::optional<std::string> OldName;

  RelatedIdentsResult(SmallVector<RelatedIdentInfo, 8> RelatedIdents,
                      std::optional<std::string> OldName)
      : RelatedIdents(RelatedIdents), OldName(OldName) {}

  static RelatedIdentsResult empty() { return RelatedIdentsResult({}, ""); }
};

/// Represent one branch of an if config.
/// Either `#if`, `#else` or `#elseif`.
struct IfConfigInfo {
  unsigned Offset;
  bool IsActive;

  IfConfigInfo(unsigned Offset, bool IsActive)
      : Offset(Offset), IsActive(IsActive) {}
};

struct ActiveRegionsInfo {
  ArrayRef<IfConfigInfo> Configs;
};

/// Filled out by LangSupport::findInterfaceDocument().
struct InterfaceDocInfo {
  /// Non-empty if a generated interface editor document has previously been
  /// opened for the requested module name.
  StringRef ModuleInterfaceName;
  /// The subset of compiler arguments that are relevant for the interface
  /// generation.
  ArrayRef<StringRef> CompilerArgs;
};

struct DocGenericParam {
  std::string Name;
  std::string Inherits;
};

struct DocEntityInfo {
  UIdent Kind;
  llvm::SmallString<32> Name;
  llvm::SmallString<32> SubModuleName;
  llvm::SmallString<32> Argument;
  llvm::SmallString<64> USR;
  llvm::SmallString<64> OriginalUSR;
  llvm::SmallString<64> ProvideImplementationOfUSR;
  llvm::SmallString<64> DocCommentAsXML;
  llvm::SmallString<64> FullyAnnotatedDecl;
  llvm::SmallString<64> FullyAnnotatedGenericSig;
  llvm::SmallString<64> LocalizationKey;
  std::vector<DocGenericParam> GenericParams;
  std::vector<std::string> GenericRequirements;
  std::vector<std::string> RequiredBystanders;
  unsigned Offset = 0;
  unsigned Length = 0;
  bool IsUnavailable = false;
  bool IsDeprecated = false;
  bool IsOptional = false;
  bool IsAsync = false;
  swift::Type Ty;
};

struct AvailableAttrInfo {
  UIdent AttrKind;
  bool IsUnavailable = false;
  bool IsDeprecated = false;
  UIdent Platform;
  llvm::SmallString<32> Message;
  std::optional<llvm::VersionTuple> Introduced;
  std::optional<llvm::VersionTuple> Deprecated;
  std::optional<llvm::VersionTuple> Obsoleted;
};

struct NoteRegion {
  UIdent Kind;
  unsigned StartLine;
  unsigned StartColumn;
  unsigned EndLine;
  unsigned EndColumn;
  std::optional<unsigned> ArgIndex;
};

struct Edit {
  /// If the edit is outside of the originally request source file, the path
  /// to the file it is editing.
  std::string Path;
  unsigned StartLine;
  unsigned StartColumn;
  unsigned EndLine;
  unsigned EndColumn;
  /// If the edit is actually a file (which could be generated/from an
  /// expansion), the name (or path) of that buffer.
  std::string BufferName;
  std::string NewText;
  SmallVector<NoteRegion, 2> RegionsWithNote;
};

struct CategorizedEdits {
  UIdent Category;
  ArrayRef<Edit> Edits;
};

struct RenameRangeDetail {
  unsigned StartLine;
  unsigned StartColumn;
  unsigned EndLine;
  unsigned EndColumn;
  UIdent Kind;
  std::optional<unsigned> ArgIndex;
};

struct CategorizedRenameRanges {
  UIdent Category;
  std::vector<RenameRangeDetail> Ranges;
};

struct IndexStoreOptions {
  std::string IndexStorePath;
  std::string IndexUnitOutputPath;
  bool IgnoreClangModules = false;
  bool IncludeSystemModules = false;
  bool IgnoreStdlib = false;
  bool DisableImplicitModules = false;
  bool IncludeLocals = false;
};

struct IndexStoreInfo{};

typedef std::function<void(RequestResult<ArrayRef<CategorizedEdits>> Result)>
    CategorizedEditsReceiver;
typedef std::function<void(
    CancellableResult<std::vector<CategorizedRenameRanges>> Result)>
    CategorizedRenameRangesReceiver;
typedef std::function<void(RequestResult<IndexStoreInfo> Result)> IndexToStoreReceiver;

class DocInfoConsumer {
  virtual void anchor();

public:
  virtual ~DocInfoConsumer() { }

  virtual void failed(StringRef ErrDescription) = 0;

  virtual bool handleSourceText(StringRef Text) = 0;

  virtual bool handleAnnotation(const DocEntityInfo &Info) = 0;

  virtual bool startSourceEntity(const DocEntityInfo &Info) = 0;

  virtual bool handleInheritsEntity(const DocEntityInfo &Info) = 0;
  virtual bool handleConformsToEntity(const DocEntityInfo &Info) = 0;
  virtual bool handleExtendsEntity(const DocEntityInfo &Info) = 0;

  virtual bool handleAvailableAttribute(const AvailableAttrInfo &Info) = 0;

  virtual bool finishSourceEntity(UIdent Kind) = 0;

  virtual bool handleDiagnostics(ArrayRef<DiagnosticEntryInfo> Diags) = 0;
};

struct TypeContextInfoItem {
  StringRef TypeName;
  StringRef TypeUSR;

  struct Member {
    StringRef Name;
    StringRef Description;
    StringRef SourceText;
    StringRef DocBrief;
  };
  ArrayRef<Member> ImplicitMembers;
};

class TypeContextInfoConsumer {
  virtual void anchor();

public:
  virtual ~TypeContextInfoConsumer() {}

  virtual void handleResult(const TypeContextInfoItem &Result) = 0;
  virtual void failed(StringRef ErrDescription) = 0;
  virtual void cancelled() = 0;
  virtual void setReusingASTContext(bool flag) = 0;
};

struct ConformingMethodListResult {
  StringRef TypeName;
  StringRef TypeUSR;

  struct Member {
    StringRef Name;
    StringRef TypeName;
    StringRef TypeUSR;
    StringRef Description;
    StringRef SourceText;
    StringRef DocBrief;
  };
  ArrayRef<Member> Members;
};

class ConformingMethodListConsumer {
  virtual void anchor();

public:
  virtual ~ConformingMethodListConsumer() {}

  virtual void handleResult(const ConformingMethodListResult &Result) = 0;
  virtual void setReusingASTContext(bool flag) = 0;
  virtual void failed(StringRef ErrDescription) = 0;
  virtual void cancelled() = 0;
};

struct CompilationResult {
  unsigned int ResultStatus;
  llvm::ArrayRef<DiagnosticEntryInfo> Diagnostics;
};

class LangSupport {
  virtual void anchor();

public:
  /// A separator between parts in a synthesized usr.
  const static std::string SynthesizedUSRSeparator;

  virtual ~LangSupport() { }

  virtual void *getOpaqueSwiftIDEInspectionInstance() { return nullptr; }

  virtual void globalConfigurationUpdated(std::shared_ptr<GlobalConfig> Config) {};

  virtual void dependencyUpdated() {}

  virtual void demangleNames(
      ArrayRef<const char *> MangledNames, bool Simplified,
      std::function<void(const RequestResult<ArrayRef<std::string>> &)>
          Receiver) = 0;

  virtual void mangleSimpleClassNames(
      ArrayRef<std::pair<StringRef, StringRef>> ModuleClassPairs,
      std::function<void(const RequestResult<ArrayRef<std::string>> &)>
          Receiver) = 0;

  virtual void indexSource(StringRef Filename,
                           IndexingConsumer &Consumer,
                           ArrayRef<const char *> Args) = 0;

  virtual void indexToStore(StringRef InputFile,
                            ArrayRef<const char *> Args,
                            IndexStoreOptions Opts,
                            SourceKitCancellationToken CancellationToken,
                            IndexToStoreReceiver Receiver) = 0;

  virtual void codeComplete(llvm::MemoryBuffer *InputBuf, unsigned Offset,
                            OptionsDictionary *options,
                            CodeCompletionConsumer &Consumer,
                            ArrayRef<const char *> Args,
                            std::optional<VFSOptions> vfsOptions,
                            SourceKitCancellationToken CancellationToken) = 0;

  virtual void codeCompleteOpen(
      StringRef name, llvm::MemoryBuffer *inputBuf, unsigned offset,
      OptionsDictionary *options, ArrayRef<FilterRule> filterRules,
      GroupedCodeCompletionConsumer &consumer, ArrayRef<const char *> args,
      std::optional<VFSOptions> vfsOptions,
      SourceKitCancellationToken CancellationToken) = 0;

  virtual void codeCompleteClose(StringRef name, unsigned offset,
                                 GroupedCodeCompletionConsumer &consumer) = 0;

  virtual void codeCompleteUpdate(StringRef name, unsigned offset,
                                  OptionsDictionary *options,
                                  SourceKitCancellationToken CancellationToken,
                                  GroupedCodeCompletionConsumer &consumer) = 0;

  virtual void codeCompleteCacheOnDisk(StringRef path) = 0;

  virtual void
  codeCompleteSetPopularAPI(ArrayRef<const char *> popularAPI,
                            ArrayRef<const char *> unpopularAPI) = 0;

  virtual void
  codeCompleteSetCustom(ArrayRef<CustomCompletionInfo> completions) = 0;

  virtual void editorOpen(StringRef Name, llvm::MemoryBuffer *Buf,
                          EditorConsumer &Consumer, ArrayRef<const char *> Args,
                          std::optional<VFSOptions> vfsOptions) = 0;

  virtual void editorOpenInterface(EditorConsumer &Consumer, StringRef Name,
                                   StringRef ModuleName,
                                   std::optional<StringRef> Group,
                                   ArrayRef<const char *> Args,
                                   bool SynthesizedExtensions,
                                   std::optional<StringRef> InterestedUSR) = 0;

  virtual void editorOpenTypeInterface(EditorConsumer &Consumer,
                                       ArrayRef<const char *> Args,
                                       StringRef TypeUSR) = 0;

  virtual void editorOpenHeaderInterface(EditorConsumer &Consumer,
                                         StringRef Name,
                                         StringRef HeaderName,
                                         ArrayRef<const char *> Args,
                                         bool UsingSwiftArgs,
                                         bool SynthesizedExtensions,
                                         StringRef swiftVersion) = 0;

  virtual void
  editorOpenSwiftSourceInterface(StringRef Name, StringRef SourceName,
                                 ArrayRef<const char *> Args,
                                 SourceKitCancellationToken CancellationToken,
                                 std::shared_ptr<EditorConsumer> Consumer) = 0;

  virtual void editorClose(StringRef Name, bool CancelBuilds,
                           bool RemoveCache) = 0;

  virtual void editorReplaceText(StringRef Name, llvm::MemoryBuffer *Buf,
                                 unsigned Offset, unsigned Length,
                                 EditorConsumer &Consumer) = 0;

  virtual void editorApplyFormatOptions(StringRef Name,
                                        OptionsDictionary &FmtOptions) = 0;

  virtual void editorFormatText(StringRef Name, unsigned Line, unsigned Length,
                                EditorConsumer &Consumer) = 0;

  virtual void editorExtractTextFromComment(StringRef Source,
                                            EditorConsumer &Consumer) = 0;

  virtual void editorConvertMarkupToXML(StringRef Source,
                                        EditorConsumer &Consumer) = 0;

  virtual void editorExpandPlaceholder(StringRef Name, unsigned Offset,
                                       unsigned Length,
                                       EditorConsumer &Consumer) = 0;

  virtual void getCursorInfo(
      StringRef PrimaryFilePath, StringRef InputBufferName, unsigned Offset,
      unsigned Length, bool Actionables, bool SymbolGraph,
      bool CancelOnSubsequentRequest, ArrayRef<const char *> Args,
      std::optional<VFSOptions> vfsOptions,
      SourceKitCancellationToken CancellationToken,
      std::function<void(const RequestResult<CursorInfoData> &)> Receiver) = 0;

  virtual void
  getDiagnostics(StringRef PrimaryFilePath, ArrayRef<const char *> Args,
                 std::optional<VFSOptions> VfsOptions,
                 SourceKitCancellationToken CancellationToken,
                 std::function<void(const RequestResult<DiagnosticsResult> &)>
                     Receiver) = 0;

  virtual void getSemanticTokens(
      StringRef PrimaryFilePath, StringRef InputBufferName,
      ArrayRef<const char *> Args, std::optional<VFSOptions> VfsOptions,
      SourceKitCancellationToken CancellationToken,
      std::function<void(const RequestResult<SemanticTokensResult> &)>
          Receiver) = 0;

  virtual void
  getNameInfo(StringRef PrimaryFilePath, StringRef InputBufferName,
              unsigned Offset, NameTranslatingInfo &Input,
              ArrayRef<const char *> Args,
              SourceKitCancellationToken CancellationToken,
              std::function<void(const RequestResult<NameTranslatingInfo> &)>
                  Receiver) = 0;

  virtual void getRangeInfo(
      StringRef PrimaryFilePath, StringRef InputBufferName, unsigned Offset,
      unsigned Length, bool CancelOnSubsequentRequest,
      ArrayRef<const char *> Args, SourceKitCancellationToken CancellationToken,
      std::function<void(const RequestResult<RangeInfo> &)> Receiver) = 0;

  virtual void getCursorInfoFromUSR(
      StringRef PrimaryFilePath, StringRef InputBufferName, StringRef USR,
      bool CancelOnSubsequentRequest, ArrayRef<const char *> Args,
      std::optional<VFSOptions> vfsOptions,
      SourceKitCancellationToken CancellationToken,
      std::function<void(const RequestResult<CursorInfoData> &)> Receiver) = 0;

  /// - Parameters:
  ///   - IncludeNonEditableBaseNames: If `true` also return results if the
  ///     referenced declaration is an initializer or subscript. This is
  ///     intended if the related identifiers response is used for rename, which
  ///     allows renaming parameter labels of these declaration.
  ///     If the function's base name should be highlighted, this should be
  ///     `false` because e.g. highlighting a subscript with
  ///     `IncludeNonEditableBaseNames = true` would return the locations of all
  ///     `[` that call that subscript.
  virtual void findRelatedIdentifiersInFile(
      StringRef PrimaryFilePath, StringRef InputBufferName, unsigned Offset,
      bool IncludeNonEditableBaseNames, bool CancelOnSubsequentRequest,
      ArrayRef<const char *> Args, SourceKitCancellationToken CancellationToken,
      std::function<void(const RequestResult<RelatedIdentsResult> &)>
          Receiver) = 0;

  virtual void findActiveRegionsInFile(
      StringRef PrimaryFilePath, StringRef InputBufferName,
      ArrayRef<const char *> Args, SourceKitCancellationToken CancellationToken,
      std::function<void(const RequestResult<ActiveRegionsInfo> &)>
          Receiver) = 0;

  virtual std::optional<std::pair<unsigned, unsigned>>
  findUSRRange(StringRef DocumentName, StringRef USR) = 0;

  virtual void findInterfaceDocument(StringRef ModuleName,
                                     ArrayRef<const char *> Args,
                    std::function<void(const RequestResult<InterfaceDocInfo> &)> Receiver) = 0;

  virtual void findModuleGroups(StringRef ModuleName,
                                ArrayRef<const char *> Args,
                                std::function<void(const RequestResult<ArrayRef<StringRef>> &)> Receiver) = 0;

  virtual CancellableResult<std::vector<CategorizedRenameRanges>>
  findRenameRanges(llvm::MemoryBuffer *InputBuf,
                   ArrayRef<RenameLoc> RenameLocations,
                   ArrayRef<const char *> Args) = 0;
  virtual void
  findLocalRenameRanges(StringRef Filename, unsigned Line, unsigned Column,
                        unsigned Length, ArrayRef<const char *> Args,
                        SourceKitCancellationToken CancellationToken,
                        CategorizedRenameRangesReceiver Receiver) = 0;

  virtual void semanticRefactoring(StringRef PrimaryFilePath,
                                   SemanticRefactoringInfo Info,
                                   ArrayRef<const char *> Args,
                                   SourceKitCancellationToken CancellationToken,
                                   CategorizedEditsReceiver Receiver) = 0;

  virtual void collectExpressionTypes(
      StringRef PrimaryFilePath, StringRef InputBufferName,
      ArrayRef<const char *> Args, ArrayRef<const char *> ExpectedProtocols,
      bool FullyQualified, bool CanonicalType,
      SourceKitCancellationToken CancellationToken,
      std::function<void(const RequestResult<ExpressionTypesInFile> &)>
          Receiver) = 0;

  /// Collects variable types for a range defined by `Offset` and `Length` in
  /// the source file. If `Offset` or `Length` are empty, variable types for
  /// the entire document are collected.
  virtual void collectVariableTypes(
      StringRef PrimaryFilePath, StringRef InputBufferName,
      ArrayRef<const char *> Args, std::optional<unsigned> Offset,
      std::optional<unsigned> Length, bool FullyQualified,
      SourceKitCancellationToken CancellationToken,
      std::function<void(const RequestResult<VariableTypesInFile> &)>
          Receiver) = 0;

  virtual void getDocInfo(llvm::MemoryBuffer *InputBuf,
                          StringRef ModuleName,
                          ArrayRef<const char *> Args,
                          DocInfoConsumer &Consumer) = 0;

  virtual void getExpressionContextInfo(
      llvm::MemoryBuffer *inputBuf, unsigned Offset, OptionsDictionary *options,
      ArrayRef<const char *> Args, SourceKitCancellationToken CancellationToken,
      TypeContextInfoConsumer &Consumer,
      std::optional<VFSOptions> vfsOptions) = 0;

  virtual void getConformingMethodList(
      llvm::MemoryBuffer *inputBuf, unsigned Offset, OptionsDictionary *options,
      ArrayRef<const char *> Args, ArrayRef<const char *> ExpectedTypes,
      SourceKitCancellationToken CancellationToken,
      ConformingMethodListConsumer &Consumer,
      std::optional<VFSOptions> vfsOptions) = 0;

  virtual void expandMacroSyntactically(llvm::MemoryBuffer *inputBuf,
                                        ArrayRef<const char *> args,
                                        ArrayRef<MacroExpansionInfo> expansions,
                                        CategorizedEditsReceiver receiver) = 0;

  virtual void
  performCompile(StringRef Name, ArrayRef<const char *> Args,
                 std::optional<VFSOptions> vfsOptions,
                 SourceKitCancellationToken CancellationToken,
                 std::function<void(const RequestResult<CompilationResult> &)>
                     Receiver) = 0;

  virtual void closeCompile(StringRef Name) = 0;

  virtual void getStatistics(StatisticsReceiver) = 0;
};
} // namespace SourceKit

#endif
