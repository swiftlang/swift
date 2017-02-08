//===--- SwiftLangSupport.h - -----------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTLANGSUPPORT_H
#define LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTLANGSUPPORT_H

#include "CodeCompletion.h"
#include "SwiftInterfaceGenContext.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Support/Concurrency.h"
#include "SourceKit/Support/ThreadSafeRefCntPtr.h"
#include "SourceKit/Support/Tracing.h"
#include "swift/Basic/ThreadSafeRefCounted.h"
#include "swift/IDE/Formatting.h"
#include "swift/Index/IndexSymbol.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Mutex.h"
#include <map>
#include <string>

namespace swift {
  class ASTContext;
  class ClangModuleLoader;
  class CompilerInstance;
  class CompilerInvocation;
  class Decl;
  class Type;
  class AbstractStorageDecl;
  class SourceFile;
  class ValueDecl;
  enum class AccessorKind;

namespace ide {
  class CodeCompletionCache;
  class OnDiskCodeCompletionCache;
  enum class CodeCompletionDeclKind;
  enum class SyntaxNodeKind : uint8_t;
  enum class SyntaxStructureKind : uint8_t;
  enum class SyntaxStructureElementKind : uint8_t;
  enum class RangeKind : int8_t;
  class CodeCompletionConsumer;
}
}

namespace SourceKit {
  class ImmutableTextSnapshot;
  typedef RefPtr<ImmutableTextSnapshot> ImmutableTextSnapshotRef;
  class SwiftASTManager;
  class SwiftLangSupport;
  class Context;

class SwiftEditorDocument :
    public ThreadSafeRefCountedBase<SwiftEditorDocument> {

  struct Implementation;
  Implementation &Impl;

public:

  SwiftEditorDocument(StringRef FilePath, SwiftLangSupport &LangSupport,
       swift::ide::CodeFormatOptions Options = swift::ide::CodeFormatOptions());
  ~SwiftEditorDocument();

  ImmutableTextSnapshotRef initializeText(llvm::MemoryBuffer *Buf,
                                          ArrayRef<const char *> Args);
  ImmutableTextSnapshotRef replaceText(unsigned Offset, unsigned Length,
                                       llvm::MemoryBuffer *Buf,
                                       bool ProvideSemanticInfo);

  void updateSemaInfo();

  void removeCachedAST();

  ImmutableTextSnapshotRef getLatestSnapshot() const;

  void parse(ImmutableTextSnapshotRef Snapshot, SwiftLangSupport &Lang);
  void readSyntaxInfo(EditorConsumer& consumer);
  void readSemanticInfo(ImmutableTextSnapshotRef Snapshot,
                        EditorConsumer& Consumer);

  void applyFormatOptions(OptionsDictionary &FmtOptions);
  void formatText(unsigned Line, unsigned Length, EditorConsumer &Consumer);
  void expandPlaceholder(unsigned Offset, unsigned Length,
                         EditorConsumer &Consumer);
  const swift::ide::CodeFormatOptions &getFormatOptions();

  static void reportDocumentStructure(swift::SourceFile &SrcFile,
                                      EditorConsumer &Consumer);
};

typedef IntrusiveRefCntPtr<SwiftEditorDocument> SwiftEditorDocumentRef;

class SwiftEditorDocumentFileMap {
  WorkQueue Queue{ WorkQueue::Dequeuing::Concurrent,
                   "sourcekit.swift.EditorDocFileMap" };
  struct DocInfo {
    SwiftEditorDocumentRef DocRef;
    std::string ResolvedPath;
  };
  llvm::StringMap<DocInfo> Docs;

public:
  bool getOrUpdate(StringRef FilePath,
                   SwiftLangSupport &LangSupport,
                   SwiftEditorDocumentRef &EditorDoc);
  /// Looks up the document only by the path name that was given initially.
  SwiftEditorDocumentRef getByUnresolvedName(StringRef FilePath);
  /// Looks up the document by resolving symlinks in the paths.
  SwiftEditorDocumentRef findByPath(StringRef FilePath);
  SwiftEditorDocumentRef remove(StringRef FilePath);
};

namespace CodeCompletion {

/// Provides a thread-safe cache for code completion results that remain valid
/// for the duration of a 'session' - for example, from the point that a user
/// invokes code completion until they accept a completion, or otherwise close
/// the list of completions.
///
/// The contents of the cache can be modified asynchronously during the session,
/// but the contained objects are immutable.
class SessionCache : public ThreadSafeRefCountedBase<SessionCache> {
  std::unique_ptr<llvm::MemoryBuffer> buffer;
  std::vector<std::string> args;
  CompletionSink sink;
  std::vector<Completion *> sortedCompletions;
  CompletionKind completionKind;
  bool completionHasExpectedTypes;
  FilterRules filterRules;
  llvm::sys::Mutex mtx;

public:
  SessionCache(CompletionSink &&sink,
               std::unique_ptr<llvm::MemoryBuffer> &&buffer,
               std::vector<std::string> &&args, CompletionKind completionKind,
               bool hasExpectedTypes, FilterRules filterRules)
      : buffer(std::move(buffer)), args(std::move(args)), sink(std::move(sink)),
        completionKind(completionKind),
        completionHasExpectedTypes(hasExpectedTypes),
        filterRules(std::move(filterRules)) {}
  void setSortedCompletions(std::vector<Completion *> &&completions);
  ArrayRef<Completion *> getSortedCompletions();
  llvm::MemoryBuffer *getBuffer();
  ArrayRef<std::string> getCompilerArgs();
  const FilterRules &getFilterRules();
  CompletionKind getCompletionKind();
  bool getCompletionHasExpectedTypes();
};
typedef RefPtr<SessionCache> SessionCacheRef;

/// A thread-safe map from (buffer, code complete offset) to \c SessionCache.
class SessionCacheMap {
  mutable unsigned nextBufferID = 0;
  mutable llvm::StringMap<unsigned> nameToBufferMap;
  typedef std::pair<unsigned, unsigned> Key;
  llvm::DenseMap<Key, SessionCacheRef> sessions;
  mutable llvm::sys::Mutex mtx;

  // Should only be called with Mtx locked.
  unsigned getBufferID(StringRef name) const;

public:
  SessionCacheRef get(StringRef name, unsigned offset) const;
  bool set(StringRef name, unsigned offset, SessionCacheRef session);
  bool remove(StringRef name, unsigned offset);
};
} // end namespace CodeCompletion

class SwiftInterfaceGenMap {
  llvm::StringMap<SwiftInterfaceGenContextRef> IFaceGens;
  mutable llvm::sys::Mutex Mtx;

public:
  SwiftInterfaceGenContextRef get(StringRef Name) const;
  void set(StringRef Name, SwiftInterfaceGenContextRef IFaceGen);
  bool remove(StringRef Name);
  SwiftInterfaceGenContextRef find(StringRef ModuleName,
                                   const swift::CompilerInvocation &Invok);
};

struct SwiftCompletionCache
    : public ThreadSafeRefCountedBase<SwiftCompletionCache> {
  std::unique_ptr<swift::ide::CodeCompletionCache> inMemory;
  std::unique_ptr<swift::ide::OnDiskCodeCompletionCache> onDisk;
  swift::ide::CodeCompletionCache &getCache();
  SwiftCompletionCache() = default;
  ~SwiftCompletionCache();
};

struct SwiftPopularAPI : public ThreadSafeRefCountedBase<SwiftPopularAPI> {
  llvm::StringMap<CodeCompletion::PopularityFactor> nameToFactor;
};

struct SwiftCustomCompletions
    : public ThreadSafeRefCountedBase<SwiftCustomCompletions> {
  std::vector<CustomCompletionInfo> customCompletions;
};

class SwiftLangSupport : public LangSupport {
  SourceKit::Context &SKCtx;
  std::string RuntimeResourcePath;
  std::unique_ptr<SwiftASTManager> ASTMgr;
  SwiftEditorDocumentFileMap EditorDocuments;
  SwiftInterfaceGenMap IFaceGenContexts;
  ThreadSafeRefCntPtr<SwiftCompletionCache> CCCache;
  ThreadSafeRefCntPtr<SwiftPopularAPI> PopularAPI;
  CodeCompletion::SessionCacheMap CCSessions;
  ThreadSafeRefCntPtr<SwiftCustomCompletions> CustomCompletions;

public:
  explicit SwiftLangSupport(SourceKit::Context &SKCtx);
  ~SwiftLangSupport();

  SourceKit::Context &getContext() { return SKCtx; }

  StringRef getRuntimeResourcePath() const { return RuntimeResourcePath; }

  SwiftASTManager &getASTManager() { return *ASTMgr; }

  SwiftEditorDocumentFileMap &getEditorDocuments() { return EditorDocuments; }
  SwiftInterfaceGenMap &getIFaceGenContexts() { return IFaceGenContexts; }
  IntrusiveRefCntPtr<SwiftCompletionCache> getCodeCompletionCache() {
    return CCCache;
  }

  static SourceKit::UIdent getUIDForDecl(const swift::Decl *D,
                                         bool IsRef = false);
  static SourceKit::UIdent getUIDForExtensionOfDecl(const swift::Decl *D);
  static SourceKit::UIdent getUIDForLocalVar(bool IsRef = false);
  static SourceKit::UIdent getUIDForCodeCompletionDeclKind(
      swift::ide::CodeCompletionDeclKind Kind, bool IsRef = false);
  static SourceKit::UIdent getUIDForAccessor(const swift::ValueDecl *D,
                                             swift::AccessorKind AccKind,
                                             bool IsRef = false);
  static SourceKit::UIdent getUIDForModuleRef();
  static SourceKit::UIdent getUIDForSyntaxNodeKind(
      swift::ide::SyntaxNodeKind Kind);
  static SourceKit::UIdent getUIDForSyntaxStructureKind(
      swift::ide::SyntaxStructureKind Kind);
  static SourceKit::UIdent getUIDForSyntaxStructureElementKind(
      swift::ide::SyntaxStructureElementKind Kind);

  static SourceKit::UIdent getUIDForSymbol(swift::index::SymbolInfo sym,
                                           bool isRef);

  static SourceKit::UIdent getUIDForRangeKind(swift::ide::RangeKind Kind);

  static std::vector<UIdent> UIDsFromDeclAttributes(const swift::DeclAttributes &Attrs);


  static bool printDisplayName(const swift::ValueDecl *D, llvm::raw_ostream &OS);

  /// Generate a USR for a Decl, including the prefix.
  /// \returns true if the results should be ignored, false otherwise.
  static bool printUSR(const swift::ValueDecl *D, llvm::raw_ostream &OS);

  /// Generate a USR for the Type of a given decl.
  /// \returns true if the results should be ignored, false otherwise.
  static bool printDeclTypeUSR(const swift::ValueDecl *D, llvm::raw_ostream &OS);

  /// Generate a USR for of a given type.
  /// \returns true if the results should be ignored, false otherwise.
  static bool printTypeUSR(swift::Type Ty, llvm::raw_ostream &OS);

  /// Generate a USR for an accessor, including the prefix.
  /// \returns true if the results should be ignored, false otherwise.
  static bool printAccessorUSR(const swift::AbstractStorageDecl *D,
                               swift::AccessorKind AccKind,
                               llvm::raw_ostream &OS);

  /// Annotates a declaration with XML tags that describe the key substructure
  /// of the declaration for CursorInfo/DocInfo.
  ///
  /// Prints declarations with decl- and type-specific tags derived from the
  /// UIDs used for decl/refs.
  ///
  /// FIXME: This move to libIDE, but currently depends on the UIdentVisitor.
  static void printFullyAnnotatedDeclaration(const swift::ValueDecl *VD,
                                             swift::Type BaseTy,
                                             llvm::raw_ostream &OS);

  static void printFullyAnnotatedSynthesizedDeclaration(
                                            const swift::ValueDecl *VD,
                                            swift::NominalTypeDecl *Target,
                                            llvm::raw_ostream &OS);

  /// Tries to resolve the path to the real file-system path. If it fails it
  /// returns the original path;
  static std::string resolvePathSymlinks(StringRef FilePath);

  //==========================================================================//
  // LangSupport Interface
  //==========================================================================//

  void indexSource(StringRef Filename, IndexingConsumer &Consumer,
                   ArrayRef<const char *> Args, StringRef Hash) override;

  void codeComplete(llvm::MemoryBuffer *InputBuf, unsigned Offset,
                    SourceKit::CodeCompletionConsumer &Consumer,
                    ArrayRef<const char *> Args) override;

  void codeCompleteOpen(StringRef name, llvm::MemoryBuffer *inputBuf,
                        unsigned offset, OptionsDictionary *options,
                        ArrayRef<FilterRule> rawFilterRules,
                        GroupedCodeCompletionConsumer &consumer,
                        ArrayRef<const char *> args) override;

  void codeCompleteClose(StringRef name, unsigned offset,
                         GroupedCodeCompletionConsumer &consumer) override;

  void codeCompleteUpdate(StringRef name, unsigned offset,
                          OptionsDictionary *options,
                          GroupedCodeCompletionConsumer &consumer) override;

  void codeCompleteCacheOnDisk(StringRef path) override;

  void codeCompleteSetPopularAPI(ArrayRef<const char *> popularAPI,
                                 ArrayRef<const char *> unpopularAPI) override;

  void
  codeCompleteSetCustom(ArrayRef<CustomCompletionInfo> completions) override;

  void editorOpen(StringRef Name, llvm::MemoryBuffer *Buf, bool EnableSyntaxMap,
                  EditorConsumer &Consumer,
                  ArrayRef<const char *> Args) override;

  void editorOpenInterface(EditorConsumer &Consumer,
                           StringRef Name,
                           StringRef ModuleName,
                           Optional<StringRef> Group,
                           ArrayRef<const char *> Args,
                           bool SynthesizedExtensions,
                           Optional<StringRef> InterestedUSR) override;

  void editorOpenTypeInterface(EditorConsumer &Consumer,
                               ArrayRef<const char *> Args,
                               StringRef TypeUSR) override;

  void editorOpenHeaderInterface(EditorConsumer &Consumer,
                                 StringRef Name,
                                 StringRef HeaderName,
                                 ArrayRef<const char *> Args,
                                 bool SynthesizedExtensions) override;

  void editorOpenSwiftSourceInterface(StringRef Name,
                                      StringRef SourceName,
                                      ArrayRef<const char *> Args,
                                      std::shared_ptr<EditorConsumer> Consumer) override;

  void editorClose(StringRef Name, bool RemoveCache) override;

  void editorReplaceText(StringRef Name, llvm::MemoryBuffer *Buf,
                         unsigned Offset, unsigned Length,
                         EditorConsumer &Consumer) override;

  void editorApplyFormatOptions(StringRef Name,
                                OptionsDictionary &FmtOptions) override;

  void editorFormatText(StringRef Name, unsigned Line, unsigned Length,
                        EditorConsumer &Consumer) override;

  void editorExtractTextFromComment(StringRef Source,
                                    EditorConsumer &Consumer) override;

  void editorExpandPlaceholder(StringRef Name, unsigned Offset, unsigned Length,
                               EditorConsumer &Consumer) override;

  void getCursorInfo(StringRef Filename, unsigned Offset,
                     unsigned Length, bool Actionables,
                     ArrayRef<const char *> Args,
                     std::function<void(const CursorInfo &)> Receiver) override;

  void getNameInfo(StringRef Filename, unsigned Offset,
                   NameTranslatingInfo Input,
                   ArrayRef<const char *> Args,
                   std::function<void(const NameTranslatingInfo &)> Receiver) override;

  void getRangeInfo(StringRef Filename, unsigned Offset, unsigned Length,
                    ArrayRef<const char *> Args,
                    std::function<void(const RangeInfo&)> Receiver) override;

  void getCursorInfoFromUSR(
      StringRef Filename, StringRef USR, ArrayRef<const char *> Args,
      std::function<void(const CursorInfo &)> Receiver) override;

  void findRelatedIdentifiersInFile(StringRef Filename, unsigned Offset,
                                    ArrayRef<const char *> Args,
              std::function<void(const RelatedIdentsInfo &)> Receiver) override;

  void getDocInfo(llvm::MemoryBuffer *InputBuf,
                  StringRef ModuleName,
                  ArrayRef<const char *> Args,
                  DocInfoConsumer &Consumer) override;

  llvm::Optional<std::pair<unsigned, unsigned>>
      findUSRRange(StringRef DocumentName, StringRef USR) override;

  void findInterfaceDocument(StringRef ModuleName, ArrayRef<const char *> Args,
               std::function<void(const InterfaceDocInfo &)> Receiver) override;

  void findModuleGroups(StringRef ModuleName, ArrayRef<const char *> Args,
               std::function<void(ArrayRef<StringRef>, StringRef Error)> Receiver) override;
};

namespace trace {
  void initTraceInfo(trace::SwiftInvocation &SwiftArgs,
                     StringRef InputFile,
                     ArrayRef<const char *> Args);

  void initTraceFiles(trace::SwiftInvocation &SwiftArgs,
                      swift::CompilerInstance &CI);
}

/// When we cannot build any more clang modules, close the .pcm / files to
/// prevent fd leaks in clients that cache the AST.
// FIXME: Remove this once rdar://problem/19720334 is complete.
class CloseClangModuleFiles {
  swift::ClangModuleLoader &loader;

public:
  CloseClangModuleFiles(swift::ClangModuleLoader &loader) : loader(loader) {}
  ~CloseClangModuleFiles();
};

} // namespace SourceKit

#endif
