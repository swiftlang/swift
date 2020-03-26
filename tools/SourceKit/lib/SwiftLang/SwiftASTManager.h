//===--- SwiftASTManager.h - ------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTASTMANAGER_H
#define LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTASTMANAGER_H

#include "SwiftInvocation.h"
#include "SourceKit/Core/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/VirtualFileSystem.h"
#include <functional>
#include <string>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {
  class CompilerInstance;
  class CompilerInvocation;
  class DiagnosticEngine;
  class SourceFile;
  class SourceManager;
}

namespace SourceKit {
  class Context;
  struct DiagnosticEntryInfo;
  class ImmutableTextSnapshot;
  typedef RefPtr<ImmutableTextSnapshot> ImmutableTextSnapshotRef;
  class SwiftEditorDocumentFileMap;
  class SwiftLangSupport;
  class SwiftInvocation;
  struct SwiftStatistics;
  class GlobalConfig;
  typedef RefPtr<SwiftInvocation> SwiftInvocationRef;
  class EditorDiagConsumer;

class ASTUnit : public SourceKit::ThreadSafeRefCountedBase<ASTUnit> {
public:
  struct Implementation;
  Implementation &Impl;

  explicit ASTUnit(uint64_t Generation, std::shared_ptr<SwiftStatistics> Stats);
  ~ASTUnit();

  swift::CompilerInstance &getCompilerInstance() const;
  uint64_t getGeneration() const;
  ArrayRef<ImmutableTextSnapshotRef> getSnapshots() const;
  EditorDiagConsumer &getEditorDiagConsumer() const;
  swift::SourceFile &getPrimarySourceFile() const;

  /// Perform \p Fn asynchronously while preventing concurrent access to the
  /// AST.
  void performAsync(std::function<void()> Fn);
};

typedef IntrusiveRefCntPtr<ASTUnit> ASTUnitRef;

class SwiftASTConsumer {
public:
  virtual ~SwiftASTConsumer() { }
  virtual void cancelled() {}
  /// If there is an existing AST, this is called before trying to update it.
  /// Consumers may choose to still accept it even though it may have stale parts.
  ///
  /// \param Snapshots that were used to create the AST.
  /// \returns true if the existing AST is acceptable to be used.
  virtual bool canUseASTWithSnapshots(
      ArrayRef<ImmutableTextSnapshotRef> Snapshots) {
    return false;
  }
  virtual void failed(StringRef Error);
  virtual void handlePrimaryAST(ASTUnitRef AstUnit) = 0;
};

typedef std::shared_ptr<SwiftASTConsumer> SwiftASTConsumerRef;

class SwiftASTManager : public std::enable_shared_from_this<SwiftASTManager> {
public:
  explicit SwiftASTManager(std::shared_ptr<SwiftEditorDocumentFileMap>,
                           std::shared_ptr<GlobalConfig> Config,
                           std::shared_ptr<SwiftStatistics> Stats,
                           StringRef RuntimeResourcePath);
  ~SwiftASTManager();

  SwiftInvocationRef getInvocation(
      ArrayRef<const char *> Args, StringRef PrimaryFile, std::string &Error);

  /// Same as the previous `getInvocation`, but allows the caller to specify a
  /// custom `FileSystem` to be used throughout the invocation.
  SwiftInvocationRef getInvocation(
      ArrayRef<const char *> Args, StringRef PrimaryFile,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      std::string &Error);

  /// Provides the AST associated with an invocation to the AST consumer,
  /// asynchronously.
  /// \param OncePerASTToken if non-null, a previous query with the same value
  /// token, that is enqueued waiting to be executed on the same AST, will be
  /// cancelled.
  void
  processASTAsync(SwiftInvocationRef Invok, SwiftASTConsumerRef ASTConsumer,
                  const void *OncePerASTToken,
                  llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fileSystem,
                  ArrayRef<ImmutableTextSnapshotRef> Snapshots =
                      ArrayRef<ImmutableTextSnapshotRef>());

  std::unique_ptr<llvm::MemoryBuffer> getMemoryBuffer(StringRef Filename,
                                                      std::string &Error);

  bool initCompilerInvocation(
      swift::CompilerInvocation &Invocation, ArrayRef<const char *> Args,
      swift::DiagnosticEngine &Diags, StringRef PrimaryFile, std::string &Error);

  /// Same as the previous `initCompilerInvocation`, but allows the caller to
  /// specify a custom `FileSystem` to be used throughout the invocation.
  bool initCompilerInvocation(
      swift::CompilerInvocation &Invocation, ArrayRef<const char *> Args,
      swift::DiagnosticEngine &Diags, StringRef PrimaryFile,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      std::string &Error);

  bool initCompilerInvocation(swift::CompilerInvocation &CompInvok,
                              ArrayRef<const char *> OrigArgs,
                              StringRef PrimaryFile,
                              std::string &Error);

  /// Initializes \p Invocation as if for typechecking, but with no inputs.
  ///
  /// If \p AllowInputs is false, it is an error for \p OrigArgs to contain any
  /// input files.
  bool initCompilerInvocationNoInputs(swift::CompilerInvocation &Invocation,
                                      ArrayRef<const char *> OrigArgs,
                                      swift::DiagnosticEngine &Diags,
                                      std::string &Error,
                                      bool AllowInputs = true);

  void removeCachedAST(SwiftInvocationRef Invok);

  struct Implementation;
  Implementation &Impl;
};

} // namespace SourceKit

#endif
