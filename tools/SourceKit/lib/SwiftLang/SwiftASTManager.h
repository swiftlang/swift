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
#include "llvm/ADT/StringRef.h"
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
  typedef RefPtr<SwiftInvocation> SwiftInvocationRef;
  class EditorDiagConsumer;

class ASTUnit : public SourceKit::ThreadSafeRefCountedBase<ASTUnit> {
public:
  struct Implementation;
  Implementation &Impl;

  explicit ASTUnit(uint64_t Generation);
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

class SwiftASTManager {
public:
  explicit SwiftASTManager(SwiftLangSupport &LangSupport);
  ~SwiftASTManager();

  SwiftInvocationRef getInvocation(ArrayRef<const char *> Args,
                                   StringRef PrimaryFile,
                                   std::string &Error);

  /// Provides the AST associated with an invocation to the AST consumer,
  /// asynchronously.
  /// \param OncePerASTToken if non-null, a previous query with the same value
  /// token, that is enqueued waiting to be executed on the same AST, will be
  /// cancelled.
  void processASTAsync(SwiftInvocationRef Invok,
                       SwiftASTConsumerRef ASTConsumer,
                       const void *OncePerASTToken,
                       ArrayRef<ImmutableTextSnapshotRef> Snapshots =
                           ArrayRef<ImmutableTextSnapshotRef>());

  std::unique_ptr<llvm::MemoryBuffer> getMemoryBuffer(StringRef Filename,
                                                      std::string &Error);

  bool initCompilerInvocation(swift::CompilerInvocation &Invocation,
                              ArrayRef<const char *> Args,
                              swift::DiagnosticEngine &Diags,
                              StringRef PrimaryFile,
                              std::string &Error);

  bool initCompilerInvocation(swift::CompilerInvocation &CompInvok,
                              ArrayRef<const char *> OrigArgs,
                              StringRef PrimaryFile,
                              std::string &Error);

  void removeCachedAST(SwiftInvocationRef Invok);

  struct Implementation;

private:
  Implementation &Impl;
};

} // namespace SourceKit

#endif
