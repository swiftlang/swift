//===--- CodeCompletionResultSink.h ---------------------------------------===//
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

#ifndef SWIFT_IDE_CODECOMPLETIONRESULTSINK
#define SWIFT_IDE_CODECOMPLETIONRESULTSINK

#include "swift/IDE/CodeCompletionResult.h"

namespace swift {
namespace ide {

struct CodeCompletionResultSink {
  using AllocatorPtr = std::shared_ptr<llvm::BumpPtrAllocator>;

  /// The allocator used to allocate results "native" to this sink.
  AllocatorPtr Allocator;

  /// Allocators that keep alive "foreign" results imported into this sink from
  /// other sinks.
  std::vector<AllocatorPtr> ForeignAllocators;

  /// Whether to annotate the results with XML.
  bool annotateResult = false;

  /// Whether to emit object literals if desired.
  bool includeObjectLiterals = true;

  /// Whether to emit type initializers in addition to type names in expression
  /// position.
  bool addInitsToTopLevel = false;

  /// Whether to include an item without any default arguments.
  bool addCallWithNoDefaultArgs = true;

private:
  /// Whether the code completion results computed for this sink are intended to
  /// only be stored in the cache. In this case no contextual information is
  /// computed and all types in \c ContextFreeCodeCompletionResult should be
  /// USR-based instead of AST-based.
  USRBasedTypeArena *USRTypeArena = nullptr;

public:
  std::vector<CodeCompletionResult *> Results;

  /// A single-element cache for module names stored in Allocator, keyed by a
  /// clang::Module * or swift::ModuleDecl *.
  std::pair<void *, NullTerminatedStringRef> LastModule;

  CodeCompletionResultSink()
      : Allocator(std::make_shared<llvm::BumpPtrAllocator>()) {}

  llvm::BumpPtrAllocator &getAllocator() { return *Allocator; }

  /// Marks the sink as producing results for the code completion cache.
  /// In this case the produced results will not contain any contextual
  /// information and all types in the \c ContextFreeCodeCompletionResult are
  /// USR-based.
  void setProduceContextFreeResults(USRBasedTypeArena &USRTypeArena) {
    this->USRTypeArena = &USRTypeArena;
  }

  /// See \c setProduceContextFreeResults.
  bool shouldProduceContextFreeResults() const {
    return USRTypeArena != nullptr;
  }

  /// If \c shouldProduceContextFreeResults is \c true, returns the arena in
  /// which the USR-based types of the \c ContextFreeCodeCompletionResult should
  /// be stored.
  USRBasedTypeArena &getUSRTypeArena() const {
    assert(USRTypeArena != nullptr &&
           "Must only be called if shouldProduceContextFreeResults is true");
    return *USRTypeArena;
  }
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONRESULTSINK
