//===--- Tracing.h - Tracing Interface --------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_SUPPORT_TRACING_H
#define LLVM_SOURCEKIT_SUPPORT_TRACING_H

#include "SourceKit/Support/UIdent.h"
#include "llvm/ADT/Optional.h"

#include <vector>

namespace SourceKit {
namespace trace {

struct SwiftArguments {
  std::string PrimaryFile;
  std::vector<std::string> Args;
};

enum class OperationKind : uint64_t {
  SimpleParse,
  PerformSema,
  AnnotAndDiag,

  ReadSyntaxInfo,
  ReadDiagnostics,
  ReadSemanticInfo,

  IndexModule,
  IndexSource,

  CursorInfoForIFaceGen,
  CursorInfoForSource,

  ExpandPlaceholder,
  FormatText,
  RelatedIdents,
  CodeCompletion,
  OpenInterface,
  OpenHeaderInterface,

  CodeCompletionInit,
};
  
typedef std::vector<std::pair<std::string, std::string>> StringPairs;

struct SwiftInvocation {
  SwiftArguments Args;
  StringPairs Files;

  void addFile(std::string FileName, std::string Text) {
    Files.push_back(std::make_pair(std::move(FileName), std::move(Text)));
  }
};
  
class TraceConsumer {
public:
  virtual ~TraceConsumer() = default;

  // Trace start of SourceKit operation
  virtual void operationStarted(uint64_t OpId, OperationKind OpKind,
                                 const SwiftInvocation &Inv,
                                 const StringPairs &OpArgs) = 0;
  
  // Operation previously started with startXXX has finished
  virtual void operationFinished(uint64_t OpId) = 0;
};

// Is tracing enabled
bool enabled();

// Enable tracing
void enable();

// Disable tracing
void disable();
  
// Trace start of SourceKit operation, returns OpId
uint64_t startOperation(OperationKind OpKind,
                        const SwiftInvocation &Inv,
                        const StringPairs &OpArgs = StringPairs());

// Operation previously started with startXXX has finished
void operationFinished(uint64_t OpId);

// Register trace consumer.
void registerConsumer(TraceConsumer *Consumer);

// Class that utilizes the RAII idiom for the operations being traced
class TracedOperation final {
  llvm::Optional<uint64_t> OpId;

public:
  TracedOperation() {}
  ~TracedOperation() {
    finish();
  }

  TracedOperation(TracedOperation &&) = delete;
  TracedOperation &operator=(TracedOperation &&) = delete;
  TracedOperation(const TracedOperation &) = delete;
  TracedOperation &operator=(const TracedOperation &) = delete;

  void start(OperationKind OpKind,
             const SwiftInvocation &Inv,
             const StringPairs &OpArgs = StringPairs()) {
    finish();
    OpId = startOperation(OpKind, Inv, OpArgs);
  }

  void finish() {
    if (OpId.hasValue()) {
      operationFinished(OpId.getValue());
      OpId.reset();
    }
  }

};

} // namespace sourcekitd
} // namespace trace

#endif /* defined(LLVM_SOURCEKIT_SUPPORT_TRACING_H) */
