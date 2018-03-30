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

#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"

#include <vector>

namespace SourceKit {
  struct DiagnosticEntryInfo;

namespace trace {

struct SwiftArguments {
  std::string PrimaryFile;
  std::vector<std::string> Args;
};

enum class OperationKind : uint64_t {
  SimpleParse = 1 << 0,
  PerformSema = 1 << 1,
  AnnotAndDiag = 1 << 2,

  ReadSyntaxInfo = 1 << 3,
  ReadDiagnostics = 1 << 4,
  ReadSemanticInfo = 1 << 5,

  IndexModule = 1 << 6,
  IndexSource = 1 << 7,

  CursorInfoForIFaceGen = 1 << 8,
  CursorInfoForSource = 1 << 9,

  ExpandPlaceholder = 1 << 10,
  FormatText = 1 << 11,
  RelatedIdents = 1 << 12,
  CodeCompletion = 1 << 13,
  OpenInterface = 1 << 14,
  OpenHeaderInterface = 1 << 15,

  CodeCompletionInit = 1 << 16,

  Last = CodeCompletionInit,
  All = (Last << 1) - 1
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
  virtual void operationFinished(uint64_t OpId, OperationKind OpKind,
                                 ArrayRef<DiagnosticEntryInfo> Diagnostics) = 0;

  /// Returns the set of operations this consumer is interested in.
  ///
  /// Note: this is only a hint. Implementations should check the operation kind
  /// if they need to.
  virtual swift::OptionSet<OperationKind> desiredOperations() {
    return OperationKind::All;
  }
};

// Is tracing enabled
bool anyEnabled();

// Is tracing enabled for \p op.
bool enabled(OperationKind op);

// Trace start of SourceKit operation, returns OpId
uint64_t startOperation(OperationKind OpKind,
                        const SwiftInvocation &Inv,
                        const StringPairs &OpArgs = StringPairs());

// Operation previously started with startXXX has finished
void operationFinished(uint64_t OpId, OperationKind OpKind,
                       ArrayRef<DiagnosticEntryInfo> Diagnostics);

// Register trace consumer.
void registerConsumer(TraceConsumer *Consumer);

// Register trace consumer.
void unregisterConsumer(TraceConsumer *Consumer);

// Class that utilizes the RAII idiom for the operations being traced
class TracedOperation final {
  OperationKind OpKind;
  llvm::Optional<uint64_t> OpId;
  bool Enabled;

public:
  TracedOperation(OperationKind OpKind) : OpKind(OpKind) {
    Enabled = trace::enabled(OpKind);
  }
  ~TracedOperation() {
    finish();
  }

  TracedOperation(TracedOperation &&) = delete;
  TracedOperation &operator=(TracedOperation &&) = delete;
  TracedOperation(const TracedOperation &) = delete;
  TracedOperation &operator=(const TracedOperation &) = delete;

  bool enabled() const { return Enabled; }

  void start(const SwiftInvocation &Inv,
             const StringPairs &OpArgs = StringPairs()) {
    assert(!OpId.hasValue());
    OpId = startOperation(OpKind, Inv, OpArgs);
  }

  void finish(ArrayRef<DiagnosticEntryInfo> Diagnostics = llvm::None) {
    if (OpId.hasValue()) {
      operationFinished(OpId.getValue(), OpKind, Diagnostics);
      OpId.reset();
    }
  }

};

} // namespace sourcekitd
} // namespace trace

#endif /* defined(LLVM_SOURCEKIT_SUPPORT_TRACING_H) */
