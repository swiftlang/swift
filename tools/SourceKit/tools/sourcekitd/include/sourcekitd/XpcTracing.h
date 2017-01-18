//===--- XpcTracing.h - XPC-side Tracing Interface --------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_SUPPORT_XPC_TRACING_H
#define LLVM_SOURCEKIT_SUPPORT_XPC_TRACING_H

#include "SourceKit/Support/Tracing.h"

namespace sourcekitd {
namespace trace {

enum class ActionKind : uint64_t {
  OperationFinished,
  OperationStarted,
};

typedef void *sourcekitd_trace_message_t;

typedef SourceKit::trace::OperationKind OperationKind;
typedef SourceKit::trace::SwiftArguments SwiftArguments;
typedef SourceKit::trace::StringPairs StringPairs;
typedef SourceKit::trace::SwiftInvocation SwiftInvocation;

void sendTraceMessage(sourcekitd_trace_message_t Msg);

uint64_t getTracingSession();

void initialize();

} // namespace sourcekitd
} // namespace trace

#endif /* defined(LLVM_SOURCEKIT_SUPPORT_XPC_TRACING_H) */
