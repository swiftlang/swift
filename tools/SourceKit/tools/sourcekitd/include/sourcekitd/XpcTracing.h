//===- XpcTracing.h - XPC-side Tracing Interface ----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
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
