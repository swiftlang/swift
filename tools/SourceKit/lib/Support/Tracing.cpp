//===--- Tracing.cpp ------------------------------------------------------===//
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

#include "SourceKit/Support/Tracing.h"

#include "swift/Frontend/Frontend.h"

#include "llvm/Support/YAMLTraits.h"

using namespace SourceKit;
using namespace llvm;



//===----------------------------------------------------------------------===//
// General
//===----------------------------------------------------------------------===//

static std::atomic<bool> tracing_enabled(false);
static std::atomic<uint64_t> operation_id(0);

//===----------------------------------------------------------------------===//
// Consumers
//===----------------------------------------------------------------------===//
struct TraceConsumerListNode {
  trace::TraceConsumer *const Consumer;
  TraceConsumerListNode *Next;
};
static std::atomic<TraceConsumerListNode *> consumers(nullptr);


//===----------------------------------------------------------------------===//
// Trace commands
//===----------------------------------------------------------------------===//

// Is tracing enabled
bool trace::enabled() {
  return tracing_enabled;
}

void trace::enable() {
  tracing_enabled = true;
}

void trace::disable() {
  tracing_enabled = false;
}

// Trace start of perform sema call, returns OpId
uint64_t trace::startOperation(trace::OperationKind OpKind,
                               const trace::SwiftInvocation &Inv,
                               const trace::StringPairs &OpArgs) {
  auto OpId = ++operation_id;
  if (trace::enabled()) {
    auto Node = consumers.load(std::memory_order_acquire);
    while (Node) {
      Node->Consumer->operationStarted(OpId, OpKind, Inv, OpArgs);
      Node = Node->Next;
    }
  }
  return OpId;
}

// Operation previously started with startXXX has finished
void trace::operationFinished(uint64_t OpId) {
  if (trace::enabled()) {
    auto Node = consumers.load(std::memory_order_acquire);
    while (Node) {
      Node->Consumer->operationFinished(OpId);
      Node = Node->Next;
    }
  }
}

// Register trace consumer
void trace::registerConsumer(trace::TraceConsumer *Consumer) {
  TraceConsumerListNode *Node = new TraceConsumerListNode {Consumer, nullptr};
  do {
    Node->Next = consumers.load(std::memory_order_relaxed);
  } while (!consumers.compare_exchange_weak(Node->Next, Node,
                                            std::memory_order_release,
                                            std::memory_order_relaxed));
}
