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

#include "llvm/Support/Mutex.h"
#include "llvm/Support/YAMLTraits.h"

using namespace SourceKit;
using namespace llvm;
using swift::OptionSet;

static llvm::sys::Mutex consumersLock;
// Must hold consumersLock to access.
static std::vector<trace::TraceConsumer *> consumers;
// Must hold consumersLock to modify, but can be read any time.
static std::atomic<uint64_t> tracedOperations;

//===----------------------------------------------------------------------===//
// Trace commands
//===----------------------------------------------------------------------===//

// Is tracing enabled
bool trace::anyEnabled() { return static_cast<bool>(tracedOperations); }

bool trace::enabled(OperationKind op) {
  return OptionSet<OperationKind>(tracedOperations).contains(op);
}

// Trace start of perform sema call, returns OpId
uint64_t trace::startOperation(trace::OperationKind OpKind,
                               const trace::SwiftInvocation &Inv,
                               const trace::StringPairs &OpArgs) {
  static std::atomic<uint64_t> operationId(0);
  auto OpId = ++operationId;
  if (trace::anyEnabled()) {
    llvm::sys::ScopedLock L(consumersLock);
    for (auto *consumer : consumers) {
      consumer->operationStarted(OpId, OpKind, Inv, OpArgs);
    }
  }
  return OpId;
}

// Operation previously started with startXXX has finished
void trace::operationFinished(uint64_t OpId, trace::OperationKind OpKind,
                              ArrayRef<DiagnosticEntryInfo> Diagnostics) {
  if (trace::anyEnabled()) {
    llvm::sys::ScopedLock L(consumersLock);
    for (auto *consumer : consumers) {
      consumer->operationFinished(OpId, OpKind, Diagnostics);
    }
  }
}

// Must be called with consumersLock held.
static void updateTracedOperations() {
  OptionSet<trace::OperationKind> operations;
  for (auto *consumer : consumers) {
    operations |= consumer->desiredOperations();
  }
  // It is safe to store without a compare, because writers hold consumersLock.
  tracedOperations.store(uint64_t(operations), std::memory_order_relaxed);
}

// Register trace consumer
void trace::registerConsumer(trace::TraceConsumer *Consumer) {
  llvm::sys::ScopedLock L(consumersLock);
  consumers.push_back(Consumer);
  updateTracedOperations();
}

void trace::unregisterConsumer(trace::TraceConsumer *Consumer) {
  llvm::sys::ScopedLock L(consumersLock);
  consumers.erase(std::remove(consumers.begin(), consumers.end(), Consumer),
                  consumers.end());
  updateTracedOperations();
}
