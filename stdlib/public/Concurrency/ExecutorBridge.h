//===--- ExecutorBridge.h - C++ side of executor bridge -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EXECUTOR_BRIDGE_H_
#define SWIFT_EXECUTOR_BRIDGE_H_

#include "swift/Runtime/Concurrency.h"

namespace swift {

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

extern "C" SWIFT_CC(swift)
SerialExecutorRef swift_getMainExecutor();

extern "C" SWIFT_CC(swift)
TaskExecutorRef swift_getDefaultExecutor();

#if !SWIFT_CONCURRENCY_EMBEDDED
extern "C" SWIFT_CC(swift)
void *swift_createDispatchEventC(void (*handler)(void *), void *context);

extern "C" SWIFT_CC(swift)
void swift_destroyDispatchEventC(void *event);

extern "C" SWIFT_CC(swift)
void swift_signalDispatchEvent(void *event);
#endif // !SWIFT_CONCURRENCY_EMBEDDED

extern "C" SWIFT_CC(swift) __attribute__((noreturn))
void swift_dispatchMain();

extern "C" SWIFT_CC(swift)
void swift_createDefaultExecutors();

extern "C" SWIFT_CC(swift)
void swift_createDefaultExecutorsOnce();

#pragma clang diagnostic pop

} // namespace swift

#endif /* SWIFT_EXECUTOR_BRIDGE_H_ */
