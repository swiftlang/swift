//===--- SwiftJobPriorityQueue.h - Priority queue of jobs -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_SWIFTJOBPRIORITYQUEUE_H
#define SWIFT_CONCURRENCY_SWIFTJOBPRIORITYQUEUE_H

#include "swift/Basic/PriorityQueue.h"
#include "swift/EmbeddedPlatform.h"
#include "swift/Runtime/Config.h"

// Keep ExecutorImpl.h after swift/Runtime/Config.h.
#include "ExecutorImpl.h"

namespace swift {

struct SwiftJobQueueTraits {
  static SwiftJob *&storage(SwiftJob *cur) {
    return reinterpret_cast<SwiftJob *&>(cur->schedulerPrivate[0]);
  }

  static SwiftJob *getNext(SwiftJob *job) { return storage(job); }
  static void setNext(SwiftJob *job, SwiftJob *next) { storage(job) = next; }
  enum { prioritiesCount = SwiftJobPriorityBucketCount };
  static int getPriorityIndex(SwiftJob *job) {
    return swift_priority_getBucketIndex(swift_job_getPriorityFromFlags(job));
  }
};

/// A FIFO queue per job priority, linked through `schedulerPrivate[0]`.
using SwiftJobPriorityQueue = PriorityQueue<SwiftJob *, SwiftJobQueueTraits>;

} // end namespace swift

#endif // SWIFT_CONCURRENCY_SWIFTJOBPRIORITYQUEUE_H
