// RUN: %swift -typecheck -target %target-cpu-apple-macosx10.9 -verify -sdk %sdk %s
// REQUIRES: OS=macosx
// REQUIRES: libdispatch

import Dispatch

// Don't warn because these APIs were deprecated in macOS 10.10 and the
// minimum deployment target is 10.9.
_ = DispatchQueue.GlobalQueuePriority.high // no-warning
_ = DispatchQueue.GlobalQueuePriority.default // no-warning
_ = DispatchQueue.GlobalQueuePriority.low // no-warning
_ = DispatchQueue.GlobalQueuePriority.background // no-warning

_ = DispatchQueue.global(priority:DispatchQueue.GlobalQueuePriority.background)  // no-warning
