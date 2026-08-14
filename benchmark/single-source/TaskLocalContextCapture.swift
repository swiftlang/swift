//===--- TaskLocalContextCapture.swift ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Measures `TaskLocalContext.current` — i.e. TaskLocal::Snapshot::capture —
// across varying binding depths. The capture path in TaskLocal.cpp walks the
// live task-local chain three times (count → layout → fill) so that the
// entire snapshot lives in a single contiguous swift_slowAlloc buffer with no
// scratch allocations, and reuses `forEachVisibleValueImpl` (the same helper
// `Storage::copyTo` uses).
//
// These benchmarks stress that capture path directly: the loop repeatedly
// takes `.current` (which allocates & fills the snapshot) and immediately
// drops it (which runs vw_destroy + release-key + swift_slowDealloc).
//
// Companion to TaskLocalGet.swift, which stresses lookup instead of capture.

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *) else {
    return []
  }
  return [
    BenchmarkInfo(
      name: "TaskLocalContextCapture.Empty",
      runFunction: run_CaptureEmpty,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskLocalContextCapture.1",
      runFunction: run_CaptureOne,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskLocalContextCapture.4",
      runFunction: run_CaptureFour,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskLocalContextCapture.16",
      runFunction: run_CaptureSixteen,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskLocalContextCapture.4.WithValues",
      runFunction: run_CaptureFourAndPushPop,
      tags: [.concurrency]
    ),
  ]
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
private enum BenchLocals {
  @TaskLocal static var a: Int = 0
  @TaskLocal static var b: Int = 0
  @TaskLocal static var c: Int = 0
  @TaskLocal static var d: Int = 0
  @TaskLocal static var e: Int = 0
  @TaskLocal static var f: Int = 0
  @TaskLocal static var g: Int = 0
  @TaskLocal static var h: Int = 0
  @TaskLocal static var i: Int = 0
  @TaskLocal static var j: Int = 0
  @TaskLocal static var k: Int = 0
  @TaskLocal static var l: Int = 0
  @TaskLocal static var m: Int = 0
  @TaskLocal static var n: Int = 0
  @TaskLocal static var o: Int = 0
  @TaskLocal static var p: Int = 0
}

// Capture with no bindings visible — exercises the `.empty` singleton
// fast path (runtime returns nullptr, Swift wrapper reuses the shared
// _TaskLocalContextStorage.empty instead of allocating).
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
@inline(never)
private func run_CaptureEmpty(_ n: Int) {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      blackHole(TaskLocalContext.current)
    }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
@inline(never)
private func run_CaptureOne(_ n: Int) {
  BenchLocals.$a.withValue(1) {
    for _ in 0..<n {
      for _ in 0..<1_000 {
        blackHole(TaskLocalContext.current)
      }
    }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
@inline(never)
private func run_CaptureFour(_ n: Int) {
  BenchLocals.$a.withValue(1) {
    BenchLocals.$b.withValue(2) {
      BenchLocals.$c.withValue(3) {
        BenchLocals.$d.withValue(4) {
          for _ in 0..<n {
            for _ in 0..<1_000 {
              blackHole(TaskLocalContext.current)
            }
          }
        }
      }
    }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
@inline(never)
private func run_CaptureSixteen(_ n: Int) {
  BenchLocals.$a.withValue(1) {
    BenchLocals.$b.withValue(2) {
      BenchLocals.$c.withValue(3) {
        BenchLocals.$d.withValue(4) {
          BenchLocals.$e.withValue(5) {
            BenchLocals.$f.withValue(6) {
              BenchLocals.$g.withValue(7) {
                BenchLocals.$h.withValue(8) {
                  BenchLocals.$i.withValue(9) {
                    BenchLocals.$j.withValue(10) {
                      BenchLocals.$k.withValue(11) {
                        BenchLocals.$l.withValue(12) {
                          BenchLocals.$m.withValue(13) {
                            BenchLocals.$n.withValue(14) {
                              BenchLocals.$o.withValue(15) {
                                BenchLocals.$p.withValue(16) {
                                  for _ in 0..<n {
                                    for _ in 0..<500 {
                                      blackHole(TaskLocalContext.current)
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

// End-to-end: capture 4 bindings, then push them + read one back + pop.
// Measures the full external-propagation shape (capture ⇒ withValues body).
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *)
@inline(never)
private func run_CaptureFourAndPushPop(_ n: Int) {
  BenchLocals.$a.withValue(1) {
    BenchLocals.$b.withValue(2) {
      BenchLocals.$c.withValue(3) {
        BenchLocals.$d.withValue(4) {
          for _ in 0..<n {
            for _ in 0..<500 {
              let ctx = TaskLocalContext.current
              ctx.withValues {
                blackHole(BenchLocals.a)
              }
            }
          }
        }
      }
    }
  }
}
