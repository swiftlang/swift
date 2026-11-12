// rdar://175857624
// RUN: %target-typecheck-verify-swift

struct Event {}

func totalDuration(_ p: UnsafePointer<Event>, _ n: UInt32) -> UInt32 { 0 }

func getTotalDuration(_ events: [Event]) -> CInt {
  return events.withUnsafeBufferPointer { ptr in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    return totalDuration(ptr.baseAddress!, UInt32(ptr.count))
  }
}
