// rdar://175857624
// RUN: %target-typecheck-verify-swift

struct Event {}

func totalDuration(_ p: UnsafePointer<Event>, _ n: UInt32) -> UInt32 { 0 }

func getTotalDuration(_ events: [Event]) -> CInt {
  return events.withUnsafeBufferPointer { ptr in // expected-error {{cannot convert return expression of type 'UInt32' to return type 'CInt' (aka 'Int32')}}
    return totalDuration(ptr.baseAddress!, UInt32(ptr.count))
  }
}
