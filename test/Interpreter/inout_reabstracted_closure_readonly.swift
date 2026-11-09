// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s
// REQUIRES: executable_test

// Reading a closure stored in a generic container through a generic inout
// round-trip must not grow the stored value: before the fix for #91348,
// every read rewrote the storage as thunkB(thunkA(original)), adding two
// stack frames per read to any later call of the stored closure. This is the
// shape of Mutex<(() -> Int)?>.withLock { $0 }: the storage lives at the
// generic (opaque) representation, and only the closure's inout parameter
// crosses the abstraction boundary. @inline(never) stands in for the opaque
// function boundary that keeps the optimizer from folding the thunk pair, so
// the -O run exercises the same accumulation.
// https://github.com/swiftlang/swift/issues/91348

@inline(never)
func withGenericInout<T, R>(_ value: inout T, _ body: (inout T) -> R) -> R {
  body(&value)
}

final class Box<T> {
  var value: T
  init(_ value: T) { self.value = value }
}

var probeAddress: UInt = 0

enum Env {
  static let box = Box<() -> Int>({
    var probe: UInt8 = 0
    withUnsafePointer(to: &probe) { probeAddress = UInt(bitPattern: $0) }
    return 0
  })

  static func read() -> () -> Int {
    withGenericInout(&box.value) { $0 }
  }
}

// Calling the stored closure from a fixed call site must observe the same
// stack depth before and after many reads.
@inline(never)
func callStored() -> UInt {
  _ = Env.box.value()
  return probeAddress
}

let base = callStored()
for _ in 0..<1_000 {
  _ = Env.read()
}
let after = callStored()
let drift = base >= after ? base - after : after - base
print("drift: \(drift)")
// CHECK: drift: 0
