// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none

// https://github.com/swiftlang/swift/issues/91477

// Check that a key path captured argument whose alignment exceeds pointer alignment
// must not have its fields corrupted.
//
// Restricted to -Onone: under -O, generic specialization of `check<V>` hits a
// separate, pre-existing bug (unrelated to this fix) where every alignment,
// not just over-aligned ones, corrupts the captured argument.

final class Token {
  nonisolated(unsafe) static var alive = 0
  init() { Token.alive += 1 }
  deinit { Token.alive -= 1 }
}

// A generic struct used as a keypath subscript index: one generic stored
// field next to a concrete non-POD field.
struct Arg<Value>: Hashable {
  static func == (l: Self, r: Self) -> Bool { l.ref === r.ref }
  func hash(into h: inout Hasher) { ObjectIdentifier(ref).hash(into: &h) }
  let value: Value
  let ref: Token
  init(_ v: Value) { value = v; ref = Token() }
}

struct Root {
  subscript<V>(meta arg: Arg<V>) -> Int { get { 0 } set { _ = newValue } }
}

func check<V>(_ label: String, _ v: V) {
  let before = Token.alive
  do {
    let arg = Arg(v)
    for _ in 0 ..< 10 {
      withExtendedLifetime(\Root[meta: arg] as WritableKeyPath<Root, Int>) {}
    }
  }
  let delta = Token.alive - before
  print("\(label): argument alignment \(MemoryLayout<Arg<V>>.alignment), " +
      "refcount delta \(delta) -> \(delta == 0 ? "ok" : "WRONG")")
}

struct A4 { var v: Int32 = 0 }
struct A8 { var v: Double = 0 }
struct A16 { var v = SIMD4<Float>() }

print("pointer alignment = \(MemoryLayout<UnsafeRawPointer>.alignment)")
check("align 4 ", A4())
check("align 8 ", A8())
check("align 16", A16())
print("done")

// CHECK: pointer alignment = {{[0-9]+}}
// CHECK: align 4 : argument alignment {{[0-9]+}}, refcount delta 0 -> ok
// CHECK: align 8 : argument alignment {{[0-9]+}}, refcount delta 0 -> ok
// CHECK: align 16: argument alignment {{[0-9]+}}, refcount delta 0 -> ok
// CHECK: done
