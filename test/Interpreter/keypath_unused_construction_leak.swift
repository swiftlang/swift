// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

// A constructed KeyPath that is never read (only kept alive briefly via
// withExtendedLifetime) is eligible for dead-code elimination at -O. If the
// subscript index argument it captures owns a refcounted value, eliminating
// the unused KeyPath must not leak that value.

final class Token {
  nonisolated(unsafe) static var alive = 0
  init() { Token.alive += 1 }
  deinit { Token.alive -= 1 }
}

struct Arg: Hashable {
  static func == (l: Self, r: Self) -> Bool { l.ref === r.ref }
  func hash(into h: inout Hasher) { ObjectIdentifier(ref).hash(into: &h) }
  let value: Int32
  let ref: Token
  init(_ v: Int32) { value = v; ref = Token() }
}

struct Root {
  subscript(meta arg: Arg) -> Int { get { 0 } set { _ = newValue } }
}

func check() {
  let before = Token.alive
  do {
    let arg = Arg(42)
    for _ in 0 ..< 10 {
      withExtendedLifetime(\Root[meta: arg] as WritableKeyPath<Root, Int>) {}
    }
  }
  let delta = Token.alive - before
  print("refcount delta \(delta) -> \(delta == 0 ? "ok" : "WRONG")")
}

check()
print("done")

// CHECK: refcount delta 0 -> ok
// CHECK: done
