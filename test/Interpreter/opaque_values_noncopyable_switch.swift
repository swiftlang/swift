// RUN: %target-run-simple-swift(-Xfrontend -enable-sil-opaque-values -Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -enable-sil-opaque-values) | %FileCheck %s

// REQUIRES: executable_test

var live = 0
var handleDeinits = 0

final class Tracked {
  let name: String
  init(_ name: String) { self.name = name; live += 1 }
  deinit { live -= 1 }
}

struct Handle: ~Copyable {
  var tracked: Tracked
  deinit { handleDeinits += 1 }
}

struct Idle<T>: ~Copyable { var value: T? }
enum State<T>: ~Copyable {
  case idle(Idle<T>), count(Int), pair(T, Tracked), tracked(Tracked)
  case handle(Handle), other
}

func name<T>(_ s: borrowing State<T>) -> String {
  switch s {
  case .idle(let idle): return "idle(\(idle.value.map { "\($0)" } ?? "nil"))"
  case .count(let n): return "count(\(n))"
  case .pair(let a, let b): return "pair(\(a), \(b.name))"
  case .tracked(let t): return "tracked(\(t.name))"
  case .handle(let h): return "handle(\(h.tracked.name))"
  case .other: return "other"
  }
}

func isIdle<T>(_ s: borrowing State<T>) -> Bool {
  switch s {
  case .idle: return true
  default: return false
  }
}

func take<T>(_ s: consuming State<T>) -> T? {
  switch consume s {
  case .idle(let idle): return idle.value
  case .count: return nil
  case .pair: return nil
  case .tracked: return nil
  case .handle: return nil
  case .other: return nil
  }
}

func takeIfNamed<T>(_ s: consuming State<T>, _ wanted: String) -> String {
  switch consume s {
  case .pair(_, let t) where t.name == wanted: return "pair \(t.name)"
  default: return "no match"
  }
}

func test() {
  let idle = State.idle(Idle(value: Tracked("a")))
  let count = State<Tracked>.count(42)
  let pair = State.pair(Tracked("b"), Tracked("c"))
  let other = State<Tracked>.other

  // CHECK: idle(main.Tracked) true
  print(name(idle), isIdle(idle))
  // Borrowing twice leaves the payload in place.
  // CHECK-NEXT: idle(main.Tracked) true
  print(name(idle), isIdle(idle))
  // CHECK-NEXT: count(42) false
  print(name(count), isIdle(count))
  // CHECK-NEXT: pair(main.Tracked, c) false
  print(name(pair), isIdle(pair))
  // CHECK-NEXT: other false
  print(name(other), isIdle(other))

  // CHECK-NEXT: a
  print(take(idle)?.name ?? "nil")
  // CHECK-NEXT: nil
  print(take(count)?.name ?? "nil")
  _ = take(pair)
  _ = take(other)

  // CHECK-NEXT: tracked(d) false
  let tracked = State<Tracked>.tracked(Tracked("d"))
  print(name(tracked), isIdle(tracked))
  // CHECK-NEXT: handle(g) false
  let handle = State<Tracked>.handle(Handle(tracked: Tracked("g")))
  print(name(handle), isIdle(handle))

  // CHECK-NEXT: pair i
  print(takeIfNamed(State.pair(Tracked("h"), Tracked("i")), "i"))
  // CHECK-NEXT: no match
  print(takeIfNamed(State.pair(Tracked("h"), Tracked("i")), "j"))
}

test()
// CHECK-NEXT: live: 0, handle deinits: 1
print("live: \(live), handle deinits: \(handleDeinits)")
