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

struct CopyableIdle<T> { var value: T? }
enum CopyableState<T> {
  case idle(CopyableIdle<T>), count(Int), opaque(T), pair(T, Tracked)
  case tracked(Tracked), other
}
enum CopyableOuter<T> { case state(CopyableState<T>), none }

func name<T>(_ s: CopyableState<T>) -> String {
  switch s {
  case .idle(let idle): return "idle(\(idle.value.map { "\($0)" } ?? "nil"))"
  case .count(let n) where n < 0: return "negative"
  case .count(let n): return "count(\(n))"
  case .opaque(let a): return "opaque(\(a))"
  case .pair(let a, let b): return "pair(\(a), \(b.name))"
  case .tracked(let t): return "tracked(\(t.name))"
  default: return "other"
  }
}

func payload<T>(_ s: CopyableState<T>) -> T? {
  switch s {
  case .idle(let idle): return idle.value
  case .opaque(let a), .pair(let a, _): return a
  default: return nil
  }
}

func nested<T>(_ o: CopyableOuter<T>) -> T? {
  switch o {
  case .state(.idle(let idle)): return idle.value
  case .state(.pair(let a, _)): return a
  case .state: return nil
  case .none: return nil
  }
}

func pairNamed<T>(_ s: CopyableState<T>, _ wanted: String) -> String {
  switch s {
  case .pair(_, let t) where t.name == wanted: return "pair \(t.name)"
  case let other: return "no match \(name(other))"
  }
}

func cast<T>(_ s: CopyableState<T>) -> String {
  switch s {
  case .pair(let a as Tracked, _): return "tracked pair \(a.name)"
  case .pair(is Int, _): return "int pair"
  default: return "other"
  }
}

func both<T>(_ a: CopyableState<T>, _ b: CopyableState<T>) -> String {
  switch (a, b) {
  case (.tracked(let x), .tracked(let y)): return "\(x.name)\(y.name)"
  case (.other, _): return "other first"
  default: return "mismatch"
  }
}

func testCopyable() {
  let idle = CopyableState.idle(CopyableIdle(value: Tracked("a")))
  let count = CopyableState<Tracked>.count(42)
  let negative = CopyableState<Tracked>.count(-1)
  let pair = CopyableState.pair(Tracked("b"), Tracked("c"))
  let tracked = CopyableState<Tracked>.tracked(Tracked("d"))
  let other = CopyableState<Tracked>.other

  // CHECK-NEXT: idle(main.Tracked) a
  print(name(idle), payload(idle)!.name)
  // Matching twice leaves the payload in place.
  // CHECK-NEXT: idle(main.Tracked) a
  print(name(idle), payload(idle)!.name)
  // CHECK-NEXT: count(42) negative
  print(name(count), name(negative))
  // CHECK-NEXT: pair(main.Tracked, c) b
  print(name(pair), payload(pair)!.name)
  // CHECK-NEXT: tracked(d) other
  print(name(tracked), name(other))
  // CHECK-NEXT: opaque(main.Tracked) f
  let opaque = CopyableState.opaque(Tracked("f"))
  print(name(opaque), payload(opaque)!.name)

  // CHECK-NEXT: a b true
  print(nested(.state(idle))!.name, nested(.state(pair))!.name,
        nested(CopyableOuter<Tracked>.none) == nil)

  // CHECK-NEXT: pair c
  print(pairNamed(pair, "c"))
  // CHECK-NEXT: no match pair(main.Tracked, c)
  print(pairNamed(pair, "x"))
  // CHECK-NEXT: no match other
  print(pairNamed(other, "c"))

  // CHECK-NEXT: tracked pair b
  print(cast(pair))
  // CHECK-NEXT: int pair
  print(cast(CopyableState.pair(1, Tracked("e"))))

  // CHECK-NEXT: dd
  print(both(tracked, tracked))
  // CHECK-NEXT: other first
  print(both(other, tracked))
  // CHECK-NEXT: mismatch
  print(both(tracked, other))
}

test()
testCopyable()
// CHECK-NEXT: live: 0, handle deinits: 1
print("live: \(live), handle deinits: \(handleDeinits)")
