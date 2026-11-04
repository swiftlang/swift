// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -parse-as-library -wmo %target-embedded-posix-shim -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_NoncopyableCasting

// Casts out of a non-`Copyable` existential in Embedded Swift: a test borrows
// the value, and a binding consumes it only when the cast succeeds, so a chain
// of attempts works. Every value is destroyed exactly once.

protocol P: ~Copyable { }

final class Counter {
  static var live = 0
}

struct Small: P, ~Copyable {
  var tag: Int
  init(_ tag: Int) { self.tag = tag; Counter.live += 1 }
  deinit { Counter.live -= 1 }
}

// Too big for the existential's inline buffer, so it is stored out of line: a
// binding cast has to take it out of its box rather than copy it.
struct Big: P, ~Copyable {
  var a: Int, b = 0, c = 0, d = 0
  init(_ a: Int) { self.a = a; Counter.live += 1 }
  deinit { Counter.live -= 1 }
}

struct Third: P, ~Copyable { }

@inline(never) func isTests(_ e: borrowing any P & ~Copyable) -> Int {
  (e is Small ? 1 : 0) + (e is Big ? 2 : 0) + (e is Third ? 4 : 0)
}

@inline(never) func ifLetChain(_ e: consuming any P & ~Copyable) -> Int {
  if let n = e as? Third { _ = n; return -1 }
  if let b = e as? Big { return b.a }
  if let s = e as? Small { return s.tag }
  _ = consume e
  return 0
}

@inline(never) func ifCaseChain(_ e: consuming any P & ~Copyable) -> Int {
  if case let b as Big = e { return b.a }
  if case let s as Small = e { return s.tag }
  _ = consume e
  return 0
}

@inline(never) func borrowingSwitch(_ e: borrowing any P & ~Copyable) -> Int {
  switch e {
  case is Big: return 2
  case is Small: return 1
  default: return 0
  }
}

@inline(never) func consumingSwitch(_ e: consuming any P & ~Copyable) -> Int {
  switch consume e {
  case let b as Big: return b.a
  case let s as Small: return s.tag
  case let other: _ = other; return 0
  }
}

// An Optional subject is cast as it stands: the runtime unwraps it, so a test
// borrows it and a binding consumes it only on success, as for an existential.
@inline(never) func optionalIsTests(_ o: borrowing (any P & ~Copyable)?) -> Int {
  var r = o is Small ? 1 : 0
  if case is Big = o { r += 2 }
  return r
}

@inline(never) func optionalIfLetChain(_ o: consuming (any P & ~Copyable)?) -> Int {
  if let b = o as? Big { return b.a }
  if let s = o as? Small { return s.tag }
  _ = consume o
  return 0
}

@inline(never) func optionalSwitch(_ o: consuming (any P & ~Copyable)?) -> Int {
  switch consume o {
  case let s as Small: return s.tag
  case .none: return -1
  case let other: _ = other; return 0
  }
}

@main struct Main {
  static func main() {
    // CHECK: 1
    // CHECK-NEXT: 2
    // CHECK-NEXT: 4
    let s: any P & ~Copyable = Small(5)
    let b: any P & ~Copyable = Big(7)
    let n: any P & ~Copyable = Third()
    print(isTests(s))
    print(isTests(b))
    print(isTests(n))
    // CHECK-NEXT: 2
    print(borrowingSwitch(b))
    _ = consume s
    _ = consume b
    _ = consume n

    // CHECK-NEXT: 5
    // CHECK-NEXT: 7
    // CHECK-NEXT: -1
    print(ifLetChain(Small(5)))
    print(ifLetChain(Big(7)))
    print(ifLetChain(Third()))
    // CHECK-NEXT: 7
    // CHECK-NEXT: 5
    print(ifCaseChain(Big(7)))
    print(ifCaseChain(Small(5)))
    // CHECK-NEXT: 7
    // CHECK-NEXT: 5
    // CHECK-NEXT: 0
    print(consumingSwitch(Big(7)))
    print(consumingSwitch(Small(5)))
    print(consumingSwitch(Third()))
    // CHECK-NEXT: 1
    // CHECK-NEXT: 2
    // CHECK-NEXT: 0
    let os: (any P & ~Copyable)? = Small(5)
    let ob: (any P & ~Copyable)? = Big(7)
    let oz: (any P & ~Copyable)? = nil
    print(optionalIsTests(os))
    print(optionalIsTests(ob))
    print(optionalIsTests(oz))
    _ = consume os
    _ = consume ob
    _ = consume oz
    // CHECK-NEXT: 7
    // CHECK-NEXT: 5
    // CHECK-NEXT: 0
    print(optionalIfLetChain(Big(7)))
    print(optionalIfLetChain(Small(5)))
    print(optionalIfLetChain(nil))
    // CHECK-NEXT: 5
    // CHECK-NEXT: -1
    // CHECK-NEXT: 0
    print(optionalSwitch(Small(5)))
    print(optionalSwitch(nil))
    print(optionalSwitch(Big(7)))

    // CHECK-NEXT: live 0
    print("live", terminator: " ")
    print(Counter.live)
  }
}
