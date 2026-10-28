// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// `if let v = x as? T`, `guard let`, and `while let` over a non-`Copyable`
// subject consume the subject only when the cast succeeds, so a failed
// attempt leaves it available to the next one.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

final class Canary {
  static var live = 0
  init() { Canary.live += 1 }
  deinit { Canary.live -= 1 }
}

struct Small: P, ~Copyable { var tag: Int }
struct WithCanary: P, ~Copyable { let canary: Canary; var tag: Int }
struct Unrelated: Q, ~Copyable { var tag: Int }

func make(_ k: Int) -> any P & ~Copyable {
  k == 0 ? Small(tag: 5) : WithCanary(canary: Canary(), tag: 7)
}

func ifChain(_ box: consuming any P & ~Copyable) -> Int {
  if let u = box as? Unrelated { return -u.tag }
  if let s = box as? Small { return s.tag }
  if let w = box as? WithCanary { return w.tag }
  return -1
}

func guardForm(_ box: consuming any P & ~Copyable) -> Int {
  guard let u = box as? Unrelated else { return ifChain(box) }
  return -u.tag
}

func whileForm(_ box: consuming any P & ~Copyable) -> Int {
  while let s = box as? Small { return s.tag }
  return ifChain(box)
}

func noMatch(_ box: consuming any P & ~Copyable) -> Int {
  if let u = box as? Unrelated { return u.tag }
  return -1
}

// CHECK: chain: 5 7
print("chain:", ifChain(make(0)), ifChain(make(1)))
// CHECK: guard: 7
print("guard:", guardForm(make(1)))
// CHECK: while: 5 7
print("while:", whileForm(make(0)), whileForm(make(1)))
// CHECK: no match: -1
print("no match:", noMatch(make(1)))
// Every value is destroyed exactly once, bound or dropped.
// CHECK: live canaries: 0
print("live canaries:", Canary.live)
