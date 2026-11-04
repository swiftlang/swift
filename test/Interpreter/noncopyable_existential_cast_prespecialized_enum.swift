// RUN: %target-run-simple-swift(-Xfrontend -prespecialize-generic-metadata -target %target-future-triple -enable-experimental-feature NoncopyableCasting -parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -prespecialize-generic-metadata -target %target-future-triple -enable-experimental-feature NoncopyableCasting -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_NoncopyableCasting
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// With prespecialized metadata, the value witness table of a conditionally
// Copyable enum instantiated with a non-`Copyable` payload -- Optional<Big>,
// One<Big> -- is emitted by the compiler. Its flags must say non-`Copyable`:
// a take out of an existential checks them, and copies a value it believes
// Copyable out of a box rather than taking it, which traps for these.

final class Counter { static var live = 0 }

// Big, and so Optional<Big> and One<Big>, are too big for an existential's
// inline buffer: they are boxed.
struct Big: ~Copyable {
  var a: Int, b = 0, c = 0, d = 0
  init(_ a: Int) { self.a = a; Counter.live += 1 }
  deinit { Counter.live -= 1 }
}

enum One<T: ~Copyable>: ~Copyable { case some(T); case none }
extension One: Copyable where T: Copyable {}

@inline(never) func wrap<T: ~Copyable>(_ t: consuming T) -> any ~Copyable { t }

@inline(never) func takeBig(_ e: consuming any ~Copyable) -> Int {
  if let b = e as? Big { return b.a }
  _ = consume e
  return -1
}

@inline(never) func takeOptionalBig(_ e: consuming any ~Copyable) -> Int {
  if let o = e as? Optional<Big> { if let b = o { return b.a }; return 0 }
  _ = consume e
  return -1
}

@inline(never) func takeOneBig(_ e: consuming any ~Copyable) -> Int {
  if let o = e as? One<Big> {
    if case .some(let b) = consume o { return b.a }
    return 0
  }
  _ = consume e
  return -1
}

@main struct Main {
  static func main() {
    // CHECK: 7
    // CHECK-NEXT: 8
    // CHECK-NEXT: 9
    print(takeBig(wrap(Optional<Big>.some(Big(7)))))
    print(takeOptionalBig(wrap(Optional<Big>.some(Big(8)))))
    print(takeOneBig(wrap(One<Big>.some(Big(9)))))
    // A failed cast leaves the value to be destroyed once.
    // CHECK-NEXT: -1
    print(takeOneBig(wrap(Optional<Big>.some(Big(7)))))
    // CHECK-NEXT: live 0
    print("live", Counter.live)
  }
}
