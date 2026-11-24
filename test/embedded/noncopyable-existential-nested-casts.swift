// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -parse-as-library -wmo %target-embedded-posix-shim -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_NoncopyableCasting

// A take out of an existential whose payload is itself an existential, or an
// Optional of one, possibly boxed at each level. The runtime moves the
// non-`Copyable` payload out of every box it unwraps and frees the box with
// the compiler-defined __swift_deallocate_boxed_opaque_existential_0.

protocol P: ~Copyable { }

final class Counter { static var live = 0 }

struct Small: P, ~Copyable {
  var tag: Int
  init(_ t: Int) { tag = t; Counter.live += 1 }
  deinit { Counter.live -= 1 }
}

// Too big for an existential's inline buffer.
struct Big: P, ~Copyable {
  var a: Int, b = 0, c = 0, d = 0
  init(_ a: Int) { self.a = a; Counter.live += 1 }
  deinit { Counter.live -= 1 }
}

typealias E = any P & ~Copyable

// Wraps any value -- an existential or an Optional too -- as the payload of
// an `any ~Copyable`.
@inline(never) func wrap<T: ~Copyable>(_ t: consuming T) -> any ~Copyable { t }

@inline(never) func takeBig(_ e: consuming any ~Copyable) -> Int {
  if let b = e as? Big { return b.a }
  _ = consume e
  return -1
}

@inline(never) func takeSmall(_ e: consuming any ~Copyable) -> Int {
  if let s = e as? Small { return s.tag }
  _ = consume e
  return -1
}

@main struct Main {
  static func main() {
    // CHECK: 7
    // CHECK-NEXT: 7
    // CHECK-NEXT: 7
    print(takeBig(wrap(Big(7) as E)))
    print(takeBig(wrap(Optional<E>.some(Big(7)))))
    print(takeBig(wrap(Optional<Optional<E>>.some(.some(Big(7))))))
    // CHECK-NEXT: 5
    // CHECK-NEXT: 5
    print(takeSmall(wrap(Small(5) as E)))
    print(takeSmall(wrap(Optional<E>.some(Small(5)))))
    // A failed cast leaves everything to be destroyed once, as usual.
    // CHECK-NEXT: -1
    // CHECK-NEXT: -1
    print(takeSmall(wrap(Optional<E>.some(Big(7)))))
    print(takeBig(wrap(Optional<E>.none)))
    // CHECK-NEXT: live 0
    print("live", terminator: " ")
    print(Counter.live)
  }
}
