// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -no-allocations -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -no-allocations -parse-as-library -wmo %target-embedded-posix-shim -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_NoncopyableCasting

// Casting non-`Copyable` existentials with -no-allocations. Every payload fits
// an existential's inline buffer (a larger one would need a box, which
// -no-allocations rejects), so the runtime never frees a box; in this mode the
// compiler-defined box deallocator traps and the program refers to no
// deallocation function (see dependencies-no-allocations.swift).

protocol P: ~Copyable { }

nonisolated(unsafe) var live = 0

struct A: P, ~Copyable {
  var tag: Int
  init(_ t: Int) { tag = t; live += 1 }
  deinit { live -= 1 }
}

struct B: P, ~Copyable {
  var tag: Int
  init(_ t: Int) { tag = t; live += 1 }
  deinit { live -= 1 }
}

@inline(never) func isA(_ e: borrowing any P & ~Copyable) -> Bool { e is A }

@inline(never) func chain(_ e: consuming any P & ~Copyable) -> Int {
  if let b = e as? B { return b.tag }
  if let a = e as? A { return a.tag }
  _ = consume e
  return -1
}

@inline(never) func sw(_ e: consuming any P & ~Copyable) -> Int {
  switch consume e {
  case let a as A: return a.tag
  case let other: _ = other; return 0
  }
}

@inline(never) func optional(_ o: consuming (any P & ~Copyable)?) -> Int {
  if let a = o as? A { return a.tag }
  return -1
}

@main struct Main {
  static func main() {
    // CHECK: yes
    // CHECK-NEXT: no
    let a: any P & ~Copyable = A(1)
    let b: any P & ~Copyable = B(2)
    print(isA(a) ? "yes" : "no")
    print(isA(b) ? "yes" : "no")
    _ = consume a
    _ = consume b
    // CHECK-NEXT: 1
    // CHECK-NEXT: 2
    print(chain(A(1)))
    print(chain(B(2)))
    // CHECK-NEXT: 1
    // CHECK-NEXT: 0
    print(sw(A(1)))
    print(sw(B(2)))
    // CHECK-NEXT: 1
    // CHECK-NEXT: -1
    print(optional(A(1)))
    print(optional(nil))
    // CHECK-NEXT: live 0
    print("live", terminator: " ")
    print(live)
  }
}
