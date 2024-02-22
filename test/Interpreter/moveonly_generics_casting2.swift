// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all -enable-experimental-feature NoncopyableGenerics) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all -enable-experimental-feature NoncopyableGenerics) | %FileCheck %s

// `G<Duck>() as? P` triggers a crash (rdar://123466649)
// XFAIL: *

protocol Q {
  associatedtype A: ~Copyable
}

struct G<T: Q> {}; extension G: P where T.A: Copyable {}

protocol P {func speak()}; extension P {func speak() { print("hello") }}

struct Chicken: Q {
  struct A: ~Copyable {}
}

struct Duck: Q {
  struct A {}
}

defer { check() }
func check() {
  if let p = G<Duck>() as? P {
    p.speak() // CHECK: hello
  }

  if (G<Chicken>() as? P) == nil {
    print("correct") // CHECK: correct
  }
}
