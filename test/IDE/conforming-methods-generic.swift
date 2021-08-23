// RUN: %target-swift-ide-test -conforming-methods -source-filename %s -code-completion-token=CM1 -module-name MyModule -conforming-methods-expected-types '$s8MyModule6TargetPD' | %FileCheck %s -check-prefix=SI
// RUN: %target-swift-ide-test -conforming-methods -source-filename %s -code-completion-token=CM2 -module-name MyModule -conforming-methods-expected-types '$s8MyModule6TargetPD' | %FileCheck %s -check-prefix=SF

protocol Target {}
struct Concrete : Target {}

struct S<T> {
  func returnsAnything<U>() -> U { fatalError() }
}

extension S where T == Int {
  func returnsConcrete<U>(_ x: U) -> Concrete { fatalError() }
}

func test(si: S<Int>, sf: S<Float>) {
  si.#^CM1^#
  // SI:      -----BEGIN CONFORMING METHOD LIST-----
  // SI-NEXT: - TypeName: S<Int>
  // SI-NEXT: - Members:
  // SI-NEXT:    - Name: returnsConcrete(_:)
  // SI-NEXT:      TypeName: Concrete
  // SI-NEXT: -----END CONFORMING METHOD LIST-----

  sf.#^CM2^#
  // SF:      -----BEGIN CONFORMING METHOD LIST-----
  // SF-NEXT: - TypeName: S<Float>
  // SF-NEXT: - Members: []
  // SF-NEXT: -----END CONFORMING METHOD LIST-----
}
