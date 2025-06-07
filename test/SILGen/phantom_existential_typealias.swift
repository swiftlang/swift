// RUN: %target-swift-emit-silgen %s | %FileCheck %s

struct S {
  var y: Int { fatalError() }
}

struct G<Element> {
  func f<T>(_: (Element) -> T) -> [T] { fatalError() }
}

protocol P {
  typealias A = S
  var x: G<A> { get }
}

// CHECK-LABEL: sil private [ossa] @$s29phantom_existential_typealias5test11pSaySiGAA1P_p_tFSiAA1SVXEfU_ : $@convention(thin) @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <S, Int> {

func test1(p: any P) -> [Int] {
  return p.x.f { $0.y }
}

func callee(_: () -> ()) {}

func test2(p: any P) {
  let a = p.x
  callee { _ = a }
}