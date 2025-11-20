// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

// https://github.com/apple/swift/issues/60958
// Make sure that `makeIterator` witness is picked over the non-witness.

struct S: Sequence {
  private var _storage: [UInt8] = []

  func makeIterator() -> Array<UInt8>.Iterator {
    _storage.makeIterator()
  }

  typealias Element = UInt8

  class Iterator: IteratorProtocol {
    func next() -> UInt8? { 0 }
    typealias Element = UInt8
  }

  func makeIterator() -> Iterator {
    Iterator()
  }
}

extension S {
  // CHECK: sil hidden [ossa] @$s4main1SV1fyyF
  func f() {
    for elt in self {
      // CHECK: [[ITERATOR_VAR:%.*]] = project_box {{.*}} : ${ var S.Iterator }, 0
      // CHECK: [[MAKE_ITERATOR_REF:%.*]] = function_ref @$s4main1SV12makeIteratorAC0C0CyF : $@convention(method) (@guaranteed S) -> @owned S.Iterator
      // CHECK-NEXT: [[ITERATOR:%.*]] = apply [[MAKE_ITERATOR_REF]](%0) : $@convention(method) (@guaranteed S) -> @owned S.Iterator
      // CHECK-NEXT: store [[ITERATOR]] to [init] [[ITERATOR_VAR]] : $*S.Iterator
      print(elt)
    }
  }
}
