// RUN: %target-swift-frontend -module-name test -O -emit-sil -primary-file %s | %FileCheck %s

// REQUIRES: optimized_stdlib,swift_stdlib_no_asserts

// Check that there is only a single retain in concat.

// CHECK-LABEL: sil hidden @$s4test6concatyAA14TestCollectionVAD_ADtF :
// CHECK:         retain
// CHECK-NOT:     retain
// CHECK-NOT:     release
// CHECK:         apply
// CHECK-NOT:     retain
// CHECK-NOT:     release
// CHECK:       } // end sil function '$s4test6concatyAA14TestCollectionVAD_ADtF'
func concat(_ l: TestCollection, _ r: TestCollection) -> TestCollection {
    l + r
}

struct TestCollection: RandomAccessCollection, RangeReplaceableCollection {
  private var base: [Int]

  init(base: [Int]) {
    self.base = base
  }

  init() {
    self.base = []
  }

  var startIndex: Int { base.startIndex }

  var endIndex: Int { base.endIndex }

  subscript(index: Int) -> Int {
    get {
      base[index]
    }
    set {
      base[index] = newValue
    }
  }

  mutating func replaceSubrange<C>(_ range: Range<Int>, with newElements: C) where C: Collection, C.Element == Int {
    self.base.replaceSubrange(range, with: newElements)
  }

  @inline(never)
  mutating func append<S>(contentsOf other: S) where S: Sequence, S.Element == Int {
    base.append(contentsOf: other)
  }
}

class C {
  func foo() {}
}

struct S: ~Copyable {
  var c: C

  // Check that there is only a single release in the deinit.

  // CHECK-LABEL: sil hidden @$s4test1SVfD :
  // CHECK-NOT:     retain
  // CHECK-NOT:     release
  // CHECK:         apply
  // CHECK:         release
  // CHECK-NOT:     release
  // CHECK:       } // end sil function '$s4test1SVfD'
  deinit {
    c.foo()
  }
}


