// RUN: %target-swift-frontend -primary-file %s -parse-as-library -module-name=test -emit-sil -O | %FileCheck %s

// Check that co-routines are inlined, even if the yielded value is something non-trivial.

struct S {
  var i: Int64
  var s: String
}

public struct Foo {
  final class Box {
    var value = S(i: 0, s: "")
  }

  var storage: Box = .init()

  var value: S {
    get {
      storage.value
    }
    _modify {
      var value = storage.value
      defer { storage.value = value }
      yield &value
    }
  }
}

// CHECK-LABEL: sil hidden @$s4test6testit1x3boxys5Int64V_AA3FooVztF :
// CHECK-NOT:     begin_apply
// CHECK:       } // end sil function '$s4test6testit1x3boxys5Int64V_AA3FooVztF'
func testit(x: Int64, box: inout Foo) {
  box.value.i ^= x
}

