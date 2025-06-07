// RUN: %target-swift-emit-sil %s -enable-experimental-feature Embedded -wmo | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// CHECK: sil @$e4main1XC3fooxyFSi_Tg5 : $@convention(method) (@guaranteed X<Int>) -> Int {

// CHECK-LABEL: sil_vtable $X<Int>
// CHECK:         #X.foo: <T> (X<T>) -> () -> T : @$e4main1XC3fooxyFSi_Tg5
// CHECK:       }

open class X<T> {

  var t: T

  init(t: T) {
    self.t = t
  }

  open func foo() -> T { t }
}

func testit() -> Int {
  let x = X(t: 27)
  return x.foo()
}
