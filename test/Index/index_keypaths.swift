// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s
// REQUIRES: objc_interop

struct MyStruct {
  struct Inner {
    let myProp = 1
  }
}

let a = \MyStruct.Inner.myProp
// CHECK: [[@LINE-1]]:25 | {{.*}} | myProp
// CHECK: [[@LINE-2]]:10 | {{.*}} | MyStruct
// CHECK: [[@LINE-3]]:19 | {{.*}} | Inner
let b: KeyPath<MyStruct.Inner, Int> = \.myProp
// CHECK: [[@LINE-1]]:41 | {{.*}} | myProp

class MyClass {
  class Inner {
    @objc var myProp = 1
    func method() {
      let c: String = #keyPath(myProp)
      // CHECK: [[@LINE-1]]:32 | {{.*}} | myProp
    }
  }
}

let d: String = #keyPath(MyClass.Inner.myProp)
// CHECK: [[@LINE-1]]:26 | {{.*}} | MyClass
// CHECK: [[@LINE-2]]:34 | {{.*}} | Inner
// CHECK: [[@LINE-3]]:40 | {{.*}} | myProp

let e = \MyClass.Inner.myProp
// CHECK: [[@LINE-1]]:24 | {{.*}} | myProp
// CHECK: [[@LINE-2]]:10 | {{.*}} | MyClass
// CHECK: [[@LINE-3]]:18 | {{.*}} | Inner
let f: KeyPath<MyClass.Inner, Int> = \.myProp
// CHECK: [[@LINE-1]]:40 | {{.*}} | myProp
