// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s
// REQUIRES: objc_interop

struct MyStruct {
  struct Inner {
    let myProp = 1
  }
}

class MyClass {
  class Inner {
    @objc var myProp = 1
  }
}

let a = \MyStruct.Inner.myProp
// CHECK: [[@LINE-1]]:25 | {{.*}} | myProp
// CHECK: [[@LINE-2]]:10 | {{.*}} | MyStruct
// CHECK: [[@LINE-3]]:19 | {{.*}} | Inner
let b: KeyPath<MyStruct.Inner, Int> = \.myProp
// CHECK: [[@LINE-1]]:41 | {{.*}} | myProp
let c = \MyClass.Inner.myProp
// CHECK: [[@LINE-1]]:24 | {{.*}} | myProp
// CHECK: [[@LINE-2]]:10 | {{.*}} | MyClass
// CHECK: [[@LINE-3]]:18 | {{.*}} | Inner
let d: KeyPath<MyClass.Inner, Int> = \.myProp
// CHECK: [[@LINE-1]]:40 | {{.*}} | myProp
