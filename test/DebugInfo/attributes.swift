// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s

// CHECK-DAG: {{.*}}i32 40960,{{.*}}null, metadata ![[TY0:.*]]} ; [ DW_TAG_structure_type ] [{{.*}}ObjCClass{{.*}}] [line [[@LINE+1]]
@objc class ObjCClass {
      @IBAction func click(_: AnyObject?) -> () {}
}
// DW_LANG_Swift = 0xa000 [FIXME: this number will change!]
// CHECK-DAG: ![[TY1:[0-9]+]] = {{.*}}i32 40960,{{.*}} [ DW_TAG_structure_type ] [{{.*}}SwiftClass{{.*}}] [line [[@LINE+1]]
class SwiftClass {
      @objc func objcmethod() -> () {}
      func swiftmethod() -> () {}
      // Block attribute
      func f(someBlock: @objc_block (Int) -> Int) {
      }
}

// FIXME: This is currently elided, but should reappear eventually as
// an artificial variable.
// DISABLED: [ DW_TAG_variable ] [OBJC_METACLASS_$__TtC10attributes9ObjCClass]

// CHECK-DAG: i32 [[@LINE+1]], metadata ![[TY0]],{{.*}}[ DW_TAG_variable ] [{{.*}}strongRef0{{.*}}]
var strongRef0 : ObjCClass
var strongRef1 : SwiftClass = SwiftClass()

// CHECK-DAG: [ DW_TAG_typedef ] [_TtXwGSqC10attributes10SwiftClass_] [line [[@LINE+1]]
weak var    weakRef1    : SwiftClass? = strongRef1
// CHECK-DAG: [ DW_TAG_typedef ] [_TtXoC10attributes10SwiftClass] [line [[@LINE+1]]
unowned var unownedRef1 : SwiftClass

@class_protocol protocol Protocol1 {
  func foo(x: Float) -> Float
}

class Implementation : Protocol1 {
  func foo(x: Float) -> Float { return 2*x }
}
