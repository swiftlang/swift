// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module %s -emit-ir -g -o - | FileCheck %s

// REQUIRES: objc_interop

// CHECK-DAG: !MDCompositeType(tag: DW_TAG_structure_type, name: "ObjCClass",{{.*}} line: [[@LINE+1]],{{.*}} runtimeLang: DW_LANG_Swift,{{.*}} identifier: [[TY0:"[^"]*"]])
@objc class ObjCClass {
      @IBAction func click(_: AnyObject?) -> () {}
}
// DW_LANG_Swift = 0xa000 [FIXME: this number will change!]
// CHECK-DAG: ![[TY1:[0-9]+]] = !MDCompositeType(tag: DW_TAG_structure_type, name: "SwiftClass",{{.*}} line: [[@LINE+1]],{{.*}} runtimeLang: DW_LANG_Swift
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

// CHECK-DAG: !MDGlobalVariable(name: "strongRef0",{{.*}}line: [[@LINE+1]],{{.*}} type: ![[TY0]],{{.*}} isLocal: false, isDefinition: true
var strongRef0 : ObjCClass
var strongRef1 : SwiftClass = SwiftClass()

// CHECK-DAG: !MDDerivedType(tag: DW_TAG_typedef, name: "_TtXwGSqC10attributes10SwiftClass_",{{.*}} line: [[@LINE+1]]
weak var    weakRef1    : SwiftClass? = strongRef1
// CHECK-DAG: !MDDerivedType(tag: DW_TAG_typedef, name: "_TtXoC10attributes10SwiftClass",{{.*}} line: [[@LINE+1]]
unowned var unownedRef1 : SwiftClass

protocol Protocol1 : class {
  func foo(x: Float) -> Float
}

class Implementation : Protocol1 {
  func foo(x: Float) -> Float { return 2*x }
}
