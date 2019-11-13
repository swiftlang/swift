// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o /dev/null -disable-objc-attr-requires-foundation-module -enable-objc-interop -emit-module-interface-path %t/dynamic_accessors.swiftinterface %s
// RUN: %target-swift-emit-silgen -disable-objc-attr-requires-foundation-module -enable-objc-interop %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -disable-objc-attr-requires-foundation-module -enable-objc-interop %t/dynamic_accessors.swiftinterface | %FileCheck %s

public class MyObjCClass {
  @objc public dynamic var a: Int {
    // CHECK-LABEL: sil [thunk] [ossa] @$s17dynamic_accessors11MyObjCClassC1aSivgTo : $@convention(objc_method) (MyObjCClass) -> Int {
    // CHECK:  function_ref @$s17dynamic_accessors11MyObjCClassC1aSivg
    // CHECK: }
    get { return 4 }

    // CHECK-LABEL: sil [thunk] [ossa] @$s17dynamic_accessors11MyObjCClassC1aSivsTo : $@convention(objc_method) (Int, MyObjCClass) -> () {
    // CHECK: function_ref @$s17dynamic_accessors11MyObjCClassC1aSivs
    // CHECK: }
    set {}
  }

  @objc public dynamic subscript(x: Int) -> Int {
    // CHECK-LABEL: sil [thunk] [ossa] @$s17dynamic_accessors11MyObjCClassCyS2icigTo : $@convention(objc_method) (Int, MyObjCClass) -> Int {
    // CHECK: function_ref @$s17dynamic_accessors11MyObjCClassCyS2icig
    // CHECK: }
    get { return x }

    // CHECK-LABEL: sil [thunk] [ossa] @$s17dynamic_accessors11MyObjCClassCyS2icisTo : $@convention(objc_method) (Int, Int, MyObjCClass) -> () {
    // CHECK: function_ref @$s17dynamic_accessors11MyObjCClassCyS2icis
    // CHECK: }
    set {}
  }

  public init() {}
}

@inlinable public func foo() {
  let x = MyObjCClass()
  // CHECK: objc_method %{{[0-9]+}} : $MyObjCClass, #MyObjCClass.a!getter.1.foreign
  // CHECK: objc_method %{{[0-9]+}} : $MyObjCClass, #MyObjCClass.a!setter.1.foreign
  x.a = x.a + 1

  // CHECK: objc_method %{{[0-9]+}} : $MyObjCClass, #MyObjCClass.subscript!getter.1.foreign
  // CHECK: objc_method %{{[0-9]+}} : $MyObjCClass, #MyObjCClass.subscript!setter.1.foreign
  x[4] = x[5]
}
