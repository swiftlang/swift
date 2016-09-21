// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class TestClass {

  final
  var finalProperty : Int { return 42 }

  final
  func finalMethod() -> Int { return 12 }


  func baseMethod() {}
}

class TestDerived : TestClass {
  final
  override func baseMethod() {}
}


// CHECK-LABEL: sil hidden @{{.*}}testDirectDispatch
// CHECK: bb0(%0 : $TestClass):
// CHECK: [[FINALMETH:%[0-9]+]] = function_ref @_TFC5final9TestClass11finalMethod
// CHECK: apply [[FINALMETH]](%0)

// CHECK: [[FINALPROP:%[0-9]+]] = function_ref @_TFC5final9TestClassg13finalPropertySi
// CHECK: apply [[FINALPROP]](%0)
func testDirectDispatch(c : TestClass) -> Int {
  return c.finalMethod()+c.finalProperty
}


// Verify that the non-overriding final methods don't get emitted to the vtable.
// CHECK-LABEL: sil_vtable TestClass {
// CHECK-NEXT:  #TestClass.baseMethod!1: _TFC5final9TestClass10baseMethod
// CHECK-NEXT:  #TestClass.deinit!
// CHECK-NEXT:  #TestClass.init!initializer.1: _TFC5final9TestClassc
// CHECK-NEXT: }

// Verify that overriding final methods don't get emitted to the vtable.
// CHECK-LABEL: sil_vtable TestDerived {
// CHECK-NEXT:  #TestClass.baseMethod!1: _TFC5final11TestDerived10baseMethod
// CHECK-NEXT:  #TestClass.init!initializer.1: _TFC5final11TestDerivedc
// CHECK-NEXT:  #TestDerived.deinit!
// CHECK-NEXT: }
