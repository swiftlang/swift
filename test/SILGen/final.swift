// RUN: %swift -emit-silgen %s | FileCheck %s

class TestClass {

  @final
  var finalProperty : Int { return 42 }

  @final
  func finalMethod() -> Int { return 12 }

}


// CHECK-LABEL: sil @{{.*}}testDirectDispatch
// CHECK-NEXT: bb0(%0 : $TestClass):
// CHECK: [[FINALMETH:%[0-9]+]] = function_ref @_TFC5final9TestClass11finalMethodfS0_FT_Si
// CHECK: apply [[FINALMETH]](%0)

// CHECK: [[FINALPROP:%[0-9]+]] = function_ref @_TFC5final9TestClassg13finalPropertySi
// CHECK: apply [[FINALPROP]](%0)
func testDirectDispatch(c : TestClass) -> Int {
  return c.finalMethod()+c.finalProperty
}
