// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil %s | FileCheck %s

// FIXME: Not sure if this an ideal source info for the branch - 
// it points to if, not the last instruction in the block.
func ifexpr() -> Int {
  var x : Int = 0; 
  if true {
    x++; 
  }
  return x;
  // CHECK-LABEL: sil hidden  @_TF13sil_locations6ifexprFT_Si
  // CHECK: apply {{.*}} line:[[@LINE-5]]:6
  // CHECK: cond_br {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]] // {{.*}} line:[[@LINE-6]]:6
  // CHECK: br [[FALSE_BB]] // {{.*}} line:[[@LINE-5]]:3
  // CHECK: return {{.*}} // {{.*}} line:[[@LINE-5]]:3:return
}

func ifelseexpr() -> Int {
  var x : Int = 0; 
  if true {
    x++; 
  } else {
    x--;
  }
  return x;
  // CHECK-LABEL: sil hidden  @_TF13sil_locations10ifelseexprFT_Si
  // CHECK: cond_br {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]] // {{.*}} line:[[@LINE-7]]:6
  // CHECK: [[TRUE_BB]]:
  // CHECK: br bb{{[0-9]+}} // {{.*}} line:[[@LINE-7]]:3
  // CHECK: [[FALSE_BB]]:
  // CHECK: br bb{{[0-9]+}} // {{.*}} line:[[@LINE-7]]:3
  // CHECK: return {{.*}} // {{.*}} line:[[@LINE-7]]:3:return
}

// The source locations are handled differently here - since 
// the return is unified, we keep the location of the return(not the if) 
// in the branch.
func ifexpr_return() -> Int {
  if true {
    return 5; 
  }
  return 6;
  // CHECK-LABEL: sil hidden  @_TF13sil_locations13ifexpr_returnFT_Si
  // CHECK: apply {{.*}} line:[[@LINE-5]]:6
  // CHECK: cond_br {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]] // {{.*}} line:[[@LINE-6]]:6
  // CHECK: [[TRUE_BB]]:
  // CHECK: br bb{{[0-9]+}}({{%.*}}) // {{.*}} line:[[@LINE-7]]:5:return
  // CHECK: [[FALSE_BB]]:
  // CHECK: br bb{{[0-9]+}}({{%.*}}) // {{.*}} line:[[@LINE-7]]:3:return
  // CHECK: return {{.*}} // {{.*}} line:[[@LINE+1]]:1:cleanup
}

func ifexpr_rval() -> Int {
  var x = true ? 5 : 6;
  return x;
  // CHECK-LABEL: sil hidden  @_TF13sil_locations11ifexpr_rvalFT_Si
  // CHECK: apply {{.*}} line:[[@LINE-3]]:11
  // CHECK: cond_br {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]] // {{.*}} line:[[@LINE-4]]:11
  // CHECK: [[TRUE_BB]]:
  // CHECK: br bb{{[0-9]+}}({{%.*}}) // {{.*}} line:[[@LINE-6]]:18
  // CHECK: [[FALSE_BB]]:
  // CHECK: br bb{{[0-9]+}}({{%.*}}) // {{.*}} line:[[@LINE-8]]:22
}

// TODO: missing info on the first branch.
func forstmt_empty_cond(var i: Int) -> Int {
  for i=0;;++i {}
    // CHECK-LABEL: sil hidden  @{{.*}}forstmt_empty_cond{{.*}}
    // CHECK: apply {{.*}} line:[[@LINE-2]]:9
    // CHECK: br [[TRUE_BB:bb[0-9]+]]
    // CHECK: [[TRUE_BB:bb[0-9]+]]:
    // CHECK: br [[TRUE_BB:bb[0-9]+]] // {{.*}} line:[[@LINE-5]]:17
}

// --- Test function calls.
func simpleDirectCallTest(i: Int) -> Int {
  return simpleDirectCallTest(i)
  // CHECK-LABEL: sil hidden  @_TF13sil_locations20simpleDirectCallTest
  // CHECK: function_ref @_TF13sil_locations20simpleDirectCallTest{{.*}} line:[[@LINE-2]]:10
  // CHECK: {{%.*}} apply {{%.*}} line:[[@LINE-3]]:10
}

func templateTest<T>(value: T) -> T {
  return value
}
func useTemplateTest() -> Int {
  return templateTest(5);
  // CHECK-LABEL: sil hidden  @_TF13sil_locations15useTemplateTestFT_Si

  // CHECK: function_ref @_TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si{{.*}} line:87
}

func foo(x: Int) -> Int {
  func bar(y: Int) -> Int {
    return x + y
  }
  return bar(1)
  // CHECK-LABEL: sil hidden  @_TF13sil_locations3foo
  // CHECK: [[CLOSURE:%[0-9]+]] = function_ref {{.*}} line:[[@LINE-5]]:8
  // CHECK: partial_apply [[CLOSURE:%[0-9]+]]
}

class LocationClass {
  func mem() {}
}
func testMethodCall() {
  var l: LocationClass;
  l.mem();
  // CHECK-LABEL: sil hidden  @_TF13sil_locations14testMethodCallFT_T_
  
  // CHECK: class_method {{.[0-9]+}} : $LocationClass, #LocationClass.mem!1 {{.*}} line:[[@LINE-3]]:5
}

func multipleReturnsImplicitAndExplicit() {
  var x = 5+3;
  if x > 10 {
    return;
  }
  x++;
  // CHECK-LABEL: sil hidden  @_TF13sil_locations34multipleReturnsImplicitAndExplicitFT_T_
  // CHECK: cond_br
  // CHECK: br bb{{[0-9]+}} // {{.*}} line:[[@LINE-5]]:5:return
  // CHECK: br bb{{[0-9]+}} // {{.*}} line:[[@LINE+2]]:1:imp_return
  // CHECK: return {{.*}} // {{.*}} line:[[@LINE+1]]:1:cleanup
}

func simplifiedImplicitReturn() -> () {
  var y = 0 
  // CHECK-LABEL: sil hidden  @_TF13sil_locations24simplifiedImplicitReturnFT_T_
  // CHECK: return {{.*}} // {{.*}} line:[[@LINE+1]]:1:imp_return
}

func switchfoo() -> Int { return 0 }
func switchbar() -> Int { return 0 }

// CHECK-LABEL: sil hidden @_TF13sil_locations10testSwitchFT_T_
func testSwitch() {
  var x:Int
  x = 0
  switch (switchfoo(), switchbar()) {
  // CHECK: store {{.*}}  // {{.*}} line:[[@LINE+1]]
  case (1,2):
  // CHECK: integer_literal $Builtin.Int2048, 2    // {{.*}} line:[[@LINE-1]]:11
  // FIXME: Location info is missing.
  // CHECK: cond_br
  //
    var z: Int = 200
  // CHECK: [[VAR_Z:%[0-9]+]] = alloc_box $Int     // {{.*}} line:[[@LINE-1]]:9
  // CHECK: integer_literal $Builtin.Int2048, 200  // {{.*}} line:[[@LINE-2]]:18
    x = z
  // CHECK:  strong_release [[VAR_Z]]{{.*}}        // {{.*}} line:[[@LINE-1]]:9:cleanup
  case (3, var y):
    x++
  }
}

func testIf() {
  if true {
    var y:Int;
  } else {
    var x:Int;
  }
  // CHECK-LABEL: sil hidden @_TF13sil_locations6testIfFT_T_
  //
  // FIXME: Missing location info here.
  // CHECK: function_ref
  // CHECK: apply
  // 
  //
  //
  // CHECK: br {{.*}}                                            // {{.*}} line:[[@LINE-13]]:6



}

func testFor() {
  for (var i:Int = 0; i<10; i++) {
    var y: Int = 300;
    y++;
    if true {
      break;
    }
    y--;
    continue;
  }

  // CHECK-LABEL: sil hidden @_TF13sil_locations7testForFT_T_
  // CHECK: [[VAR_Y_IN_FOR:%[0-9]+]]  = alloc_box $Int                 // {{.*}} line:[[@LINE-10]]:9
  // CHECK: integer_literal $Builtin.Int2048, 300                        // {{.*}} line:[[@LINE-11]]:18
  // CHECK: strong_release [[VAR_Y_IN_FOR]]#0 : $Builtin.NativeObject   // {{.*}} line:[[@LINE-5]]:3:cleanup
  // CHECK: br bb{{.*}}                                                  // {{.*}} line:[[@LINE-10]]:7
  // CHECK: strong_release [[VAR_Y_IN_FOR]]#0 : $Builtin.NativeObject   // {{.*}} line:[[@LINE-7]]:3:cleanup
  // CHECK: br bb{{.*}}                                                  // {{.*}} line:[[@LINE-9]]:5
  
  
}

func testTuples() {
  var t = (2,3)
  var tt = (2, (4,5))
  var d = "foo"
  // CHECK-LABEL: sil hidden @_TF13sil_locations10testTuplesFT_T_


  // CHECK: tuple_element_addr                                       {{.*}} line:[[@LINE-6]]:11
  // CHECK: integer_literal $Builtin.Int2048, 2                      {{.*}} line:[[@LINE-7]]:12
  // CHECK: integer_literal $Builtin.Int2048, 3                      {{.*}} line:[[@LINE-8]]:14
  // CHECK: tuple_element_addr                                       {{.*}} line:[[@LINE-8]]:12
  // CHECK: tuple_element_addr                                       {{.*}} line:[[@LINE-9]]:16  
}

// Test tuple emploding/exploding.
protocol Ordinable {
  func ord() -> Int
}

func b<T : Ordinable>(seq: T) -> (Int) -> Int {
  return {i in i + seq.ord() }
}

func captures_tuple<T, U>(x: (T, U)) -> () -> (T, U) {
  return {x}
  // CHECK-LABEL: sil hidden @_TF13sil_locations14captures_tuple


  // CHECK: tuple_element_addr                                    {{.*}} line:[[@LINE-5]]:27
  // CHECK: copy_addr [take]                                      {{.*}} line:[[@LINE-6]]:27
  // CHECK: function_ref                                          {{.*}} line:[[@LINE-6]]:10

  
  // CHECK-LABEL: sil shared @_TFF13sil_locations14captures_tuple
  // CHECK: copy_addr                                             {{.*}} line:[[@LINE-10]]:11
}

func interpolated_string(x: Int, y: String) -> String {
  return "The \(x) Million Dollar \(y)"
  // CHECK-LABEL: sil hidden @_TF13sil_locations19interpolated_string



  // CHECK: retain_value{{.*}}                                         {{.*}} line:[[@LINE-5]]:37


}


func int(x: Int) {}
func tuple() -> (Int, Float) { return (1, 1.0) }  
func tuple_element(x: (Int, Float)) {
  int(tuple().0)
  // CHECK-LABEL: sil hidden @_TF13sil_locations13tuple_element

  // CHECK: apply                                                    {{.*}} line:[[@LINE-3]]:7
  // CHECK: tuple_extract{{.*}}, 0                                   {{.*}} line:[[@LINE-4]]:7
  // CHECK: tuple_extract{{.*}}, 1                                   {{.*}} line:[[@LINE-5]]:7
  // CHECK: apply                                                    {{.*}} line:[[@LINE-6]]:3
     
}

func containers() -> ([Int], Dictionary<String, Int>) {
  return ([1, 2, 3], ["Ankeny": 1, "Burnside": 2, "Couch": 3])
  // CHECK-LABEL: sil hidden @_TF13sil_locations10containers
  // CHECK: apply {{%.*}}<(String, Int)>({{%.*}})            {{.*}} line:[[@LINE-2]]:22
  
  // CHECK: string_literal utf8 "Ankeny"                             {{.*}} line:[[@LINE-4]]:23

  // CHECK: integer_literal $Builtin.Int2048, 1                      {{.*}} line:[[@LINE-6]]:33
  // CHECK: integer_literal $Builtin.Int2048, 2                      {{.*}} line:[[@LINE-7]]:48

  
  
}


func a() {}
func b() -> Int { return 0 }
protocol P { func p() }
struct X : P { func p() {} }
func test_isa_2(p: P) {
  switch (p, b()) {
  case (is X, b()):
    a()
  case _:
    a()
  }
  


  // CHECK-LABEL: sil hidden @_TF13sil_locations10test_isa_2
  // CHECK: alloc_stack $(P, Int)                                  {{.*}} line:[[@LINE-10]]:10
  // CHECK: tuple_element_addr{{.*}} $*(P, Int), 0                 {{.*}} line:[[@LINE-11]]:10
  // CHECK: tuple_element_addr{{.*}} $*(P, Int), 1                 {{.*}} line:[[@LINE-12]]:10
  // CHECK: load                                                     {{.*}} line:[[@LINE-12]]:8
  //
  // CHECK: checked_cast_addr_br                                     {{.*}} line:[[@LINE-14]]:9
  // CHECK: load                                                     {{.*}} line:[[@LINE-15]]:9
    
}

protocol Runcible {
  func runce()
}
enum SinglePayloadAddressOnly {
  case x(Runcible)
  case y
}
func printSinglePayloadAddressOnly(v:SinglePayloadAddressOnly) {
  switch v {
  case .x(var runcible):
    runcible.runce()
  case .y:
    println("Why?")
  }
  
  
  // CHECK_LABEL: sil hidden @_TF13sil_locations29printSinglePayloadAddressOnly
  // CHECK: bb0
  // CHECK: switch_enum_addr {{.*}} [[FALSE_BB:bb[0-9]+]] // {{.*}} line:[[@LINE-10]]:3
  // CHECK: [[FALSE_BB]]:

}


func testStringForEachStmt() {
  var i = 0;
  for index in 1..<20 {
    i++
    if i == 15 {
      break
    }
  }
  
  // CHECK-LABEL: sil hidden @_TF13sil_locations21testStringForEachStmtFT_T_
  // CHECK: br         {{.*}} line:[[@LINE-8]]:3
  // CHECK: cond_br {{.*}} line:[[@LINE-9]]:3
  // CHECK: cond_br {{.*}} line:[[@LINE-8]]:8
  // Break branch:
  // CHECK: br         {{.*}} line:[[@LINE-9]]:7
  // Looping back branch:
  // CHECK: br         {{.*}} line:[[@LINE-9]]:3
  // Condition is false branch:
  // CHECK: br         {{.*}} line:[[@LINE-16]]:3
  
  
  
  
}


func testForStmt() {
  var i = 0;
  var m = 0;
  for (i = 0; i < 10; ++i) {
    m++
    if m == 15 {
      break
    } else {
      continue
    }

  }


  // CHECK-LABEL: sil hidden @_TF13sil_locations11testForStmtFT_T_
  // CHECK: br         {{.*}} line:[[@LINE-12]]:3
  // CHECK: cond_br {{.*}} line:[[@LINE-13]]:15
  // CHECK: cond_br {{.*}} line:[[@LINE-12]]:8
  // Break branch:
  // CHECK: br         {{.*}} line:[[@LINE-13]]:7
  // Continue branch:
  // CHECK: br         {{.*}} line:[[@LINE-13]]:7
  // Looping back branch:
  // CHECK: br         {{.*}} line:[[@LINE-12]]:3
  // Condition is false branch:
  // CHECK: br         {{.*}} line:[[@LINE-22]]:3

}


func testRepeatWhile() {
  var m = 0;
  repeat {
    m++
  } while (m < 200)
  
  
  // CHECK-LABEL: sil hidden @_TF13sil_locations15testRepeatWhileFT_T_
  // CHECK: br         {{.*}} line:[[@LINE-6]]:3
  // CHECK: cond_br {{.*}} line:[[@LINE-5]]:11
  // Loop back branch:
  // CHECK: br         {{.*}} line:[[@LINE-7]]:11  
}



func testWhile() {
  var m = 0;
  while m < 100 {
    m++
    if m > 5 {
      break
    }
    m++
  }
  
  // CHECK-LABEL: sil hidden @_TF13sil_locations9testWhileFT_T_
  // CHECK: br         {{.*}} line:[[@LINE-9]]:3
  // While loop conditional branch:
  // CHECK: cond_br {{.*}} line:[[@LINE-11]]:9
  // If stmt condition branch:
  // CHECK: cond_br {{.*}} line:[[@LINE-11]]:8
  // Break branch:
  // CHECK: br         {{.*}} line:[[@LINE-12]]:7
  // Looping back branch:
  // CHECK: br         {{.*}} line:[[@LINE-11]]:3


  
}
