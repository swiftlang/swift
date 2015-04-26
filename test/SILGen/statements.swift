// RUN: %target-swift-frontend -parse-as-library -emit-silgen -verify %s | FileCheck %s

class MyClass { 
  func foo() { }
}

var global_cond: Bool = false

func bar(x: Int) {}
func foo(x: Int, _ y: Bool) {}

func assignment(var x: Int, var y: Int) {
  x = 42
  y = 57
  
  (x, y) = (1,2)
}

// CHECK-LABEL: sil hidden  @{{.*}}assignment
// CHECK: integer_literal $Builtin.Int2048, 42
// CHECK: assign
// CHECK: integer_literal $Builtin.Int2048, 57
// CHECK: assign

func if_test(x: Int, y: Bool) {
  if (y) {
   bar(x);
  }
  bar(x);
}

// CHECK-LABEL: sil hidden  @_TF10statements7if_test

func if_else(x: Int, y: Bool) {
  if (y) {
   bar(x);
  } else {
   foo(x, y);
  }
  bar(x);
}

// CHECK-LABEL: sil hidden  @_TF10statements7if_else

func nested_if(x: Int, y: Bool, z: Bool) {
  if (y) {
    if (z) {
      bar(x);
    }
  } else {
    if (z) {
      foo(x, y);
    }
  }
  bar(x);
}

// CHECK-LABEL: sil hidden  @_TF10statements9nested_if

func nested_if_merge_noret(x: Int, y: Bool, z: Bool) {
  if (y) {
    if (z) {
      bar(x);
    }
  } else {
    if (z) {
      foo(x, y);
    }
  }
}

// CHECK-LABEL: sil hidden  @_TF10statements21nested_if_merge_noret

func nested_if_merge_ret(x: Int, y: Bool, z: Bool) -> Int {
  if (y) {
    if (z) {
      bar(x);
    }
    return 1;
  } else {
    if (z) {
      foo(x, y);
    }
  }
  return 2;
}

// CHECK-LABEL: sil hidden  @_TF10statements19nested_if_merge_ret

func else_break(x: Int, y: Bool, z: Bool) {
  while z {
    if y {
    } else {
      break
    }
  }
}

// CHECK-LABEL: sil hidden  @_TF10statements10else_break

func loop_with_break(x: Int, _ y: Bool, _ z: Bool) -> Int {
  while (x > 2) {
   if (y) {
     bar(x);
     break;
   }
  }
}

// CHECK-LABEL: sil hidden  @_TF10statements15loop_with_break

func loop_with_continue(x: Int, y: Bool, z: Bool) -> Int {
  while (x > 2) {
    if (y) {
     bar(x);
     continue;
    }
    loop_with_break(x, y, z);
  }
  bar(x);
}

// CHECK-LABEL: sil hidden  @_TF10statements18loop_with_continue

func do_loop_with_continue(x: Int, y: Bool, z: Bool) -> Int {
  repeat {
    if (x < 42) {
     bar(x);
     continue;
    }
    loop_with_break(x, y, z);
  }
  while (x > 2);
  bar(x);
}

// CHECK-LABEL: sil hidden  @_TF10statements21do_loop_with_continue 


// CHECK-LABEL: sil hidden  @{{.*}}for_loops1
func for_loops1(var x: Int, c: Bool) {
  for i in 1..<100 {
    println(i)
  }
  
  for ; x < 40;  {
   println(x)
   ++x
  }
  
  for var i = 0; i < 100; ++i {
  }
  
  for var i = 0; i < 100; i {
  }
}

// CHECK-LABEL: sil hidden  @{{.*}}for_loops2
func for_loops2() {
  // rdar://problem/19316670
  // CHECK: [[NEXT:%[0-9]+]] = function_ref @_TFVSs17IndexingGenerator4next
  // CHECK-NEXT: alloc_stack $Optional<MyClass>
  // CHECK-NEXT: apply [[NEXT]]<[MyClass]
  // CHECK: class_method [[OBJ:%[0-9]+]] : $MyClass, #MyClass.foo!1
  var objects = [MyClass(), MyClass() ]
  for obj in objects {
    obj.foo()
  }

  return 
}

func void_return() {
  var b:Bool
  if b {
    return
  }
}
// CHECK-LABEL: sil hidden  @_TF10statements11void_return
// CHECK: cond_br {{%[0-9]+}}, [[BB1:bb[0-9]+]], [[BB2:bb[0-9]+]]
// CHECK: [[BB1]]:
// CHECK:   br [[EPILOG:bb[0-9]+]]
// CHECK: [[BB2]]:
// CHECK:   br [[EPILOG]]
// CHECK: [[EPILOG]]:
// CHECK:   [[R:%[0-9]+]] = tuple ()
// CHECK:   return [[R]]

func foo() {}

// <rdar://problem/13549626>
// CHECK-LABEL: sil hidden  @_TF10statements14return_from_if
func return_from_if(a: Bool) -> Int {
  // CHECK: bb0(%0 : $Bool):
  // CHECK: cond_br {{.*}}, [[THEN:bb[0-9]+]], [[ELSE:bb[0-9]+]]
  if a {
    // CHECK: [[THEN]]:
    // CHECK: br [[EPILOG:bb[0-9]+]]({{%.*}})
    return 1
  } else {
    // CHECK: [[ELSE]]:
    // CHECK: br [[EPILOG]]({{%.*}})
    return 0
  }
  // CHECK-NOT: function_ref @foo
  // CHECK: [[EPILOG]]([[RET:%.*]] : $Int):
  // CHECK:   return [[RET]]
  foo()  // expected-warning {{will never be executed}}
}

class C {}

func use(c: C) {}

func for_each_loop(x: [C]) {
  for i in x {
    use(i)
  }
  var y = 0
}

// <rdar://problem/16650625>
func for_ignored_lvalue_init() {
  var i = 0
  for i; i < 10; ++i {}
}


// CHECK-LABEL: sil hidden @{{.*}}test_break
func test_break(i : Int) {
  switch i {
  case (let x) where x != 17: 
    if x == 42 { break } 
    print(x)
  default:
    break
  }
}


// <rdar://problem/19150249> Allow labeled "break" from an "if" statement

// CHECK-LABEL: sil hidden @_TF10statements13test_if_breakFGSqCS_1C_T_
func test_if_break(c : C?) {
label1:
  // CHECK: switch_enum %0 : $Optional<C>, case #Optional.Some!enumelt.1: [[TRUE:bb[0-9]+]], default [[FALSE:bb[0-9]+]]
  if let x? = c {
// CHECK: [[TRUE]]({{.*}} : $C):

    // CHECK: apply
    foo()

    // CHECK: strong_release
    // CHECK: br [[FALSE:bb[0-9]+]]
    break label1
    use(x)  // expected-warning {{will never be executed}}
  }

  // CHECK: [[FALSE]]:
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF10statements18test_if_else_breakFGSqCS_1C_T_
func test_if_else_break(c : C?) {
label2:
  // CHECK: switch_enum %0 : $Optional<C>, case #Optional.Some!enumelt.1: [[TRUE:bb[0-9]+]], default [[FALSE:bb[0-9]+]]
  if let x? = c {
    // CHECK: [[TRUE]]({{.*}} : $C):
    use(x)
    // CHECK: br [[CONT:bb[0-9]+]]
  } else {
    // CHECK: [[FALSE]]:
    // CHECK: apply
    // CHECK: br [[CONT]]
    foo()
    break label2
    foo() // expected-warning {{will never be executed}}
  }
  // CHECK: [[CONT]]:
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF10statements23test_if_else_then_breakFTSbGSqCS_1C__T_
func test_if_else_then_break(a : Bool, _ c : C?) {
  label3:
  // CHECK: switch_enum %1 : $Optional<C>, case #Optional.Some!enumelt.1: [[TRUE:bb[0-9]+]], default [[FALSE:bb[0-9]+]]
  if let x? = c {
    // CHECK: [[TRUE]]({{.*}} : $C):
    use(x)
    // CHECK: br [[CONT:bb[0-9]+]]
  } else if a {
    // CHECK: [[FALSE]]:
    // CHECK: cond_br {{.*}}, [[TRUE2:bb[0-9]+]], [[FALSE2:bb[0-9]+]]
    // CHECK: apply
    // CHECK: br [[CONT]]
    foo()
    break label3
    foo()    // expected-warning {{will never be executed}}
  }

  // CHECK: [[FALSE2]]:
  // CHECK: br [[CONT]]
  // CHECK: [[CONT]]:
  // CHECK: return


}


// CHECK-LABEL: sil hidden @_TF10statements13test_if_breakFSbT_
func test_if_break(a : Bool) {
  // CHECK: br [[LOOP:bb[0-9]+]]
  // CHECK: [[LOOP]]:
  // CHECK: function_ref @_TFSb21_getBuiltinLogicValuefSbFT_Bi1_
  // CHECK-NEXT: apply
  // CHECK-NEXT: cond_br {{.*}}, [[LOOPTRUE:bb[0-9]+]], [[OUT:bb[0-9]+]]
  while a {
    if a {
      foo()
      break  // breaks out of while, not if.
    }
    foo()
  }

  // CHECK: [[LOOPTRUE]]:
  // CHECK: function_ref @_TFSb21_getBuiltinLogicValuefSbFT_Bi1_
  // CHECK-NEXT: apply
  // CHECK-NEXT: cond_br {{.*}}, [[IFTRUE:bb[0-9]+]], [[IFFALSE:bb[0-9]+]]

  // [[IFTRUE]]:
  // CHECK: function_ref statements.foo
  // CHECK: br [[OUT]]

  // CHECK: [[IFFALSE]]:
  // CHECK: function_ref statements.foo
  // CHECK: br [[LOOP]]

  // CHECK: [[OUT]]:
  // CHECK:   return
}

// rdar://problem/18643692
func for_loop_multi_iter() {
  for (var i = 0, x = 0; i < 10; i++, x) { }
}

// CHECK-LABEL: sil hidden @_TF10statements7test_doFT_T_
func test_do() {
  // CHECK: [[BAR:%.*]] = function_ref @_TF10statements3barFSiT_
  // CHECK: integer_literal $Builtin.Int2048, 0
  // CHECK: apply [[BAR]](
  bar(0)
  // CHECK-NOT: br bb
  do {
    // CHECK: [[CTOR:%.*]] = function_ref @_TFC10statements7MyClassCfMS0_FT_S0_
    // CHECK: [[OBJ:%.*]] = apply [[CTOR]](
    let obj = MyClass()

    // CHECK: [[BAR:%.*]] = function_ref @_TF10statements3barFSiT_
    // CHECK: integer_literal $Builtin.Int2048, 1
    // CHECK: apply [[BAR]](
    bar(1)

    // CHECK-NOT: br bb
    // CHECK: strong_release [[OBJ]]
    // CHECK-NOT: br bb
  }

  // CHECK: [[BAR:%.*]] = function_ref @_TF10statements3barFSiT_
  // CHECK: integer_literal $Builtin.Int2048, 2
  // CHECK: apply [[BAR]](
  bar(2)
}

// CHECK-LABEL: sil hidden @_TF10statements15test_do_labeledFT_T_
func test_do_labeled() {
  // CHECK: [[BAR:%.*]] = function_ref @_TF10statements3barFSiT_
  // CHECK: integer_literal $Builtin.Int2048, 0
  // CHECK: apply [[BAR]](
  bar(0)
  // CHECK: br bb1
  // CHECK: bb1:
  lbl: do {
    // CHECK: [[CTOR:%.*]] = function_ref @_TFC10statements7MyClassCfMS0_FT_S0_
    // CHECK: [[OBJ:%.*]] = apply [[CTOR]](
    let obj = MyClass()

    // CHECK: [[BAR:%.*]] = function_ref @_TF10statements3barFSiT_
    // CHECK: integer_literal $Builtin.Int2048, 1
    // CHECK: apply [[BAR]](
    bar(1)

    // CHECK: [[GLOBAL:%.*]] = function_ref @_TF10statementsau11global_condSb
    // CHECK: cond_br {{%.*}}, bb2, bb3
    if (global_cond) {
      // CHECK: bb2:
      // CHECK: strong_release [[OBJ]]
      // CHECK: br bb1
      continue lbl
    }

    // CHECK: bb3:
    // CHECK: [[BAR:%.*]] = function_ref @_TF10statements3barFSiT_
    // CHECK: integer_literal $Builtin.Int2048, 2
    // CHECK: apply [[BAR]](
    bar(2)

    // CHECK: [[GLOBAL:%.*]] = function_ref @_TF10statementsau11global_condSb
    // CHECK: cond_br {{%.*}}, bb4, bb5
    if (global_cond) {
      // CHECK: bb4:
      // CHECK: strong_release [[OBJ]]
      // CHECK: br bb6
      break lbl
    }

    // CHECK: bb5:
    // CHECK: [[BAR:%.*]] = function_ref @_TF10statements3barFSiT_
    // CHECK: integer_literal $Builtin.Int2048, 3
    // CHECK: apply [[BAR]](
    bar(3)

    // CHECK: strong_release [[OBJ]]
    // CHECK: br bb6
  }

  // CHECK: [[BAR:%.*]] = function_ref @_TF10statements3barFSiT_
  // CHECK: integer_literal $Builtin.Int2048, 4
  // CHECK: apply [[BAR]](
  bar(4)
}


func callee1() {}
func callee2() {}
func callee3() {}

// CHECK-LABEL: sil hidden @_TF10statements11defer_test1FT_T_
func defer_test1() {
  defer { callee1() }
  defer { callee2() }
  callee3()
  
  // CHECK: [[C1:%.*]] = function_ref @{{.*}}_TFF10statements11defer_test1FT_T_U_FT_T_
  // CHECK: [[C1T:%.*]] = thin_to_thick_function [[C1]]
  // CHECK: [[C2:%.*]] = function_ref @{{.*}}_TFF10statements11defer_test1FT_T_U0_FT_T_
  // CHECK: [[C2T:%.*]] = thin_to_thick_function [[C2]]
  // CHECK: [[C3:%.*]] = function_ref @{{.*}}callee3FT_T_
  // CHECK: apply [[C3]]
  // CHECK: apply [[C2T]]
  // CHECK: apply [[C1T]]
}
// CHECK: sil shared @_TFF10statements11defer_test1FT_T_U_FT_T_
// CHECK: function_ref @{{.*}}callee1FT_T_

// CHECK: sil shared @_TFF10statements11defer_test1FT_T_U0_FT_T_
// CHECK: function_ref @{{.*}}callee2FT_T_

// CHECK-LABEL: sil hidden @_TF10statements11defer_test2FSbT_
func defer_test2(cond : Bool) {
  // CHECK: [[C3:%.*]] = function_ref @{{.*}}callee3FT_T_
  // CHECK: apply [[C3]]
  // CHECK: br [[LOOP:bb[0-9]+]]
  callee3()
  
// CHECK: [[LOOP]]:
// test the condition.
// CHECK:  [[CONDTRUE:%.*]] = apply {{.*}}(%0)
// CHECK: cond_br [[CONDTRUE]], [[BODY:bb[0-9]+]], [[EXIT:bb[0-9]+]]
  while cond {
// CHECK: [[BODY]]:
  // CHECK: [[C1:%.*]] = function_ref @_TFF10statements11defer_test2FSbT_U_FT_T_
  // CHECK: [[C1T:%.*]] = thin_to_thick_function [[C1]]
  // CHECK: [[C2:%.*]] = function_ref @{{.*}}callee2FT_T_
  // CHECK: apply [[C2]]

  // CHECK: apply [[C1T]]
  // CHECK: br [[EXIT]]
    defer { callee1() }
    callee2()
    break
  }
  
// CHECK: [[EXIT]]:
// CHECK: [[C3:%.*]] = function_ref @{{.*}}callee3FT_T_
// CHECK: apply [[C3]]

  callee3()
}





