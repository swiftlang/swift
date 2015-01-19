// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | FileCheck %s

func bar(x: Int) {}
func foo(x: Int, y: Bool) {}

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

func loop_with_break(x: Int, y: Bool, z: Bool) -> Int {
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
  do {
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


func for_loops(var x: Int, c: Bool) {
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
  
  return 
}
// CHECK-LABEL: sil hidden  @{{.*}}for_loops

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


