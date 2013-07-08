// RUN: %swift -parse-as-library -emit-sil %s | FileCheck %s

func bar(x:Int) {}
func foo(x:Int, y:Bool) {}

func assignment(x : Int, y : Int) {
  x = 42
  y = 57
  
  (x, y) = (1,2)
}

// CHECK: sil @_T10statements10assignmentFT1xSi1ySi_T_
// CHECK: integer_literal $Builtin.Int128, 42
// CHECK: store
// CHECK: integer_literal $Builtin.Int128, 57
// CHECK: store

func if_test(x:Int, y:Bool) {
  if (y) {
   bar(x);
  }
  bar(x);
}

// CHECK: sil @_T10statements7if_testFT1xSi1ySb_T_

func if_else(x:Int, y:Bool) {
  if (y) {
   bar(x);
  } else {
   foo(x, y);
  }
  bar(x);
}

// CHECK: sil @_T10statements7if_elseFT1xSi1ySb_T_

func nested_if(x:Int, y:Bool, z:Bool) {
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

// CHECK: sil @_T10statements9nested_ifFT1xSi1ySb1zSb_T_

func nested_if_merge_noret(x:Int, y:Bool, z:Bool) {
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

// CHECK: sil @_T10statements21nested_if_merge_noretFT1xSi1ySb1zSb_T_

func nested_if_merge_ret(x:Int, y:Bool, z:Bool) -> Int {
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

// CHECK: sil @_T10statements19nested_if_merge_retFT1xSi1ySb1zSb_Si

func else_break(x:Int, y:Bool, z:Bool) {
  while z {
    if y {
    } else {
      break
    }
  }
}

// CHECK: sil @_T10statements10else_breakFT1xSi1ySb1zSb_T_

func loop_with_break(x:Int, y:Bool, z:Bool) -> Int {
  while (x > 2) {
   if (y) {
     bar(x);
     break;
   }
  }
}

// CHECK: sil @_T10statements15loop_with_breakFT1xSi1ySb1zSb_Si

func loop_with_continue(x:Int, y:Bool, z:Bool) -> Int {
  while (x > 2) {
    if (y) {
     bar(x);
     continue;
    }
    loop_with_break(x, y, z);
  }
  bar(x);
}

// CHECK: sil @_T10statements18loop_with_continueFT1xSi1ySb1zSb_Si

func do_loop_with_continue(x:Int, y:Bool, z:Bool) -> Int {
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

// CHECK: sil @_T10statements21do_loop_with_continueFT1xSi1ySb1zSb_Si


func for_loops(x:Int, c : Bool) {
  for i in 1..100 {
    println(i)
  }
  
  for ; x < 40;  {
   println(x)
   ++x
  }
  
  for var i = 0; i < 100; ++i {
  }
  
  return 
}
// CHECK: sil @_T10statements9for_loopsFT1xSi1cSb_T_

func void_return() {
  var b:Bool
  if b {
    return
  }
}
// CHECK: sil @_T10statements11void_returnFT_T_
// CHECK: condbranch {{%[0-9]+}}, [[BB1:bb[0-9]+]], [[BB2:bb[0-9]+]]
// CHECK: [[BB1]]:
// CHECK: [[R1:%[0-9]+]] = tuple ()
// CHECK: return [[R1]]
// CHECK: [[BB2]]:
// CHECK: [[R2:%[0-9]+]] = tuple ()
// CHECK: return [[R2]]

func foo() {}

// <rdar://problem/13549626>
// CHECK: sil @_T10statements14return_from_ifFT1aSb_Si
func return_from_if(a:Bool) -> Int {
  // CHECK: bb0(%0 : $Bool):
  // CHECK: condbranch {{.*}}, [[THEN:bb.*]], [[ELSE:bb.*]]
  if a {
    // CHECK: [[THEN]]:
    // CHECK: return
    return 1
  } else {
    // CHECK: [[ELSE]]:
    // CHECK: return
    return 0
  }
  // CHECK-NOT: function_ref @foo
  foo()
}
