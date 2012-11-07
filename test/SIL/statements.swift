// RUN: %swift -dump-cfg %s | FileCheck %s

func bar(x:Int)
func foo(x:Int, y:Bool);

func assignment(x : Int, y : Int) {
  x = 42
  y = 57
  
  (x, y) = (1,2)
}

// CHECK: func_decl assignment
// CHECK: integerliteral 42, width=64
// CHECK: store
// CHECK: integerliteral 57, width=64
// CHECK: store

func if_test(x:Int, y:Bool) {
  if (y) {
   bar(x);
  }
  bar(x);
}

// CHECK: func_decl if_test

func if_else(x:Int, y:Bool) {
  if (y) {
   bar(x);
  } else {
   foo(x, y);
  }
  bar(x);
}

// CHECK: func_decl if_else

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

// CHECK: func_decl nested_if

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

// CHECK: func_decl nested_if_merge_noret

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

// CHECK: func_decl nested_if_merge_ret

func loop_with_break(x:Int, y:Bool, z:Bool) -> Int {
  while (x > 2) {
   if (y) {
     bar(x);
     break;
   }
  }
}

// CHECK: func_decl loop_with_break

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

// CHECK: func_decl loop_with_continue

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

// CHECK: func_decl do_loop_with_continue


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
  
  return ()
}
// CHECK: func_decl for_loops


