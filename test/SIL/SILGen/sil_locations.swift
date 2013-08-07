// RUN: %swift -emit-silgen -v %s | FileCheck %s

// FIXME: Not sure if this an ideal source info for the branch - 
// it points to if, not the last instruction in the block.
func ifexpr () -> Int {
  var x : Int = 0; 
  if true {
    x++; 
  }
  return x;
  // CHECK: sil @_T13sil_locations6ifexprFT_Si
  // CHECK: apply {{.*}} line:7:6
  // CHECK: condbranch {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]] // line:7:3
  // CHECK: br [[FALSE_BB]] // line:7:3
  // CHECK: return {{.*}} // line:10:3
}

func ifelseexpr () -> Int {
  var x : Int = 0; 
  if true {
    x++; 
  } else {
    x--;
  }
  return x;
  // CHECK: sil @_T13sil_locations10ifelseexprFT_Si
  // CHECK: condbranch {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]] // line:20:3
  // CHECK: [[TRUE_BB]]:
  // CHECK: br bb{{[0-9]+}} // line:20:3
  // CHECK: [[FALSE_BB]]:
  // CHECK: br bb{{[0-9]+}} // line:20:3
  // CHECK: return {{.*}} // line:25:3
}

// The source locations are handled differently here - since 
// the return is unified, we keep the location of the return(not the if) 
// in the branch.
func ifexpr_return () -> Int {
  if true {
    return 5; 
  }
  return 6;
  // CHECK: sil @_T13sil_locations13ifexpr_returnFT_Si
  // CHECK: apply {{.*}} line:39:6
  // CHECK: condbranch {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]] // line:39:3
  // CHECK: [[TRUE_BB]]:
  // CHECK: br bb{{[0-9]+}}({{%.*}}) // line:40:5
  // CHECK: [[FALSE_BB]]:
  // CHECK: br bb{{[0-9]+}}({{%.*}}) // line:42:3
  // CHECK: return {{.*}} // line:42:3
}

func ifexpr_rval () -> Int {
  var x = true ? 5 : 6;
  return x;
  // CHECK: sil @_T13sil_locations11ifexpr_rvalFT_Si
  // CHECK: apply {{.*}} line:54:11
  // CHECK: condbranch {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]] // line:54:16
  // CHECK: [[TRUE_BB]]:
  // CHECK: br bb{{[0-9]+}}({{%.*}}) // line:54:16
  // CHECK: [[FALSE_BB]]:
  // CHECK: br bb{{[0-9]+}}({{%.*}}) // line:54:16
}

// TODO: missing info on the first branch.
func forstmt_empty_cond (i : Int) -> Int {
  for i=0;;++i {}
    // CHECK: sil @_T13sil_locations18forstmt_empty_condFT1iSi_Si
    // CHECK: apply {{.*}} line:67:9
    // CHECK: br [[TRUE_BB:bb[0-9]+]]
    // CHECK: [[TRUE_BB:bb[0-9]+]]:
    // CHECK: br [[TRUE_BB:bb[0-9]+]] // line:67:3
}

