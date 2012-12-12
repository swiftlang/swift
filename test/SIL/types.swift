// RUN: %swift -parse-as-library -dump-sil %s | FileCheck %s

class C {
  var member : Int

  // CHECK: func_decl foo
  func foo(x:Int) {
    // CHECK: bb0([[THIS:%[0-9]+]] : C, [[X:%[0-9]+]] : Int):
    member = x
    // FIXME don't box 'this'?
    // CHECK: [[THISBOX:%[0-9]+]] = alloc_box $C
    // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int
    // CHECK: [[X1:%[0-9]+]] = load [[XBOX]]#1
    // CHECK: [[THIS1:%[0-9]+]] = load [[THISBOX]]#1
    // CHECK: [[MEMBER:%[0-9]+]] = ref_element_addr [[THIS1]], 0
    // CHECK: store [[X1]] to [[MEMBER]]
  }
}

struct S {
  var member : Int

  // CHECK: func_decl foo
  func foo(x:Int) {
    // CHECK: bb0([[THIS:%[0-9]+]] : [byref] S, [[X:%[0-9]+]] : Int):
    member = x
    // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int
    // CHECK: [[X1:%[0-9]+]] = load [[XBOX]]#1
    // CHECK: [[MEMBER:%[0-9]+]] = element_addr [[THIS]], 0
    // CHECK: store [[X1]] to [[MEMBER]]
  }

  class SC {
    // CHECK: func_decl bar
    func bar() {}
  }
}

func f() {
  class FC {
    // CHECK: func_decl zim
    func zim() {}
  }
}
