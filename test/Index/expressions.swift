// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

protocol P1 {}

// CHECK: [[@LINE+1]]:8 | struct/Swift | S1 | [[S1_USR:.*]] | Def
struct S1 : P1 {}

func test(_ o: P1?) {
  switch o {
  // CHECK-NOT: [[@LINE+2]]:17 | enumerator/Swift | some |
  // CHECK: [[@LINE+1]]:17 | struct/Swift | S1 | [[S1_USR]] | Ref
  case let s as S1:
    test(s)
  default:
    test(o)
  }
}

func foo() {} // CHECK: [[@LINE]]:6 | function/Swift | foo() | [[foo_USR:.*]] | Def

func test1() { // CHECK: [[@LINE]]:6 | function/Swift | test1() | [[test1_USR:.*]] | Def
  func local_func() {
    // FIXME: Saying that 'test1' is the caller of 'foo' is inaccurate, but we'd need
    // to switch to recording local symbols in order to model this properly.
    foo() // CHECK: [[@LINE]]:5 | function/Swift | foo() | [[foo_USR]] | Ref,Call,RelCall,RelCont | rel: 1
      // CHECK-NEXT: RelCall,RelCont | function/Swift | test1() | [[test1_USR]]
  }

  var local_prop: Int {
    get {
      foo() // CHECK: [[@LINE]]:7 | function/Swift | foo() | [[foo_USR]] | Ref,Call,RelCall,RelCont | rel: 1
        // CHECK-NEXT: RelCall,RelCont | function/Swift | test1() | [[test1_USR]]
      return 0
    }
    set {
      foo() // CHECK: [[@LINE]]:7 | function/Swift | foo() | [[foo_USR]] | Ref,Call,RelCall,RelCont | rel: 1
        // CHECK-NEXT: RelCall,RelCont | function/Swift | test1() | [[test1_USR]]
    }
  }

  struct LocalS {
    func meth() {
      foo() // CHECK: [[@LINE]]:7 | function/Swift | foo() | [[foo_USR]] | Ref,Call,RelCall,RelCont | rel: 1
        // CHECK-NEXT: RelCall,RelCont | function/Swift | test1() | [[test1_USR]]
    }
  }
}
