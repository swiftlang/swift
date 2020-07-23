// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

// CHECK: [[@LINE+1]]:10 | protocol/Swift | P1 | [[P1_USR:.*]] | Def
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

protocol AP {
  // CHECK: [[@LINE+1]]:18 | type-alias/associated-type/Swift | A | [[AP_P_USR:.*]] | Def,RelChild | rel: 1
  associatedtype A
}
// CHECK: [[@LINE+1]]:19 | param/Swift | x | [[TEST2_X_USR:.*]] | Def,RelChild | rel: 1
func test2<X: AP>(x: X) {
  // CHECK: [[@LINE+1]]:9 | type-alias/associated-type/Swift | A | [[AP_P_USR]] | Ref,RelCont | rel: 1
  _ = X.A.self
  // CHECK: [[@LINE+2]]:16 | param/Swift | x | [[TEST2_X_USR]] | Ref,Read,RelCont | rel: 1
  // CHECK: [[@LINE+1]]:19 | type-alias/associated-type/Swift | A | [[AP_P_USR]] | Ref,RelCont | rel: 1
  _ = type(of: x).A.self
}

@available(*, unavailable, renamed: "test")
func test2(_ o: S1?) {
  // CHECK: [[@LINE-1]]:6 | function/Swift | test2(_:) | [[test2_unavailable_USR:.*]] | Def
  // CHECK: [[@LINE-2]]:17 | struct/Swift | S1 | [[S1_USR]] | Ref
  test(o) // CHECK: [[@LINE]]:3 | function/Swift | test(_:) | {{.*}} | Ref,Call,RelCall,RelCont | rel: 1
    // CHECK-NEXT: RelCall,RelCont | function/Swift | test2(_:) | [[test2_unavailable_USR]]
}

protocol Disposable {
  func dispose()
}

func useDisposable(_ d: Disposable?) {
  // CHECK: [[@LINE+1]]:26 | instance-method/Swift | dispose() | s:14swift_ide_test10DisposableP7disposeyyF | Ref,RelCont | rel: 1
  guard let dispose = d?.dispose else { return }
  _ = dispose
}

func castExpr(x: Any) {
    // CHECK: [[@LINE+2]]:9 | struct/Swift | S1 | [[S1_USR]] | Ref
    // CHECK: [[@LINE+1]]:17 | protocol/Swift | P1 | [[P1_USR]] | Ref
    _ = S1() as P1
    // CHECK: [[@LINE+1]]:15 | struct/Swift | S1 | [[S1_USR]] | Ref
    _ = x as! S1
    // CHECK: [[@LINE+1]]:15 | struct/Swift | S1 | [[S1_USR]] | Ref
    _ = x as? S1
}
