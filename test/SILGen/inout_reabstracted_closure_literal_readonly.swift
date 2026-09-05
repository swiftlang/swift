// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// A closure literal whose inout parameter is received at a more abstract
// representation gets a reabstracted shadow copy with a writeback cleanup
// (WritebackReabstractedInoutCleanup). When the body never mutates the
// parameter, the writeback must be skipped: reabstracting the unmodified
// value back would wrap it in a fresh pair of reabstraction thunks on every
// call, growing the stored value without bound until the stack overflows
// when it is finally called.
// https://github.com/swiftlang/swift/issues/91348

func withGenericInout<T, R>(_ value: inout T, _ body: (inout T) -> R) -> R {
  body(&value)
}

// The read-only body takes the incoming value by copy and never writes back
// to the inout parameter, so the stored value is left untouched.
//
// CHECK-LABEL: sil private [ossa] @${{.*}}11readClosure{{.*}}fU_ :
// CHECK: bb0({{.*}}, [[PARAM:%[0-9]+]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>):
// CHECK: load [copy] [[PARAM]]{{ }}
// CHECK-NOT: store {{.*}} to [init] [[PARAM]]{{ }}
// CHECK-LABEL: } // end sil function '${{.*}}11readClosure{{.*}}fU_'
func readClosure(_ stored: inout () -> Int) -> () -> Int {
  withGenericInout(&stored) { $0 }
}

// A body that assigns to the parameter still takes the incoming value and
// writes the final value back on exit.
//
// CHECK-LABEL: sil private [ossa] @${{.*}}12writeClosure{{.*}}fU_ :
// CHECK: bb0({{.*}}, [[PARAM:%[0-9]+]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>, {{.*}}):
// CHECK: load [take] [[PARAM]]{{ }}
// CHECK: store {{.*}} to [init] [[PARAM]]{{ }}
// CHECK-LABEL: } // end sil function '${{.*}}12writeClosure{{.*}}fU_'
func writeClosure(_ stored: inout () -> Int, _ newValue: @escaping () -> Int) {
  withGenericInout(&stored) { $0 = newValue }
}

// Passing the parameter inout again counts as a mutation: the writeback is
// kept.
//
// CHECK-LABEL: sil private [ossa] @${{.*}}14forwardClosure{{.*}}fU_ :
// CHECK: bb0({{.*}}, [[PARAM:%[0-9]+]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>):
// CHECK: store {{.*}} to [init] [[PARAM]]{{ }}
// CHECK-LABEL: } // end sil function '${{.*}}14forwardClosure{{.*}}fU_'
func mutateElsewhere(_ value: inout () -> Int) {}
func forwardClosure(_ stored: inout () -> Int) {
  withGenericInout(&stored) { mutateElsewhere(&$0) }
}

// An assignment inside a defer still counts as a mutation.
//
// CHECK-LABEL: sil private [ossa] @${{.*}}10deferWrite{{.*}}fU_ :
// CHECK: bb0({{.*}}, [[PARAM:%[0-9]+]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>, {{.*}}):
// CHECK: load [take] [[PARAM]]{{ }}
// CHECK: store {{.*}} to [init] [[PARAM]]{{ }}
// CHECK-LABEL: } // end sil function '${{.*}}10deferWrite{{.*}}fU_'
func deferWrite(_ stored: inout () -> Int, _ newValue: @escaping () -> Int) {
  withGenericInout(&stored) { (p: inout () -> Int) in
    defer { p = newValue }
    return ()
  }
}

// An assignment inside a nested local function still counts as a mutation.
//
// CHECK-LABEL: sil private [ossa] @${{.*}}14localFuncWrite{{.*}}fU_ :
// CHECK: bb0({{.*}}, [[PARAM:%[0-9]+]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>, {{.*}}):
// CHECK: load [take] [[PARAM]]{{ }}
// CHECK: store {{.*}} to [init] [[PARAM]]{{ }}
// CHECK-LABEL: } // end sil function '${{.*}}14localFuncWrite{{.*}}fU_'
func localFuncWrite(_ stored: inout () -> Int, _ newValue: @escaping () -> Int) {
  withGenericInout(&stored) { (p: inout () -> Int) in
    func update() { p = newValue }
    update()
    return ()
  }
}

// Uses inside a nested closure are conservatively treated as mutations even
// when they only read.
//
// CHECK-LABEL: sil private [ossa] @${{.*}}17nestedClosureRead{{.*}}fU_ :
// CHECK: bb0({{.*}}, [[PARAM:%[0-9]+]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>):
// CHECK: load [take] [[PARAM]]{{ }}
// CHECK: store {{.*}} to [init] [[PARAM]]{{ }}
// CHECK-LABEL: } // end sil function '${{.*}}17nestedClosureRead{{.*}}fU_'
func nestedClosureRead(_ stored: inout () -> Int) -> Int {
  withGenericInout(&stored) { (p: inout () -> Int) in
    let g = { p() }
    return g()
  }
}

// An unused parameter is trivially never mutated: no writeback.
//
// CHECK-LABEL: sil private [ossa] @${{.*}}11unusedParam{{.*}}fU_ :
// CHECK: bb0({{.*}}, [[PARAM:%[0-9]+]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>):
// CHECK: load [copy] [[PARAM]]{{ }}
// CHECK-NOT: store {{.*}} to [init] [[PARAM]]{{ }}
// CHECK-LABEL: } // end sil function '${{.*}}11unusedParam{{.*}}fU_'
func unusedParam(_ stored: inout () -> Int) -> Int {
  withGenericInout(&stored) { _ in 1 }
}

// Optional storage, the shape of Mutex<(() -> Int)?>.withLock { $0 }: a plain
// read skips the writeback.
//
// CHECK-LABEL: sil private [ossa] @${{.*}}12optionalRead{{.*}}fU_ :
// CHECK: bb0({{.*}}, [[PARAM:%[0-9]+]] : $*Optional<@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>>):
// CHECK: load [copy] [[PARAM]]{{ }}
// CHECK-NOT: store {{.*}} to [init] [[PARAM]]{{ }}
// CHECK-LABEL: } // end sil function '${{.*}}12optionalRead{{.*}}fU_'
func optionalRead(_ stored: inout (() -> Int)?) -> (() -> Int)? {
  withGenericInout(&stored) { $0 }
}
