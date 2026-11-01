// RUN: %target-swift-frontend -primary-file %s -emit-sil -O -Xllvm -debug-only=closure-scope -module-name main -o /dev/null 2>&1 | %FileCheck %s
// REQUIRES: asserts

public protocol Apply {
  func apply(_ body: (Apply) -> ())
}

public func testRecursiveClosure(a: Apply, x: inout Int) {
  func localFunc(b: Apply) {
    x += 1
    let closure = { (c: Apply) in
      c.apply(localFunc)
    }
    b.apply(closure)
  }
  a.apply(localFunc)
}

public func testDeadClosureCycle(a: Apply, x: inout Int) {
  func localFunc(b: Apply) {
    x += 1
    let closure = { (c: Apply) in
      c.apply(localFunc)
    }
    b.apply(closure)
  }
}

// The graph and order are dumped once per closure group. The important things
// are:
//
// - the closure scope graph has the same shape
// - the localFunc is marked CYCLE_HEAD
// - the RPO function order does not change

// testRecursiveClosure's group:
//
// CHECK: SCOPE: testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF'
// CHECK:     CLOSURE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: SCOPE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK:     CLOSURE: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: SCOPE: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK:     CLOSURE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: CLOSURE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK:     SCOPE: testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF'
// CHECK:     SCOPE: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: CLOSURE: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK:     SCOPE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: RPO function order:
// CHECK: testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF'
// CHECK: CYCLE HEAD: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'

// testDeadClosureCycle's group. No root reaches the dead cycle, so its
// localFunc is a DFS root of its own:
//
// CHECK: SCOPE: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK:     CLOSURE: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: SCOPE: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK:     CLOSURE: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: CLOSURE: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK:     SCOPE: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: CLOSURE: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK:     SCOPE: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: RPO function order:
// CHECK: CYCLE HEAD: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
