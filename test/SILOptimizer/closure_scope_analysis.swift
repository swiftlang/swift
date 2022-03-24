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

// The order of the top-level SCOPE nodes is sensitive to the module's
// function list ordering. It may change. The important thing is that:
//
// - the closure scope graph has the same shape
// - the localFunc is marked CYCLE_HEAD
// - the RPO function order does not change

// testRecursiveClosure scopes:
//
// CHECK: SCOPE: testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF'
// CHECK:     CLOSURE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: SCOPE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK:     CLOSURE: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: SCOPE: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK:     CLOSURE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'

// testDeadClosureCycle scopes:
//
// CHECK: SCOPE: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK:     CLOSURE: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: SCOPE: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK:     CLOSURE: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'

// testRecursiveClosure closures:
//
// CHECK: CLOSURE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK:     SCOPE: testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF'
// CHECK:     SCOPE: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: CLOSURE: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK:     SCOPE: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'

// testDeadClosureCycle closures:
//
// CHECK: CLOSURE: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK:     SCOPE: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: CLOSURE: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK:     SCOPE: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'

// CHECK: RPO function order:

// CHECK: main 'main'
// CHECK: testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF'
// CHECK: CYCLE HEAD: localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: closure #1 in localFunc #1 (b:) in testRecursiveClosure(a:x:) '$s4main20testRecursiveClosure1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
// CHECK: Int.init(_builtinIntegerLiteral:) '$sSi22_builtinIntegerLiteralSiBI_tcfC'
// CHECK: static Int.+= infix(_:_:) '$sSi2peoiyySiz_SitFZ'

// CHECK: testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF'
// CHECK: CYCLE HEAD: localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tF'
// CHECK: closure #1 in localFunc #1 (b:) in testDeadClosureCycle(a:x:) '$s4main20testDeadClosureCycle1a1xyAA5Apply_p_SiztF9localFuncL_1byAaE_p_tFyAaE_pcfU_'
