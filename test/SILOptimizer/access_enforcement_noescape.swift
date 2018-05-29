
// RUN: %target-swift-frontend -module-name access_enforcement_noescape -enable-sil-ownership -enforce-exclusivity=checked -Onone -emit-sil -swift-version 4 -verify -parse-as-library %s
// RUN: %target-swift-frontend -module-name access_enforcement_noescape -enable-sil-ownership -enforce-exclusivity=checked -Onone -emit-sil -swift-version 3 -parse-as-library %s | %FileCheck %s
// REQUIRES: asserts

// This tests SILGen and AccessEnforcementSelection as a single set of tests.
// (Some static/dynamic enforcement selection is done in SILGen, and some is
// deferred. That may change over time but we want the outcome to be the same).
//
// These tests attempt to fully cover the possibilities of reads and
// modifications to captures along with `inout` arguments on both the caller and
// callee side.

// Helper
func doOne(_ f: () -> ()) {
  f()
}

// Helper
func doTwo(_: ()->(), _: ()->()) {}

// Helper
func doOneInout(_: ()->(), _: inout Int) {}

// Error: Cannot capture nonescaping closure.
// This triggers an early diagnostics, so it's handled in inout_capture_disgnostics.swift.
// func reentrantCapturedNoescape(fn: (() -> ()) -> ()) {
//   let c = { fn {} }
//   fn(c)
// }

// Helper
struct Frob {
  mutating func outerMut() { doOne { innerMut() } }
  mutating func innerMut() {}
}

// Allow nested mutable access via closures.
func nestedNoEscape(f: inout Frob) {
  doOne { f.outerMut() }
}
// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape14nestedNoEscape1fyAA4FrobVz_tF : $@convention(thin) (@inout Frob) -> () {
// CHECK-NOT: begin_access
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape14nestedNoEscape1fyAA4FrobVz_tF'

// closure #1 in nestedNoEscape(f:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape14nestedNoEscape1fyAA4FrobVz_tFyyXEfU_ : $@convention(thin) (@inout_aliasable Frob) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Frob
// CHECK: %{{.*}} = apply %{{.*}}([[ACCESS]]) : $@convention(method) (@inout Frob) -> ()
// CHECK: end_access [[ACCESS]] : $*Frob
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape14nestedNoEscape1fyAA4FrobVz_tFyyXEfU_'

// Allow aliased noescape reads.
func readRead() {
  var x = 3
  // Inside each closure: [read] [static]
  doTwo({ _ = x }, { _ = x })
  x = 42
}
// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape8readReadyyF : $@convention(thin) () -> () {
// CHECK: [[ALLOC:%.*]] = alloc_stack $Int, var, name "x"
// CHECK-NOT: begin_access
// CHECK: apply
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape8readReadyyF'

// closure #1 in readRead()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape8readReadyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK-NOT: begin_access [read] [dynamic]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape8readReadyyFyyXEfU_'

// closure #2 in readRead()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape8readReadyyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK-NOT: begin_access [read] [dynamic]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape8readReadyyFyyXEfU0_'

// Allow aliased noescape reads of an `inout` arg.
func inoutReadRead(x: inout Int) {
  // Inside each closure: [read] [static]
  doTwo({ _ = x }, { _ = x })
}
// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape09inoutReadE01xySiz_tF : $@convention(thin) (@inout Int) -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NOT: begin_access
// CHECK: apply %{{.*}}([[CVT1]], [[CVT2]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape09inoutReadE01xySiz_tF'

// closure #1 in inoutReadRead(x:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape09inoutReadE01xySiz_tFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK-NOT: begin_access [read] [dynamic]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape09inoutReadE01xySiz_tFyyXEfU_'

// closure #2 in inoutReadRead(x:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape09inoutReadE01xySiz_tFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK-NOT: begin_access [read] [dynamic]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape09inoutReadE01xySiz_tFyyXEfU0_'

// Allow aliased noescape read + boxed read.
func readBoxRead() {
  var x = 3
  let c = { _ = x }
  // Inside may-escape closure `c`: [read] [dynamic]
  // Inside never-escape closure: [read] [dynamic]
  doTwo(c, { _ = x })
  x = 42
}
// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape11readBoxReadyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NOT: begin_access
// CHECK: apply %{{.*}}([[CVT1]], [[CVT2]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape11readBoxReadyyF'

// closure #1 in readBoxRead()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape11readBoxReadyyFyycfU_ : $@convention(thin) (@guaranteed { var Int }) -> () {
// CHECK: [[ADDR:%.*]] = project_box %0 : ${ var Int }, 0
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[ADDR]] : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape11readBoxReadyyFyycfU_'

// closure #2 in readBoxRead()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape11readBoxReadyyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape11readBoxReadyyFyyXEfU0_'

// Error: cannout capture inout.
//
// func inoutReadReadBox(x: inout Int) {
//   let c = { _ = x }
//   doTwo({ _ = x }, c)
// }

// Allow aliased noescape read + write.
func readWrite() {
  var x = 3
  // Inside closure 1: [read] [static]
  // Inside closure 2: [modify] [static]
  doTwo({ _ = x }, { x = 42 })
}
// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape9readWriteyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NOT: begin_access
// CHECK: apply %{{.*}}([[CVT1]], [[CVT2]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape9readWriteyyF'

// closure #1 in readWrite()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape9readWriteyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK-NOT: [[ACCESS:%.*]] = begin_access [read] [dynamic]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape9readWriteyyFyyXEfU_'

// closure #2 in readWrite()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape9readWriteyyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK-NOT: [[ACCESS:%.*]] = begin_access [modify] [dynamic]
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape9readWriteyyFyyXEfU0_'

// Allow aliased noescape read + write of an `inout` arg.
func inoutReadWrite(x: inout Int) {
  // Inside closure 1: [read] [static]
  // Inside closure 2: [modify] [static]
  doTwo({ _ = x }, { x = 3 })
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape14inoutReadWrite1xySiz_tF : $@convention(thin) (@inout Int) -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: apply %{{.*}}([[CVT1]], [[CVT2]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape14inoutReadWrite1xySiz_tF'

// closure #1 in inoutReadWrite(x:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape14inoutReadWrite1xySiz_tFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape14inoutReadWrite1xySiz_tFyyXEfU_'

// closure #2 in inoutReadWrite(x:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape14inoutReadWrite1xySiz_tFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape14inoutReadWrite1xySiz_tFyyXEfU0_'


func readBoxWrite() {
  var x = 3
  let c = { _ = x }
  // Inside may-escape closure `c`: [read] [dynamic]
  // Inside never-escape closure: [modify] [dynamic]
  doTwo(c, { x = 42 })
}
// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape12readBoxWriteyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NOT: begin_access
// CHECK: apply %{{.*}}([[CVT1]], [[CVT2]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape12readBoxWriteyyF'

// closure #1 in readBoxWrite()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape12readBoxWriteyyFyycfU_ : $@convention(thin) (@guaranteed { var Int }) -> () {
// CHECK: [[ADDR:%.*]] = project_box %0 : ${ var Int }, 0
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[ADDR]] : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape12readBoxWriteyyFyycfU_'

// closure #2 in readBoxWrite()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape12readBoxWriteyyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape12readBoxWriteyyFyyXEfU0_'

// Error: cannout capture inout.
// func inoutReadBoxWrite(x: inout Int) {
//   let c = { _ = x }
//    doTwo({ x = 42 }, c)
// }

func readWriteBox() {
  var x = 3
  let c = { x = 42 }
  // Inside may-escape closure `c`: [modify] [dynamic]
  // Inside never-escape closure: [read] [dynamic]
  doTwo({ _ = x }, c)
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape12readWriteBoxyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NOT: begin_access
// CHECK: apply %{{.*}}([[CVT2]], [[CVT1]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape12readWriteBoxyyF'

// closure #1 in readWriteBox()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape12readWriteBoxyyFyycfU_ : $@convention(thin) (@guaranteed { var Int }) -> () {
// CHECK: [[ADDR:%.*]] = project_box %0 : ${ var Int }, 0
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[ADDR]] : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape12readWriteBoxyyFyycfU_'

// closure #2 in readWriteBox()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape12readWriteBoxyyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape12readWriteBoxyyFyyXEfU0_'

// Error: cannout capture inout.
// func inoutReadWriteBox(x: inout Int) {
//   let c = { x = 42 }
//   doTwo({ _ = x }, c)
// }

// Error: noescape read + write inout.
func readWriteInout() {
  var x = 3
  // Around the call: [modify] [static]
  // Inside closure: [modify] [static]
  // expected-error@+2{{overlapping accesses to 'x', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  doOneInout({ _ = x }, &x)
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape14readWriteInoutyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[ACCESS2:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: apply %{{.*}}([[CVT]], [[ACCESS2]])
// CHECK: end_access [[ACCESS2]]
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape14readWriteInoutyyF'

// closure #1 in readWriteInout()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape14readWriteInoutyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape14readWriteInoutyyFyyXEfU_'

// Error: noescape read + write inout of an inout.
func inoutReadWriteInout(x: inout Int) {
  // Around the call: [modify] [static]
  // Inside closure: [modify] [static]
  // expected-error@+2{{overlapping accesses to 'x', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  doOneInout({ _ = x }, &x)
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape19inoutReadWriteInout1xySiz_tF : $@convention(thin) (@inout Int) -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[ACCESS2:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: apply %{{.*}}([[CVT]], [[ACCESS2]])
// CHECK: end_access [[ACCESS2]]
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape19inoutReadWriteInout1xySiz_tF'

// closure #1 in inoutReadWriteInout(x:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape19inoutReadWriteInout1xySiz_tFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape19inoutReadWriteInout1xySiz_tFyyXEfU_'

// Traps on boxed read + write inout.
// Covered by Interpreter/enforce_exclusive_access.swift.
func readBoxWriteInout() {
  var x = 3
  let c = { _ = x }
  // Around the call: [modify] [dynamic]
  // Inside closure: [read] [dynamic]
  doOneInout(c, &x)
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape17readBoxWriteInoutyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] %1 : $*Int
// CHECK: apply %{{.*}}([[CVT]], [[ACCESS]])
// CHECK: end_access [[ACCESS]]
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape17readBoxWriteInoutyyF'

// closure #1 in readBoxWriteInout()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape17readBoxWriteInoutyyFyycfU_ : $@convention(thin) (@guaranteed { var Int }) -> () {
// CHECK: [[ADDR:%.*]] = project_box %0 : ${ var Int }, 0
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[ADDR]] : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape17readBoxWriteInoutyyFyycfU_'

// Error: inout cannot be captured.
// This triggers an early diagnostics, so it's handled in inout_capture_disgnostics.swift.
// func inoutReadBoxWriteInout(x: inout Int) {
//   let c = { _ = x }
//   doOneInout(c, &x)
// }

// Allow aliased noescape write + write.
func writeWrite() {
  var x = 3
  // Inside closure 1: [modify] [static]
  // Inside closure 2: [modify] [static]
  doTwo({ x = 42 }, { x = 87 })
  _ = x
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape10writeWriteyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NOT: begin_access
// CHECK: apply %{{.*}}([[CVT1]], [[CVT2]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape10writeWriteyyF'

// closure #1 in writeWrite()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape10writeWriteyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape10writeWriteyyFyyXEfU_'

// closure #2 in writeWrite()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape10writeWriteyyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape10writeWriteyyFyyXEfU0_'

  
// Allow aliased noescape write + write of an `inout` arg.
func inoutWriteWrite(x: inout Int) {
  // Inside closure 1: [modify] [static]
  // Inside closure 2: [modify] [static]
  doTwo({ x = 42}, { x = 87 })
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape010inoutWriteE01xySiz_tF : $@convention(thin) (@inout Int) -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NOT: begin_access
// CHECK: apply %{{.*}}([[CVT1]], [[CVT2]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape010inoutWriteE01xySiz_tF'

// closure #1 in inoutWriteWrite(x:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape010inoutWriteE01xySiz_tFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape010inoutWriteE01xySiz_tFyyXEfU_'

// closure #2 in inoutWriteWrite(x:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape010inoutWriteE01xySiz_tFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape010inoutWriteE01xySiz_tFyyXEfU0_'

// Traps on aliased boxed write + noescape write.
// Covered by Interpreter/enforce_exclusive_access.swift.
func writeWriteBox() {
  var x = 3
  let c = { x = 87 }
  // Inside may-escape closure `c`: [modify] [dynamic]
  // Inside never-escape closure: [modify] [dynamic]
  doTwo({ x = 42 }, c)
  _ = x
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape13writeWriteBoxyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NOT: begin_access
// CHECK: apply %{{.*}}([[CVT2]], [[CVT1]])
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape13writeWriteBoxyyF'

// closure #1 in writeWriteBox()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape13writeWriteBoxyyFyycfU_ : $@convention(thin) (@guaranteed { var Int }) -> () {
// CHECK: [[ADDR:%.*]] = project_box %0 : ${ var Int }, 0
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[ADDR]] : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape13writeWriteBoxyyFyycfU_'

// closure #2 in writeWriteBox()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape13writeWriteBoxyyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape13writeWriteBoxyyFyyXEfU0_'

// Error: inout cannot be captured.
// func inoutWriteWriteBox(x: inout Int) {
//   let c = { x = 87 }
//   doTwo({ x = 42 }, c)
// }

// Error: on noescape write + write inout.
func writeWriteInout() {
  var x = 3
  // Around the call: [modify] [static]
  // Inside closure: [modify] [static]
  // expected-error@+2{{overlapping accesses to 'x', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  doOneInout({ x = 42 }, &x)
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape15writeWriteInoutyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[ACCESS2:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: apply %{{.*}}([[CVT]], [[ACCESS2]])
// CHECK: end_access [[ACCESS2]]
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape15writeWriteInoutyyF'

// closure #1 in writeWriteInout()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape15writeWriteInoutyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape15writeWriteInoutyyFyyXEfU_'

// Error: on noescape write + write inout.
func inoutWriteWriteInout(x: inout Int) {
  // Around the call: [modify] [static]
  // Inside closure: [modify] [static]
  // expected-error@+2{{overlapping accesses to 'x', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  doOneInout({ x = 42 }, &x)
}

// inoutWriteWriteInout(x:)
// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape010inoutWriteE5Inout1xySiz_tF : $@convention(thin) (@inout Int) -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[ACCESS2:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: apply %{{.*}}([[CVT]], [[ACCESS2]])
// CHECK: end_access [[ACCESS2]]
// CHECK-LABEL: // end sil function '$S27access_enforcement_noescape010inoutWriteE5Inout1xySiz_tF'

// closure #1 in inoutWriteWriteInout(x:)
// CHECK-LABEL: sil private @$S27access_enforcement_noescape010inoutWriteE5Inout1xySiz_tFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: end_access [[ACCESS]] 
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape010inoutWriteE5Inout1xySiz_tFyyXEfU_'

// Traps on boxed write + write inout.
// Covered by Interpreter/enforce_exclusive_access.swift.
func writeBoxWriteInout() {
  var x = 3
  let c = { x = 42 }
  // Around the call: [modify] [dynamic]
  // Inside closure: [modify] [dynamic]
  doOneInout(c, &x)
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape18writeBoxWriteInoutyyF : $@convention(thin) () -> () {
// CHECK: [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK: [[CVT:%.*]] = convert_escape_to_noescape [[PA1]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] %1 : $*Int
// CHECK: apply %{{.*}}([[CVT]], [[ACCESS]])
// CHECK: end_access [[ACCESS]]
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape18writeBoxWriteInoutyyF'

// closure #1 in writeBoxWriteInout()
// CHECK-LABEL: sil private @$S27access_enforcement_noescape18writeBoxWriteInoutyyFyycfU_ : $@convention(thin) (@guaranteed { var Int }) -> () {
// CHECK: [[ADDR:%.*]] = project_box %0 : ${ var Int }, 0
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[ADDR]] : $*Int
// CHECK: end_access [[ACCESS]]
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape18writeBoxWriteInoutyyFyycfU_'

// Error: Cannot capture inout
// This triggers an early diagnostics, so it's handled in inout_capture_disgnostics.swift.
// func inoutWriteBoxWriteInout(x: inout Int) {
//   let c = { x = 42 }
//   doOneInout(c, &x)
// }

// Helper
func doBlockInout(_: @convention(block) ()->(), _: inout Int) {}

func readBlockWriteInout() {
  var x = 3
  // Around the call: [modify] [static]
  // Inside closure: [read] [static]
  // expected-error@+2{{overlapping accesses to 'x', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  doBlockInout({ _ = x }, &x)
}

// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape19readBlockWriteInoutyyF : $@convention(thin) () -> () {
// CHECK: [[F1:%.*]] = function_ref @$S27access_enforcement_noescape19readBlockWriteInoutyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> ()
// CHECK: [[PA:%.*]] = partial_apply [callee_guaranteed] [[F1]](%0) : $@convention(thin) (@inout_aliasable Int) -> ()
// CHECK-NOT: begin_access
// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: apply
// CHECK: end_access [[WRITE]] : $*Int
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape19readBlockWriteInoutyyF'

// CHECK-LABEL: sil private @$S27access_enforcement_noescape19readBlockWriteInoutyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: begin_access [read] [static] %0 : $*Int
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape19readBlockWriteInoutyyFyyXEfU_'

// Test AccessSummaryAnalysis.
//
// The captured @inout_aliasable argument to `doOne` is re-partially applied,
// then stored is a box before passing it to doBlockInout.
func noEscapeBlock() {
  var x = 3
  doOne {
    // expected-error@+2{{overlapping accesses to 'x', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    doBlockInout({ _ = x }, &x)
  }
}
// CHECK-LABEL: sil hidden @$S27access_enforcement_noescape13noEscapeBlockyyF : $@convention(thin) () -> () {
// CHECK: partial_apply [callee_guaranteed]
// CHECK-NOT: begin_access
// CHECK: apply
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape13noEscapeBlockyyF'

// CHECK-LABEL: sil private @$S27access_enforcement_noescape13noEscapeBlockyyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: [[F1:%.*]] = function_ref @$S27access_enforcement_noescape13noEscapeBlockyyFyyXEfU_yyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> ()
// CHECK: [[PA:%.*]] = partial_apply [callee_guaranteed] [[F1]](%0) : $@convention(thin) (@inout_aliasable Int) -> ()
// CHECK: [[CVT:%.*]] = convert_escape_to_noescape [[PA]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK: [[PA2:%.*]] = partial_apply
// CHECK: [[SENTINEL:%.*]] = mark_dependence [[PA2]]{{.*}}on{{.*}}[[CVT]]
// CHECK: strong_retain [[SENTINEL]]
// CHECK: [[STORAGE:%.*]] = alloc_stack $@block_storage @callee_guaranteed () -> ()
// CHECK: [[ADDR:%.*]] = project_block_storage [[STORAGE]] : $*@block_storage @callee_guaranteed () -> ()
// CHECK: store [[SENTINEL]] to [[ADDR]] : $*@callee_guaranteed () -> ()
// CHECK: [[F2:%.*]] = function_ref @$SIeg_IyB_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed () -> ()) -> ()
// CHECK: [[BLOCK:%.*]] = init_block_storage_header [[STORAGE]] : $*@block_storage @callee_guaranteed () -> (), invoke [[F2]] : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed () -> ()) -> (), type $@convention(block) @noescape () -> ()
// CHECK: [[ARG:%.*]] = copy_block [[BLOCK]] : $@convention(block) @noescape () -> ()
// CHECK: [[WRITE:%.*]] = begin_access [modify] [static] %0 : $*Int
// CHECK: apply %{{.*}}([[ARG]], [[WRITE]]) : $@convention(thin) (@guaranteed @convention(block) @noescape () -> (), @inout Int) -> ()
// CHECK: end_access [[WRITE]] : $*Int
// CHECK: dealloc_stack [[STORAGE]] : $*@block_storage @callee_guaranteed () -> ()
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape13noEscapeBlockyyFyyXEfU_'

// CHECK-LABEL: sil private @$S27access_enforcement_noescape13noEscapeBlockyyFyyXEfU_yyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// CHECK: begin_access [read] [static] %0 : $*Int
// CHECK-LABEL: } // end sil function '$S27access_enforcement_noescape13noEscapeBlockyyFyyXEfU_yyXEfU_'
