// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature CalledAttribute -O -Xllvm -sil-print-after=ClosureSpecialization %s -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

final class Tracker {
  var x: Int = 0
}

struct BorrowableValue: ~Copyable {
  var y: Int

  borrowing func peek() -> Int { y }
}

@inline(never)
func applyInt(_ f: (Int) -> Int, _ x: Int) -> Int { f(x) }

@inline(never)
func callOnce(_ fn: @called(once) () -> Void) { fn() }

@inline(never)
func callOnceEscaping(_ fn: @escaping @called(once) () -> Void) { fn() }

// A single owned Copyable capture
@inline(never)
func testLocalVariableClass(_ t: Tracker, _ y: Int) {
  callOnce {
    t.x += y
  }
}

// CHECK-LABEL: sil shared [noinline] [ossa] @{{.*}}callOnce{{.*}}testLocalVariableClass{{.*}} : $@convention(thin) (@owned Tracker, Int) -> () {
// CHECK: bb0([[TRACKER:%.*]] : @owned $Tracker, [[Y:%.*]] : $Int):
// CHECK: [[CLOSURE_IMPL:%.*]] = function_ref @{{.*}}testLocalVariableClass{{.*}} : $@convention(thin) (@guaranteed Tracker, Int) -> ()
// CHECK: [[TRACKER_COPY:%.*]] = copy_value [[TRACKER]]
// CHECK: partial_apply [on_stack] [called_once] [[CLOSURE_IMPL]]([[TRACKER]], [[Y]])
// CHECK: apply [[CLOSURE_IMPL]]([[TRACKER_COPY]], [[Y]])
// CHECK: }

// Mixed owned Copyable + borrowed ~Copyable captures
@inline(never)
func testMixedCaptures(_ t: Tracker, _ v: borrowing BorrowableValue, _ delta: Int) {
  callOnce {
    t.x += v.peek() + delta
  }
}

// CHECK-LABEL: sil shared [noinline] [ossa] @{{.*}}callOnce{{.*}}testMixedCaptures{{.*}} : $@convention(thin) (@owned Tracker, @guaranteed BorrowableValue, Int) -> () {
// CHECK: bb0([[TRACKER:%.*]] : @owned $Tracker, [[V:%.*]] : @guaranteed $BorrowableValue, [[DELTA:%.*]] : $Int):
// CHECK: [[CLOSURE_IMPL:%.*]] = function_ref @{{.*}}testMixedCaptures{{.*}} : $@convention(thin) (@guaranteed Tracker, @guaranteed BorrowableValue, Int) -> ()
// CHECK: [[TRACKER_COPY:%.*]] = copy_value [[TRACKER]]
// CHECK: partial_apply [on_stack] [called_once] [[CLOSURE_IMPL]]([[TRACKER]], [[V]], [[DELTA]])
// CHECK: apply [[CLOSURE_IMPL]]([[TRACKER_COPY]], [[V]], [[DELTA]])
// CHECK: }

@inline(never)
func testEscapingCapture(_ t: Tracker, _ y: Int) {
  callOnceEscaping {
    t.x += y
  }
}

// CHECK-LABEL: sil shared [noinline] [ossa] @{{.*}}callOnceEscaping{{.*}} : $@convention(thin) (@owned Tracker, Int) -> () {
// CHECK: bb0([[TRACKER:%.*]] : @owned $Tracker, [[Y:%.*]] : $Int):
// CHECK: [[CLOSURE_IMPL:%.*]] = function_ref @{{.*}} : $@convention(thin) (@guaranteed Tracker, Int) -> ()
// CHECK: [[TRACKER_COPY:%.*]] = copy_value [[TRACKER]]
// CHECK: partial_apply [called_once] [[CLOSURE_IMPL]]([[TRACKER]], [[Y]])
// CHECK: apply [[CLOSURE_IMPL]]([[TRACKER_COPY]], [[Y]])
// CHECK: }

@inline(never)
func testOnStackCallSite(_ t: Tracker, _ y: Int) {
  callOnce { t.x += y }
}

@inline(never)
func testOnStackCallSite2(_ t: Tracker, _ y: Int) {
  callOnce { t.x -= y }
}

@inline(never)
func testEscapingCallSite(_ t: Tracker, _ y: Int) {
  callOnceEscaping { t.x += y }
}

// Captured closure value triggers multiple rounds of specialization

@inline(never)
func testNestedClosureCapture(_ t: Tracker, _ y: Int) {
  let inner: (Int) -> Int = { $0 + t.x }
  callOnce {
    t.x = applyInt(inner, y)
  }
}

// CHECK-LABEL: sil shared [noinline] [ossa] @{{.*}}callOnce{{.*}} : $@convention(thin) (@owned Tracker, @owned @callee_guaranteed (Int) -> Int, Int) -> () {
// CHECK: bb0([[TRACKER:%.*]] : @owned $Tracker, [[INNER:%.*]] : @owned $@callee_guaranteed (Int) -> Int, [[Y:%.*]] : $Int):
// CHECK: [[CLOSURE_IMPL:%.*]] = function_ref @{{.*}} : $@convention(thin) (@guaranteed Tracker, @guaranteed @callee_guaranteed (Int) -> Int, Int) -> ()
// CHECK: [[TRACKER_COPY:%.*]] = copy_value [[TRACKER]]
// CHECK: [[INNER_COPY:%.*]] = copy_value [[INNER]]
// CHECK: partial_apply [on_stack] [called_once] [[CLOSURE_IMPL]]([[TRACKER]], [[INNER]], [[Y]])
// CHECK: apply [[CLOSURE_IMPL]]([[TRACKER_COPY]], [[INNER_COPY]], [[Y]])
// CHECK: }

// Trigger specialization
public func run() {
  let t = Tracker()
  testLocalVariableClass(t, 42)

  let t2 = Tracker()
  let v = BorrowableValue(y: 10)
  testMixedCaptures(t2, v, 5)

  let t3 = Tracker()
  testEscapingCapture(t3, 42)

  let t4 = Tracker()
  testOnStackCallSite(t4, 1)
  testOnStackCallSite2(t4, 2)
  let t5 = Tracker()
  testEscapingCallSite(t5, 3)

  let t6 = Tracker()
  testNestedClosureCapture(t6, 5)
}
