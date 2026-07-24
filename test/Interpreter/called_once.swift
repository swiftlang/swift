// RUN: %target-run-simple-swift(-enable-experimental-feature CalledAttribute) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CalledAttribute

func makeClosure(_ tag: String) -> @called(once) () -> Void {
  return { print("called \(tag)") }
}

func callIt(_ f: @called(once) () -> Void) {
  f()
}

// A `@called(once)` closure invoked directly.
func testDirectCall() {
  let f = makeClosure("direct")
  f()
}

// CHECK: called direct
testDirectCall()

// Passing a `@called(once)` value through a parameter and calling it there.
func testPassThrough() {
  callIt(makeClosure("passthrough"))
}

// CHECK-NEXT: called passthrough
testPassThrough()

// A `@called(once)` value captured (moved) into another `@called(once)`
// closure at formation time, then invoked through the wrapper.
func testWrappedCapture() {
  let f = makeClosure("wrapped")
  let g = { @called(once) in f() }
  g()
}

// CHECK-NEXT: called wrapped
testWrappedCapture()

// A captured `var` is moved into the closure at formation; reassigning the
// var afterward doesn't affect what the closure already captured, and the
// new value assigned to `f` is independently callable.
func testVarCaptureReassignedAfterFormation() {
  var f = makeClosure("original")
  let g = { @called(once) in f() }
  f = makeClosure("reassigned")
  g()
  f()
}

// CHECK-NEXT: called original
// CHECK-NEXT: called reassigned
testVarCaptureReassignedAfterFormation()

// Nested `@called(once)` closures: the outer closure's capture (`inner`)
// itself captured `f` at its own formation time.
func testNestedClosures() {
  let f = makeClosure("nested")
  let inner = { @called(once) in f() }
  let outer = { @called(once) in inner() }
  outer()
}

// CHECK-NEXT: called nested
testNestedClosures()
