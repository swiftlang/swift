// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

public class C {

  // Test that curry thunks don't have line table entries.
  // CHECK: define {{.*}}@_TFC4main1C11someHandlerFT_T_(%C4main1C*)
  // CHECK-SAME:         !dbg ![[CURRY_THUNK:[0-9]+]]
  // CHECK-NOT: ret {{.*}},
  // CHECK: {{.*}}, !dbg ![[DBG:[0-9]+]]
  // CHECK: ret {{.*}}, !dbg ![[DBG]]
  // CHECK: ![[DBG]] = !DILocation(line: 0, scope: ![[CURRY_THUNK]])
  func someHandler() { }

  func doSomethingWithHandler(_ theHandler: ((Void) -> Void)!) -> Void {
	  theHandler()
  }

  public func entry() {
	  doSomethingWithHandler(someHandler)
  }
}
