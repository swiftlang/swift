// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s

public class C {

  // Test that curry thunks don't have line table entries.
  // CHECK: define {{.*}}@_T04main1CC11someHandleryyFTc(%T4main1CC*)
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
