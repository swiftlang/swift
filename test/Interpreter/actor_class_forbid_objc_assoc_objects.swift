// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver  -target %target-swift-5.1-abi-triple %s -o %t/out
// RUN: %target-codesign %t/out
// RUN: %target-run %t/out

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import ObjectiveC
import _Concurrency
import StdlibUnittest

defer { runAllTests() }

var Tests = TestSuite("Actor.AssocObject")

@available(SwiftStdlib 5.0, *)
final actor Actor {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("final class crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(SwiftStdlib 5.0, *)
actor Actor2 {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("non-final class crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor2()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(SwiftStdlib 5.0, *)
actor Actor5<T> {
  var state: T
  init(state: T) { self.state = state }
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("base generic class crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor5(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }

  Tests.test("base generic class metatype crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor5<Int>.self
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(SwiftStdlib 5.0, *)
actor ActorNSObjectSubKlass : NSObject {}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("no crash when inherit from nsobject")
  .code {
    let x = ActorNSObjectSubKlass()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(SwiftStdlib 5.0, *)
actor ActorNSObjectSubKlassGeneric<T> : NSObject {
  var state: T
  init(state: T) { self.state = state }
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("no crash when generic inherit from nsobject")
  .code {
    let x = ActorNSObjectSubKlassGeneric(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}
