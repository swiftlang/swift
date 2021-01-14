// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -Xfrontend -enable-experimental-concurrency %s -o %t/out
// RUN: %target-run %t/out

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: executable_test

import ObjectiveC
import _Concurrency
import StdlibUnittest

defer { runAllTests() }

var Tests = TestSuite("Actor.AssocObject")

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
final actor class Actor {
}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("final class crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
actor class Actor2 {
}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("non-final class crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor2()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
class Actor3 : Actor2 {}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("non-final subclass crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor3()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
final class Actor3Final : Actor2 {}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("final subclass crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor3Final()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
class Actor4<T> : Actor2 {
  var state: T
  init(state: T) { self.state = state }
}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("generic subclass crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor4(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
actor class Actor5<T> {
  var state: T
  init(state: T) { self.state = state }
}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
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

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
class Actor6<T> : Actor5<T> {
  override init(state: T) { super.init(state: state) }
}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("sub-generic class base generic class crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor6(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
final class Actor6Final<T> : Actor5<T> {
  override init(state: T) { super.init(state: state) }
}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("final sub-generic class base generic class crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor6Final(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }

  Tests.test("final sub-generic class base generic class crash when set assoc object2")
  .code {
    let x = Actor6Final(state: 5)
    print(type(of: x))
  }

  Tests.test("final sub-generic class metatype, base generic class crash when set assoc object")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = Actor6Final<Int>.self
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
actor class ActorNSObjectSubKlass : NSObject {}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("no crash when inherit from nsobject")
  .code {
    let x = ActorNSObjectSubKlass()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *)
actor class ActorNSObjectSubKlassGeneric<T> : NSObject {
  var state: T
  init(state: T) { self.state = state }
}

if #available(macOS 10.4.4, iOS 12.2, watchOS 5.2, tvOS 12.2, *) {
  Tests.test("no crash when generic inherit from nsobject")
  .code {
    let x = ActorNSObjectSubKlassGeneric(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}
