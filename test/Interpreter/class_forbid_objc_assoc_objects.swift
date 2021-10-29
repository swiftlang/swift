// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-O)

// REQUIRES: objc_interop
// REQUIRES: executable_test

import ObjectiveC
import StdlibUnittest

defer { runAllTests() }

var Tests = TestSuite("AssocObject")

@available(SwiftStdlib 5.0, *)
final class AllowedToHaveAssocObject {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("no crash when set assoc object, assign") {
    let x = AllowedToHaveAssocObject()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_ASSIGN)
  }

  Tests.test("no crash when set assoc object, copy") {
    let x = AllowedToHaveAssocObject()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_COPY)
  }

  Tests.test("no crash when set assoc object, copy_nonatomic") {
    let x = AllowedToHaveAssocObject()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_COPY_NONATOMIC)
  }

  Tests.test("no crash when set assoc object, retain") {
    let x = AllowedToHaveAssocObject()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }

  Tests.test("no crash when set assoc object, retain_nonatomic") {
    let x = AllowedToHaveAssocObject()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
  }
}

@available(SwiftStdlib 5.0, *)
@_semantics("objc.forbidAssociatedObjects")
final class UnableToHaveAssocObjects {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("crash when set assoc object, assign")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnableToHaveAssocObjects()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_ASSIGN)
  }

  Tests.test("crash when set assoc object, copy")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnableToHaveAssocObjects()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_COPY)
  }

  Tests.test("crash when set assoc object, copy_nonatomic")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnableToHaveAssocObjects()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_COPY_NONATOMIC)
  }

  Tests.test("crash when set assoc object, retain")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnableToHaveAssocObjects()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }

  Tests.test("crash when set assoc object, retain_nonatomic")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnableToHaveAssocObjects()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
  }
}

@available(SwiftStdlib 5.0, *)
@_semantics("objc.forbidAssociatedObjects")
final class UnableToHaveAssocObjectsGeneric<T> {
  var state: T

  init(state: T) { self.state = state }
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("crash when set assoc object (generic)")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnableToHaveAssocObjectsGeneric(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

// In this case, we mark the child. This is unsound since we will get different
// answers since the type checker isn't enforcing this.

@available(SwiftStdlib 5.0, *)
class UnsoundAbleToHaveAssocObjectsParentClass {
}

@available(SwiftStdlib 5.0, *)
@_semantics("objc.forbidAssociatedObjects")
final class UnsoundUnableToHaveAssocObjectsSubClass : UnsoundAbleToHaveAssocObjectsParentClass {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("no crash when set assoc object set only on child subclass, but assoc to parent")
  .code {
    let x = UnsoundAbleToHaveAssocObjectsParentClass()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }

  Tests.test("crash when set assoc object set only on child subclass")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnsoundUnableToHaveAssocObjectsSubClass()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

// In this case, we mark the parent. It seems like the bit is propagated... I am
// not sure.
@available(SwiftStdlib 5.0, *)
@_semantics("objc.forbidAssociatedObjects")
class UnsoundAbleToHaveAssocObjectsParentClass2 {
}

@available(SwiftStdlib 5.0, *)
final class UnsoundUnableToHaveAssocObjectsSubClass2 : UnsoundAbleToHaveAssocObjectsParentClass2 {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("crash when set assoc object set only on parent class")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnsoundUnableToHaveAssocObjectsSubClass2()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(SwiftStdlib 5.0, *)
class UnsoundUnableToHaveAssocObjectsSubClass3 : UnsoundAbleToHaveAssocObjectsParentClass2 {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("crash when set assoc object set only on parent class, child not final")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = UnsoundUnableToHaveAssocObjectsSubClass3()
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

// More Generic Tests

// In this case, we mark the child. This is unsound since we will get different
// answers since the type checker isn't enforcing this.
@available(SwiftStdlib 5.0, *)
class GenericAbleToHaveAssocObjectsParentClass<T> {
  public var state: T
  init(state: T) { self.state = state }
}

@available(SwiftStdlib 5.0, *)
@_semantics("objc.forbidAssociatedObjects")
final class GenericUnableToHaveAssocObjectsSubClass<T> : GenericAbleToHaveAssocObjectsParentClass<T> {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("no crash when set assoc object set only on child subclass, but assoc to parent")
  .code {
    let x = GenericAbleToHaveAssocObjectsParentClass(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }

  Tests.test("crash when set assoc object set only on child subclass")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = GenericUnableToHaveAssocObjectsSubClass(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

// In this case, we mark the parent. It seems like the bit is propagated... I am
// not sure.
@available(SwiftStdlib 5.0, *)
@_semantics("objc.forbidAssociatedObjects")
class GenericAbleToHaveAssocObjectsParentClass2<T> {
  public var state: T
  init(state: T) { self.state = state }
}

@available(SwiftStdlib 5.0, *)
final class GenericUnableToHaveAssocObjectsSubClass2<T> : GenericAbleToHaveAssocObjectsParentClass2<T> {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("crash when set assoc object set only on parent class")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = GenericUnableToHaveAssocObjectsSubClass2(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}

@available(SwiftStdlib 5.0, *)
class GenericUnableToHaveAssocObjectsSubClass3<T> : GenericAbleToHaveAssocObjectsParentClass2<T> {
}

if #available(SwiftStdlib 5.0, *) {
  Tests.test("crash when set assoc object set only on parent class, child not final")
  .crashOutputMatches("objc_setAssociatedObject called on instance")
  .code {
    expectCrashLater()
    let x = GenericUnableToHaveAssocObjectsSubClass3(state: 5)
    objc_setAssociatedObject(x, "myKey", "myValue", .OBJC_ASSOCIATION_RETAIN)
  }
}
