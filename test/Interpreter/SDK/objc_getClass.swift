// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

// Test Swift's hook for objc_getClass()

import StdlibUnittest
import ObjectiveC
import Foundation

// Old OS versions do not have this hook.
let getClassHookMissing = {
  nil == dlsym(UnsafeMutableRawPointer(bitPattern: -2),
    "objc_setHook_getClass")
}()

var testSuite = TestSuite("objc_getClass")


class SwiftSuperclass { }
class SwiftSubclass : SwiftSuperclass { }

class ObjCSuperclass : NSObject { }
class ObjCSubclass : ObjCSuperclass { }


class MangledSwiftSuperclass { }
class MangledSwiftSubclass : MangledSwiftSuperclass { }

class MangledObjCSuperclass : NSObject { }
class MangledObjCSubclass : MangledObjCSuperclass { }


class GenericSwiftClass<Value> {
  let value: Value
  init(value: Value) { self.value = value }
}
class ConstrainedSwiftSuperclass : GenericSwiftClass<String> {
  init() { super.init(value:"") }
}
class ConstrainedSwiftSubclass : ConstrainedSwiftSuperclass { }


class MangledGenericSwiftClass<Value> {
  let value: Value
  init(value: Value) { self.value = value }
}
class MangledConstrainedSwiftSuperclass : MangledGenericSwiftClass<String> {
  init() { super.init(value:"") }
}
class MangledConstrainedSwiftSubclass : MangledConstrainedSwiftSuperclass { }


class GenericObjCClass<Value> : NSObject {
  let value: Value
  init(value: Value) { self.value = value }
}
class ConstrainedObjCSuperclass : GenericObjCClass<String> {
  init() { super.init(value:"") }
}
class ConstrainedObjCSubclass : ConstrainedObjCSuperclass { }


class MangledGenericObjCClass<Value> : NSObject {
  let value: Value
  init(value: Value) { self.value = value }
}
class MangledConstrainedObjCSuperclass : MangledGenericObjCClass<String> {
  init() { super.init(value:"") }
}
class MangledConstrainedObjCSubclass : MangledConstrainedObjCSuperclass { }


func requireClass(named name: String, demangledName: String) {
  for _ in 1...2 {
    let cls: AnyClass? = NSClassFromString(name)
    expectNotNil(cls, "class named \(name) unexpectedly not found")
    expectEqual(NSStringFromClass(cls!), demangledName,
                "class named \(name) has the wrong name");
  }
}

func requireClass(named name: String) {
  return requireClass(named: name, demangledName: name)
}

testSuite.test("Basic") {
  requireClass(named: "main.SwiftSubclass")
  requireClass(named: "main.SwiftSuperclass")
  requireClass(named: "main.ObjCSubclass")
  requireClass(named: "main.ObjCSuperclass")
}

testSuite.test("BasicMangled") {
  requireClass(named:   "_TtC4main20MangledSwiftSubclass",
               demangledName: "main.MangledSwiftSubclass")
  requireClass(named:   "_TtC4main22MangledSwiftSuperclass",
               demangledName: "main.MangledSwiftSuperclass")
  requireClass(named:   "_TtC4main19MangledObjCSubclass",
               demangledName: "main.MangledObjCSubclass")
  requireClass(named:   "_TtC4main21MangledObjCSuperclass",
               demangledName: "main.MangledObjCSuperclass")
}

testSuite.test("Generic")
  .skip(.custom({ getClassHookMissing },
                reason: "objc_getClass hook not present"))
  .code {
  requireClass(named: "main.ConstrainedSwiftSubclass")
  requireClass(named: "main.ConstrainedSwiftSuperclass")
  requireClass(named: "main.ConstrainedObjCSubclass")
  requireClass(named: "main.ConstrainedObjCSuperclass")
}

testSuite.test("GenericMangled")
  .skip(.custom({ getClassHookMissing },
                reason: "objc_getClass hook not present"))
  .code {
  requireClass(named:   "_TtC4main24ConstrainedSwiftSubclass",
               demangledName: "main.ConstrainedSwiftSubclass")
  requireClass(named:   "_TtC4main26ConstrainedSwiftSuperclass",
               demangledName: "main.ConstrainedSwiftSuperclass")
  requireClass(named:   "_TtC4main23ConstrainedObjCSubclass",
               demangledName: "main.ConstrainedObjCSubclass")
  requireClass(named:   "_TtC4main25ConstrainedObjCSuperclass",
               demangledName: "main.ConstrainedObjCSuperclass")
}


testSuite.test("NotPresent") {
  // This class does not exist.
  expectNil(NSClassFromString("main.ThisClassDoesNotExist"));

  // This name is improperly mangled
  expectNil(NSClassFromString("_TtC5main"));

  // Swift.Int is not a class type.
  expectNil(NSClassFromString("Si"))
}

runAllTests()

