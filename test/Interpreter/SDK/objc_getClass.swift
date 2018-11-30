// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/libresilient_struct.%target-dylib-extension) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/libresilient_struct.%target-dylib-extension

// RUN: %target-build-swift-dylib(%t/libresilient_class.%target-dylib-extension) -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience %S/../../Inputs/resilient_class.swift -emit-module -emit-module-path %t/resilient_class.swiftmodule -module-name resilient_class -I%t -L%t -lresilient_struct
// RUN: %target-codesign %t/libresilient_class.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -lresilient_class -o %t/main -Xlinker -rpath -Xlinker %t
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/libresilient_struct.%target-dylib-extension %t/libresilient_class.%target-dylib-extension


// REQUIRES: executable_test
// REQUIRES: objc_interop

// Test Swift's hook for objc_getClass()

import StdlibUnittest
import ObjectiveC
import Foundation
import resilient_struct
import resilient_class

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


class ResilientSuperclass : ResilientOutsideParent {
  var supervalue = 10
}

class ResilientSubclass : ResilientSuperclass {
  var subvalue = 20
}


class ResilientFieldSuperclassSwift {
  var supervalue = ResilientInt(i: 1)
}

class ResilientFieldSubclassSwift : ResilientFieldSuperclassSwift {
  var subvalue = ResilientInt(i: 2)
}

class ResilientFieldSuperclassObjC : NSObject {
  var supervalue = ResilientInt(i: 3)
}
class ResilientFieldSubclassObjC : ResilientFieldSuperclassObjC {
  var subvalue = ResilientInt(i: 4)
}


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

testSuite.test("ResilientSubclass")
  .skip(.custom({ getClassHookMissing },
                reason: "objc_getClass hook not present"))
  .code {
  requireClass(named: "main.ResilientSubclass")
  requireClass(named: "main.ResilientSuperclass")

  expectEqual(ResilientSuperclass().supervalue, 10)
  expectEqual(ResilientSubclass().supervalue, 10)
  expectEqual(ResilientSubclass().subvalue, 20)
}

testSuite.test("ResilientField")
  .skip(.custom({ getClassHookMissing },
                reason: "objc_getClass hook not present"))
  .code {
  requireClass(named: "main.ResilientFieldSubclassSwift")
  requireClass(named: "main.ResilientFieldSuperclassSwift")
  requireClass(named: "main.ResilientFieldSubclassObjC")
  requireClass(named: "main.ResilientFieldSuperclassObjC")

  expectEqual(ResilientFieldSuperclassSwift().supervalue.i, 1)
  expectEqual(ResilientFieldSubclassSwift().supervalue.i, 1)
  expectEqual(ResilientFieldSubclassSwift().subvalue.i, 2)
  expectEqual(ResilientFieldSuperclassObjC().supervalue.i, 3)
  expectEqual(ResilientFieldSubclassObjC().supervalue.i, 3)
  expectEqual(ResilientFieldSubclassObjC().subvalue.i, 4)
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

