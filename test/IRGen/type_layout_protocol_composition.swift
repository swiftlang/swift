// REQUIRES: objc_interop
// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Onone -Xfrontend -enable-type-layout %s -o %t/main_type_layouts
// RUN: %target-build-swift -Onone -Xfrontend -disable-type-layout %s -o %t/main_no_type_layouts
// RUN: %target-codesign %t/main_type_layouts
// RUN: %target-codesign %t/main_no_type_layouts
// RUN: %target-run %t/main_type_layouts
// RUN: %target-run %t/main_no_type_layouts

import Swift
import Foundation
import StdlibUnittest

public class SomeObject {
    init() {
        a = LifetimeTracked(0)
    }
    let a: LifetimeTracked
}
public class SomeNSObject : NSObject {
    override init() {
        a = LifetimeTracked(0)
    }
    let a: LifetimeTracked
}

public protocol ProtoA{}

public protocol SomeProtoType { }
public typealias ObjcComposition = SomeNSObject & SomeProtoType
public typealias NativeComposition = SomeObject & SomeProtoType

public struct Thing1<T: ProtoA> {
    let a: ObjcComposition
    let b: T

    init(
         a: ObjcComposition,
         b: T
    ) {
        self.a = a
        self.b = b
    }
}

public struct Thing2<T: ProtoA> {
    let a: NativeComposition
    let b: T

    init(a: NativeComposition,
         b: T
    ) {
        self.a = a
        self.b = b
    }
}


public class Comp1 : SomeNSObject, SomeProtoType {
    override init() {
        b = LifetimeTracked(0)
    }
    let b: LifetimeTracked

}
public class Comp2 : SomeObject, SomeProtoType {
    override init() {
        b = LifetimeTracked(0)
    }
    let b: LifetimeTracked
}
public struct Proto1 : ProtoA {
    init() {
        a = LifetimeTracked(0)
    }
    let a: LifetimeTracked
}

let ProtocolComp = TestSuite("ProtocolComposition")

func forceVWUsage<T>(_ value1: T, _ value2: T) -> T {
    var a = value1
    var b = value2
    var c = a
    a = b
    b = c
    c = a
    return c;
}
ProtocolComp.test("Objc Class Compositions should destroy properly") {
    var thing1 = Thing1<Proto1>(a: Comp1(), b: Proto1())
    expectTrue((thing1.a as! Comp1).b.value == 0)
    thing1 = forceVWUsage(thing1, thing1)
    expectTrue((thing1.a as! Comp1).b.value == 0)
}

ProtocolComp.test("NativeClassCompositions") {
    var thing2 = Thing2<Proto1>(a: Comp2(), b: Proto1())
    expectTrue((thing2.a as! Comp2).b.value == 0)
    thing2 = forceVWUsage(thing2, thing2)
}

runAllTests()
