// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-swift-frontend -target %target-future-triple -I %S/Inputs/CTypes -prespecialize-generic-metadata -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -enable-layout-string-value-witnesses -enable-layout-string-value-witnesses-instantiation -enable-type-layout -enable-autolinking-runtime-compatibility-bytecode-layouts -parse-stdlib -emit-module -emit-module-path=%t/layout_string_witnesses_types.swiftmodule %S/Inputs/layout_string_witnesses_types.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(layout_string_witnesses_types)) -target %target-future-triple -I %S/Inputs/CTypes -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnesses -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnessesInstantiation -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-layout-string-value-witnesses-instantiation -Xfrontend -enable-type-layout -Xfrontend -parse-stdlib -parse-as-library %S/Inputs/layout_string_witnesses_types.swift
// RUN: %target-codesign %t/%target-library-name(layout_string_witnesses_types)
// RUN: %target-build-swift -target %target-future-triple -g -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnesses -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnessesInstantiation -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-layout-string-value-witnesses-instantiation -Xfrontend -enable-type-layout -parse-stdlib -module-name layout_string_witnesses_dynamic -llayout_string_witnesses_types -L%t -I %S/Inputs/ObjCClasses/ %t/ObjCClasses.o -I %t -o %t/main %s %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(layout_string_witnesses_types) | %FileCheck %s --check-prefix=CHECK

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_LayoutStringValueWitnesses
// REQUIRES: swift_feature_LayoutStringValueWitnessesInstantiation

// Requires runtime functions added in Swift 5.9.
// UNSUPPORTED: use_os_stdlib

import Swift
import layout_string_witnesses_types
import ObjCClasses
import Foundation

func testNestedResilientObjc() {
    let ptr = allocateInternalGenericPtr(of: NestedWrapper<ObjCPrintOnDealloc>.self)

    do {
        let x = NestedWrapper<ObjCPrintOnDealloc>(x: .init(x: ObjCPrintOnDealloc()), y: .init(x: ObjCPrintOnDealloc()))
        testGenericInit(ptr, to: x)
    }

    do {
        let y = NestedWrapper<ObjCPrintOnDealloc>(x: .init(x: ObjCPrintOnDealloc()), y: .init(x: ObjCPrintOnDealloc()))
        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
        // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
        testGenericAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
    // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
    testGenericDestroy(ptr, of: NestedWrapper<ObjCPrintOnDealloc>.self)

    ptr.deallocate()
}

testNestedResilientObjc()

protocol P {}

extension ObjCPrintOnDealloc: P {}

enum MultiPayloadObjCExistential {
    case x(AnyObject)
    case y(P & ObjCPrintOnDealloc)
}

struct MultiPayloadObjCExistentialWrapper {
    let x: MultiPayloadObjCExistential
    let y: Int = 0
}

func testMultiPayloadObjCExistentialWrapper() {
    let ptr = allocateInternalGenericPtr(of: MultiPayloadObjCExistentialWrapper.self)

    do {
        let x = MultiPayloadObjCExistentialWrapper(x: .y(ObjCPrintOnDealloc()))
        testGenericInit(ptr, to: x)
    }

    do {
        let y = MultiPayloadObjCExistentialWrapper(x: .y(ObjCPrintOnDealloc()))
        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
        testGenericAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
    testGenericDestroy(ptr, of: MultiPayloadObjCExistentialWrapper.self)

    ptr.deallocate()
}

testMultiPayloadObjCExistentialWrapper()

@objc
class SwiftObjC: NSObject {
    deinit {
        print("SwiftObjC deinitialized!")
    }
}

enum MultiPayloadNativeSwiftObjC {
    case x(SwiftObjC)
    case y(SwiftObjC)
    case z(SwiftObjC)
}

func testMultiPayloadNativeSwiftObjC() {
    let ptr = allocateInternalGenericPtr(of: MultiPayloadNativeSwiftObjC.self)

    do {
        let x = MultiPayloadNativeSwiftObjC.y(SwiftObjC())
        testGenericInit(ptr, to: x)
    }

    do {
        let y = MultiPayloadNativeSwiftObjC.z(SwiftObjC())
        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SwiftObjC deinitialized!
        testGenericAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SwiftObjC deinitialized!
    testGenericDestroy(ptr, of: MultiPayloadNativeSwiftObjC.self)

    ptr.deallocate()
}

testMultiPayloadNativeSwiftObjC()

public enum MultiPayloadBlock {
#if _pointerBitWidth(_32)
    public typealias PaddingPayload = (Int16, Int8, Bool)
#else
    public typealias PaddingPayload = (Int32, Int16, Int8, Bool)
#endif

    case x(PaddingPayload)
    case y(@convention(block) () -> Void)
}

func testMultiPayloadBlock() {
    let ptr = UnsafeMutablePointer<MultiPayloadBlock>.allocate(capacity: 1)

    // initWithCopy
    do {
        let instance = SimpleClass(x: 0)
        let x = MultiPayloadBlock.y({ print(instance) })
        testInit(ptr, to: x)
    }

    // assignWithTake
    do {
        let instance = SimpleClass(x: 1)
        let y = MultiPayloadBlock.y({ print(instance) })

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        let instance = SimpleClass(x: 2)
        var z = MultiPayloadBlock.y({ print(instance) })

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssignCopy(ptr, from: &z)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // destroy
    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testMultiPayloadBlock()

class GenericOuterClassNSObject<T: NSObject> {
    enum InnerEnum {
        case x(T.Type)
        case y(T)
    }
}

func testNestedGenericEnumNSObject() {
    let ptr = UnsafeMutablePointer<GenericOuterClassNSObject<ObjCPrintOnDealloc>.InnerEnum>.allocate(capacity: 1)

    // initWithCopy
    do {
        let x = GenericOuterClassNSObject<ObjCPrintOnDealloc>.InnerEnum.y(ObjCPrintOnDealloc())
        testInit(ptr, to: x)
    }

    // assignWithTake
    do {
        let y = GenericOuterClassNSObject<ObjCPrintOnDealloc>.InnerEnum.y(ObjCPrintOnDealloc())

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        var z = GenericOuterClassNSObject<ObjCPrintOnDealloc>.InnerEnum.y(ObjCPrintOnDealloc())

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
        testAssignCopy(ptr, from: &z)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // destroy
    // CHECK-NEXT: ObjCPrintOnDealloc deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testNestedGenericEnumNSObject()

class GenericOuterClassSwiftObjC<T: SwiftObjC> {
    enum InnerEnum {
        case x(T.Type)
        case y(T)
    }
}

func testNestedGenericEnumSwiftObjC() {
    let ptr = UnsafeMutablePointer<GenericOuterClassSwiftObjC<SwiftObjC>.InnerEnum>.allocate(capacity: 1)

    // initWithCopy
    do {
        let x = GenericOuterClassSwiftObjC<SwiftObjC>.InnerEnum.y(SwiftObjC())
        testInit(ptr, to: x)
    }

    // assignWithTake
    do {
        let y = GenericOuterClassSwiftObjC<SwiftObjC>.InnerEnum.y(SwiftObjC())

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SwiftObjC deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        var z = GenericOuterClassSwiftObjC<SwiftObjC>.InnerEnum.y(SwiftObjC())

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SwiftObjC deinitialized!
        testAssignCopy(ptr, from: &z)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // destroy
    // CHECK-NEXT: SwiftObjC deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testNestedGenericEnumSwiftObjC()

struct SwiftObjCAndWeakObjC {
    let x: SwiftObjC
    weak var y: NSObject?
}

func testSwiftObjCAndWeakObjC() {
    let ptr = UnsafeMutablePointer<SwiftObjCAndWeakObjC>.allocate(capacity: 1)

    // initWithCopy
    do {
        let x = SwiftObjCAndWeakObjC(x: SwiftObjC(), y: nil)
        testInit(ptr, to: x)
    }

    // assignWithTake
    do {
        let y = SwiftObjCAndWeakObjC(x: SwiftObjC(), y: nil)

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SwiftObjC deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        var z = SwiftObjCAndWeakObjC(x: SwiftObjC(), y: nil)

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SwiftObjC deinitialized!
        testAssignCopy(ptr, from: &z)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // destroy
    // CHECK-NEXT: SwiftObjC deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testSwiftObjCAndWeakObjC()
