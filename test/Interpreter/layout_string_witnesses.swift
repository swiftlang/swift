// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -emit-module -emit-module-path=%t/layout_string_witnesses_types.swiftmodule %S/Inputs/layout_string_witnesses_types.swift
// RUN: %target-build-swift -O -c -parse-as-library -o %t/layout_string_witnesses_types.o %S/Inputs/layout_string_witnesses_types.swift
// RUN: %target-build-swift -O -module-name layout_string_witnesses %t/layout_string_witnesses_types.o -I %t -o %t/main %s
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s --check-prefix=CHECK -check-prefix=CHECK-%target-os

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -enable-autolinking-runtime-compatibility-bytecode-layouts -force-struct-type-layouts -emit-module -emit-module-path=%t/layout_string_witnesses_types.swiftmodule %S/Inputs/layout_string_witnesses_types.swift
// RUN: %target-build-swift -O -Xfrontend -enable-autolinking-runtime-compatibility-bytecode-layouts -Xfrontend -force-struct-type-layouts -c -parse-as-library -o %t/layout_string_witnesses_types.o %S/Inputs/layout_string_witnesses_types.swift
// RUN: %target-build-swift -O -Xfrontend -enable-autolinking-runtime-compatibility-bytecode-layouts -Xfrontend -force-struct-type-layouts -module-name layout_string_witnesses %t/layout_string_witnesses_types.o -I %t -o %t/main %s
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s --check-prefix=CHECK -check-prefix=CHECK-%target-os

// REQUIRES: executable_test

import layout_string_witnesses_types

func testSimple() {
    let ptr = UnsafeMutablePointer<Simple>.allocate(capacity: 1)

    do {
        let x = Simple(x: 3, y: SimpleClass(x: 23), z: SimpleBig())
        testInit(ptr, to: x)
    }

    // CHECK: 3 - 23
    print("\(ptr.pointee.x) - \(ptr.pointee.y.x)")

    do {
        let y = Simple(x: 2, y: SimpleClass(x: 1), z: SimpleBig())
        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: 2 - 1
    print("\(ptr.pointee.x) - \(ptr.pointee.y.x)")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)
    
    ptr.deallocate()
}

testSimple()

class TestClass {

    init() {}

    deinit {
        print("TestClass deinitialized!")
    }
}

func testGeneric() {
    let ptr = allocateInternalGenericPtr(of: TestClass.self)
    
    do {
        let x = TestClass()
        testGenericInit(ptr, to: x)
    }

    do {
        let y = TestClass()
        // CHECK: TestClass deinitialized!
        testGenericAssign(ptr, from: y)
    }

    // CHECK-NEXT: TestClass deinitialized!
    testGenericDestroy(ptr, of: TestClass.self)

    ptr.deallocate()
}

testGeneric()

func testWeakNative() {
    let ptr = UnsafeMutablePointer<WeakNativeWrapper>.allocate(capacity: 1)

    do {
        let ref = SimpleClass(x: 2)
        withExtendedLifetime(ref) {
            testInit(ptr, to: WeakNativeWrapper(x: ref))

            guard let x = ptr.pointee.x else {
                fatalError("Weak reference prematurely destroyed")
            }

            // CHECK: value: 2
            print("value: \(x.x)")
        }
    }

    do {
        let ref = SimpleClass(x: 34)
        
        withExtendedLifetime(ref) {
            // CHECK-NEXT: SimpleClass deinitialized!
            testAssign(ptr, from: WeakNativeWrapper(x: ref))

            guard let x = ptr.pointee.x else {
                fatalError("Weak reference prematurely destroyed")
            }

            // CHECK-NEXT: value: 34
            print("value: \(x.x)")
        }
    }

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testWeakNative()

func testUnownedNative() {
    let ptr = UnsafeMutablePointer<UnownedNativeWrapper>.allocate(capacity: 1)

    do {
        let ref = SimpleClass(x: 2)
        withExtendedLifetime(ref) {
            testInit(ptr, to: UnownedNativeWrapper(x: ref))

            // CHECK: value: 2
            print("value: \(ptr.pointee.x.x)")
        }
    }

    do {
        let ref = SimpleClass(x: 34)
        
        withExtendedLifetime(ref) {
            // CHECK-NEXT: SimpleClass deinitialized!
            testAssign(ptr, from: UnownedNativeWrapper(x: ref))

            // CHECK-NEXT: value: 34
            print("value: \(ptr.pointee.x.x)")
        }
    }

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testUnownedNative()

func testClosure() {
    let ptr = UnsafeMutablePointer<ClosureWrapper>.allocate(capacity: 1)

    do {
        let ref = SimpleClass(x: 2)
        testInit(ptr, to: ClosureWrapper(f: { print("value: \(ref.x)") }))

        // CHECK: value: 2
        ptr.pointee.f()
    }

    do {
        let ref = SimpleClass(x: 34)
        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: ClosureWrapper(f: { print("value: \(ref.x)") }))

        // CHECK-NEXT: value: 34
        ptr.pointee.f()
    }

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testClosure()

enum TestEnum {
    case empty
    case nonEmpty(TestClass)
}

func testGenericWithEnumNonEmpty() {
let ptr = allocateInternalGenericPtr(of: TestEnum.self)
    
    do {
        let x = TestClass()
        testGenericInit(ptr, to: TestEnum.nonEmpty(x))
    }

    do {
        let y = TestClass()
        // CHECK: TestClass deinitialized!
        testGenericAssign(ptr, from: TestEnum.nonEmpty(y))
    }

    // CHECK-NEXT: TestClass deinitialized!
    testGenericDestroy(ptr, of: TestEnum.self)

    ptr.deallocate()
}

testGenericWithEnumNonEmpty()

#if os(macOS)
func testObjc() {
    let ptr = UnsafeMutablePointer<ObjcWrapper>.allocate(capacity: 1)

    do {
        let x = ObjcWrapper(x: ObjcClass(x: 2))
        testInit(ptr, to: x)

        // CHECK-macosx: value: 2
        print("value: \(ptr.pointee.x.x)")
    }

    do {
        let x = ObjcWrapper(x: ObjcClass(x: 34))
        // CHECK-macosx-NEXT: ObjcClass deinitialized!
        testAssign(ptr, from: x)

        // CHECK-macosx-NEXT: value: 34
        print("value: \(ptr.pointee.x.x)")
    }

    // CHECK-macosx-NEXT: ObjcClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testObjc()

func testWeakObjc() {
    let ptr = UnsafeMutablePointer<WeakObjcWrapper>.allocate(capacity: 1)

    do {
        let ref = ObjcClass(x: 2)

        withExtendedLifetime(ref) {
            testInit(ptr, to: WeakObjcWrapper(x: ref))

            guard let x = ptr.pointee.x else {
                fatalError("Weak reference prematurely destroyed")
            }

            // CHECK-macosx: value: 2
            print("value: \(x.x)")
        }
    }

    do {
        let ref = ObjcClass(x: 34)

        withExtendedLifetime(ref) {
            // CHECK-macosx-NEXT: ObjcClass deinitialized!
            testAssign(ptr, from: WeakObjcWrapper(x: ref))

            guard let x = ptr.pointee.x else {
                fatalError("Weak reference prematurely destroyed")
            }

            // CHECK-macosx-NEXT: value: 34
            print("value: \(x.x)")
        }
    }

    // CHECK-macosx-NEXT: ObjcClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testWeakObjc()

func testUnownedObjc() {
    let ptr = UnsafeMutablePointer<UnownedObjcWrapper>.allocate(capacity: 1)

    do {
        let ref = ObjcClass(x: 2)
        withExtendedLifetime(ref) {
            testInit(ptr, to: UnownedObjcWrapper(x: ref))

            // CHECK-macosx: value: 2
            print("value: \(ptr.pointee.x.x)")
        }
    }

    do {
        let ref = ObjcClass(x: 34)
        
        withExtendedLifetime(ref) {
            // CHECK-macosx-NEXT: ObjcClass deinitialized!
            testAssign(ptr, from: UnownedObjcWrapper(x: ref))

            // CHECK-macosx-NEXT: value: 34
            print("value: \(ptr.pointee.x.x)")
        }
    }

    // CHECK-macosx-NEXT: ObjcClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testUnownedObjc()
#endif
