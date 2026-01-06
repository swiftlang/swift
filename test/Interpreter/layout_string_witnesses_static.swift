// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-future-triple -O -I %S/Inputs/CTypes -enable-experimental-feature LayoutStringValueWitnesses -enable-layout-string-value-witnesses -parse-stdlib -emit-module -emit-module-path=%t/layout_string_witnesses_types.swiftmodule %S/Inputs/layout_string_witnesses_types.swift

// NOTE: We have to build this as dylib to turn private external symbols into local symbols, so we can observe potential issues with linkage
// RUN: %target-build-swift-dylib(%t/%target-library-name(layout_string_witnesses_types)) -target %target-future-triple -O -I %S/Inputs/CTypes -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnesses -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -parse-stdlib -parse-as-library %S/Inputs/layout_string_witnesses_types.swift
// RUN: %target-codesign %t/%target-library-name(layout_string_witnesses_types)
// RUN: %target-swift-frontend -target %target-future-triple -O -enable-experimental-feature LayoutStringValueWitnesses -enable-layout-string-value-witnesses -enable-library-evolution -emit-module -emit-module-path=%t/layout_string_witnesses_types_resilient.swiftmodule %S/Inputs/layout_string_witnesses_types_resilient.swift
// RUN: %target-build-swift -target %target-future-triple -O -g -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnesses -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-library-evolution -c -parse-as-library -o %t/layout_string_witnesses_types_resilient.o %S/Inputs/layout_string_witnesses_types_resilient.swift
// RUN: %target-build-swift -target %target-future-triple -O -g -Xfrontend -enable-experimental-feature -Xfrontend LayoutStringValueWitnesses -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-type-layout -Xfrontend -parse-stdlib -module-name layout_string_witnesses_static -llayout_string_witnesses_types -L%t %t/layout_string_witnesses_types_resilient.o -I %t -o %t/main %s %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(layout_string_witnesses_types) | %FileCheck %s --check-prefix=CHECK -check-prefix=CHECK-%target-os

// REQUIRES: executable_test
// REQUIRES: swift_feature_LayoutStringValueWitnesses

// Requires runtime functions added in Swift 5.9.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Swift
import layout_string_witnesses_types
import layout_string_witnesses_types_resilient

class TestClass {
    init() {}

    deinit {
        print("TestClass deinitialized!")
    }
}

func testSimple() {
    let ptr = UnsafeMutablePointer<Simple>.allocate(capacity: 1)

    // initWithCopy
    do {
        let x = Simple(x: 3, y: SimpleClass(x: 23), z: SimpleBig())
        testInit(ptr, to: x)
    }

    // CHECK: 3 - 23
    print("\(ptr.pointee.x) - \(ptr.pointee.y.x)")

    // assignWithTake
    do {
        let y = Simple(x: 2, y: SimpleClass(x: 1), z: SimpleBig())

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: 2 - 1
    print("\(ptr.pointee.x) - \(ptr.pointee.y.x)")

    // assignWithCopy
    do {
        var z = Simple(x: 23, y: SimpleClass(x: 5), z: SimpleBig())

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssignCopy(ptr, from: &z)
    }

    // CHECK-NEXT: 23 - 5
    print("\(ptr.pointee.x) - \(ptr.pointee.y.x)")

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // destroy
    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testSimple()

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

            // NOTE: There is still a strong reference to it here
            // CHECK-NEXT: Before deinit
            print("Before deinit")
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

            // NOTE: There is still a strong reference to it here
            // CHECK-NEXT: Before deinit
            print("Before deinit")
        }
    }

    do {
        let ref = SimpleClass(x: 23)

        withExtendedLifetime(ref) {

            // CHECK-NEXT: SimpleClass deinitialized!
            var wrapper = WeakNativeWrapper(x: ref)
            testAssignCopy(ptr, from: &wrapper)

            guard let x = ptr.pointee.x else {
                fatalError("Weak reference prematurely destroyed")
            }

            // CHECK-NEXT: value: 23
            print("value: \(x.x)")

            // NOTE: There is still a strong reference to it here
            // CHECK-NEXT: Before deinit
            print("Before deinit")
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

            // NOTE: There is still a strong reference to it here
            // CHECK-NEXT: Before deinit
            print("Before deinit")
        }
    }

    do {
        let ref = SimpleClass(x: 34)

        withExtendedLifetime(ref) {
            // CHECK-NEXT: SimpleClass deinitialized!
            testAssign(ptr, from: UnownedNativeWrapper(x: ref))

            // CHECK-NEXT: value: 34
            print("value: \(ptr.pointee.x.x)")

            // NOTE: There is still a strong reference to it here
            // CHECK-NEXT: Before deinit
            print("Before deinit")
        }
    }

    do {
        let ref = SimpleClass(x: 23)

        withExtendedLifetime(ref) {
            // CHECK-NEXT: SimpleClass deinitialized!
            var wrapper = UnownedNativeWrapper(x: ref)
            testAssignCopy(ptr, from: &wrapper)

            // CHECK-NEXT: value: 23
            print("value: \(ptr.pointee.x.x)")

            // NOTE: There is still a strong reference to it here
            // CHECK-NEXT: Before deinit
            print("Before deinit")
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
        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: ClosureWrapper(f: { print("value: \(ref.x)") }))

        // CHECK-NEXT: value: 34
        ptr.pointee.f()
    }

    do {
        let ref = SimpleClass(x: 23)
        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        var wrapper = ClosureWrapper(f: { print("value: \(ref.x)") })
        testAssignCopy(ptr, from: &wrapper)

        // CHECK-NEXT: value: 23
        ptr.pointee.f()
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testClosure()

class ClassWithSomeProtocol: SomeProtocol {
    deinit {
        print("ClassWithSomeProtocol deinitialized!")
    }
}

func testExistentialClass() {
    let ptr = UnsafeMutablePointer<ExistentialWrapper>.allocate(capacity: 1)

    do {
        let x = ClassWithSomeProtocol()
        testInit(ptr, to: ExistentialWrapper(x: x))
    }

    do {
        let y = ClassWithSomeProtocol()

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ClassWithSomeProtocol deinitialized!
        testAssign(ptr, from: ExistentialWrapper(x: y))
    }

    do {
        let z = ClassWithSomeProtocol()

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ClassWithSomeProtocol deinitialized!
        var wrapper = ExistentialWrapper(x: z)
        testAssignCopy(ptr, from: &wrapper)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: ClassWithSomeProtocol deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testExistentialClass()

struct StructWithSomeProtocolInline: SomeProtocol {
    let y: Int = 0
    let x: SimpleClass
}

struct StructWithSomeProtocolInlineShifted: SomeProtocol {
    let y: Int = 0
    let y2: Int = 0
    let x: SimpleClass
}

func testExistentialStructInline() {
    let ptr = UnsafeMutablePointer<ExistentialWrapper>.allocate(capacity: 1)

    do {
        let x = StructWithSomeProtocolInline(x: SimpleClass(x: 23))
        testInit(ptr, to: createExistentialWrapper(x))
    }

    do {
        let y = StructWithSomeProtocolInline(x: SimpleClass(x: 32))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: createExistentialWrapper(y))
    }

    do {
        let z = StructWithSomeProtocolInline(x: SimpleClass(x: 32))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        var wrapper = createExistentialWrapper(z)
        testAssignCopy(ptr, from: &wrapper)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testExistentialStructInline()

struct StructWithSomeProtocolBox: SomeProtocol {
    let y: Int = 0
    let x: SimpleClass
    let z: Int = 0
    let zz: Int = 0
    let zzz: Int = 0
}

func testExistentialStructBox() {
    let ptr = UnsafeMutablePointer<ExistentialWrapper>.allocate(capacity: 1)

    do {
        let x = StructWithSomeProtocolBox(x: SimpleClass(x: 23))
        testInit(ptr, to: createExistentialWrapper(x))
    }

    do {
        let y = StructWithSomeProtocolBox(x: SimpleClass(x: 32))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: createExistentialWrapper(y))
    }

    do {
        let z = StructWithSomeProtocolBox(x: SimpleClass(x: 32))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        var wrapper = createExistentialWrapper(z)
        testAssignCopy(ptr, from: &wrapper)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testExistentialStructBox()

func testExistentialStructTypeChange() {
    let ptr = UnsafeMutablePointer<ExistentialWrapper>.allocate(capacity: 1)

    do {
        let x = StructWithSomeProtocolInline(x: SimpleClass(x: 23))
        testInit(ptr, to: createExistentialWrapper(x))
    }

    do {
        let y = StructWithSomeProtocolInlineShifted(x: SimpleClass(x: 32))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        var wrapper = createExistentialWrapper(y)
        testAssignCopy(ptr, from: &wrapper)
    }

    do {
        let z = StructWithSomeProtocolBox(x: SimpleClass(x: 32))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        var wrapper = createExistentialWrapper(z)
        testAssignCopy(ptr, from: &wrapper)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testExistentialStructTypeChange()

func testAnyWrapper() {
    let ptr = UnsafeMutablePointer<AnyWrapper>.allocate(capacity: 1)

    do {
        let x = TestClass()
        testInit(ptr, to: AnyWrapper(y: x, z: SimpleClass(x: 23)))
    }

    do {
        let y = TestClass()

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: TestClass deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: AnyWrapper(y: y, z: SimpleClass(x: 32)))
    }

    do {
        let z = TestClass()

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: TestClass deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        var wrapper = AnyWrapper(y: z, z: SimpleClass(x: 32))
        testAssignCopy(ptr, from: &wrapper)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: TestClass deinitialized!
    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testAnyWrapper()

class ClassWithABC: A, B, C {
    deinit {
        print("ClassWithABC deinitialized!")
    }
}

func testMultiProtocolExistential() {
    let ptr = UnsafeMutablePointer<MultiProtocolExistentialWrapper>.allocate(capacity: 1)

    do {
        let x = ClassWithABC()
        testInit(ptr, to: MultiProtocolExistentialWrapper(y: x, z: SimpleClass(x: 23)))
    }

    do {
        let y = ClassWithABC()

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ClassWithABC deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: MultiProtocolExistentialWrapper(y: y, z: SimpleClass(x: 32)))
    }

    do {
        let z = ClassWithABC()

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ClassWithABC deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        var wrapper = MultiProtocolExistentialWrapper(y: z, z: SimpleClass(x: 32))
        testAssignCopy(ptr, from: &wrapper)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: ClassWithABC deinitialized!
    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testMultiProtocolExistential()

class ClassWithSomeClassProtocol: SomeClassProtocol {
    deinit {
        print("ClassWithSomeClassProtocol deinitialized!")
    }
}

func testExistentialReference() {
    let ptr = UnsafeMutablePointer<ExistentialRefWrapper>.allocate(capacity: 1)

    do {
        let x = ClassWithSomeClassProtocol()
        testInit(ptr, to: ExistentialRefWrapper(x: x))
    }

    do {
        let y = ClassWithSomeClassProtocol()

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ClassWithSomeClassProtocol deinitialized!
        testAssign(ptr, from: ExistentialRefWrapper(x: y))
    }

    do {
        let z = ClassWithSomeClassProtocol()

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: ClassWithSomeClassProtocol deinitialized!
        var wrapper = ExistentialRefWrapper(x: z)
        testAssignCopy(ptr, from: &wrapper)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: ClassWithSomeClassProtocol deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testExistentialReference()

func testSinglePayloadSimpleClassEnum() {
    let ptr = UnsafeMutablePointer<SinglePayloadSimpleClassEnum>.allocate(capacity: 1)

    do {
        let x = SinglePayloadSimpleClassEnum.nonEmpty(SimpleClass(x: 23))
        testInit(ptr, to: x)
    }

    do {
        // CHECK: Value: 23
        if case .nonEmpty(let c) = ptr.pointee {
            print("Value: \(c.x)")
        }

        let y = SinglePayloadSimpleClassEnum.nonEmpty(SimpleClass(x: 28))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    do {
        // CHECK: Value: 28
        if case .nonEmpty(let c) = ptr.pointee {
            print("Value: \(c.x)")
        }

        var z = SinglePayloadSimpleClassEnum.nonEmpty(SimpleClass(x: 32))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssignCopy(ptr, from: &z)
    }

    // CHECK-NEXT: Value: 32
    if case .nonEmpty(let c) = ptr.pointee {
        print("Value: \(c.x)")
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testSinglePayloadSimpleClassEnum()

func testSinglePayloadSimpleClassEnumEmpty() {
    let ptr = UnsafeMutablePointer<SinglePayloadSimpleClassEnum>.allocate(capacity: 1)

    do {
        let x = SinglePayloadSimpleClassEnum.empty0
        testInit(ptr, to: x)
    }

    do {
        let y = SinglePayloadSimpleClassEnum.empty1

        // CHECK: Before deinit
        print("Before deinit")

        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    testDestroy(ptr)

    ptr.deallocate()
}

testSinglePayloadSimpleClassEnumEmpty()

func testContainsSinglePayloadSimpleClassEnum() {
    let ptr = UnsafeMutablePointer<ContainsSinglePayloadSimpleClassEnum>.allocate(capacity: 1)

    do {
        let x = ContainsSinglePayloadSimpleClassEnum(x: SinglePayloadSimpleClassEnum.nonEmpty(SimpleClass(x: 23)), y: TestClass())
        testInit(ptr, to: x)
    }

    do {
        // CHECK: Value: 23
        if case .nonEmpty(let c) = ptr.pointee.x {
            print("Value: \(c.x)")
        }

        let y = ContainsSinglePayloadSimpleClassEnum(x: SinglePayloadSimpleClassEnum.nonEmpty(SimpleClass(x: 28)), y: TestClass())

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        // CHECK-NEXT: TestClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Value: 28
    if case .nonEmpty(let c) = ptr.pointee.x {
        print("Value: \(c.x)")
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    // CHECK-NEXT: TestClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testContainsSinglePayloadSimpleClassEnum()

func testContainsSinglePayloadSimpleClassEnumEmpty() {
    let ptr = UnsafeMutablePointer<ContainsSinglePayloadSimpleClassEnum>.allocate(capacity: 1)

    do {
        let x = ContainsSinglePayloadSimpleClassEnum(x: SinglePayloadSimpleClassEnum.empty0, y: TestClass())
        testInit(ptr, to: x)
    }

    do {
        let y = ContainsSinglePayloadSimpleClassEnum(x: SinglePayloadSimpleClassEnum.empty1, y: TestClass())

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: TestClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: TestClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testContainsSinglePayloadSimpleClassEnumEmpty()

func testSinglePayloadAnyHashableEnum() {
    let ptr = UnsafeMutablePointer<SinglePayloadAnyHashableEnum>.allocate(capacity: 1)

    do {
        let x = SinglePayloadAnyHashableEnum.empty0
        testInit(ptr, to: x)
    }

    do {
        // CHECK: empty0
        if case .empty0 = ptr.pointee {
            print("empty0")
        }

        let y = SinglePayloadAnyHashableEnum.empty0

        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: empty0
    if case .empty0 = ptr.pointee {
        print("empty0")
    }

    testDestroy(ptr)

    ptr.deallocate()
}

testSinglePayloadAnyHashableEnum()

func testMultiPayloadEnum() {
    let ptr = UnsafeMutablePointer<MultiPayloadEnumWrapper>.allocate(capacity: 1)

    do {
        let x = MultiPayloadEnumWrapper(x: .d, y: SimpleClass(x: 23))
        testInit(ptr, to: x)
    }

    do {
        let y = MultiPayloadEnumWrapper(x: .d, y: SimpleClass(x: 28))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testMultiPayloadEnum()

func testMultiPayloadEnumMultiLarge() {
    let ptr = UnsafeMutablePointer<MultiPayloadEnumMultiLarge>.allocate(capacity: 1)

    do {
        let x = MultiPayloadEnumMultiLarge.nonEmpty(
            0, SimpleClass(x: 1), 2, SimpleClass(x: 3), 4, true,
            SimpleClass(x: 6), false, SimpleClass(x: 8), true)
        testInit(ptr, to: x)
    }

    do {
        var y = MultiPayloadEnumMultiLarge.nonEmpty(
            10, SimpleClass(x: 11), 12, SimpleClass(x: 13), 14, false,
            SimpleClass(x: 6), true, SimpleClass(x: 8), false)

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        testAssignCopy(ptr, from: &y)
    }

    do {
        var z = MultiPayloadEnumMultiLarge.nonEmpty2(
            SimpleClass(x: 20), 21, 22, SimpleClass(x: 23), 24, true,
            SimpleClass(x: 26), false, SimpleClass(x: 28), true)

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        // CHECK-NEXT: SimpleClass deinitialized!
        testAssignCopy(ptr, from: &z)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    // CHECK-NEXT: SimpleClass deinitialized!
    // CHECK-NEXT: SimpleClass deinitialized!
    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testMultiPayloadEnumMultiLarge()

func testNullableRefEnum() {
    let ptr = UnsafeMutablePointer<NullableRefEnum>.allocate(capacity: 1)

    do {
        let x = NullableRefEnum.nonEmpty(SimpleClass(x: 23))
        testInit(ptr, to: x)
    }

    do {
        let y = NullableRefEnum.nonEmpty(SimpleClass(x: 28))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testNullableRefEnum()

func testForwardToPayloadEnum() {
    let ptr = UnsafeMutablePointer<ForwardToPayloadEnum>.allocate(capacity: 1)

    do {
        let x = ForwardToPayloadEnum.nonEmpty(SimpleClass(x: 23), 43)
        testInit(ptr, to: x)
    }

    do {
        let y = ForwardToPayloadEnum.nonEmpty(SimpleClass(x: 28), 65)

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testForwardToPayloadEnum()

struct InternalEnumWrapperWrapper {
    let x: InternalEnumWrapper
}

func testInternalEnumWrapper() {
    let ptr = UnsafeMutablePointer<InternalEnumWrapperWrapper>.allocate(capacity: 1)

    do {
        let x = InternalEnumWrapper(x: SimpleClass(x: 23))
        testInit(ptr, to: .init(x: x))
    }

    do {
        let y = InternalEnumWrapper(x: SimpleClass(x: 28))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: .init(x: y))
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testInternalEnumWrapper()

func testSinglePayloadEnumExtraTagBytesWrapper() {
    let ptr = UnsafeMutablePointer<SinglePayloadEnumExtraTagBytesWrapper>.allocate(capacity: 1)

    do {
        let x = SinglePayloadEnumExtraTagBytesWrapper(x: .empty0, y: SimpleClass(x: 23))
        testInit(ptr, to: x)
    }

    do {
        let y = SinglePayloadEnumExtraTagBytesWrapper(x: .empty0, y: SimpleClass(x: 28))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testSinglePayloadEnumExtraTagBytesWrapper()

func testSinglePayloadEnumManyXI() {
    let ptr = UnsafeMutablePointer<SinglePayloadEnumManyXI>.allocate(capacity: 1)

    do {
        let x = SinglePayloadEnumManyXI.nonEmpty(Builtin.zeroInitializer(), SimpleClass(x: 23))
        testInit(ptr, to: x)
    }

    do {
        let y = SinglePayloadEnumManyXI.nonEmpty(Builtin.zeroInitializer(), SimpleClass(x: 28))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testSinglePayloadEnumManyXI()

func testSinglePayloadEnumManyXIEmpty() {
    let ptr = UnsafeMutablePointer<SinglePayloadEnumManyXI>.allocate(capacity: 1)

    do {
        let x = SinglePayloadEnumManyXI.empty0
        testInit(ptr, to: x)
    }

    do {
        let y = SinglePayloadEnumManyXI.empty1

        // CHECK: Before deinit
        print("Before deinit")

        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    testDestroy(ptr)

    ptr.deallocate()
}

testSinglePayloadEnumManyXIEmpty()

func testEnumWithExistential() {
    // Regression test for rdar://117755666
    // A missing call to memcpy in `handleSingleRefCountInitWithCopy`
    // was causing unexpected behavior like wrong enum cases, crashes etc.
    // Without the fix, this test would not release the references.
    struct SomeProtocolImpl: SomeProtocol {
        let x: AnyObject
    }

    let ptr = UnsafeMutablePointer<SinglePayloadEnumExistential>.allocate(capacity: 1)

    do {
        let x = SinglePayloadEnumExistential.b
        testInit(ptr, to: x)
    }

    do {
        var z = SinglePayloadEnumExistential.a(SomeProtocolImpl(x: SimpleClass(x: 23)), SimpleClass(x: 43))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        testAssignCopy(ptr, from: &z)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testEnumWithExistential()

// Regression test for rdar://118606044
func testNotBitwiseTakableBridge() {
    let ptr = UnsafeMutablePointer<NotBitwiseTakableBridge<SimpleClass>>.allocate(capacity: 1)

    // initWithTake
    do {
        let x = NotBitwiseTakableBridge([SimpleClass(x: 23)])
        testInitTake(ptr, to: consume x)
    }

    // assignWithTake
    do {
        let y = NotBitwiseTakableBridge([SimpleClass(x: 33)])

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        var z = NotBitwiseTakableBridge([SimpleClass(x: 43)])

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

testNotBitwiseTakableBridge()

// Regression test for rdar://121868127
func testMultiPayloadEnumNested() {
    let ptr = UnsafeMutablePointer<NestedMultiPayloadOuter>.allocate(capacity: 1)

    do {
        testInit(ptr, to: .b(.b(SimpleClass(x: 23))))
    }

    do {
        let y = NestedMultiPayloadOuter.b(.b(SimpleClass(x: 23)))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testMultiPayloadEnumNested()

struct MyError: Error {
    let x: SimpleClass
}

// Regression test for rdar://122911427
func testMultiPayloadError() {
    let ptr = UnsafeMutablePointer<MultiPayloadError>.allocate(capacity: 1)

    // initWithTake
    do {
        let x = MultiPayloadError.error2(0, MyError(x: SimpleClass(x: 23)))
        testInitTake(ptr, to: consume x)
    }

    // assignWithTake
    do {
        let y = MultiPayloadError.error2(1, MyError(x: SimpleClass(x: 32)))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        var z = MultiPayloadError.error2(2, MyError(x: SimpleClass(x: 41)))

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

    // initWithCopy
    do {
        let x = MultiPayloadError.error3(0, MyError(x: SimpleClass(x: 23)))
        testInit(ptr, to: x)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // destroy
    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testMultiPayloadError()

// Regression test for rdar://127379960
func testMultiPayloadErrorKeepsTagIntact() {
    let ptr = UnsafeMutablePointer<MultiPayloadError>.allocate(capacity: 1)

    // initWithTake
    do {
        let x = MultiPayloadError.error2(0, MyError(x: SimpleClass(x: 23)))
        testInit(ptr, to: x)
    }

    // CHECK: Got error2!
    switch ptr.pointee {
        case .error1: print("Get error1!")
        case .error2: print("Got error2!")
        case .error3: print("Got error3!")
        case .empty: print("Got empty!")
    }

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)
    ptr.deallocate()
}

testMultiPayloadErrorKeepsTagIntact()

func testCTypeAligned() {
    let ptr = UnsafeMutablePointer<CTypeAligned>.allocate(capacity: 1)

    // initWithCopy
    do {
        let x = CTypeAligned(SimpleClass(x: 23))
        testInit(ptr, to: x)
    }

    // assignWithTake
    do {
        let y = CTypeAligned(SimpleClass(x: 1))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        var z = CTypeAligned(SimpleClass(x: 5))

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

testCTypeAligned()

func testCTypeUnderAligned() {
    let ptr = UnsafeMutablePointer<CTypeUnderAligned>.allocate(capacity: 1)

    // initWithCopy
    do {
        let x = CTypeUnderAligned(SimpleClass(x: 23))
        testInit(ptr, to: x)
    }

    // assignWithTake
    do {
        let y = CTypeUnderAligned(SimpleClass(x: 1))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        var z = CTypeUnderAligned(SimpleClass(x: 5))

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

testCTypeUnderAligned()

func testNestedTwoPayload() {
    let ptr = UnsafeMutablePointer<TwoPayloadOuter>.allocate(capacity: 1)

    do {
        let x = TwoPayloadOuter.y(TwoPayloadInner.y(SimpleClass(x: 23)))
        testInit(ptr, to: x)
    }

    do {
        let y = TwoPayloadOuter.y(TwoPayloadInner.y(SimpleClass(x: 1)))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testNestedTwoPayload()

func testMultiPayloadOneExtraTagValue() {
    let ptr = UnsafeMutablePointer<OneExtraTagValue>.allocate(capacity: 1)

    do {
        let x = OneExtraTagValue.y(SimpleClass(x: 23))
        testInit(ptr, to: x)
    }

    do {
        let y = OneExtraTagValue.y(SimpleClass(x: 1))

        // CHECK: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // CHECK-NEXT: Before deinit
    print("Before deinit")

    // CHECK-NEXT: SimpleClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testMultiPayloadOneExtraTagValue()

func testMultiPayloadAnyObject() {
    let ptr = UnsafeMutablePointer<MultiPayloadAnyObject>.allocate(capacity: 1)

    // initWithCopy
    do {
        let x = MultiPayloadAnyObject.y(SimpleClass(x: 0))
        testInit(ptr, to: x)
    }

    // assignWithTake
    do {
        let y = MultiPayloadAnyObject.z(SimpleClass(x: 1))

        // CHECK-NEXT: Before deinit
        print("Before deinit")

        // CHECK-NEXT: SimpleClass deinitialized!
        testAssign(ptr, from: y)
    }

    // assignWithCopy
    do {
        var z = MultiPayloadAnyObject.z(SimpleClass(x: 2))

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

testMultiPayloadAnyObject()

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
        // CHECK-macosx-NEXT: Before deinit
        print("Before deinit")

        // CHECK-macosx-NEXT: ObjcClass deinitialized!
        testAssign(ptr, from: x)

        // CHECK-macosx-NEXT: value: 34
        print("value: \(ptr.pointee.x.x)")
    }

    do {
        var x = ObjcWrapper(x: ObjcClass(x: 34))
        // CHECK-macosx-NEXT: Before deinit
        print("Before deinit")

        // CHECK-macosx-NEXT: ObjcClass deinitialized!
        testAssignCopy(ptr, from: &x)

        // CHECK-macosx-NEXT: value: 34
        print("value: \(ptr.pointee.x.x)")
    }

    // CHECK-macosx-NEXT: Before deinit
    print("Before deinit")

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

            // NOTE: There is still a strong reference to it here
            // CHECK-macosx-NEXT: Before deinit
            print("Before deinit")
        }
    }

    do {
        let ref = ObjcClass(x: 23)

        withExtendedLifetime(ref) {

            // CHECK-macosx-NEXT: ObjcClass deinitialized!
            var wrapper = WeakObjcWrapper(x: ref)
            testAssignCopy(ptr, from: &wrapper)

            guard let x = ptr.pointee.x else {
                fatalError("Weak reference prematurely destroyed")
            }

            // CHECK-macosx-NEXT: value: 23
            print("value: \(x.x)")

            // NOTE: There is still a strong reference to it here
            // CHECK-macosx-NEXT: Before deinit
            print("Before deinit")
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

            // NOTE: There is still a strong reference to it here
            // CHECK-macosx-NEXT: Before deinit
            print("Before deinit")
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

            // NOTE: There is still a strong reference to it here
            // CHECK-macosx-NEXT: Before deinit
            print("Before deinit")
        }
    }

    do {
        let ref = ObjcClass(x: 34)

        withExtendedLifetime(ref) {
            // CHECK-macosx-NEXT: ObjcClass deinitialized!
            testAssign(ptr, from: UnownedObjcWrapper(x: ref))

            // CHECK-macosx-NEXT: value: 34
            print("value: \(ptr.pointee.x.x)")

            // NOTE: There is still a strong reference to it here
            // CHECK-macosx-NEXT: Before deinit
            print("Before deinit")
        }
    }

    do {
        let ref = ObjcClass(x: 23)

        withExtendedLifetime(ref) {
            // CHECK-macosx-NEXT: ObjcClass deinitialized!
            var wrapper = UnownedObjcWrapper(x: ref)
            testAssignCopy(ptr, from: &wrapper)

            // CHECK-macosx-NEXT: value: 23
            print("value: \(ptr.pointee.x.x)")

            // NOTE: There is still a strong reference to it here
            // CHECK-macosx-NEXT: Before deinit
            print("Before deinit")
        }
    }

    // CHECK-macosx-NEXT: ObjcClass deinitialized!
    testDestroy(ptr)

    ptr.deallocate()
}

testUnownedObjc()
#endif
