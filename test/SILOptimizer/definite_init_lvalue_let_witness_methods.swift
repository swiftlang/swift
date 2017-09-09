// RUN: %target-swift-frontend -emit-sil -enable-sil-ownership -disable-objc-attr-requires-foundation-module -verify %s

// High-level tests that DI rejects passing let constants to
// mutating witness methods


// Mark: General Definitions

protocol TestProtocol {
    var foo: Int { get set }
}

struct TestStruct: TestProtocol {
    var foo: Int
}

// Mark: - Case1: Illegally mutating let property of class in initializer

class TestClass {
    let testObject: TestProtocol // expected-note {{change 'let' to 'var' to make it mutable}}
    init() {
        testObject = TestStruct(foo: 42)
        testObject.foo = 666 // expected-error {{cannot perform mutating operation: 'self.testObject' is a 'let' constant}}
    }
}

// Mark: - Case2: Illegally mutating global let constant

let testObject: TestProtocol  // expected-note {{change 'let' to 'var' to make it mutable}}
testObject = TestStruct(foo: 42)

testObject.foo = 666 // expected-error {{cannot perform mutating operation: 'testObject' is a 'let' constant}}

extension TestProtocol {
    mutating func messThingsUp() {
        foo = 666
    }
}

// Mark: - Case3: Illegally muatating let constant in a function scope

let testObject2: TestProtocol  // expected-note {{change 'let' to 'var' to make it mutable}}
testObject2 = TestStruct(foo: 42)
testObject2.messThingsUp() // expected-error {{cannot perform mutating operation: 'testObject2' is a 'let' constant}}

func testFunc() {
    let testObject: TestProtocol // expected-note {{change 'let' to 'var' to make it mutable}}

    testObject = TestStruct(foo: 42)
    testObject.foo = 666 // expected-error {{cannot perform mutating operation: 'testObject' is a 'let' constant}}
}

// Mark: - Case4: Illegally passing a let constants property as an inout parameter

let testObject3: TestProtocol  // expected-note {{change 'let' to 'var' to make it mutable}}
testObject3 = TestStruct(foo: 42)

func mutateThis(mutatee: inout Int) {
    mutatee = 666
}
mutateThis(mutatee: &testObject3.foo) // expected-error {{cannot perform mutating operation: 'testObject3' is a 'let' constant}}
