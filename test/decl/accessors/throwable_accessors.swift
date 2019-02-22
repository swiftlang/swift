// RUN: %target-typecheck-verify-swift

extension String: Error {}

/// Throwing properties and subscripts ///

struct Foo {
    var _arr: [Int] = [2, 3, 4]

    subscript(x: Int) -> Int {
        get throws {
            if !_arr.indices.contains(x) {
                throw "Accessing invalid index!"
            } else {
                return _arr[x]
            }
        }
        set throws {
            if x < 0 {
                throw "Assigning a value to invalid index!"
            } else {
                _arr[x] = newValue
            }
        }
    }

    var arr: [Int] {
        get throws {
            if _arr.isEmpty {
                throw "Cannot return an empty array of [Int]!"
            } else {
                return _arr
            }
        }
        set throws {
            if newValue.isEmpty {
                throw "Cannot assign an empty array of [Int]!"
            } else {
                _arr = newValue
            }
        }
    }

    var _arr2: [String] = ["Foo", "Bar", "Baz"]
    
    var arr2: [String] {
        mutating get throws {
            if _arr2.isEmpty {
                throw "Cannot return an empty array of [String]!"
            } else {
                return _arr2
            }
        }
        mutating set throws {
            if newValue.isEmpty {
                throw "Cannot assign an empty array of [String]!"
            } else {
                _arr2 = newValue
            }
        }
    }
}

func testThrowableSubscript() {
    var instance = Foo()

    let _ = try instance[0] // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    let _ = try? instance[0] // Okay
    let _ = try! instance[0] // Okay

    try instance[0] = 1 // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    try? instance[0] = 1 // Okay
    try! instance[0] = 1 // Okay
}

func testThrowableProperty() {
    var instance = Foo()

    let _ = try instance.arr // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    let _ = try? instance.arr  // Okay
    let _ = try! instance.arr  // Okay

    try instance.arr = [1, 2, 3] // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    try? instance.arr = [1, 2, 3] // Okay
    try! instance.arr = [1, 2, 3] // Okay
}

func testMutatingProperty() {
    var anotherInstance = Foo()

    let _ = try anotherInstance.arr2 // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    let _ = try? anotherInstance.arr2  // Okay
    let _ = try! anotherInstance.arr2  // Okay

    try anotherInstance.arr2 = ["Foo"] // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    try? anotherInstance.arr2 = ["Foo"] // Okay
    try! anotherInstance.arr2 = ["Foo"] // Okay
}

func testThrowingAccessor() throws {
    var instance = Foo()
    let _ = try instance.arr
    let _ = try instance[0]
    try instance.arr = [1]
    try instance[0] = 1
}

testThrowingAccessor() // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}

try testThrowingAccessor() // Okay
try? testThrowingAccessor() // Okay
try! testThrowingAccessor() // Okay

func testNestedFunctionThatContainsThrowingAccessor() {
    func nested() {
        var instance = Foo()
        let _ = try instance.arr // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
        let _ = try? instance.arr // Okay
        let _ = try! instance.arr // Okay
    }
}

func testNestedFunctionThatThrowsAndContainsThrowingAccessor() {
    func nested() throws {
        var instance = Foo()
        let _ = try instance.arr
    }

    nested() // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
    try nested() // expected-error {{errors thrown from here are not handled}}
    try? nested() // Okay
    try! nested() // Okay
}

func testFunctionThatTakesInOut(_ f: inout Foo) {
    let _ = try f.arr // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    let _ = try? f.arr // Okay
    let _ = try! f.arr // Okay
}

func testThrowingFunctionThatTakesInOut(_ f: inout Foo) throws {
    let _ = try f.arr // Okay
    let _ = try? f.arr // Okay
    let _ = try! f.arr // Okay
}

var instance = Foo()
testThrowingFunctionThatTakesInOut(&instance) // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
try testThrowingFunctionThatTakesInOut(&instance) // Okay
try? testThrowingFunctionThatTakesInOut(&instance) // Okay
try! testThrowingFunctionThatTakesInOut(&instance) // Okay

/// Throwing properties and subscripts in protocols ///

protocol P {
    var a: Int { get throws set throws } // Okay
    var b: Int { get throws set } // Okay
    var c: Int { get set throws } // Okay
    var d: Int { get set } // Okay

    subscript(e: Int) -> Int { get throws set throws } // Okay
    subscript(f: String) -> String { get throws set } // Okay
    subscript(g: Float) -> Float { get set throws } // Okay
    subscript(h: Double) -> Double { get set } // Okay
}

class S: P {
    var a: Int {
        get throws {} // Okay
        set throws {} // Okay
    }

    var b: Int {
        get throws {} // Okay
        set {} // Okay
    }

    var c: Int {
        get {} // Okay
        set throws {} // Okay
    }
    
    var d: Int {
        get {} // Okay
        set {} // Okay
    }

    subscript(e: Int) -> Int {
        get throws {} // Okay
        set throws {} // Okay
    }

    subscript(f: String) -> String {
        get throws {} // Okay
        set {} // Okay
    }

    subscript(g: Float) -> Float {
        get {} // Okay
        set throws {} // Okay
    }

    subscript(h: Double) -> Double {
        get {} // Okay
        set {} // Okay
    }
}

/// Protocols with non-throwing accessors ///

protocol NonThrowingP {
    var foo: Int { get } // expected-note {{requirement 'foo' declared here}}
    var baz: Int { get set } // expected-note {{requirement 'baz' declared here}}
}

struct NonThrowingS: NonThrowingP { // expected-error {{type 'NonThrowingS' does not conform to protocol 'NonThrowingP'}}
    var foo: Int { // expected-error {{cannot satisfy the requirement of a non-throwing accessor with a throwing one}}
        get throws {}
    }

    var baz: Int { // expected-error {{cannot satisfy the requirement of a non-throwing accessor with a throwing one}}
        get {}
        set throws {}
    }
}

/// Class with non-throwing accessor ///

class NonThrowingPropInClass {
  var foo: Int {
    get { return 0 } // expected-note {{overridden declaration is here}}
  }
}

class ThrowingPropInClass: NonThrowingPropInClass {
  override var foo: Int {
    get throws {} // expected-error {{cannot override non-throwing method with throwing method}}
  }
}

/// Protocol with throwing accessor refinement ///

protocol P1 {
  var foo: Int { get throws }
}

protocol Q1: P1 {
  var foo: Int { get }
}

struct S1: Q1 {
  var foo: Int {
    get { return 0 } // Ok
  }
}

struct S2: P1 {
  var foo: Int {
    get { return 0 } // Ok
  }
}

/// Protocol with non-throwing accessor refinement ///

protocol P2 {
  var baz: Int { get } // expected-note {{overridden declaration is here}}
}

protocol Q2: P2 {
  var baz: Int { get throws } // expected-error {{cannot override non-throwing method with throwing method}}
}

struct S3: Q2 {
  var baz: Int {
    get { return 0 }
  }
}

/// Protocol conformance/refinement tests ///

protocol P3 {
  var run: Int { get throws set throws }
}

protocol Q3: P3 {
  var run: Int { get set }
}

struct S4: Q3 {
  private var _run: Int = 0
  var run: Int {
    get { return _run }
    set { _run = newValue }
  }
}

let proto: Q1 = S1()
let _ = proto.foo // Okay
let _ = (proto as Q1).foo // Okay
let _ = (proto as P1).foo  // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
let _ = try (proto as P1).foo // Okay
let _ = try? (proto as P1).foo // Okay
let _ = try! (proto as P1).foo // Okay

let anotherProto: S2 = S2()
let _ = anotherProto.foo // Okay
let _ = (anotherProto as P1).foo // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
let _ = try (anotherProto as P1).foo // Okay
let _ = try? (anotherProto as P1).foo // Okay
let _ = try! (anotherProto as P1).foo // Okay

var oneMoreProto: Q3 = S4()
oneMoreProto.run = 1 // Okay

var oneExtraProto: P3 = S4()
oneExtraProto.run = 2 // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
try oneExtraProto.run = 3 // Okay
try? oneExtraProto.run = 3 // Okay
try! oneExtraProto.run = 3 // Okay

protocol P4 {
  subscript(fizz: Int) -> Int { get throws set throws }
}

protocol Q4: P4 {
  subscript(fizz: Int) -> Int { get set }
}

struct S5: Q4 {
  subscript(fizz: Int) -> Int {
    get { return 0 }
    set {}
  }
}

var subsProto: Q4 = S5()
let _ = subsProto[0] // Okay
let _ = (subsProto as Q4)[0] // Okay
let _ = (subsProto as P4)[0] // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
let _ = try (subsProto as P4)[0] // Okay
let _ = try? (subsProto as P4)[0] // Okay
let _ = try! (subsProto as P4)[0] // Okay
subsProto[0] = 1 // Okay

var subsProto1: P4 = S5()
let _ = subsProto1[0] // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
let _ = try subsProto1[0] // Okay
let _ = try? subsProto1[0] // Okay
let _ = try! subsProto1[0] // Okay
subsProto1[0] = 1 // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}

protocol P5 {
  var bar: Int { get throws set throws }
}

struct S6: P5 {
  var bar: Int {
    get { return 0 }
    set {}
  }
}

func protocolThrowingFunc() {
  let instance: P5 = S6()
  let _ = try instance.bar // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
  let _ = try? instance.bar // Okay
  let _ = try! instance.bar // Okay
}

func protocolThrowingFuncThrows() throws {
  let instance: P5 = S6()
  let _ = try instance.bar // Okay
}

protocol P6 {
  subscript(fizz: Int) -> Int { get throws set throws }
}

struct S7: P6 {
  subscript(fizz: Int) -> Int {
    get { return 0 }
    set { }
  }
}

func protocolThrowingSubscriptFunc() {
  var instance: P6 = S7()

  let _ = instance[0] // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
  let _ = try instance[0] // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
  let _ = try? instance[0] // Okay
  let _ = try! instance[0] // Okay

  instance[1] = 0 // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
  try instance[1] = 0 // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
  try? instance[1] = 0 // Okay
  try! instance[1] = 0 // Okay
}

func protocolThrowingSubscriptFuncThrows() throws {
  var instance: P6 = S7()

  let _ = instance[0] // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
  let _ = try instance[0] // Okay
  let _ = try? instance[0] // Okay
  let _ = try! instance[0] // Okay

  instance[1] = 0 // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
  try instance[1] = 0 // Okay
  try? instance[1] = 0 // Okay
  try! instance[1] = 0 // Okay
}

/// Throw sites not marked with try ///

struct ThrowingStruct1 {
  var a: Int {
    get throws { throw "Error" }
    set throws { throw "Error" }
  }

  var b: Int {
    get {}
    set throws { throw "Error" }
  }

  subscript(x: Int) -> Int {
    get {}
    set throws {}
  }
}

func functionThatUsesThrowingStructProp() {
  var instance = ThrowingStruct1()
  
  let _ = instance.a // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
  instance.a = 1 // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
}

var outsideInstance = ThrowingStruct1()
let _ = outsideInstance.a // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
outsideInstance.a = 1 // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}

let _ = try outsideInstance.a // Okay
let _ = try? outsideInstance.a // Okay
let _ = try! outsideInstance.a // Okay

let _ = outsideInstance.b // Okay
outsideInstance.b = 1 // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
try outsideInstance.b = 1 // Okay
try? outsideInstance.b = 1 // Okay
try! outsideInstance.b = 1 // Okay

let _ = outsideInstance[0] // Okay
outsideInstance[0] = 1 // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
try outsideInstance[0] = 1 // Okay
try? outsideInstance[0] = 1 // Okay
try! outsideInstance[0] = 1 // Okay

struct ThrowingStruct2 {
  subscript(y: Int) -> Int {
    get throws {}
    set throws {}
  }
}

var outsideInstance2 = ThrowingStruct2()

let _ = outsideInstance2[0] // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
let _ = try outsideInstance2[0] // Okay
let _ = try? outsideInstance2[0] // Okay
let _ = try! outsideInstance2[0] // Okay

outsideInstance2[0] = 1 // expected-error {{call can throw but is not marked with 'try'}} // expected-note {{did you mean to use 'try'?}} // expected-note {{did you mean to handle error as optional value?}} // expected-note {{did you mean to disable error propagation?}}
try outsideInstance2[0] = 1 // Okay
try? outsideInstance2[0] = 1 // Okay
try! outsideInstance2[0] = 1 // Okay