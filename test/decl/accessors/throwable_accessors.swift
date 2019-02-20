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
