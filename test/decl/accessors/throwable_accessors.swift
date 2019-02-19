// RUN: %target-typecheck-verify-swift

extension String: Error {}

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
    let _ = try! instance[0] // Ok

    try instance[0] = 1 // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    try? instance[0] = 1 // Okay
    try! instance[0] = 1 // Okay
}

func testThrowableProperty() {
    var instance = Foo()

    let _ = try instance.arr // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    let _ = try? instance.arr  // Okay
    let _ = try! instance.arr  // Ok

    try instance.arr = [1, 2, 3] // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    try? instance.arr = [1, 2, 3] // Okay
    try! instance.arr = [1, 2, 3] // Okay
}

func testMutatingProperty() {
    var anotherInstance = Foo()

    let _ = try anotherInstance.arr2 // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
    let _ = try? anotherInstance.arr2  // Okay
    let _ = try! anotherInstance.arr2  // Ok

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