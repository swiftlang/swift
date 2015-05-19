// RUN: %target-parse-verify-swift

func test15921520() {
    var x: Int = 0
    func f<T>(x: T) {}
    f(&x) // expected-error{{cannot invoke 'f' with an argument list of type '(inout Int)'}} expected-note{{expected an argument list of type '(T)'}}
}

func test20807269() {
    var x: Int = 0
    func f<T>(x: T) {} // expected-note{{in call to function 'f'}}
    f(1, &x) // expected-error{{type '(IntegerLiteralConvertible, inout Int)' is not materializable}}
}

func test15921530() {
    struct X {}

    func makef<T>() -> (T)->() { // expected-note{{in call to function 'makef'}}
      return {
        x in ()
      }
    }
    var f: (inout X)->() = makef() // expected-error{{argument for generic parameter 'T' could not be inferred}}
}
