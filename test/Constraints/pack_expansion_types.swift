// RUN: %target-typecheck-verify-swift -enable-experimental-variadic-generics

func returnTuple1<@_typeSequence T>() -> (T...) { fatalError() }

func returnTuple2<@_typeSequence T>() -> (Int, T...) { fatalError() }

func returnTupleLabel1<@_typeSequence T>() -> (x: T...) { fatalError() }

func returnTupleLabel2<@_typeSequence T>() -> (Int, x: T...) { fatalError() }

func returnTupleLabel3<@_typeSequence T>() -> (Int, T..., y: Float) { fatalError() }
// expected-note@-1 {{in call to function 'returnTupleLabel3()'}}

func returnTupleLabel4<@_typeSequence T>() -> (Int, x: T..., y: Float) { fatalError() }

func returnTupleLabel5<@_typeSequence T, @_typeSequence U>() -> (Int, T..., y: U...) { fatalError() }
// expected-note@-1 {{in call to function 'returnTupleLabel5()'}}

func returnTupleLabel6<@_typeSequence T, @_typeSequence U>() -> (Int, x: T..., y: U...) { fatalError() }

func concreteReturnTupleValid() {
  let _: () = returnTuple1()
  let _: (_: Int) = returnTuple1()
  let _: (Int, String) = returnTuple1()

  let _: (_: Int) = returnTuple2()
  let _: (Int, String) = returnTuple2()
  let _: (Int, String, Float) = returnTuple2()

  let _: () = returnTupleLabel1()
  let _: (x: Int) = returnTupleLabel1()
  let _: (x: Int, String) = returnTupleLabel1()
  let _: (x: Int, String, Float) = returnTupleLabel1()

  let _: (_: Int) = returnTupleLabel2()
  let _: (Int, x: Int) = returnTupleLabel2()
  let _: (Int, x: Int, String) = returnTupleLabel2()
  let _: (Int, x: Int, String, Float) = returnTupleLabel2()

  let _: (Int, y: Float) = returnTupleLabel3()
  let _: (Int, Int, y: Float) = returnTupleLabel3()
  let _: (Int, Int, String, y: Float) = returnTupleLabel3()

  let _: (Int, y: Float) = returnTupleLabel4()
  let _: (Int, x: Int, y: Float) = returnTupleLabel4()
  let _: (Int, x: Int, String, y: Float) = returnTupleLabel4()

  let _: (_: Int) = returnTupleLabel5()
  let _: (Int, Int) = returnTupleLabel5()
  let _: (Int, Int, String) = returnTupleLabel5()
  let _: (Int, y: Double, Float) = returnTupleLabel5()
  let _: (Int, Int, String, Float, y: Double) = returnTupleLabel5()

  let _: (_: Int) = returnTupleLabel6()
  let _: (Int, x: Int) = returnTupleLabel6()
  let _: (Int, x: Int, String) = returnTupleLabel6()
  let _: (Int, y: Double, Float) = returnTupleLabel6()
  let _: (Int, x: Int, String, Float, y: Double) = returnTupleLabel6()
}

func concreteReturnTypeInvalid() {
  let _: (x: Int) = returnTuple1() // expected-error {{type of expression is ambiguous without more context}}
  let _: () = returnTuple2() // expected-error {{type of expression is ambiguous without more context}}
  let _: (x: Int) = returnTupleLabel3() // expected-error {{type of expression is ambiguous without more context}}
  let _: (Int, Int, y: Float) = returnTupleLabel4() // expected-error {{type of expression is ambiguous without more context}}
  let _: () = returnTupleLabel5() // expected-error {{type of expression is ambiguous without more context}} 
}

func genericReturnTupleValid<@_typeSequence T>(_: T...) {
  let _: (T...) = returnTuple1()
  let _: (Int, T...) = returnTuple1()

  let _: (Int, T...) = returnTuple2()
  let _: (Int, String, T...) = returnTuple2()

  let _: (x: T...) = returnTupleLabel1()
  let _: (x: Int, T...) = returnTupleLabel1()

  let _: (Int, x: T...) = returnTupleLabel2()
  let _: (Int, x: String, T...) = returnTupleLabel2()

  let _: (Int, T..., y: Float) = returnTupleLabel3()
  let _: (Int, String, T..., y: Float) = returnTupleLabel3()

  let _: (Int, x: T..., y: Float) = returnTupleLabel4()
  let _: (Int, x: String, T..., y: Float) = returnTupleLabel4()

  let _: (Int, T..., y: T...) = returnTupleLabel5()
  let _: (Int, String, T..., y: Float, T...) = returnTupleLabel5()

  let _: (Int, x: T..., y: T...) = returnTupleLabel6()
  let _: (Int, x: String, T..., y: Float, T...) = returnTupleLabel6()
}

func genericReturnTupleInvalid<@_typeSequence T>(_: T...) {
  let _: (x: T...) = returnTuple1() // expected-error {{type of expression is ambiguous without more context}}
  let _: (x: Int, T...) = returnTuple1() // expected-error {{type of expression is ambiguous without more context}}

  let _: (Int, x: T...) = returnTuple2() // expected-error {{type of expression is ambiguous without more context}}
  let _: (Int, x: String, T...) = returnTuple2() // expected-error {{type of expression is ambiguous without more context}}

  let _: (y: T...) = returnTupleLabel1() // expected-error {{type of expression is ambiguous without more context}}
  let _: (y: Int, T...) = returnTupleLabel1() // expected-error {{type of expression is ambiguous without more context}}

  let _: (x: T...) = returnTupleLabel2() // expected-error {{type of expression is ambiguous without more context}}
  let _: (Int, y: String, T...) = returnTupleLabel2() // expected-error {{type of expression is ambiguous without more context}}

  let _: (T..., y: Float) = returnTupleLabel3() // expected-error {{generic parameter 'T' could not be inferred}}
  // expected-error@-1 {{'(Int, T..., y: Float)' is not convertible to '(T..., y: Float)', tuples have a different number of elements}}
  let _: (Int, String, T..., x: Float) = returnTupleLabel3() // expected-error {{type of expression is ambiguous without more context}}

  let _: (T..., y: Float) = returnTupleLabel4() // expected-error {{cannot convert value of type '(Int, y: Float)' to specified type '(T..., y: Float)'}}
  let _: (Int, x: String, y: T...) = returnTupleLabel4() // expected-error {{cannot convert value of type '(Int, x: String, y: Float)' to specified type '(Int, x: String, y: T...)'}}

  let _: (Int, T..., x: T...) = returnTupleLabel5() // expected-error {{type of expression is ambiguous without more context}}
  let _: (T..., y: Float, T...) = returnTupleLabel5() // expected-error {{generic parameter 'T' could not be inferred}}
  // expected-error@-1 {{'(Int, T..., y: Float, T...)' is not convertible to '(T..., y: Float, T...)', tuples have a different number of elements}}

  let _: (T..., y: Int) = returnTupleLabel6() // expected-error {{cannot convert value of type '(Int, y: Int)' to specified type '(T..., y: Int)'}}
}

func returnFunction1<@_typeSequence T>() -> (T...) -> () {}

func returnFunction2<@_typeSequence T>() -> (Int, T...) -> () {} // expected-note {{in call to function 'returnFunction2()'}}

func returnFunction3<@_typeSequence T>() -> (T..., Float) -> () {} // expected-note {{in call to function 'returnFunction3()'}}

func returnFunction4<@_typeSequence T>() -> (Int, T..., Float) -> () {} // expected-note 2{{in call to function 'returnFunction4()'}}

func concreteReturnFunctionValid() {
  let _: () -> () = returnFunction1()
  let _: (Int) -> () = returnFunction1()
  let _: (String, Double) -> () = returnFunction1()

  let _: (Int) -> () = returnFunction2()
  let _: (Int, Int) -> () = returnFunction2()
  let _: (Int, String, Double) -> () = returnFunction2()

  let _: (Float) -> () = returnFunction3()
  let _: (Int, Float) -> () = returnFunction3()
  let _: (String, Double, Float) -> () = returnFunction3()

  let _: (Int, Float) -> () = returnFunction4()
  let _: (Int, Int, Float) -> () = returnFunction4()
  let _: (Int, String, Double, Float) -> () = returnFunction4()
}

func concreteReturnFunctionInvalid() {
  let _: () -> () = returnFunction2() // expected-error {{cannot convert value of type '(Int, T...) -> ()' to specified type '() -> ()'}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  let _: (String) -> () = returnFunction2() // expected-error {{cannot convert value of type '(Int) -> ()' to specified type '(String) -> ()'}}
  let _: (String, Int) -> () = returnFunction2() // expected-error {{cannot convert value of type '(Int, Int) -> ()' to specified type '(String, Int) -> ()'}}

  let _: () -> () = returnFunction3() // expected-error {{cannot convert value of type '(T..., Float) -> ()' to specified type '() -> ()'}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  let _: (Float, Int) -> () = returnFunction3() // expected-error {{cannot convert value of type '(Float, Float) -> ()' to specified type '(Float, Int) -> ()'}}
  let _: (Float, Double, String) -> () = returnFunction3() // expected-error {{cannot convert value of type '(Float, Double, Float) -> ()' to specified type '(Float, Double, String) -> ()'}}

  let _: () -> () = returnFunction4() // expected-error {{cannot convert value of type '(Int, T..., Float) -> ()' to specified type '() -> ()'}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  let _: (Int) -> () = returnFunction4() // expected-error {{cannot convert value of type '(Int, T..., Float) -> ()' to specified type '(Int) -> ()'}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  let _: (Float, Int) -> () = returnFunction4() // expected-error {{cannot convert value of type '(Int, Float) -> ()' to specified type '(Float, Int) -> ()'}}
}