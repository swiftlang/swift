// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func returnTuple1<T...>() -> (T...) { fatalError() }

func returnTuple2<T...>() -> (Int, T...) { fatalError() }

func returnTupleLabel1<T...>() -> (x: T...) { fatalError() }

func returnTupleLabel2<T...>() -> (Int, x: T...) { fatalError() }

func returnTupleLabel3<T...>() -> (Int, T..., y: Float) { fatalError() }

func returnTupleLabel4<T...>() -> (Int, x: T..., y: Float) { fatalError() } // expected-note {{in call to function 'returnTupleLabel4()'}}

func returnTupleLabel5<T..., U...>() -> (Int, T..., y: U...) { fatalError() }

func returnTupleLabel6<T..., U...>() -> (Int, x: T..., y: U...) { fatalError() } // expected-note {{in call to function 'returnTupleLabel6()'}}

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

func genericReturnTupleValid<T...>(_: T...) {
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

func genericReturnTupleInvalid<T...>(_: T...) {
  let _: (x: T...) = returnTuple1() // expected-error {{type of expression is ambiguous without more context}}
  let _: (x: Int, T...) = returnTuple1() // expected-error {{type of expression is ambiguous without more context}}

  let _: (Int, x: T...) = returnTuple2() // expected-error {{type of expression is ambiguous without more context}}
  let _: (Int, x: String, T...) = returnTuple2() // expected-error {{type of expression is ambiguous without more context}}

  let _: (y: T...) = returnTupleLabel1() // expected-error {{type of expression is ambiguous without more context}}
  let _: (y: Int, T...) = returnTupleLabel1() // expected-error {{type of expression is ambiguous without more context}}

  let _: (x: T...) = returnTupleLabel2() // expected-error {{type of expression is ambiguous without more context}}
  let _: (Int, y: String, T...) = returnTupleLabel2() // expected-error {{type of expression is ambiguous without more context}}

  let _: (T..., y: Float) = returnTupleLabel3() // expected-error {{type of expression is ambiguous without more context}}

  let _: (Int, String, T..., x: Float) = returnTupleLabel3() // expected-error {{type of expression is ambiguous without more context}}

  let _: (T..., y: Float) = returnTupleLabel4() // expected-error {{'(Int, x: T..., y: Float)' is not convertible to '(T..., y: Float)', tuples have a different number of elements}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _: (Int, x: String, y: T...) = returnTupleLabel4() // expected-error {{cannot convert value of type '(Int, x: String, y: Float)' to specified type '(Int, x: String, y: T...)'}}

  let _: (Int, T..., x: T...) = returnTupleLabel5() // expected-error {{type of expression is ambiguous without more context}}

  let _: (T..., y: Float, T...) = returnTupleLabel5() // expected-error {{type of expression is ambiguous without more context}}

  let _: (T..., y: Int) = returnTupleLabel6() // expected-error {{'(Int, x: T..., y: U...)' is not convertible to '(T..., y: Int)', tuples have a different number of elements}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  // expected-error@-2 {{generic parameter 'U' could not be inferred}}
}

func returnFunction1<T...>() -> (T...) -> () {}

func returnFunction2<T...>() -> (Int, T...) -> () {} // expected-note {{in call to function 'returnFunction2()'}}

func returnFunction3<T...>() -> (T..., Float) -> () {} // expected-note {{in call to function 'returnFunction3()'}}

func returnFunction4<T...>() -> (Int, T..., Float) -> () {} // expected-note 2{{in call to function 'returnFunction4()'}}

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

func patternInstantiationTupleTest1<T...>() -> (Array<T>...) {} // expected-note {{in call to function 'patternInstantiationTupleTest1()'}}
func patternInstantiationTupleTest2<T..., U...>() -> (Dictionary<T, U>...) {}

func patternInstantiationFunctionTest1<T...>() -> (Array<T>...) -> () {}
func patternInstantiationFunctionTest2<T..., U...>() -> (Dictionary<T, U>...) -> () {}

func patternInstantiationConcreteValid() {
  let _: () = patternInstantiationTupleTest1()
  let _: (_: Array<Int>) = patternInstantiationTupleTest1()
  let _: (Array<Int>, Array<String>) = patternInstantiationTupleTest1()
  let _: (Array<Int>, Array<String>, Array<Float>) = patternInstantiationTupleTest1()

  let _: () = patternInstantiationTupleTest2()
  let _: (_: Dictionary<Int, String>) = patternInstantiationTupleTest2()
  let _: (Dictionary<Int, String>, Dictionary<Float, Bool>) = patternInstantiationTupleTest2()
  let _: (Dictionary<Int, String>, Dictionary<Float, Bool>, Dictionary<Double, Character>) = patternInstantiationTupleTest2()

  let _: () -> () = patternInstantiationFunctionTest1()
  let _: (_: Array<Int>) -> () = patternInstantiationFunctionTest1()
  let _: (Array<Int>, Array<String>) -> () = patternInstantiationFunctionTest1()
  let _: (Array<Int>, Array<String>, Array<Float>) -> () = patternInstantiationFunctionTest1()

  let _: () -> () = patternInstantiationFunctionTest2()
  let _: (_: Dictionary<Int, String>) -> () = patternInstantiationFunctionTest2()
  let _: (Dictionary<Int, String>, Dictionary<Float, Bool>) -> () = patternInstantiationFunctionTest2()
  let _: (Dictionary<Int, String>, Dictionary<Float, Bool>, Dictionary<Double, Character>) -> () = patternInstantiationFunctionTest2()
}

func patternInstantiationConcreteInvalid() {
  let _: (_: Set<Int>) = patternInstantiationTupleTest1() // expected-error {{type of expression is ambiguous without more context}}
  let _: (Array<Int>, Set<String>) = patternInstantiationTupleTest1() // expected-error {{type of expression is ambiguous without more context}}
}

func patternInstantiationGenericValid<T..., U...>(t: T..., u: U...) where ((T, U)...): Any, T: Hashable {
  let _: (Array<T>...) = patternInstantiationTupleTest1()
  let _: (Array<T>..., Array<String>) = patternInstantiationTupleTest1()
  let _: (Array<String>, Array<T>...) = patternInstantiationTupleTest1()
  let _: (Array<Int>, Array<T>..., Array<Float>) = patternInstantiationTupleTest1()

  let _: (Dictionary<T, U>...) = patternInstantiationTupleTest2()
  let _: (Dictionary<T, U>..., Dictionary<Float, Bool>) = patternInstantiationTupleTest2()
  let _: (Dictionary<Int, String>, Dictionary<T, U>...) = patternInstantiationTupleTest2()
  let _: (Dictionary<Int, String>, Dictionary<T, U>..., Dictionary<Double, Character>) = patternInstantiationTupleTest2()

  let _: (Array<T>...) -> () = patternInstantiationFunctionTest1()
  let _: (Array<T>..., Array<String>) -> () = patternInstantiationFunctionTest1()
  let _: (Array<String>, Array<T>...) -> () = patternInstantiationFunctionTest1()
  let _: (Array<Int>, Array<T>..., Array<Float>) -> () = patternInstantiationFunctionTest1()

  let _: (Dictionary<T, U>...) -> () = patternInstantiationFunctionTest2()
  let _: (Dictionary<T, U>..., Dictionary<Float, Bool>) -> () = patternInstantiationFunctionTest2()
  let _: (Dictionary<Int, String>, Dictionary<T, U>...) -> () = patternInstantiationFunctionTest2()
  let _: (Dictionary<Int, String>, Dictionary<T, U>..., Dictionary<Double, Character>) -> () = patternInstantiationFunctionTest2()
}

func patternInstantiationGenericInvalid<T...>(t: T...) where T: Hashable {
  let _: (Set<T>...) = patternInstantiationTupleTest1() // expected-error {{cannot convert value of type '(Array<T>...)' to specified type '(Set<T>...)}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _: (Array<T>..., Set<String>) = patternInstantiationTupleTest1() // expected-error {{type of expression is ambiguous without more context}}
}
