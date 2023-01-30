// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func returnTuple1<T...>() -> (repeat each T) { fatalError() }
// expected-note@-1 3 {{in call to function 'returnTuple1()'}}

func returnTuple2<T...>() -> (Int, repeat each T) { fatalError() }
// expected-note@-1 3 {{in call to function 'returnTuple2()'}}

func returnTupleLabel1<T...>() -> (x: repeat each T) { fatalError() }
// expected-note@-1 2 {{in call to function 'returnTupleLabel1()'}}

func returnTupleLabel2<T...>() -> (Int, x: repeat each T) { fatalError() }
// expected-note@-1 2 {{in call to function 'returnTupleLabel2()'}}

func returnTupleLabel3<T...>() -> (Int, repeat each T, y: Float) { fatalError() }
// expected-note@-1 3 {{in call to function 'returnTupleLabel3()'}}

func returnTupleLabel4<T...>() -> (Int, x: repeat each T, y: Float) { fatalError() }
// expected-note@-1 2 {{in call to function 'returnTupleLabel4()'}}

func returnTupleLabel5<T..., U...>() -> (Int, repeat each T, y: repeat each U) { fatalError() }
// expected-note@-1 3 {{in call to function 'returnTupleLabel5()'}}

func returnTupleLabel6<T..., U...>() -> (Int, x: repeat each T, y: repeat each U) { fatalError() }
// expected-note@-1 {{in call to function 'returnTupleLabel6()'}}

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
  let _: (x: Int) = returnTuple1()
  // expected-error@-1 {{cannot convert value of type '(repeat each T)' to specified type '(x: Int)'}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: () = returnTuple2()
  // expected-error@-1 {{'(Int, repeat each T)' is not convertible to '()', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (x: Int) = returnTupleLabel3()
  // expected-error@-1 {{'(Int, repeat each T, y: Float)' is not convertible to '(x: Int)', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (Int, Int, y: Float) = returnTupleLabel4()
  // expected-error@-1 {{cannot convert value of type '(Int, x: repeat each T, y: Float)' to specified type '(Int, Int, y: Float)'}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: () = returnTupleLabel5()
  // expected-error@-1 {{'(Int, repeat each T, y: repeat each U)' is not convertible to '()', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}
  // expected-error@-3 {{generic parameter 'U' could not be inferred}}
}

func genericReturnTupleValid<T...>(_: repeat each T) {
  let _: (repeat each T) = returnTuple1()
  let _: (Int, repeat each T) = returnTuple1()

  let _: (Int, repeat each T) = returnTuple2()
  let _: (Int, String, repeat each T) = returnTuple2()

  let _: (x: repeat each T) = returnTupleLabel1()
  let _: (x: Int, repeat each T) = returnTupleLabel1()

  let _: (Int, x: repeat each T) = returnTupleLabel2()
  let _: (Int, x: String, repeat each T) = returnTupleLabel2()

  let _: (Int, repeat each T, y: Float) = returnTupleLabel3()
  let _: (Int, String, repeat each T, y: Float) = returnTupleLabel3()

  let _: (Int, x: repeat each T, y: Float) = returnTupleLabel4()
  let _: (Int, x: String, repeat each T, y: Float) = returnTupleLabel4()

  let _: (Int, repeat each T, y: repeat each T) = returnTupleLabel5()
  let _: (Int, String, repeat each T, y: Float, repeat each T) = returnTupleLabel5()

  let _: (Int, x: repeat each T, y: repeat each T) = returnTupleLabel6()
  let _: (Int, x: String, repeat each T, y: Float, repeat each T) = returnTupleLabel6()
}

func genericReturnTupleInvalid<T...>(_: repeat each T) {
  let _: (x: repeat each T) = returnTuple1()
  // expected-error@-1 {{cannot convert value of type '(repeat each T)' to specified type '(x: repeat each T)'}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (x: Int, repeat each T) = returnTuple1()
  // expected-error@-1 {{'(repeat each T)' is not convertible to '(x: Int, repeat each T)', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (Int, x: repeat each T) = returnTuple2()
  // expected-error@-1 {{cannot convert value of type '(Int, repeat each T)' to specified type '(Int, x: repeat each T)'}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (Int, x: String, repeat each T) = returnTuple2()
  // expected-error@-1 {{'(Int, repeat each T)' is not convertible to '(Int, x: String, repeat each T)', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (y: repeat each T) = returnTupleLabel1()
  // expected-error@-1 {{cannot convert value of type '(x: repeat each T)' to specified type '(y: repeat each T)'}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (y: Int, repeat each T) = returnTupleLabel1()
  // expected-error@-1 {{'(x: repeat each T)' is not convertible to '(y: Int, repeat each T)', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (x: repeat each T) = returnTupleLabel2()
  // expected-error@-1 {{'(Int, x: repeat each T)' is not convertible to '(x: repeat each T)', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (Int, y: String, repeat each T) = returnTupleLabel2()
  // expected-error@-1 {{'(Int, x: repeat each T)' is not convertible to '(Int, y: String, repeat each T)', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (repeat each T, y: Float) = returnTupleLabel3()
  // expected-error@-1 {{'(Int, repeat each T, y: Float)' is not convertible to '(repeat each T, y: Float)', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (Int, String, repeat each T, x: Float) = returnTupleLabel3()
  // expected-error@-1 {{'(Int, repeat each T, y: Float)' is not convertible to '(Int, String, repeat each T, x: Float)', tuples have a different number of elements}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}

  let _: (repeat each T, y: Float) = returnTupleLabel4() // expected-error {{'(Int, x: repeat each T, y: Float)' is not convertible to '(repeat each T, y: Float)', tuples have a different number of elements}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _: (Int, x: String, y: repeat each T) = returnTupleLabel4() // expected-error {{cannot convert value of type '(Int, x: String, y: Float)' to specified type '(Int, x: String, y: repeat each T)'}}

  let _: (Int, repeat each T, x: repeat each T) = returnTupleLabel5()
  // expected-error@-1 {{cannot convert value of type '(Int, repeat each T, y: repeat each U)' to specified type '(Int, repeat each T, x: repeat each T)'}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}
  // expected-error@-3 {{generic parameter 'U' could not be inferred}}

  let _: (repeat each T, y: Float, repeat each T) = returnTupleLabel5()
  // expected-error@-1 {{cannot convert value of type '(Int, repeat each T, y: repeat each U)' to specified type '(repeat each T, y: Float, repeat each T)'}}
  // expected-error@-2 {{generic parameter 'T' could not be inferred}}
  // expected-error@-3 {{generic parameter 'U' could not be inferred}}

  let _: (repeat each T, y: Int) = returnTupleLabel6() // expected-error {{'(Int, x: repeat each T, y: repeat each U)' is not convertible to '(repeat each T, y: Int)', tuples have a different number of elements}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  // expected-error@-2 {{generic parameter 'U' could not be inferred}}
}

func returnFunction1<T...>() -> (repeat each T) -> () {}

func returnFunction2<T...>() -> (Int, repeat each T) -> () {} // expected-note {{in call to function 'returnFunction2()'}}

func returnFunction3<T...>() -> (repeat each T, Float) -> () {} // expected-note {{in call to function 'returnFunction3()'}}

func returnFunction4<T...>() -> (Int, repeat each T, Float) -> () {} // expected-note 2{{in call to function 'returnFunction4()'}}

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
  let _: () -> () = returnFunction2() // expected-error {{cannot convert value of type '(Int, repeat each T) -> ()' to specified type '() -> ()'}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _: (String) -> () = returnFunction2() // expected-error {{cannot convert value of type '(Int) -> ()' to specified type '(String) -> ()'}}
  let _: (String, Int) -> () = returnFunction2() // expected-error {{cannot convert value of type '(Int, Int) -> ()' to specified type '(String, Int) -> ()'}}

  let _: () -> () = returnFunction3() // expected-error {{cannot convert value of type '(repeat each T, Float) -> ()' to specified type '() -> ()'}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _: (Float, Int) -> () = returnFunction3() // expected-error {{cannot convert value of type '(Float, Float) -> ()' to specified type '(Float, Int) -> ()'}}
  let _: (Float, Double, String) -> () = returnFunction3() // expected-error {{cannot convert value of type '(Float, Double, Float) -> ()' to specified type '(Float, Double, String) -> ()'}}

  let _: () -> () = returnFunction4() // expected-error {{cannot convert value of type '(Int, repeat each T, Float) -> ()' to specified type '() -> ()'}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _: (Int) -> () = returnFunction4() // expected-error {{cannot convert value of type '(Int, repeat each T, Float) -> ()' to specified type '(Int) -> ()'}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _: (Float, Int) -> () = returnFunction4() // expected-error {{cannot convert value of type '(Int, Float) -> ()' to specified type '(Float, Int) -> ()'}}
}

func patternInstantiationTupleTest1<T...>() -> (repeat Array<each T>) {} // expected-note {{in call to function 'patternInstantiationTupleTest1()'}}
func patternInstantiationTupleTest2<T..., U...>() -> (repeat Dictionary<each T, each U>) {}

func patternInstantiationFunctionTest1<T...>() -> (repeat Array<each T>) -> () {}
func patternInstantiationFunctionTest2<T..., U...>() -> (repeat Dictionary<each T, each U>) -> () {}

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

func patternInstantiationGenericValid<T..., U...>(t: repeat each T, u: repeat each U) where (repeat (each T, each U)): Any, T: Hashable {
  let _: (repeat Array<each T>) = patternInstantiationTupleTest1()
  let _: (repeat Array<each T>, Array<String>) = patternInstantiationTupleTest1()
  let _: (Array<String>, repeat Array<each T>) = patternInstantiationTupleTest1()
  let _: (Array<Int>, repeat Array<each T>, Array<Float>) = patternInstantiationTupleTest1()

  let _: (repeat Dictionary<each T, each U>) = patternInstantiationTupleTest2()
  let _: (repeat Dictionary<each T, each U>, Dictionary<Float, Bool>) = patternInstantiationTupleTest2()
  let _: (Dictionary<Int, String>, repeat Dictionary<each T, each U>) = patternInstantiationTupleTest2()
  let _: (Dictionary<Int, String>, repeat Dictionary<each T, each U>, Dictionary<Double, Character>) = patternInstantiationTupleTest2()

  let _: (repeat Array<each T>) -> () = patternInstantiationFunctionTest1()
  let _: (repeat Array<each T>, Array<String>) -> () = patternInstantiationFunctionTest1()
  let _: (Array<String>, repeat Array<each T>) -> () = patternInstantiationFunctionTest1()
  let _: (Array<Int>, repeat Array<each T>, Array<Float>) -> () = patternInstantiationFunctionTest1()

  let _: (repeat Dictionary<each T, each U>) -> () = patternInstantiationFunctionTest2()
  let _: (repeat Dictionary<each T, each U>, Dictionary<Float, Bool>) -> () = patternInstantiationFunctionTest2()
  let _: (Dictionary<Int, String>, repeat Dictionary<each T, each U>) -> () = patternInstantiationFunctionTest2()
  let _: (Dictionary<Int, String>, repeat Dictionary<each T, each U>, Dictionary<Double, Character>) -> () = patternInstantiationFunctionTest2()
}

func patternInstantiationGenericInvalid<T...>(t: repeat each T) where T: Hashable {
  let _: (repeat Set<each T>) = patternInstantiationTupleTest1() // expected-error {{cannot convert value of type '(repeat Array<each T>)' to specified type '(repeat Set<each T>)}}
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  let _: (repeat Array<each T>, Set<String>) = patternInstantiationTupleTest1() // expected-error {{type of expression is ambiguous without more context}}
}
