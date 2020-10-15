// RUN: %target-typecheck-verify-swift

// SR-13503
class Root {}
class Sub: Root {}

// No callAsFunction overload
struct SR13503No {}

// A single callAsFunction overload
struct SR13503Single {
  func callAsFunction() -> Int { 0 }
}

// One overload matches one fail callAsFunction overload
struct SR13503Match {
  func callAsFunction() -> Int { 0 }
  func callAsFunction() -> Double { 0.0 }
}

// A single overload with all parameters default matches.
struct SR13503Default {
  func callAsFunction(a: Int = 0) -> Int { 0 }
}

// A single overload with parameters.
struct SR13503Param {
  func callAsFunction(a: Int) -> Int { 0 }
}
struct SR13503Param2 {
  func callAsFunction(a: Int, b: Int) -> Int { 0 }
}


struct SR13503 {
  func callAsFunction() -> Int { 0 }
  func callAsFunction(a: Int = 0) -> Int { 0 }
}

struct SR13503A {
  private func callAsFunction() -> Int { 0 }
  private func callAsFunction(a: Int = 0) -> Int { 0 }
}

struct SR13503Sub {
  func callAsFunction() -> Root { fatalError() }
  func callAsFunction() -> Sub { fatalError() }
}

struct SR13503G<T> {
  func callAsFunction() -> T { fatalError() }
  func callAsFunction(a: Int = 0) -> T { fatalError() }
}

func testReturn() -> SR13503 { SR13503() }

let value = SR13503()
let valueSin = SR13503Single()
let valueNo = SR13503No()
let valueg = SR13503G<Int>()
let valueg1 = SR13503G<String>()
let valuesub = SR13503Sub()
let valuea = SR13503A()
let valueM = SR13503Match()
let valueD = SR13503Default()
let valueP = SR13503Param()
let valueP2 = SR13503Param2()

func fSR13503(class: Root) {}

func fSR13503(arg: Int) {}
func fSR13503_S(arg: String) {}

fSR13503(arg: valueNo) // expected-error {{cannot convert value of type 'SR13503No' to expected argument type 'Int'}}
fSR13503(arg: valueSin) // expected-error {{value of callable type 'SR13503Single' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{23-23=()}}
fSR13503_S(arg: valueSin) // expected-error {{cannot convert value of type 'SR13503Single' to expected argument type 'String'}}
fSR13503(arg: value) // expected-error {{value of callable type 'SR13503' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{20-20=()}}
fSR13503_S(arg: value) // expected-error {{cannot convert value of type 'SR13503' to expected argument type 'String'}}
fSR13503(arg: valueg) // expected-error {{value of callable type 'SR13503G<Int>' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{21-21=()}}
fSR13503(arg: valueg1) // expected-error {{cannot convert value of type 'SR13503G<String>' to expected argument type 'Int'}}
fSR13503(arg: testReturn()) // expected-error {{value of callable type 'SR13503' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{27-27=()}}
fSR13503_S(arg: testReturn()) // expected-error {{cannot convert value of type 'SR13503' to expected argument type 'String'}}
fSR13503(arg: valueM) // expected-error {{value of callable type 'SR13503Match' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{21-21=()}}
fSR13503_S(arg: valueM) // expected-error {{cannot convert value of type 'SR13503Match' to expected argument type 'String'}}
fSR13503(arg: valueD) // expected-error {{value of callable type 'SR13503Default' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{21-21=()}}
fSR13503_S(arg: valueD) // expected-error {{cannot convert value of type 'SR13503Default' to expected argument type 'String'}}
fSR13503(arg: valueP) // expected-error {{cannot convert value of type 'SR13503Param' to expected argument type 'Int'}}
fSR13503_S(arg: valueP) // expected-error {{cannot convert value of type 'SR13503Param' to expected argument type 'String'}}

// Access control no visible callAsFunction
fSR13503(arg: valuea) // expected-error {{cannot convert value of type 'SR13503A' to expected argument type 'Int'}}
// In case we have subtype callAsFunction overloads, suggest call with '()' may lead to ambiguity
fSR13503(class: valuesub) // expected-error {{value of callable type 'SR13503Sub' can be called to produce expected type 'Root'; did you mean to call it with '()'?}} {{25-25=()}}

let _ : Int = value // expected-error {{value of callable type 'SR13503' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{20-20=()}}
let _ : String = value // expected-error {{cannot convert value of type 'SR13503' to specified type 'String'}}
let _ : Int = valueg // expected-error {{value of callable type 'SR13503G<Int>' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{21-21=()}}
let _ : Int = valueg1 // expected-error {{cannot convert value of type 'SR13503G<String>' to specified type 'Int'}}
let _ : Int = testReturn() // expected-error {{value of callable type 'SR13503' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{27-27=()}}
let _ : Void = value // expected-error {{cannot convert value of type 'SR13503' to specified type 'Void'}}

// Access control no visible callAsFunction
let _ : Int = valuea // expected-error {{cannot convert value of type 'SR13503A' to specified type 'Int'}}
// In case we have subtype callAsFunction overloads, suggest call with '()' may lead to ambiguity
let _ : Root = valuesub // expected-error {{value of callable type 'SR13503Sub' can be called to produce expected type 'Root'; did you mean to call it with '()'?}} {{24-24=()}}

// @dynamicCallable

// A single @dynamicCallable overload
@dynamicCallable
struct SR13503Single_DC {
  func dynamicallyCall(withArguments: [Int]) -> Int { 0 }
}

@dynamicCallable
struct SR13503_DC {
  func dynamicallyCall(withArguments: [Int]) -> Int { 0 }
  func dynamicallyCall(withArguments: [Int]) -> Double { 0 }
}

@dynamicCallable
struct SR13503A_DC {
  private func dynamicallyCall(withKeywordArguments: [String: Any]) -> Int { 0 }
}

@dynamicCallable
struct SR13503Sub_DC {
  func dynamicallyCall(withKeywordArguments: [String: Any]) -> Root { fatalError() }
  func dynamicallyCall(withKeywordArguments: [String: Any]) -> Sub { fatalError() }
}

@dynamicCallable
struct SR13503G_DC<T> {
  func dynamicallyCall(withArguments: [Int]) -> T { fatalError() }
  func dynamicallyCall(withKeywordArguments: [String: Any]) -> T { fatalError() }
}

func testReturn_DC() -> SR13503_DC { SR13503_DC() }

let value_sin_dc = SR13503Single_DC()
let value_dc = SR13503_DC()
let valueg_dc = SR13503G_DC<Int>()
let valueg1_dc = SR13503G_DC<String>()
let valuesub_dc = SR13503Sub_DC()
let valuea_dc = SR13503A_DC()

fSR13503(arg: value_sin_dc) // expected-error {{value of callable type 'SR13503Single_DC' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{27-27=()}}
fSR13503_S(arg: value_sin_dc) // expected-error {{cannot convert value of type 'SR13503Single_DC' to expected argument type 'String'}}
fSR13503(arg: value_dc) // expected-error {{value of callable type 'SR13503_DC' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{23-23=()}}
fSR13503_S(arg: value_dc) // expected-error {{cannot convert value of type 'SR13503_DC' to expected argument type 'String'}}
fSR13503(arg: valueg_dc) // expected-error {{value of callable type 'SR13503G_DC<Int>' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{24-24=()}}
fSR13503(arg: valueg1_dc) // expected-error {{cannot convert value of type 'SR13503G_DC<String>' to expected argument type 'Int'}}
fSR13503(arg: testReturn_DC()) // expected-error {{value of callable type 'SR13503_DC' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{30-30=()}}
fSR13503_S(arg: testReturn_DC()) // expected-error {{cannot convert value of type 'SR13503_DC' to expected argument type 'String'}}

// Values that cannot be called with () because they have only callAsFunction definitions that expect parameters.
fSR13503(arg: valueP) // expected-error {{cannot convert value of type 'SR13503Param' to expected argument type 'Int'}}
fSR13503_S(arg: valueP) // expected-error {{cannot convert value of type 'SR13503Param' to expected argument type 'String'}}

fSR13503(arg: valueP2) // expected-error {{cannot convert value of type 'SR13503Param2' to expected argument type 'Int'}}
fSR13503_S(arg: valueP2) // expected-error {{cannot convert value of type 'SR13503Param2' to expected argument type 'String'}}

// Access control no visible dynamicallyCall
fSR13503(arg: valuea_dc) // expected-error {{cannot convert value of type 'SR13503A_DC' to expected argument type 'Int'}}
// In case we have subtype dynamicallyCall overloads, suggest call with '()' may lead to ambiguity
fSR13503(class: valuesub_dc) // expected-error {{value of callable type 'SR13503Sub_DC' can be called to produce expected type 'Root'; did you mean to call it with '()'?}} {{28-28=()}}

let _ : Int = value_dc // expected-error {{value of callable type 'SR13503_DC' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{23-23=()}}
let _ : String = value_dc // expected-error {{cannot convert value of type 'SR13503_DC' to specified type 'String'}}
let _ : Int = valueg_dc // expected-error {{value of callable type 'SR13503G_DC<Int>' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{24-24=()}}
let _ : Int = valueg1_dc // expected-error {{cannot convert value of type 'SR13503G_DC<String>' to specified type 'Int'}}
let _ : Int =  testReturn_DC() // expected-error {{value of callable type 'SR13503_DC' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{31-31=()}}
let _ : String =  testReturn_DC()  // expected-error {{cannot convert value of type 'SR13503_DC' to specified type 'String'}}

// Access control no visible dynamicallyCall
let _ : Int = valuea_dc // expected-error {{cannot convert value of type 'SR13503A_DC' to specified type 'Int'}}
// In case we have subtype dynamicallyCall overloads, suggest call with '()' may lead to ambiguity
let _ : Root =  valuesub_dc // expected-error {{value of callable type 'SR13503Sub_DC' can be called to produce expected type 'Root'; did you mean to call it with '()'?}} {{28-28=()}}

// Callable Literal
extension String {
  func dynamicallyCall(withArguments arguments: [Int]) -> Int {
    return arguments.count
  }

  func callAsFunction() -> Int { 0 }
}

let strRef = "foo"
let _: Int = "foo" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}
let _: Int = strRef // expected-error {{value of callable type 'String' can be called to produce expected type 'Int'; did you mean to call it with '()'?}} {{20-20=()}}
