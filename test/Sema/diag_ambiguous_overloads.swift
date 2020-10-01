// RUN: %target-typecheck-verify-swift

enum E : String {
  case foo = "foo"
  case bar = "bar" // expected-note {{'bar' declared here}}
}
func fe(_: E) {}
func fe(_: Int) {}
func fe(_: Int, _: E) {}
func fe(_: Int, _: Int) {}

fe(E.baz) // expected-error {{type 'E' has no member 'baz'; did you mean 'bar'?}}
fe(.baz) // expected-error {{reference to member 'baz' cannot be resolved without a contextual type}}

fe(.nope, .nyet) // expected-error {{type 'Int' has no member 'nope'}}
// expected-error@-1 {{reference to member 'nyet' cannot be resolved without a contextual type}}

func fg<T>(_ f: (T) -> T) -> Void {}
fg({x in x}) // expected-error {{unable to infer type of a closure parameter 'x' in the current context}}


struct S {
  func f<T>(_ i: (T) -> T, _ j: Int) -> Void {}
  func f(_ d: (Double) -> Double) -> Void {}
  func test() -> Void {
    f({x in x}, 2) // expected-error {{unable to infer type of a closure parameter 'x' in the current context}}
  }
  
  func g<T>(_ a: T, _ b: Int) -> Void {}
  func g(_ a: String) -> Void {}
  func test2() -> Void {
    g(.notAThing, 7) // expected-error {{cannot infer contextual base in reference to member 'notAThing'}}
  }
  
  func h(_ a: Int, _ b: Int) -> Void {}
  func h(_ a: String) -> Void {}
  func test3() -> Void {
    h(.notAThing, 3) // expected-error {{type 'Int' has no member 'notAThing'}}
    h(.notAThing) // expected-error {{type 'String' has no member 'notAThing'}}
  }
}

struct School {
  var name: String
}
func testDiagnoseForAmbiguityCrash(schools: [School]) {
  schools.map({ $0.name }).sorted(by: { $0.nothing < $1.notAThing })
  // expected-error@-1 {{value of type 'String' has no member 'nothing'}}
  // expected-error@-2 {{value of type 'String' has no member 'notAThing'}}
}

class DefaultValue {
  static func foo(_ a: Int) {}
  static func foo(_ a: Int, _ b: Int = 1) {}
  static func foo(_ a: Int, _ b: Int = 1, _ c: Int = 2) {}
}
DefaultValue.foo(1.0, 1) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}


class Variadic {
  static func foo(_ a: Int) {}
  static func foo(_ a: Int, _ b: Double...) {}
}
Variadic.foo(1.0, 2.0, 3.0) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}


//=-------------- SR-7918 --------------=/
class sr7918_Suit {
  static func foo<T: Any>(_ :inout T) {}
  static func foo() {}
}

class sr7918_RandomNumberGenerator {}

let myRNG = sr7918_RandomNumberGenerator() // expected-note {{change 'let' to 'var' to make it mutable}}
_ = sr7918_Suit.foo(&myRNG) // expected-error {{cannot pass immutable value as inout argument: 'myRNG' is a 'let' constant}}


//=-------------- SR-7786 --------------=/
struct sr7786 {
  func foo() -> UInt { return 0 }
  func foo<T: UnsignedInteger>(bar: T) -> T { // expected-note {{where 'T' = 'Int'}}
    return bar
  }
}

let s = sr7786()
let a = s.foo()
let b = s.foo(bar: 123) // expected-error {{instance method 'foo(bar:)' requires that 'Int' conform to 'UnsignedInteger'}}
let c: UInt = s.foo(bar: 123)
let d = s.foo(bar: 123 as UInt)


//=-------------- SR-7440 --------------=/
struct sr7440_ITunesGenre {
  let genre: Int // expected-note {{'genre' declared here}}
  let name: String
}
class sr7440_Genre {
  static func fetch<B: BinaryInteger>(genreID: B, name: String) {}
  static func fetch(_ iTunesGenre: sr7440_ITunesGenre) -> sr7440_Genre {
    return sr7440_Genre.fetch(genreID: iTunesGenre.genreID, name: iTunesGenre.name)
// expected-error@-1 {{value of type 'sr7440_ITunesGenre' has no member 'genreID'; did you mean 'genre'?}}
// expected-error@-2 {{cannot convert return expression of type '()' to return type 'sr7440_Genre'}}
  }
}


//=-------------- SR-5154 --------------=/
protocol sr5154_Scheduler {
  func inBackground(run task: @escaping () -> Void)
}

extension sr5154_Scheduler {
  func inBackground(run task: @escaping () -> [Count], completedBy resultHandler: @escaping ([Count]) -> Void) {}
}

struct Count { // expected-note {{'init(title:)' declared here}}
  let title: String
}

func getCounts(_ scheduler: sr5154_Scheduler, _ handler: @escaping ([Count]) -> Void) {
  scheduler.inBackground(run: {
    return [Count()] // expected-error {{missing argument for parameter 'title' in call}}
  }, completedBy: { // expected-error {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{20-20= _ in}}
  })
}

// SR-12689
func SR12689(_ u: UnsafeBufferPointer<UInt16>) {}

let array : [UInt16] = [1, 2]

array.withUnsafeBufferPointer {
  SR12689(UnsafeRawPointer($0).bindMemory(to: UInt16.self, capacity: 1)) // expected-error {{cannot convert value of type 'UnsafePointer<UInt16>' to expected argument type 'UnsafeBufferPointer<UInt16>'}}
  // expected-error@-1 {{no exact matches in call to initializer}}
  // expected-note@-2 {{candidate expects value of type 'UnsafeRawPointer' for parameter #1}}
  // expected-note@-3 {{candidate expects value of type 'UnsafeMutableRawPointer' for parameter #1}}
  // expected-note@-4 {{candidate expects value of type 'OpaquePointer' for parameter #1}}
  // expected-note@-5 {{candidate expects value of type 'Builtin.RawPointer' for parameter #1}}

  UnsafeRawPointer($0) as UnsafeBufferPointer<UInt16> // expected-error {{cannot convert value of type 'UnsafeRawPointer' to type 'UnsafeBufferPointer<UInt16>' in coercion}}
  // expected-error@-1 {{no exact matches in call to initializer}}
  // expected-note@-2 {{found candidate with type '(UnsafeRawPointer) -> UnsafeRawPointer'}}
  // expected-note@-3 {{found candidate with type '(Builtin.RawPointer) -> UnsafeRawPointer'}}
  // expected-note@-4 {{found candidate with type '(OpaquePointer) -> UnsafeRawPointer'}}
  // expected-note@-5 {{found candidate with type '(UnsafeMutableRawPointer) -> UnsafeRawPointer'}}
}

func SR12689_1(_ u: Int) -> String { "" } // expected-note {{found this candidate}} expected-note {{candidate expects value of type 'Int' for parameter #1}}
func SR12689_1(_ u: String) -> Double { 0 } // expected-note {{found this candidate}} expected-note {{candidate expects value of type 'String' for parameter #1}}
func SR12689_2(_ u: Int) {}

SR12689_2(SR12689_1(1 as Double)) // expected-error {{no exact matches in call to global function 'SR12689_1'}}
SR12689_1(1 as Double) as Int // expected-error {{no exact matches in call to global function 'SR12689_1'}}

// Ambiguos OverloadRefExpr
func SR12689_O(_ p: Int) {} // expected-note {{found candidate with type '(Int) -> ()'}}
func SR12689_O(_ p: Double) {} // expected-note {{found candidate with type '(Double) -> ()'}}
func SR12689_3(_ param: (String)-> Void) {}

SR12689_3(SR12689_O) // expected-error {{no 'SR12689_O' candidates produce the expected type '(String) -> Void' for parameter #0}}
