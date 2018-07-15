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

// FIXME: maybe complain about .nope also?
fe(.nope, .nyet) // expected-error {{reference to member 'nyet' cannot be resolved without a contextual type}}

func fg<T>(_ f: (T) -> T) -> Void {} // expected-note {{in call to function 'fg'}}
fg({x in x}) // expected-error {{generic parameter 'T' could not be inferred}}


struct S {
  func f<T>(_ i: (T) -> T, _ j: Int) -> Void {}
  func f(_ d: (Double) -> Double) -> Void {}
  func test() -> Void {
    f({x in x}, 2) // expected-error {{generic parameter 'T' could not be inferred}}
  }
  
  func g<T>(_ a: T, _ b: Int) -> Void {}
  func g(_ a: String) -> Void {}
  func test2() -> Void {
    g(.notAThing, 7) // expected-error {{reference to member 'notAThing' cannot be resolved without a contextual type}}
  }
  
  func h(_ a: Int, _ b: Int) -> Void {}
  func h(_ a: String) -> Void {}
  func test3() -> Void {
    h(.notAThing, 3) // expected-error {{type 'Int' has no member 'notAThing'}}
    h(.notAThing) // expected-error {{type 'String' has no member 'notAThing'}}
  }
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
  func foo<T: UnsignedInteger>(bar: T) -> T {
    return bar
  }
}

let s = sr7786()
let a = s.foo()
let b = s.foo(bar: 123) // expected-error {{argument type 'Int' does not conform to expected type 'UnsignedInteger'}}
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
  }, completedBy: {
  })
}

