// RUN: %target-parse-verify-swift

func takeFunc(f: (Int) -> Int) -> Int {}
func takeValueAndFunc(value: Int, _ f: (Int) -> Int) {}
func takeTwoFuncs(f: (Int) -> Int, _ g: (Int) -> Int) {}
func takeFuncWithDefault(f f : ((Int) -> Int)? = nil) {}
func takeTwoFuncsWithDefaults(f1 f1 : (Int -> Int)? = nil, f2 : (String -> String)? = nil) {}

struct X {
  func takeFunc(f: (Int) -> Int) {}
  func takeValueAndFunc(value: Int, f: (Int) -> Int) {}
  func takeTwoFuncs(f: (Int) -> Int, g: (Int) -> Int) {}
}

func addToMemberCalls(x: X) {
  x.takeFunc() { x in x }
  x.takeFunc() { $0 }
  x.takeValueAndFunc(1) { x in x }
  x.takeTwoFuncs({ x in x }) { y in y }
}

func addToCalls() {
  takeFunc() { x in x }
  takeFunc() { $0 }
  takeValueAndFunc(1) { x in x }
  takeTwoFuncs({ x in x }) { y in y }
}

func makeCalls() {
  takeFunc { x in x }
  takeFunc { $0 }
  takeTwoFuncs ({ x in x }) { y in y }
}

func notPostfix() {
  1 + takeFunc { $0 }
}

class C {
  func map(x: Int -> Int) -> C { return self }
  func filter(x: Int -> Bool) -> C { return self }
}

var a = C().map {$0 + 1}.filter {$0 % 3 == 0}

var b = C().map {$0 + 1}
  .filter {$0 % 3 == 0}

var c = C().map
{
  $0 + 1
}

var c2 = C().map // expected-note{{parsing trailing closure for this call}}

{ // expected-warning{{trailing closure is separated from call site}}
  $0 + 1
}

var c3 = C().map // expected-note{{parsing trailing closure for this call}}
// blah blah blah
{ // expected-warning{{trailing closure is separated from call site}}
  $0 + 1
}

// Calls with multiple trailing closures should be rejected until we have time
// to design it right.
// <rdar://problem/16835718> Ban multiple trailing closures
func multiTrailingClosure(a : () -> (), b : () -> ()) {
  multiTrailingClosure({}) {} // ok
  multiTrailingClosure {} {}   // expected-error {{missing argument for parameter #1 in call}} expected-error {{consecutive statements on a line must be separated by ';'}} expected-error {{braced block of statements is an unused closure}} expected-error{{type of expression is ambiguous without more context}}
  
  
}

func labeledArgumentAndTrailingClosure() {
  // Trailing closures can bind to labeled parameters.
  takeFuncWithDefault { $0 + 1 }
  takeFuncWithDefault() { $0 + 1 }
  // ... but not non-trailing closures.
  takeFuncWithDefault({ $0 + 1 }) // expected-error {{missing argument label 'f:' in call}}
  takeFuncWithDefault(f: { $0 + 1 })

  // Trailing closure binds to last parameter, always.
 takeTwoFuncsWithDefaults { "Hello, " + $0 }
  takeTwoFuncsWithDefaults { $0 + 1 } // expected-error {{cannot invoke 'takeTwoFuncsWithDefaults' with an argument list of type '((_) -> _)'}} 
  // expected-note@-1{{expected an argument list of type '(f1: (Int -> Int)?, f2: (String -> String)?)'}}
  takeTwoFuncsWithDefaults(f1: {$0 + 1 })
}

// rdar://problem/17965209
func rdar17965209_f<T>(t: T) {}
func rdar17965209(x x: Int = 0, _ handler: (y: Int) -> ()) {}
func rdar17965209_test() {
  rdar17965209() {
    (y) -> () in
    rdar17965209_f(1)
  }

  rdar17965209(x: 5) {
    (y) -> () in
    rdar17965209_f(1)
  }
}
