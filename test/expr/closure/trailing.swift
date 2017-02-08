// RUN: %target-typecheck-verify-swift

@discardableResult
func takeFunc(_ f: (Int) -> Int) -> Int {}
func takeValueAndFunc(_ value: Int, _ f: (Int) -> Int) {}
func takeTwoFuncs(_ f: (Int) -> Int, _ g: (Int) -> Int) {}
func takeFuncWithDefault(f : ((Int) -> Int)? = nil) {}
func takeTwoFuncsWithDefaults(f1 : ((Int) -> Int)? = nil, f2 : ((String) -> String)? = nil) {}

struct X {
  func takeFunc(_ f: (Int) -> Int) {}
  func takeValueAndFunc(_ value: Int, f: (Int) -> Int) {}
  func takeTwoFuncs(_ f: (Int) -> Int, g: (Int) -> Int) {}
}

func addToMemberCalls(_ x: X) {
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
  _ = 1 + takeFunc { $0 }
}

func notLiterals() {
  struct SR3671 { // <https://bugs.swift.org/browse/SR-3671>
    var v: Int = 1 { // expected-error {{variable with getter/setter cannot have an initial value}}
      get {
        return self.v
      }
    }
  }

  var x: Int? = nil { get { } } // expected-error {{variable with getter/setter cannot have an initial value}}
  _ = 1 {}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{closure expression is unused}} expected-note@-2 {{did you mean to use a 'do' statement?}} {{9-9=do }}
  _ = "hello" {}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{closure expression is unused}} expected-note@-2 {{did you mean to use a 'do' statement?}} {{15-15=do }}
  _ = [42] {}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{closure expression is unused}} expected-note@-2 {{did you mean to use a 'do' statement?}} {{12-12=do }}
  _ = (6765, 10946, 17711) {}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{closure expression is unused}} expected-note@-2 {{did you mean to use a 'do' statement?}} {{28-28=do }}
}

class C {
  func map(_ x: (Int) -> Int) -> C { return self }
  func filter(_ x: (Int) -> Bool) -> C { return self }
}

var a = C().map {$0 + 1}.filter {$0 % 3 == 0}

var b = C().map {$0 + 1}
  .filter {$0 % 3 == 0}

var c = C().map
{
  $0 + 1
}

var c2 = C().map // expected-note{{callee is here}}

{ // expected-warning{{braces here form a trailing closure separated from its callee by multiple newlines}}
  $0 + 1
}

var c3 = C().map // expected-note{{callee is here}}
// blah blah blah
{ // expected-warning{{braces here form a trailing closure separated from its callee by multiple newlines}}
  $0 + 1
}

// Calls with multiple trailing closures should be rejected until we have time
// to design it right.
// <rdar://problem/16835718> Ban multiple trailing closures
func multiTrailingClosure(_ a : () -> (), b : () -> ()) {  // expected-note {{'multiTrailingClosure(_:b:)' declared here}}
  multiTrailingClosure({}) {} // ok
  multiTrailingClosure {} {}   // expected-error {{missing argument for parameter #1 in call}} expected-error {{consecutive statements on a line must be separated by ';'}} {{26-26=;}} expected-error {{closure expression is unused}} expected-note{{did you mean to use a 'do' statement?}} {{27-27=do }}
  
  
}

func labeledArgumentAndTrailingClosure() {
  // Trailing closures can bind to labeled parameters.
  takeFuncWithDefault { $0 + 1 }
  takeFuncWithDefault() { $0 + 1 }
  // ... but not non-trailing closures.
  takeFuncWithDefault({ $0 + 1 }) // expected-error {{missing argument label 'f:' in call}} {{23-23=f: }}
  takeFuncWithDefault(f: { $0 + 1 })

  // Trailing closure binds to last parameter, always.
 takeTwoFuncsWithDefaults { "Hello, " + $0 }
  takeTwoFuncsWithDefaults { $0 + 1 } // expected-error {{cannot convert value of type '(Int) -> Int' to expected argument type '((String) -> String)?'}} 
  takeTwoFuncsWithDefaults(f1: {$0 + 1 })
}

// rdar://problem/17965209
func rdar17965209_f<T>(_ t: T) {}
func rdar17965209(x: Int = 0, _ handler: (_ y: Int) -> ()) {}
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


// <rdar://problem/22298549> QoI: Unwanted trailing closure produces weird error
func limitXY(_ xy:Int, toGamut gamut: [Int]) {}
let someInt = 0
let intArray = [someInt]
limitXY(someInt, toGamut: intArray) {}  // expected-error {{extra argument 'toGamut' in call}}


// <rdar://problem/23036383> QoI: Invalid trailing closures in stmt-conditions produce lowsy diagnostics
func retBool(x: () -> Int) -> Bool {}
func maybeInt(_: () -> Int) -> Int? {}
class Foo23036383 {
  init() {}
  func map(_: (Int) -> Int) -> Int? {}
  func meth1(x: Int, _: () -> Int) -> Bool {}
  func meth2(_: Int, y: () -> Int) -> Bool {}
  func filter(by: (Int) -> Bool) -> [Int] {}
}
enum MyErr : Error {
  case A
}

func r23036383(foo: Foo23036383?, obj: Foo23036383) {

  if retBool(x: { 1 }) { } // OK
  if (retBool { 1 }) { } // OK

  if retBool{ 1 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{13-13=(x: }} {{18-18=)}}
  }
  if retBool { 1 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{13-14=(x: }} {{19-19=)}}
  }
  if retBool() { 1 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{14-16=x: }} {{21-21=)}}
  } else if retBool( ) { 0 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{21-24=x: }} {{29-29=)}}
  }

  if let _ = maybeInt { 1 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{22-23=(}} {{28-28=)}}
  }
  if let _ = maybeInt { 1 } , true {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{22-23=(}} {{28-28=)}}
  }

  if let _ = foo?.map {$0+1} {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{22-23=(}} {{29-29=)}}
  }
  if let _ = foo?.map() {$0+1} {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{23-25=}} {{31-31=)}}
  }
  if let _ = foo, retBool { 1 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{26-27=(x: }} {{32-32=)}}
  }

  if obj.meth1(x: 1) { 0 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{20-22=, }} {{27-27=)}}
  }
  if obj.meth2(1) { 0 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{17-19=, y: }} {{24-24=)}}
  }

  for _ in obj.filter {$0 > 4} {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{22-23=(by: }} {{31-31=)}}
  }
  for _ in obj.filter {$0 > 4} where true {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{22-23=(by: }} {{31-31=)}}
  }
  for _ in [1,2] where retBool { 1 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{31-32=(x: }} {{37-37=)}}
  }

  while retBool { 1 } { // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{16-17=(x: }} {{22-22=)}}
  }
  while let _ = foo, retBool { 1 } { // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{29-30=(x: }} {{35-35=)}}
  }

  switch retBool { return 1 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{17-18=(x: }} {{30-30=)}}
    default: break
  }

  do {
    throw MyErr.A;
  } catch MyErr.A where retBool { 1 } {  // expected-error {{trailing closure requires parentheses for disambiguation in this context}} {{32-33=(x: }} {{38-38=)}}
  } catch { }

  if let _ = maybeInt { 1 }, retBool { 1 } { }
  // expected-error@-1 {{trailing closure requires parentheses for disambiguation in this context}} {{22-23=(}} {{28-28=)}} 
  // expected-error@-2 {{trailing closure requires parentheses for disambiguation in this context}} {{37-38=(x: }} {{43-43=)}} 
}

func overloadOnLabel(a: () -> Void) {}
func overloadOnLabel(b: () -> Void) {}
func overloadOnLabel(c: () -> Void) {}

func overloadOnLabel2(a: () -> Void) {}
func overloadOnLabel2(_: () -> Void) {}

func overloadOnLabelArgs(_: Int, a: () -> Void) {}
func overloadOnLabelArgs(_: Int, b: () -> Void) {}

func overloadOnLabelArgs2(_: Int, a: () -> Void) {}
func overloadOnLabelArgs2(_: Int, _: () -> Void) {}

func overloadOnLabelDefaultArgs(x: Int = 0, a: () -> Void) {}
func overloadOnLabelDefaultArgs(x: Int = 1, b: () -> Void) {}

func overloadOnLabelSomeDefaultArgs(_: Int, x: Int = 0, a: () -> Void) {}
func overloadOnLabelSomeDefaultArgs(_: Int, x: Int = 1, b: () -> Void) {}

func overloadOnDefaultArgsOnly(x: Int = 0, a: () -> Void) {} // expected-note 2 {{found this candidate}}
func overloadOnDefaultArgsOnly(y: Int = 1, a: () -> Void) {} // expected-note 2 {{found this candidate}}

func overloadOnDefaultArgsOnly2(x: Int = 0, a: () -> Void) {} // expected-note 2 {{found this candidate}}
func overloadOnDefaultArgsOnly2(y: Bool = true, a: () -> Void) {} // expected-note 2 {{found this candidate}}

func overloadOnDefaultArgsOnly3(x: Int = 0, a: () -> Void) {} // expected-note 2 {{found this candidate}}
func overloadOnDefaultArgsOnly3(x: Bool = true, a: () -> Void) {} // expected-note 2 {{found this candidate}}

func overloadOnSomeDefaultArgsOnly(_: Int, x: Int = 0, a: () -> Void) {} // expected-note {{found this candidate}}
func overloadOnSomeDefaultArgsOnly(_: Int, y: Int = 1, a: () -> Void) {} // expected-note {{found this candidate}}

func overloadOnSomeDefaultArgsOnly2(_: Int, x: Int = 0, a: () -> Void) {} // expected-note {{found this candidate}}
func overloadOnSomeDefaultArgsOnly2(_: Int, y: Bool = true, a: () -> Void) {} // expected-note {{found this candidate}}

func overloadOnSomeDefaultArgsOnly3(_: Int, x: Int = 0, a: () -> Void) {} // expected-note {{found this candidate}}
func overloadOnSomeDefaultArgsOnly3(_: Int, x: Bool = true, a: () -> Void) {} // expected-note {{found this candidate}}

func testOverloadAmbiguity() {
  overloadOnLabel {} // expected-error {{ambiguous use of 'overloadOnLabel'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabel(a:)'}} {{18-19=(a: }} {{21-21=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabel(b:)'}} {{18-19=(b: }} {{21-21=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabel(c:)'}} {{18-19=(c: }} {{21-21=)}}
  overloadOnLabel() {} // expected-error {{ambiguous use of 'overloadOnLabel'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabel(a:)'}} {{19-21=a: }} {{23-23=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabel(b:)'}} {{19-21=b: }} {{23-23=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabel(c:)'}} {{19-21=c: }} {{23-23=)}}
  overloadOnLabel2 {} // expected-error {{ambiguous use of 'overloadOnLabel2'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabel2(a:)'}} {{19-20=(a: }} {{22-22=)}} expected-note {{avoid using a trailing closure to call 'overloadOnLabel2'}} {{19-20=(}} {{22-22=)}}
  overloadOnLabel2() {} // expected-error {{ambiguous use of 'overloadOnLabel2'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabel2(a:)'}} {{20-22=a: }} {{24-24=)}} expected-note {{avoid using a trailing closure to call 'overloadOnLabel2'}} {{20-22=}} {{24-24=)}}
  overloadOnLabelArgs(1) {} // expected-error {{ambiguous use of 'overloadOnLabelArgs'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelArgs(_:a:)'}} {{24-26=, a: }} {{28-28=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelArgs(_:b:)'}} {{24-26=, b: }} {{28-28=)}}
  overloadOnLabelArgs2(1) {} // expected-error {{ambiguous use of 'overloadOnLabelArgs2'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelArgs2(_:a:)'}} {{25-27=, a: }} {{29-29=)}} expected-note {{avoid using a trailing closure to call 'overloadOnLabelArgs2'}} {{25-27=, }} {{29-29=)}}
  overloadOnLabelDefaultArgs {} // expected-error {{ambiguous use of 'overloadOnLabelDefaultArgs'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelDefaultArgs(x:a:)'}} {{29-30=(a: }} {{32-32=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelDefaultArgs(x:b:)'}} {{29-30=(b: }} {{32-32=)}}
  overloadOnLabelDefaultArgs() {} // expected-error {{ambiguous use of 'overloadOnLabelDefaultArgs'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelDefaultArgs(x:a:)'}} {{30-32=a: }} {{34-34=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelDefaultArgs(x:b:)'}} {{30-32=b: }} {{34-34=)}}
  overloadOnLabelDefaultArgs(x: 2) {} // expected-error {{ambiguous use of 'overloadOnLabelDefaultArgs'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelDefaultArgs(x:a:)'}} {{34-36=, a: }} {{38-38=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelDefaultArgs(x:b:)'}} {{34-36=, b: }} {{38-38=)}}
  overloadOnLabelSomeDefaultArgs(1) {} // expected-error {{ambiguous use of 'overloadOnLabelSomeDefaultArgs'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelSomeDefaultArgs(_:x:a:)'}} {{35-37=, a: }} {{39-39=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelSomeDefaultArgs(_:x:b:)'}} {{35-37=, b: }} {{39-39=)}}
  overloadOnLabelSomeDefaultArgs(1, x: 2) {} // expected-error {{ambiguous use of 'overloadOnLabelSomeDefaultArgs'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelSomeDefaultArgs(_:x:a:)'}} {{41-43=, a: }} {{45-45=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelSomeDefaultArgs(_:x:b:)'}} {{41-43=, b: }} {{45-45=)}}

  overloadOnLabelSomeDefaultArgs( // expected-error {{ambiguous use of 'overloadOnLabelSomeDefaultArgs'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelSomeDefaultArgs(_:x:a:)'}} {{12-5=, a: }} {{4-4=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelSomeDefaultArgs(_:x:b:)'}} {{12-5=, b: }} {{4-4=)}}
    1, x: 2
  ) {
    // some
  }

  overloadOnDefaultArgsOnly {} // expected-error {{ambiguous use of 'overloadOnDefaultArgsOnly'}}
  overloadOnDefaultArgsOnly() {} // expected-error {{ambiguous use of 'overloadOnDefaultArgsOnly'}}
  overloadOnDefaultArgsOnly2 {} // expected-error {{ambiguous use of 'overloadOnDefaultArgsOnly2'}}
  overloadOnDefaultArgsOnly2() {} // expected-error {{ambiguous use of 'overloadOnDefaultArgsOnly2'}}
  overloadOnDefaultArgsOnly3 {} // expected-error {{ambiguous use of 'overloadOnDefaultArgsOnly3(x:a:)'}}
  overloadOnDefaultArgsOnly3() {} // expected-error {{ambiguous use of 'overloadOnDefaultArgsOnly3(x:a:)'}}

  overloadOnSomeDefaultArgsOnly(1) {} // expected-error {{ambiguous use of 'overloadOnSomeDefaultArgsOnly'}}
  overloadOnSomeDefaultArgsOnly2(1) {} // expected-error {{ambiguous use of 'overloadOnSomeDefaultArgsOnly2'}}
  overloadOnSomeDefaultArgsOnly3(1) {} // expected-error {{ambiguous use of 'overloadOnSomeDefaultArgsOnly3(_:x:a:)'}}
}
