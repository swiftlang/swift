// RUN: %target-typecheck-verify-swift -swift-version 4

@discardableResult
func takeFunc(_ f: (Int) -> Int) -> Int {}
func takeValueAndFunc(_ value: Int, _ f: (Int) -> Int) {}
func takeTwoFuncs(_ f: (Int) -> Int, _ g: (Int) -> Int) {}
func takeFuncWithDefault(f : ((Int) -> Int)? = nil) {}
func takeTwoFuncsWithDefaults(f1 : ((Int) -> Int)? = nil, f2 : ((String) -> String)? = nil) {}
// expected-note@-1{{'takeTwoFuncsWithDefaults(f1:f2:)' declared here}}

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
  // https://github.com/apple/swift/issues/46256
  struct S {
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
  multiTrailingClosure {} {}   // expected-error {{missing argument for parameter 'b' in call}} expected-error {{consecutive statements on a line must be separated by ';'}} {{26-26=;}} expected-error {{closure expression is unused}} expected-note{{did you mean to use a 'do' statement?}} {{27-27=do }}
}

func labeledArgumentAndTrailingClosure() {
  // Trailing closures can bind to labeled parameters.
  takeFuncWithDefault { $0 + 1 }
  takeFuncWithDefault() { $0 + 1 }
  // ... but not non-trailing closures.
  takeFuncWithDefault({ $0 + 1 }) // expected-error {{missing argument label 'f:' in call}} {{23-23=f: }}
  takeFuncWithDefault(f: { $0 + 1 })

  // Trailing closure binds to first parameter... unless only the second matches.
 takeTwoFuncsWithDefaults { "Hello, " + $0 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'f2' to suppress this warning}}
 takeTwoFuncsWithDefaults { $0 + 1 }
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
limitXY(someInt, toGamut: intArray) {}  // expected-error{{extra trailing closure passed in call}}

// <rdar://problem/23036383> QoI: Invalid trailing closures in stmt-conditions produce lousy diagnostics
func retBool(x: () -> Int) -> Bool {}
func maybeInt(_: () -> Int) -> Int? {}
func twoClosureArgs(_:()->Void, _:()->Void) -> Bool {}
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

  if retBool{ 1 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{13-13=(x: }} {{18-18=)}}
  }
  if retBool { 1 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{13-14=(x: }} {{19-19=)}}
  }
  if retBool() { 1 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{14-16=x: }} {{21-21=)}}
  } else if retBool( ) { 0 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{21-24=x: }} {{29-29=)}}
  }

  if let _ = maybeInt { 1 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(}} {{28-28=)}}
  }
  if let _ = maybeInt { 1 } , true {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(}} {{28-28=)}}
  }

  if let _ = foo?.map {$0+1} {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(}} {{29-29=)}}
  }
  if let _ = foo?.map() {$0+1} {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{23-25=}} {{31-31=)}}
  }
  if let _ = foo, retBool { 1 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{26-27=(x: }} {{32-32=)}}
  }

  if obj.meth1(x: 1) { 0 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{20-22=, }} {{27-27=)}}
  }
  if obj.meth2(1) { 0 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{17-19=, y: }} {{24-24=)}}
  }

  for _ in obj.filter {$0 > 4} {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(by: }} {{31-31=)}}
  }
  for _ in obj.filter {$0 > 4} where true {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(by: }} {{31-31=)}}
  }
  for _ in [1,2] where retBool { 1 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{31-32=(x: }} {{37-37=)}}
  }

  while retBool { 1 } { // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{16-17=(x: }} {{22-22=)}}
  }
  while let _ = foo, retBool { 1 } { // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{29-30=(x: }} {{35-35=)}}
  }

  switch retBool { return 1 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{17-18=(x: }} {{30-30=)}}
    default: break
  }

  do {
    throw MyErr.A;
  } catch MyErr.A where retBool { 1 } {  // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{32-33=(x: }} {{38-38=)}}
  } catch { }

  if let _ = maybeInt { 1 }, retBool { 1 } { }
  // expected-warning@-1 {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(}} {{28-28=)}}
  // expected-warning@-2 {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{37-38=(x: }} {{43-43=)}}

  if let _ = foo?.map {$0+1}.map {$0+1} {} // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}  {{33-34=(}} {{40-40=)}}
  // expected-warning@-1 {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(}} {{29-29=)}}

  if let _ = foo?.map {$0+1}.map({$0+1}) {} // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(}} {{29-29=)}}

  if let _ = foo?.map {$0+1}.map({$0+1}).map{$0+1} {} // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{45-45=(}} {{51-51=)}}
  // expected-warning@-1 {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{22-23=(}} {{29-29=)}}

  if twoClosureArgs({}) {} {} // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}} {{23-25=, }} {{27-27=)}}

  if let _ = (foo?.map {$0+1}.map({$0+1}).map{$0+1}) {} // OK

  if let _ = (foo?.map {$0+1}.map({$0+1})).map({$0+1}) {} // OK
}

// https://github.com/apple/swift/issues/51245

func id<T>(fn: () -> T) -> T { return fn() }
func any<T>(fn: () -> T) -> Any { return fn() }

func test_51245() {
  if !id { true } { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if id { true } == true { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if true == id { true } { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if id { true } ? true : false { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if true ? id { true } : false { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if true ? true : id { false } { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if id { [false,true] }[0] { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if id { { true } } () { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if any { true } as! Bool { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if let _ = any { "test" } as? Int { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if any { "test" } is Int { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if let _ = id { [] as [Int]? }?.first { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if id { true as Bool? }! { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if case id { 1 } = 1 { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if case 1 = id { 1 } { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if case 1 = id { 1 } /*comment*/ { return } // expected-warning {{trailing closure in this context is confusable with the body of the statement; pass as a parenthesized argument to silence this warning}}
  
  if case (id { 1 }) = 1 { return } // OK
  
  if case 1 = (id { 1 }) { return } // OK
  
  if [id { true }].count == 0 { return } // OK
  
  if [id { true } : "test"].keys.count == 0 { return } // OK
  
  if "\(id { true })" == "foo" { return } // OK
  
  if (id { true }) { return } // OK
  
  if (id { true }) { }
  [1, 2, 3].count // expected-warning {{expression of type 'Int' is unused}}
  
  if true { }
  () // OK
  
  if true
  {
    
  }
  () // OK
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

  overloadOnLabelSomeDefaultArgs( // expected-error {{ambiguous use of 'overloadOnLabelSomeDefaultArgs'}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelSomeDefaultArgs(_:x:a:)'}} {{+1:12-+2:5=, a: }} {{+4:4-4=)}} expected-note {{use an explicit argument label instead of a trailing closure to call 'overloadOnLabelSomeDefaultArgs(_:x:b:)'}} {{+1:12-+2:5=, b: }} {{+4:4-4=)}}
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

func overloadMismatch(a: () -> Void) -> Bool { return true}
func overloadMismatch(x: Int = 0, a: () -> Void) -> Int { return 0 }

func overloadMismatchLabel(a: () -> Void) -> Bool { return true}
func overloadMismatchLabel(x: Int = 0, b: () -> Void) -> Int { return 0 }

func overloadMismatchArgs(_: Int, a: () -> Void) -> Bool { return true}
func overloadMismatchArgs(_: Int, x: Int = 0, a: () -> Void) -> Int { return 0 }

func overloadMismatchArgsLabel(_: Int, a: () -> Void) -> Bool { return true}
func overloadMismatchArgsLabel(_: Int, x: Int = 0, b: () -> Void) -> Int { return 0 }

func overloadMismatchMultiArgs(_: Int, a: () -> Void) -> Bool { return true}
func overloadMismatchMultiArgs(_: Int, x: Int = 0, y: Int = 1, a: () -> Void) -> Int { return 0 }

func overloadMismatchMultiArgsLabel(_: Int, a: () -> Void) -> Bool { return true}
func overloadMismatchMultiArgsLabel(_: Int, x: Int = 0, y: Int = 1, b: () -> Void) -> Int { return 0 }

func overloadMismatchMultiArgs2(_: Int, z: Int = 0, a: () -> Void) -> Bool { return true}
func overloadMismatchMultiArgs2(_: Int, x: Int = 0, y: Int = 1, a: () -> Void) -> Int { return 0 }

func overloadMismatchMultiArgs2Label(_: Int, z: Int = 0, a: () -> Void) -> Bool { return true}
func overloadMismatchMultiArgs2Label(_: Int, x: Int = 0, y: Int = 1, b: () -> Void) -> Int { return 0 }

func testOverloadDefaultArgs() {
  let a = overloadMismatch {}
  _ = a as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}
  let b = overloadMismatch() {}
  _ = b as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let c = overloadMismatchLabel {}
  _ = c as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}
  let d = overloadMismatchLabel() {}
  _ = d as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let e = overloadMismatchArgs(0) {}
  _ = e as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let f = overloadMismatchArgsLabel(0) {}
  _ = f as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let g = overloadMismatchMultiArgs(0) {}
  _ = g as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let h = overloadMismatchMultiArgsLabel(0) {}
  _ = h as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let i = overloadMismatchMultiArgs2(0) {}
  _ = i as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let j = overloadMismatchMultiArgs2Label(0) {}
  _ = j as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}
}

func variadic(_: (() -> Void)...) {}
func variadicLabel(closures: (() -> Void)...) {}

func variadicOverloadMismatch(_: (() -> Void)...) -> Bool { return true }
func variadicOverloadMismatch(x: Int = 0, _: (() -> Void)...) -> Int { return 0 }

func variadicOverloadMismatchLabel(a: (() -> Void)...) -> Bool { return true }
func variadicOverloadMismatchLabel(x: Int = 0, b: (() -> Void)...) -> Int { return 0 }

func variadicAndNonOverload(_: (() -> Void)) -> Bool { return false }
func variadicAndNonOverload(_: (() -> Void)...) -> Int { return 0 }

func variadicAndNonOverloadLabel(a: (() -> Void)) -> Bool { return false }
func variadicAndNonOverloadLabel(b: (() -> Void)...) -> Int { return 0 }

func testVariadic() {
  variadic {}
  variadic() {}
  variadic({}) {}

  variadicLabel {}
  variadicLabel() {}
  variadicLabel(closures: {}) {}

  let a1 = variadicOverloadMismatch {}
  _ = a1 as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}
  let a2 = variadicOverloadMismatch() {}
  _ = a2 as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let b1 = variadicOverloadMismatchLabel {}
  _ = b1 as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}
  let b2 = variadicOverloadMismatchLabel() {}
  _ = b2 as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let c1 = variadicAndNonOverloadLabel {}
  _ = c1 as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}

  let c2 = variadicAndNonOverload {}
  _ = c2 as String // expected-error {{cannot convert value of type 'Bool' to type 'String'}}
}

// rdar://18426302

class TrailingBase {
  init(fn: () -> ()) {}
  init(x: Int, fn: () -> ()) {}

  convenience init() {
    self.init {}
  }

  convenience init(x: Int) {
    self.init(x: x) {}
  }
}

class TrailingDerived : TrailingBase {
  init(foo: ()) {
    super.init {}
  }

  init(x: Int, foo: ()) {
    super.init(x: x) {}
  }
}

// rdar://85343171 - Spurious warning on trailing closure in argument list.
func rdar85343171() {
  func foo(_ i: Int) -> Bool { true }
  func bar(_ fn: () -> Void) -> Int { 0 }

  // Okay, as trailing closure is nested in argument list.
  if foo(bar {}) {}
}

// rdar://92521618 - Spurious warning on trailing closure nested within a
// closure initializer.

func rdar92521618() {
  func foo(_ fn: () -> Void) -> Int? { 0 }

  if let _ = { foo {} }() {}
  guard let _ = { foo {} }() else { return }
}

// Argument matching never binds trailing closure arguments to
// defaulted/variadic parameters of non-function type.
do {
  // Trailing closure not considered fulfilled by 'arg'.
  // Note: Used to crash.
  do {
    func variadic(arg: Int...) {} // expected-note@:10 {{'variadic(arg:)' declared here}}{{none}}
    func defaulted(arg: Int = 0) {}

    let _ = variadic { return () }
    // expected-error@-1:22 {{trailing closure passed to parameter of type 'Int' that does not accept a closure}}{{none}}
    let _ = defaulted { return () }
    // expected-error@-1:23 {{extra trailing closure passed in call}}{{none}}
  }
  // Trailing closure considered fulfilled by 'x' instead of 'arg'.
  do {
    func variadic(arg: Int..., x: String) {} // expected-note@:10 {{'variadic(arg:x:)' declared here}}{{none}}
    func defaulted(arg: Int = 0, x: String) {} // expected-note@:10 {{'defaulted(arg:x:)' declared here}}{{none}}

    let _ = variadic { return () }
    // expected-error@-1:22 {{trailing closure passed to parameter of type 'String' that does not accept a closure}}{{none}}
    let _ = defaulted { return () }
    // expected-error@-1:23 {{trailing closure passed to parameter of type 'String' that does not accept a closure}}{{none}}
  }
  // Trailing closure considered fulfilled by 'arg'; has function type.
  do {
    func variadic(arg: ((Int) -> Void)...) {}
    func defaulted(arg: ((Int) -> Void) = { _ in }) {}

    let _ = variadic { return () }
    // expected-error@-1:22 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}}{{23-23= _ in}}
    let _ = defaulted { return () }
    // expected-error@-1:23 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}}{{24-24= _ in}}
  }
}
