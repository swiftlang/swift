// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature UnificationDiagnostic -primary-file -verify-ignore-unrelated %s

var afterMessageCount : Int?

func uintFunc() -> UInt {}
func takeVoidVoidFn(_ a : () -> ()) {}
takeVoidVoidFn { () -> Void in
  afterMessageCount = uintFunc()  // expected-error {{cannot assign value of type 'UInt' to type 'Int'}} {{23-23=Int(}} {{33-33=)}}
}

func f19997471(_ x: String) {}
func f19997471(_ x: Int) {}

// FIXME: List both options of String and Int, not func is overloaded
func someGeneric19997471<T>(_ x: T) {
  takeVoidVoidFn {
    f19997471(x) // expected-error {{cannot convert value of type 'T' to expected argument type 'Int'}}
  }
}

[1].forEach { _ in
  _ = 1 + "hi" // expected-error {{cannot convert value of type 'String' for use in overloaded function; changing to 'Int' reduces ambiguity}}
  // TODO: regaining binary operator would improve message
}

_ = 1 + "hi" // expected-error {{binary operator '+' cannot be applied to operands of type 'Int' and 'String'}}
// expected-note@-1 {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String)}}
// TODO: These two expressions should have a consistent message

// Multiple different overloads, none of which conform to Sequence
func fn(_: Int) -> Int {}
func fn(_: Int) -> Double {}

func test(_: () -> Void) {}
func test(_: (Int) -> Void) {}

// FIXME: Show there are other possible types that have been seen for both cases
test {
  for x in fn { // expected-error {{for-in loop requires '(Int) -> Double' to conform to 'Sequence'}}
    print(x)
  }
}

test {
  for x in fn(42) { // expected-error {{for-in loop requires 'Double' to conform to 'Sequence'}}
    print(x)
  }
}

// FIXME: Show that fn is an overload
test {
  fn(5) && fn(5) // expected-error 2 {{cannot convert value of type 'Double' to expected argument type 'Bool'}}
}


class Aaron {
  init(x: Int) {
    func foo() {
      // Make sure we recover and assume 'self.init'.
      // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{11-11=self.}}
      // expected-error@+1 {{failed to produce diagnostic for expression}}
      _ = init
    }
  }
  convenience init() {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{5-5=self.}}
    // expected-error@+1 {{cannot convert value of type 'Bool' to expected argument type 'Int'}}
    init(x: true)

    // FIXME: self.init considered initializer delegation in nested function?
    // expected-error@+2 {{initializer delegation ('self.init') cannot be nested in another expression}}
    // expected-error@+1 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{22-22=self.}}
    func foo() { _ = init() }
  }

  required init(y: Int) {}
}


class Theodosia: Aaron {
  // expected-error@+3 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{20-20=self.}}
  // expected-error@+2 {{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}}
  // expected-error@+1 {{cannot reference 'mutating' method as function value}}
  func foo() { _ = init }

  required init(y: Int) {}
}

//TODO: Fix this with foundation import as well
//TODO: There are many potential types other than '()' which will provide a better diagnostic
func f_54877(_ e: Error) {
  func foo<T>(_ a: T, _ op: ((T, T) -> Bool)) {}
  foo(e, ==) // expected-error {{cannot convert value of type 'any Error' for use in overloaded function; looking for a type like '()'}}
}
