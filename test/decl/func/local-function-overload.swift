// RUN: %target-typecheck-verify-swift

func valid1() {
  func inner(_: Int) {}
  func inner(_: String) {}

  func inner(label: Int) {}
  func inner(label: String) {}

  inner(123)
  inner("hello")

  inner(label: 123)
  inner(label: "hello")
}

func valid2() {
  func inner(_: Int = 0) {}
  func inner() -> Bool {}
  func inner(first: Int, second: Int = 0) {}

  let _: Bool = inner()
  let _ = inner()

  inner(first: 123)
}

func invalid1() {
  func inner(_: Int) {}
  // expected-note@-1 {{'inner' previously declared here}}
  func inner(_: Int) {}
  // expected-error@-1 {{invalid redeclaration of 'inner'}}
}

func invalid2() {
  func inner(_: Int) {}
  // expected-note@-1 {{candidate expects value of type 'Int' for parameter #1}}
  // expected-note@-2 {{found this candidate}}
  // expected-note@-3 {{did you mean 'inner'?}}
  func inner(_: String) {}
  // expected-note@-1 {{candidate expects value of type 'String' for parameter #1}}
  // expected-note@-2 {{found this candidate}}

  func inner(label: Int) {}
  // expected-note@-1 {{found this candidate}}

  inner([])
  // expected-error@-1 {{no exact matches in call to local function 'inner'}}

  inner(label: "hi")
  // expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'Int'}}

  _ = inner
  // expected-error@-1 {{ambiguous use of 'inner'}}

  _ = inner(label:) // no-error

  // FIXME: This isn't as good as in the non-local function case?
  _ = inner(invalidLabel:)
  // expected-error@-1 {{cannot find 'inner(invalidLabel:)' in scope}}
}
