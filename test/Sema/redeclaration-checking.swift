// RUN: %target-typecheck-verify-swift

// Test redeclaration checking in local context.

func test1() {
  let x = 123 // expected-note{{'x' previously declared here}}
  func f() {} // expected-note{{'f()' previously declared here}}
  struct S {} // expected-note{{'S' previously declared here}}
  let x = 321 // expected-error{{invalid redeclaration of 'x'}}
  func f() {} // expected-error{{invalid redeclaration of 'f()'}}
  struct S {} // expected-error{{invalid redeclaration of 'S'}}
}

func test2() {
  let x = 123 // expected-warning {{never used}}
  func f() {}
  struct S {}
  do {
    let x = 321 // expected-warning {{never used}}
    func f() {}
    struct S {}
  }
}

func test3<T, T>(_: T, _: T) {}
// expected-note@-1 {{'T' previously declared here}}
// expected-error@-2 {{invalid redeclaration of 'T'}}
// expected-error@-3 {{generic parameter 'T' is not used in function signature}}

func test4(x: Int, x: Int) {}
// expected-note@-1 {{'x' previously declared here}}
// expected-error@-2 {{invalid redeclaration of 'x'}}

struct Test4<T, T> {}
// expected-note@-1 {{'T' previously declared here}}
// expected-error@-2 {{invalid redeclaration of 'T'}}

typealias Test5<T, T> = ()
// expected-note@-1 {{'T' previously declared here}}
// expected-error@-2 {{invalid redeclaration of 'T'}}

enum E {
  case test6(x: Int, x: Int)
  // expected-note@-1 {{'x' previously declared here}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}

  subscript(x: Int, x: Int) -> Int { return 0 }
  // expected-note@-1 {{'x' previously declared here}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}
}

_ = { (x: Int, x: Int) in }
// expected-note@-1 {{'x' previously declared here}}
// expected-error@-2 {{invalid redeclaration of 'x'}}

enum MyError : Error {
  case error(Int, Int)
}

func stmtTest() {
  let n: (Int, Int)? = nil

  if case (let x, let x)? = n {}
  // expected-note@-1 {{'x' previously declared here}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}

  for case (let x, let x) in [(Int, Int)]() {}
  // expected-note@-1 {{'x' previously declared here}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}

  switch n {
  case (let x, let x)?: _ = ()
  // expected-note@-1 {{'x' previously declared here}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}
  case nil: _ = ()
  }

  while case (let x, let x)? = n {}
  // expected-note@-1 {{'x' previously declared here}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}

  guard case (let x, let x)? = n else {}
  // expected-note@-1 {{'x' previously declared here}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}

  do {} catch MyError.error(let x, let x) {}
  // expected-note@-1 {{'x' previously declared here}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}
  // expected-warning@-3 {{unreachable}}
}

func fullNameTest() {
  let x = 123 // expected-warning {{never used}}
  func x() {}
}

// For source compatibility, allow duplicate parameter labels on
// protocol requirements.
protocol SillyProtocol {
  init(x: Int, x: Int)
  init(a x: Int, b x: Int)

  func foo(x: Int, x: Int)
  func foo(a x: Int, b x: Int)

  subscript(x: Int, x: Int) -> Int { get }
  subscript(a x: Int, b x: Int) -> Int { get }
}

// https://github.com/apple/swift/issues/63750
let issue63750 = {
  for (x,x) in [(0,0)] {}
  // expected-error@-1 {{invalid redeclaration of 'x'}}
  // expected-note@-2 {{'x' previously declared here}}

  if case let (x,x) = (0,0) {}
  // expected-error@-1 {{invalid redeclaration of 'x'}}
  // expected-note@-2 {{'x' previously declared here}}

  switch (0,0) {
  case let (x,x):
    // expected-error@-1 {{invalid redeclaration of 'x'}}
    // expected-note@-2 {{'x' previously declared here}}
    ()
  }
  
  func bar(_ x: Int) -> Int { x }
  if case (bar(let x), let x) = (0,0) {}
  // expected-error@-1 {{'let' binding pattern cannot appear in an expression}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}
  // expected-note@-3 {{'x' previously declared here}}
}
func issue63750fn() {
  // Make sure the behavior is consistent with the multi-statement closure case.
  for (x,x) in [(0,0)] {}
  // expected-error@-1 {{invalid redeclaration of 'x'}}
  // expected-note@-2 {{'x' previously declared here}}

  if case let (x,x) = (0,0) {} // expected-warning {{'if' condition is always true}}
  // expected-error@-1 {{invalid redeclaration of 'x'}}
  // expected-note@-2 {{'x' previously declared here}}

  switch (0,0) {
  case let (x,x):
    // expected-error@-1 {{invalid redeclaration of 'x'}}
    // expected-note@-2 {{'x' previously declared here}}
    ()
  }
  func bar(_ x: Int) -> Int { x }
  if case (bar(let x), let x) = (0,0) {}
  // expected-error@-1 {{'let' binding pattern cannot appear in an expression}}
  // expected-error@-2 {{invalid redeclaration of 'x'}}
  // expected-note@-3 {{'x' previously declared here}}
}
