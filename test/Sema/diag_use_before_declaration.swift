// RUN: %target-typecheck-verify-swift

// SR-5163
func sr5163() {
  func foo(_ x: Int) -> Int? { return 1 }
  
  func fn() {
    let a = foo(c) // expected-error {{use of local variable 'c' before its declaration}}
    guard let b = a else { return }
    let c = b // expected-note {{'c' declared here}}
  }
}

// SR-6726
var foo: Int?

func test() {
  guard let bar = foo else {
    return
  }
  let foo = String(bar)
}

// SR-7660
class C {
  var variable: Int?
  func f() {
    guard let _ = variable else { return }
    let variable = 1 // expected-warning {{initialization of immutable value 'variable' was never used; consider replacing with assignment to '_' or removing it}}
  }
}

//===----------------------------------------------------------------------===//
// Nested scope
//===----------------------------------------------------------------------===//

func nested_scope_1() {
  do {
    do {
      let _ = x // expected-error {{use of local variable 'x' before its declaration}}
      let x = 111 // expected-note {{'x' declared here}}
    }
    let x = 11
  }
  let x = 1
}

func nested_scope_2() {
  do {
    let x = 11
    do {
      let _ = x
      let x = 111 // expected-warning {{initialization of immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}}
    }
  }
  let x = 1  // expected-warning {{initialization of immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}}
}

func nested_scope_3() {
  let x = 1
  do {
    do {
      let _ = x
      let x = 111 // expected-warning {{initialization of immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}}
    }
    let x = 11 // expected-warning {{initialization of immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}}
  }
}

//===----------------------------------------------------------------------===//
// Type scope
//===----------------------------------------------------------------------===//

class Ty {
  var v : Int?

  func fn() {
    let _ = v
    let v = 1 // expected-warning {{initialization of immutable value 'v' was never used; consider replacing with assignment to '_' or removing it}}
  }
}

//===----------------------------------------------------------------------===//
// File scope
//===----------------------------------------------------------------------===//

let g = 0

func file_scope_1() {
  let _ = g
  let g = 1 // expected-warning {{initialization of immutable value 'g' was never used; consider replacing with assignment to '_' or removing it}}
}

func file_scope_2() {
  let _ = gg // expected-error {{use of local variable 'gg' before its declaration}}
  let gg = 1 // expected-note {{'gg' declared here}}
}

//===----------------------------------------------------------------------===//
// Module scope
//===----------------------------------------------------------------------===//

func module_scope_1() {
  let _ = print // Legal use of func print declared in Swift Standard Library
  let print = "something" // expected-warning {{initialization of immutable value 'print' was never used; consider replacing with assignment to '_' or removing it}}
}

func module_scope_2() {
  let _ = another_print // expected-error {{use of local variable 'another_print' before its declaration}}
  let another_print = "something" // expected-note {{'another_print' declared here}}
}
