// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/47739
do {
  func foo(_ x: Int) -> Int? { return 1 }
  
  func fn() {
    let a = foo(c) // expected-error {{use of local variable 'c' before its declaration}}
    guard let b = a else { return }
    let c = b // expected-note {{'c' declared here}}
  }
}

// https://github.com/apple/swift/issues/49275

var foo: Int?

test: do {
  guard let bar = foo else {
    break test
  }
  let foo = String(bar) // expected-warning {{initialization of immutable value 'foo' was never used; consider replacing with assignment to '_' or removing it}}
}

// https://github.com/apple/swift/issues/50200
class C {
  var variable: Int?
  func f() {
    guard let _ = variable else { return }
    let variable = 1 // expected-warning {{initialization of immutable value 'variable' was never used; consider replacing with assignment to '_' or removing it}}
  }
}

// https://github.com/apple/swift/issues/50059
do {
  let app = app2 // expected-error {{use of local variable 'app2' before its declaration}}
  let app2 = app // expected-note {{'app2' declared here}}
}

// https://github.com/apple/swift/issues/50968
func test_circular() {
  let obj = x // expected-error {{use of local variable 'x' before its declaration}}
  let _ = obj.prop, x // expected-note {{'x' declared here}} expected-error {{type annotation missing in pattern}}
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

func captureInClosure() {
  let x = { (i: Int) in
    currentTotal += i // expected-error {{use of local variable 'currentTotal' before its declaration}}
  }

  var currentTotal = 0 // expected-note {{'currentTotal' declared here}}

  _ = x
}

class class77933460 {}

func func77933460() {
  var obj: class77933460 = { obj }()
  // expected-error@-1 {{use of local variable 'obj' before its declaration}}
  // expected-note@-2 {{'obj' declared here}}
}

protocol P {}

enum E {
  static func static_gen_fwd<T>(_ g: () -> T) -> T { g() }
}

func global_fwd(_ a: () -> Any) -> Any { a() }
func global_gen_fwd<T>(_ g: () -> T) -> T { g() }
func global_fwd_p(_ p: () -> any P) -> any P { p() }

func forward_declared_let_captures() {
  do {
    let bad: Any = { bad }()
    // expected-error@-1 {{use of local variable 'bad' before its declaration}}
    // expected-note@-2 {{'bad' declared here}}
  }

  do {
    func fwd(_ i: () -> Any) -> Any { i() }
    let bad = fwd { bad }
    // expected-error@-1 {{use of local variable 'bad' before its declaration}}
    // expected-note@-2 {{'bad' declared here}}
  }

  do {
    let bad = global_fwd { bad }
    // expected-error@-1 {{use of local variable 'bad' before its declaration}}
    // expected-note@-2 {{'bad' declared here}}
  }

  do {
    let bad: Any = global_gen_fwd { bad }
    // expected-error@-1 {{use of local variable 'bad' before its declaration}}
    // expected-note@-2 {{'bad' declared here}}
  }

  do {
    let bad: Any = E.static_gen_fwd { bad }
    // expected-error@-1 {{use of local variable 'bad' before its declaration}}
    // expected-note@-2 {{'bad' declared here}}
  }

  do {
    let badNested: Any = global_fwd { { [badNested] in badNested }() }
    // expected-error@-1 {{use of local variable 'badNested' before its declaration}}
    // expected-note@-2 {{'badNested' declared here}}
  }

  do {
    let badOpt: Any? = { () -> Any? in badOpt }()
    // expected-error@-1 {{use of local variable 'badOpt' before its declaration}}
    // expected-note@-2 {{'badOpt' declared here}}
  }

  do {
    let badTup: (Any, Any) = { (badTup.0, badTup.1) }()
    // expected-error@-1 2{{use of local variable 'badTup' before its declaration}}
    // expected-note@-2 2{{'badTup' declared here}}
  }

  do {
    let badTup: (Int, Any) = { (badTup.0, badTup.1) }()
    // expected-error@-1 2{{use of local variable 'badTup' before its declaration}}
    // expected-note@-2 2{{'badTup' declared here}}
  }

  do {
    let (badTup3, badTup4): (Any, Any) = { (badTup4, badTup3) }()
    // expected-error@-1 {{use of local variable 'badTup3' before its declaration}}
    // expected-note@-2 {{'badTup3' declared here}}
    // expected-error@-3 {{use of local variable 'badTup4' before its declaration}}
    // expected-note@-4 {{'badTup4' declared here}}
  }

  do {
    struct S { var p: Any }
    let badStruct: S = { S(p: badStruct.p) }()
    // expected-error@-1 {{use of local variable 'badStruct' before its declaration}}
    // expected-note@-2 {{'badStruct' declared here}}
  }

  do {
    enum EE {
      case boring
      case weird(Any)
      case strange(Any)
    }

    let badEnum: EE = { .weird(EE.strange(badEnum)) }()
    // expected-error@-1 {{use of local variable 'badEnum' before its declaration}}
    // expected-note@-2 {{'badEnum' declared here}}
  }

  do {
    let badproto: any P = global_fwd_p { badproto }
    // expected-error@-1 {{use of local variable 'badproto' before its declaration}}
    // expected-note@-2 {{'badproto' declared here}}
  }
}

func forward_declared_local_lazy_captures() {
  lazy var infiniteRecurse: Any = { infiniteRecurse }()
  // expected-error@-1 {{use of local variable 'infiniteRecurse' before its declaration}}
  // expected-note@-2 {{'infiniteRecurse' declared here}}

  lazy var hmm: () -> Any = { hmm }
  // expected-error@-1 {{use of local variable 'hmm' before its declaration}}
  // expected-note@-2 {{'hmm' declared here}}
}

func forward_declared_computed_locals() {
  // In principle we could allow these, but it's simpler to just reject them.
  let x = z // expected-error {{use of local variable 'z' before its declaration}}
  let y = { z } // expected-error {{use of local variable 'z' before its declaration}}
  var z: Int { 0 } // expected-note 2{{'z' declared here}}
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
