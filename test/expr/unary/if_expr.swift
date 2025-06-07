// RUN: %target-typecheck-verify-swift -disable-availability-checking

// MARK: Functions

func foo() -> Int {
  if .random() { 1 } else { 2 }
}

func foo2() -> Int {
  return if .random() { 1 } else { 2 }
}

func foo3() -> Int {
  if .random() { 1 } else { 2 } as Int
}

func foo4() -> Int {
  return if .random() { 1 } else { 2 } as Int
}

func foo5() -> Int {
  // We only allow coercions as a narrow case in the parser, so attempting to
  // double them up is invalid.
  if .random() { 1 } else { 2 } as Int as Int
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected expression}}
}

func foo6() -> Int {
  let x = if .random() { 1 } else { 2 } as Int as Int
  return x
}

func foo7() -> String {
  if .random() { 1 } else { 2 } as String
  // expected-error@-1 {{cannot convert value of type 'Int' to specified type 'String'}}
}

func foo8() -> Int {
  return (if .random() { 1 } else { 2 } as Int)
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}

func foo9() -> String? {
  if .random() { 1 as Any } else { 2 as Any } as? String
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}

func foo10() -> String {
  if .random() { 1 as Any } else { 2 as Any } as! String
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}

func foo11() -> Bool {
  // We don't parse this.
  if .random() { 1 as Any } else { 2 as Any } is String
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected expression}}
  // expected-error@-3 {{cannot convert value of type 'Any' to specified type 'Bool'}}
}

func foo12() -> Bool {
  let x = if .random() { 1 as Any } else { 2 as Any } is String
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
  return x
}

func bar() -> Int {
  if .random() {
    fatalError()
  } else {
    2
  }
}

func baz() -> Int {
  if .random() {
    "" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}
  } else {
    0
  }
}

func qux(_ x: Int?) -> Int {
  if let x = x { x } else { 0 }
}

func quux(_ x: Int?) -> Int {
  if case let x? = x { x } else { 0 }
}

func elseIf(_ x: Int?) -> Int {
  if .random() {
    0
  } else if let x = x {
    x
  } else if .random() {
    1
  } else {
    7 + 8
  }
}

func takesValue<T>(_ x: T) {}

// expected-error@+1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
takesValue(if .random() {
  0
} else {
  1
})
takesValue(if .random() { 0 } else { 1 })
// expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

// Cannot parse labeled if as expression.
do {
  takesValue(x: if .random() { 0 } else { 1 })
  // expected-error@-1 {{extraneous argument label 'x:' in call}}
  // expected-error@-2 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  takesValue(_: x: if .random() { 0 } else { 1 })
  // expected-error@-1 {{expected argument label before colon}}
  // expected-error@-2 {{expected ',' separator}}
  // expected-error@-3 {{cannot find 'x' in scope}}
  // expected-error@-4 {{extra argument in call}}
  // expected-error@-5 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}
func takesValueWithLabel<T>(x: T) {}
do {
  takesValueWithLabel(x: if .random() { 1 } else { 2 })
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  takesValueWithLabel(x: y: if .random() { 1 } else { 2 })
  // expected-error@-1 {{expected argument label before colon}}
  // expected-error@-2 {{expected ',' separator}}
  // expected-error@-3 {{cannot find 'y' in scope}}
  // expected-error@-4 {{extra argument in call}}
  // expected-error@-5 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}
func takesValueAndTrailingClosure<T>(_ x: T, _ fn: () -> Int) {}
takesValueAndTrailingClosure(if .random() { 0 } else { 1 }) { 2 }
// expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

func takesInOut<T>(_ x: inout T) {}
takesInOut(&if .random() { 1 } else { 2 })
// expected-error@-1 {{cannot pass immutable value of type 'Int' as inout argument}}
// expected-error@-2 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

struct HasSubscript {
  static subscript(x: Int) -> Void { () }

  subscript(x: Int...) -> Void { () }
}
HasSubscript[if .random() { 1 } else { 2 }]
// expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

func proposalExample1(isRoot: Bool, count: Int, willExpand: Bool, maxDepth: Int) -> String {
  let bullet =
      if isRoot && (count == 0 || !willExpand) { "" }
      else if count == 0 { "- " }
      else if maxDepth <= 0 { "▹ " }
      else { "▿ " }
  return bullet
}

var inAComputedVar: String {
  if .random() { "a" } else { "b" }
}

// MARK: Outer pound if

func withPoundIf() -> Int {
  #if true
  if .random() { 0 } else { 1 }
  #endif
}

func withPoundIfClosure() -> Int {
  let fn = {
    #if true
    if .random() { 0 } else { 1 }
    #endif
  }
  return fn()
}

func withPoundIfElse1() -> Int {
  #if true
  if .random() { 0 } else { 1 }
  #else
  0
  #endif
}

func withPoundIfElse2() -> Int {
  #if true
  0
  #else
  if .random() { 0 } else { 1 }
  #endif
}

func withPoundIfElseIf1() -> Int {
  #if true
  if .random() { 0 } else { 1 }
  #elseif true
  0
  #endif
}


func withPoundIfElseIf2() -> Int {
  #if true
  0
  #elseif true
  if .random() { 0 } else { 1 }
  #endif
}

func withPoundIfElseIfElse1() -> Int {
  #if true
  if .random() { 0 } else { 1 }
  #elseif true
  0
  #else
  0
  #endif
}

func withPoundIfElseIfElse2() -> Int {
  #if true
  0
  #elseif true
  if .random() { 0 } else { 1 }
  #else
  0
  #endif
}

func withPoundIfElseIfElse3() -> Int {
  #if true
  0
  #elseif true
  0
  #else
  if .random() { 0 } else { 1 }
  #endif
}

func withVeryNestedPoundIf() -> Int {
  #if true
    #if true
      #if false
      ""
      #else
      if .random() { 0 } else { 1 }
      #endif
    #elseif true
    0
    #endif
  #endif
}

func withVeryNestedPoundIfClosure() -> Int {
  let fn = {
    #if true
      #if true
        #if false
            ""
        #else
            if .random() { 0 } else { 1 }
        #endif
      #elseif true
          0
      #endif
    #endif
  }
  return fn()
}

// MARK: Explicit returns

func explicitReturn1() -> Int {
  print("hello")
  return if .random() { 0 } else { 1 }
}

func explicitReturn2() -> Int {
  return
  if .random() { 0 } else { 1 }
  // expected-warning@-1 {{expression following 'return' is treated as an argument of the 'return'}}
  // expected-note@-2 {{indent the expression to silence this warning}}
}

func explicitReturn3() -> Int {
  return
    if .random() { 0 } else { 1 }
}

func explicitReturn4() {
  // This used to be legal, but is now treated as a return of the if expression.
  return
    if .random() { 0 } else { 1 }
  // expected-error@-1 {{unexpected non-void return value in void function}}
  // expected-note@-2 {{did you mean to add a return type?}}
}

func explicitReturn5() {
  return;
  if .random() { 0 } else { 1 } // expected-warning 2{{integer literal is unused}}
}

func explicitReturn6() {
  return ()
  if .random() { 0 } else { 1 } // expected-warning 2{{integer literal is unused}}
}

var explicitReturn7: String {
  return if .random() { "a" } else { "b" }
}

struct AsPropertyInit {
  var x: Int = if Bool.random() { 1 } else { 0 }
  var y = if .random() { 1 } else { 0 }
}

func testNestedAssignment() {
  var x = 0
  x = if .random() { 0 } else { 1 } // Okay
  let fn = {
    x = if .random() { 0 } else { 1 } // Also okay
  }

  // We don't allow in a nested assignment.
  // TODO: We could improve this error.
  print(x = if .random() { 0 } else { 1 })
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  _ = x; _ = fn
}

struct TestFailableInit {
  init?(_ x: Bool) {
    let y = if x {
      0
    } else {
      return nil // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
    }
    _ = y
  }
}

struct TestFailableInitFatalError {
  init?() {
    // In this case, the if does not become an expression.
    if .random() {
      fatalError()
    } else {
      return nil
    }
  }
}

// MARK: Expressions

let a = if .random() {
  0
} else {
  1
}

let b = (if .random() { 1 } else { 2 })  // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

let c = (if .random() { 1 } else { 2 }, k: if .random() { 1 } else { 2 })  // expected-error 2{{'if' may only be used as expression in return, throw, or as the source of an assignment}}

var d = if .random() { if .random() { 1 } else { 2 } } else { 3 }

d = if .random() { 0 } else { 1 }

let e = "\(if .random() { 1 } else { 2 })" // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

let f = { if .random() { 1 } else { 2 } }

func throwsError() throws {
  struct E: Error {}
  throw if .random() { E() } else { E() }
}

// FIXME: If we ever support this, we need to fix the premature inference of '[Any]'/'[AnyHashable: Any]'.
// The issue is that we're attempting to bind defaults to type variables before solving the conjuction.
let g = [if .random() { "a" } else { "b" }]
// expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
// expected-error@-2 {{heterogeneous collection literal could only be inferred to '[Any]'; add explicit type annotation if this is intentional}}

let h = [if .random() { 1 } else { 2 } : if .random() { "a" } else { "b" }]
// expected-error@-1 2{{'if' may only be used as expression in return, throw, or as the source of an assignment}}
// expected-error@-2 {{heterogeneous collection literal could only be inferred to '[AnyHashable : Any]'; add explicit type annotation if this is intentional}}

let i = [if .random() { 1 } else { 2 }: if .random() { "a" } else { "b" }]
// expected-error@-1 2{{'if' may only be used as expression in return, throw, or as the source of an assignment}}
// expected-error@-2 {{heterogeneous collection literal could only be inferred to '[AnyHashable : Any]'; add explicit type annotation if this is intentional}}

let j = [if .random() { 1 } else { 2 }:if .random() { "a" } else { "b" }]
// expected-error@-1 2{{'if' may only be used as expression in return, throw, or as the source of an assignment}}
// expected-error@-2 {{heterogeneous collection literal could only be inferred to '[AnyHashable : Any]'; add explicit type annotation if this is intentional}}

let k = [if .random() { 1 } else { 2 } :if .random() { "a" } else { "b" }]
// expected-error@-1 2{{'if' may only be used as expression in return, throw, or as the source of an assignment}}
// expected-error@-2 {{heterogeneous collection literal could only be inferred to '[AnyHashable : Any]'; add explicit type annotation if this is intentional}}

let l = if .random() { 1 } else { 2 } as Any

let _ = type(of: if .random() { 1 } else { 2 })  // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

let _ = (if .random() { () } else { () }) // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

let _ = if .random() { 0 } // expected-error {{'if' must have an unconditional 'else' to be used as expression}}
let _ = if .random() { 0 } else if .random() { 1 } // expected-error {{'if' must have an unconditional 'else' to be used as expression}}

func testNonExhaustiveInFunction() {
  if .random() { 0 } // expected-warning {{integer literal is unused}}
}

func testLabelRejection1() -> Int {
  // This was legal before, so remains legal.
  x: if .random() { 0 } else { 1 }
  // expected-warning@-1 2{{integer literal is unused}}
}

func testLabelRejection2() -> Int {
  // This was never legal, so reject.
  x: if .random() { 0 } else { 1 } as Int
  // expected-error@-1 {{'if' cannot have a jump label when used as expression}}
}

do {
  if .random() { 1 } else { 2 } = 3
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected expression}}
  // expected-warning@-3 2{{integer literal is unused}}}
}
let m: Void = if .random() { 1 } else { 2 } // expected-error {{cannot convert value of type 'Int' to specified type 'Void'}}
let n: Never = if .random() { 1 } else { 2 } // expected-error {{cannot convert value of type 'Int' to specified type 'Never'}}

func testConditionalBinding1(_ x: Int?) -> Int {
  if let x = if .random() { 0 } else { Int?.none } { // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    x
  } else {
    0
  }
}

func testConditionalBinding2(_ x: Int?) -> Int {
  if case let x? = if .random() { 0 } else { Int?.none } { // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    x
  } else {
    0
  }
}

// MARK: Operators

let o = !if .random() { true } else { false }  // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

// FIXME: Shouldn't be ambiguous
let p = if .random() { 1 } else { 2 } + // expected-error {{ambiguous use of operator '+'}}
        if .random() { 3 } else { 4 } +
        if .random() { 5 } else { 6 }
// expected-error@-3 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
// expected-error@-3 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
// expected-error@-3 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

let p1 = if .random() { 1 } else { 2 } +  5
// expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

let q = .random() ? if .random() { 1 } else { 2 }
                  : if .random() { 3 } else { 4 }
// expected-error@-2 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
// expected-error@-2 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

let r = if .random() { 1 } else { 2 }...if .random() { 1 } else { 2 }
// expected-error@-1 2{{'if' may only be used as expression in return, throw, or as the source of an assignment}}

let s = if .random() { 1 } else { 2 } ... if .random() { 1 } else { 2 }
// expected-error@-1 2{{'if' may only be used as expression in return, throw, or as the source of an assignment}}

// MARK: Lookup

do {
  let t = if .random() { t } else { 0 }
  // expected-error@-1 {{use of local variable 't' before its declaration}}
  // expected-note@-2 {{'t' declared here}}
}

// MARK: Postfix

// We don't allow postfix parsing.
do {
  let _ = if .random() { [1] } else { [1, 2] }.count
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{reference to member 'count' cannot be resolved without a contextual type}}

  let _ = (if .random() { [1] } else { [1, 2] }).count
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}
do {
  let _ = if .random() { Int?.none } else { 1 as Int? }?.bitWidth
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected expression}}

  // FIXME: The type error is likely due to not solving the conjunction before attempting default type var bindings.
  let _ = (if .random() { Int?.none } else { 1 as Int? })?.bitWidth
  // expected-error@-1 {{type of expression is ambiguous without a type annotation}}
  // expected-error@-2 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}
do {
  let _ = if .random() { Int?.none } else { 1 as Int? }!
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected expression}}

  let _ = (if .random() { Int?.none } else { 1 as Int? })!
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}
do {
  func takesInts(_ x: Int...) {}
  let _ = if .random() { takesInts } else { takesInts }(1, 2, 3)
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-warning@-2 {{expression of type '(Int, Int, Int)' is unused}}

  let _ = (if .random() { takesInts } else { takesInts })(1, 2, 3)
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}
func testSubscriptPostfix(_ x: HasSubscript) {
  if .random() { x } else { x }[1, 2, 3]
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-warning@-2 {{expression of type 'HasSubscript' is unused}}
  // expected-warning@-3 {{expression of type '[Int]' is unused}}
  // expected-warning@-4 {{expression of type 'HasSubscript' is unused}}

  (if .random() { x } else { x })[1, 2, 3]
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}
do {
  func takesClosure(_ fn: () -> Int) {}

  let _ = if .random() { takesClosure } else { takesClosure } { 3 }
  // expected-error@-1 {{getter/setter can only be defined for a single variable}}

  let _ = (if .random() { takesClosure } else { takesClosure }) { 3 }
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}

// MARK: Statements

func stmts() {
  if if .random() { true } else { false } {}
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  if try if .random() { true } else { false } {}
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
  // expected-warning@-2 {{'try' has no effect on 'if' expression}}

  // expected-error@+1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
  guard if .random() { true } else { false } else {
    return
  }

  switch if .random() { true } else { false } {
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
  case _ where if .random() { true } else { false }:
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    break
  case if .random() { true } else { false }:
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    break
  case if .random() { true } else { false } && false:
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    break
  default:
    break
  }

  for b in [true] where if b { true } else { false } {}
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  for _ in if .random() { [true] } else { [false] } {}
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  for _ in if .random() { [true] } else { [false] } {} // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  // Make sure this doesn't parse as an if expr pattern with a label.
  let x = 0
  switch 0 {
  case x: if .random() { 1 } else { 2 }
    // expected-warning@-1 2{{integer literal is unused}}
  default:
    break
  }
}

// MARK: Non-expression branches

func noElse() -> Int {
  // Not an expression because no else.
  if .random() {
    0 // expected-warning {{integer literal is unused}}
  }
  1 // expected-warning {{integer literal is unused}}
}

func returnBranches() -> Int {
  // This is not an expression because the branches are not expressions.
  if .random() {
    return 0
  } else {
    return 1
  }
}

func returnBranches1() -> Int {
  return if .random() { // expected-error {{cannot convert return expression of type 'Void' to return type 'Int'}}
    return 0 // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
  } else {
    return 1 // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
  }
}

func returnBranchVoid() {
  return if .random() { return } else { return () }
  // expected-error@-1 2{{cannot use 'return' to transfer control out of 'if' expression}}
}

func returnBranchBinding() -> Int {
  let x = if .random() {
    // expected-warning@-1 {{constant 'x' inferred to have type 'Void', which may be unexpected}}
    // expected-note@-2 {{add an explicit type annotation to silence this warning}}
    return 0 // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
  } else {
    return 1 // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
  }
  return x // expected-error {{cannot convert return expression of type 'Void' to return type 'Int'}}
}

func returnBranches2() -> Int {
  // We don't allow multiple expressions.
  if .random() {
    print("hello")
    0 // expected-warning {{integer literal is unused}}
  } else {
    1 // expected-warning {{integer literal is unused}}
  }
}

func returnBranches3() -> Int {
  if .random() {
    print("hello")
    return 0
  } else {
    1 // expected-warning {{integer literal is unused}}
  }
}

func returnBranches4() -> Int {
  if .random() { return 1 } else { 0 } // expected-warning {{integer literal is unused}}
}

struct Err: Error {}

func returnBranches5() throws -> Int {
  let i = if .random() {
    // expected-warning@-1 {{constant 'i' inferred to have type 'Void', which may be unexpected}}
    // expected-note@-2 {{add an explicit type annotation to silence this warning}}
    return 0 // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
  } else {
    return 1 // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
  }
  let j = if .random() {
    // expected-warning@-1 {{constant 'j' inferred to have type 'Void', which may be unexpected}}
    // expected-note@-2 {{add an explicit type annotation to silence this warning}}
    throw Err()
  } else {
    throw Err()
  }
  return i // expected-error {{cannot convert return expression of type 'Void' to return type 'Int'}}
}

func returnBranches6() -> Int {
  // We don't allow multiple expressions.
  let i = if .random() {
    print("hello")
    0 // expected-warning {{integer literal is unused}}
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    1
  }
  return i
}

func returnBranches6PoundIf() -> Int {
  // We don't allow multiple expressions.
  let i = if .random() {
    #if true
    print("hello")
    0 // expected-warning {{integer literal is unused}}
    #endif
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    1
  }
  return i
}

func returnBranches6PoundIf2() -> Int {
  // We don't allow multiple expressions.
  let i = if .random() { // expected-error{{expected expression in branch of 'if' expression}}
    #if false
    print("hello")
    0
    #endif
  } else {
    1
  }
  return i
}

func returnBranches7() -> Int {
  let i = if .random() {
    print("hello")
    return 0  // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
  } else {
    1
  }
  return i
}

func returnBranches8() -> Int {
  let i = if .random() { return 1 } else { 0 }  // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
  return i
}

func returnBranches9() -> Int {
  let i = if .random() {
    print("hello")
    if .random() {}
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    1
  }
  return i
}

func returnBranches10() -> Int {
  let i = if .random() {
    print("hello")
    if .random() {
      0 // expected-warning {{integer literal is unused}}
    } else {
      2 // expected-warning {{integer literal is unused}}
    }
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    1
  }
  return i
}

func returnBranches11() -> Int {
  let i = if .random() {
    print("hello")
    if .random() {
      "" // expected-warning {{string literal is unused}}
    } else {
      0 // expected-warning {{integer literal is unused}}
    }
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    1
  }
  return i
}

func returnBranches12() -> Int {
  if .random() {
    print("hello")
    if .random() {}
  } else {
    1 // expected-warning {{integer literal is unused}}
  }
}

func returnBranches13() -> Int {
  if .random() {
    print("hello")
    if .random() {
      0 // expected-warning {{integer literal is unused}}
    } else {
      2 // expected-warning {{integer literal is unused}}
    }
  } else {
    1 // expected-warning {{integer literal is unused}}
  }
}

func returnBranches14() -> Int {
  if .random() {
    print("hello")
    if .random() {
      "" // expected-warning {{string literal is unused}}
    } else {
      0 // expected-warning {{integer literal is unused}}
    }
  } else {
    1 // expected-warning {{integer literal is unused}}
  }
}

func nestedType() -> Int {
  if .random() {
    struct S {
      var x: Int
    }
    return S(x: 0).x
  } else {
    0 // expected-warning {{integer literal is unused}}
  }
}

func testEmptyBranch() -> Int {
  let x = if .random() {} else { 0 }
  // expected-error@-1:24 {{expected expression in branch of 'if' expression}}
  return x
}

// MARK: Pound if branches

func testPoundIfBranch1() -> Int {
  if .random() {
    #if true
    0
    #endif
  } else {
    0
  }
}

func testPoundIfBranch2() -> Int {
  if .random() {
    #if false
    0
    #endif
  } else {
    0 // expected-warning {{integer literal is unused}}
  }
}

func testPoundIfBranch3() -> Int {
  let x = if .random() { // expected-error{{expected expression in branch of 'if' expression}}
    #if false
    0
    #endif
  } else {
    0
  }
  return x
}

func testPoundIfBranch4() -> Int {
  if .random() {
    #if true
    0
    #endif
  } else {
    #if true
    0
    #endif
  }
}

func testPoundIfBranch5() -> Int {
  // Inactive #if regions don't count
  if .random() {
    #if false
    0
    #endif
    0
  } else {
    1
  }
}

func testPoundIfBranch6() -> Int {
  // Inactive #if regions don't count
  let x = if .random() {
    #if false
    0
    #endif
    0
  } else {
    1
  }
  return x
}

func testPoundIfBranch7() -> Int {
  if .random() {
    #if true
      #if true
        #if false
            ""
        #else
            0
        #endif
      #elseif true
          ""
      #endif
    #endif
  } else {
    0
  }
}

func testPoundIfBranch8() -> Int {
  if .random() {
    #if false
    0
    #else
    #if true
    if .random() { 0 } else { 1 }
    #endif
    #endif
  } else {
    #if true
    if .random() { 0 } else { 1 }
    #endif
  }
}

// MARK: Jumping

func break1() -> Int {
  switch true {
  case true:
    let j = if .random() {
      break // expected-error {{cannot use 'break' to transfer control out of 'if' expression}}
    } else {
      0
    }
    return j
  case false:
    return 0
  }
}

func continue1() -> Int {
  for _ in 0 ... 5 {
    let i = if true { continue } else { 1 }
    // expected-error@-1 {{cannot use 'continue' to transfer control out of 'if' expression}}
    return i
  }
}

func return1() -> Int {
  // Make sure we always reject a return.
  let i = if .random() {
    ()
    do {
      for _ in [0] {
        while true {
          switch 0 {
          default:
            return 0 // expected-error {{cannot use 'return' to transfer control out of 'if' expression}}
          }
        }
      }
    }
  } else {
    0
  }
  return i
}

func return2() throws -> Int {
  // In a nested function is okay though.
  let i = if .random() {
    func foo() { return }
    throw Err()
  } else {
    0
  }
  return i
}

func return3() throws -> Int {
  // A nested type is also okay.
  let i = if .random() {
    struct Nested {
      func foo() { return }
    }
    throw Err()
  } else {
    0
  }
  return i
}

func return4() throws -> Int {
  // A nested closure is also okay.
  let i = if .random() {
    let _ = { return }
    throw Err()
  } else {
    0
  }
  return i
}

// https://github.com/swiftlang/swift/issues/75880
func fallthrough1() throws {
  switch Bool.random() {
  case true:
    let _ = if .random() {
      if .random () {
        fallthrough // expected-error {{cannot use 'fallthrough' to transfer control out of 'if' expression}}
      }
      throw Err()
    } else {
      0
    }
  case false:
    break
  }
}

func fallthrough2() throws -> Int {
  let x = switch Bool.random() {
  case true:
    if .random() {
      if .random () {
        fallthrough // expected-error {{cannot use 'fallthrough' to transfer control out of 'if' expression}}
      }
      throw Err()
    } else {
      0
    }
  case false:
    1
  }
  return x
}

func fallthrough3() -> Int {
  let x = switch Bool.random() {
  case true:
    if .random() {
      fallthrough // expected-error {{cannot use 'fallthrough' to transfer control out of 'if' expression}}
    } else {
      0
    }
  case false:
    1
  }
  return x
}

func fallthrough4() -> Int {
  let x = if .random() {
    fallthrough // expected-error {{'fallthrough' is only allowed inside a switch}}
  } else {
    0
  }
  return x
}

// MARK: Effect specifiers

func tryIf1() -> Int {
  try if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
}

func tryIf2() -> Int {
  let x = try if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  return x
}

func tryIf3() -> Int {
  return try if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
}

func tryIf4() throws -> Int {
  return try if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
}

func tryIf5() throws -> Int {
  return try if .random() { tryIf4() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
}

func tryIf6() throws -> Int {
  try if .random() { tryIf4() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
}

func tryIf7() throws -> Int {
  let x = try if .random() { tryIf4() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
  return x
}

func tryIf8() throws -> Int {
  return try if .random() { try tryIf4() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
}

func tryIf9() throws -> Int {
  try if .random() { try tryIf4() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
}

func tryIf10() throws -> Int {
  let x = try if .random() { try tryIf4() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  return x
}

func tryIf11() throws -> Int {
  let x = try if .random() { try tryIf4() } else { tryIf4() }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
  return x
}

func tryIf12() throws -> Int {
  let x = try if .random() { tryIf4() } else { tryIf4() }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 2{{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 2{{did you mean to use 'try'?}}
  // expected-note@-4 2{{did you mean to handle error as optional value?}}
  // expected-note@-5 2{{did you mean to disable error propagation?}}
  return x
}

func tryIf13() throws -> Int {
  let x = try if .random() { // expected-warning {{'try' has no effect on 'if' expression}}
    tryIf4() // expected-warning {{result of call to 'tryIf4()' is unused}}
    // expected-warning@-1 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
    // expected-note@-2 {{did you mean to use 'try'?}}
    // expected-note@-3 {{did you mean to handle error as optional value?}}
    // expected-note@-4 {{did you mean to disable error propagation?}}

    _ = tryIf4()
    // expected-warning@-1 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
    // expected-note@-2 {{did you mean to use 'try'?}}
    // expected-note@-3 {{did you mean to handle error as optional value?}}
    // expected-note@-4 {{did you mean to disable error propagation?}}

    _ = try tryIf4() // Okay.

    // Okay.
    do {
      _ = try tryIf4()
    } catch {}

    print("hello")
    throw Err()
  } else {
    0
  }
  return x
}

func throwsBool() throws -> Bool { true }

func tryIf14() throws -> Int {
  try if throwsBool() { 0 } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
}

func tryIf15() throws -> Int {
  try if try throwsBool() { 0 } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
}

func tryIf16() throws -> Int {
  if throwsBool() { 0 } else { 1 }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{did you mean to use 'try'?}}
  // expected-note@-3 {{did you mean to handle error as optional value?}}
  // expected-note@-4 {{did you mean to disable error propagation?}}
}

func tryIf17() throws -> Int {
  if .random() { tryIf4() } else { 1 }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{did you mean to use 'try'?}}
  // expected-note@-3 {{did you mean to handle error as optional value?}}
  // expected-note@-4 {{did you mean to disable error propagation?}}
}

func tryIf18() {
  // Make sure we don't warn here.
  do {
    let _ = if .random() { try tryIf4() } else { 1 }
  } catch {}
}

func tryIf19() {
  // Make sure we don't warn here.
  do {
    let _ = if .random() { throw Err() } else { 1 }
  } catch {}
}

func tryIf19() throws -> Int {
  let x = if .random() { throw Err() } else { 1 }
  return x
}

func tryIf20() throws -> Int {
  if .random() { throw Err() } else { 1 }
}

func tryIf21(_ fn: () throws -> Int) rethrows -> Int {
  let x = if .random() { try fn() } else { 1 }
  return x
}

func tryIf22(_ fn: () throws -> Int) rethrows -> Int {
  if .random() { try fn() } else { 1 }
}

func tryIf23(_ fn: () throws -> Int) rethrows -> Int {
  let x = if .random() { try fn() } else { throw Err() }
  // expected-error@-1 {{a function declared 'rethrows' may only throw if its parameter does}}
  return x
}

func tryIf24(_ fn: () throws -> Int) rethrows -> Int {
  let x = if .random() { try fn() } else { try tryIf4() }
  // expected-error@-1 {{a function declared 'rethrows' may only throw if its parameter does}}
  return x
}

func tryIf25(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = if .random() { try fn() } else { try tryIf4() }
    return x
  } catch {
    return 0
  }
}

func tryIf26(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = if .random() { try fn() } else { throw Err() }
    return x
  } catch {
    return 0
  }
}

func tryIf27(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = if .random() { try fn() } else { try tryIf4() }
    return x
  } catch {
    throw error  // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func tryIf28(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = if .random() { try fn() } else { throw Err() }
    return x
  } catch {
    throw error  // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func tryIf29(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = if .random() { try fn() } else { 0 }
    return x
  } catch {
    throw error // Okay.
  }
}

func awaitIf1() async -> Int {
  await if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
}

func awaitIf2() async -> Int {
  let x = await if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
  return x
}

func awaitIf3() async -> Int {
  return await if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
}

func awaitIf4() async -> Int {
  return await if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
}

func awaitIf5() async -> Int {
  return await if .random() { awaitIf4() } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{call is 'async'}}
}

func awaitIf6() async -> Int {
  await if .random() { awaitIf4() } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{call is 'async'}}
}

func awaitIf7() async -> Int {
  let x = await if .random() { awaitIf4() } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{call is 'async'}}
  return x
}

func awaitIf8() async -> Int {
  return await if .random() { await awaitIf4() } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
}

func awaitIf9() async -> Int {
  await if .random() { await awaitIf4() } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
}

func awaitIf10() async -> Int {
  let x = await if .random() { await awaitIf4() } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
  return x
}

func awaitIf11() async -> Int {
  let x = await if .random() { await awaitIf4() } else { awaitIf4() }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{call is 'async'}}
  return x
}

func awaitIf12() async -> Int {
  let x = await if .random() { awaitIf4() } else { awaitIf4() }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
  // expected-warning@-2 2{{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 2{{call is 'async'}}
  return x
}

func awaitIf13() async throws -> Int {
  let x = await if .random() { // expected-warning {{'await' has no effect on 'if' expression}}
    awaitIf4() // expected-warning {{result of call to 'awaitIf4()' is unused}}
    // expected-warning@-1 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
    // expected-note@-2 {{call is 'async'}}

    _ = awaitIf4()
    // expected-warning@-1 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
    // expected-note@-2 {{call is 'async'}}

    _ = await awaitIf4() // Okay.

    // Okay.
    let _ = {
      _ = await awaitIf4()
    }

    print("hello")
    throw Err()
  } else {
    0
  }
  return x
}

func asyncBool() async -> Bool { true }

func awaitIf14() async -> Int {
  await if asyncBool() { 0 } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{call is 'async'}}
}

func awaitIf15() async -> Int {
  await if await asyncBool() { 0 } else { 1 }
  // expected-warning@-1 {{'await' has no effect on 'if' expression}}
}

func awaitIf16() async -> Int {
  if asyncBool() { 0 } else { 1 }
  // expected-error@-1 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@-2 {{call is 'async'}}
}

func awaitIf17() async -> Int {
  if .random() { awaitIf4() } else { 1 }
  // expected-error@-1 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@-2 {{call is 'async'}}
}

func awaitIf18() {
  let _ = {
    let _ = if .random() { await awaitIf4() } else { 1 }
  }
}

func awaitIf19() async -> Int {
  let x = if .random() { await awaitIf4() } else { 1 }
  return x
}

func awaitIf20() async -> Int {
  if .random() { await awaitIf4() } else { 1 }
}

func tryAwaitIf1() async throws -> Int {
  try await if .random() { 0 } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
}

func tryAwaitIf2() async throws -> Int {
  try await if .random() { 0 } else { 1 } as Int
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
}

func tryAwaitIf3() async throws -> Int {
  try await if .random() { tryAwaitIf2() } else { 1 } as Int
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
  // expected-warning@-3 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{did you mean to use 'try'?}}
  // expected-note@-5 {{did you mean to handle error as optional value?}}
  // expected-note@-6 {{did you mean to disable error propagation?}}
  // expected-warning@-7 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-8 {{call is 'async'}}
}

func tryAwaitIf4() async throws -> Int {
  try await if .random() { try tryAwaitIf2() } else { 1 } as Int
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
  // expected-warning@-3 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{call is 'async'}}
}

func tryAwaitIf5() async throws -> Int {
  try await if .random() { await tryAwaitIf2() } else { 1 } as Int
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
  // expected-warning@-3 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{did you mean to use 'try'?}}
  // expected-note@-5 {{did you mean to handle error as optional value?}}
  // expected-note@-6 {{did you mean to disable error propagation?}}
}

func tryAwaitIf6() async throws -> Int {
  try await if .random() { try await tryAwaitIf2() } else { 1 } as Int
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
}

func tryAwaitIf7() async throws -> Int {
  try await if .random() { tryAwaitIf2() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
  // expected-warning@-3 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{did you mean to use 'try'?}}
  // expected-note@-5 {{did you mean to handle error as optional value?}}
  // expected-note@-6 {{did you mean to disable error propagation?}}
  // expected-warning@-7 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-8 {{call is 'async'}}
}

func tryAwaitIf8() async throws -> Int {
  try await if .random() { try tryAwaitIf2() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
  // expected-warning@-3 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{call is 'async'}}
}

func tryAwaitIf9() async throws -> Int {
  try await if .random() { await tryAwaitIf2() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
  // expected-warning@-3 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{did you mean to use 'try'?}}
  // expected-note@-5 {{did you mean to handle error as optional value?}}
  // expected-note@-6 {{did you mean to disable error propagation?}}
}

func tryAwaitIf10() async throws -> Int {
  try await if .random() { try await tryAwaitIf2() } else { 1 }
  // expected-warning@-1 {{'try' has no effect on 'if' expression}}
  // expected-warning@-2 {{'await' has no effect on 'if' expression}}
}

func tryAwaitIf11(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = if .random() { try await fn() } else { try await tryAwaitIf4() }
    return x
  } catch {
    return 0
  }
}

func tryAwaitIf12(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = if .random() { try await fn() } else { throw Err() }
    return x
  } catch {
    return 0
  }
}

func tryAwaitIf13(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = if .random() { try await fn() } else { try await tryAwaitIf4() }
    return x
  } catch {
    throw error  // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func tryAwaitIf14(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = if .random() { try await fn() } else { throw Err() }
    return x
  } catch {
    throw error  // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func tryAwaitIf15(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = if .random() { try await fn() } else { 0 }
    return x
  } catch {
    throw error // Okay.
  }
}

func asyncLetIf(cond: Bool, _ fn: () async throws -> Int) async throws -> Int {
  async let x = if cond {
    fn()
  } else {
    0
  }

  return try await x
}

struct AnyEraserP: EraserP {
  init<T: EraserP>(erasing: T) {}
}

@_typeEraser(AnyEraserP)
protocol EraserP {}
struct SomeEraserP: EraserP {}

// rdar://113435870 - Make sure we allow an implicit init(erasing:) call.
dynamic func testDynamicOpaqueErase() -> some EraserP {
  if .random() { SomeEraserP() } else { SomeEraserP() }
}

struct NonExhaustiveProperty {
  let i = if .random() { 0 }
  // expected-error@-1 {{'if' must have an unconditional 'else' to be used as expression}}
}

// MARK: Out of place if exprs

func inDefaultArg(x: Int = if .random() { 0 } else { 0 }) {}
// expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

func inDefaultArg2(x: Int = { (if .random() { 0 } else { 0 }) }()) {}
// expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

struct InType {
  let inPropertyInit1 = if .random() { 0 } else { 1 }
  let inPropertyInit2 = (if .random() { 0 } else { 1 })
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  let inPropertyInit3 = {
    let _ = if .random() { 0 } else { 1 }
    let _ = (if .random() { 0 } else { 1 })
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

    func foo() {
      let _ = if .random() { 0 } else { 1 }
      let _ = (if .random() { 0 } else { 1 })
      // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    }
    if .random() {
      return if .random() { 0 } else { 1 }
    } else {
      return (if .random() { 0 } else { 1 })
      // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    }
  }

  subscript(x: Int = if .random() { 0 } else { 0 }) -> Int {
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

    let _ = if .random() { 0 } else { 1 }
    let _ = (if .random() { 0 } else { 1 })
    // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
    return 0
  }
}

func testCaptureList() {
  let _ = { [x = if .random() { 0 } else { 1 }] in x }
  let _ = { [x = (if .random() { 0 } else { 1 })] in x }
  // expected-error@-1 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
}
