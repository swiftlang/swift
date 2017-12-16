// RUN: %target-typecheck-verify-swift

func basic() {
  var i: Int! = 0
  let _: Int = i
  i = 7
}

func takesIUOs(i: Int!, j: inout Int!) -> Int {
  j = 7
  return i
}

struct S {
  let i: Int!
  var j: Int!
  let k: Int
  var m: Int

  func fn() -> Int! { return i }

  static func static_fn() -> Int! { return 0 }

  init(i: Int!, j: Int!, k: Int, m: Int) {
    self.i = i
    self.j = j
    self.k = k
    self.m = m
  }

  init!() {
    i = 0
    j = 0
    k = 0
    m = 0
  }
}

func takesStruct(s: S) {
  let _: Int = s.i
  let _: Int = s.j
  var t: S! = s
  t.j = 7
}

var a: (Int, Int)! = (0, 0)
a.0 = 42

var s: S! = S(i: nil, j: 1, k: 2, m: 3)
_ = s.i
let _: Int = s.j
_ = s.k
s.m = 7
s.j = 3

var s2: S = S()

struct T {
  let i: Float!
  var j: Float!

  func fn() -> Float! { return i }
}

func overloaded() -> S { return S(i: 0, j: 1, k: 2, m: 3) }
func overloaded() -> T { return T(i: 0.5, j: 1.5) }

let _: Int = overloaded().i

func cflow(i: Int!, j: inout Bool!, s: S) {
  let k: Int? = i
  let m: Int = i
  let b: Bool! = i == 0

  if i == 7 {
    if s.i == 7 {
    }
  }
  let _ = i ? 7 : 0 // expected-error {{optional type 'Int!' cannot be used as a boolean; test for '!= nil' instead}}
  let _ = b ? i : k
  let _ = b ? i : m
  let _ = b ? j : b
  let _ = i ? i : k // expected-error {{result values in '? :' expression have mismatching types 'Int!' and 'Int?'}}
  let _ = i ? i : m // expected-error {{result values in '? :' expression have mismatching types 'Int!' and 'Int'}}
  let _ = s.i ? s.j : s.k // expected-error {{result values in '? :' expression have mismatching types 'Int!' and 'Int'}}
  let _ = b ? s.j : s.k

  if j {}
  let _ = j ? 7 : 0
}

func forcedResultInt() -> Int! {
  return 0
}

let _: Int = forcedResultInt()

func forcedResult() -> Int! {
  return 0
}

func forcedResult() -> Float! {
  return 0
}

func overloadedForcedResult() -> Int {
  return forcedResult()
}

func forceMemberResult(s: S) -> Int {
  return s.fn()
}

func forceStaticMemberResult() -> Int {
  return S.static_fn()
}

func overloadedForceMemberResult() -> Int {
  return overloaded().fn()
}

func overloadedForcedStructResult() -> S! { return S(i: 0, j: 1, k: 2, m: 3) }
func overloadedForcedStructResult() -> T! { return T(i: 0.5, j: 1.5) }

let _: S = overloadedForcedStructResult()
let _: Int = overloadedForcedStructResult().i

let x: Int? = 1
let y0: Int = x as Int! // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}
let y1: Int = (x as Int!)! // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}
let z0: Int = x as! Int! // expected-warning {{forced cast from 'Int?' to 'Int!' always succeeds; did you mean to use 'as'?}}
// expected-warning@-1 {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}
let z1: Int = (x as! Int!)! // expected-warning {{forced cast from 'Int?' to 'Int!' always succeeds; did you mean to use 'as'?}}
// expected-warning@-1 {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}
let w0: Int = (x as? Int!)! // expected-warning {{conditional cast from 'Int?' to 'Int!' always succeeds}}
// expected-warning@-1 {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}
let w1: Int = (x as? Int!)!! // expected-warning {{conditional cast from 'Int?' to 'Int!' always succeeds}}
// expected-warning@-1 {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}

func id<T>(_ t: T) -> T { return t }

protocol P { }
extension P {
  func iuoResult(_ b: Bool) -> Self! { }
  static func iuoResultStatic(_ b: Bool) -> Self! { }
}

func cast<T : P>(_ t: T) {
  let _: (T) -> (Bool) -> T? = id(T.iuoResult as (T) -> (Bool) -> T?)
  let _: (Bool) -> T? = id(T.iuoResult(t) as (Bool) -> T?)
  let _: T! = id(T.iuoResult(t)(true))
  let _: (Bool) -> T? = id(t.iuoResult as (Bool) -> T?)
  let _: T! = id(t.iuoResult(true))
  let _: T = id(t.iuoResult(true))
  let _: (Bool) -> T? = id(T.iuoResultStatic as (Bool) -> T?)
  let _: T! = id(T.iuoResultStatic(true))
}
