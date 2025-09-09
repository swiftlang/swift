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
  var n: Int! {
    get {
      return m
    }
    set {
      m = newValue
    }
  }
  var o: Int! {
    willSet {
      m = newValue
    }
    didSet {
      m = oldValue
    }
  }

  func fn() -> Int! { return i }

  static func static_fn() -> Int! { return 0 }

  subscript(i: Int) -> Int! {
    set {
      m = newValue
    }
    get {
      return i
    }
  }

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

let _: Int = s[0]

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

  let _ = b ? i : k
  let _ = b ? i : m
  let _ = b ? j : b

  let _ = b ? s.j : s.k

  if b {}
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

class rdar37241550 {
  public init(blah: Float) { fatalError() }
  public convenience init() { fatalError() }
  public convenience init!(with void: ()) { fatalError() }

  static func f(_ fn: () -> rdar37241550) {}
  static func test() {
    f(rdar37241550.init) // no error, the failable init is not applicable
  }
}

// https://github.com/apple/swift/issues/49536
// Ensure that we select the overload that does *not* involve forcing an IUO.
do {
  func f(x: Int?, y: Int?) -> Int { return x! }
  func f(x: Int, y: Int) -> Float { return Float(x) }

  let x: Int! = nil
  let y: Int = 2

  let r = f(x: x, y: y)
  let _: Int = r
}

// rdar://problem/58455441
// https://github.com/apple/swift/issues/54432
do {
  class C<T> {}
  let _: C! = C<Int>()
}

// rdar://problem/83352038
// https://github.com/apple/swift/issues/57541
// Make sure we don't crash if an IUO param becomes a placeholder.
do {
  func foo(_: UnsafeRawPointer) -> Undefined {} // expected-error {{cannot find type 'Undefined' in scope}}
  let _ = { (cnode: AlsoUndefined!) -> UnsafeMutableRawPointer in // expected-error {{cannot find type 'AlsoUndefined' in scope}}
    return foo(cnode)
  }
}

// Make sure we reject an attempt at a function conversion.
func returnsIUO() -> Int! { 0 }
let _ = (returnsIUO as () -> Int)() // expected-error {{cannot convert value of type '() -> Int?' to type '() -> Int' in coercion}}

// Make sure we only permit an IUO unwrap on the first application.
func returnsIUOFn() -> (() -> Int?)! { nil }
let _: (() -> Int?)? = returnsIUOFn()
let _: (() -> Int)? = returnsIUOFn() // expected-error {{cannot assign value of type '(() -> Int?)?' to type '(() -> Int)?'}}
// expected-note@-1 {{arguments to generic parameter 'Wrapped' ('() -> Int?' and '() -> Int') are expected to be equal}}
let _: () -> Int? = returnsIUOFn()
let _: () -> Int = returnsIUOFn() // expected-error {{cannot convert value of type '(() -> Int?)?' to specified type '() -> Int'}}
let _: Int? = returnsIUOFn()()
let _: Int = returnsIUOFn()() // expected-error {{value of optional type 'Int?' must be unwrapped to a value of type 'Int'}}
// expected-note@-1 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
// expected-note@-2 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}

// Make sure it works for compound function references.
func testCompoundRefs() {
  func hasArgLabel(x: Int) -> Int! { x }
  struct S {
    func hasArgLabel(x: Int) -> Int! { x }
  }
  let _ = hasArgLabel(x:)(0)
  let _: Int? = hasArgLabel(x:)(0)
  let _: Int = hasArgLabel(x:)(0)

  let _ = S.hasArgLabel(x:)(S())(0)
  let _: Int? = S.hasArgLabel(x:)(S())(0)
  let _: Int = S.hasArgLabel(x:)(S())(0)

  let _ = S().hasArgLabel(x:)(0)
  let _: Int? = S().hasArgLabel(x:)(0)
  let _: Int = S().hasArgLabel(x:)(0)

  // We still don't allow IUOs for the function itself.
  let _: (Int) -> Int = hasArgLabel(x:) // expected-error {{cannot convert value of type '(Int) -> Int?' to specified type '(Int) -> Int}}
  let _: (Int) -> Int? = hasArgLabel(x:)
}
