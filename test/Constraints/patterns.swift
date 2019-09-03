// RUN: %target-typecheck-verify-swift

// Leaf expression patterns are matched to corresponding pieces of a switch
// subject (TODO: or ~= expression) using ~= overload resolution.
switch (1, 2.5, "three") {
case (1, _, _):
  ()
// Double is ExpressibleByIntegerLiteral
case (_, 2, _),
     (_, 2.5, _),
     (_, _, "three"):
  ()
// ~= overloaded for (Range<Int>, Int)
case (0..<10, _, _),
     (0..<10, 2.5, "three"),
     (0...9, _, _),
     (0...9, 2.5, "three"):
  ()
default:
  ()
}

switch (1, 2) {
case (var a, a): // expected-error {{use of unresolved identifier 'a'}}
  ()
}

// 'is' patterns can perform the same checks that 'is' expressions do.

protocol P { func p() }

class B : P { 
  init() {} 
  func p() {}
  func b() {}
}
class D : B {
  override init() { super.init() } 
  func d() {}
}
class E {
  init() {} 
  func e() {}
}

struct S : P {
  func p() {}
  func s() {}
}

// Existential-to-concrete.
var bp : P = B()
switch bp {
case is B,
     is D,
     is S:
  ()
case is E:
  ()
default:
  ()
}

switch bp {
case let b as B:
  b.b()
case let d as D:
  d.b()
  d.d()
case let s as S:
  s.s()
case let e as E:
  e.e()
default:
  ()
}

// Super-to-subclass.
var db : B = D()
switch db {
case is D:
  ()
case is E: // expected-warning {{always fails}}
  ()
default:
  ()
}

// Raise an error if pattern productions are used in expressions.
var b = var a // expected-error{{expected initial value after '='}} expected-error {{type annotation missing in pattern}} expected-error {{consecutive statements on a line must be separated by ';'}} {{8-8=;}}
var c = is Int // expected-error{{expected initial value after '='}} expected-error {{expected expression}}  expected-error {{consecutive statements on a line must be separated by ';'}} {{8-8=;}}

// TODO: Bad recovery in these cases. Although patterns are never valid
// expr-unary productions, it would be good to parse them anyway for recovery.
//var e = 2 + var y
//var e = var y + 2

// 'E.Case' can be used in a dynamic type context as an equivalent to
// '.Case as E'.
protocol HairType {}
enum MacbookHair: HairType {
  case HairSupply(S)
}

enum iPadHair<T>: HairType {
  case HairForceOne
}

enum Watch {
  case Sport, Watch, Edition
}

let hair: HairType = MacbookHair.HairSupply(S())
switch hair {
case MacbookHair.HairSupply(let s):
  s.s()
case iPadHair<S>.HairForceOne:
  ()
case iPadHair<E>.HairForceOne:
  ()
case iPadHair.HairForceOne: // expected-error{{generic enum type 'iPadHair' is ambiguous without explicit generic parameters when matching value of type 'HairType'}}
  ()
case Watch.Edition: // expected-warning {{cast from 'HairType' to unrelated type 'Watch' always fails}}
  ()
case .HairForceOne: // expected-error{{type 'HairType' has no member 'HairForceOne'}}
  ()
default:
  break
}


// <rdar://problem/19382878> Introduce new x? pattern
switch Optional(42) {
case let x?: break // expected-warning{{immutable value 'x' was never used; consider replacing with '_' or removing it}}
case nil: break
}

func SR2066(x: Int?) {
    // nil literals should still work when wrapped in parentheses
    switch x {
    case (nil): break
    case _?: break
    }
    switch x {
    case ((nil)): break
    case _?: break
    }
    switch (x, x) {
    case ((nil), _): break
    case (_?, _): break
    }
}

// Test x???? patterns.
switch (nil as Int???) {
case let x???: print(x, terminator: "")
case let x??: print(x as Any, terminator: "")
case let x?: print(x as Any, terminator: "")
case 4???: break
case nil??: break // expected-warning {{case is already handled by previous patterns; consider removing it}}
case nil?: break // expected-warning {{case is already handled by previous patterns; consider removing it}}
default: break
}

switch ("foo" as String?) {
case "what": break
default: break
}


// Test some value patterns.
let x : Int?

extension Int {
  func method() -> Int { return 42 }
}

func ~= <T : Equatable>(lhs: T?, rhs: T?) -> Bool {
  return lhs == rhs
}

switch 4 as Int? {
case x?.method(): break // match value
default: break
}

switch 4 {
case x ?? 42: break // match value
default: break
}

for (var x) in 0...100 {} // expected-warning{{variable 'x' was never used; consider replacing with '_' or removing it}}
for var x in 0...100 {}  // rdar://20167543 expected-warning{{variable 'x' was never used; consider replacing with '_' or removing it}}
for (let x) in 0...100 { _ = x} // expected-error {{'let' pattern cannot appear nested in an already immutable context}}

var (let y) = 42  // expected-error {{'let' cannot appear nested inside another 'var' or 'let' pattern}}
let (var z) = 42  // expected-error {{'var' cannot appear nested inside another 'var' or 'let' pattern}}


// Crash when re-typechecking EnumElementPattern.
// FIXME: This should actually type-check -- the diagnostics are bogus. But
// at least we don't crash anymore.

protocol PP {
  associatedtype E
}

struct A<T> : PP {
  typealias E = T
}

extension PP {
  func map<T>(_ f: (Self.E) -> T) -> T {}
}

enum EE {
  case A
  case B
}

func good(_ a: A<EE>) -> Int {
  return a.map {
    switch $0 {
    case .A:
      return 1
    default:
      return 2
    }
  }
}

func bad(_ a: A<EE>) {
  a.map { // expected-error {{unable to infer complex closure return type; add explicit type to disambiguate}} {{10-10= () -> Int in }}
    let _: EE = $0
    return 1
  }
}

func ugly(_ a: A<EE>) {
  a.map { // expected-error {{unable to infer complex closure return type; add explicit type to disambiguate}} {{10-10= () -> Int in }}
    switch $0 {
    case .A:
      return 1
    default:
      return 2
    }
  }
}

// SR-2057

enum SR2057 {
  case foo
}

let sr2057: SR2057?
if case .foo = sr2057 { } // Ok


// Invalid 'is' pattern
class SomeClass {}
if case let doesNotExist as SomeClass:AlsoDoesNotExist {}
// expected-error@-1 {{use of undeclared type 'AlsoDoesNotExist'}}
// expected-error@-2 {{variable binding in a condition requires an initializer}}

// `.foo` and `.bar(...)` pattern syntax should also be able to match
// static members as expr patterns

struct StaticMembers: Equatable {
  init() {}
  init(_: Int) {}
  init?(opt: Int) {}
  static var prop = StaticMembers()
  static var optProp: Optional = StaticMembers()

  static func method(_: Int) -> StaticMembers { return prop }
  static func method(withLabel: Int) -> StaticMembers { return prop }
  static func optMethod(_: Int) -> StaticMembers? { return optProp }

  static func ==(x: StaticMembers, y: StaticMembers) -> Bool { return true }
}

let staticMembers = StaticMembers()
let optStaticMembers: Optional = StaticMembers()

switch staticMembers {
  case .init: break // expected-error{{cannot match values of type 'StaticMembers'}}
  case .init(opt:): break // expected-error{{cannot match values of type 'StaticMembers'}}
  case .init(): break

  case .init(0): break
  case .init(_): break // expected-error{{'_' can only appear in a pattern}}
  case .init(let x): break // expected-error{{cannot appear in an expression}}
  case .init(opt: 0): break // expected-error{{value of optional type 'StaticMembers?' must be unwrapped to a value of type 'StaticMembers'}}
  // expected-note@-1 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
  // expected-note@-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}

  case .prop: break
  // TODO: repeated error message
  case .optProp: break // expected-error* {{not unwrapped}}

  case .method: break // expected-error{{cannot match}}
  case .method(0): break
  case .method(_): break // expected-error{{'_' can only appear in a pattern}}
  case .method(let x): break // expected-error{{cannot appear in an expression}}

  case .method(withLabel:): break // expected-error{{cannot match}}
  case .method(withLabel: 0): break
  case .method(withLabel: _): break // expected-error{{'_' can only appear in a pattern}}
  case .method(withLabel: let x): break // expected-error{{cannot appear in an expression}}

  case .optMethod: break // expected-error{{cannot match}}
  case .optMethod(0): break
  // expected-error@-1 {{value of optional type 'StaticMembers?' must be unwrapped to a value of type 'StaticMembers'}}
  // expected-note@-2 {{coalesce}}
  // expected-note@-3 {{force-unwrap}}
}

_ = 0

// rdar://problem/32241441 - Add fix-it for cases in switch with optional chaining

struct S_32241441 {
  enum E_32241441 {
    case foo
    case bar
  }

  var type: E_32241441 = E_32241441.foo
}

func rdar32241441() {
  let s: S_32241441? = S_32241441()

  switch s?.type { // expected-error {{switch must be exhaustive}} expected-note {{add missing case: '.none'}}
  case .foo: // Ok
    break;
  case .bar: // Ok
    break;
  }
}


// SR-6100
struct One<Two> {
    public enum E: Error {
        // if you remove associated value, everything works
        case SomeError(String)
    }
}

func testOne() {
  do {
  } catch let error { // expected-warning{{'catch' block is unreachable because no errors are thrown in 'do' block}}
    if case One.E.SomeError = error {} // expected-error{{generic enum type 'One.E' is ambiguous without explicit generic parameters when matching value of type 'Error'}}
  }
}

// SR-8347
// constrain initializer expressions of optional some pattern bindings to be optional
func test8347() -> String {
  struct C {
    subscript (s: String) -> String? {
      return ""
    }
    subscript (s: String) -> [String] {
      return [""]
    }

    func f() -> String? {
      return ""
    }
    func f() -> Int {
      return 3
    }

    func g() -> String {
      return ""
    }

    func h() -> String { // expected-note {{found this candidate}}
      return ""
    }
    func h() -> Double { // expected-note {{found this candidate}}
      return 3.0
    }
    func h() -> Int? { //expected-note{{found this candidate}}
      return 2
    }
    func h() -> Float? { //expected-note{{found this candidate}}
      return nil
    }

  }

  let c = C()
  if let s = c[""] {
    return s
  }
  if let s = c.f() {
    return s
  }
  if let s = c.g() { //expected-error{{initializer for conditional binding must have Optional type, not 'String'}}
    return s
  }
  if let s = c.h() { //expected-error{{ambiguous use of 'h()'}}
    return s
  }
}

enum SR_7799 {
 case baz
 case bar
}

let sr7799: SR_7799? = .bar

switch sr7799 {
 case .bar?: break // Ok
 case .baz: break // Ok
 default: break
}

let sr7799_1: SR_7799?? = .baz

switch sr7799_1 {
 case .bar?: break // Ok
 case .baz: break // Ok
 default: break
}

if case .baz = sr7799_1 {} // Ok
if case .bar? = sr7799_1 {} // Ok
