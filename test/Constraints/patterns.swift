// RUN: %target-parse-verify-swift

// Leaf expression patterns are matched to corresponding pieces of a switch
// subject (TODO: or ~= expression) using ~= overload resolution.
switch (1, 2.5, "three") {
case (1, _, _):
  ()
// Double is IntegerLiteralConvertible
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
}

// Super-to-subclass.
var db : B = D()
switch db {
case is D:
  ()
case is E: // expected-warning {{always fails}}
  ()
}

// Raise an error if pattern productions are used in expressions.
var b = var a // expected-error{{expected initial value after '='}} expected-error {{type annotation missing in pattern}}
var c = is Int // expected-error{{expected initial value after '='}}

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
case Watch.Edition: // TODO: should warn that cast can't succeed with currently known conformances
  ()
case .HairForceOne: // expected-error{{enum case 'HairForceOne' not found in type 'HairType'}}
  ()
default:
  break
}


// <rdar://problem/19382878> Introduce new x? pattern
switch Optional(42) {
case let x?: break
case nil: break
}

// Test x???? patterns.
switch (nil as Int???) {
case let x???: print(x)
case let x??: print(x)
case let x?: print(x)
case 4???: break
case nil??: break
case nil?: break
default: break
}


// Test some value patterns.
let x : Int? = nil

extension Int {
  func method() -> Int { return 42 }
}

func ~= <T : Equatable>(lhs: T?, rhs: T?) -> Bool {
  return lhs == rhs
}

switch 4 as Int? {
case x?.method(): break // match value
}

switch 4 {
case x ?? 42: break // match value
default: break
}

for (var x) in 0...100 {}
for var x in 0...100 {}  // rdar://20167543
for (let x) in 0...100 {} // expected-error {{'let' pattern cannot appear nested in an already immutable context}}

var (let y) = 42  // expected-error {{'let' cannot appear nested inside another 'var' or 'let' pattern}}
let (var z) = 42  // expected-error {{'var' cannot appear nested inside another 'var' or 'let' pattern}}




