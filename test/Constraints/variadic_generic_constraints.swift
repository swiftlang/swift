// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

// Test instantiation of constraint solver constraints from generic requirements
// involving type pack parameters

// Conformance requirements

protocol P {}

func takesP<T...: P>(_: T...) {}  // expected-note {{where 'T' = 'DoesNotConformToP'}}

struct ConformsToP: P {}
struct DoesNotConformToP {}

takesP()  // ok
takesP(ConformsToP(), ConformsToP(), ConformsToP())  // ok

takesP(ConformsToP(), DoesNotConformToP(), ConformsToP()) // expected-error {{global function 'takesP' requires that 'DoesNotConformToP' conform to 'P'}}

// Superclass requirements

class C {}

class SubclassOfC: C {}
class NotSubclassOfC {}

func takesC<T...: C>(_: T...) {}  // expected-note {{where 'T' = 'NotSubclassOfC'}}

takesC()  // ok
takesC(SubclassOfC(), SubclassOfC(), SubclassOfC())  // ok

takesC(SubclassOfC(), NotSubclassOfC(), SubclassOfC())  // expected-error {{global function 'takesC' requires that 'NotSubclassOfC' inherit from 'C'}}

// Layout requirements

struct S {}

func takesAnyObject<T...: AnyObject>(_: T...) {}

takesAnyObject()
takesAnyObject(C(), C(), C())

// FIXME: Bad diagnostic
takesAnyObject(C(), S(), C())  // expected-error {{type of expression is ambiguous without more context}}

// Same-type requirements

func takesParallelSequences<T..., U...>(t: T..., u: U...) where T: Sequence, U: Sequence, T.Element == U.Element {}
// expected-note@-1 {{where 'T.Element' = 'String', 'U.Element' = 'Int'}}

takesParallelSequences()  // ok
takesParallelSequences(t: Array<Int>(), u: Set<Int>())  // ok
takesParallelSequences(t: Array<String>(), Set<Int>(), u: Set<String>(), Array<Int>())  // ok
takesParallelSequences(t: Array<String>(), Set<Int>(), u: Array<Int>(), Set<String>())  // expected-error {{global function 'takesParallelSequences(t:u:)' requires the types 'String' and 'Int' be equivalent}}

// Same-shape requirements

func zip<T..., U...>(t: T..., u: U...) -> ((T, U)...) {}
// expected-note@-1 {{where 'T' = 'T', 'U' = 'U'}}

let _ = zip()  // ok
let _ = zip(t: 1, u: "hi")  // ok
let _ = zip(t: 1, 2, u: "hi", "hello")  // ok
let _ = zip(t: 1, 2, 3, u: "hi", "hello", "greetings")  // ok

// FIXME: Bad diagnostic
let _ = zip(t: 1, u: "hi", "hello", "greetings")  // expected-error {{type of expression is ambiguous without more context}}

func goodCallToZip<T..., U...>(t: T..., u: U...) where ((T, U)...): Any {
  _ = zip(t: (each t)..., u: (each u)...)
}

func badCallToZip<T..., U...>(t: T..., u: U...) {
  _ = zip(t: (each t)..., u: (each u)...)
  // expected-error@-1 {{global function 'zip(t:u:)' requires the type packs 'T' and 'U' have the same shape}}
}
