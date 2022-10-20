// RUN: %target-typecheck-verify-swift -enable-experimental-variadic-generics

// Test instantiation of constraint solver constraints from generic requirements
// involving type pack parameters

protocol P {}

func takesP<T...: P>(_: T...) {}  // expected-note {{where 'T' = 'DoesNotConformToP'}}

struct ConformsToP: P {}
struct DoesNotConformToP {}

takesP()  // ok
takesP(ConformsToP(), ConformsToP(), ConformsToP())  // ok

// FIXME: Bad diagnostic
takesP(ConformsToP(), DoesNotConformToP(), ConformsToP()) // expected-error {{global function 'takesP' requires that 'DoesNotConformToP' conform to 'P'}}

class C {}

class SubclassOfC: C {}
class NotSubclassOfC {}

func takesC<T...: C>(_: T...) {}  // expected-note {{where 'T' = 'NotSubclassOfC'}}

takesC()  // ok
takesC(SubclassOfC(), SubclassOfC(), SubclassOfC())  // ok

takesC(SubclassOfC(), NotSubclassOfC(), SubclassOfC())  // expected-error {{global function 'takesC' requires that 'NotSubclassOfC' inherit from 'C'}}