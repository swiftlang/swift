// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -suppress-notes

import StdPair

let u = HasMethodThatReturnsUnsafePair()
u.getUnsafePair() // expected-error {{value of type 'HasMethodThatReturnsUnsafePair' has no member 'getUnsafePair'}}

u.getIteratorPair() // expected-error {{value of type 'HasMethodThatReturnsUnsafePair' has no member 'getIteratorPair'}}

func takeCopyable<T: Copyable>(_ _: T) {} 
func takeCopyablePair(_ _: any CxxPair) {} 
func takeNonCopyablePair(_ _: consuming any CxxPair & ~Copyable) {} 

let p1 = NonCopyableFirst(first: NonCopyable(field: 1), second: 2)
takeCopyable(p1) // expected-error {{conform to 'Copyable'}}

takeCopyablePair(p1) // expected-error {{does not conform to expected type 'Copyable'}}

takeNonCopyablePair(p1)

let p2 = NonCopyableBoth(first: NonCopyable(field: 3), second: NonCopyable(field: 4))

takeCopyable(p2) // expected-error {{conform to 'Copyable'}}

takeCopyablePair(p2) // expected-error {{does not conform to expected type 'Copyable'}}

takeNonCopyablePair(p2)

let p3 = PairInts(first: 5, second: 6)
takeCopyable(p3)
takeCopyablePair(p3)
takeNonCopyablePair(p3)
