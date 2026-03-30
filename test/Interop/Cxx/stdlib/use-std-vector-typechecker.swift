// RUN: %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -verify

import StdVector
import StdVectorNoCPlusPlusRequirement
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} // expected-note *{{'where T: Copyable' is implicit here}}
func takeCxxVector<V: CxxVector>(_ v: V) {} // expected-note {{where 'V' = 'VectorOfNonCopyable'}}

let vecNC = VectorOfNonCopyable()
takeCopyable(vecNC) // expected-error {{global function 'takeCopyable' requires that 'VectorOfNonCopyable'}}

takeCxxVector(vecNC) // expected-error {{global function 'takeCxxVector' requires that 'VectorOfNonCopyable'}}

let vecPointer = VectorOfPointer()
takeCopyable(vecPointer)
takeCxxVector(vecPointer)

// Make sure that a specialization of std::vector that is declared in a Clang
// module which does not declare 'requires cplusplus' is still conformed to
// CxxVector.
let vecFloat = VectorOfFloat()
takeCopyable(vecFloat)
takeCxxVector(vecFloat)

let hasVector = HasVector()
takeCopyable(hasVector) // expected-error {{global function 'takeCopyable' requires that 'HasVector' conform to 'Copyable'}}

let baseHasVector = BaseHasVector()
takeCopyable(baseHasVector) // expected-error {{global function 'takeCopyable' requires that 'BaseHasVector' conform to 'Copyable'}}
