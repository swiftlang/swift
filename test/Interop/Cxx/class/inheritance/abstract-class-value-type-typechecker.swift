// RUN: %target-typecheck-verify-swift -suppress-notes -I %S/Inputs -cxx-interoperability-mode=default

import AbstractClassValueType

func takesAbstractByValue(_ x: AbstractBase) {} // expected-error {{'AbstractBase' is unavailable: abstract C++ classes cannot be used as values in Swift}}
func returnsAbstract() -> AbstractBase {} // expected-error {{'AbstractBase' is unavailable: abstract C++ classes cannot be used as values in Swift}}

_ = AbstractBase.self // expected-error {{'AbstractBase' is unavailable: abstract C++ classes cannot be used as values in Swift}}

func takesStillAbstract(_ x: StillAbstractDerived) {} // expected-error {{'StillAbstractDerived' is unavailable: abstract C++ classes cannot be used as values in Swift}}

let a = ConcreteDerived()
let b = a
_ = b.getValue()
