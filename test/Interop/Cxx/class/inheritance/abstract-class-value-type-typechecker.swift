// RUN: %target-typecheck-verify-swift -suppress-notes -I %S/Inputs -cxx-interoperability-mode=default

import AbstractClassValueType

func takesAbstractByValue(_ x: AbstractBase) {} // expected-warning {{'AbstractBase' is deprecated: abstract C++ classes cannot be used as values in Swift}}
func returnsAbstract() -> AbstractBase {} // expected-warning {{'AbstractBase' is deprecated: abstract C++ classes cannot be used as values in Swift}}

_ = AbstractBase.self // expected-warning {{'AbstractBase' is deprecated: abstract C++ classes cannot be used as values in Swift}}

func takesStillAbstract(_ x: StillAbstractDerived) {} // expected-warning {{'StillAbstractDerived' is deprecated: abstract C++ classes cannot be used as values in Swift}}

let a = ConcreteDerived()
let b = a
_ = b.getValue()
