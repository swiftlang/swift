// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import StaticFactoryAsInitGeneric

let generic = GenericFactory(fromGeneric: 1 as CInt)
let _: CInt = generic.value

let multi = MultiGenericFactory(2 as CInt, other: 3 as CInt)
let _: CInt = multi.value

let mixed = MixedGenericFactory(generic: 4 as CInt, concrete: 5)
let _: CInt = mixed.value
