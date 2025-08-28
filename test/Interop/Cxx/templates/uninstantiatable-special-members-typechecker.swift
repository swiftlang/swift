// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -I %S/Inputs

import UninstantiatableSpecialMembers

let nonCopyableValue = NonCopyableInst(123)
let copy1 = copy nonCopyableValue // expected-error {{'copy' cannot be applied to noncopyable types}}

let nonCopyableDerived = DerivedNonCopyableInst(567)
let copy2 = copy nonCopyableDerived // expected-error {{'copy' cannot be applied to noncopyable types}}
