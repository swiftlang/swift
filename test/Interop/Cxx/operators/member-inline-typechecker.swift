// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import MemberInline

var lhs = LoadableIntWrapper(value: 42)
let rhs = LoadableIntWrapper(value: 23)

let resultPlus = lhs - rhs
let resultCall0 = lhs()
let resultCall1 = lhs(1)
let resultCall2 = lhs(1, 2)

var addressOnly = AddressOnlyIntWrapper(42)

let addressOnlyResultCall0 = addressOnly()
let addressOnlyResultCall1 = addressOnly(1)
let addressOnlyResultCall2 = addressOnly(1, 2)
