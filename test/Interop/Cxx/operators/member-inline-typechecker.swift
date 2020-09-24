// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import MemberInline

var lhs = IntBox(value: 42)
let rhs = IntBox(value: 23)

let resultPlus = lhs - rhs
