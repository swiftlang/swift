// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import NonMemberInline

var lhs = IntBox(value: 42)
var rhs = IntBox(value: 23)

let resultPlus = lhs + rhs
let resultMinus = lhs - rhs
let resultStar = lhs * rhs
let resultSlash = lhs / rhs
