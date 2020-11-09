// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import MemberInline

var lhs = LoadableIntWrapper(value: 42)
let rhs = LoadableIntWrapper(value: 23)

let resultPlus = lhs - rhs
