// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop
// expect-no-diagnostics

import MutableMembers

let obj = HasPublicMutableMember(a: 42)
let i = obj.foo()
