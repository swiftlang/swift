// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import SubscriptOverloads

var tri = TemplatedReturningInt()
tri[CInt(0)] = CInt(4)
let _ = tri[CInt(0)]

var tti = TemplatedTakingInt()
tti[CInt(0)] = CInt(4)
let _: CInt = tti[CInt(0)]
