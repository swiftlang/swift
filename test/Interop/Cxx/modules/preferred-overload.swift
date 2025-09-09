// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import AmbiguousA
import AmbiguousB

func g(_ x: CInt) {
    f(x)
}
