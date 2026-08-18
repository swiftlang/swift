// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -nostdinc++

// expected-warning@<unknown> * {{libstdc++ not found for }}

import NoCXXStdlib

let _ = my_sum(12, 15)
