// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20

import StdNumeric

let _ = getGCD(12, 15)
