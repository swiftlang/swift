// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20

import StdRanges

let x = dereferenceV
