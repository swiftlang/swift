// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

import StdHash
import CxxStdlib

let hash = C.hash(into:)
// CHECK: error: type 'C' has no member 'hash(into:)'

let dictC: [C : String] = [:]
// CHECK: error: type 'C' does not conform to protocol 'Hashable'

let dictD: [D : String] = [:]
// CHECK: error: type 'D' does not conform to protocol 'Hashable'
