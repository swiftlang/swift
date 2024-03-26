// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

import StdPair

let u = HasMethodThatReturnsUnsafePair()
u.getUnsafePair()
// CHECK: error: value of type 'HasMethodThatReturnsUnsafePair' has no member 'getUnsafePair'
// CHECK: note: C++ method 'getUnsafePair' may return an interior pointer

u.getIteratorPair()
// CHECK: error: value of type 'HasMethodThatReturnsUnsafePair' has no member 'getIteratorPair'
// CHECK: note: C++ method 'getIteratorPair' may return an interior pointer
