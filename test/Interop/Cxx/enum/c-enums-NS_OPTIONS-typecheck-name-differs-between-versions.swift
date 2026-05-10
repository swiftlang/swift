// RUN: %target-swift-frontend -I %S/Inputs -enable-experimental-cxx-interop -typecheck %s
// REQUIRES: objc_interop

import CenumsNSOptions

let foo = CFunctionReturningNSOption()
CFunctionTakingNSOption(foo)
let foo2 = NSOptionTypeCheckTest.methodReturningNSOption()
NSOptionTypeCheckTest.methodTakingNSOption(foo)
