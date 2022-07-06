// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import ForwardDeclaredInNamespace

public func test(c: Space.C) { c.test() }
