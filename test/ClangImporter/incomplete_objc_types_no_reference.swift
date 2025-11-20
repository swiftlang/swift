// RUN: not %target-swift-frontend -enable-upcoming-feature ImportObjcForwardDeclarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -swift-version 6 -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes %s 2>&1 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

import IncompleteTypeLibrary1

class Dummy {
    init() {}
}

let dummy = Dummy()

// CHECK: error: 'ForwardDeclaredInterface' is unavailable: This Objective-C class has only been forward-declared; import its owning module to use it
assert(!(dummy is ForwardDeclaredInterface))

// CHECK: error: 'ForwardDeclaredProtocol' is unavailable: This Objective-C protocol has only been forward-declared; import its owning module to use it
assert(!(dummy is ForwardDeclaredProtocol))

// CHECK: error: 'ForwardDeclaredInterface' is unavailable: This Objective-C class has only been forward-declared; import its owning module to use it
func interfaceTestFunc(param: ForwardDeclaredInterface) {}
// CHECK: error: 'ForwardDeclaredProtocol' is unavailable: This Objective-C protocol has only been forward-declared; import its owning module to use it
func protocolTestFunc(param: ForwardDeclaredProtocol) {}

// CHECK: error: 'ForwardDeclaredInterface' is unavailable: This Objective-C class has only been forward-declared; import its owning module to use it
class interfaceTestClass : ForwardDeclaredInterface {}
// CHECK: error: 'ForwardDeclaredProtocol' is unavailable: This Objective-C protocol has only been forward-declared; import its owning module to use it
class protocolTestClass : ForwardDeclaredProtocol {}

// CHECK: error: 'ForwardDeclaredInterface' cannot be constructed because it has no accessible initializers
let interfaceTestInstance = ForwardDeclaredInterface()
