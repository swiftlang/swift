// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-type-library-1.m -c -o %t/incomplete-type-library-1.o
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/complete-types.m -c -o %t/complete-types.o

// RUN: %target-build-swift -Xfrontend -enable-upcoming-feature -Xfrontend ImportObjcForwardDeclarations -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-type-library-1.o %t/complete-types.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

import IncompleteTypeLibrary1
import CompleteTypes

class Dummy {
    init() {}
}

let dummy = Dummy()

assert(!(dummy is ForwardDeclaredInterface))
assert(!(dummy is ForwardDeclaredProtocol))

func interfaceTestFunc(param: ForwardDeclaredInterface) {}
func protocolTestFunc(param: ForwardDeclaredProtocol) {}

class InterfaceTestClass : ForwardDeclaredInterface {}

class ProtocolTestClass : ForwardDeclaredProtocol {
    init() {}
    func doSomethingForwardDeclaredProtocolsCan() {

    }
}

let interfaceTestClassInstance = InterfaceTestClass()!
let interfaceTestInstance = ForwardDeclaredInterface()!
let protocolTestClassInstance = ProtocolTestClass()

interfaceTestClassInstance.doSomethingForwardDeclaredInterfacesCan()
interfaceTestInstance.doSomethingForwardDeclaredInterfacesCan()
protocolTestClassInstance.doSomethingForwardDeclaredProtocolsCan()
