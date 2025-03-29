// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-type-library-1.m -c -o %t/incomplete-type-library-1.o
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-type-library-2.m -c -o %t/incomplete-type-library-2.o
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/complete-types.m -c -o %t/complete-types.o

// RUN: %target-build-swift -Xfrontend -enable-upcoming-feature -Xfrontend ImportObjcForwardDeclarations -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-type-library-1.o %t/incomplete-type-library-2.o  %t/complete-types.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// RUN: %target-build-swift -swift-version 6 -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-type-library-1.o %t/incomplete-type-library-2.o  %t/complete-types.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

// "ForwardDeclaredInterface" and "ForwardDeclaredProtocol" are forward declared in IncompleteTypeLibrary1/2, and
// completely declared in CompleteTypes. This test verifies that instances of the complete types can be passed
// to the consuming libraries.

import CompleteTypes
import IncompleteTypeLibrary1
import IncompleteTypeLibrary2

let incompleteTypeConsumer1 = IncompleteTypeConsumer1()!
let incompleteTypeConsumer2 = IncompleteTypeConsumer2()!

let completeInterface = ForwardDeclaredInterface()
let incompleteInterface = CFunctionReturningAForwardDeclaredInterface1()

incompleteTypeConsumer1.methodTakingAForwardDeclaredInterface1(completeInterface)
let interfacePropertyCopy = incompleteTypeConsumer1.propertyUsingAForwardDeclaredInterface1
incompleteTypeConsumer1.propertyUsingAForwardDeclaredInterface1 = completeInterface
CFunctionTakingAForwardDeclaredInterface1(completeInterface)

incompleteTypeConsumer2.methodTakingAForwardDeclaredInterface2(completeInterface)
incompleteTypeConsumer2.propertyUsingAForwardDeclaredInterface2 = completeInterface
CFunctionTakingAForwardDeclaredInterface2(completeInterface)

class SwiftTypeConformingToForwardDeclaredProtocol : ForwardDeclaredProtocol {
    init() {}
    func doSomethingForwardDeclaredProtocolsCan() {
        print("Doing something forward declared protocols can!");
    }
}

let completeProtocol = SwiftTypeConformingToForwardDeclaredProtocol()
let incompleteProtocol = CFunctionReturningAForwardDeclaredProtocol1()

incompleteTypeConsumer1.methodTakingAForwardDeclaredProtocol1(completeProtocol)
let protocolPropertyCopy = incompleteTypeConsumer1.propertyUsingAForwardDeclaredProtocol1
incompleteTypeConsumer1.propertyUsingAForwardDeclaredProtocol1 = completeProtocol
_ = CFunctionReturningAForwardDeclaredProtocol1()
CFunctionTakingAForwardDeclaredProtocol1(completeProtocol)

incompleteTypeConsumer2.methodTakingAForwardDeclaredProtocol2(completeProtocol)
incompleteTypeConsumer2.propertyUsingAForwardDeclaredProtocol2 = completeProtocol
_ = CFunctionReturningAForwardDeclaredProtocol2()
CFunctionTakingAForwardDeclaredProtocol2(completeProtocol)

takeACompleteInterface(incompleteInterface)
takeACompleteInterface(interfacePropertyCopy)
takeACompleteProtocol(incompleteProtocol)
takeACompleteProtocol(protocolPropertyCopy)
