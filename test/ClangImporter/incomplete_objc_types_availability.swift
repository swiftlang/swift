// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-type-library-1.m -c -o %t/incomplete-type-library-1.o
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/complete-types.m -c -o %t/complete-types.o

// RUN: %target-build-swift -Xfrontend -enable-upcoming-feature -Xfrontend ImportObjcForwardDeclarations -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-type-library-1.o %t/complete-types.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// RUN: %target-build-swift -swift-version 6 -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-type-library-1.o %t/complete-types.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

// Verify that Clang declarations referencing either of the forward declares types "ForwardDeclaredInterface" or
// "ForwardDeclaredProtocol" are usable from Swift.

import IncompleteTypeLibrary1

let incompleteTypeConsumer = IncompleteTypeConsumer1()!

let incompleteInterface = incompleteTypeConsumer.methodReturningForwardDeclaredInterface1()!
incompleteTypeConsumer.methodTakingAForwardDeclaredInterface1(incompleteInterface)
let interfacePropertyCopy = incompleteTypeConsumer.propertyUsingAForwardDeclaredInterface1
incompleteTypeConsumer.propertyUsingAForwardDeclaredInterface1 = incompleteInterface
_ = CFunctionReturningAForwardDeclaredInterface1()
CFunctionTakingAForwardDeclaredInterface1(incompleteInterface)

let incompleteProtocol = incompleteTypeConsumer.methodReturningForwardDeclaredProtocol1()!
incompleteTypeConsumer.methodTakingAForwardDeclaredProtocol1(incompleteProtocol)
let protocolPropertyCopy = incompleteTypeConsumer.propertyUsingAForwardDeclaredProtocol1
incompleteTypeConsumer.propertyUsingAForwardDeclaredProtocol1 = incompleteProtocol
_ = CFunctionReturningAForwardDeclaredProtocol1()
CFunctionTakingAForwardDeclaredProtocol1(incompleteProtocol)
