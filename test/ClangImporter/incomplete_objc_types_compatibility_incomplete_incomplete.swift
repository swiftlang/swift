// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-type-library-1.m -c -o %t/incomplete-type-library-1.o
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-type-library-2.m -c -o %t/incomplete-type-library-2.o
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/complete-types.m -c -o %t/complete-types.o

// RUN: %target-build-swift -Xfrontend -enable-upcoming-feature -Xfrontend ImportObjcForwardDeclarations -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-type-library-1.o %t/incomplete-type-library-2.o %t/complete-types.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// RUN: %target-build-swift -swift-version 6 -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-type-library-1.o %t/incomplete-type-library-2.o %t/complete-types.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

// Both libraries "IncompleteTypeConsumer1" and "IncompleteTypeConsumer2" forward declare an interface "ForwardDeclaredInterface"
// and a protocol "ForwardDeclaredProtocol". This test verifies that the synthesized Swift declaration created to represent
// these types are interchangeable (ie. You can pass an IncompleteTypeConsumer1.ForwardDeclaredProtocol instance to a
// function expecting an IncompleteTypeConsumer2.ForwardDeclaredInterface and vice versa)

import IncompleteTypeLibrary1
import IncompleteTypeLibrary2

let incompleteTypeConsumer1 = IncompleteTypeConsumer1()!
let incompleteTypeConsumer2 = IncompleteTypeConsumer2()!

let incompleteInterface1 = incompleteTypeConsumer1.methodReturningForwardDeclaredInterface1()!
let incompleteInterface2 = incompleteTypeConsumer2.methodReturningForwardDeclaredInterface2()!

incompleteTypeConsumer1.methodTakingAForwardDeclaredInterface1(incompleteInterface2)
incompleteTypeConsumer2.methodTakingAForwardDeclaredInterface2(incompleteInterface1)

let interfacePropertyCopy1 = incompleteTypeConsumer1.propertyUsingAForwardDeclaredInterface1
let interfacePropertyCopy2 = incompleteTypeConsumer2.propertyUsingAForwardDeclaredInterface2
incompleteTypeConsumer1.propertyUsingAForwardDeclaredInterface1 = incompleteInterface2
incompleteTypeConsumer2.propertyUsingAForwardDeclaredInterface2 = incompleteInterface1

CFunctionTakingAForwardDeclaredInterface1(incompleteInterface2)
CFunctionTakingAForwardDeclaredInterface2(incompleteInterface1)

let incompleteProtocol1 = incompleteTypeConsumer1.methodReturningForwardDeclaredProtocol1()!
let incompleteProtocol2 = incompleteTypeConsumer2.methodReturningForwardDeclaredProtocol2()!

incompleteTypeConsumer1.methodTakingAForwardDeclaredProtocol1(incompleteProtocol2)
incompleteTypeConsumer2.methodTakingAForwardDeclaredProtocol2(incompleteProtocol1)

let protocolPropertyCopy1 = incompleteTypeConsumer1.propertyUsingAForwardDeclaredProtocol1
let protocolPropertyCopy2 = incompleteTypeConsumer2.propertyUsingAForwardDeclaredProtocol2
incompleteTypeConsumer1.propertyUsingAForwardDeclaredProtocol1 = incompleteProtocol2
incompleteTypeConsumer2.propertyUsingAForwardDeclaredProtocol2 = incompleteProtocol1

CFunctionTakingAForwardDeclaredProtocol1(incompleteProtocol2)
CFunctionTakingAForwardDeclaredProtocol2(incompleteProtocol1)
