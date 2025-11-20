// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-noroottype-protocol-library.m -c -o %t/incomplete-noroottype-protocol-library.o

// RUN: %target-build-swift -Xfrontend -enable-upcoming-feature -Xfrontend ImportObjcForwardDeclarations -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-noroottype-protocol-library.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// RUN: %target-build-swift -swift-version 6 -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-noroottype-protocol-library.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

// Verify that a forward declared protocol not inheriting from NSObject is usable
// from Swift, if cumbersome

import IncompleteNoRootTypeProtocolLibrary

let consumer = NoRootTypeProtocolConsumer()!

let incompleteNoRootTypeProtocol = consumer.methodReturningForwardDeclaredNoRootTypeProtocol()!
consumer.methodTakingAForwardDeclaredNoRootTypeProtocol(incompleteNoRootTypeProtocol)
let interfacePropertyCopy = consumer.propertyUsingAForwardDeclaredNoRootTypeProtocol
consumer.propertyUsingAForwardDeclaredNoRootTypeProtocol = incompleteNoRootTypeProtocol
_ = CFunctionReturningAForwardDeclaredNoRootTypeProtocol()
CFunctionTakingAForwardDeclaredNoRootTypeProtocol(incompleteNoRootTypeProtocol)
