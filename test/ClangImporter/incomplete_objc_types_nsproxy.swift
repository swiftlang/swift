// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-nsproxy-library.m -c -o %t/incomplete-nsproxy-library.o

// RUN: %target-build-swift -Xfrontend -enable-upcoming-feature -Xfrontend ImportObjcForwardDeclarations -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-nsproxy-library.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// RUN: %target-build-swift -swift-version 6 -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %s %t/incomplete-nsproxy-library.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

// Verify that a forward declared interface for a type inheriting from NSProxy instead
// of NSObject is still usable from Swift.

import IncompleteNSProxyLibrary
import Foundation

let consumer = NSProxyConsumer()!

let incompleteNSProxyInterface = consumer.methodReturningForwardDeclaredNSProxyInterface()!
consumer.methodTakingAForwardDeclaredNSProxyInterface(incompleteNSProxyInterface)
let interfacePropertyCopy = consumer.propertyUsingAForwardDeclaredNSProxyInterface
consumer.propertyUsingAForwardDeclaredNSProxyInterface = incompleteNSProxyInterface
_ = CFunctionReturningAForwardDeclaredNSProxyInterface()
CFunctionTakingAForwardDeclaredNSProxyInterface(incompleteNSProxyInterface)

_ = incompleteNSProxyInterface.perform(#selector(NSObject.description))
_ = incompleteNSProxyInterface.perform(#selector(NSObject.debugDescription))
_ = incompleteNSProxyInterface.perform(#selector(NSObject.hash))
