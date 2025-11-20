// This test checks that swift-ide-test output under either -swift-version 6 or 
// -enable-upcoming-feature ImportObjCForwardDeclarations the output matches the
// import capabilities of the compiler.

// (1) Print the interface with -enable-upcoming-feature ImportObjCForwardDeclarations
// RUN: %target-swift-ide-test -print-module -module-to-print IncompleteTypeLibrary1 -I %S/Inputs/custom-modules/IncompleteTypes \
// RUN:     -enable-objc-interop -enable-upcoming-feature ImportObjcForwardDeclarations -source-filename x | %FileCheck %s

// (2) Print the interface with -swift-version 6
// RUN: %target-swift-ide-test -print-module -module-to-print IncompleteTypeLibrary1 -I %S/Inputs/custom-modules/IncompleteTypes \
// RUN:     -enable-objc-interop -swift-version 6 -source-filename x | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

// CHECK: import Foundation
// CHECK: @available(*, unavailable, message: "This Objective-C class has only been forward-declared; import its owning module to use it")
// CHECK: class ForwardDeclaredInterface {
// CHECK: }
// CHECK: @available(*, unavailable, message: "This Objective-C protocol has only been forward-declared; import its owning module to use it")
// CHECK: protocol ForwardDeclaredProtocol {
// CHECK: }
// CHECK: class IncompleteTypeConsumer1 : NSObject {
// CHECK:   var propertyUsingAForwardDeclaredProtocol1: (any ForwardDeclaredProtocol)!
// CHECK:   var propertyUsingAForwardDeclaredInterface1: ForwardDeclaredInterface!
// CHECK:   init!()
// CHECK:   func methodReturningForwardDeclaredProtocol1() -> (any ForwardDeclaredProtocol & NSObjectProtocol)!
// CHECK:   func methodReturningForwardDeclaredInterface1() -> ForwardDeclaredInterface!
// CHECK:   func methodTakingAForwardDeclaredProtocol1(_ param: (any ForwardDeclaredProtocol)!)
// CHECK:   func methodTakingAForwardDeclaredInterface1(_ param: ForwardDeclaredInterface!)
// CHECK: }
// CHECK: func CFunctionReturningAForwardDeclaredInterface1() -> ForwardDeclaredInterface!
// CHECK: func CFunctionTakingAForwardDeclaredInterface1(_ param: ForwardDeclaredInterface!)
// CHECK: func CFunctionReturningAForwardDeclaredProtocol1() -> (any ForwardDeclaredProtocol & NSObjectProtocol)!
// CHECK: func CFunctionTakingAForwardDeclaredProtocol1(_ param: (any ForwardDeclaredProtocol)!)
