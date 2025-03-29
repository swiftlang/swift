// Variant of Interpreter/objc_implementation_objc_client.m that tests resilient stored properties.
// Will not execute correctly without ObjC runtime support.
// REQUIRES: rdar109171643

// REQUIRES-X: rdar101497120

//
// Build objc_implementation.framework
//
// RUN: %empty-directory(%t-frameworks)
// RUN: %empty-directory(%t-frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule)
// RUN: %empty-directory(%t-frameworks/objc_implementation.framework/Headers)
// RUN: cp %S/Inputs/objc_implementation.modulemap %t-frameworks/objc_implementation.framework/Modules/module.modulemap
// RUN: cp %S/Inputs/objc_implementation.h %t-frameworks/objc_implementation.framework/Headers
// RUN: %target-build-swift-dylib(%t-frameworks/objc_implementation.framework/objc_implementation) -emit-module-path %t-frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name objc_implementation -F %t-frameworks -import-underlying-module -Xlinker -install_name -Xlinker %t-frameworks/objc_implementation.framework/objc_implementation %S/objc_implementation.swift -D RESILIENCE -enable-experimental-feature CImplementation -enable-experimental-feature ObjCImplementationWithResilientStorage -target %target-future-triple
//
// Execute this file
//
// RUN: %empty-directory(%t)
// RUN: %target-clang %S/objc_implementation_objc_client.m -isysroot %sdk -F %t-frameworks -lobjc -fmodules -fobjc-arc -o %t/objc_implementation_objc_client -D RESILIENCE
// RUN: %target-codesign %t/objc_implementation_objc_client
// RUN: %target-run %t/objc_implementation_objc_client 2>&1 | %FileCheck %S/objc_implementation_objc_client.m --check-prefixes CHECK,CHECK-RESILIENCE

// REQUIRES: executable_test
// REQUIRES: objc_interop

// FIXME: This test fails in Swift CI simulators, but I have not been able to
//        reproduce this locally.
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_CImplementation
// REQUIRES: swift_feature_ObjCImplementationWithResilientStorage
