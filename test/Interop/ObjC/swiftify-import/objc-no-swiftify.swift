// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=NoSwiftifyClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/NoSwiftify.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s -verify -verify-additional-file %S/Inputs/objc-no-swiftify.h

import NoSwiftifyClang

// CHECK-NOT: @_alwaysEmitIntoClient @_disfavoredOverload public func callAutoreleaseParam
// CHECK-NOT: @_alwaysEmitIntoClient @_disfavoredOverload public func callAutoreleaseReturn

public func callAutoreleaseParam(_ p: UnsafeMutableBufferPointer<SomeClass>) {
    // expected-error@+2{{missing argument for parameter #2 in call}}
    // expected-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<SomeClass>' to expected argument type 'AutoreleasingUnsafeMutablePointer<SomeClass?>'}}
    autoreleaseParam(p)
}

public func callAutoreleaseReturn() {
    // expected-error@+1{{cannot convert value of type 'AutoreleasingUnsafeMutablePointer<SomeClass?>?' to specified type 'UnsafeMutableBufferPointer<SomeClass>'}}
    let p: UnsafeMutableBufferPointer<SomeClass> = autoreleaseReturn(10)
}