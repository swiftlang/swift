/// Check that we correctly refer to @_spiOnly imported modules in the private
/// swiftinterface and not to one of the redeclarations.
// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Client.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -experimental-spi-only-imports \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t

/// We print a reference to A in the private swiftinterface.
// RUN: %FileCheck %s < %t/Client.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -I %t \
// RUN:   -module-name Client

//--- module.modulemap
module A {
    header "A.h"
}

module B {
    header "B.h"
}

//--- A.h
@interface MovingType
@end

//--- B.h
@class MovingType;

//--- Client.swift
import B
@_spiOnly import A

@_spi(_)
public func foo(_ a: MovingType) {}
// CHECK: A.MovingType
