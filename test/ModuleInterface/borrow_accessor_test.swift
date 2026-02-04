// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature BorrowAndMutateAccessors \
// RUN:     -o %t/borrow_accessors.swiftmodule \
// RUN:     -emit-module-interface-path %t/borrow_accessors.swiftinterface \
// RUN:     %S/Inputs/borrow_accessors.swift

// Check the interfaces

// RUN: %FileCheck %s < %t/borrow_accessors.swiftinterface

// See if we can compile a module through just the interface and typecheck using it.

// RUN: %target-swift-frontend -compile-module-from-interface \
// RUN:    -enable-experimental-feature BorrowAndMutateAccessors \
// RUN:    %t/borrow_accessors.swiftinterface -o %t/borrow_accessors.swiftmodule

// RUN: %target-swift-frontend -typecheck -I %t %s \
// RUN:    -enable-experimental-feature BorrowAndMutateAccessors

// REQUIRES: swift_feature_BorrowAndMutateAccessors

import borrow_accessors

// CHECK: public protocol P {
// CHECK:   #if compiler(>=5.3) && $BorrowAndMutateAccessors
// CHECK:   var k: borrow_accessors.Klass { borrow mutate }
// CHECK:   #endif
// CHECK: }
// CHECK: public struct Wrapper : borrow_accessors.P {
// CHECK:   #if compiler(>=5.3) && $BorrowAndMutateAccessors
// CHECK:   public var k: borrow_accessors.Klass {
// CHECK:     borrow
// CHECK:     mutate
// CHECK:   }
// CHECK:   #endif
// CHECK: }

