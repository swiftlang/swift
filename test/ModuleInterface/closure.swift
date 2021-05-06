// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-module-interface-path %t/main.swiftinterface -enable-library-evolution
// RUN: %FileCheck %s < %t/main.swiftinterface

// CHECK: import Swift

// CHECK: public let MyClosureVar: (Swift.Int) -> Swift.Int
public let MyClosureVar: (Int) -> Int = { $0 }

// CHECK: public var MyOtherClosureVar: (_ x: Swift.Int) -> Swift.Int
public let MyOtherClosureVar: (_ x: Int) -> Int
