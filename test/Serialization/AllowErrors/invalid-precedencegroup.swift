// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -verify -emit-module -experimental-allow-module-with-compiler-errors %s -o %t/error.swiftmodule
// RUN: %target-swift-frontend -module-name error -emit-module -experimental-allow-module-with-compiler-errors %t/error.swiftmodule -o %t/error2.swiftmodule 2>&1 | %FileCheck %s

// CHECK: allowing deserialization of invalid declaration

precedencegroup SomePrecedence {
  associativity: right
  higherThan: MissingType // expected-error {{unknown precedence group 'MissingType'}}
}

infix operator <>: SomePrecedence
