// Check round-trip through a .swiftmodule: both plain @cxx and @cxx(...).

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   -emit-module %s \
// RUN:   -module-name Print \
// RUN:   -o %t/Print.swiftmodule
// RUN: %target-swift-ide-test \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %S/Inputs \
// RUN:   -I %t \
// RUN:   -print-module \
// RUN:   -module-to-print=Print \
// RUN:   -source-filename=x | %FileCheck %s

// REQUIRES: swift_feature_CxxImplementation

import Functions

// CHECK: @cxx{{$}}
// CHECK-NEXT: @implementation func bar(_ x: Int32) -> Int32
@cxx @implementation
public func bar(_ x: Int32) -> Int32 {
  return x
}

// A name that does not lex as a plain Swift identifier round-trips with its
// backticks.
// CHECK: @cxx(`defer`)
// CHECK-NEXT: @implementation func deferAlias(_ x: Int32) -> Int32
@cxx(`defer`) @implementation
public func deferAlias(_ x: Int32) -> Int32 {
  return x
}

// CHECK: @cxx(foo)
// CHECK-NEXT: @implementation func fooAlias(_ x: Int32) -> Int32
@cxx(foo) @implementation
public func fooAlias(_ x: Int32) -> Int32 {
  return x
}
