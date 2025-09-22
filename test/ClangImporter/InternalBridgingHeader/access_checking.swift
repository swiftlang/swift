// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/tmp

// Test with the normal bridging header.
// RUN: %target-typecheck-verify-swift -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk

// Test with a precompiled bridging header.
// RUN: %target-swift-frontend -emit-pch -o %t/c-bridging-header.pch %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk
// RUN: %target-typecheck-verify-swift -internal-import-bridging-header %t/c-bridging-header.pch -sdk %clang-importer-sdk


// Overrides the internal import that comes through the bridging header.
public import macros

public func getRed() -> Color { red } // expected-error{{function cannot be declared public because its result uses an internal type}}
// expected-note@-1{{struct 'Color' is imported by this file as 'internal' from bridging header}}

public func getX(point: MyPoint) -> Double { point.x } // expected-error{{function cannot be declared public because its parameter uses an internal type}}
// expected-note@-1{{struct 'MyPoint' is imported by this file as 'internal' from bridging header}}

// Comes from the macros module.
public func returnsFromMacrosModule() -> okay_t { 0 }

public protocol P {
  associatedtype A
}

public struct MyType: P {
  public typealias A = MyDouble
  // expected-error@-1{{type alias cannot be declared public because its underlying type uses an internal type}}
  // expected-note@-2{{type alias 'MyDouble' is imported by this file as 'internal' from bridging header}}
}

// expected-error@+1{{cannot use struct 'MyPoint' in an extension with public or '@usableFromInline' members; it was imported via the internal bridging header}}
extension MyPoint: P {
  public typealias A = MyDouble
  // expected-error@-1{{type alias cannot be declared public because its underlying type uses an internal type}}
  // expected-note@-2{{type alias 'MyDouble' is imported by this file as 'internal' from bridging header}}
}
