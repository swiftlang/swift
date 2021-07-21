// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s

@propertyWrapper public struct W { public var wrappedValue: ()
public init(wrappedValue: ()) {} }
public struct S { @W var prop: () }
