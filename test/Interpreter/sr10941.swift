// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

// https://bugs.swift.org/browse/SR-10941

public protocol SupportedPropertyType { }

public protocol TypedSupportedType: SupportedPropertyType where FactoryType.BuildType == Self {
    associatedtype FactoryType: TypedSupportedTypeFactory
}

public protocol TypedSupportedTypeFactory {
    associatedtype BuildType: SupportedPropertyType
}

public class Factory<BuildType: TypedSupportedType>: TypedSupportedTypeFactory {
}

public struct ContentMode : TypedSupportedType {
    public typealias FactoryType = Factory<ContentMode>
}

func bar<T : SupportedPropertyType>(_: T.Type) {}

func baz<T : TypedSupportedType>(_ t: T.Type) {
  bar(t)
}

// This used to recurse infinitely because the base witness table
// for 'ContentMode : SupportedPropertyType' was incorrectly
// minimized away.
baz(ContentMode.self)
