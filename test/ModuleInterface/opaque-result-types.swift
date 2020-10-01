// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name OpaqueResultTypes -emit-module-interface-path %t/OpaqueResultTypes.swiftinterface %s
// RUN: %FileCheck %s < %t/OpaqueResultTypes.swiftinterface
// RUN: %target-swift-frontend -I %t -typecheck -verify %S/Inputs/opaque-result-types-client.swift

public protocol Foo {}
extension Int: Foo {}

// CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
public func foo(_: Int) -> some Foo {
  return 1738
}

// CHECK-LABEL: @inlinable public func foo(_: Swift.String) -> some OpaqueResultTypes.Foo {
@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
@inlinable public func foo(_: String) -> some Foo {
  return 679
}

// CHECK-LABEL: public func foo<T>(_ x: T) -> some OpaqueResultTypes.Foo where T : OpaqueResultTypes.Foo
@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
public func foo<T: Foo>(_ x: T) -> some Foo {
  return x
}

public protocol AssocTypeInference {
  associatedtype Assoc: Foo
  associatedtype AssocProperty: Foo
  associatedtype AssocSubscript: Foo

  func foo(_: Int) -> Assoc

  var prop: AssocProperty { get }
  subscript() -> AssocSubscript { get }
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
public struct Bar<T>: AssocTypeInference {
  public init() {}

  // CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public func foo(_: Int) -> some Foo {
    return 20721
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public func foo(_: String) -> some Foo {
    return 219
  }

  // CHECK-LABEL: public func foo<U>(_ x: U) -> some OpaqueResultTypes.Foo where U : OpaqueResultTypes.Foo
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public func foo<U: Foo>(_ x: U) -> some Foo {
    return x
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public struct Bas: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo<U>(_ x: U) -> some OpaqueResultTypes.Foo where U : OpaqueResultTypes.Foo
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo<U: Foo>(_ x: U) -> some Foo {
      return x
    }

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public var prop: some Foo {
      return 123
    }
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public subscript() -> some Foo {
      return 123
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
    // CHECK-LABEL: public typealias AssocProperty = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
    // CHECK-LABEL: public typealias AssocSubscript = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public struct Bass<U: Foo>: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo(_ x: U) -> some OpaqueResultTypes.Foo
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_ x: U) -> some Foo {
      return x
    }

    // CHECK-LABEL: public func foo<V>(_ x: V) -> some OpaqueResultTypes.Foo where V : OpaqueResultTypes.Foo
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo<V: Foo>(_ x: V) -> some Foo {
      return x
    }
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public var prop: some Foo {
      return 123
    }
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public subscript() -> some Foo {
      return 123
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T, U>
    // CHECK-LABEL: public typealias AssocProperty = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T, U>
    // CHECK-LABEL: public typealias AssocSubscript = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T, U>
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public var prop: some Foo {
    return 123
  }
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public subscript() -> some Foo {
    return 123
  }

  // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
  // CHECK-LABEL: public typealias AssocProperty = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
  // CHECK-LABEL: public typealias AssocSubscript = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
public struct Zim: AssocTypeInference {
  public init() {}

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public func foo(_: Int) -> some Foo {
    return 20721
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public func foo(_: String) -> some Foo {
    return 219
  }

  // CHECK-LABEL: public func foo<U>(_ x: U) -> some OpaqueResultTypes.Foo where U : OpaqueResultTypes.Foo
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public func foo<U: Foo>(_ x: U) -> some Foo {
    return x
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public struct Zang: AssocTypeInference {
    public init() {}

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo<U>(_ x: U) -> some OpaqueResultTypes.Foo where U : OpaqueResultTypes.Foo
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo<U: Foo>(_ x: U) -> some Foo {
      return x
    }

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public var prop: some Foo {
      return 123
    }
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public subscript() -> some Foo {
      return 123
    }
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public struct Zung<U: Foo>: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_: String) -> some Foo {
      return 219
    }

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo(_ x: U) -> some Foo {
      return x
    }

    // CHECK-LABEL: public func foo<V>(_ x: V) -> some OpaqueResultTypes.Foo where V : OpaqueResultTypes.Foo
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public func foo<V: Foo>(_ x: V) -> some Foo {
      return x
    }

    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public var prop: some Foo {
      return 123
    }
    @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
    public subscript() -> some Foo {
      return 123
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<U>
    // CHECK-LABEL: public typealias AssocProperty = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<U>
    // CHECK-LABEL: public typealias AssocSubscript = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<U>
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public var prop: some Foo {
    return 123
  }
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  public subscript() -> some Foo {
    return 123
  }
}
