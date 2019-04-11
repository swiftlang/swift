// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name OpaqueResultTypes -emit-parseable-module-interface-path %t/OpaqueResultTypes.swiftinterface %s
// RUN: %FileCheck %s < %t/OpaqueResultTypes.swiftinterface
// RUN: %target-swift-frontend -I %t -typecheck -verify %S/Inputs/opaque-result-types-client.swift

public protocol Foo {}
extension Int: Foo {}

// CHECK-LABEL: public func foo(_: Int) -> some Foo
public func foo(_: Int) -> some Foo {
  return 1738
}

// CHECK-LABEL: @inlinable public func foo(_: String) -> some Foo {
@inlinable public func foo(_: String) -> some Foo {
  return 679
}

// CHECK-LABEL: public func foo<T>(_ x: T) -> some Foo where T : OpaqueResultTypes.Foo
public func foo<T: Foo>(_ x: T) -> some Foo {
  return x
}

public protocol AssocTypeInference {
  associatedtype Assoc: Foo

  func foo(_: Int) -> Assoc
}

public struct Bar<T>: AssocTypeInference {
  public init() {}

  // CHECK-LABEL: public func foo(_: Int) -> some Foo
  public func foo(_: Int) -> some Foo {
    return 20721
  }

  public func foo(_: String) -> some Foo {
    return 219
  }

  // CHECK-LABEL: public func foo<U>(_ x: U) -> some Foo where U : OpaqueResultTypes.Foo
  public func foo<U: Foo>(_ x: U) -> some Foo {
    return x
  }

  public struct Bas: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Int) -> some Foo
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo<U>(_ x: U) -> some Foo where U : OpaqueResultTypes.Foo
    public func foo<U: Foo>(_ x: U) -> some Foo {
      return x
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
  }

  public struct Bass<U: Foo>: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Int) -> some Foo
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo(_ x: U) -> some Foo
    public func foo(_ x: U) -> some Foo {
      return x
    }

    // CHECK-LABEL: public func foo<V>(_ x: V) -> some Foo where V : OpaqueResultTypes.Foo
    public func foo<V: Foo>(_ x: V) -> some Foo {
      return x
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T, U>
  }

  // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
}

public struct Zim: AssocTypeInference {
  public init() {}

  public func foo(_: Int) -> some Foo {
    return 20721
  }

  public func foo(_: String) -> some Foo {
    return 219
  }

  // CHECK-LABEL: public func foo<U>(_ x: U) -> some Foo where U : OpaqueResultTypes.Foo
  public func foo<U: Foo>(_ x: U) -> some Foo {
    return x
  }

  public struct Zang: AssocTypeInference {
    public init() {}

    public func foo(_: Int) -> some Foo {
      return 20721
    }

    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo<U>(_ x: U) -> some Foo where U : OpaqueResultTypes.Foo
    public func foo<U: Foo>(_ x: U) -> some Foo {
      return x
    }
  }

  public struct Zung<U: Foo>: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Int) -> some Foo
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    public func foo(_: String) -> some Foo {
      return 219
    }

    public func foo(_ x: U) -> some Foo {
      return x
    }

    // CHECK-LABEL: public func foo<V>(_ x: V) -> some Foo where V : OpaqueResultTypes.Foo
    public func foo<V: Foo>(_ x: V) -> some Foo {
      return x
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<U>
  }
}
