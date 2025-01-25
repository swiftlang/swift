// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/OpaqueResultTypes.swiftinterface) %s -module-name OpaqueResultTypes
// RUN: %target-swift-typecheck-module-from-interface(%t/OpaqueResultTypes.swiftinterface) -module-name OpaqueResultTypes
// RUN: %FileCheck %s < %t/OpaqueResultTypes.swiftinterface
// RUN: %target-swift-frontend -I %t -typecheck -verify %S/Inputs/opaque-result-types-client.swift

public protocol Foo {}
extension Int: Foo {}

// CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
@available(SwiftStdlib 5.1, *)
public func foo(_: Int) -> some Foo {
  return 1738
}

// CHECK-LABEL: @inlinable public func foo(_: Swift.String) -> some OpaqueResultTypes.Foo {
@available(SwiftStdlib 5.1, *)
@inlinable public func foo(_: String) -> some Foo {
  return 679
}

// CHECK-LABEL: public func foo<T>(_ x: T) -> some OpaqueResultTypes.Foo where T : OpaqueResultTypes.Foo
@available(SwiftStdlib 5.1, *)
public func foo<T: Foo>(_ x: T) -> some Foo {
  return x
}

// CHECK-LABEL: public var globalComputedVar: some OpaqueResultTypes.Foo {
// CHECK-NEXT:    get
// CHECK-NEXT:  }
@available(SwiftStdlib 5.1, *)
public var globalComputedVar: some Foo { 123 }

// CHECK-LABEL: public var globalVar: some OpaqueResultTypes.Foo{{$}}
@available(SwiftStdlib 5.1, *)
public var globalVar: some Foo = 123

// CHECK-LABEL: public var globalVarTuple: (some OpaqueResultTypes.Foo, some OpaqueResultTypes.Foo){{$}}
@available(SwiftStdlib 5.1, *)
public var globalVarTuple: (some Foo, some Foo) = (123, foo(123))

public protocol AssocTypeInference {
  associatedtype Assoc: Foo
  associatedtype AssocProperty: Foo
  associatedtype AssocSubscript: Foo

  func foo(_: Int) -> Assoc

  var prop: AssocProperty { get }
  subscript() -> AssocSubscript { get }
}

@available(SwiftStdlib 5.1, *)
public struct Bar<T>: AssocTypeInference {
  public init() {}

  // CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
  @available(SwiftStdlib 5.1, *)
  public func foo(_: Int) -> some Foo {
    return 20721
  }

  @available(SwiftStdlib 5.1, *)
  public func foo(_: String) -> some Foo {
    return 219
  }

  // CHECK-LABEL: public func foo<U>(_ x: U) -> some OpaqueResultTypes.Foo where U : OpaqueResultTypes.Foo
  @available(SwiftStdlib 5.1, *)
  public func foo<U: Foo>(_ x: U) -> some Foo {
    return x
  }

  @available(SwiftStdlib 5.1, *)
  public struct Bas: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
    @available(SwiftStdlib 5.1, *)
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    @available(SwiftStdlib 5.1, *)
    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo<U>(_ x: U) -> some OpaqueResultTypes.Foo where U : OpaqueResultTypes.Foo
    @available(SwiftStdlib 5.1, *)
    public func foo<U: Foo>(_ x: U) -> some Foo {
      return x
    }

    @available(SwiftStdlib 5.1, *)
    public var prop: some Foo {
      return 123
    }
    @available(SwiftStdlib 5.1, *)
    public subscript() -> some Foo {
      return 123
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
    // CHECK-LABEL: public typealias AssocProperty = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
    // CHECK-LABEL: public typealias AssocSubscript = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
  }

  @available(SwiftStdlib 5.1, *)
  public struct Bass<U: Foo>: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
    @available(SwiftStdlib 5.1, *)
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    @available(SwiftStdlib 5.1, *)
    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo(_ x: U) -> some OpaqueResultTypes.Foo
    @available(SwiftStdlib 5.1, *)
    public func foo(_ x: U) -> some Foo {
      return x
    }

    // CHECK-LABEL: public func foo<V>(_ x: V) -> some OpaqueResultTypes.Foo where V : OpaqueResultTypes.Foo
    @available(SwiftStdlib 5.1, *)
    public func foo<V: Foo>(_ x: V) -> some Foo {
      return x
    }
    @available(SwiftStdlib 5.1, *)
    public var prop: some Foo {
      return 123
    }
    @available(SwiftStdlib 5.1, *)
    public subscript() -> some Foo {
      return 123
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T, U>
    // CHECK-LABEL: public typealias AssocProperty = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T, U>
    // CHECK-LABEL: public typealias AssocSubscript = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T, U>
  }

  @available(SwiftStdlib 5.1, *)
  public var prop: some Foo {
    return 123
  }
  @available(SwiftStdlib 5.1, *)
  public subscript() -> some Foo {
    return 123
  }

  // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
  // CHECK-LABEL: public typealias AssocProperty = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
  // CHECK-LABEL: public typealias AssocSubscript = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<T>
}

@available(SwiftStdlib 5.1, *)
public struct Zim: AssocTypeInference {
  public init() {}

  @available(SwiftStdlib 5.1, *)
  public func foo(_: Int) -> some Foo {
    return 20721
  }

  @available(SwiftStdlib 5.1, *)
  public func foo(_: String) -> some Foo {
    return 219
  }

  // CHECK-LABEL: public func foo<U>(_ x: U) -> some OpaqueResultTypes.Foo where U : OpaqueResultTypes.Foo
  @available(SwiftStdlib 5.1, *)
  public func foo<U: Foo>(_ x: U) -> some Foo {
    return x
  }

  @available(SwiftStdlib 5.1, *)
  public struct Zang: AssocTypeInference {
    public init() {}

    @available(SwiftStdlib 5.1, *)
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    @available(SwiftStdlib 5.1, *)
    public func foo(_: String) -> some Foo {
      return 219
    }

    // CHECK-LABEL: public func foo<U>(_ x: U) -> some OpaqueResultTypes.Foo where U : OpaqueResultTypes.Foo
    @available(SwiftStdlib 5.1, *)
    public func foo<U: Foo>(_ x: U) -> some Foo {
      return x
    }

    @available(SwiftStdlib 5.1, *)
    public var prop: some Foo {
      return 123
    }
    @available(SwiftStdlib 5.1, *)
    public subscript() -> some Foo {
      return 123
    }
  }

  @available(SwiftStdlib 5.1, *)
  public struct Zung<U: Foo>: AssocTypeInference {
    public init() {}

    // CHECK-LABEL: public func foo(_: Swift.Int) -> some OpaqueResultTypes.Foo
    @available(SwiftStdlib 5.1, *)
    public func foo(_: Int) -> some Foo {
      return 20721
    }

    @available(SwiftStdlib 5.1, *)
    public func foo(_: String) -> some Foo {
      return 219
    }

    @available(SwiftStdlib 5.1, *)
    public func foo(_ x: U) -> some Foo {
      return x
    }

    // CHECK-LABEL: public func foo<V>(_ x: V) -> some OpaqueResultTypes.Foo where V : OpaqueResultTypes.Foo
    @available(SwiftStdlib 5.1, *)
    public func foo<V: Foo>(_ x: V) -> some Foo {
      return x
    }

    @available(SwiftStdlib 5.1, *)
    public var prop: some Foo {
      return 123
    }
    @available(SwiftStdlib 5.1, *)
    public subscript() -> some Foo {
      return 123
    }

    // CHECK-LABEL: public typealias Assoc = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<U>
    // CHECK-LABEL: public typealias AssocProperty = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<U>
    // CHECK-LABEL: public typealias AssocSubscript = @_opaqueReturnTypeOf("{{.*}}", 0) {{.*}}<U>
  }

  @available(SwiftStdlib 5.1, *)
  public var prop: some Foo {
    return 123
  }
  @available(SwiftStdlib 5.1, *)
  public subscript() -> some Foo {
    return 123
  }
}
