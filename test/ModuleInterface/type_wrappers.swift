// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/TypeWrappers.swiftinterface) %s -module-name TypeWrappers -enable-experimental-feature TypeWrappers
// RUN: %target-swift-typecheck-module-from-interface(%t/TypeWrappers.swiftinterface) -module-name TypeWrappers
// RUN: %FileCheck %s < %t/TypeWrappers.swiftinterface

// REQUIRES: asserts

// CHECK: @typeWrapper public struct Wrapper<W, S> {
// CHECK-NEXT: public init(for: W.Type, storage: S)
// CHECK-NEXT:   public subscript<V>(propertyKeyPath _: Swift.KeyPath<W, V>, storageKeyPath path: Swift.KeyPath<S, V>) -> V {
// CHECK-NEXT:     get
// CHECK-NEXT:   }
// CHECK-NEXT:   public subscript<V>(propertyKeyPath _: Swift.KeyPath<W, V>, storageKeyPath path: Swift.WritableKeyPath<S, V>) -> V {
// CHECK-NEXT:     get
// CHECK-NEXT:     set
// CHECK-NEXT:   }
// CHECK-NEXT: }

@typeWrapper
public struct Wrapper<W, S> {
  public init(for: W.Type, storage: S) {}

  public subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }

  public subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> V {
    get { fatalError() }
    set { }
  }
}

// CHECK: @TypeWrappers.Wrapper public class Test<T> where T : Swift.StringProtocol {
// CHECK:   public init(a: Swift.Int, b: [T])
// CHECK:   public init(storageWrapper: TypeWrappers.Wrapper<TypeWrappers.Test<T>, TypeWrappers.Test<T>.$Storage>)
// CHECK: }

@Wrapper
public class Test<T: StringProtocol> {
  var a: Int
  let b: [T]
}

@Wrapper
public protocol Wrapped {
  init()
}

public protocol OuterWrapped : Wrapped {}

// CHECK:  @TypeWrappers.Wrapper public struct WithProtocol : TypeWrappers.Wrapped {
// CHECK:   public var a: Swift.Int {
// CHECK:     get
// CHECK:     set
// CHECK:   }
// CHECK:   public var b: Swift.String {
// CHECK:     get
// CHECK:     set
// CHECK:   }
// CHECK:   public init()
// CHECK:   public var $storage: TypeWrappers.Wrapper<TypeWrappers.WithProtocol, TypeWrappers.WithProtocol.$Storage>
// CHECK:   public struct $Storage {
// CHECK:   }
// CHECK:   public init(storageWrapper: TypeWrappers.Wrapper<TypeWrappers.WithProtocol, TypeWrappers.WithProtocol.$Storage>)
// CHECK: }

public struct WithProtocol : Wrapped {
  public var a: Int
  public var b: String

  public init() {
  }
}

// CHECK: @TypeWrappers.Wrapper final public class ClassWithProtoocol : TypeWrappers.Wrapped {
// CHECK:   required public init()
// CHECK:   final public var $storage: TypeWrappers.Wrapper<TypeWrappers.ClassWithProtoocol, TypeWrappers.ClassWithProtoocol.$Storage>
// CHECK:   public struct $Storage {
// CHECK:   }
// CHECK:   public init(storageWrapper: TypeWrappers.Wrapper<TypeWrappers.ClassWithProtoocol, TypeWrappers.ClassWithProtoocol.$Storage>)
// CHECK: }

@Wrapper
public final class ClassWithProtoocol : Wrapped {
  var test: String = ""

  public required init() {}
}

// CHECK: @TypeWrappers.Wrapper public struct WithOuterWrapper<T> : TypeWrappers.OuterWrapped {
// CHECK:   public init()
// CHECK:   public init(_ v: T)
// CHECK:   public var $storage: TypeWrappers.Wrapper<TypeWrappers.WithOuterWrapper<T>, TypeWrappers.WithOuterWrapper<T>.$Storage>
// CHECK:   public struct $Storage {
// CHECK:   }
// CHECK:   public init(storageWrapper: TypeWrappers.Wrapper<TypeWrappers.WithOuterWrapper<T>, TypeWrappers.WithOuterWrapper<T>.$Storage>)
// CHECK: }

@Wrapper
public struct WithOuterWrapper<T> : OuterWrapped {
  var test: T? = nil

  public init() {}

  public init(_ v: T) {
    self.test = v
  }
}

protocol UnrelatedProtocol {
}

// CHECK: @TypeWrappers.Wrapper public struct WithComposition {
// CHECK:   public init()
// CHECK:   public var $storage: TypeWrappers.Wrapper<TypeWrappers.WithComposition, TypeWrappers.WithComposition.$Storage>
// CHECK:   public struct $Storage {
// CHECK:   }
// CHECK:   public init(storageWrapper: TypeWrappers.Wrapper<TypeWrappers.WithComposition, TypeWrappers.WithComposition.$Storage>)
// CHECK: }
// CHECK: extension TypeWrappers.WithComposition : TypeWrappers.OuterWrapped {}

@Wrapper
public struct WithComposition : OuterWrapped & UnrelatedProtocol {
  public init() {}
}
