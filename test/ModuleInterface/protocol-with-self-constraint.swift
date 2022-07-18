// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t/Test.swiftinterface

public protocol P {
}

public class Foo<T> : P {
  public struct Nested {}
}

extension P {
  public static func blah1<T>(_: Self) where Self == Foo<T> {}
  public static func blah2<T>(_: Self.Nested) where Self == Foo<T> {}

  public static func blah3<T>(_: Self) where Self : Foo<T> {}
  public static func blah4<T>(_: Self.Nested) where Self : Foo<T> {}
}

// CHECK-LABEL: extension Test.P {
// CHECK-NEXT:    public static func blah1<T>(_: Self) where Self == Test.Foo<T>
// CHECK-NEXT:    public static func blah2<T>(_: Test.Foo<T>.Nested) where Self == Test.Foo<T>
// CHECK-NEXT:    public static func blah3<T>(_: Self) where Self : Test.Foo<T>
// CHECK-NEXT:    public static func blah4<T>(_: Test.Foo<T>.Nested) where Self : Test.Foo<T>
// CHECK-NEXT:  }
