// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interfaces(%t/Foo.swiftinterface, %t/Foo.private.swiftinterface) -parse-as-library %s -module-name Foo -library-level api -swift-version 6

// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.public.swiftmodule -module-name Foo
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.private.swiftinterface -o %t/Foo.private.swiftmodule -module-name Foo
// RUN: %FileCheck %s --check-prefix=CHECK-PUBLIC < %t/Foo.swiftinterface
// RUN: %FileCheck %s --check-prefix=CHECK-PRIVATE < %t/Foo.private.swiftinterface

// The public interface prints `@_spi_available` overrides as unavailable, but
// it must still build in a language mode that rejects unavailable overrides.
// RUN: %if swift_future %{ %target-swift-emit-module-interfaces(%t/FooFuture.swiftinterface, %t/FooFuture.private.swiftinterface) -parse-as-library %s -module-name Foo -library-level api -swift-version future %}
// RUN: %if swift_future %{ %target-swift-frontend -compile-module-from-interface %t/FooFuture.swiftinterface -o %t/FooFuture.public.swiftmodule -module-name Foo %}
// RUN: %if swift_future %{ %target-swift-frontend -compile-module-from-interface %t/FooFuture.private.swiftinterface -o %t/FooFuture.private.swiftmodule -module-name Foo %}

@_spi_available(macOS 10.10, tvOS 14.0, *)
@available(iOS 8.0, *)
public class SPIClass {}

// RUN: %FileCheck %s -check-prefix CHECK-PUBLIC < %t/Foo.swiftinterface
// RUN: %FileCheck %s -check-prefix CHECK-PRIVATE < %t/Foo.private.swiftinterface

// CHECK-PUBLIC: @available(macOS, unavailable)
// CHECK-PUBLIC: @available(tvOS, unavailable)

// CHECK-PRIVATE: @_spi_available(macOS, introduced: 10.10)
// CHECK-PRIVATE: @_spi_available(tvOS, introduced: 14.0)

open class SPIOverrideBase {
  public init() {}
  open func method() {}
}

open class SPIOverrideDerived: SPIOverrideBase {
  @_spi_available(macOS 10.9, *)
  open override func method() {}
}

// CHECK-PUBLIC:      open class SPIOverrideDerived
// CHECK-PUBLIC:      @available(macOS, unavailable)
// CHECK-PUBLIC-NEXT: override open func method()

// CHECK-PRIVATE:      open class SPIOverrideDerived
// CHECK-PRIVATE:      @_spi_available(macOS, introduced: 10.9)
// CHECK-PRIVATE-NEXT: override open func method()

@_spi_available(macOS 10.15, *)
@frozen public struct SPIFrozenStruct: Hashable {
  public var x: Int
  public init(x: Int) { self.x = x }

  public static let zero = SPIFrozenStruct(x: 0)
}

// CHECK-PUBLIC:      @frozen public struct SPIFrozenStruct
// CHECK-PUBLIC:      {{^}$}}
// CHECK-PUBLIC-NEXT: @available(macOS, unavailable)
// CHECK-PUBLIC-NEXT: extension Foo::SPIFrozenStruct : Swift::Sendable {}

// CHECK-PRIVATE:      @frozen public struct SPIFrozenStruct
// CHECK-PRIVATE:      {{^}$}}
// CHECK-PRIVATE-NEXT: @_spi_available(macOS, introduced: 10.15)
// CHECK-PRIVATE-NEXT: extension Foo::SPIFrozenStruct : Swift::Sendable {}
