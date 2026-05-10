// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interfaces(%t/Foo.swiftinterface, %t/Foo.private.swiftinterface) -parse-as-library %s -library-level api

// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.public.swiftmodule -module-name Foo
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.private.swiftinterface -o %t/Foo.private.swiftmodule -module-name Foo
// RUN: %FileCheck %s --check-prefix=CHECK-PUBLIC < %t/Foo.swiftinterface
// RUN: %FileCheck %s --check-prefix=CHECK-PRIVATE < %t/Foo.private.swiftinterface

@_spi_available(macOS 10.10, tvOS 14.0, *)
@available(iOS 8.0, *)
public class SPIClass {}

// RUN: %FileCheck %s -check-prefix CHECK-PUBLIC < %t/Foo.swiftinterface
// RUN: %FileCheck %s -check-prefix CHECK-PRIVATE < %t/Foo.private.swiftinterface

// CHECK-PUBLIC: @available(macOS, unavailable)
// CHECK-PUBLIC: @available(tvOS, unavailable)

// CHECK-PRIVATE: @_spi_available(macOS, introduced: 10.10)
// CHECK-PRIVATE: @_spi_available(tvOS, introduced: 14.0)
