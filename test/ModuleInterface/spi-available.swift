// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library %s -emit-module -library-level=api -emit-module-path %t/Foo.swiftmodule -module-name Foo -emit-module-interface-path %t/Foo.swiftinterface -emit-private-module-interface-path %t/Foo.private.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftinterface -o %t/Foo.public.swiftmodule -module-name Foo
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.private.swiftinterface -o %t/Foo.private.swiftmodule -module-name Foo

@_spi_available(macOS 10.10, tvOS 14.0, *)
@available(iOS 8.0, *)
public class SPIClass {}

// RUN: %FileCheck %s -check-prefix CHECK-PUBLIC < %t/Foo.swiftinterface
// RUN: %FileCheck %s -check-prefix CHECK-PRIVATE < %t/Foo.private.swiftinterface

// CHECK-PUBLIC: @available(macOS, unavailable)
// CHECK-PUBLIC: @available(tvOS, unavailable)

// CHECK-PRIVATE: @_spi_available(macOS, introduced: 10.10)
// CHECK-PRIVATE: @_spi_available(tvOS, introduced: 14.0)
