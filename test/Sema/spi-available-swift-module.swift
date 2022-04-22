// REQUIRES: OS=ios
// REQUIRES: rdar91325474
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/macos)
// RUN: %empty-directory(%t/ios)

// RUN: %target-swift-frontend -target x86_64-apple-macosx11.9 -parse-as-library %s -emit-module -library-level=api -emit-module-path %t/macos/Foo.swiftmodule -module-name Foo -DFoo
// RUN: %target-swift-frontend -target arm64-apple-ios13.0 -parse-as-library %s -emit-module -library-level=api -emit-module-path %t/ios/Foo.swiftmodule -module-name Foo -DFoo

// RUN: not %target-swift-frontend -target x86_64-apple-macosx11.9 -parse-as-library %s -typecheck -library-level=api -I %t/macos >& %t/macos.txt
// RUN: %FileCheck %s < %t/macos.txt

// RUN: %target-swift-frontend -target arm64-apple-ios13.0 -parse-as-library %s -typecheck -library-level=api -I %t/ios

#if Foo

@_spi_available(macOS 10.10, *)
@available(iOS 8.0, *)
public class SPIClass {}

#else

import Foo
public func foo(_ c: SPIClass) {}

#endif

// CHECK: cannot use class 'SPIClass' here; it is an SPI imported from 'Foo'
