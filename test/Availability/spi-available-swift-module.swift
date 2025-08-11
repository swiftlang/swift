// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -parse-as-library %t/Foo.swift -emit-module -library-level api -emit-module-path %t/Foo.swiftmodule -module-name Foo
// RUN: %target-swift-frontend-typecheck -parse-as-library %t/Client.swift -verify -library-level api -I %t

//--- Foo.swift

@_spi_available(macOS 10.10, *)
@available(iOS 8.0, *)
public class MacOSSPIClass {}

@_spi_available(iOS 8.0, *)
@available(macOS 10.10, *)
public class iOSSPIClass {}

//--- Client.swift

import Foo

@available(macOS 10.10, iOS 8.0, *)
public struct Foo {
  public var macos: MacOSSPIClass // expected-error {{cannot use class 'MacOSSPIClass' here; it is an SPI imported from 'Foo'}}
  public var ios: iOSSPIClass
}
