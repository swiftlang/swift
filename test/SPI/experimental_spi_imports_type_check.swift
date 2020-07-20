/// Test the use of implementation-only types with -experimental-spi-imports.

/// Build LibCore an internal module and LibPublic a public module using LibCore.
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -DLIB_CORE %s -module-name LibCore -emit-module-path %t/LibCore.swiftmodule -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend -emit-module -DLIB_PUBLIC %s -module-name LibPublic -emit-module-path %t/LibPublic.swiftmodule -I %t -emit-module-interface-path %t/LibPublic.swiftinterface -emit-private-module-interface-path %t/LibPublic.private.swiftinterface -enable-library-evolution -swift-version 5 -experimental-spi-imports

/// Test with the swiftmodule file, the compiler raises an error only when
/// LibCore isn't loaded by the client.
// RUN: %target-typecheck-verify-swift -DCLIENT -I %t
// RUN: %target-swift-frontend -typecheck %s -DCLIENT -DCLIENT_LOAD_CORE -I %t

/// Test with the private swiftinterface file, the compiler raises an error
/// only when LibCore isn't loaded by the client.
// RUN: rm %t/LibPublic.swiftmodule
// RUN: %target-typecheck-verify-swift -DCLIENT -I %t
// RUN: %target-swift-frontend -typecheck %s -DCLIENT -DCLIENT_LOAD_CORE -I %t

/// Test with the public swiftinterface file, the SPI is unknown.
// RUN: rm %t/LibPublic.private.swiftinterface
// RUN: %target-typecheck-verify-swift -DCLIENT -I %t
// RUN: %target-typecheck-verify-swift -DCLIENT -DCLIENT_LOAD_CORE -I %t

#if LIB_CORE

public struct CoreStruct {
  public init() {}
  public func coreMethod() {}
}


#elseif LIB_PUBLIC

@_spi(dummy) @_implementationOnly import LibCore

@_spi(A) public func SPIFunc() -> CoreStruct { return CoreStruct() }


#elseif CLIENT

@_spi(A) import LibPublic

#if CLIENT_LOAD_CORE
import LibCore
#endif

let x = SPIFunc() // expected-error {{cannot find 'SPIFunc' in scope}}
x.coreMethod()

#endif
