/// A module should be able to leak SPI types from an import through SPI decls

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -DLIB_A %s -module-name A -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -DLIB_C %s -I %t

#if LIB_A

@_spi(A) public struct SecretStruct {
  @_spi(A) public func bar() {}
}

#elseif LIB_B

@_spi(A) import A

@_spi(B) public func foo() -> SecretStruct { fatalError() }

#elseif LIB_C

@_spi(B) import B

var a = foo() // OK
a.bar() // expected-error{{'bar' is inaccessible due to '@_spi' protection level}}

var b = SecretStruct() // expected-error{{cannot find 'SecretStruct' in scope}}

#endif
