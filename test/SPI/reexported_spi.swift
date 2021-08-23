/// Test @_spi and @_exported imports.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -DLIB_A %s -module-name A -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t
// RUN: %target-swift-frontend -typecheck -DCLIENT_EXTERNAL %s -I %t -verify

#if LIB_A

@_spi(A) public func spiFunc() {}

@_spi(A) public struct SPIStruct {
    public init() {}
}

#elseif LIB_B

@_exported import A
@_spi(A) import A

spiFunc() // OK
let x = SPIStruct() // OK

#elseif CLIENT_EXTERNAL

import B

spiFunc() // expected-error{{cannot find 'spiFunc' in scope}}
let x = SPIStruct() // expected-error{{cannot find 'SPIStruct' in scope}}

#endif
