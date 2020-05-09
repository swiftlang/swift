/// @_implementationOnly imports should be compatible with @_spi imports

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -DLIB_A %s -module-name A -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t
// RUN: rm %t/A.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -DLIB_C %s -I %t

#if LIB_A

@_spi(A) public func foo() {}

#elseif LIB_B

@_spi(A) @_implementationOnly import A
foo() // OK

#elseif LIB_C

import B
foo() // expected-error{{cannot find 'foo' in scope}}

#endif
