// RUN: %empty-directory(%t)
// REQUIRES: VENDOR=apple
// REQUIRES: asserts

/// Prepare the SDK.
// RUN: cp -r %S/Inputs/public-private-sdk %t/sdk
// RUN: %target-swift-frontend -emit-module -module-name PublicSwift \
// RUN:   %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -emit-module -module-name PrivateSwift \
// RUN:   %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftmodule-name

/// Expect errors when building a public client.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -verify -D PUBLIC_IMPORTS -module-name MainLib

/// Expect no errors when building an SPI client.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level spi -D PUBLIC_IMPORTS -module-name MainLib

/// The driver should also accept the flag and pass it along.
// RUN: %target-swiftc_driver -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level spi -D PUBLIC_IMPORTS -module-name MainLib

/// Expect no errors when building a client with some other library level.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -D PUBLIC_IMPORTS -module-name MainLib
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level other -D PUBLIC_IMPORTS -module-name MainLib
#if PUBLIC_IMPORTS
import PublicSwift
import PrivateSwift // expected-error{{private module 'PrivateSwift' is imported publicly from the public module 'MainLib'}}

import PublicClang
import PublicClang_Private // expected-error{{private module 'PublicClang_Private' is imported publicly from the public module 'MainLib'}}
import FullyPrivateClang // expected-error{{private module 'FullyPrivateClang' is imported publicly from the public module 'MainLib'}}
import LocalClang // expected-error{{private module 'LocalClang' is imported publicly from the public module 'MainLib'}}
@_exported import MainLib // expected-warning{{private module 'MainLib' is imported publicly from the public module 'MainLib'}}

/// Expect no errors with implementation-only imports.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -D IMPL_ONLY_IMPORTS
#elseif IMPL_ONLY_IMPORTS

@_implementationOnly import PrivateSwift
@_implementationOnly import PublicClang_Private
@_implementationOnly import FullyPrivateClang
@_implementationOnly import LocalClang

#endif

/// Test error message on an unknown library level name.
// RUN: not %target-swift-frontend -typecheck %s -library-level ThatsNotALibraryLevel 2>&1 \
// RUN:   | %FileCheck %s --check-prefix CHECK-ARG
// CHECK-ARG: error: unknown library level 'ThatsNotALibraryLevel', expected one of 'api', 'spi' or 'other'
