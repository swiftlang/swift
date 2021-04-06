// RUN: %empty-directory(%t)
// REQUIRES: VENDOR=apple

/// Prepare the SDK.
// RUN: cp -r %S/Inputs/public-private-sdk %t/sdk
// RUN: %target-swift-frontend -emit-module -module-name PublicSwift \
// RUN:   %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -emit-module -module-name PrivateSwift \
// RUN:   %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftmodule-name

/// Expect warnings when building a public client.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -verify -D PUBLIC_IMPORTS

/// Expect no warnings when building an SPI client.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level spi -D PUBLIC_IMPORTS

/// Expect no warnings when building a client with some other library level.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -D PUBLIC_IMPORTS
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level other -D PUBLIC_IMPORTS
#if PUBLIC_IMPORTS
import PublicSwift
import PrivateSwift // expected-warning{{private module 'PrivateSwift' is imported publicly from the public module 'main'}}

import PublicClang
import PublicClang_Private // expected-warning{{private module 'PublicClang_Private' is imported publicly from the public module 'main'}}
import FullyPrivateClang // expected-warning{{private module 'FullyPrivateClang' is imported publicly from the public module 'main'}}
import main // expected-warning{{'implementation-only-import-suggestion.swift' is part of module 'main'; ignoring import}}

/// Expect no warnings with implementation-only imports.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -D IMPL_ONLY_IMPORTS
#elseif IMPL_ONLY_IMPORTS

@_implementationOnly import PrivateSwift
@_implementationOnly import PublicClang_Private
@_implementationOnly import FullyPrivateClang

#endif

/// Test error message on an unknown library level name.
// RUN: not %target-swift-frontend -typecheck %s -library-level ThatsNotALibraryLevel 2>&1 \
// RUN:   | %FileCheck %s --check-prefix CHECK-ARG
// CHECK-ARG: error: unknown library level 'ThatsNotALibraryLevel', expected one of 'api', 'spi' or 'other'
