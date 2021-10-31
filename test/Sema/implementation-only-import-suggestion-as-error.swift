/// Same test as implementation-only-import-suggestion upgrading warnings to
/// errors. We can remove this test when this becomes the default behavior.

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

/// Expect errors when building a public client.
// RUN: env ENABLE_PUBLIC_IMPORT_OF_PRIVATE_AS_ERROR=1 \
// RUN:    %target-swift-frontend -typecheck -sdk %t/sdk -module-cache-path %t %s \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -verify

import PublicSwift
import PrivateSwift // expected-error{{private module 'PrivateSwift' is imported publicly from the public module 'main'}}

import PublicClang
import PublicClang_Private // expected-error{{private module 'PublicClang_Private' is imported publicly from the public module 'main'}}
import FullyPrivateClang // expected-error{{private module 'FullyPrivateClang' is imported publicly from the public module 'main'}}
import main // expected-warning{{'implementation-only-import-suggestion-as-error.swift' is part of module 'main'; ignoring import}}
