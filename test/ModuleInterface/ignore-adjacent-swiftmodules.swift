// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// REQUIRES: VENDOR=apple

/// Prepare the SDK.
// RUN: cp -r %S/../Sema/Inputs/public-private-sdk %t/sdk
// RUN: %target-swift-frontend -emit-module -module-name PublicSwift -enable-library-evolution -swift-version 5 \
// RUN:   %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftinterface-name) -module-name PublicSwift
// RUN: %target-swift-frontend -emit-module -module-name PrivateSwift -enable-library-evolution -swift-version 5 \
// RUN:   %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftinterface-name) -module-name PrivateSwift

/// Break the swiftmodules.
// RUN: echo "This is a malformed swiftmodule" > %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftmodule-name
// RUN: echo "This is a malformed swiftmodule" > %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftmodule-name

/// There should be no attempt at loading the malformed PublicSwift swiftmodule.
/// This means no notes about:
/// * compiled module is out of date
/// * unable to load compiled module '*': malformed
// RUN: %target-swift-frontend -typecheck %s -sdk %t/sdk \
// RUN:   -module-name Main -module-cache-path %t/cache \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -verify -Rmodule-interface-rebuild

import PublicSwift // expected-remark {{rebuilding module 'PublicSwift' from interface}}

// The private adjacent module under PrivateFrameworks should still be tried first, and then rebuilt.
import PrivateSwift
// expected-remark @-1 {{rebuilding module 'PrivateSwift' from interface}}
// expected-note @-2 {{compiled module is out of date}}
// expected-note @-3 {{: malformed}}
