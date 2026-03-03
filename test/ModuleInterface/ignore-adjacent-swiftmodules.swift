// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache0)
// RUN: %empty-directory(%t/cache1)
// RUN: cp -r %S/../Sema/Inputs/public-private-sdk %t/sdk
// REQUIRES: VENDOR=apple

/// Prepare the SDK.
//// stdlib
// RUN: %target-swift-frontend -emit-module -module-name Swift -enable-library-evolution -swift-version 5 \
// RUN:   -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -Osize \
// RUN:   %t/sdk/usr/lib/swift/Swift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/usr/lib/swift/Swift.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/usr/lib/swift/Swift.swiftmodule/%target-swiftinterface-name \
// RUN:   -parse-stdlib
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/usr/lib/swift/Swift.swiftmodule/%target-swiftinterface-name) -module-name Swift -parse-stdlib

//// Public framework
// RUN: %target-swift-frontend -emit-module -module-name PublicSwift -enable-library-evolution -swift-version 5 \
// RUN:   -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -Osize -sdk %t/sdk \
// RUN:   %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftinterface-name) -module-name PublicSwift

//// Private framework
// RUN: %target-swift-frontend -emit-module -module-name PrivateSwift -enable-library-evolution -swift-version 5 \
// RUN:   -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -Osize -sdk %t/sdk \
// RUN:   %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftinterface-name) -module-name PrivateSwift

//// Public library
// RUN: %target-swift-frontend -emit-module -module-name PublicSwiftLibrary -enable-library-evolution -swift-version 5 \
// RUN:   -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -Osize -sdk %t/sdk \
// RUN:   %t/sdk/usr/lib/swift/PublicSwiftLibrary.swiftmodule/source.swift \
// RUN:   -o %t/sdk/usr/lib/swift/PublicSwiftLibrary.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/usr/lib/swift/PublicSwiftLibrary.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/usr/lib/swift/PublicSwiftLibrary.swiftmodule/%target-swiftinterface-name) -module-name PublicSwiftLibrary

//// Public subframework
// RUN: %target-swift-frontend -emit-module -module-name SubSwift -enable-library-evolution -swift-version 5 \
// RUN:   -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -Osize -sdk %t/sdk \
// RUN:   %t/sdk/System/Library/SubFrameworks/SubSwift.framework/Modules/SubSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/SubFrameworks/SubSwift.framework/Modules/SubSwift.swiftmodule/%target-swiftmodule-name \
// RUN:   -emit-module-interface-path %t/sdk/System/Library/SubFrameworks/SubSwift.framework/Modules/SubSwift.swiftmodule/%target-swiftinterface-name
// RUN: %target-swift-typecheck-module-from-interface(%t/sdk/System/Library/SubFrameworks/SubSwift.framework/Modules/SubSwift.swiftmodule/%target-swiftinterface-name) -module-name SubSwift

/// Break the swiftmodules.
// RUN: echo "This is a malformed swiftmodule" > %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftmodule-name
// RUN: echo "This is a malformed swiftmodule" > %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftmodule-name
// RUN: echo "This is a malformed swiftmodule" > %t/sdk/System/Library/SubFrameworks/SubSwift.framework/Modules/SubSwift.swiftmodule/%target-swiftmodule-name
// RUN: echo "This is a malformed swiftmodule" > %t/sdk/usr/lib/swift/PublicSwiftLibrary.swiftmodule/%target-swiftmodule-name

/// Check the loading behavior from attempts at loading the malformed swiftmodules,
/// printing the notes:
/// * compiled module is out of date
/// * unable to load compiled module '*': malformed

/// Check diagnostics in the local file:
// RUN: %target-swift-frontend -typecheck %s -sdk %t/sdk \
// RUN:   -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import \
// RUN:   -module-name Main -module-cache-path %t/cache0 \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -resource-dir "" \
// RUN:   -verify -verify-ignore-unknown -Rmodule-interface-rebuild -diagnostic-style=llvm

/// Check diagnostic for implicit imports:
// RUN: echo "This is a malformed swiftmodule" > %t/sdk/usr/lib/swift/Swift.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -typecheck %s -sdk %t/sdk \
// RUN:   -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import \
// RUN:   -module-name Main -module-cache-path %t/cache1 \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -resource-dir "" \
// RUN:   -Rmodule-interface-rebuild -Rmodule-loading -diagnostic-style=llvm 2> %t/out
// RUN: %FileCheck --input-file %t/out %s

import Swift
// CHECK: rebuilding module 'Swift' from interface
// CHECK-NEXT: compiled module is out of date
// CHECK-NEXT: : malformed

import PublicSwift // expected-remark {{rebuilding module 'PublicSwift' from interface}}
// expected-note @-1 {{was ignored because it belongs to a framework in the SDK}}

// The private adjacent module under PrivateFrameworks should still be tried first, and then rebuilt.
import PrivateSwift
// expected-remark @-1 {{rebuilding module 'PrivateSwift' from interface}}
// expected-note @-2 {{compiled module is out of date}}
// expected-note @-3 {{: malformed}}

import PublicSwiftLibrary // expected-remark {{rebuilding module 'PublicSwiftLibrary' from interface}}
// expected-note @-1 {{was ignored because it's a library module in the SDK}}

import SubSwift // expected-remark {{rebuilding module 'SubSwift' from interface}}
// expected-note @-1 {{was ignored because it belongs to a framework in the SDK}}
