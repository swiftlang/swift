// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t

/// Create a Swift module to import.
// RUN: cp %t/module.modulemap %t/sdk/
// RUN: %target-swift-frontend -emit-module \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/sdk/LocalSystemModule.swiftmodule \
// RUN:   -emit-module-interface-path %t/sdk/LocalSystemModule.swiftinterface \
// RUN:   %t/LocalSystemModule.swift -I %t/sdk \
// RUN:   -enable-testing

/// Build a test client once with indexing, not setting an SDK.
// RUN: %target-swift-frontend -typecheck %t/TestClient.swift \
// RUN:   -I %t/sdk -module-cache-path %t/cache \
// RUN:   -index-system-modules \
// RUN:   -index-store-path %t/idx \
// RUN:   -index-ignore-stdlib \
// RUN:   -sdk %t/someothersdk -Rmodule-loading 2>&1 \
// RUN:   | %FileCheck -check-prefix=CHECK-SWIFTMODULE %s
// CHECK-SWIFTMODULE: source: '{{.*}}LocalSystemModule.swiftmodule'

/// Build the test client again not setting an SDK, this one should still use
/// the adjacent module that is built for testing.
// RUN: %target-swift-frontend -typecheck %t/TestClient.swift \
// RUN:   -I %t/sdk -module-cache-path %t/cache \
// RUN:   -sdk %t/someothersdk -Rmodule-loading 2>&1 \
// RUN:   | %FileCheck -check-prefix=CHECK-SWIFTMODULE %s

/// @testable import of a module in the SDK works once but not after it's cached.
// RUN: %empty-directory(%t/idx)
// RUN: %target-swift-frontend -typecheck %t/TestClient.swift \
// RUN:   -I %t/sdk -module-cache-path %t/cache \
// RUN:   -index-system-modules \
// RUN:   -index-store-path %t/idx \
// RUN:   -index-ignore-stdlib \
// RUN:   -sdk %t/sdk -Rmodule-loading 2>&1 \
// RUN:   | %FileCheck -check-prefix=CHECK-SWIFTMODULE %s

/// Failing case when building against the cached swiftmodule.
// RUN: %target-swift-frontend -typecheck %t/TestClient.swift \
// RUN:   -I %t/sdk -module-cache-path %t/cache \
// RUN:   -sdk %t/sdk -Rmodule-loading \
// RUN:   -verify -verify-ignore-unknown -show-diagnostics-after-fatal 2>&1

//--- module.modulemap

module LocalSystemModule [system] { }

//--- LocalSystemModule.swift

@_exported import LocalSystemModule

//--- TestClient.swift

@testable import LocalSystemModule // expected-error {{module 'LocalSystemModule' was not compiled for testing}}
// expected-remark @-1 {{LocalSystemModule.swiftinterface}}
// expected-remark @-2 {{'LocalSystemModule' has a required transitive dependency on 'LocalSystemModule'}}
// expected-remark @-3 {{'LocalSystemModule' has a required transitive dependency on 'Swift'}}
// expected-remark @-4 {{'LocalSystemModule' has a required transitive dependency on 'SwiftOnoneSupport'}}
// expected-remark @-5 {{'LocalSystemModule' has a required transitive dependency on '_Concurrency'}}
// expected-remark @-6 {{'LocalSystemModule' has a required transitive dependency on '_StringProcessing'}}
// expected-remark @-7 {{'LocalSystemModule' has a required transitive dependency on '_SwiftConcurrencyShims'}}

