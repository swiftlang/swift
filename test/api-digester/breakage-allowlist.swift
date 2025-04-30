// REQUIRES: VENDOR=apple

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.mod1)
// RUN: %empty-directory(%t.mod2)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %empty-directory(%t.baseline/ABI)

// RUN: echo "public func foo() {}" > %t.mod1/Foo.swift
// RUN: echo "public func bar() {}" > %t.mod2/Foo.swift

// RUN: echo "ABI breakage: func foo() has been removed" > %t/incomplete-allowlist.txt
// RUN: echo "ABI breakage: func foo() has been removed" > %t/complete-allowlist.txt
// RUN: echo "ABI breakage: func bar() is a new API without '@available'" >> %t/complete-allowlist.txt

// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t.mod1/Foo.swiftmodule  %t.mod1/Foo.swift -parse-as-library -enable-library-evolution -emit-module-source-info -emit-module-source-info-path %t.mod1/Foo.swiftsourceinfo -emit-module-interface-path %t.mod1/Foo.swiftinterface -module-name Foo -swift-version 5

// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t.mod2/Foo.swiftmodule  %t.mod2/Foo.swift -parse-as-library -enable-library-evolution -emit-module-source-info -emit-module-source-info-path %t.mod2/Foo.swiftsourceinfo -emit-module-interface-path %t.mod2/Foo.swiftinterface -module-name Foo -swift-version 5

// RUN: %api-digester -dump-sdk -module Foo -output-dir %t.baseline -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod1 -abi -use-interface-for-module Foo

// RUN: %api-digester -diagnose-sdk -baseline-dir %t.baseline -module Foo -I %t.mod2 -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -breakage-allowlist-path %t/complete-allowlist.txt -o %t/expected-diags.txt

// RUN: not %api-digester -diagnose-sdk -baseline-dir %t.baseline -module Foo -I %t.mod2 -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -compiler-style-diags

// RUN: %api-digester -diagnose-sdk -baseline-dir %t.baseline -module Foo -I %t.mod2 -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -compiler-style-diags -breakage-allowlist-path %t/incomplete-allowlist.txt

// RUN: %api-digester -diagnose-sdk -baseline-dir %t.baseline -module Foo -I %t.mod2 -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -compiler-style-diags -breakage-allowlist-path %t/complete-allowlist.txt

// RUN: not %api-digester -diagnose-sdk -baseline-dir %t.baseline -module Foo -I %t.mod2 -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -serialize-diagnostics-path %t/serialized-diag.dia
// RUN: ls %t/serialized-diag.dia

// RUN: %api-digester -diagnose-sdk -baseline-dir %t.baseline -module Foo -I %t.mod2 -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -serialize-diagnostics-path %t/serialized-diag.dia -breakage-allowlist-path %t/incomplete-allowlist.txt
// RUN: ls %t/serialized-diag.dia

// RUN: %api-digester -diagnose-sdk -baseline-dir %t.baseline -module Foo -I %t.mod2 -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -serialize-diagnostics-path %t/serialized-diag.dia -breakage-allowlist-path %t/complete-allowlist.txt
// RUN: ls %t/serialized-diag.dia
