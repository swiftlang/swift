// REQUIRES: VENDOR=apple

// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)

// RUN: echo "public func foo() {}" > %t.swift

// RUN: %target-swift-frontend -emit-module -emit-module-interface-path %t.mod/cake.swiftinterface %t.swift %clang-importer-sdk-nosource -parse-as-library -enable-library-evolution -disable-objc-attr-requires-foundation-module -module-cache-path %t.module-cache -emit-module-path %t.mod/cake.swiftmodule -module-name cake -swift-version 5

// Step 1: we should be able to load if we prefer cake.swiftinterface
// RUN: %api-digester -dump-sdk -print-module -module cake -I %t.mod -sdk %clang-importer-sdk-path -module-cache-path %t.module-cache -o %t.json -abi -abort-on-module-fail -use-interface-for-module cake

// RUN: echo "Swift Syntax Error" >> %t.mod/cake.swiftinterface

// Step 2: we shouldn't be able to load if we prefer cake.swiftinterface and cake.swiftinterface is broken
// RUN: not %api-digester -dump-sdk -print-module -module cake -I %t.mod -sdk %clang-importer-sdk-path -module-cache-path %t.module-cache -o %t.json -abi -abort-on-module-fail -use-interface-for-module cake

// Step 3: we should be able to load if we don't prefer cake.swiftinterface
// RUN: %api-digester -dump-sdk -print-module -module cake -I %t.mod -sdk %clang-importer-sdk-path -module-cache-path %t.module-cache -o %t.json -abi -abort-on-module-fail
