// RUN: %empty-directory(%t/sdk)
// RUN: %empty-directory(%t/sdk/usr/lib/swift/apinotes)
// RUN: %empty-directory(%t/sdk/usr/lib/swift/%target-sdk-name)
// RUN: cp -r %clang-importer-sdk-path/usr/include %t/sdk/usr
// RUN: cp -r %test-resource-dir/shims %t/sdk/usr/lib/swift
// RUN: cp %S/Inputs/cfuncs.apinotes %t/sdk/usr/lib/swift/apinotes
// RUN: cp -r %platform-module-dir/Swift.swiftmodule %t/sdk/usr/lib/swift/%target-sdk-name/
// RUN: %target-swift-frontend(mock-sdk: -sdk %t/sdk) -typecheck %s -verify -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import

import cfuncs

let array: [UnsafePointer<CChar>?] = [nil]
array.withUnsafeBufferPointer { nullability_note($0.baseAddress!) }
