// REQUIRES: OS=macosx

// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)

// There is a difference in the order of conformances between x86_64 and arm64.
// See https://github.com/swiftlang/swift/pull/84193 for context.
// RUN: expected_sdk="%S/Outputs/dump-module/cake-%target-cpu.json"
// RUN: expected_sdk_abi="%S/Outputs/dump-module/cake-abi-%target-cpu.json"

// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t.mod/cake.swiftmodule %S/Inputs/cake.swift -parse-as-library -I %S/Inputs/ClangCake %clang-importer-sdk-nosource

// RUN: %api-digester -dump-sdk -module cake -o %t.dump.json -module-cache-path %t.module-cache -swift-only -I %t.mod -I %S/Inputs/ClangCake %clang-importer-sdk-nosource -avoid-tool-args
// RUN: diff -u $expected_sdk %t.dump.json
// RUN: %api-digester -dump-sdk -module cake -o %t.dump.json -module-cache-path %t.module-cache -swift-only -I %t.mod -abi -I %S/Inputs/ClangCake %clang-importer-sdk-nosource -avoid-tool-args
// RUN: diff -u $expected_sdk_abi %t.dump.json

// RUN: %api-digester -diagnose-sdk --input-paths %t.dump.json -input-paths $expected_sdk

// Round-trip testing:
// RUN: %api-digester -deserialize-sdk --input-paths $expected_sdk -o %t.dump.json
// RUN: diff -u $expected_sdk %t.dump.json
// RUN: %api-digester -deserialize-sdk --input-paths $expected_sdk_abi -o %t.dump.json
// RUN: diff -u $expected_sdk_abi %t.dump.json

// The input JSON files need to be modified when standard library declarations
// are reordered. This is expected behavior and we simply shouldn't run the test
// when automatically evolving the standard library.
// UNSUPPORTED: swift_evolve


