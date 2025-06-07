// REQUIRES: OS=macosx
// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t.mod/cake.swiftmodule %S/Inputs/cake.swift -parse-as-library -I %S/Inputs/ClangCake %clang-importer-sdk-nosource
// RUN: %api-digester -dump-sdk -module cake -o %t.dump.json -module-cache-path %t.module-cache -swift-only -I %t.mod -I %S/Inputs/ClangCake %clang-importer-sdk-nosource -avoid-tool-args
// RUN: diff -u %S/Outputs/cake.json %t.dump.json
// RUN: %api-digester -dump-sdk -module cake -o %t.dump.json -module-cache-path %t.module-cache -swift-only -I %t.mod -abi -I %S/Inputs/ClangCake %clang-importer-sdk-nosource -avoid-tool-args
// RUN: diff -u %S/Outputs/cake-abi.json %t.dump.json
// RUN: %api-digester -diagnose-sdk --input-paths %t.dump.json -input-paths %S/Outputs/cake.json

// Round-trip testing:
// RUN: %api-digester -deserialize-sdk --input-paths %S/Outputs/cake.json -o %t.dump.json
// RUN: diff -u %S/Outputs/cake.json %t.dump.json
// RUN: %api-digester -deserialize-sdk --input-paths %S/Outputs/cake-abi.json -o %t.dump.json
// RUN: diff -u %S/Outputs/cake-abi.json %t.dump.json

// The input JSON files need to be modified when standard library declarations
// are reordered. This is expected behavior and we simply shouldn't run the test
// when automatically evolving the standard library.
// UNSUPPORTED: swift_evolve


