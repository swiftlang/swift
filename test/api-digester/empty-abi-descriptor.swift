// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi.json %S/Inputs/ConstExtraction/SimpleReferences.swift -empty-abi-descriptor
// RUN: %api-digester -deserialize-sdk -input-paths %t/abi.json -o %t/abi.result.json
// RUN: %api-digester -generate-empty-baseline -o %t/abi-tool.json -avoid-tool-args -abi
// RUN: diff -u %t/abi-tool.json %t/abi.result.json
