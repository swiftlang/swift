// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -parse-as-library -verify -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/../Inputs/clang-importer-sdk -I %S/../Inputs/clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %swift -parse -parse-as-library -verify -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/../Inputs/clang-importer-sdk -I %S/../Inputs/clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: ls -lR %t/clang-module-cache | grep ObjectiveC.pcm
// RUN: ls -lR %t/clang-module-cache | grep Foundation.pcm

