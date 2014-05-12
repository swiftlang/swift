// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %swift -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: Foundation{{.*}}.pcm
// CHECK: ObjectiveC{{.*}}.pcm

