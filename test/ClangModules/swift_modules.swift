// RUN: %swift -parse -parse-as-library -verify -target x86_64-apple-macosx10.9 %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %swift -parse -parse-as-library -verify -target x86_64-apple-macosx10.9 %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift

