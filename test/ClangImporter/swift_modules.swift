// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift

// REQUIRES: objc_interop

