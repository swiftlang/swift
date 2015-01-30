// RUN: %target-swift-frontend -parse -parse-as-library -verify %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %target-swift-frontend -parse -parse-as-library -verify %clang-importer-sdk %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift

// REQUIRES: objc_interop

