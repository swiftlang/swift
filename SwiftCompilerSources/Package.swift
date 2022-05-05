// swift-tools-version:5.3
//===--- Package.swift.in - SwiftCompiler SwiftPM package -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import PackageDescription

private extension Target {
  static let defaultSwiftSettings: [SwiftSetting] = [
    .unsafeFlags([
      "-Xfrontend", "-validate-tbd-against-ir=none",
      "-Xfrontend", "-enable-cxx-interop",
      // Bridging modules and headers
      "-Xcc", "-I", "-Xcc", "../include",
      // LLVM modules and headers
      "-Xcc", "-I", "-Xcc", "../../llvm-project/llvm/include",
      "-cross-module-optimization"
    ]),
  ]

  static func compilerModuleTarget(
    name: String,
    dependencies: [Dependency],
    path: String? = nil,
    sources: [String]? = nil,
    swiftSettings: [SwiftSetting] = []) -> Target {
      .target(
        name: name,
        dependencies: dependencies,
        path: path ?? "Sources/\(name)",
        exclude: ["CMakeLists.txt"],
        sources: sources,
        swiftSettings: defaultSwiftSettings + swiftSettings)
    }
}

let package = Package(
  name: "SwiftCompilerSources",
  platforms: [
    .macOS("10.9"),
  ],
  products: [
    .library(
      name: "swiftCompilerModules",
      type: .static,
      targets: ["Basic", "AST", "Parse", "SIL", "Optimizer", "_CompilerRegexParser"]),
  ],
  dependencies: [
  ],
  // Note that targets and their dependencies must align with
  // 'SwiftCompilerSources/Sources/CMakeLists.txt'
  targets: [
    .compilerModuleTarget(
      name: "_CompilerRegexParser",
      dependencies: [],
      path: "_RegexParser_Sources",
      swiftSettings: [
        // Workaround until `_CompilerRegexParser` is imported as implementation-only
        // by `_StringProcessing`.
        .unsafeFlags([
          "-Xfrontend",
          "-disable-implicit-string-processing-module-import"
        ])]),
    .compilerModuleTarget(
      name: "Basic",
      dependencies: []),
    .compilerModuleTarget(
      name: "AST",
      dependencies: ["Basic"]),
    .compilerModuleTarget(
      name: "Parse",
      dependencies: ["Basic", "AST", "_CompilerRegexParser"]),
    .compilerModuleTarget(
      name: "SIL",
      dependencies: ["Basic"]),
    .compilerModuleTarget(
      name: "Optimizer",
      dependencies: ["Basic", "SIL", "Parse"]),
  ]
)
