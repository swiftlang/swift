// swift-tools-version: 5.6

// The CMake build system is the only one that's able to produce a working
// compiler. This Package.swift makes it easier to build and work with the
// swiftASTGen library within IDEs, but it's mainly there for editing---it
// won't create something that can be meaningfully executed. Most things with
// the new Swift parser are better implemented/tested within or on top of the
// swift-syntax package.

import PackageDescription

let package = Package(
  name: "ASTGen",
  platforms: [
    .macOS(.v10_15)
  ],
  products: [
    .library(name: "swiftASTGen", targets: ["swiftASTGen"])
  ],
  dependencies: [
    .package(path: "../../../swift-syntax")
  ],
  targets: [
    .target(
      name: "swiftASTGen",
      dependencies: [
        .product(name: "SwiftDiagnostics", package: "swift-syntax"),
        .product(name: "SwiftSyntax", package: "swift-syntax"),
        .product(name: "SwiftParser", package: "swift-syntax"),
        .product(name: "_SwiftSyntaxMacros", package: "swift-syntax"),
      ],
      path: ".",
      exclude: ["CMakeLists.txt"],
      swiftSettings: [
        .unsafeFlags([
          "-I", "../../include/swift/",
          "-I", "../../include",
        ])
      ])
  ]
)
