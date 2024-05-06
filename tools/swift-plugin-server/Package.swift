// swift-tools-version: 5.6

import PackageDescription

let package = Package(
  name: "swift-plugin-server",
  platforms: [
    .macOS(.v10_15)
  ],
  dependencies: [
    .package(path: "../../../swift-syntax"),
  ],
  targets: [
    .target(
      name: "CSwiftPluginServer",
      cxxSettings: [
        .unsafeFlags([
          "-I", "../../include",
          "-I", "../../../llvm-project/llvm/include",
        ])
      ]
    ),
    .executableTarget(
      name: "swift-plugin-server",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "SwiftDiagnostics", package: "swift-syntax"),
        .product(name: "SwiftSyntax", package: "swift-syntax"),
        .product(name: "SwiftOperators", package: "swift-syntax"),
        .product(name: "SwiftParser", package: "swift-syntax"),
        .product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
        "CSwiftPluginServer"
      ]
    ),
  ],
  cxxLanguageStandard: .cxx17
)
