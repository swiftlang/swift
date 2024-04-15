// swift-tools-version: 5.9

import PackageDescription

let package = Package(
  name: "swift-plugin-server",
  platforms: [
    .macOS(.v10_15)
  ],
  products: [
    .library(name: "swift-plugin-server", targets: ["swift-plugin-server"]),
    .library(name: "swift-wasm-plugin-server", targets: ["swift-wasm-plugin-server"]),
  ],
  dependencies: [
    .package(path: "../../../swift-syntax"),
    .package(path: "../../lib/ASTGen"),
  ],
  targets: [
    .target(
      name: "CSwiftPluginServer",
      cxxSettings: [
        .unsafeFlags([
          "-I", "../../include",
          "-I", "../../stdlib/public/SwiftShims",
          "-I", "../../../build/Default/swift/include",
          "-I", "../../../build/Default/llvm/include",
          "-I", "../../../llvm-project/llvm/include",
        ])
      ]
    ),
    .target(
      name: "swift-plugin-server",
      dependencies: [
        .product(name: "swiftLLVMJSON", package: "ASTGen"),
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "SwiftDiagnostics", package: "swift-syntax"),
        .product(name: "SwiftSyntax", package: "swift-syntax"),
        .product(name: "SwiftOperators", package: "swift-syntax"),
        .product(name: "SwiftParser", package: "swift-syntax"),
        .product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
        "CSwiftPluginServer"
      ],
      swiftSettings: [.interoperabilityMode(.Cxx)]
    ),
    .target(
      name: "swift-wasm-plugin-server",
      dependencies: [
        .product(name: "swiftLLVMJSON", package: "ASTGen"),
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        "CSwiftPluginServer"
      ],
      swiftSettings: [.interoperabilityMode(.Cxx)]
    ),
  ],
  cxxLanguageStandard: .cxx17
)
