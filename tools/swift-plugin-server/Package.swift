// swift-tools-version: 5.9

import PackageDescription

let allowJSC = true

let package = Package(
  name: "swift-plugin-server",
  platforms: [
    .macOS(.v11),
  ],
  products: [
    .library(name: "swift-plugin-server", targets: ["swift-plugin-server"]),
    .library(name: "swift-wasm-plugin-server", targets: ["swift-wasm-plugin-server"]),
  ],
  dependencies: [
    .package(path: "../../../swift-syntax"),
    .package(path: "../../../wasmkit"),
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
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
        "CSwiftPluginServer",
      ],
      swiftSettings: [.interoperabilityMode(.Cxx)]
    ),
    .target(
      name: "swift-wasm-plugin-server",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "WASI", package: "WasmKit"),
        .product(name: "WasmKitWASI", package: "WasmKit", condition: .when(platforms: [.linux, .windows])),
      ],
      swiftSettings: allowJSC ? [.define("SWIFT_WASM_USE_JSC", .when(platforms: [.macOS]))] : []
    ),
  ],
  cxxLanguageStandard: .cxx17
)
