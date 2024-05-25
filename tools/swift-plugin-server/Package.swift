// swift-tools-version: 5.9

import PackageDescription

let allowJSC = true

let package = Package(
  name: "swift-plugin-server",
  platforms: [
    .macOS(.v10_15)
  ],
  products: [
    .executable(name: "swift-plugin-server", targets: ["swift-plugin-server"]),
  ],
  dependencies: [
    .package(path: "../../../swift-syntax"),
    .package(path: "../../../wasmkit"),
  ],
  targets: [
    .executableTarget(
      name: "swift-plugin-server",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "SwiftLibraryPluginProvider", package: "swift-syntax"),
        .product(name: "WASI", package: "WasmKit"),
        .product(name: "WasmKitWASI", package: "WasmKit", condition: .when(
            platforms: [.linux, .windows] + (allowJSC ? [] : [.macOS])
        )),
      ],
      swiftSettings: allowJSC ? [.define("SWIFT_WASM_USE_JSC", .when(platforms: [.macOS]))] : []
    ),
  ],
  cxxLanguageStandard: .cxx17
)
