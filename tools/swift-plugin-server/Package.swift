// swift-tools-version: 6.0

import PackageDescription

// swift/CMakeLists.txt leaves SWIFT_ENABLE_WASM_PLUGINS off by default on Windows.
let wasmSupportingHostPlatforms: [Platform] = [.macOS, .linux, .android, .openbsd, .custom("freebsd")]

let package = Package(
  name: "swift-plugin-server",
  platforms: [
    .macOS(.v15)
  ],
  products: [
    .executable(name: "swift-plugin-server", targets: ["swift-plugin-server"]),
    .library(name: "SwiftInProcPluginServer", type: .dynamic, targets: ["SwiftInProcPluginServer"]),
  ],
  dependencies: [
    .package(path: "../../../swift-syntax"),
    .package(path: "../../../wasmkit"),
    .package(path: "../../../swift-system"),
  ],
  targets: [
    .executableTarget(
      name: "swift-plugin-server",
      dependencies: [
        .product(name: "_SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "_SwiftLibraryPluginProvider", package: "swift-syntax"),
        .product(name: "SystemPackage", package: "swift-system", condition: .when(platforms: wasmSupportingHostPlatforms)),
        .product(name: "WASI", package: "WasmKit", condition: .when(platforms: wasmSupportingHostPlatforms)),
        .product(name: "WasmKit", package: "WasmKit", condition: .when(platforms: wasmSupportingHostPlatforms)),
        .product(name: "WasmKitWASI", package: "WasmKit", condition: .when(platforms: wasmSupportingHostPlatforms)),
      ],
      swiftSettings: [.define("SWIFT_PLUGIN_SERVER_SUPPORTS_WASM", .when(platforms: wasmSupportingHostPlatforms))]
    ),
    .target(
      name: "SwiftInProcPluginServer",
      dependencies: [
        .product(name: "_SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "_SwiftLibraryPluginProvider", package: "swift-syntax"),
      ]
    ),
    .testTarget(
      name: "PluginServerTests",
      dependencies: [
        .product(name: "SystemPackage", package: "swift-system", condition: .when(platforms: wasmSupportingHostPlatforms)),
        .product(name: "WASI", package: "WasmKit", condition: .when(platforms: wasmSupportingHostPlatforms)),
        .product(name: "WAT", package: "WasmKit", condition: .when(platforms: wasmSupportingHostPlatforms)),
        .product(name: "WasmKit", package: "WasmKit", condition: .when(platforms: wasmSupportingHostPlatforms)),
        "swift-plugin-server",
      ],
      swiftSettings: [.define("SWIFT_PLUGIN_SERVER_SUPPORTS_WASM", .when(platforms: wasmSupportingHostPlatforms))]
    ),
  ],
  cxxLanguageStandard: .cxx17
)
