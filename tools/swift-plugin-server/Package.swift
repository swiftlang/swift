// swift-tools-version: 5.9

import PackageDescription

let package = Package(
  name: "swift-plugin-server",
  platforms: [
    .macOS(.v10_15)
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
        .product(name: "WasmKitWASI", package: "WasmKit"),
      ]
    ),
  ],
  cxxLanguageStandard: .cxx17
)
