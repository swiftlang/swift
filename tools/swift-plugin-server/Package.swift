// swift-tools-version: 5.6

import PackageDescription

let package = Package(
  name: "swift-plugin-server",
  platforms: [
    .macOS(.v10_15)
  ],
  products: [
    .executable(name: "swift-plugin-server", targets: ["swift-plugin-server"]),
    .library(name: "SwiftInProcPluginServer", type: .dynamic, targets: ["SwiftInProcPluginServer"]),
  ],
  dependencies: [
    .package(path: "../../../swift-syntax"),
  ],
  targets: [
    .executableTarget(
      name: "swift-plugin-server",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "SwiftLibraryPluginProvider", package: "swift-syntax"),
      ]
    ),
    .target(
      name: "SwiftInProcPluginServer",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
        .product(name: "SwiftLibraryPluginProvider", package: "swift-syntax"),
      ]
    ),
  ],
  cxxLanguageStandard: .cxx17
)
