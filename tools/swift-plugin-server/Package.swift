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
    .executableTarget(
      name: "swift-plugin-server",
      dependencies: [
        .product(name: "SwiftCompilerPluginMessageHandling", package: "swift-syntax"),
      ]
    ),
  ],
  cxxLanguageStandard: .cxx17
)
