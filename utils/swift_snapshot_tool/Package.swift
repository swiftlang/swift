// swift-tools-version: 5.10.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
  name: "swift_snapshot_tool",
  platforms: [.macOS(.v14)],
  products: [
    // Products define the executables and libraries a package produces, making them visible to other packages.
    .executable(
      name: "swift_snapshot_tool",
      targets: ["swift_snapshot_tool"]
    )
  ],
  dependencies: [
    // other dependencies
    .package(url: "https://github.com/apple/swift-argument-parser", from: "1.3.0")
  ],
  targets: [
    // Targets are the basic building blocks of a package, defining a module or a test suite.
    // Targets can depend on other targets in this package and products from dependencies.
    .executableTarget(
      name: "swift_snapshot_tool",
      dependencies: [.product(name: "ArgumentParser", package: "swift-argument-parser")]
    ),
    .testTarget(
      name: "swift_snapshot_toolTests",
      dependencies: ["swift_snapshot_tool"]
    ),
  ]
)
