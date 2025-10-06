// swift-tools-version: 6.2

import PackageDescription

let package = Package(
  name: "swift-function-caller-generator",
  platforms: [.macOS(.v13)],
  products: [
    .executable(name: "swift-function-caller-generator", targets: ["swift-function-caller-generator"]),
  ],
  dependencies: [
      .package(path: "../../../swift-syntax")
  ],
  targets: [
    .executableTarget(
      name: "swift-function-caller-generator",
      dependencies: [
        .product(name: "SwiftSyntax", package: "swift-syntax"),
        .product(name: "SwiftParser", package: "swift-syntax"),
        .product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
      ]
    ),
  ],
)
