// swift-tools-version: 5.8
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

import class Foundation.ProcessInfo

let package = Package(
  name: "swift-xcodegen",
  platforms: [.macOS(.v13)],
  targets: [
    .target(
      name: "SwiftXcodeGen",
      dependencies: [
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
        .product(name: "SwiftOptions", package: "swift-driver"),
      ],
      exclude: [
        "Xcodeproj/README.md"
      ],
      swiftSettings: [
        .enableExperimentalFeature("StrictConcurrency")
      ]
    ),
    .executableTarget(
      name: "swift-xcodegen",
      dependencies: [
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
        "SwiftXcodeGen",
      ],
      swiftSettings: [
        .enableExperimentalFeature("StrictConcurrency")
      ]
    ),
    .testTarget(
      name: "SwiftXcodeGenTest",
      dependencies: ["SwiftXcodeGen"]
    ),
  ]
)

if ProcessInfo.processInfo.environment["SWIFTCI_USE_LOCAL_DEPS"] == nil {
  package.dependencies += [
    .package(url: "https://github.com/apple/swift-argument-parser", from: "1.4.0"),
    .package(url: "https://github.com/swiftlang/swift-driver", branch: "main"),
  ]
} else {
  package.dependencies += [
    .package(path: "../../../swift-argument-parser"),
    .package(path: "../../../swift-driver"),
  ]
}
