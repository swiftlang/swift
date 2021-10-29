// swift-tools-version:5.4

import PackageDescription

let package = Package(
  name: "GenUnicodeData",
  platforms: [.macOS(.v10_15)],
  targets: [
    .target(
      name: "GenUtils",
      dependencies: []
    ),
    .executableTarget(
      name: "GenNormalization",
      dependencies: ["GenUtils"]
    )
  ]
)
