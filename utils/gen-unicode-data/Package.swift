// swift-tools-version:5.6

import PackageDescription

let package = Package(
  name: "GenUnicodeData",
  platforms: [.macOS(.v12)],
  targets: [
    .target(
      name: "GenUtils",
      dependencies: []
    ),
    .executableTarget(
      name: "GenGraphemeBreakProperty",
      dependencies: ["GenUtils"]
    ),
    .executableTarget(
      name: "GenNormalization",
      dependencies: ["GenUtils"]
    ),
    .executableTarget(
      name: "GenScalarProps",
      dependencies: ["GenUtils"]
    ),
    .executableTarget(
      name: "GenWordBreak",
      dependencies: ["GenUtils"]
    ),
    .executableTarget(
      name: "GenCaseFolding",
      dependencies: ["GenUtils"]
    ),
    .executableTarget(
      name: "GenScripts",
      dependencies: ["GenUtils"]
    )
  ]
)
