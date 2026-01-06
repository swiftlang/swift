// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription
import class Foundation.ProcessInfo

let package = Package(
    name: "swift-xcodegen",
    platforms: [.macOS(.v15)],
    targets: [
        .target(
            name: "SwiftXcodeGen",
            dependencies: [
              .product(name: "ArgumentParser", package: "swift-argument-parser"),
              .product(name: "SwiftOptions", package: "swift-driver"),
            ],
            exclude: [
              "Xcodeproj/README.md",
            ]
        ),
        .executableTarget(
          name: "swift-xcodegen",
          dependencies: [
            .product(name: "ArgumentParser", package: "swift-argument-parser"),
            "SwiftXcodeGen"
          ]
        ),
        .testTarget(
          name: "SwiftXcodeGenTest",
          dependencies: ["SwiftXcodeGen"]
        )
    ],
    swiftLanguageModes: [.v6]
)

// Apply global Swift settings to targets.
do {
  var globalSwiftSettings: [SwiftSetting] = [
    // Swift 7 mode upcoming features. These must be compatible with 'swift-tools-version'.
    .enableUpcomingFeature("ExistentialAny"),
    .enableUpcomingFeature("InternalImportsByDefault"),
    .enableUpcomingFeature("MemberImportVisibility"),
    .enableUpcomingFeature("NonisolatedNonsendingByDefault"),
  ]

  #if compiler(>=6.1)
  globalSwiftSettings.append(
    .unsafeFlags(["-Werror", "ExistentialAny"])
  )
  #endif

  globalSwiftSettings += []  // avoid unused warning

  for target in package.targets where target.type != .plugin {
    if let swiftSettings = target.swiftSettings {
      // Target-specific settings should come last.
      target.swiftSettings = globalSwiftSettings + swiftSettings
    } else {
      target.swiftSettings = globalSwiftSettings
    }
  }
}

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
