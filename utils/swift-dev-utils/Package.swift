// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription
import class Foundation.ProcessInfo

let package = Package(
    name: "swift-dev-utils",
    platforms: [.macOS(.v15)],
    targets: [
        // MARK: swift-xcodegen
        .target(
            name: "SwiftXcodeGen",
            dependencies: [
              "Utils",
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
          ],
          exclude: [
            "README.md",
          ]
        ),
        .testTarget(
          name: "SwiftXcodeGenTest",
          dependencies: ["SwiftXcodeGen"]
        ),

        // MARK: Utils
        .target(
          name: "Utils",
          dependencies: [
            .product(name: "ArgumentParser", package: "swift-argument-parser"),
          ],
          swiftSettings: [
            // Treat everything as @inlinable
            .unsafeFlags(["-enable-cmo-everything"]),
          ]
        ),
        .testTarget(
          name: "UtilsTest",
          dependencies: ["Utils"]
        )
    ],
    swiftLanguageModes: [.v6]
)

let useLocalDeps = ProcessInfo.processInfo.environment["SWIFTCI_USE_LOCAL_DEPS"] != nil

// crash-reduce requires swift-subprocess, which isn't yet supported with local deps.
if !useLocalDeps {
  package.targets += [
    // MARK: crash-reduce
    .target(
      name: "CrashReduce",
      dependencies: [
        "Utils",
        .product(name: "Subprocess", package: "swift-subprocess"),
        .product(name: "SwiftSyntax", package: "swift-syntax"),
        .product(name: "SwiftParser", package: "swift-syntax"),
        .product(name: "SwiftBasicFormat", package: "swift-syntax"),
        .product(name: "SwiftOperators", package: "swift-syntax"),
        .product(name: "SwiftFormat", package: "swift-format"),
      ]
    ),
    .executableTarget(
      name: "crash-reduce",
      dependencies: [
        "CrashReduce",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
        .product(name: "Subprocess", package: "swift-subprocess"),
      ],
      exclude: [
        "README.md",
      ]
    ),
    .testTarget(
      name: "CrashReduceTests",
      dependencies: ["CrashReduce"]
    ),
  ]
}

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

if !useLocalDeps {
  package.dependencies += [
    .package(url: "https://github.com/apple/swift-argument-parser", from: "1.4.0"),
    .package(url: "https://github.com/swiftlang/swift-driver", branch: "main"),
    .package(url: "https://github.com/swiftlang/swift-syntax.git", branch: "main"),
    .package(url: "https://github.com/swiftlang/swift-format.git", branch: "main"),
    .package(url: "https://github.com/swiftlang/swift-subprocess.git", branch: "main"),
  ]
} else {
  package.dependencies += [
    .package(path: "../../../swift-argument-parser"),
    .package(path: "../../../swift-driver"),

    // crash-reduce deps
    // FIXME: Subprocess isn't currently supported as a local dependency
    // .package(path: "../../../swift-syntax"),
    // .package(path: "../../../swift-format"),
    // .package(path: "../../../swift-subprocess"),
  ]
}
