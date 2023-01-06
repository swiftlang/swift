// swift-tools-version: 5.7

import PackageDescription

let package = Package(
  name: "Reflection",
  platforms: [
    .macOS(.v13)
  ],
  products: [
    .library(
      name: "Runtime",
      type: .dynamic,
      targets: ["Runtime"]
    ),
    .library(
      name: "Reflection",
      type: .dynamic,
      targets: ["Reflection"]
    ),
  ],
  targets: [
    .target(
      name: "CRuntime"
    ),
    .target(
      name: "Runtime",
      dependencies: ["CRuntime"],
      swiftSettings: [
        .unsafeFlags([
          "-parse-stdlib",
          "-enable-library-evolution",
          "-Xfrontend", "-disable-implicit-concurrency-module-import",
          "-Xfrontend", "-disable-implicit-string-processing-module-import",
          "-assert-config", "Release"
        ])
      ]
    ),
    .target(
      name: "Reflection",
      dependencies: ["Runtime"],
      swiftSettings: [
        .unsafeFlags([
          "-parse-stdlib",
          "-enable-library-evolution",
          "-Xfrontend", "-disable-implicit-concurrency-module-import",
          "-Xfrontend", "-disable-implicit-string-processing-module-import",
          "-assert-config", "Release"
        ])
      ]
    ),
    
    .testTarget(
      name: "RuntimeTests",
      dependencies: ["Runtime"]
    ),
    .testTarget(
      name: "ReflectionTests",
      dependencies: ["Runtime", "Reflection"]
    ),
  ]
)
