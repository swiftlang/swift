// swift-tools-version: 5.7

import PackageDescription

let package = Package(
  name: "Reflection",
  platforms: [
    .macOS(.v13)
  ],
  products: [
    .library(
      name: "_Runtime",
      type: .dynamic,
      targets: ["_Runtime"]
    ),
    .library(
      name: "Reflection",
      type: .dynamic,
      targets: ["Reflection"]
    ),
  ],
  targets: [
    .target(
      name: "_Runtime",
      swiftSettings: [
        .unsafeFlags([
          "-I", "\(Context.packageDirectory)/Sources/CRuntime",
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
      dependencies: ["_Runtime"],
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
      dependencies: ["_Runtime"]
    ),
    .testTarget(
      name: "ReflectionTests",
      dependencies: ["_Runtime", "Reflection"]
    ),
  ]
)
