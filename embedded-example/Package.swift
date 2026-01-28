// swift-tools-version: 6.3
import PackageDescription

let package = Package(
  name: "EmbeddedExample",
  platforms: [.macOS(.v15), .iOS(.v13), .tvOS(.v13), .watchOS(.v6), .macCatalyst(.v13)],
  products: [
    .executable(
      name: "EmbeddedHello",
      targets: ["EmbeddedHello"]
    ),
  ],
  dependencies: [
    .package(path: "../stdlib/public"),
  ],
  targets: [
    .executableTarget(
      name: "EmbeddedHello",
      dependencies: [
        .product(name: "Swift", package: "public"),
        .product(name: "_Builtin_float", package: "public"),
      ],
      swiftSettings: [
        .enableExperimentalFeature("Embedded"),
        .unsafeFlags([
            "-nostdimport", "-nostdlibimport",
          ])
      ]
    ),
  ]
)
