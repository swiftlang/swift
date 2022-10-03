// swift-tools-version: 5.6

import PackageDescription

let package = Package(
    name: "ASTGen",
    platforms: [
        .macOS(.v10_15)
    ],
    products: [
        .library(name: "swiftASTGen", targets: ["swiftASTGen"]),
    ],
    dependencies: [
         .package(path: "../../../swift-syntax"),
    ],
    targets: [
        .target(
            name: "swiftASTGen",
            dependencies: [
              .product(name: "SwiftSyntax", package: "swift-syntax"),
              .product(name: "SwiftParser", package: "swift-syntax")
            ],
            path: ".",
            exclude: ["CMakeLists.txt"],
        swiftSettings: [
          .unsafeFlags([
            "-I", "../../include/swift/",
            "-I", "../../include"
          ])
        ]),
    ]
)
